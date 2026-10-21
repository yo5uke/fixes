# ============================================================================
# bootstrap_cs.R
# Multiplier bootstrap (Algorithm 1, Callaway & Sant'Anna 2021) for the
# unconditional CS estimator.
#
# Public helpers (noRd, used internally by run_es):
#   .mammen_weights(n)
#   .compute_influence_cs(data, att_gt, ...)
#   .cs_aggregate_if(ctx, att_gt, specs)
#   .bootstrap_cs(psi, att_gt, gt_index, B, alpha, seed)
# ============================================================================

# ---------------------------------------------------------------------------
# Mammen (1993) two-point multiplier weights
#
# Draws n iid Bernoulli variables with
#   P(V = 1 - kappa) = kappa / sqrt(5)    [kappa = (sqrt(5)+1)/2]
#   P(V =     kappa) = 1 - kappa / sqrt(5)
# These satisfy E[V] = 0, Var[V] = 1, and have finite third moment.
# ---------------------------------------------------------------------------
#' @noRd
.mammen_weights <- function(n) {
  kappa <- (sqrt(5) + 1) / 2 # golden ratio ≈ 1.618
  p1 <- kappa / sqrt(5) # P(V = 1 - kappa) ≈ 0.7236
  ifelse(stats::runif(n) < p1, 1 - kappa, kappa)
}

# ---------------------------------------------------------------------------
# Shared inputs of the (g,t)-level influence functions: the unit x period
# outcome matrix and each unit's cohort (NA = never treated). Rows follow
# sort(unique(data[[unit_chr]])).
# ---------------------------------------------------------------------------
#' @noRd
.cs_influence_context <- function(
  data,
  control_group,
  unit_chr,
  time_chr,
  timing_chr,
  outcome_chr,
  anticipation
) {
  all_units <- sort(unique(data[[unit_chr]]))
  n_units <- length(all_units)

  all_times <- sort(unique(data[[time_chr]]))
  n_times <- length(all_times)

  unit_pos <- match(data[[unit_chr]], all_units)
  time_pos <- match(data[[time_chr]], all_times)

  Y_mat <- matrix(NA_real_, nrow = n_units, ncol = n_times)
  Y_mat[cbind(unit_pos, time_pos)] <- data[[outcome_chr]]

  # With duplicated positions the last row wins.
  timing_vals <- data[[timing_chr]]
  unit_timing <- rep(NA_real_, n_units)
  ok <- !is.na(timing_vals)
  unit_timing[unit_pos[ok]] <- timing_vals[ok]

  list(
    Y_mat         = Y_mat,
    unit_timing   = unit_timing,
    time_to_col   = setNames(seq_len(n_times), as.character(all_times)),
    control_group = control_group,
    anticipation  = as.integer(anticipation),
    n_units       = n_units
  )
}

# psi_hat(i; g, t) for the (g, t) pairs given, as an n_units x length(g)
# matrix. With `weight` and `out_col`, column j of the n_units x n_out result
# is instead sum_{k: out_col[k] = j} weight[k] * psi_hat(i; g_k, t_k).
#' @noRd
.cs_psi_cols <- function(ctx, g, t, weight = rep(1, length(g)),
                         out_col = seq_along(g), n_out = length(g)) {
  col_t    <- unname(ctx$time_to_col[as.character(t)])
  col_base <- unname(ctx$time_to_col[as.character(g - 1L - ctx$anticipation)])
  compute_psi_cs_cpp(
    Y_mat         = ctx$Y_mat,
    unit_timing   = as.numeric(ctx$unit_timing),
    gt_g          = as.integer(g),
    gt_t          = as.integer(t),
    col_t         = as.integer(col_t),
    col_base      = as.integer(col_base),
    control_group = ctx$control_group,
    weight        = as.numeric(weight),
    out_col       = as.integer(out_col),
    n_out         = as.integer(n_out)
  )
}

#' Influence Functions for the Callaway-Sant'Anna (2021) Multiplier Bootstrap
#'
#' @description
#' Computes unit-level influence functions psi_hat_{g,t}(i) for each
#' (cohort g, calendar period t) pair, as required by Algorithm 1 of
#' Callaway and Sant'Anna (2021).  In the unconditional case (no covariates,
#' delta = 0), the propensity score and outcome regression terms in eq. (4.4)
#' drop out, leaving the two-sample DiD score:
#'
#' ```
#'   psi_hat(i; g, t) =
#'     (n / n_treat) * 1(G_i = g) * (DeltaY_i - mean_g)    [treated term]
#'   - (n / n_ctrl)  * C_i        * (DeltaY_i - mean_c)    [control term]
#' ```
#'
#' where
#'   DeltaY_i = Y_{i,t} - Y_{i, g-1-anticipation}
#'   mean_g   = sample mean of DeltaY among cohort-g units
#'   mean_c   = sample mean of DeltaY among comparison units
#'   n        = total number of unique units in the panel
#'   C_i      = 1 iff unit i belongs to the comparison group for (g, t)
#'
#' Units not in cohort g and not in the comparison group contribute psi = 0.
#'
#' The "large" scaling (n / n_treat, n / n_ctrl) ensures:
#'   (1) En\[psi_hat\] = 0 exactly (by construction — each term is centered on
#'       its own group mean).
#'   (2) Var(psi_hat\[, j\]) / n  ≈  SE^2(ATT(g,t))  asymptotically, so that
#'       the bootstrap draw En\[V * psi_hat\] has the correct variance SE^2/n.
#'
#' @param data data.frame, one row per unit-period (balanced panel).
#' @param att_gt data.frame of ATT(g,t) point estimates.  Required columns:
#'   `g`, `t`, and one column named by `att_col` for the ATT
#'   values.  Typically the `$att_gt` component returned by
#'   `.run_cs()`, which uses the column name `"estimate"`.
#' @param control_group `"nevertreated"` (default) or
#'   `"notyettreated"`.  Must match the value used in `.run_cs()`.
#' @param unit_chr   Column name (character) for the unit identifier.
#' @param time_chr   Column name (character) for the calendar time variable.
#' @param timing_chr Column name (character) for the first treatment period;
#'   `NA` marks never-treated units.
#' @param outcome_chr Column name (character) for the outcome variable.
#' @param att_col    Column name in `att_gt` that holds the ATT estimate.
#'   Defaults to `"estimate"` (the name used by `.run_cs()`).
#'   Alternatively `"att"` for the raw Rcpp output.
#' @param anticipation Non-negative integer; anticipated treatment periods
#'   (shifts base period from g-1 to g-1-anticipation).  Default 0.
#'
#' @return A named list with two components:
#' \describe{
#'   \item{`psi`}{Numeric matrix of dimension (n_units x n_gt).  Column j
#'     holds psi_hat_{g_j, t_j}(i) for every unit i (row order matches
#'     `sort(unique(data[[unit_chr]]))`).}
#'   \item{`gt_index`}{data.frame with columns `col_idx`,
#'     `g`, `t` mapping each column of `psi` to its (g,t) pair.}
#' }
#' @noRd
.compute_influence_cs <- function(
  data,
  att_gt,
  control_group = c("nevertreated", "notyettreated"),
  unit_chr = "unit",
  time_chr = "time",
  timing_chr = "timing",
  outcome_chr = "outcome",
  att_col = "estimate",
  anticipation = 0L
) {
  control_group <- match.arg(control_group)
  anticipation <- as.integer(anticipation)

  # ---- resolve att column ---------------------------------------------------
  if (!att_col %in% names(att_gt)) {
    fallback <- setdiff(c("att", "estimate"), att_col)
    fallback <- fallback[fallback %in% names(att_gt)]
    if (length(fallback) == 0L) {
      stop(
        ".compute_influence_cs: att_gt must have a column named '",
        att_col,
        "' (or 'att' / 'estimate')."
      )
    }
    att_col <- fallback[1L]
  }

  ctx <- .cs_influence_context(
    data, control_group, unit_chr, time_chr, timing_chr, outcome_chr,
    anticipation
  )

  n_gt <- nrow(att_gt)
  gt_index <- data.frame(
    col_idx = seq_len(n_gt),
    g = att_gt$g,
    t = att_gt$t,
    stringsAsFactors = FALSE
  )

  psi <- .cs_psi_cols(ctx, att_gt$g, att_gt$t)

  list(psi = psi, gt_index = gt_index)
}

# ---------------------------------------------------------------------------
# Influence functions of aggregated parameters theta = sum_k w_k ATT(g_k, t_k).
#
# Each spec is list(rows, w, p):
#   rows : rows of att_gt entering the aggregate
#   w    : their weights
#   p    : NULL when the weights are known constants; otherwise the
#          unnormalised cohort shares p_k = n_{g_k} / n with w = p / sum(p).
# The influence function is sum_k w_k psi_k plus, when `p` is given, the
# term for the estimated weights (CS 2021, Sec. 4; did::aggte's `wif`):
#   sum_k ATT_k * d w_k = [(A_i - S) / P] - (M_i - P) * S / P^2,
# with P = sum(p), S = sum(ATT_k p_k), A_i = sum of ATT_k over the k in
# unit i's cohort and M_i the number of such k.
#
# SE(theta) = sqrt(sum_i IF_i^2) / n.
#
# @return n_units x length(specs) matrix of influence functions.
# @noRd
.cs_aggregate_if <- function(ctx, att_gt, specs) {
  rows <- unlist(lapply(specs, `[[`, "rows"))
  out  <- .cs_psi_cols(
    ctx, att_gt$g[rows], att_gt$t[rows],
    weight  = unlist(lapply(specs, `[[`, "w")),
    out_col = rep(seq_along(specs), lengths(lapply(specs, `[[`, "rows"))),
    n_out   = length(specs)
  )
  # The weight term depends on the unit only through its cohort.
  cohorts <- sort(unique(att_gt$g))
  i_g <- match(ctx$unit_timing, cohorts, nomatch = length(cohorts) + 1L)
  for (j in seq_along(specs)) {
    sp <- specs[[j]]
    if (is.null(sp$p)) next
    att <- att_gt$estimate[sp$rows]
    pos <- match(att_gt$g[sp$rows], cohorts)
    P   <- sum(sp$p)
    S   <- sum(att * sp$p)
    A_g <- c(vapply(seq_along(cohorts), function(c) sum(att[pos == c]), numeric(1L)), 0)
    M_g <- c(tabulate(pos, length(cohorts)), 0)
    out[, j] <- out[, j] + ((A_g - S) / P - (M_g - P) * S / P^2)[i_g]
  }
  out
}

# ---------------------------------------------------------------------------
# Multiplier bootstrap for simultaneous inference — Algorithm 1 of
# Callaway & Sant'Anna (2021).
#
# Works at any aggregation level: pass psi for ATT(g,t)-level bands or
# the event-study aggregated psi (from .cs_aggregate_if) for simultaneous
# bands over the event-study curve.
#
# Implementation:
#   Pilot (199 draws) — estimate Sigma^{1/2}(g,t) via the normalised IQR of
#   sqrt(n) * En[V * psi_{g,t}] across pilot draws (Algorithm 1, step 4).
#   Main  (B draws)  — compute t-stat = max |R*(g,t)| / Sigma^{1/2}(g,t)
#   per draw; the (1-alpha) quantile is the simultaneous critical value.
#
#' @param psi      Numeric matrix (n_units x n_params).  Columns are the
#'   unit-level influence functions, one per parameter (g,t) or relative time.
#' @param att_gt   data.frame of point estimates with columns \code{estimate}
#'   (or \code{att}) and \code{std_error} (or \code{se}).  Rows must
#'   correspond 1-to-1 with columns of \code{psi}.
#' @param gt_index data.frame whose rows map column indices of \code{psi} to
#'   parameter labels.  All columns are forwarded to the output unchanged.
#' @param B        Number of main bootstrap draws (default 999).
#' @param alpha    Significance level for the simultaneous band (default 0.05).
#' @param seed     Integer seed for reproducibility; \code{NULL} skips seeding.
#'
#' @return data.frame with all columns of \code{gt_index} plus:
#'   \code{att}, \code{se}, \code{conf_low_sim}, \code{conf_high_sim},
#'   \code{critical_value} (scalar ĉ_{1-alpha}), \code{B}.
#' @noRd
.bootstrap_cs <- function(
  psi,
  att_gt,
  gt_index,
  B = 999L,
  alpha = 0.05,
  seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # ---- resolve column names ------------------------------------------------
  att_col <- if ("estimate" %in% names(att_gt)) "estimate" else "att"
  se_col <- if ("std_error" %in% names(att_gt)) "std_error" else "se"
  att_vals <- att_gt[[att_col]]
  se_vals <- att_gt[[se_col]]

  # ---- Core computation via Rcpp (bootstrap_cs_cpp) -------------------------
  # Mammen weights are drawn inside Rcpp using R::runif(), so the same
  # set.seed() before this call produces identical results to the pure-R version.
  boot_result <- bootstrap_cs_cpp(psi, as.integer(B), alpha, 199L)
  c_hat <- boot_result$c_hat
  margin <- boot_result$margin

  # ---- Assemble output ----------------------------------------------------
  out <- gt_index
  out$att <- att_vals
  out$se <- se_vals
  out$conf_low_sim <- att_vals - margin
  out$conf_high_sim <- att_vals + margin
  out$critical_value <- c_hat
  out$B <- as.integer(B)
  rownames(out) <- NULL
  out
}
