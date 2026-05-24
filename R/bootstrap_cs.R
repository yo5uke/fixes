# ============================================================================
# bootstrap_cs.R
# Multiplier bootstrap (Algorithm 1, Callaway & Sant'Anna 2021) for the
# unconditional CS estimator.
#
# Public helpers (noRd, used internally by run_es):
#   .mammen_weights(n)
#   .compute_influence_cs(data, att_gt, ...)
#   .aggregate_psi_es(psi_gt, gt_index, att_gt, cohort_sizes)
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

  # ---- bookkeeping ----------------------------------------------------------
  all_units <- sort(unique(data[[unit_chr]]))
  n_units <- length(all_units)

  all_times <- sort(unique(data[[time_chr]]))
  n_times <- length(all_times)

  # row-to-position maps (integer index into all_units / all_times)
  unit_pos <- match(data[[unit_chr]], all_units) # length nrow(data)
  time_pos <- match(data[[time_chr]], all_times) # length nrow(data)

  # ---- outcome matrix [unit_pos x time_pos] ---------------------------------
  Y_mat <- matrix(NA_real_, nrow = n_units, ncol = n_times)
  Y_mat[cbind(unit_pos, time_pos)] <- data[[outcome_chr]]

  # time value -> column index (for O(1) lookup)
  time_to_col <- setNames(seq_len(n_times), as.character(all_times))

  # ---- per-unit timing (one timing value per unit, or NA) ------------------
  # Collapse: for each unit, pick the unique non-NA timing value.
  timing_vals <- data[[timing_chr]]
  # Build unit_timing[k] = timing for the k-th position in all_units
  unit_timing <- rep(NA_real_, n_units)
  for (k in seq_len(nrow(data))) {
    if (!is.na(timing_vals[k])) {
      unit_timing[unit_pos[k]] <- timing_vals[k]
    }
  }
  # unit_timing is now length n_units; NA = never-treated

  # ---- initialise output matrix ---------------------------------------------
  n_gt <- nrow(att_gt)
  psi <- matrix(0.0, nrow = n_units, ncol = n_gt)

  gt_index <- data.frame(
    col_idx = seq_len(n_gt),
    g = att_gt$g,
    t = att_gt$t,
    stringsAsFactors = FALSE
  )

  # ---- main loop: one column per (g, t) pair --------------------------------
  for (j in seq_len(n_gt)) {
    g_j <- att_gt$g[j]
    t_j <- att_gt$t[j]
    att_j <- att_gt[[att_col]][j]
    base_t <- g_j - 1L - anticipation

    col_t <- time_to_col[as.character(t_j)]
    col_base <- time_to_col[as.character(base_t)]
    if (is.na(col_t) || is.na(col_base)) {
      next
    }

    # ΔY_{i, g, t} = Y_{i,t} - Y_{i, base_t}  for every unit
    delta_y <- Y_mat[, col_t] - Y_mat[, col_base] # length n_units; NA if obs missing

    # treated: G_i = g_j
    is_treat <- !is.na(unit_timing) & unit_timing == g_j
    # control: depends on control_group
    if (control_group == "nevertreated") {
      is_ctrl <- is.na(unit_timing)
    } else {
      # notyettreated: G_i > t_j (including never-treated), excluding cohort g_j
      is_ctrl <- (is.na(unit_timing) | unit_timing > t_j) &
        (is.na(unit_timing) | unit_timing != g_j)
    }

    # restrict to units with valid ΔY observations
    valid_treat <- is_treat & !is.na(delta_y)
    valid_ctrl <- is_ctrl & !is.na(delta_y)

    n_treat <- sum(valid_treat)
    n_ctrl <- sum(valid_ctrl)
    if (n_treat == 0L || n_ctrl == 0L) {
      next
    }

    mean_g <- mean(delta_y[valid_treat])
    mean_c <- mean(delta_y[valid_ctrl])

    # large-scale weights: n_units / group_size
    w_treat <- n_units / n_treat
    w_ctrl <- n_units / n_ctrl

    # treated contribution (positive)
    psi[valid_treat, j] <- w_treat * (delta_y[valid_treat] - mean_g)
    # control contribution (negative)
    psi[valid_ctrl, j] <- psi[valid_ctrl, j] -
      w_ctrl * (delta_y[valid_ctrl] - mean_c)
  }

  list(psi = psi, gt_index = gt_index)
}

# ---------------------------------------------------------------------------
# Aggregate (g,t)-level influence functions to event-study (relative time)
# level, using the same cohort-size weights as .run_cs().
#
# For each relative time ell, the event-study aggregate is:
#   theta_es(ell) = sum_{g: g+ell valid} w(g,ell) * ATT(g, g+ell)
# where w(g,ell) = n_g / sum_{g'} n_{g'}.  The corresponding influence
# function column is:
#   psi_es[i, ell] = sum_{g} w(g,ell) * psi_gt[i, idx(g, g+ell)]
#
# @param psi_gt     matrix (n_units x n_gt) from .compute_influence_cs()
# @param gt_index   data.frame (col_idx, g, t) from .compute_influence_cs()
# @param att_gt     data.frame with g, t, relative_time from .run_cs()
# @param cohort_sizes named integer vector (cohort label -> n_units in cohort)
# @return list(psi_es = matrix n_units x n_rel_times,
#              gt_es_index = data.frame(col_idx, relative_time, estimate, std_error))
# @noRd
.aggregate_psi_es <- function(psi_gt, gt_index, att_gt, cohort_sizes, att_es) {
  event_times <- att_es$relative_time # already sorted by .run_cs()
  n_units <- nrow(psi_gt)
  n_es <- length(event_times)

  psi_es <- matrix(0.0, nrow = n_units, ncol = n_es)
  gt_es_idx <- data.frame(
    col_idx = seq_len(n_es),
    relative_time = event_times,
    estimate = att_es$estimate,
    std_error = att_es$std_error,
    stringsAsFactors = FALSE
  )

  for (j in seq_len(n_es)) {
    ell <- event_times[j]
    # rows of att_gt with this relative time
    rows_ell <- which(att_gt$relative_time == ell)
    if (length(rows_ell) == 0L) {
      next
    }
    eligible_g <- att_gt$g[rows_ell]

    sz <- cohort_sizes[as.character(eligible_g)]
    total <- sum(sz, na.rm = TRUE)
    if (total == 0L) {
      next
    }
    w <- sz / total

    for (k in seq_along(eligible_g)) {
      g_k <- eligible_g[k]
      t_k <- g_k + ell
      col_k <- gt_index$col_idx[gt_index$g == g_k & gt_index$t == t_k]
      if (length(col_k) != 1L) {
        next
      }
      psi_es[, j] <- psi_es[, j] + w[k] * psi_gt[, col_k]
    }
  }

  list(psi_es = psi_es, gt_es_idx = gt_es_idx)
}

# ---------------------------------------------------------------------------
# Multiplier bootstrap for simultaneous inference — Algorithm 1 of
# Callaway & Sant'Anna (2021).
#
# Works at any aggregation level: pass psi for ATT(g,t)-level bands or
# the event-study aggregated psi (from .aggregate_psi_es) for simultaneous
# bands over the event-study curve.
#
# Implementation:
#   Pilot (199 draws) — estimate Sigma^{1/2}(g,t) via the normalised IQR of
#   sqrt(n) * En[V * psi_{g,t}] across pilot draws (Algorithm 1, step 4).
#   Main  (B draws)  — compute t-stat = max |R*(g,t)| / Sigma^{1/2}(g,t)
#   per draw; the (1-alpha) quantile is the simultaneous critical value.
#
# Vectorised: all B draws use a single matrix multiply (V %*% psi), so the
# per-draw cost is dominated by BLAS, not R loop overhead.
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
