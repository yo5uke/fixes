#' Wooldridge (2025) Two-Way Mundlak (TWM) Estimator
#'
#' @description
#' Implements the POLS-on-cohort-dummies estimator from Wooldridge (2025,
#' Procedure 5.1) for staggered adoption panel data.  The regression includes
#' cohort-by-calendar-time treatment indicators and two-way fixed effects:
#' \deqn{Y_{it} = \alpha_i + \alpha_t +
#'   \sum_{g}\sum_{s \neq c_0(g)} \tau_{g,s}
#'   \cdot \mathbf{1}(G_i=g) \cdot \mathbf{1}(t=s) + \varepsilon_{it}}
#' where \eqn{c_0(g) = g + \text{baseline}} is the cohort-specific excluded
#' calendar period.  After estimation, the \eqn{\tau_{g,s}} are aggregated to
#' the event-study curve using the same cohort-size weights as SA (2021):
#' \deqn{\hat\theta^{TWM}(\ell) =
#'   \sum_g \hat\tau_{g,\,g+\ell} \cdot w(g,\ell)}
#'
#' @section Equivalence with SA (2021):
#' When \code{trends = FALSE} and no covariates are included, this estimator is
#' \strong{algebraically identical} to \code{.run_sa()}.  The only difference is
#' the parameterisation: TWM uses cohort × calendar-time coordinates while SA
#' uses cohort × relative-time coordinates.  The event-study aggregates must
#' match to floating-point precision; this is tested in \code{test-twm.R}.
#'
#' @param data data.frame with one row per unit-period (balanced panel).
#' @param outcome_chr Column name (character) for the outcome variable.
#' @param timing_chr Column name (character) for first treatment period;
#'   \code{NA} marks never-treated units.
#' @param time_chr Column name (character) for calendar time (numeric).
#' @param unit_chr Column name (character) for unit identifier.
#' @param fe_str Right-hand side of the fixed-effects specification as a
#'   character string, e.g. \code{"id + year"}.
#' @param baseline Integer reference (base) relative period, default \code{-1L}.
#'   The excluded calendar period for cohort \eqn{g} is \eqn{g + \text{baseline}}.
#' @param cluster Cluster specification forwarded to \code{fixest::feols()}.
#' @param vcov_type VCOV type string, e.g. \code{"HC1"} (default).
#' @param vcov_args List of extra arguments forwarded to
#'   \code{fixest::vcov()}.
#' @param conf.level Numeric confidence level(s), default \code{0.95}.
#'
#' @return A list with:
#' \describe{
#'   \item{\code{es}}{data.frame of TWM event-study estimates: columns
#'     \code{relative_time}, \code{estimate}, \code{std_error}, and
#'     \code{conf_low_XX}/\code{conf_high_XX} per level.}
#'   \item{\code{tau_gt}}{data.frame of raw \eqn{\tau_{g,s}} estimates:
#'     \code{g}, \code{t} (calendar), \code{l} (relative = t - g),
#'     \code{estimate}, \code{std_error}, \code{col_name}.}
#'   \item{\code{cohorts}}{Sorted numeric vector of cohort values.}
#'   \item{\code{n_never}}{Number of never-treated units.}
#'   \item{\code{cohort_sizes}}{Named integer vector of cohort sizes.}
#'   \item{\code{n_obs}}{Number of observations used in the regression.}
#'   \item{\code{formula_str}}{Character string of the estimated formula.}
#' }
#' @noRd
.run_twm <- function(data,
                     outcome_chr,
                     timing_chr,
                     time_chr,
                     unit_chr,
                     fe_str,
                     baseline   = -1L,
                     cluster    = NULL,
                     vcov_type  = "HC1",
                     vcov_args  = list(),
                     conf.level = 0.95) {

  baseline <- as.integer(baseline)

  # ---- Validate inputs -------------------------------------------------------
  for (col in c(outcome_chr, timing_chr, time_chr, unit_chr)) {
    if (!col %in% names(data))
      stop("Column '", col, "' not found in data.")
  }
  if (!is.numeric(data[[time_chr]]))
    stop("'", time_chr, "' must be numeric.")

  # ---- Bookkeeping -----------------------------------------------------------
  data        <- data[order(data[[unit_chr]], data[[time_chr]]), ]
  all_periods <- sort(unique(data[[time_chr]]))
  min_t       <- min(all_periods)
  max_t       <- max(all_periods)

  timing_vec <- data[[timing_chr]]
  cohorts    <- sort(unique(timing_vec[!is.na(timing_vec)]))
  if (length(cohorts) == 0L)
    stop("No treated units found: all '", timing_chr, "' values are NA.")

  never_units  <- unique(data[[unit_chr]][is.na(timing_vec)])
  n_never      <- length(never_units)

  cohort_sizes <- vapply(cohorts, function(g)
    length(unique(data[[unit_chr]][!is.na(timing_vec) & timing_vec == g])),
    integer(1L))
  names(cohort_sizes) <- as.character(cohorts)

  # ---- Build cohort x calendar-time indicator matrix -------------------------
  # TWM (Wooldridge 2025, Procedure 5.1):
  #   T_{g,s}(i,t) = 1(G_i=g) * 1(t=s)
  # For each cohort g, exclude the calendar period c_0(g) = g + baseline.
  #
  # Design follows .run_sa() exactly: pre-allocate an integer matrix (.twm_X),
  # store it as a single matrix column in data, and let feols expand the K
  # columns internally.  This avoids allocating K separate vectors and copying
  # the full data frame K times.
  #
  # feols prepends the matrix-column name ".twm_X" to each coefficient, so
  # ".twm__g__1998__t__1995" becomes ".twm_X.twm__g__1998__t__1995".

  # All feasible (g, s) pairs — same structure as SA's (g, l) pairs, but
  # here s is CALENDAR time and l = s - g is derived afterwards.
  gs_pairs <- do.call(rbind, lapply(cohorts, function(g) {
    excl_s <- g + baseline                        # excluded calendar period
    fs     <- all_periods[all_periods != excl_s]  # all other calendar periods
    if (length(fs) == 0L) return(NULL)
    data.frame(g = g, s = fs, stringsAsFactors = FALSE)
  }))

  if (is.null(gs_pairs) || nrow(gs_pairs) == 0L)
    stop("No cohort-by-period interactions could be constructed. ",
         "Check that timing, time, and baseline are consistent.")

  K       <- nrow(gs_pairs)
  N       <- nrow(data)
  ind_mat <- matrix(0L, nrow = N, ncol = K)
  col_names <- character(K)

  k <- 0L
  for (g in cohorts) {
    g_mask <- !is.na(timing_vec) & timing_vec == g   # computed once per cohort
    for (j in which(gs_pairs$g == g)) {
      s <- gs_pairs$s[j]
      k <- k + 1L
      # Safe string for negative calendar times (unlikely but handled)
      s_safe <- if (s < 0L) paste0("neg", -s) else as.character(s)
      col_names[k] <- paste0(".twm__g__", g, "__t__", s_safe)
      ind_mat[, k] <- as.integer(g_mask & data[[time_chr]] == s)
    }
  }
  colnames(ind_mat) <- col_names

  # tau_meta: maps column index k → (g, s, l, prefixed feols coefficient name)
  # feols naming rule for matrix regressors:
  #   K > 1:  coefficient = "<matrix_name><col_name>"  (e.g. ".twm_X.twm__g__1998__t__1995")
  #   K == 1: coefficient = "<matrix_name>"            (e.g. ".twm_X")
  tau_meta <- lapply(seq_len(K), function(k)
    list(col = if (K == 1L) ".twm_X" else paste0(".twm_X", col_names[k]),
         g   = gs_pairs$g[k],
         s   = gs_pairs$s[k],
         l   = gs_pairs$s[k] - gs_pairs$g[k]))   # relative time l = s - g

  data$.twm_X <- ind_mat

  formula_str <- if (nzchar(fe_str)) {
    paste0(outcome_chr, " ~ .twm_X | ", fe_str)
  } else {
    paste0(outcome_chr, " ~ .twm_X")
  }

  # ---- Run regression --------------------------------------------------------
  model_args <- list(stats::as.formula(formula_str), data = data)
  if (!is.null(cluster)) model_args$cluster <- cluster

  model <- tryCatch(
    do.call(fixest::feols, model_args),
    error = function(e)
      stop("TWM regression failed: ", e$message,
           "\nCheck for collinearity (too few control units, or window ",
           "spanning the full sample).")
  )

  # ---- Extract coefficients and full VCOV ------------------------------------
  if (!is.null(cluster) && identical(vcov_type, "HC1")) {
    V_full <- stats::vcov(model)
  } else {
    V_full <- tryCatch(
      stats::vcov(model, vcov = vcov_type, .vcov_args = vcov_args),
      error = function(e) stats::vcov(model)
    )
  }
  coef_names <- rownames(V_full)
  tidy_df    <- broom::tidy(model, vcov = V_full)

  # ---- Extract tau_{g,s} point estimates -------------------------------------
  tau_rows <- list()
  for (m in tau_meta) {
    idx <- match(m$col, tidy_df$term)
    if (is.na(idx)) next
    tau_rows[[length(tau_rows) + 1L]] <- data.frame(
      g         = m$g,
      t         = m$s,
      l         = m$l,
      estimate  = tidy_df$estimate[idx],
      std_error = tidy_df$std.error[idx],
      col_name  = m$col,
      stringsAsFactors = FALSE
    )
  }

  if (length(tau_rows) == 0L)
    stop("No tau_{g,s} coefficients could be extracted from the TWM regression.")

  tau_gt           <- do.call(rbind, tau_rows)
  tau_gt$col_name  <- as.character(tau_gt$col_name)
  rownames(tau_gt) <- NULL

  # ---- IW aggregation — same formula as SA (2021) eq. (4) -------------------
  # theta_es(l) = sum_g  tau_{g, g+l} * w(g,l)
  # w(g,l)      = n_g / sum_{g': g'+l in [min_t, max_t]} n_{g'}
  # Var(theta_es(l)) = w(l)' * Sigma_l * w(l)   [quadratic form on VCOV block]

  event_times <- sort(unique(tau_gt$l))
  es_rows     <- list()

  for (l in event_times) {
    sub      <- tau_gt[tau_gt$l == l, ]
    idx_in_V <- match(sub$col_name, coef_names)
    valid    <- !is.na(idx_in_V)
    if (!any(valid)) next

    sub_v <- sub[valid, ]
    idx_v <- idx_in_V[valid]

    in_samp  <- cohorts[(cohorts + l) >= min_t & (cohorts + l) <= max_t]
    sz_denom <- sum(cohort_sizes[as.character(in_samp)])
    if (sz_denom == 0L) next

    w        <- cohort_sizes[as.character(sub_v$g)] / sz_denom
    theta    <- sum(w * sub_v$estimate)

    V_sub     <- V_full[idx_v, idx_v, drop = FALSE]
    var_theta <- as.numeric(t(w) %*% V_sub %*% w)

    es_rows[[length(es_rows) + 1L]] <- data.frame(
      relative_time = l,
      estimate      = theta,
      std_error     = sqrt(max(var_theta, 0)),
      stringsAsFactors = FALSE
    )
  }

  if (length(es_rows) == 0L)
    stop("TWM event-study aggregation produced no estimates.")

  es           <- do.call(rbind, es_rows)
  es           <- es[order(es$relative_time), ]
  rownames(es) <- NULL

  # ---- Confidence intervals --------------------------------------------------
  conf.level <- sort(unique(conf.level))
  for (cl in conf.level) {
    z   <- stats::qnorm(1 - (1 - cl) / 2)
    suf <- sprintf("%.0f", cl * 100)
    es[[paste0("conf_low_",  suf)]] <- es$estimate - z * es$std_error
    es[[paste0("conf_high_", suf)]] <- es$estimate + z * es$std_error
  }

  list(
    es           = es,
    tau_gt       = tau_gt,
    cohorts      = cohorts,
    n_never      = n_never,
    cohort_sizes = cohort_sizes,
    n_obs        = stats::nobs(model),
    formula_str  = formula_str
  )
}
