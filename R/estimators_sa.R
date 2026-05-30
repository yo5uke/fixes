#' Sun-Abraham (2021) Interaction-Weighted Estimator
#'
#' @description
#' Implements the Sun and Abraham (2021) interaction-weighted (IW) estimator.
#' Step 1 estimates cohort-average treatment effects (CATTs) via a saturated
#' OLS regression with unit and time fixed effects (SA 2021, eq. 1):
#' \deqn{Y_{it} = \alpha_i + \alpha_t +
#'   \sum_{g}\sum_{\ell \neq \ell_0} \delta_{g,\ell}
#'   \cdot \mathbf{1}(G_i=g) \cdot \mathbf{1}(t - G_i = \ell) + \varepsilon_{it}}
#' Step 2 aggregates with cohort-share weights (SA 2021, eq. 4):
#' \deqn{\hat\theta^{IW}(\ell) =
#'   \sum_g \hat\delta_{g,\ell} \cdot \widehat{E}[\mathbf{1}(G=g)\mid K=\ell]}
#' where \eqn{\widehat{E}[\mathbf{1}(G=g)\mid K=\ell] = n_g /
#'   \sum_{g':\,g'+\ell \in [t_{\min},t_{\max}]} n_{g'}}.
#' Standard errors use the full quadratic form
#' \eqn{SE^2(\hat\theta^{IW}(\ell)) = \mathbf{w}(\ell)^\top \Sigma_\ell
#' \mathbf{w}(\ell)} over the VCOV block for all cohorts at \eqn{\ell}.
#'
#' @param data data.frame with one row per unit-period (balanced panel).
#' @param outcome_chr Column name (character) for the outcome variable.
#' @param timing_chr Column name (character) for first treatment period;
#'   \code{NA} marks never-treated units.
#' @param time_chr Column name (character) for calendar time (numeric).
#' @param unit_chr Column name (character) for unit identifier.
#' @param fe_str Right-hand side of the fixed-effects specification as a
#'   character string, e.g. \code{"id + year"}. Passed after \code{|} in
#'   the \pkg{fixest} formula.
#' @param baseline Integer reference (base) relative period, default \code{-1L}.
#'   Interactions at \eqn{\ell = \code{baseline}} are excluded from the
#'   regression (identification normalisation).
#' @param cluster Cluster specification forwarded to \code{fixest::feols()}.
#' @param vcov_type VCOV type string, e.g. \code{"HC1"} (default).
#' @param vcov_args List of extra arguments forwarded to
#'   \code{fixest::vcov()}.
#' @param conf.level Numeric confidence level(s), default \code{0.95}.
#'
#' @return A list with:
#' \describe{
#'   \item{\code{es}}{data.frame of IW event-study estimates: columns
#'     \code{relative_time}, \code{estimate}, \code{std_error}, and
#'     \code{conf_low_XX}/\code{conf_high_XX} per level.}
#'   \item{\code{catt_df}}{data.frame of raw CATT(g,\eqn{\ell}) estimates:
#'     \code{g}, \code{l}, \code{estimate}, \code{std_error}, \code{col_name}.}
#'   \item{\code{cohorts}}{Sorted numeric vector of cohort values.}
#'   \item{\code{n_never}}{Number of never-treated units.}
#'   \item{\code{cohort_sizes}}{Named integer vector of cohort sizes.}
#'   \item{\code{n_obs}}{Number of observations used in the regression.}
#'   \item{\code{formula_str}}{Character string of the estimated formula.}
#' }
#' @noRd
.run_sa <- function(data,
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
  .validate_panel_cols(data, c(outcome_chr, timing_chr, time_chr, unit_chr), time_chr)

  # ---- Bookkeeping -----------------------------------------------------------
  data        <- data[order(data[[unit_chr]], data[[time_chr]]), ]
  all_periods <- sort(unique(data[[time_chr]]))
  min_t       <- min(all_periods)
  max_t       <- max(all_periods)

  cohorts <- sort(unique(data[[timing_chr]][!is.na(data[[timing_chr]])]))
  if (length(cohorts) == 0L)
    stop("No treated units found: all '", timing_chr, "' values are NA.")

  never_units <- unique(data[[unit_chr]][is.na(data[[timing_chr]])])
  n_never     <- length(never_units)

  cohort_sizes <- .compute_cohort_sizes(data, timing_chr, unit_chr, cohorts)

  # ---- Build cohort x relative-time indicator matrix -------------------------
  # SA (2021) eq. (1): D_{g,l}(i,t) = 1(G_i=g) * 1(t-G_i=l)
  #
  # Optimisation: instead of adding 76 indicator columns to the data frame
  # (one per (g,l) pair) and passing the bloated frame to feols(), we build a
  # pre-allocated integer matrix and pass it as a single matrix column (.sa_X).
  # feols() then sees the same 76 regressors but receives a lean data frame
  # (only outcome + FE variables + the matrix), eliminating the ~120 ms of GC
  # pressure that comes from feols copying 76 × 40,000 × 4 bytes of indicators.
  #
  # Key facts:
  #   - Coefficient estimates are bit-for-bit identical to the reference loop
  #     (same regression, same collinearity handling — verified in proto_sa_compact3.R)
  #   - feols prepends the matrix-column name ".sa_X" to each coefficient name:
  #     ".sa__1995__neg5" → ".sa_X.sa__1995__neg5"  (handled in CATT extraction)
  #   - feols timing: wide (76 cols) ~240 ms → compact (matrix col) ~105 ms

  timing_vec  <- data[[timing_chr]]
  reltime_vec <- data[[time_chr]] - timing_vec   # NA for never-treated

  # All feasible (g, l) pairs — same logic as the reference for loop
  gl_pairs <- do.call(rbind, lapply(cohorts, function(g) {
    fl <- all_periods - g
    fl <- fl[fl != baseline]
    if (length(fl) == 0L) return(NULL)
    data.frame(g = g, l = fl, stringsAsFactors = FALSE)
  }))

  if (is.null(gl_pairs) || nrow(gl_pairs) == 0L)
    stop("No cohort-by-period interactions could be constructed. ",
         "Check that the timing column, time column, and baseline are consistent.")

  K         <- nrow(gl_pairs)
  N         <- nrow(data)
  col_names <- character(K)
  k <- 0L
  for (g in cohorts) {
    for (j in which(gl_pairs$g == g)) {
      l <- gl_pairs$l[j]
      k <- k + 1L
      col_names[k] <- paste0(".sa__", g, "__",
                             if (l < 0L) paste0("neg", -l) else as.character(l))
    }
  }

  # Build indicator matrix via Rcpp (replaces nested R for-loop).
  # SA uses relative time as the "s" axis: cohort_id=timing, time_id=reltime.
  ind_mat <- build_indicator_matrix_cpp(
    cohort_id = as.integer(timing_vec),
    time_id   = as.integer(reltime_vec),
    gs_g      = as.integer(gl_pairs$g),
    gs_s      = as.integer(gl_pairs$l)
  )
  colnames(ind_mat) <- col_names

  # catt_meta: used in CATT extraction (feols prepends ".sa_X" to col names)
  catt_meta <- lapply(seq_len(K), function(k)
    list(col      = paste0(".sa_X", col_names[k]),   # as seen in tidy_df$term
         orig_col = col_names[k],                     # for display / formula_str
         g        = gl_pairs$g[k],
         l        = gl_pairs$l[k]))

  # Pass indicator matrix as a single matrix column; feols reads the K columns
  # directly without the data-frame allocation overhead of 76 separate vectors.
  data$.sa_X  <- ind_mat

  formula_str <- if (nzchar(fe_str)) {
    paste0(outcome_chr, " ~ .sa_X | ", fe_str)
  } else {
    paste0(outcome_chr, " ~ .sa_X")
  }

  # REFERENCE IMPLEMENTATION (pre-optimisation)
  # Retained for correctness verification.
  #
  # int_cols  <- character(0)
  # catt_meta_old <- list()
  # for (g in cohorts) {
  #   feasible_l <- all_periods - g
  #   feasible_l <- feasible_l[feasible_l != baseline]
  #   g_mask <- !is.na(data[[timing_chr]]) & data[[timing_chr]] == g
  #   for (l in feasible_l) {
  #     safe_l <- if (l < 0L) paste0("neg", -l) else as.character(l)
  #     col    <- paste0(".sa__", g, "__", safe_l)
  #     data[[col]] <- as.integer(
  #       g_mask & (data[[time_chr]] - data[[timing_chr]]) == l
  #     )
  #     int_cols <- c(int_cols, col)
  #     catt_meta_old[[...]] <- list(col = col, g = g, l = l)
  #   }
  # }
  # rhs_str <- paste(int_cols, collapse = " + ")
  # formula_str <- paste0(outcome_chr, " ~ ", rhs_str, " | ", fe_str)

  model_args <- list(stats::as.formula(formula_str), data = data)
  if (!is.null(cluster)) model_args$cluster <- cluster

  model <- tryCatch(
    do.call(fixest::feols, model_args),
    error = function(e)
      stop("SA regression failed: ", e$message,
           "\nCheck for near-perfect collinearity (too few control units, ",
           "or lead_range/lag_range spanning the full sample).")
  )

  # ---- Extract coefficients and full VCOV -----------------------------------
  # Always extract the full matrix so we can do the quadratic-form variance.
  V_full     <- .model_vcov_full(model, vcov_type, cluster, vcov_args)
  coef_names <- rownames(V_full)

  tidy_df    <- broom::tidy(model, vcov = V_full)

  # ---- Extract CATT(g,l) point estimates ------------------------------------
  # feols prepends the matrix-column name to coefficient names, so
  # ".sa__g__l" becomes ".sa_X.sa__g__l".  catt_meta$col already stores the
  # prefixed name; we match it against tidy_df$term directly.
  catt_rows <- list()
  for (m in catt_meta) {
    idx <- match(m$col, tidy_df$term)  # m$col = ".sa_X.sa__g__l"
    if (is.na(idx)) next               # dropped as collinear by fixest — skip
    catt_rows[[length(catt_rows) + 1L]] <- data.frame(
      g         = m$g,
      l         = m$l,
      estimate  = tidy_df$estimate[idx],
      std_error = tidy_df$std.error[idx],
      col_name  = m$col,              # ".sa_X.sa__g__l" — used for VCOV lookup
      stringsAsFactors = FALSE
    )
  }

  if (length(catt_rows) == 0L)
    stop("No CATT(g,l) coefficients could be extracted. ",
         "The SA regression may be degenerate.")

  catt_df           <- do.call(rbind, catt_rows)
  catt_df$col_name  <- as.character(catt_df$col_name)
  rownames(catt_df) <- NULL

  # ---- IW aggregation — SA (2021), eq. (4) ----------------------------------
  # theta_es(l) = sum_g  delta_{g,l} * w(g,l)
  # w(g,l)      = n_g / sum_{g': g'+l in [min_t,max_t]} n_{g'}
  # Var(theta_es(l)) = w(l)' * Sigma_l * w(l)
  #
  # Uses aggregate_iw_cpp (RcppArmadillo) for the quadratic-form VCOV step.
  es <- .aggregate_iw(catt_df, V_full, coef_names, cohort_sizes, min_t, max_t)

  if (nrow(es) == 0L)
    stop("SA event-study aggregation produced no estimates.")

  # ---- Confidence intervals -------------------------------------------------
  conf.level <- sort(unique(conf.level))
  es <- .add_ci_columns(es, conf.level, se_col = "std_error")

  list(
    es           = es,
    catt_df      = catt_df,
    cohorts      = cohorts,
    n_never      = n_never,
    cohort_sizes = cohort_sizes,
    n_obs        = stats::nobs(model),
    formula_str  = formula_str
  )
}
