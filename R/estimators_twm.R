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
                     baseline       = -1L,
                     trends         = FALSE,
                     covariate_chrs = NULL,
                     cluster        = NULL,
                     vcov_type      = "HC1",
                     vcov_args      = list(),
                     conf.level     = 0.95) {

  baseline <- as.integer(baseline)

  # ---- Validate inputs -------------------------------------------------------
  .validate_panel_cols(data, c(outcome_chr, timing_chr, time_chr, unit_chr), time_chr)

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

  cohort_sizes <- .compute_cohort_sizes(data, timing_chr, unit_chr, cohorts)

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
  #
  # When trends = TRUE (Wooldridge 2025, Section 8), include only POST-TREATMENT
  # periods (s >= g).  The pre-treatment data then identifies the cohort-specific
  # trend, so adding d_g*t to the regression is not collinear with the treatment
  # cells.  When trends = FALSE, include all periods as usual.
  gs_pairs <- do.call(rbind, lapply(cohorts, function(g) {
    excl_s <- g + baseline                        # excluded calendar period
    if (isTRUE(trends)) {
      # post-treatment only; if excl_s >= g it falls in the post window → exclude
      fs <- all_periods[all_periods >= g & all_periods != excl_s]
    } else {
      fs <- all_periods[all_periods != excl_s]    # all periods (pre + post)
    }
    if (length(fs) == 0L) return(NULL)
    data.frame(g = g, s = fs, stringsAsFactors = FALSE)
  }))

  if (is.null(gs_pairs) || nrow(gs_pairs) == 0L)
    stop("No cohort-by-period interactions could be constructed. ",
         "Check that timing, time, and baseline are consistent.")

  K         <- nrow(gs_pairs)
  N         <- nrow(data)
  col_names <- character(K)
  k <- 0L
  for (g in cohorts) {
    for (j in which(gs_pairs$g == g)) {
      s <- gs_pairs$s[j]
      k <- k + 1L
      s_safe       <- if (s < 0L) paste0("neg", -s) else as.character(s)
      col_names[k] <- paste0(".twm__g__", g, "__t__", s_safe)
    }
  }

  # Build indicator matrix via Rcpp.
  ind_mat <- build_indicator_matrix_cpp(
    cohort_id = as.integer(timing_vec),
    time_id   = as.integer(data[[time_chr]]),
    gs_g      = as.integer(gs_pairs$g),
    gs_s      = as.integer(gs_pairs$s)
  )
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

  # ---- Optional: cohort-specific linear trends (Wooldridge 2025, Section 8) --
  # Procedure 5.1 + trends uses POLS with cohort + time FE (NOT unit FE).
  # With unit FE, d_g (cohort indicator) is absorbed, making d_g*t collinear
  # with the treatment cell indicators.  Switching to cohort + time FE keeps
  # d_g explicit so d_g*t (the trend column) is identifiable.
  # Each cohort gets its own linear trend regressor: value = t if G_i=g, else 0.
  if (isTRUE(trends)) {
    K_trend     <- length(cohorts)
    trend_mat   <- matrix(0L, nrow = N, ncol = K_trend)
    trend_names <- character(K_trend)
    for (gi in seq_along(cohorts)) {
      g      <- cohorts[gi]
      g_mask <- !is.na(timing_vec) & timing_vec == g
      trend_mat[g_mask, gi] <- as.integer(data[[time_chr]][g_mask])
      trend_names[gi]       <- paste0(".twm_trend__g__", g)

      pre_periods <- all_periods[all_periods < g]
      if (length(pre_periods) < 2L)
        warning("Cohort ", g, " has fewer than 2 pre-treatment periods; ",
                "cohort-specific linear trend may not be identified.")
    }
    colnames(trend_mat) <- trend_names
    data$.twm_T_X <- trend_mat
    formula_trend <- " + .twm_T_X"
  } else {
    formula_trend <- ""
  }
  fe_str_eff <- fe_str

  # ---- Optional: covariate interactions (Wooldridge 2025, Procedure 5.1) -----
  # With unit FE, x_i main effects and d_g*x_i terms are absorbed.  What needs
  # adding: (a) treatment-cell × centred-cov interactions (δ_{g,s} parameters),
  # and (b) time × covariate interactions for conditional parallel trends.
  # Centering: ẋ_ig = x_i - x̄_g (cohort mean per Eq. 5.2).
  if (!is.null(covariate_chrs) && length(covariate_chrs) > 0L) {
    for (cv in covariate_chrs)
      if (!cv %in% names(data)) stop("Covariate '", cv, "' not found in data.")

    cov_mat <- as.matrix(data[, covariate_chrs, drop = FALSE])
    n_cov   <- ncol(cov_mat)

    # group_key: cohort value per obs (NA for never-treated → skipped / zero)
    # build_cov_interactions_cpp centres within each cohort group and
    # multiplies by ind_mat columns — replaces two nested R for-loops.
    ci_names <- character(K * n_cov)
    for (j in seq_len(n_cov))
      for (kk in seq_len(K))
        ci_names[(j - 1L) * K + kk] <- paste0(".twm_cov_X_",
                                               covariate_chrs[j], "__k__", kk)

    cov_int_mat            <- build_cov_interactions_cpp(
                               cov_mat, ind_mat, as.integer(timing_vec))
    colnames(cov_int_mat)  <- ci_names
    data$.twm_cov_X        <- cov_int_mat

    # Time × covariate terms for conditional parallel trends
    # i(time, x_j, ref=excl_t) creates (T-1) coefficients per covariate.
    excl_t     <- as.integer(cohorts[1L] + baseline)
    time_x_str <- paste(
      vapply(covariate_chrs, function(cv)
        sprintf("i(%s, %s, ref = %d)", time_chr, cv, excl_t),
        character(1L)),
      collapse = " + ")
    formula_cov <- paste0(" + .twm_cov_X + ", time_x_str)
  } else {
    formula_cov <- ""
  }

  fe_part     <- if (nzchar(fe_str_eff)) paste0(" | ", fe_str_eff) else ""
  formula_str <- paste0(outcome_chr, " ~ .twm_X",
                        formula_trend, formula_cov, fe_part)

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
  V_full     <- .model_vcov_full(model, vcov_type, cluster, vcov_args)
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
  # Uses aggregate_iw_cpp (RcppArmadillo) for the quadratic-form VCOV step.
  es <- .aggregate_iw(tau_gt, V_full, coef_names, cohort_sizes, min_t, max_t)

  if (nrow(es) == 0L)
    stop("TWM event-study aggregation produced no estimates.")

  # ---- Confidence intervals --------------------------------------------------
  conf.level <- sort(unique(conf.level))
  es <- .add_ci_columns(es, conf.level, se_col = "std_error")

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
