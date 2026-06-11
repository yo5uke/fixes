#' Deb, Norton, Wooldridge & Zabel (2024) FLEX Estimator for Repeated Cross-Sections
#'
#' @description
#' Implements the FLEX (Flexible Linear Model Estimated by OLS) estimator from
#' Deb et al. (2024) for difference-in-differences with \strong{repeated
#' cross-section} (RCS) data.  Unlike panel estimators, FLEX does not require
#' the same individuals to be observed across time periods.
#'
#' The (no-covariate) regression is (Deb et al. 2024, eq. 3.1 simplified):
#' \deqn{Y_{i,t} = \sum_{g,s \neq c_0(g)} \tau_{g,s}
#'   \cdot R_{ig} \cdot P_{i,t}
#'   + \sum_g \alpha_g R_{ig}
#'   + \sum_t \lambda_t P_{i,t}
#'   + \varepsilon_{i,t}}
#' where \eqn{R_{ig} = \mathbf{1}(\text{group}_i = g)} identifies which treatment
#' group (cohort) individual \eqn{i} belongs to, and \eqn{P_{i,t} = 1} if
#' individual \eqn{i} is observed at time \eqn{t}.  The excluded calendar period
#' is \eqn{c_0(g) = g + \text{baseline}} per cohort.
#'
#' @section Key differences from panel estimators:
#' \itemize{
#'   \item No unit fixed effect \eqn{\alpha_i} (different individuals each period)
#'   \item Group FE \eqn{\alpha_g} (cohort-level) replaces unit FE
#'   \item Cohort size \eqn{n_g} = number of unique groups in cohort \eqn{g}
#'     (not observations)
#'   \item Default clustering: by \code{group_chr} (recommended for RCS)
#' }
#'
#' @section Algebraic equivalence (Proposition 2.1):
#' FLEX OLS estimates \eqn{\hat\tau_{g,s}} are algebraically identical to the
#' multi-step imputation estimator that (1) estimates group+time FE on untreated
#' observations, (2) imputes counterfactuals, and (3) averages residuals by
#' (group, time) cell.  Standard OLS standard errors from \code{fixest::feols()}
#' apply directly.
#'
#' @param data data.frame with one row per individual-period observation.
#'   RCS assumption: each individual appears at most once per period.
#' @param outcome_chr Column name (character) for the outcome variable.
#' @param timing_chr Column name (character) for first treatment period;
#'   \code{NA} marks never-treated groups.  Must be constant within each group.
#' @param time_chr Column name (character) for calendar time (numeric).
#' @param group_chr Column name (character) for the group identifier
#'   (\eqn{R_{ig}} in Deb et al.).  Each group must map to exactly one timing value.
#' @param baseline Integer reference (base) relative period, default \code{-1L}.
#'   The excluded calendar period for cohort \eqn{g} is \eqn{g + \text{baseline}}.
#' @param cluster Cluster specification forwarded to \code{fixest::feols()}.
#'   Defaults to clustering by \code{group_chr} if \code{NULL}.
#' @param vcov_type VCOV type string, e.g. \code{"HC1"} (default).
#' @param vcov_args List of extra arguments forwarded to
#'   \code{fixest::vcov()}.
#' @param conf.level Numeric confidence level(s), default \code{0.95}.
#'
#' @return A list with:
#' \describe{
#'   \item{\code{es}}{data.frame of FLEX event-study estimates.}
#'   \item{\code{tau_gt}}{data.frame of raw \eqn{\tau_{g,s}} estimates:
#'     \code{g}, \code{t} (calendar), \code{l} (= t - g), \code{estimate},
#'     \code{std_error}, \code{col_name}.}
#'   \item{\code{cohorts}}{Sorted numeric vector of treatment cohort values.}
#'   \item{\code{n_never_groups}}{Number of never-treated groups.}
#'   \item{\code{cohort_sizes}}{Named integer vector: number of groups per cohort.}
#'   \item{\code{n_obs}}{Number of observations used in the regression.}
#'   \item{\code{formula_str}}{Character string of the estimated formula.}
#' }
#' @noRd
.run_flex <- function(data,
                      outcome_chr,
                      timing_chr,
                      time_chr,
                      group_chr,
                      baseline       = -1L,
                      covariate_chrs = NULL,
                      cluster        = NULL,
                      vcov_type      = "HC1",
                      vcov_args      = list(),
                      conf.level     = 0.95) {

  baseline <- as.integer(baseline)

  # ---- Validate inputs -------------------------------------------------------
  .validate_panel_cols(data, c(outcome_chr, timing_chr, time_chr, group_chr), time_chr)

  # ---- Group → cohort mapping (one timing value per group) -------------------
  gc_pairs <- unique(data[, c(group_chr, timing_chr), drop = FALSE])
  # Detect groups with multiple treatment dates
  dup_groups <- gc_pairs[[group_chr]][duplicated(gc_pairs[[group_chr]])]
  if (length(dup_groups) > 0L)
    stop("Groups with multiple treatment dates: ",
         paste(unique(dup_groups), collapse = ", "),
         ". Each group must have a single unique timing value.")

  # ---- Bookkeeping -----------------------------------------------------------
  data        <- data[order(data[[group_chr]], data[[time_chr]]), ]
  all_periods <- sort(unique(data[[time_chr]]))
  min_t       <- min(all_periods)
  max_t       <- max(all_periods)

  timing_vec <- data[[timing_chr]]
  group_vec  <- data[[group_chr]]

  # All cohort (treatment date) values — from the group-level mapping
  all_groups     <- unique(gc_pairs[[group_chr]])
  timing_by_group <- setNames(gc_pairs[[timing_chr]],
                              as.character(gc_pairs[[group_chr]]))
  cohorts <- sort(unique(timing_by_group[!is.na(timing_by_group)]))

  if (length(cohorts) == 0L)
    stop("No treated groups found: all '", timing_chr, "' values are NA.")

  # Cohort size = number of UNIQUE GROUPS in cohort g (not observations)
  cohort_sizes <- vapply(cohorts, function(g) {
    sum(!is.na(timing_by_group) & timing_by_group == g)
  }, integer(1L))
  names(cohort_sizes) <- as.character(cohorts)

  never_groups   <- names(timing_by_group)[is.na(timing_by_group)]
  n_never_groups <- length(never_groups)

  # Default cluster: by group (recommended for RCS per Deb et al.)
  if (is.null(cluster)) {
    warning("No `cluster` specified; defaulting to clustering by '", group_chr,
            "' (recommended for RCS data).")
    cluster <- stats::as.formula(paste0("~ ", group_chr))
  }

  # ---- Build group x calendar-time indicator matrix --------------------------
  # FLEX eq. (3.1, no-covariate): T_{g,s}(i) = R_{ig} * P_{i,s}
  #   = 1{group_i belongs to cohort g} * 1{time_i == s}
  # For each cohort g, exclude calendar period c_0(g) = g + baseline.
  #
  # Same pre-allocation strategy as SA and TWM: integer matrix stored as a
  # single matrix column in data (.flex_X); feols expands internally.
  # feols prepends ".flex_X" → coefficient ".flex_X.flex__g__2__t__1"

  .assert_integerish(data[[time_chr]], time_chr)
  .assert_integerish(data[[timing_chr]], timing_chr)

  if (!any((cohorts + baseline) %in% all_periods))
    warning("Excluded calendar period g + baseline (baseline = ", baseline,
            ") is not observed for any cohort, so no reference period was ",
            "excluded. Check the time grid spacing and the `baseline` ",
            "argument.", call. = FALSE)

  # All feasible (g, s) pairs
  gs_pairs <- do.call(rbind, lapply(cohorts, function(g) {
    excl_s <- g + baseline
    fs     <- all_periods[all_periods != excl_s]
    if (length(fs) == 0L) return(NULL)
    # Groups belonging to cohort g
    data.frame(g = g,
               s = fs,
               stringsAsFactors = FALSE)
  }))

  if (is.null(gs_pairs) || nrow(gs_pairs) == 0L)
    stop("No cohort-by-period interactions could be constructed. ",
         "Check timing, time, and baseline.")

  K         <- nrow(gs_pairs)
  N         <- nrow(data)
  col_names <- character(K)
  k <- 0L
  for (g in cohorts) {
    for (j in which(gs_pairs$g == g)) {
      s <- gs_pairs$s[j]
      k <- k + 1L
      s_safe       <- if (s < 0L) paste0("neg", -s) else as.character(s)
      col_names[k] <- paste0(".flex__g__", g, "__t__", s_safe)
    }
  }

  # FLEX: map each observation's group → cohort value (NA for never-treated)
  cohort_of_obs <- as.integer(timing_by_group[as.character(group_vec)])

  # Build indicator matrix via Rcpp (shared with SA and TWM).
  # cohort_of_obs plays the role of cohort_id (NA_integer_ = never-treated).
  ind_mat <- build_indicator_matrix_cpp(
    cohort_id = cohort_of_obs,
    time_id   = as.integer(data[[time_chr]]),
    gs_g      = as.integer(gs_pairs$g),
    gs_s      = as.integer(gs_pairs$s)
  )
  colnames(ind_mat) <- col_names

  # feols naming rule for matrix regressors:
  #   K > 1:  coefficient = "<matrix_name><col_name>"  (e.g. ".flex_X.flex__g__2__t__2")
  #   K == 1: coefficient = "<matrix_name>"            (e.g. ".flex_X")
  tau_meta <- lapply(seq_len(K), function(k)
    list(col = if (K == 1L) ".flex_X" else paste0(".flex_X", col_names[k]),
         g   = gs_pairs$g[k],
         s   = gs_pairs$s[k],
         l   = gs_pairs$s[k] - gs_pairs$g[k]))

  data$.flex_X <- ind_mat

  # ---- Optional: covariate interactions (Deb et al. 2024, Eq. 3.1) -----------
  # Centering by (group, time) cell per Eq. 2.11: X̄_{g,t} = mean(X_{i,t}) for
  # obs in group g at time t.  Treatment interaction columns: R_{ig}*P_{i,t}*
  # (X_{i,t} - X̄_{g,t}).  Also add i(time,x) and i(group,x) for conditional PT.
  if (!is.null(covariate_chrs) && length(covariate_chrs) > 0L) {
    for (cv in covariate_chrs)
      if (!cv %in% names(data)) stop("Covariate '", cv, "' not found in data.")

    cov_mat <- as.matrix(data[, covariate_chrs, drop = FALSE])
    n_cov   <- ncol(cov_mat)

    # cell_key: unique integer per (cohort, time) cell; NA for never-treated.
    # build_cov_interactions_cpp centres within each cell and multiplies by
    # ind_mat — replaces the triple nested R for-loop over cohorts × periods × covariates.
    cohort_chr <- ifelse(is.na(cohort_of_obs), "NA", as.character(cohort_of_obs))
    cell_key   <- as.integer(factor(paste(cohort_chr,
                                          as.character(data[[time_chr]]),
                                          sep = "_")))
    cell_key[is.na(cohort_of_obs)] <- NA_integer_

    ci_names <- character(K * n_cov)
    for (j in seq_len(n_cov))
      for (kk in seq_len(K))
        ci_names[(j - 1L) * K + kk] <- paste0(".flex_cov_X_",
                                               covariate_chrs[j], "__k__", kk)

    cov_int_mat           <- build_cov_interactions_cpp(
                              cov_mat, ind_mat, cell_key)
    colnames(cov_int_mat) <- ci_names
    data$.flex_cov_X      <- cov_int_mat

    excl_t    <- as.integer(cohorts[1L] + baseline)
    ref_group <- sort(unique(group_vec))[1L]
    time_x_str <- paste(
      vapply(covariate_chrs, function(cv)
        sprintf("i(%s, %s, ref = %d)", time_chr, cv, excl_t),
        character(1L)),
      collapse = " + ")
    grp_x_str  <- paste(
      vapply(covariate_chrs, function(cv)
        sprintf("i(%s, %s, ref = %s)", group_chr, cv,
                as.character(ref_group)),
        character(1L)),
      collapse = " + ")
    formula_cov <- paste0(" + .flex_cov_X + ", time_x_str, " + ", grp_x_str)
  } else {
    formula_cov <- ""
  }

  # FE: group + time (not unit + time, since RCS has no unit tracking)
  fe_str_flex <- paste(group_chr, time_chr, sep = " + ")
  formula_str <- paste0(outcome_chr, " ~ .flex_X", formula_cov, " | ", fe_str_flex)

  # ---- Run regression --------------------------------------------------------
  model_args <- list(stats::as.formula(formula_str), data = data)
  model_args$cluster <- cluster

  model <- tryCatch(
    do.call(fixest::feols, model_args),
    error = function(e)
      stop("FLEX regression failed: ", e$message,
           "\nCheck for collinearity or too few observations per cell.")
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
    stop("No tau_{g,s} coefficients could be extracted from the FLEX regression.")

  tau_gt           <- do.call(rbind, tau_rows)
  tau_gt$col_name  <- as.character(tau_gt$col_name)
  rownames(tau_gt) <- NULL

  # ---- IW aggregation — cohort-size-weighted (n_g = unique groups per cohort)
  # Uses aggregate_iw_cpp (RcppArmadillo) for the quadratic-form VCOV step.
  es <- .aggregate_iw(tau_gt, V_full, coef_names, cohort_sizes, min_t, max_t)

  if (nrow(es) == 0L)
    stop("FLEX event-study aggregation produced no estimates.")

  # ---- Confidence intervals --------------------------------------------------
  conf.level <- sort(unique(conf.level))
  es <- .add_ci_columns(es, conf.level, se_col = "std_error")

  list(
    es             = es,
    tau_gt         = tau_gt,
    cohorts        = cohorts,
    n_never_groups = n_never_groups,
    cohort_sizes   = cohort_sizes,
    n_obs          = stats::nobs(model),
    formula_str    = formula_str
  )
}
