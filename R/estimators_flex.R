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
                      baseline   = -1L,
                      cluster    = NULL,
                      vcov_type  = "HC1",
                      vcov_args  = list(),
                      conf.level = 0.95) {

  baseline <- as.integer(baseline)

  # ---- Validate inputs -------------------------------------------------------
  for (col in c(outcome_chr, timing_chr, time_chr, group_chr)) {
    if (!col %in% names(data))
      stop("Column '", col, "' not found in data.")
  }
  if (!is.numeric(data[[time_chr]]))
    stop("'", time_chr, "' must be numeric.")

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

  K       <- nrow(gs_pairs)
  N       <- nrow(data)
  ind_mat <- matrix(0L, nrow = N, ncol = K)
  col_names <- character(K)

  k <- 0L
  for (g in cohorts) {
    # All group IDs that belong to cohort g
    groups_in_g <- names(timing_by_group)[!is.na(timing_by_group) &
                                           timing_by_group == g]
    # Row mask: individual belongs to one of groups_in_g
    g_mask <- group_vec %in% groups_in_g

    for (j in which(gs_pairs$g == g)) {
      s <- gs_pairs$s[j]
      k <- k + 1L
      s_safe       <- if (s < 0L) paste0("neg", -s) else as.character(s)
      col_names[k] <- paste0(".flex__g__", g, "__t__", s_safe)
      ind_mat[, k] <- as.integer(g_mask & data[[time_chr]] == s)
    }
  }
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

  # FE: group + time (not unit + time, since RCS has no unit tracking)
  fe_str_flex <- paste(group_chr, time_chr, sep = " + ")
  formula_str <- paste0(outcome_chr, " ~ .flex_X | ", fe_str_flex)

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
    stop("No tau_{g,s} coefficients could be extracted from the FLEX regression.")

  tau_gt           <- do.call(rbind, tau_rows)
  tau_gt$col_name  <- as.character(tau_gt$col_name)
  rownames(tau_gt) <- NULL

  # ---- IW aggregation — cohort-size-weighted average over groups at each l --
  # theta_es(l) = sum_g  tau_{g, g+l} * w(g,l)
  # w(g,l)      = n_g / sum_{g': g'+l in [min_t, max_t]} n_{g'}
  # where n_g = number of groups in cohort g
  # Var(theta_es(l)) = w(l)' * Sigma_l * w(l)  [quadratic form]

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
    stop("FLEX event-study aggregation produced no estimates.")

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
    es             = es,
    tau_gt         = tau_gt,
    cohorts        = cohorts,
    n_never_groups = n_never_groups,
    cohort_sizes   = cohort_sizes,
    n_obs          = stats::nobs(model),
    formula_str    = formula_str
  )
}
