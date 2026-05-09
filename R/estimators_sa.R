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

  cohorts <- sort(unique(data[[timing_chr]][!is.na(data[[timing_chr]])]))
  if (length(cohorts) == 0L)
    stop("No treated units found: all '", timing_chr, "' values are NA.")

  never_units <- unique(data[[unit_chr]][is.na(data[[timing_chr]])])
  n_never     <- length(never_units)

  cohort_sizes <- vapply(cohorts, function(g) {
    length(unique(data[[unit_chr]][
      !is.na(data[[timing_chr]]) & data[[timing_chr]] == g
    ]))
  }, integer(1L))
  names(cohort_sizes) <- as.character(cohorts)

  # ---- Build cohort x relative-time interaction indicators ------------------
  # SA (2021) eq. (1): create D_{g,l}(i,t) = 1(G_i=g) * 1(t-G_i=l)
  # Only for feasible (g,l) pairs: g+l in [min_t, max_t] and l != baseline.
  # Column names: .sa__{g}__{safe_l}  where safe_l = "neg{|l|}" for l<0.

  int_cols  <- character(0)
  catt_meta <- list()   # list of (col, g, l) for later extraction

  for (g in cohorts) {
    feasible_l <- all_periods - g          # relative times with g+l in sample
    feasible_l <- feasible_l[feasible_l != baseline]

    g_mask <- !is.na(data[[timing_chr]]) & data[[timing_chr]] == g

    for (l in feasible_l) {
      safe_l <- if (l < 0L) paste0("neg", -l) else as.character(l)
      col    <- paste0(".sa__", g, "__", safe_l)

      data[[col]] <- as.integer(
        g_mask & (data[[time_chr]] - data[[timing_chr]]) == l
      )
      int_cols <- c(int_cols, col)
      catt_meta[[length(catt_meta) + 1L]] <- list(col = col, g = g, l = l)
    }
  }

  if (length(int_cols) == 0L)
    stop("No cohort-by-period interactions could be constructed. ",
         "Check that the timing column, time column, and baseline are consistent.")

  # ---- Run TWFE regression --------------------------------------------------
  rhs_str     <- paste(int_cols, collapse = " + ")
  formula_str <- if (nzchar(fe_str)) {
    paste0(outcome_chr, " ~ ", rhs_str, " | ", fe_str)
  } else {
    paste0(outcome_chr, " ~ ", rhs_str)
  }

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
  if (!is.null(cluster) && identical(vcov_type, "HC1")) {
    V_full <- stats::vcov(model)          # model default = clustered SE
  } else {
    V_full <- tryCatch(
      stats::vcov(model, vcov = vcov_type, .vcov_args = vcov_args),
      error = function(e) stats::vcov(model)
    )
  }
  coef_names <- rownames(V_full)

  tidy_df    <- broom::tidy(model, vcov = V_full)

  # ---- Extract CATT(g,l) point estimates ------------------------------------
  catt_rows <- list()
  for (m in catt_meta) {
    idx <- match(m$col, tidy_df$term)
    if (is.na(idx)) next     # dropped as collinear by fixest — skip
    catt_rows[[length(catt_rows) + 1L]] <- data.frame(
      g         = m$g,
      l         = m$l,
      estimate  = tidy_df$estimate[idx],
      std_error = tidy_df$std.error[idx],
      col_name  = m$col,
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
  #
  # Var(theta_es(l)) = w(l)' * Sigma_l * w(l)
  #   Sigma_l = VCOV sub-block for {delta_{g,l}: g with valid estimate at l}
  # Cohorts with g+l inside sample but whose CATT was dropped by fixest
  # retain zero weight in the numerator but still contribute to the denominator
  # (consistent with the population weight definition).

  event_times <- sort(unique(catt_df$l))
  es_rows     <- list()

  for (l in event_times) {
    sub    <- catt_df[catt_df$l == l, ]

    # Map each CATT column to its position in the VCOV matrix
    idx_in_V <- match(sub$col_name, coef_names)
    valid    <- !is.na(idx_in_V)
    if (!any(valid)) next

    sub_v    <- sub[valid, ]
    idx_v    <- idx_in_V[valid]

    # Denominator: ALL cohorts with g+l in sample (population weight definition)
    in_samp  <- cohorts[(cohorts + l) >= min_t & (cohorts + l) <= max_t]
    sz_denom <- sum(cohort_sizes[as.character(in_samp)])
    if (sz_denom == 0L) next

    w        <- cohort_sizes[as.character(sub_v$g)] / sz_denom

    # Weighted point estimate
    theta    <- sum(w * sub_v$estimate)

    # Variance: full quadratic form over the VCOV sub-block
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
    stop("SA event-study aggregation produced no estimates.")

  es           <- do.call(rbind, es_rows)
  es           <- es[order(es$relative_time), ]
  rownames(es) <- NULL

  # ---- Confidence intervals -------------------------------------------------
  conf.level <- sort(unique(conf.level))
  for (cl in conf.level) {
    z   <- stats::qnorm(1 - (1 - cl) / 2)
    suf <- sprintf("%.0f", cl * 100)
    es[[paste0("conf_low_",  suf)]] <- es$estimate - z * es$std_error
    es[[paste0("conf_high_", suf)]] <- es$estimate + z * es$std_error
  }

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
