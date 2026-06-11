# --------------------------------------------------------------------------- #
#  Shared internal helpers (not exported)                                       #
#                                                                               #
#  These consolidate boilerplate that was previously duplicated across          #
#  run_es(), calc_att(), run_did(), and the five estimator backends             #
#  (estimators_cs/sa/bjs/twm/flex).  Each helper is behaviour-preserving: it    #
#  reproduces the exact computation (and error messages) of the call sites it   #
#  replaces.                                                                     #
# --------------------------------------------------------------------------- #

# Resolve an NSE column expression to a character column name (or expression
# text).  Unifies the three formerly-identical resolvers in run_es(),
# calc_att(), and run_did().
#
# expr       : result of rlang::enexpr() — unevaluated user input
# data       : data.frame to check column existence against
# allow_call : if TRUE, expressions like log(y) are accepted as-is (text)
.resolve_col <- function(expr, data, allow_call = FALSE) {
  if (rlang::is_symbol(expr)) {
    var <- rlang::as_string(expr)
    if (!var %in% names(data)) stop("Column '", var, "' not found in data.")
    return(var)
  } else if (allow_call && rlang::is_call(expr)) {
    return(rlang::expr_text(expr))
  } else if (is.character(expr) && length(expr) == 1L && expr %in% names(data)) {
    return(expr)
  } else {
    stop("Invalid column/expression: ", rlang::expr_text(expr))
  }
}

# Append symmetric normal-approximation CI columns (conf_low_XX / conf_high_XX)
# for each requested confidence level.  Iterates `conf.level` in the order
# given (callers pass an already-sorted vector), preserving column order.
#
# df         : data.frame with point-estimate and SE columns
# conf.level : numeric vector of confidence levels
# est_col    : name of the point-estimate column (default "estimate")
# se_col     : name of the standard-error column (default "std_error")
.add_ci_columns <- function(df, conf.level, est_col = "estimate",
                            se_col = "std_error") {
  est <- df[[est_col]]
  se  <- df[[se_col]]
  for (cl in conf.level) {
    z   <- stats::qnorm(1 - (1 - cl) / 2)
    suf <- sprintf("%.0f", cl * 100)
    df[[paste0("conf_low_",  suf)]] <- est - z * se
    df[[paste0("conf_high_", suf)]] <- est + z * se
  }
  df
}

# Validate that the required columns exist and that the time column is numeric.
# Reproduces the validation block shared by all five estimator backends.
.validate_panel_cols <- function(data, cols, time_chr) {
  for (col in cols) {
    if (!col %in% names(data))
      stop("Column '", col, "' not found in data.")
  }
  if (!is.numeric(data[[time_chr]]))
    stop("'", time_chr, "' must be numeric.")
}

# Assert that a numeric vector is integer-valued (NAs allowed).  The CS, SA,
# TWM, and FLEX backends coerce time/timing with as.integer(), which silently
# truncates fractional values and would misassign treatment cells.
.assert_integerish <- function(v, name) {
  vv <- v[!is.na(v)]
  if (length(vv) > 0L && is.numeric(vv) && any(vv != round(vv)))
    stop("'", name, "' must be integer-valued for this estimator ",
         "(fractional values would be truncated when building treatment ",
         "cells). Rescale it to integers, e.g. via a consecutive time index.")
}

# Cohort sizes = number of unique units per cohort.  Returns a named integer
# vector keyed by cohort (as character).  Used by CS, SA, and TWM (FLEX counts
# unique groups instead and keeps its own logic).
.compute_cohort_sizes <- function(data, timing_chr, unit_chr, cohorts) {
  tv <- data[[timing_chr]]
  uv <- data[[unit_chr]]
  cs <- vapply(cohorts, function(g) {
    length(unique(uv[!is.na(tv) & tv == g]))
  }, integer(1L))
  names(cs) <- as.character(cohorts)
  cs
}

# Extract the full coefficient VCOV from a fixest model, honouring the same
# cluster/HC1 precedence used by SA, TWM, and FLEX:
#   - cluster supplied + vcov left at default "HC1"  -> model's clustered VCOV
#   - otherwise -> requested vcov_type (falling back to model default on error)
.model_vcov_full <- function(model, vcov_type, cluster, vcov_args) {
  if (!is.null(cluster) && identical(vcov_type, "HC1")) {
    stats::vcov(model)
  } else {
    tryCatch(
      stats::vcov(model, vcov = vcov_type, .vcov_args = vcov_args),
      error = function(e) stats::vcov(model)
    )
  }
}

# Build the full covariance matrix of the event-study coefficients, ordered by
# relative time, for downstream sensitivity analysis (honest_sensitivity()).
#
# V_full    : full coefficient covariance matrix (rownames = coefficient terms)
# terms     : character vector of the model coefficient names for the kept
#             (non-baseline) event-study coefficients
# rel_times : their relative-time values (integer)
#
# Returns a square numeric matrix with dimnames = relative times (as character),
# ordered by relative time, or NULL if no event-study coefficient could be
# matched into V_full.
.build_es_vcov <- function(V_full, terms, rel_times) {
  if (is.null(V_full)) return(NULL)
  keep <- !is.na(rel_times) & terms %in% rownames(V_full)
  if (!any(keep)) return(NULL)
  t_in <- terms[keep]
  r_in <- rel_times[keep]
  ord  <- order(r_in)
  t_in <- t_in[ord]
  r_in <- r_in[ord]
  M <- V_full[t_in, t_in, drop = FALSE]
  dimnames(M) <- list(as.character(r_in), as.character(r_in))
  M
}

# Interaction-weighted (IW) event-study aggregation shared by SA, TWM, FLEX.
# Builds the 0-based VCOV index vector and calls aggregate_iw_cpp().
#
# coef_df      : data.frame with columns estimate, l, g, col_name
# V_full       : full coefficient covariance matrix
# coef_names   : rownames(V_full)
# cohort_sizes : named integer vector (cohort -> size)
# min_t, max_t : scalar calendar-time bounds
.aggregate_iw <- function(coef_df, V_full, coef_names, cohort_sizes,
                          min_t, max_t) {
  idx_V <- match(coef_df$col_name, coef_names) - 1L  # 0-based
  idx_V[is.na(idx_V)] <- -1L

  aggregate_iw_cpp(
    estimates   = coef_df$estimate,
    l_vals      = as.integer(coef_df$l),
    cohort_vals = as.integer(coef_df$g),
    idx_in_V    = idx_V,
    V_full_r    = V_full,
    unique_l    = as.integer(sort(unique(coef_df$l))),
    cs_keys     = as.integer(names(cohort_sizes)),
    cs_vals     = as.integer(cohort_sizes),
    min_t       = as.integer(min_t),
    max_t       = as.integer(max_t)
  )
}
