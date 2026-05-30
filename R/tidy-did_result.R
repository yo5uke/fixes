#' Tidy a `did_result` object
#'
#' @description
#' Returns a tidy data frame of model coefficients from a [run_did()] result.
#' Delegates to [broom::tidy.fixest()] on the underlying `fixest` model so
#' that all regressors (treatment and covariates) appear in the output — the
#' format expected by [modelsummary::modelsummary()].
#'
#' @param x A `did_result` object returned by [run_did()].
#' @param conf.int Logical; add `conf.low`/`conf.high` columns? Default `FALSE`.
#' @param conf.level Confidence level for `conf.int`. Default `0.95`.
#' @param ... Additional arguments passed to [broom::tidy.fixest()].
#'
#' @return A data frame with columns `term`, `estimate`, `std.error`,
#'   `statistic`, `p.value` (and optionally `conf.low`, `conf.high`).
#'
#' @examples
#' \dontrun{
#' res <- run_did(df, outcome = y, treatment = D, fe = ~ id + year)
#' broom::tidy(res)
#' broom::tidy(res, conf.int = TRUE)
#' }
#'
#' @exportS3Method broom::tidy did_result
tidy.did_result <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {
  vcov_type <- attr(x, "vcov_type")
  V <- tryCatch(
    stats::vcov(x$model, vcov = vcov_type),
    error = function(e) NULL
  )
  if (is.null(V)) {
    broom::tidy(x$model, conf.int = conf.int, conf.level = conf.level, ...)
  } else {
    broom::tidy(x$model, vcov = V, conf.int = conf.int, conf.level = conf.level, ...)
  }
}

#' Glance at a `did_result` object
#'
#' @description
#' Returns a single-row summary of model-level statistics from a [run_did()]
#' result.  Delegates to [broom::glance.fixest()] which provides
#' `nobs`, `r.squared`, `adj.r.squared`, `within.r.squared`, `AIC`, `BIC`,
#' and related statistics.
#'
#' @param x A `did_result` object returned by [run_did()].
#' @param ... Additional arguments passed to [broom::glance.fixest()].
#'
#' @return A one-row data frame of model-level statistics.
#'
#' @examples
#' \dontrun{
#' res <- run_did(df, outcome = y, treatment = D, fe = ~ id + year)
#' broom::glance(res)
#' }
#'
#' @exportS3Method broom::glance did_result
glance.did_result <- function(x, ...) {
  broom::glance(x$model, ...)
}
