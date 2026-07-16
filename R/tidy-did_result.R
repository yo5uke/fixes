#' Tidy a `did_result` object
#'
#' @description
#' Returns a tidy data frame of model coefficients from a [run_did()] result:
#' all regressors (treatment and covariates), in the format expected by
#' `modelsummary::modelsummary()`. The table is read from the fit stored at
#' estimation time; the VCOV type chosen in [run_did()] (`vcov` / `cluster`)
#' is already reflected in the standard errors.
#'
#' @param x A `did_result` object returned by [run_did()].
#' @param conf.int Logical; add `conf.low`/`conf.high` columns? Default `FALSE`.
#' @param conf.level Confidence level for `conf.int`. Default `0.95`.
#'   Intervals use Student-t quantiles with the fit's degrees of freedom,
#'   matching `fixest`/`broom` conventions.
#' @param ... Unused; kept for compatibility with the [broom::tidy()] generic.
#'
#' @return A tibble with columns `term`, `estimate`, `std.error`,
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
  td <- x$coeftable
  if (isTRUE(conf.int)) {
    q <- if (!is.null(x$df.t) && is.finite(x$df.t)) {
      stats::qt(1 - (1 - conf.level) / 2, x$df.t)
    } else {
      stats::qnorm(1 - (1 - conf.level) / 2)
    }
    td$conf.low  <- td$estimate - q * td$std.error
    td$conf.high <- td$estimate + q * td$std.error
  }
  tibble::as_tibble(td)
}

#' Glance at a `did_result` object
#'
#' @description
#' Returns a single-row summary of model-level statistics from a [run_did()]
#' result: `r.squared`, `adj.r.squared`, `within.r.squared`, `sigma`,
#' `nobs`, `AIC`, `BIC`, and `logLik`, in the column layout of
#' [broom::glance()] on a `fixest` model.
#'
#' @param x A `did_result` object returned by [run_did()].
#' @param ... Unused; kept for compatibility with the [broom::glance()]
#'   generic.
#'
#' @return A one-row tibble of model-level statistics.
#'
#' @examples
#' \dontrun{
#' res <- run_did(df, outcome = y, treatment = D, fe = ~ id + year)
#' broom::glance(res)
#' }
#'
#' @exportS3Method broom::glance did_result
glance.did_result <- function(x, ...) {
  st <- x$fit_stats
  tibble::tibble(
    r.squared        = st$r.squared,
    adj.r.squared    = st$adj.r.squared,
    within.r.squared = st$within.r.squared,
    pseudo.r.squared = NA_real_,
    sigma            = st$sigma,
    nobs             = st$nobs,
    AIC              = st$AIC,
    BIC              = st$BIC,
    logLik           = st$logLik
  )
}
