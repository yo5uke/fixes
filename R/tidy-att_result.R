#' Tidy an `att_result` object
#'
#' @description
#' Returns a tidy data frame of the aggregated ATT estimates from a
#' [calc_att()] result, in the column format expected by
#' `modelsummary::modelsummary()` (and therefore renderable with
#' `tinytable::tt()`).  This makes it easy to put overall, by-cohort, or
#' by-time ATTs into a publication table, and to stack several estimators
#' (e.g. [run_did()] TWFE, CS, and BJS) side by side in one table.
#'
#' The `term` column is derived from the aggregation type so that rows align
#' across models:
#' \itemize{
#'   \item `aggregation = "simple"` -> `"ATT"`
#'   \item `aggregation = "by_cohort"` -> `"Cohort <g>"`
#'   \item `aggregation = "by_time"` -> `"Time <t>"`
#' }
#'
#' @param x An `att_result` object returned by [calc_att()].
#' @param conf.int Logical; add `conf.low`/`conf.high` columns? Default `FALSE`.
#' @param conf.level Confidence level for `conf.int` (normal approximation).
#'   Default `0.95`.
#' @param ... Unused; for S3 generic compatibility.
#'
#' @return A data frame with columns `term`, `estimate`, `std.error`,
#'   `statistic`, `p.value` (and optionally `conf.low`, `conf.high`).
#'
#' @examples
#' \dontrun{
#' att <- calc_att(df, outcome = y, time = year, timing = g, unit = id,
#'                 estimator = "cs", aggregation = "by_cohort")
#' broom::tidy(att)
#'
#' # Compare estimators in one table:
#' twfe <- run_did(df, outcome = y, treatment = D, fe = ~ id + year)
#' cs   <- calc_att(df, outcome = y, time = year, timing = g, unit = id,
#'                  estimator = "cs")
#' bjs  <- calc_att(df, outcome = y, time = year, timing = g, unit = id,
#'                  estimator = "bjs")
#' modelsummary::modelsummary(list(TWFE = twfe, CS = cs, BJS = bjs))
#' }
#'
#' @importFrom stats qnorm
#' @exportS3Method broom::tidy att_result
tidy.att_result <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {
  aggregation <- attr(x, "aggregation")
  term <- switch(
    aggregation,
    simple    = rep("ATT", nrow(x)),
    by_cohort = paste0("Cohort ", x$group),
    by_time   = paste0("Time ",   x$group),
    as.character(x$group)
  )

  out <- data.frame(
    term      = term,
    estimate  = as.numeric(x$estimate),
    std.error = as.numeric(x$std.error),
    statistic = as.numeric(x$statistic),
    p.value   = as.numeric(x$p.value),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (isTRUE(conf.int)) {
    z <- stats::qnorm(1 - (1 - conf.level) / 2)
    out$conf.low  <- out$estimate - z * out$std.error
    out$conf.high <- out$estimate + z * out$std.error
  }
  out
}

#' Glance at an `att_result` object
#'
#' @description
#' Returns a one-row summary of an [calc_att()] result for the goodness-of-fit
#' block of a `modelsummary::modelsummary()` table.
#'
#' @param x An `att_result` object returned by [calc_att()].
#' @param ... Unused; for S3 generic compatibility.
#'
#' @return A one-row data frame with `nobs`, `n.units`, `n.treated`,
#'   `estimator`, and `aggregation`.
#'
#' @examples
#' \dontrun{
#' att <- calc_att(df, outcome = y, time = year, timing = g, unit = id)
#' broom::glance(att)
#' }
#'
#' @exportS3Method broom::glance att_result
glance.att_result <- function(x, ...) {
  data.frame(
    nobs        = as.integer(attr(x, "N")),
    n.units     = as.integer(attr(x, "N_units")),
    n.treated   = as.integer(attr(x, "N_treated")),
    estimator   = toupper(attr(x, "estimator")),
    aggregation = attr(x, "aggregation"),
    stringsAsFactors = FALSE
  )
}
