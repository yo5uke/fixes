#' Extract per-unit propensity scores from a DiD result
#'
#' @description
#' Returns the fitted propensity scores stored on a result from
#' `did(estimator = "dr")`, or from `event_study(estimator = "cs")` /
#' `att(estimator = "cs")` when `keep_ps = TRUE` is supplied together with
#' covariates.
#'
#' @param x A `did_result`, `att_result`, or `es_result` object.
#'
#' @return A `data.frame` of fitted propensity scores:
#' - For `did(estimator = "dr")`: columns `unit`, `treated`, and
#'   `propensity_score`.
#' - For `att(estimator = "cs")` / `event_study(estimator = "cs")`: columns
#'   `g` (cohort), `t` (calendar time; `NA` when `control_group = "nevertreated"`
#'   because the propensity score does not vary over `t`), `unit`, `treated`,
#'   and `propensity_score`.
#'
#' @examples
#' \dontrun{
#' res <- did(y ~ D + x1 | id + year, data = df, time = year, estimator = "dr")
#' ps <- propensity_scores(res)
#'
#' cs <- att(y ~ x1 | id + year, data = df, time = year, timing = g,
#'           estimator = "cs", keep_ps = TRUE)
#' ps_cs <- propensity_scores(cs)
#' }
#'
#' @seealso [did()], [att()], [event_study()], [att_gt()]
#' @export
propensity_scores <- function(x) {
  ps <- attr(x, "propensity_scores")
  if (is.null(ps)) {
    if (inherits(x, "did_result")) {
      if (identical(attr(x, "est_method"), "reg")) {
        stop(
          "Propensity scores are not computed when `est_method = \"reg\"`.",
          call. = FALSE
        )
      }
      stop(
        "No propensity scores found on `x`. Use `did(..., estimator = \"dr\")` ",
        "with `est_method` in \"trad\", \"imp\", or \"ipw\".",
        call. = FALSE
      )
    }
    if (inherits(x, c("att_result", "es_result"))) {
      stop(
        "No propensity scores found on `x`. Use `estimator = \"cs\"` with ",
        "covariates, `est_method` in \"trad\", \"imp\", or \"ipw\", and ",
        "`keep_ps = TRUE`.",
        call. = FALSE
      )
    }
    stop("No propensity scores found on `x`.", call. = FALSE)
  }
  out <- as.data.frame(ps)
  rownames(out) <- NULL
  out
}
