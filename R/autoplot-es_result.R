#' Autoplot for event-study results
#'
#' @description
#' S3 method that plots an `es_result` (from [event_study()]).
#' It forwards arguments to [plot.es_result()].
#'
#' @param object An `es_result` returned by [event_study()].
#' @param ci_level Confidence level (numeric, e.g., 0.95).
#' @param type Plot type: `"ribbon"` (default) or `"errorbar"`.
#' @param ...  Additional styling arguments; see [plot.es_result()].
#'
#' @return A `ggplot` object.
#'
#' @examples
#' # res <- event_study(...)
#' # ggplot2::autoplot(res, ci_level = 0.95, type = "ribbon")
#'
#' @importFrom ggplot2 autoplot
#' @exportS3Method ggplot2::autoplot es_result
autoplot.es_result <- function(
  object,
  ci_level = 0.95,
  type = c("ribbon", "errorbar"),
  ...
) {
  type <- match.arg(type)
  .plot_es_impl(object, ci_level = ci_level, type = type, ...)
}
