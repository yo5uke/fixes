#' Autoplot for event-study results
#'
#' @description
#' S3 method that plots an `es_result` (from [run_es()]).
#' It forwards arguments to [plot_es()].
#'
#' @param object An `es_result` returned by [run_es()].
#' @param ci_level Confidence level (numeric, e.g., 0.95). Passed to [plot_es()].
#' @param type Plot type: `"ribbon"` (default) or `"errorbar"`. Passed to [plot_es()].
#' @param ...  Additional arguments forwarded to [plot_es()].
#'
#' @return A `ggplot` object.
#'
#' @examples
#' # res <- run_es(...)
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
  plot_es(object, ci_level = ci_level, type = type, ...)
}
