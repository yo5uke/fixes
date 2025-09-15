#' Autoplot for event-study results
#'
#' @description
#' S3 method that plots an \code{es_result} (from \code{run_es()}).
#' It forwards arguments to \code{\link{plot_es}}.
#'
#' @param object An \code{es_result} returned by \code{run_es()}.
#' @param ci_level Confidence level (numeric, e.g., 0.95). Passed to \code{plot_es()}.
#' @param type Plot type: \code{"ribbon"} (default) or \code{"errorbar"}. Passed to \code{plot_es()}.
#' @param ...  Additional arguments forwarded to \code{\link{plot_es}}.
#'
#' @return A \code{ggplot2} \code{ggplot} object.
#'
#' @examples
#' # res <- run_es(...)
#' # ggplot2::autoplot(res, ci_level = 0.95, type = "ribbon")
#'
#' @importFrom ggplot2 autoplot
#' @exportS3Method ggplot2::autoplot es_result
autoplot.es_result <- function(object, ci_level = 0.95, type = c("ribbon", "errorbar"), ...) {
  type <- match.arg(type)
  plot_es(object, ci_level = ci_level, type = type, ...)
}

