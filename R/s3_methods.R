#' Print method for event study results
#'
#' @description
#' Displays summary statistics and model metadata from an \code{es_result} object.
#' Shows 95% confidence intervals by default when displaying estimates.
#'
#' @param x An \code{es_result} object returned by \code{run_es()}.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns \code{x}.
#' @export
print.es_result <- function(x, ...) {
  cat("Event Study Result (fixes)\n")
  n  <- attr(x, "N"); nu <- attr(x, "N_units")
  nt <- attr(x, "N_treated"); nn <- attr(x, "N_nevertreated")
  fe <- attr(x, "fe"); vc <- attr(x, "vcov_type")
  cl <- attr(x, "cluster_vars"); su <- attr(x, "sunab_used")
  lg <- attr(x, "lead_range"); rg <- attr(x, "lag_range"); bs <- attr(x, "baseline")
  conf_levels <- attr(x, "conf.level")

  cat("  N:", n, " | Units:", nu,
      " | Treated units:", nt, " | Never-treated:", nn, "\n")
  cat("  FE: ", fe, "\n", sep = "")
  cat("  VCOV:", vc, " | Cluster:", paste(if (is.null(cl)) "-" else cl, collapse = " + "), "\n")
  if (isTRUE(su)) {
    cat("  Method: SUNAB (staggered-safe)\n")
  } else {
    cat("  Method: classic  | lead_range:", lg, " lag_range:", rg, " baseline:", bs, "\n")
  }

  # Note about multiple CI levels
  if (!is.null(conf_levels) && length(conf_levels) > 1) {
    ci_pct <- sprintf("%.0f%%", conf_levels * 100)
    cat("  Note: Multiple confidence levels calculated (", paste(ci_pct, collapse = ", "), ").\n", sep = "")
    cat("        Use plot_es_interactive() for dynamic CI switching.\n")
  }

  invisible(x)
}

#' Autoplot method for event study results
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
