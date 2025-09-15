#' Plot event-study results with ribbons or error bars
#'
#' @param data An object of class \code{es_result} returned by \code{run_es()}.
#' @param ci_level Confidence level to display (e.g., 0.95).
#' @param type One of \code{"ribbon"} (default) or \code{"errorbar"}.
#' @param vline_val,hline_val Numeric locations for vertical/horizontal reference lines (default 0).
#' @param vline_color,hline_color Colors for reference lines.
#' @param linewidth,pointsize,alpha,barwidth Styling parameters for lines/points/bands/bars.
#' @param color,fill Optional, override line/point color and ribbon fill.
#' @param theme_style One of \code{"bw"}, \code{"minimal"}, or \code{"classic"} for ggplot theme.
#'
#' @return A \code{ggplot} object.
#' @export
#'
#' @examples
#' # Assuming `res <- run_es(...)`
#' # p <- plot_es(res, ci_level = 0.95, type = "ribbon")
#' # print(p)
plot_es <- function(
    data,
    ci_level   = 0.95,
    type       = "ribbon",
    vline_val  = 0,
    vline_color = "#000",
    hline_val  = 0,
    hline_color = "#000",
    linewidth  = 1,
    pointsize  = 2,
    alpha      = .2,
    barwidth   = .2,
    color      = "#B25D91FF",
    fill       = "#B25D91FF",
    theme_style = "bw"
) {
  if (!inherits(data, "es_result")) {
    warning("`data` is not class 'es_result'. Attempting to proceed.")
  }
  ci_str <- sprintf("%.0f", ci_level * 100)
  conf_low_col  <- paste0("conf_low_",  ci_str)
  conf_high_col <- paste0("conf_high_", ci_str)
  if (!conf_low_col %in% names(data))  conf_low_col  <- "conf_low_95"
  if (!conf_high_col %in% names(data)) conf_high_col <- "conf_high_95"

  plot_data <- data
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$relative_time, y = .data$estimate, group = 1)) +
    ggplot2::geom_vline(xintercept = vline_val, linetype = "dashed", color = vline_color) +
    ggplot2::geom_hline(yintercept = hline_val, linetype = "dashed", color = hline_color) +
    ggplot2::geom_point(size = pointsize, color = color) +
    ggplot2::labs(x = "Relative Time to Treatment", y = sprintf("Estimate and %.0f%% CI", as.numeric(ci_str)))

  if (type == "ribbon") {
    p <- p +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data[[conf_low_col]], ymax = .data[[conf_high_col]]),
        fill = fill, alpha = alpha
      ) +
      ggplot2::geom_line(linewidth = linewidth, color = color)
  } else if (type == "errorbar") {
    p <- p +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = .data[[conf_low_col]], ymax = .data[[conf_high_col]]),
        color = color, width = barwidth, linewidth = linewidth
      )
  } else {
    stop("Invalid `type`. Choose 'ribbon' or 'errorbar'.")
  }

  if (is.numeric(plot_data$relative_time)) {
    p <- p + ggplot2::scale_x_continuous(
      breaks = seq(floor(min(plot_data$relative_time, na.rm = TRUE)),
                   ceiling(max(plot_data$relative_time, na.rm = TRUE)), by = 1)
    )
  }

  theme_style <- match.arg(theme_style, c("bw","minimal","classic"))
  if (theme_style == "bw") {
    p <- p + ggplot2::theme_bw() + ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  } else if (theme_style == "minimal") {
    p <- p + ggplot2::theme_minimal() + ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  } else {
    p <- p + ggplot2::theme_classic()
  }

  p
}
