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
#' @param show_simultaneous Logical; if \code{TRUE}, overlays the simultaneous bootstrap CI
#'   (lighter band, alpha 0.15) alongside the standard pointwise CI (alpha 0.3), with a
#'   legend distinguishing the two bands.  Requires \code{bootstrap = TRUE} in the
#'   originating \code{run_es()} call.  Default \code{FALSE}.
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
    theme_style = "bw",
    show_simultaneous = FALSE
) {
  if (!inherits(data, "es_result")) {
    warning("`data` is not class 'es_result'. Attempting to proceed.")
  }

  if (isTRUE(show_simultaneous)) {
    if (!all(c("conf_low_sim", "conf_high_sim") %in% names(data))) {
      stop("Simultaneous CIs not found. Re-run with bootstrap = TRUE in run_es().")
    }
  }

  ci_str <- sprintf("%.0f", ci_level * 100)
  conf_low_col  <- paste0("conf_low_",  ci_str)
  conf_high_col <- paste0("conf_high_", ci_str)
  if (!conf_low_col %in% names(data))  conf_low_col  <- "conf_low_95"
  if (!conf_high_col %in% names(data)) conf_high_col <- "conf_high_95"

  plot_data <- data

  # Build legend labels for simultaneous mode (must happen before ggplot() call
  # so the label columns exist in the data that gets stored inside `p`)
  if (isTRUE(show_simultaneous)) {
    boot_alpha <- attr(data, "boot_alpha")
    ci_pct_str <- sprintf("%.0f%%", (1 - if (!is.null(boot_alpha)) boot_alpha else 0.05) * 100)
    pw_label   <- paste0(ci_pct_str, " pointwise CI")
    sim_label  <- paste0(ci_pct_str, " simultaneous CI")
    plot_data$.pw_label  <- pw_label
    plot_data$.sim_label <- sim_label
  }

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$relative_time, y = .data$estimate, group = 1)) +
    ggplot2::geom_vline(xintercept = vline_val, linetype = "dashed", color = vline_color) +
    ggplot2::geom_hline(yintercept = hline_val, linetype = "dashed", color = hline_color) +
    ggplot2::geom_point(size = pointsize, color = color) +
    ggplot2::labs(x = "Relative Time to Treatment", y = sprintf("Estimate and %.0f%% CI", as.numeric(ci_str)))

  if (type == "ribbon") {
    if (isTRUE(show_simultaneous)) {
      # Simultaneous band first (wider, lighter) so pointwise sits on top
      p <- p +
        ggplot2::geom_ribbon(
          ggplot2::aes(ymin = .data[["conf_low_sim"]], ymax = .data[["conf_high_sim"]],
                       fill = .data[[".sim_label"]]),
          alpha = 0.15
        ) +
        ggplot2::geom_ribbon(
          ggplot2::aes(ymin = .data[[conf_low_col]], ymax = .data[[conf_high_col]],
                       fill = .data[[".pw_label"]]),
          alpha = 0.3
        ) +
        ggplot2::scale_fill_manual(
          name   = NULL,
          values = setNames(c(fill, fill), c(pw_label, sim_label)),
          breaks = c(pw_label, sim_label)
        ) +
        ggplot2::geom_line(linewidth = linewidth, color = color)
    } else {
      p <- p +
        ggplot2::geom_ribbon(
          ggplot2::aes(ymin = .data[[conf_low_col]], ymax = .data[[conf_high_col]]),
          fill = fill, alpha = alpha
        ) +
        ggplot2::geom_line(linewidth = linewidth, color = color)
    }
  } else if (type == "errorbar") {
    if (isTRUE(show_simultaneous)) {
      p <- p +
        ggplot2::geom_errorbar(
          ggplot2::aes(ymin = .data[["conf_low_sim"]], ymax = .data[["conf_high_sim"]],
                       color = .data[[".sim_label"]]),
          width = barwidth * 1.2, linewidth = linewidth * 0.8
        ) +
        ggplot2::geom_errorbar(
          ggplot2::aes(ymin = .data[[conf_low_col]], ymax = .data[[conf_high_col]],
                       color = .data[[".pw_label"]]),
          width = barwidth, linewidth = linewidth
        ) +
        ggplot2::scale_color_manual(
          name   = NULL,
          values = setNames(c(color, color), c(pw_label, sim_label)),
          breaks = c(pw_label, sim_label)
        )
    } else {
      p <- p +
        ggplot2::geom_errorbar(
          ggplot2::aes(ymin = .data[[conf_low_col]], ymax = .data[[conf_high_col]]),
          color = color, width = barwidth, linewidth = linewidth
        )
    }
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
