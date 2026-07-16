# Static event-study plot (ribbon or errorbar). Shared engine behind
# plot.es_result() and the deprecated plot_es().
.plot_es_impl <- function(
  data,
  ci_level = 0.95,
  type = "ribbon",
  vline_val = 0,
  vline_color = "#000",
  hline_val = 0,
  hline_color = "#000",
  linewidth = 1,
  pointsize = 2,
  alpha = .2,
  barwidth = .2,
  color = "#B25D91FF",
  fill = "#B25D91FF",
  theme_style = "bw",
  show_simultaneous = FALSE
) {
  if (!inherits(data, "es_result")) {
    warning("`data` is not class 'es_result'. Attempting to proceed.")
  }

  if (isTRUE(show_simultaneous)) {
    if (!all(c("conf_low_sim", "conf_high_sim") %in% names(data))) {
      stop(
        "Simultaneous CIs not found. Re-run with bootstrap = TRUE in event_study()."
      )
    }
  }

  ci_str <- sprintf("%.0f", ci_level * 100)
  conf_low_col <- paste0("conf_low_", ci_str)
  conf_high_col <- paste0("conf_high_", ci_str)
  if (!conf_low_col %in% names(data)) {
    conf_low_col <- "conf_low_95"
  }
  if (!conf_high_col %in% names(data)) {
    conf_high_col <- "conf_high_95"
  }

  plot_data <- data

  # Build legend labels for simultaneous mode (must happen before ggplot() call
  # so the label columns exist in the data that gets stored inside `p`)
  if (isTRUE(show_simultaneous)) {
    boot_alpha <- attr(data, "boot_alpha")
    ci_pct_str <- sprintf(
      "%.0f%%",
      (1 - if (!is.null(boot_alpha)) boot_alpha else 0.05) * 100
    )
    pw_label <- paste0(ci_pct_str, " pointwise CI")
    sim_label <- paste0(ci_pct_str, " simultaneous CI")
    plot_data$.pw_label <- pw_label
    plot_data$.sim_label <- sim_label
  }

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$relative_time, y = .data$estimate, group = 1)
  ) +
    ggplot2::geom_vline(
      xintercept = vline_val,
      linetype = "dashed",
      color = vline_color
    ) +
    ggplot2::geom_hline(
      yintercept = hline_val,
      linetype = "dashed",
      color = hline_color
    ) +
    ggplot2::geom_point(size = pointsize, color = color) +
    ggplot2::labs(
      x = "Relative Time to Treatment",
      y = sprintf("Estimate and %.0f%% CI", as.numeric(ci_str))
    )

  if (type == "ribbon") {
    if (isTRUE(show_simultaneous)) {
      # Simultaneous band first (wider, lighter) so pointwise sits on top
      p <- p +
        ggplot2::geom_ribbon(
          ggplot2::aes(
            ymin = .data[["conf_low_sim"]],
            ymax = .data[["conf_high_sim"]],
            fill = .data[[".sim_label"]]
          ),
          alpha = 0.15
        ) +
        ggplot2::geom_ribbon(
          ggplot2::aes(
            ymin = .data[[conf_low_col]],
            ymax = .data[[conf_high_col]],
            fill = .data[[".pw_label"]]
          ),
          alpha = 0.3
        ) +
        ggplot2::scale_fill_manual(
          name = NULL,
          values = setNames(c(fill, fill), c(pw_label, sim_label)),
          breaks = c(pw_label, sim_label)
        ) +
        ggplot2::geom_line(linewidth = linewidth, color = color)
    } else {
      p <- p +
        ggplot2::geom_ribbon(
          ggplot2::aes(
            ymin = .data[[conf_low_col]],
            ymax = .data[[conf_high_col]]
          ),
          fill = fill,
          alpha = alpha
        ) +
        ggplot2::geom_line(linewidth = linewidth, color = color)
    }
  } else if (type == "errorbar") {
    if (isTRUE(show_simultaneous)) {
      p <- p +
        ggplot2::geom_errorbar(
          ggplot2::aes(
            ymin = .data[["conf_low_sim"]],
            ymax = .data[["conf_high_sim"]],
            color = .data[[".sim_label"]]
          ),
          width = barwidth * 1.2,
          linewidth = linewidth * 0.8
        ) +
        ggplot2::geom_errorbar(
          ggplot2::aes(
            ymin = .data[[conf_low_col]],
            ymax = .data[[conf_high_col]],
            color = .data[[".pw_label"]]
          ),
          width = barwidth,
          linewidth = linewidth
        ) +
        ggplot2::scale_color_manual(
          name = NULL,
          values = setNames(c(color, color), c(pw_label, sim_label)),
          breaks = c(pw_label, sim_label)
        )
    } else {
      p <- p +
        ggplot2::geom_errorbar(
          ggplot2::aes(
            ymin = .data[[conf_low_col]],
            ymax = .data[[conf_high_col]]
          ),
          color = color,
          width = barwidth,
          linewidth = linewidth
        )
    }
  } else {
    stop("Invalid `type`. Choose 'ribbon' or 'errorbar'.")
  }

  if (is.numeric(plot_data$relative_time)) {
    p <- p +
      ggplot2::scale_x_continuous(
        breaks = seq(
          floor(min(plot_data$relative_time, na.rm = TRUE)),
          ceiling(max(plot_data$relative_time, na.rm = TRUE)),
          by = 1
        )
      )
  }

  theme_style <- match.arg(theme_style, c("bw", "minimal", "classic"))
  if (theme_style == "bw") {
    p <- p +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  } else if (theme_style == "minimal") {
    p <- p +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  } else {
    p <- p + ggplot2::theme_classic()
  }

  p
}

#' Plot an event-study result
#'
#' @description
#' Base `plot()` method for `es_result` objects returned by [event_study()]
#' (or the deprecated [run_es()]). Draws the event-study curve with pointwise
#' confidence bands as a static ggplot, or — with `interactive = TRUE` — an
#' interactive plotly chart with hover tooltips (requires the suggested
#' \{plotly\} package).
#'
#' @param x An `es_result` object.
#' @param ci_level Confidence level to display (default `0.95`).
#' @param type `"ribbon"` (default) or `"errorbar"`. Static plots only.
#' @param interactive Logical; if `TRUE`, return an interactive plotly chart
#'   instead of a ggplot. Default `FALSE`.
#' @param show_simultaneous Logical; overlay the simultaneous bootstrap CI
#'   band (requires `bootstrap = TRUE` in the originating [event_study()]
#'   call). Default `FALSE`.
#' @param ... Further styling arguments: for static plots
#'   `vline_val`, `hline_val`, `vline_color`, `hline_color`, `linewidth`,
#'   `pointsize`, `alpha`, `barwidth`, `color`, `fill`, `theme_style`
#'   (`"bw"`, `"minimal"`, or `"classic"`); for interactive plots
#'   `markersize`, `show_ribbon`, `height`, `width`, and the shared color
#'   arguments.
#'
#' @return A `ggplot` object, or a `plotly` object when
#'   `interactive = TRUE`.
#'
#' @examples
#' \dontrun{
#' res <- event_study(df, outcome = y, time = year, timing = g,
#'                    unit = id, estimator = "cs")
#' plot(res)
#' plot(res, type = "errorbar", ci_level = 0.9)
#' plot(res, interactive = TRUE)
#' }
#'
#' @seealso [event_study()], [autoplot.es_result()]
#' @export
plot.es_result <- function(
  x,
  ci_level = 0.95,
  type = c("ribbon", "errorbar"),
  interactive = FALSE,
  show_simultaneous = FALSE,
  ...
) {
  if (isTRUE(interactive)) {
    return(.plot_es_interactive_impl(
      x, ci_level = ci_level, show_simultaneous = show_simultaneous, ...
    ))
  }
  type <- match.arg(type)
  .plot_es_impl(x, ci_level = ci_level, type = type,
                show_simultaneous = show_simultaneous, ...)
}
