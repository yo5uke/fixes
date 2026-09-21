# Static event-study plot (ribbon or errorbar). Shared engine behind
# plot.es_result() and the deprecated plot_es().
.plot_es_impl <- function(
  data,
  ci_level = 0.95,
  type = "errorbar",
  vline_val = NULL,
  vline_color = "#000",
  hline_val = 0,
  hline_color = "#000",
  linewidth = NULL,
  pointsize = 2,
  alpha = NULL,
  barwidth = .2,
  color = NULL,
  fill = NULL,
  theme_style = "bw",
  show_simultaneous = FALSE,
  time_axis = "relative",
  errorbar_color = NULL
) {
  if (!inherits(data, "es_result")) {
    warning("`data` is not class 'es_result'. Attempting to proceed.")
  }
  type <- match.arg(type, c("errorbar", "ribbon"))
  theme_style <- match.arg(theme_style, c("bw", "minimal", "classic"))

  # Colours default per display: black for error bars, steel blue for ribbons.
  pal <- .fixes_palette()
  if (is.null(color)) {
    color <- if (type == "ribbon") pal$line else pal$point
  }
  if (is.null(fill)) fill <- pal$ribbon
  if (is.null(alpha)) alpha <- pal$alpha
  if (is.null(errorbar_color)) errorbar_color <- color
  # Error bars are drawn thin; the ribbon's own line carries more weight.
  if (is.null(linewidth)) {
    linewidth <- if (type == "ribbon") pal$linewidth else pal$barwidth_line
  }

  .es_check_simultaneous(data, show_simultaneous)
  ci <- .es_ci_cols(data, ci_level)
  ax <- .es_x_axis(data, time_axis)
  if (is.null(vline_val)) vline_val <- ax$ref_line

  plot_data <- data
  plot_data$.x <- ax$values

  # Legend labels for the simultaneous overlay must exist as columns before
  # ggplot() is called, since `p` stores the data it was built from.
  if (isTRUE(show_simultaneous)) {
    sim_pct <- .es_sim_pct(data)
    pw_label <- paste0(sim_pct, " pointwise CI")
    sim_label <- paste0(sim_pct, " simultaneous CI")
    plot_data$.pw_label <- pw_label
    plot_data$.sim_label <- sim_label
  }

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data[[".x"]], y = .data$estimate, group = 1)
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
    ggplot2::labs(
      x = ax$label,
      y = sprintf("Estimate and %s%% CI", ci$pct)
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
          alpha = alpha * 0.55
        ) +
        ggplot2::geom_ribbon(
          ggplot2::aes(
            ymin = .data[[ci$low]],
            ymax = .data[[ci$high]],
            fill = .data[[".pw_label"]]
          ),
          alpha = alpha
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
            ymin = .data[[ci$low]],
            ymax = .data[[ci$high]]
          ),
          fill = fill,
          alpha = alpha
        ) +
        ggplot2::geom_line(linewidth = linewidth, color = color)
    }
  } else {
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
            ymin = .data[[ci$low]],
            ymax = .data[[ci$high]],
            color = .data[[".pw_label"]]
          ),
          width = barwidth,
          linewidth = linewidth
        ) +
        ggplot2::scale_color_manual(
          name = NULL,
          values = setNames(
            c(errorbar_color, errorbar_color),
            c(pw_label, sim_label)
          ),
          breaks = c(pw_label, sim_label)
        )
    } else {
      p <- p +
        ggplot2::geom_errorbar(
          ggplot2::aes(
            ymin = .data[[ci$low]],
            ymax = .data[[ci$high]]
          ),
          color = errorbar_color,
          width = barwidth,
          linewidth = linewidth
        )
    }
  }

  # Points go on last so the bars (or band) never cover them.
  p <- p + ggplot2::geom_point(size = pointsize, color = color)

  # One break per estimated period; a Date axis needs its own scale.
  if (ax$is_date) {
    p <- p + ggplot2::scale_x_date(breaks = ax$breaks)
  } else if (is.numeric(ax$values)) {
    p <- p + ggplot2::scale_x_continuous(breaks = ax$breaks)
  }

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
#' @details
#' The default display is black error bars. `type = "ribbon"` switches to a
#' line with a shaded band and, unless `color`/`fill` say otherwise, to the
#' package's muted steel-blue palette. In either display `color` sets the
#' points (and the line), while `errorbar_color` can recolour the bars on
#' their own.
#'
#' `time_axis` controls what the horizontal axis shows. The default
#' `"relative"` plots event time (periods since treatment) and labels the axis
#' "Relative Time to Treatment". `"calendar"` instead plots the original time
#' values (e.g. `2010`, or a `Date`), labels the axis with the `time` column's
#' name, and moves the dashed vertical reference line to the treatment period.
#' It is available whenever every treated unit adopts in the same period —
#' universal timing, or a staggered design that happens to have a single
#' cohort — and errors otherwise, because with several cohorts one relative
#' period corresponds to several calendar periods.
#'
#' @param x An `es_result` object.
#' @param ci_level Confidence level to display (default `0.95`). Must be one of
#'   the levels the result was estimated at (`conf_level` in [event_study()]).
#' @param type `"errorbar"` (default) or `"ribbon"`. Static plots only.
#' @param time_axis `"relative"` (default) for event time, or `"calendar"` for
#'   the original time values. See Details.
#' @param interactive Logical; if `TRUE`, return an interactive plotly chart
#'   instead of a ggplot. Default `FALSE`.
#' @param show_simultaneous Logical; overlay the simultaneous bootstrap CI
#'   band (requires `bootstrap = TRUE` in the originating [event_study()]
#'   call). Default `FALSE`.
#' @param ... Further styling arguments: for static plots
#'   `vline_val`, `hline_val`, `vline_color`, `hline_color`, `linewidth`,
#'   `pointsize`, `alpha`, `barwidth`, `color`, `fill`, `errorbar_color`,
#'   `theme_style` (`"bw"`, `"minimal"`, or `"classic"`); for interactive
#'   plots `markersize`, `show_ribbon`, `height`, `width`, and the shared
#'   color arguments. `vline_val` defaults to relative time -1, the last
#'   period before treatment, on whichever axis is in use. `errorbar_color` colours the bars alone and
#'   follows `color` unless set.
#'
#' @return A `ggplot` object, or a `plotly` object when
#'   `interactive = TRUE`.
#'
#' @examples
#' \dontrun{
#' res <- event_study(df, outcome = y, time = year, timing = g,
#'                    unit = id, estimator = "cs")
#' plot(res)
#' plot(res, type = "ribbon", ci_level = 0.9)
#' plot(res, color = "#1B4965", errorbar_color = "grey50")
#' plot(res, interactive = TRUE)
#'
#' # Universal timing: label the axis with calendar years instead
#' uni <- event_study(df, outcome = y, treatment = d, time = year,
#'                    timing = 2010, fe = ~ id + year)
#' plot(uni, time_axis = "calendar")
#' }
#'
#' @seealso [event_study()], [autoplot.es_result()]
#' @export
plot.es_result <- function(
  x,
  ci_level = 0.95,
  type = c("errorbar", "ribbon"),
  time_axis = c("relative", "calendar"),
  interactive = FALSE,
  show_simultaneous = FALSE,
  ...
) {
  time_axis <- match.arg(time_axis)
  if (isTRUE(interactive)) {
    return(.plot_es_interactive_impl(
      x, ci_level = ci_level, time_axis = time_axis,
      show_simultaneous = show_simultaneous, ...
    ))
  }
  type <- match.arg(type)
  .plot_es_impl(x, ci_level = ci_level, type = type, time_axis = time_axis,
                show_simultaneous = show_simultaneous, ...)
}
