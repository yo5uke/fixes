# Interactive plotly event-study plot with hover tooltips (period, estimate,
# CI bounds, SE, p-value, and simultaneous CI bounds when requested). Shared
# engine behind plot(x, interactive = TRUE) and the deprecated
# plot_es_interactive(). Requires the suggested {plotly} package.
.plot_es_interactive_impl <- function(
  data,
  ci_level = 0.95,
  vline_val = NULL,
  hline_val = 0,
  vline_color = "#000",
  hline_color = "#000",
  color = NULL,
  fill = NULL,
  alpha = NULL,
  linewidth = 2,
  markersize = 8,
  show_ribbon = TRUE,
  show_simultaneous = FALSE,
  height = NULL,
  width = NULL,
  time_axis = "relative"
) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop(
      "Package 'plotly' is required for interactive plots. ",
      "Install it with: install.packages('plotly')",
      call. = FALSE
    )
  }

  if (!inherits(data, "es_result")) {
    warning("`data` is not class 'es_result'. Attempting to proceed.")
  }

  pal <- .fixes_palette()
  if (is.null(color)) color <- pal$line
  if (is.null(fill)) fill <- pal$ribbon
  if (is.null(alpha)) alpha <- pal$alpha

  .es_check_simultaneous(data, show_simultaneous)
  ci <- .es_ci_cols(data, ci_level)
  ax <- .es_x_axis(data, time_axis)
  if (is.null(vline_val)) vline_val <- ax$ref_line

  plot_data <- data
  plot_data$x <- ax$values
  plot_data$conf_low <- plot_data[[ci$low]]
  plot_data$conf_high <- plot_data[[ci$high]]

  sim_ci_pct <- .es_sim_pct(data)

  x_hover_label <- if (identical(time_axis, "calendar")) {
    ax$label
  } else {
    "Relative Time"
  }
  plot_data$hover_text <- paste0(
    "<b>", x_hover_label, ":</b> ", format(plot_data$x), "<br>",
    "<b>Estimate:</b> ", sprintf("%.4f", plot_data$estimate), "<br>",
    "<b>Std. Error:</b> ", sprintf("%.4f", plot_data$std.error), "<br>",
    "<b>", ci$pct, "% CI:</b> [",
    sprintf("%.4f", plot_data$conf_low), ", ",
    sprintf("%.4f", plot_data$conf_high), "]<br>",
    if (isTRUE(show_simultaneous)) {
      paste0(
        "<b>Simultaneous CI:</b> [",
        sprintf("%.4f", plot_data$conf_low_sim), ", ",
        sprintf("%.4f", plot_data$conf_high_sim), "]<br>"
      )
    } else {
      ""
    },
    "<b>P-value:</b> ", sprintf("%.4f", plot_data$p.value)
  )

  fig <- plotly::plot_ly(height = height, width = width)

  # Simultaneous CI ribbon (wider, lighter — drawn underneath)
  if (isTRUE(show_simultaneous)) {
    fig <- fig |>
      plotly::add_ribbons(
        data = plot_data,
        x = ~x,
        ymin = ~conf_low_sim,
        ymax = ~conf_high_sim,
        fillcolor = fill,
        opacity = alpha * 0.5,
        line = list(width = 0),
        name = paste0(sim_ci_pct, " Simultaneous CI"),
        hoverinfo = "skip",
        showlegend = TRUE
      )
  }

  if (isTRUE(show_ribbon)) {
    fig <- fig |>
      plotly::add_ribbons(
        data = plot_data,
        x = ~x,
        ymin = ~conf_low,
        ymax = ~conf_high,
        fillcolor = fill,
        opacity = alpha,
        line = list(width = 0),
        name = paste0(ci$pct, "% Pointwise CI"),
        hoverinfo = "skip",
        showlegend = isTRUE(show_simultaneous)
      )
  }

  fig <- fig |>
    plotly::add_trace(
      data = plot_data,
      x = ~x,
      y = ~estimate,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = color, width = linewidth),
      marker = list(
        size = markersize,
        color = color,
        line = list(color = "#fff", width = 1)
      ),
      text = ~hover_text,
      hoverinfo = "text",
      name = "Estimate",
      showlegend = FALSE
    )

  # Reference lines. The vertical one must span the drawn y range, so pad the
  # range additively — scaling the bounds leaves the line short whenever they
  # do not straddle zero.
  y_lo <- min(c(plot_data$conf_low, plot_data$estimate), na.rm = TRUE)
  y_hi <- max(c(plot_data$conf_high, plot_data$estimate), na.rm = TRUE)
  if (isTRUE(show_simultaneous)) {
    y_lo <- min(y_lo, min(plot_data$conf_low_sim, na.rm = TRUE))
    y_hi <- max(y_hi, max(plot_data$conf_high_sim, na.rm = TRUE))
  }
  y_pad <- 0.05 * max(y_hi - y_lo, .Machine$double.eps)

  # Half a period of padding on either side, in the axis's own units.
  x_num <- as.numeric(plot_data$x)
  x_step <- if (length(unique(x_num)) > 1L) {
    min(diff(sort(unique(x_num))))
  } else {
    1
  }
  x_lo <- min(x_num, na.rm = TRUE) - x_step / 2
  x_hi <- max(x_num, na.rm = TRUE) + x_step / 2
  if (ax$is_date) {
    x_lo <- as.Date(x_lo, origin = "1970-01-01")
    x_hi <- as.Date(x_hi, origin = "1970-01-01")
  }

  fig <- fig |>
    plotly::add_segments(
      x = vline_val,
      xend = vline_val,
      y = y_lo - y_pad,
      yend = y_hi + y_pad,
      line = list(color = vline_color, dash = "dash", width = 1),
      hoverinfo = "skip",
      showlegend = FALSE,
      inherit = FALSE
    ) |>
    plotly::add_segments(
      x = x_lo,
      xend = x_hi,
      y = hline_val,
      yend = hline_val,
      line = list(color = hline_color, dash = "dash", width = 1),
      hoverinfo = "skip",
      showlegend = FALSE,
      inherit = FALSE
    )

  fig <- fig |>
    plotly::layout(
      xaxis = list(
        title = ax$label,
        zeroline = FALSE,
        gridcolor = "#eee"
      ),
      yaxis = list(
        title = sprintf("Estimate and %s%% CI", ci$pct),
        zeroline = FALSE,
        gridcolor = "#eee"
      ),
      plot_bgcolor = "#fff",
      paper_bgcolor = "#fff",
      hovermode = "closest",
      font = list(family = "Arial, sans-serif", size = 12)
    )

  fig |>
    plotly::config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c(
        "pan2d",
        "select2d",
        "lasso2d",
        "autoScale2d",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      ),
      displaylogo = FALSE
    )
}
