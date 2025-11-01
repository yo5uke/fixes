#' Interactive event-study plot with hover details
#'
#' Creates an interactive plotly visualization of event study results with hover-over
#' displays showing coefficients, confidence intervals, and other details.
#'
#' @param data An object of class \code{es_result} returned by \code{run_es()}.
#' @param ci_level Confidence level to display (e.g., 0.95). Default is 0.95.
#' @param vline_val Numeric location for vertical reference line (default 0).
#' @param hline_val Numeric location for horizontal reference line (default 0).
#' @param vline_color Color for vertical reference line (default "#000").
#' @param hline_color Color for horizontal reference line (default "#000").
#' @param color Point and line color (default "#B25D91FF").
#' @param fill Ribbon/band fill color (default "#B25D91FF").
#' @param alpha Ribbon transparency (default 0.2).
#' @param linewidth Line width (default 2).
#' @param markersize Marker size (default 8).
#' @param show_ribbon Logical; if TRUE, shows confidence interval as a ribbon band (default TRUE).
#' @param height Plot height in pixels (default NULL for auto).
#' @param width Plot width in pixels (default NULL for auto).
#'
#' @return A \code{plotly} object that can be displayed interactively.
#'
#' @details
#' The hover tooltip displays:
#' \itemize{
#'   \item Relative time to treatment
#'   \item Point estimate (coefficient)
#'   \item Confidence interval bounds
#'   \item Standard error
#'   \item P-value
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming res <- run_es(...)
#' plot_es_interactive(res)
#' plot_es_interactive(res, ci_level = 0.99, show_ribbon = FALSE)
#' }
plot_es_interactive <- function(
    data,
    ci_level     = 0.95,
    vline_val    = 0,
    hline_val    = 0,
    vline_color  = "#000",
    hline_color  = "#000",
    color        = "#B25D91FF",
    fill         = "#B25D91FF",
    alpha        = 0.2,
    linewidth    = 2,
    markersize   = 8,
    show_ribbon  = TRUE,
    height       = NULL,
    width        = NULL
) {
  # Check if plotly is available
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required for interactive plots. Install it with: install.packages('plotly')")
  }

  if (!inherits(data, "es_result")) {
    warning("`data` is not class 'es_result'. Attempting to proceed.")
  }

  # Get confidence interval columns
  ci_str <- sprintf("%.0f", ci_level * 100)
  conf_low_col  <- paste0("conf_low_",  ci_str)
  conf_high_col <- paste0("conf_high_", ci_str)

  # Fallback to 95% CI if requested level not available
  if (!conf_low_col %in% names(data))  conf_low_col  <- "conf_low_95"
  if (!conf_high_col %in% names(data)) conf_high_col <- "conf_high_95"

  # Prepare data
  plot_data <- data
  plot_data$conf_low  <- plot_data[[conf_low_col]]
  plot_data$conf_high <- plot_data[[conf_high_col]]

  # Create hover text
  plot_data$hover_text <- paste0(
    "<b>Relative Time:</b> ", plot_data$relative_time, "<br>",
    "<b>Estimate:</b> ", sprintf("%.4f", plot_data$estimate), "<br>",
    "<b>Std. Error:</b> ", sprintf("%.4f", plot_data$std.error), "<br>",
    "<b>", ci_str, "% CI:</b> [", sprintf("%.4f", plot_data$conf_low), ", ",
    sprintf("%.4f", plot_data$conf_high), "]<br>",
    "<b>P-value:</b> ", sprintf("%.4f", plot_data$p.value)
  )

  # Initialize plotly figure
  fig <- plotly::plot_ly(height = height, width = width)

  # Add confidence interval ribbon if requested
  if (show_ribbon) {
    fig <- fig %>%
      plotly::add_ribbons(
        data = plot_data,
        x = ~relative_time,
        ymin = ~conf_low,
        ymax = ~conf_high,
        fillcolor = fill,
        opacity = alpha,
        line = list(width = 0),
        name = paste0(ci_str, "% CI"),
        hoverinfo = "skip",
        showlegend = FALSE
      )
  }

  # Add point estimates with line
  fig <- fig %>%
    plotly::add_trace(
      data = plot_data,
      x = ~relative_time,
      y = ~estimate,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = color, width = linewidth),
      marker = list(
        size = markersize,
        color = color,
        line = list(color = '#fff', width = 1)
      ),
      text = ~hover_text,
      hoverinfo = 'text',
      name = 'Estimate',
      showlegend = FALSE
    )

  # Add vertical reference line at treatment time
  fig <- fig %>%
    plotly::add_segments(
      x = vline_val, xend = vline_val,
      y = min(plot_data$conf_low, na.rm = TRUE) * 1.1,
      yend = max(plot_data$conf_high, na.rm = TRUE) * 1.1,
      line = list(color = vline_color, dash = 'dash', width = 1),
      hoverinfo = 'skip',
      showlegend = FALSE,
      inherit = FALSE
    )

  # Add horizontal reference line at zero
  fig <- fig %>%
    plotly::add_segments(
      x = min(plot_data$relative_time, na.rm = TRUE) - 0.5,
      xend = max(plot_data$relative_time, na.rm = TRUE) + 0.5,
      y = hline_val, yend = hline_val,
      line = list(color = hline_color, dash = 'dash', width = 1),
      hoverinfo = 'skip',
      showlegend = FALSE,
      inherit = FALSE
    )

  # Configure layout
  fig <- fig %>%
    plotly::layout(
      xaxis = list(
        title = "Relative Time to Treatment",
        zeroline = FALSE,
        gridcolor = '#eee'
      ),
      yaxis = list(
        title = sprintf("Estimate and %s%% CI", ci_str),
        zeroline = FALSE,
        gridcolor = '#eee'
      ),
      plot_bgcolor = '#fff',
      paper_bgcolor = '#fff',
      hovermode = 'closest',
      font = list(family = "Arial, sans-serif", size = 12)
    )

  # Configure modebar
  fig <- fig %>%
    plotly::config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c(
        'pan2d', 'select2d', 'lasso2d', 'autoScale2d',
        'hoverClosestCartesian', 'hoverCompareCartesian'
      ),
      displaylogo = FALSE
    )

  return(fig)
}
