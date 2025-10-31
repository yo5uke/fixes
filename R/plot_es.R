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
  # Early validation
  if (!inherits(data, "es_result")) {
    warning("`data` is not class 'es_result'. Attempting to proceed.")
  }

  type <- match.arg(type, c("ribbon", "errorbar"))
  theme_style <- match.arg(theme_style, c("bw", "minimal", "classic"))

  # Pre-compute confidence interval column names once
  ci_str <- sprintf("%.0f", ci_level * 100)
  conf_low_col  <- paste0("conf_low_",  ci_str)
  conf_high_col <- paste0("conf_high_", ci_str)

  # Fallback to 95% CI if requested level not found
  if (!conf_low_col %in% names(data))  conf_low_col  <- "conf_low_95"
  if (!conf_high_col %in% names(data)) conf_high_col <- "conf_high_95"

  # Pre-compute y-axis label
  y_label <- sprintf("Estimate and %.0f%% CI", as.numeric(ci_str))

  # Build base plot (removed redundant group = 1)
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$relative_time, y = .data$estimate)) +
    ggplot2::geom_vline(xintercept = vline_val, linetype = "dashed", color = vline_color) +
    ggplot2::geom_hline(yintercept = hline_val, linetype = "dashed", color = hline_color) +
    ggplot2::geom_point(size = pointsize, color = color) +
    ggplot2::labs(x = "Relative Time to Treatment", y = y_label)

  # Add plot type-specific layers
  if (type == "ribbon") {
    p <- p +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data[[conf_low_col]], ymax = .data[[conf_high_col]]),
        fill = fill, alpha = alpha
      ) +
      ggplot2::geom_line(linewidth = linewidth, color = color)
  } else {
    p <- p +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = .data[[conf_low_col]], ymax = .data[[conf_high_col]]),
        color = color, width = barwidth, linewidth = linewidth
      )
  }

  # Add integer x-axis breaks for numeric relative_time
  if (is.numeric(data$relative_time)) {
    rt_range <- range(data$relative_time, na.rm = TRUE)
    p <- p + ggplot2::scale_x_continuous(
      breaks = seq(floor(rt_range[1]), ceiling(rt_range[2]), by = 1)
    )
  }

  # Apply theme using switch for cleaner code
  p <- p + switch(theme_style,
    bw = ggplot2::theme_bw() + ggplot2::theme(panel.grid.minor = ggplot2::element_blank()),
    minimal = ggplot2::theme_minimal() + ggplot2::theme(panel.grid.minor = ggplot2::element_blank()),
    classic = ggplot2::theme_classic()
  )

  p
}

#' Interactive plot of event-study results with dynamic CI switching
#'
#' @description
#' Creates an interactive plotly visualization of event study results with
#' dynamic confidence interval switching via buttons. Supports hover tooltips,
#' zoom/pan, and toggleable legends.
#'
#' @param data An object of class \code{es_result} returned by \code{run_es()}.
#' @param conf.level Initial confidence level to display (default 0.95).
#' @param display_type One of \code{"ribbon"} (default) or \code{"errorbar"}.
#' @param theme One of \code{"bw"}, \code{"minimal"}, or \code{"classic"}.
#' @param color Line and point color (default \code{"#B25D91FF"}).
#' @param fill Ribbon fill color (default \code{"#B25D91FF"}).
#' @param alpha Ribbon transparency (default 0.2).
#' @param linewidth Line width for estimates (default 1).
#' @param pointsize Point size for estimates (default 2).
#'
#' @details
#' This function creates an interactive plotly plot with the following features:
#' \itemize{
#'   \item \strong{Dynamic CI switching:} Buttons allow switching between 90%, 95%, and 99% confidence intervals
#'   \item \strong{Hover tooltips:} Display relative time, estimate, CI bounds, p-value, and sample size
#'   \item \strong{Zoom and pan:} Built-in plotly interactivity
#'   \item \strong{Legend toggle:} Click legend items to show/hide
#' }
#'
#' If not all confidence interval levels (90%, 95%, 99%) are present in the data,
#' a warning is issued and only available levels are displayed. To enable full
#' functionality, run \code{run_es()} with \code{conf.level = c(0.90, 0.95, 0.99)}.
#'
#' @return A \code{plotly} object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage (requires multiple CI levels)
#' res <- run_es(data, outcome = y, treatment = d, time = t,
#'               timing = 2010, fe = ~ id)
#' plot_es_interactive(res)
#'
#' # With errorbar display
#' plot_es_interactive(res, display_type = "errorbar")
#'
#' # Start with 90% CI
#' plot_es_interactive(res, conf.level = 0.90)
#'
#' # Custom styling
#' plot_es_interactive(res, theme = "minimal", color = "#E64B35")
#' }
plot_es_interactive <- function(
    data,
    conf.level = 0.95,
    display_type = "ribbon",
    theme = "bw",
    color = "#B25D91FF",
    fill = "#B25D91FF",
    alpha = 0.2,
    linewidth = 1,
    pointsize = 2
) {
  # Check for plotly package
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required for interactive plots. Install with: install.packages('plotly')")
  }

  # Early validation
  if (!inherits(data, "es_result")) {
    warning("`data` is not class 'es_result'. Attempting to proceed.")
  }

  display_type <- match.arg(display_type, c("ribbon", "errorbar"))
  theme <- match.arg(theme, c("bw", "minimal", "classic"))

  # Check for available CI levels
  ci_levels <- c(0.90, 0.95, 0.99)
  ci_strings <- c("90", "95", "99")
  available_cis <- character()
  available_levels <- numeric()

  for (i in seq_along(ci_levels)) {
    col_low <- paste0("conf_low_", ci_strings[i])
    col_high <- paste0("conf_high_", ci_strings[i])
    if (col_low %in% names(data) && col_high %in% names(data)) {
      available_cis <- c(available_cis, ci_strings[i])
      available_levels <- c(available_levels, ci_levels[i])
    }
  }

  # Warn if not all levels available
  if (length(available_cis) < 3) {
    if (length(available_cis) == 0) {
      stop("No standard confidence intervals (90%, 95%, 99%) found in data.")
    }
    warning(
      sprintf(
        "Only %s%% CI available. Run run_es() with conf.level = c(0.90, 0.95, 0.99) to enable CI level switching.",
        paste(available_cis, collapse = "%, ")
      )
    )
  }

  # Validate initial conf.level
  ci_init_str <- sprintf("%.0f", conf.level * 100)
  if (!ci_init_str %in% available_cis) {
    warning(sprintf("Requested %.0f%% CI not available. Using %s%% CI instead.",
                    conf.level * 100, available_cis[1]))
    ci_init_str <- available_cis[1]
    conf.level <- available_levels[1]
  }

  # Get metadata for hover
  N <- attr(data, "N")
  N_display <- if (!is.null(N) && !is.na(N)) N else "N/A"

  # Convert hex color to rgba for fill
  fill_rgba <- .hex_to_rgba(fill, alpha)

  # Theme settings
  theme_config <- .get_plotly_theme(theme)

  # Create base data for plotting
  plot_data <- data
  plot_data <- plot_data[order(plot_data$relative_time), ]

  # Build hover text template
  hover_template <- paste0(
    "<b>Relative Time:</b> %{x}<br>",
    "<b>Estimate:</b> %{y:.4f}<br>",
    "<b>CI:</b> [%{customdata[0]:.4f}, %{customdata[1]:.4f}]<br>",
    "<b>P-value:</b> %{customdata[2]:.4g}<br>",
    "<b>N:</b> ", N_display,
    "<extra></extra>"
  )

  # Initialize plotly figure
  fig <- plotly::plot_ly()

  # Add traces for each available CI level
  for (i in seq_along(available_cis)) {
    ci_str <- available_cis[i]
    conf_low_col <- paste0("conf_low_", ci_str)
    conf_high_col <- paste0("conf_high_", ci_str)

    # Determine visibility
    visible <- (ci_str == ci_init_str)

    # Custom data for hover
    custom_data <- cbind(
      plot_data[[conf_low_col]],
      plot_data[[conf_high_col]],
      plot_data$p.value
    )

    if (display_type == "ribbon") {
      # Add ribbon (shaded area)
      fig <- plotly::add_trace(
        fig,
        x = plot_data$relative_time,
        y = plot_data[[conf_high_col]],
        type = "scatter",
        mode = "lines",
        line = list(width = 0),
        showlegend = FALSE,
        hoverinfo = "skip",
        name = paste0(ci_str, "% CI Upper"),
        visible = visible,
        legendgroup = ci_str
      )

      fig <- plotly::add_trace(
        fig,
        x = plot_data$relative_time,
        y = plot_data[[conf_low_col]],
        type = "scatter",
        mode = "lines",
        fill = "tonexty",
        fillcolor = fill_rgba,
        line = list(width = 0),
        showlegend = FALSE,
        hoverinfo = "skip",
        name = paste0(ci_str, "% CI Lower"),
        visible = visible,
        legendgroup = ci_str
      )

      # Add estimate line
      fig <- plotly::add_trace(
        fig,
        x = plot_data$relative_time,
        y = plot_data$estimate,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = color, width = linewidth),
        marker = list(size = pointsize, color = color),
        name = paste0(ci_str, "% CI"),
        hovertemplate = hover_template,
        customdata = custom_data,
        visible = visible,
        legendgroup = ci_str
      )

    } else {  # errorbar
      # Add errorbars
      error_y <- list(
        type = "data",
        symmetric = FALSE,
        array = plot_data[[conf_high_col]] - plot_data$estimate,
        arrayminus = plot_data$estimate - plot_data[[conf_low_col]],
        color = color,
        thickness = linewidth,
        width = 5
      )

      fig <- plotly::add_trace(
        fig,
        x = plot_data$relative_time,
        y = plot_data$estimate,
        type = "scatter",
        mode = "markers",
        marker = list(size = pointsize, color = color),
        error_y = error_y,
        name = paste0(ci_str, "% CI"),
        hovertemplate = hover_template,
        customdata = custom_data,
        visible = visible
      )
    }
  }

  # Add reference lines
  fig <- plotly::add_trace(
    fig,
    x = c(min(plot_data$relative_time, na.rm = TRUE),
          max(plot_data$relative_time, na.rm = TRUE)),
    y = c(0, 0),
    type = "scatter",
    mode = "lines",
    line = list(dash = "dash", color = "black", width = 1),
    showlegend = FALSE,
    hoverinfo = "skip",
    name = "Zero line"
  )

  fig <- plotly::add_trace(
    fig,
    x = c(0, 0),
    y = c(min(plot_data[[paste0("conf_low_", available_cis[1])]], na.rm = TRUE) * 1.1,
          max(plot_data[[paste0("conf_high_", available_cis[1])]], na.rm = TRUE) * 1.1),
    type = "scatter",
    mode = "lines",
    line = list(dash = "dash", color = "black", width = 1),
    showlegend = FALSE,
    hoverinfo = "skip",
    name = "Event time"
  )

  # Create buttons for CI switching
  buttons <- list()
  n_traces_per_ci <- if (display_type == "ribbon") 3 else 1
  n_ref_lines <- 2

  for (i in seq_along(available_cis)) {
    ci_str <- available_cis[i]

    # Create visibility array
    visible_array <- rep(FALSE, length(available_cis) * n_traces_per_ci + n_ref_lines)

    # Set visible traces for this CI level
    start_idx <- (i - 1) * n_traces_per_ci + 1
    end_idx <- i * n_traces_per_ci
    visible_array[start_idx:end_idx] <- TRUE

    # Reference lines always visible
    visible_array[(length(visible_array) - 1):length(visible_array)] <- TRUE

    buttons[[i]] <- list(
      label = paste0(ci_str, "%"),
      method = "update",
      args = list(
        list(visible = visible_array),
        list(title = paste0("Event Study Results (", ci_str, "% CI)"))
      )
    )
  }

  # Layout configuration
  fig <- plotly::layout(
    fig,
    title = paste0("Event Study Results (", ci_init_str, "% CI)"),
    xaxis = list(
      title = "Relative Time to Treatment",
      zeroline = FALSE,
      gridcolor = theme_config$gridcolor,
      dtick = 1
    ),
    yaxis = list(
      title = paste0("Estimate and ", ci_init_str, "% CI"),
      zeroline = FALSE,
      gridcolor = theme_config$gridcolor
    ),
    plot_bgcolor = theme_config$plot_bgcolor,
    paper_bgcolor = theme_config$paper_bgcolor,
    updatemenus = list(
      list(
        type = "buttons",
        direction = "right",
        x = 0.7,
        y = 1.15,
        xanchor = "left",
        yanchor = "top",
        buttons = buttons,
        bgcolor = "#E0E0E0",
        bordercolor = "#999999",
        font = list(size = 11)
      )
    ),
    annotations = list(
      list(
        text = "Confidence Level:",
        x = 0.65,
        y = 1.15,
        xref = "paper",
        yref = "paper",
        xanchor = "right",
        yanchor = "top",
        showarrow = FALSE,
        font = list(size = 12)
      )
    ),
    hovermode = "closest"
  )

  # Configure plotly options
  fig <- plotly::config(
    fig,
    displayModeBar = TRUE,
    modeBarButtonsToRemove = c("lasso2d", "select2d"),
    displaylogo = FALSE
  )

  fig
}

# Helper: Convert hex color to rgba
.hex_to_rgba <- function(hex, alpha = 0.2) {
  hex <- gsub("#", "", hex)
  r <- strtoi(substring(hex, 1, 2), 16L)
  g <- strtoi(substring(hex, 3, 4), 16L)
  b <- strtoi(substring(hex, 5, 6), 16L)
  sprintf("rgba(%d, %d, %d, %.2f)", r, g, b, alpha)
}

# Helper: Get plotly theme settings
.get_plotly_theme <- function(theme) {
  switch(theme,
    bw = list(
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      gridcolor = "#E5E5E5"
    ),
    minimal = list(
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      gridcolor = "#F0F0F0"
    ),
    classic = list(
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      gridcolor = "white"
    )
  )
}
