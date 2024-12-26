utils::globalVariables(c(
  "relative_time", "estimate", "conf_low", "conf_high", "term", "std.error", ":="
))

#' Run Event Study with Fixed Effects
#'
#' This function performs an event study using fixed effects regression.
#' It first generates lead and lag dummy variables relative to the treatment timing,
#' and then estimates the regression model.
#'
#' @param data A dataframe containing the dataset.
#' @param outcome_var The name of the outcome variable (e.g., "y").
#' @param treated_var The name of the treatment variable (e.g., "treated").
#' @param time_var The name of the time variable (e.g., "year").
#' @param timing The time period when the treatment occurred.
#' @param lead_range Number of time periods to include before the treatment (negative leads).
#' @param lag_range Number of time periods to include after the treatment (positive lags).
#' @param fe_var A vector of fixed effects variables (e.g., c("id", "year")).
#' @param cluster_var An optional variable for clustering standard errors.
#' @return A tidy dataframe with regression results, including estimates,
#' confidence intervals, and a relative time column.
#' @export
run_es <- function(data,
                   outcome_var,
                   treated_var,
                   time_var,
                   timing,
                   lead_range,
                   lag_range,
                   fe_var,
                   cluster_var = NULL) {
  # --- 1. Create lead and lag variables ---

  # Convert treated and time variables to symbols
  treated_var_sym <- dplyr::ensym(treated_var)
  time_var_sym    <- dplyr::ensym(time_var)

  # (1) Create a column for relative time
  data <- data |>
    dplyr::mutate(relative_time = !!time_var_sym - timing)

  # (2) Check the range of relative time
  min_relative_time <- min(data$relative_time, na.rm = TRUE)
  max_relative_time <- max(data$relative_time, na.rm = TRUE)

  # (3) Warn if the specified lead_range or lag_range exceeds the data range
  if (lead_range > abs(min_relative_time)) {
    warning(
      "The specified lead_range (", lead_range,
      ") exceeds the available range in the data on the lead side (", abs(min_relative_time), ")."
    )
  }

  if (lag_range > max_relative_time) {
    warning(
      "The specified lag_range (", lag_range,
      ") exceeds the available range in the data on the lag side (", max_relative_time, ")."
    )
  }

  # (4) Filter the data within the specified lead and lag range
  data <- data |>
    dplyr::filter(
      dplyr::between(relative_time, -lead_range, lag_range)
    )

  # (5) Create lead and lag dummy variables
  for (i in seq(-lead_range, lag_range, by = 1)) {
    col_name <- if (i < 0) {
      paste0("lead", abs(i))
    } else {
      paste0("lag", i)
    }
    data <- data |>
      dplyr::mutate(
        !!col_name := dplyr::if_else(
          !!treated_var_sym == 1 & (relative_time == i),
          1,
          0
        )
      )
  }

  # --- 2. Run the regression model ---

  # Convert outcome variable to a symbol
  outcome_var_sym <- dplyr::ensym(outcome_var)

  # Create the right-hand side of the formula
  RHS_formula <- paste(
    c(paste0("lead", seq(2, lead_range)),
      paste0("lag", seq(0, lag_range))),
    collapse = "+"
  )

  # Combine fixed effects variables into a formula
  fe_formula <- paste(fe_var, collapse = "+")

  # Construct the regression formula
  model_formula <- stats::as.formula(
    paste(
      outcome_var_sym,
      "~",
      RHS_formula,
      "|",
      fe_formula
    )
  )

  # Estimate the model using fixest::feols
  if (!is.null(cluster_var)) {
    model <- fixest::feols(model_formula, data = data, cluster = cluster_var)
  } else {
    model <- fixest::feols(model_formula, data = data)
  }

  # Format the regression results using broom::tidy
  result <- broom::tidy(model)

  # Create the order for sorting terms
  order <- c(
    paste0("lead", seq(lead_range, 1)),
    paste0("lag", seq(0, lag_range))
  )

  # Add a baseline row for lead1
  lead1_row <- tibble::tibble(
    term = "lead1",
    estimate = 0,
    std.error = 0,
    statistic = NA_real_,
    p.value = NA_real_
  )

  # Combine the baseline with results, reorder terms, and calculate confidence intervals
  result <- result |>
    dplyr::bind_rows(lead1_row) |>
    dplyr::mutate(term = factor(term, levels = order)) |>
    dplyr::arrange(term) |>
    dplyr::mutate(
      conf_high = estimate + 1.96 * std.error,
      conf_low  = estimate - 1.96 * std.error
    )

  # Add a relative time column
  result <- result |>
    dplyr::mutate(
      relative_time = seq(-lead_range, lag_range)
    )

  return(result)
}


#' Plot Event Study Results
#'
#' This function creates a plot for event study results using `ggplot2`.
#' Users can choose between ribbon-style confidence intervals or error bars.
#'
#' @param data A dataframe containing the results from the `run_es` function.
#' @param type The type of confidence interval visualization: "ribbon" (default) or "errorbar".
#' @param vline_val The x-intercept for the vertical reference line (default: 0).
#' @param vline_color Color for the vertical reference line (default: "#000").
#' @param hline_val The y-intercept for the horizontal reference line (default: 0).
#' @param hline_color Color for the horizontal reference line (default: "#000").
#' @param linewidth The width of the lines for the plot (default: 1).
#' @param pointsize The size of the points for the estimates (default: 2).
#' @param alpha The transparency level for ribbons (default: 0.2).
#' @param barwidth The width of the error bars (default: 0.2).
#' @param color The color for the lines and points (default: "#B25D91FF").
#' @param fill The fill color for ribbons (default: "#B25D91FF").
#' @return A ggplot object displaying the event study results.
#' @export
plot_es <- function(data, type = "ribbon", vline_val = 0, vline_color = "#000", hline_val = 0, hline_color = "#000", linewidth = 1, pointsize = 2, alpha = .2, barwidth = .2, color = "#B25D91FF", fill = "#B25D91FF") {
  # Validate the type of confidence interval visualization
  if (!type %in% c("ribbon", "errorbar")) {
    stop("Invalid type. Please choose 'ribbon' or 'errorbar'.")
  }

  # Create the base plot with vertical and horizontal reference lines
  base_plot <-
    ggplot2::ggplot(
      data = data,
      ggplot2::aes(x = relative_time, y = estimate, group = 1)
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
    ggplot2::geom_line(
      linewidth = linewidth,
      color = color
    ) +
    ggplot2::geom_point(
      size = pointsize,
      color = color
    ) +
    ggplot2::labs(
      x = "Relative Time to Treatment",
      y = "Estimate and 95% Confidence Interval"
    ) +
    ggplot2::theme_minimal()

  # Add ribbon-style confidence intervals
  if (type == "ribbon") {
    base_plot <- base_plot +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = conf_low, ymax = conf_high),
        fill = fill,
        alpha = alpha
      )
  }
  # Add error bars for confidence intervals
  else if (type == "errorbar") {
    data_ <- data |>
      dplyr::mutate(
        std.error = dplyr::if_else(term == "lead1", NA_real_, std.error),
        conf_high = estimate + 1.96 * std.error,
        conf_low  = estimate - 1.96 * std.error
      )
    base_plot <- base_plot +
      ggplot2::geom_errorbar(
        data = data_,
        ggplot2::aes(ymin = conf_low, ymax = conf_high),
        color = color,
        width = barwidth,
        linewidth = linewidth
      )
  }

  return(base_plot)
}
