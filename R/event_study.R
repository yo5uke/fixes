#' Run Event Study with Fixed Effects
#'
#' This function performs an event study using fixed effects regression.
#' It first generates lead and lag dummy variables relative to the treatment timing,
#' scales the time intervals if specified, and then estimates the regression model.
#'
#' @param data A dataframe containing the dataset.
#' @param outcome_var The name of the outcome variable (e.g., "y"). Should be unquoted.
#' @param treated_var The name of the treatment variable (e.g., "treated"). Should be unquoted.
#' @param time_var The name of the time variable (e.g., "year"). Should be unquoted.
#' @param timing The time period when the treatment occurred. For example, if the treatment was implemented in 1995, set `timing = 1995`.
#' @param lead_range Number of time periods to include before the treatment (negative leads). For example, `lead_range = 3` includes 3 periods before the treatment.
#' @param lag_range Number of time periods to include after the treatment (positive lags). For example, `lag_range = 2` includes 2 periods after the treatment.
#' @param fe_var A vector of fixed effects variables (e.g., c("id", "year")). These variables account for unobserved heterogeneity.
#' @param cluster_var An optional variable for clustering standard errors. For example, `cluster_var = "state"`.
#' @param baseline The relative time period to use as the baseline (default: -1). The corresponding dummy variable is excluded from the regression and treated as the reference group. For example, if `baseline = 0`, the treatment year is the baseline.
#' @param interval The time interval between observations (default: 1). For example, use `interval = 5` for datasets where time steps are in 5-year intervals.
#' @return A tidy dataframe with regression results. This includes:
#' - `term`: The lead or lag variable names.
#' - `estimate`: Estimated coefficients.
#' - `std.error`: Standard errors.
#' - `conf.high`: Upper bound of the 95% confidence interval.
#' - `conf.low`: Lower bound of the 95% confidence interval.
#' - `relative_time`: Scaled relative time based on the specified `interval`.
#' @details
#' This function is designed for panel data and supports time intervals other than 1 (e.g., 5-year intervals). It automatically scales the relative time variable using the `interval` parameter.
#'
#' Steps:
#' 1. Compute the relative time for each observation as `(time_var - timing) / interval`.
#' 2. Generate lead and lag dummy variables within the specified ranges (`lead_range`, `lag_range`).
#' 3. Construct and estimate the fixed effects regression model using `fixest::feols`.
#' 4. Format the regression results into a tidy dataframe.
#'
#' If `interval > 1`, ensure that the specified `lead_range` and `lag_range` correspond to the number of time intervals, not the absolute number of years.
#'
#' @export
run_es <- function(data,
                   outcome_var,
                   treated_var,
                   time_var,
                   timing,
                   lead_range,
                   lag_range,
                   fe_var,
                   cluster_var = NULL,
                   baseline = -1,
                   interval = 1) {
  # ---- 0. Helper function ----
  # Generate dummy variable names (leadX / lagX) based on relative year i
  get_term_name <- function(i) {
    if (i < 0) {
      paste0("lead", abs(i))
    } else {
      paste0("lag", i)
    }
  }

  # ---- 0.1 Check for column existence ----
  # Convert unquoted arguments to strings
  outcome_var_chr <- rlang::as_string(dplyr::ensym(outcome_var))
  treated_var_chr <- rlang::as_string(dplyr::ensym(treated_var))
  time_var_chr    <- rlang::as_string(dplyr::ensym(time_var))

  # Check if these columns exist in data
  # (If not, stop with an informative error)
  if (!outcome_var_chr %in% colnames(data)) {
    stop(
      "The specified outcome_var ('", outcome_var_chr,
      "') does not exist in the dataframe. Please specify an existing column."
    )
  }
  if (!treated_var_chr %in% colnames(data)) {
    stop(
      "The specified treated_var ('", treated_var_chr,
      "') does not exist in the dataframe. Please specify an existing column."
    )
  }
  if (!time_var_chr %in% colnames(data)) {
    stop(
      "The specified time_var ('", time_var_chr,
      "') does not exist in the dataframe. Please specify an existing column."
    )
  }

  # fe_var may contain multiple columns, so check them all if provided
  if (length(fe_var) > 0) {
    missing_fe_vars <- fe_var[!fe_var %in% colnames(data)]
    if (length(missing_fe_vars) > 0) {
      stop(
        "The specified fixed effects variable(s) (",
        paste(missing_fe_vars, collapse = ", "),
        ") do not exist in the dataframe. Please specify existing columns."
      )
    }
  }

  # cluster_var may be NULL or a string/column name
  if (!is.null(cluster_var)) {
    # convert to string if it's not already
    cluster_var_chr <- rlang::as_string(dplyr::ensym(cluster_var))
    if (!cluster_var_chr %in% colnames(data)) {
      stop(
        "The specified cluster_var ('", cluster_var_chr,
        "') does not exist in the dataframe. Please specify an existing column."
      )
    }
  }

  # ---- 1. Create lead and lag variables ----

  # Convert treated_var and time_var to symbols again for usage
  treated_var_sym <- dplyr::ensym(treated_var)
  time_var_sym    <- dplyr::ensym(time_var)

  # (1) Create relative_time column with scaling (1 unit = 'interval' years)
  data <- data |>
    dplyr::mutate(relative_time = ( !!time_var_sym - timing ) / interval)

  # (2) Check the minimum and maximum values of relative_time
  min_relative_time <- min(data$relative_time, na.rm = TRUE)
  max_relative_time <- max(data$relative_time, na.rm = TRUE)

  # (3) Warn if lead_range or lag_range exceeds the range of data
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

  # (4) Filter data to include only the specified lead and lag range
  data <- data |>
    dplyr::filter(
      dplyr::between(relative_time, -lead_range, lag_range)
    )

  # (5) Create dummy variables for each lead and lag
  for (i in seq(-lead_range, lag_range, by = 1)) {
    col_name <- get_term_name(i)
    data <- data |>
      dplyr::mutate(
        !!col_name := dplyr::if_else(
          !!treated_var_sym == 1 & (relative_time == i),
          1,
          0
        )
      )
  }

  # ---- 2. Construct and estimate the regression model ----

  # Convert outcome_var to a symbol
  outcome_var_sym <- dplyr::ensym(outcome_var)

  # 2-1) Generate the full list of terms for lead and lag variables
  all_terms <- c(
    paste0("lead", seq(lead_range, 1)),     # lead5,...,lead1
    paste0("lag", seq(0, lag_range))        # lag0, lag1,...
  )

  # 2-2) Determine the baseline variable to exclude
  baseline_term <- get_term_name(baseline)

  # 2-3) Exclude the baseline term from the list of terms
  included_terms <- setdiff(all_terms, baseline_term)

  # 2-4) Create the right-hand side (RHS) of the regression formula
  RHS_formula <- paste(included_terms, collapse = "+")

  # 2-5) Combine fixed effect variables into a formula
  fe_formula <- paste(fe_var, collapse = "+")

  # 2-6) Construct the regression formula: outcome ~ RHS | FE
  model_formula <- stats::as.formula(
    paste(
      rlang::as_string(outcome_var_sym),
      "~",
      RHS_formula,
      "|",
      fe_formula
    )
  )

  # 2-7) Estimate the model using fixest::feols
  if (!is.null(cluster_var)) {
    model <- fixest::feols(model_formula, data = data, cluster = cluster_var)
  } else {
    model <- fixest::feols(model_formula, data = data)
  }

  # ---- 3. Format the results ----
  result <- broom::tidy(model)

  # 3-1) Create the full order of terms for sorting
  full_order <- c(
    paste0("lead", seq(lead_range, 1)),
    paste0("lag", seq(0, lag_range))
  )

  # 3-2) Add the baseline row with estimate = 0
  baseline_row <- tibble::tibble(
    term = baseline_term,
    estimate = 0,
    std.error = 0,
    statistic = NA_real_,
    p.value = NA_real_
  )

  # 3-3) Combine and reorder results
  result <- result |>
    dplyr::bind_rows(baseline_row) |>
    dplyr::mutate(
      term = factor(term, levels = full_order)
    ) |>
    dplyr::arrange(term) |>
    dplyr::mutate(
      conf_high = estimate + 1.96 * std.error,
      conf_low  = estimate - 1.96 * std.error
    )

  # 3-4) Add the relative_time column based on the scaled interval
  rel_times <- c(seq(-lead_range, -1), seq(0, lag_range)) * interval
  rel_map <- tibble::tibble(
    term = factor(full_order, levels = full_order),
    relative_time = rel_times
  )

  result <- result |>
    dplyr::left_join(rel_map, by = "term")

  return(result)
}


#' Plot Event Study Results
#'
#' This function creates a plot for event study results using `ggplot2`.
#' Users can choose between ribbon-style confidence intervals or error bars
#' to visualize the estimates and their uncertainty.
#'
#' @param data A dataframe containing the results from the `run_es` function.
#' The dataframe must include the following columns:
#' - `relative_time`: The scaled time relative to the treatment.
#' - `estimate`: The estimated coefficients.
#' - `conf_low`: The lower bound of the 95% confidence interval.
#' - `conf_high`: The upper bound of the 95% confidence interval.
#' - `std.error`: The standard errors (required if `type = "errorbar"`).
#' @param type The type of confidence interval visualization: "ribbon" (default) or "errorbar".
#' - "ribbon": Shaded area representing the confidence intervals.
#' - "errorbar": Vertical error bars for each estimate.
#' @param vline_val The x-intercept for the vertical reference line (default: 0).
#' Typically represents the time of treatment.
#' @param vline_color The color of the vertical reference line (default: "#000").
#' @param hline_val The y-intercept for the horizontal reference line (default: 0).
#' Usually represents the null effect line.
#' @param hline_color The color of the horizontal reference line (default: "#000").
#' @param linewidth The width of the lines in the plot (default: 1).
#' @param pointsize The size of the points for the estimates (default: 2).
#' @param alpha The transparency level for the ribbon (default: 0.2).
#' @param barwidth The width of the error bars (default: 0.2).
#' @param color The color of the lines and points (default: "#B25D91FF").
#' @param fill The fill color for the ribbon (default: "#B25D91FF").
#' @return A ggplot object displaying the event study results. The plot includes:
#' - A line connecting the estimates over relative time.
#' - Points for the estimated coefficients.
#' - Either ribbon-style confidence intervals or error bars, depending on `type`.
#' - Vertical and horizontal reference lines for better interpretability.
#' @details
#' This function provides a flexible visualization tool for event study results.
#' Users can customize the appearance of the plot by adjusting the parameters
#' for line styles, point sizes, colors, and confidence interval types.
#'
#' **Column Requirements**:
#' The input dataframe (`data`) must include:
#' - `relative_time`: A numeric column for the time relative to the treatment.
#' - `estimate`: The estimated coefficients for each relative time.
#' - `conf_low` and `conf_high`: The bounds of the confidence intervals.
#' - `std.error`: The standard errors (only required if `type = "errorbar"`).
#'
#' **Type Options**:
#' - `"ribbon"`: A shaded area to represent the confidence intervals.
#' - `"errorbar"`: Error bars for each point. Standard errors (`std.error`) are required.
#'
#' @note If `type = "errorbar"`, ensure that the `std.error` column is present in the input dataframe.
#' Missing values in the `std.error` column for any term will result in incomplete confidence intervals.
#'
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
      ) +
      ggplot2::geom_line(
        linewidth = linewidth,
        color = color
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
