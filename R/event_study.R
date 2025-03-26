#' Run Event Study with Fixed Effects
#'
#' This function performs an event study using fixed effects regression.
#' It first generates lead and lag dummy variables relative to the treatment timing,
#' scales the time intervals if specified, and then estimates the regression model.
#'
#' @param data A dataframe containing the dataset.
#' @param outcome The name of the outcome variable (e.g., "y"). Should be unquoted.
#' @param treatment The name of the treatment variable (e.g., "treated"). Should be unquoted.
#' @param time The name of the time variable (e.g., "year"). Should be unquoted.
#' @param timing The time period when the treatment occurred. For example, if the treatment was implemented in 1995, set `timing = 1995`.
#' @param lead_range Number of time periods to include before the treatment (negative leads). For example, `lead_range = 3` includes 3 periods before the treatment.
#' @param lag_range Number of time periods to include after the treatment (positive lags). For example, `lag_range = 2` includes 2 periods after the treatment.
#' @param covariates Optional covariates to include in the regression.
#' You can specify covariates using either:
#' - an additive expression (e.g., `x1 + x2 + x3`), or
#' - a character vector of variable names (e.g., `c("x1", "x2", "x3")`).
#' These variables are added to the right-hand side of the regression model.
#' @param fe A vector of fixed effects variables or an additive expression (e.g., firm_id + year). These variables account for unobserved heterogeneity.
#' @param cluster An optional variable for clustering standard errors. For example, `cluster = "state"`.
#' @param baseline The relative time period to use as the baseline (default: -1). The corresponding dummy variable is excluded from the regression and treated as the reference group. For example, if `baseline = 0`, the treatment year is the baseline.
#' @param interval The time interval between observations (default: 1). For example, use `interval = 5` for datasets where time steps are in 5-year intervals.
#'
#' @return A tidy dataframe with regression results. This includes:
#' - `term`: The lead or lag variable names.
#' - `estimate`: Estimated coefficients.
#' - `std.error`: Standard errors.
#' - `conf.high`: Upper bound of the 95% confidence interval.
#' - `conf.low`: Lower bound of the 95% confidence interval.
#' - `relative_time`: Scaled relative time based on the specified `interval`.
#'
#' @details
#' This function is designed for panel data and supports time intervals other than 1 (e.g., 5-year intervals).
#' It automatically scales the relative time variable using the `interval` parameter.
#'
#' Steps:
#' 1. Compute the relative time for each observation as `(time - timing) / interval`.
#' 2. Generate lead and lag dummy variables within the specified ranges (`lead_range`, `lag_range`).
#' 3. Construct and estimate the fixed effects regression model using `fixest::feols`.
#' 4. Format the regression results into a tidy dataframe.
#'
#' If `interval > 1`, ensure that the specified `lead_range` and `lag_range` correspond to the number of time intervals, not the absolute number of years.
#'
#'
#' You can include additional covariates using the `covariates` argument.
#' These covariates will be added to the regression alongside the lead and lag dummy variables.
#' You can provide:
#' - an additive expression (e.g., `x1 + x2 + x3`), or
#' - a character vector (e.g., `c("x1", "x2", "x3")`).
#'
#' @examples
#' # Simulate panel data
#' df <- tibble::tibble(
#'   firm_id = rep(1:50, each = 10),  # 50 firms over 10 years
#'   state_id = rep(sample(1:10, size = 50, replace = TRUE), each = 10),
#'   year = rep(2000:2009, times = 50),
#'   is_treated = rep(sample(c(1, 0), size = 50, replace = TRUE, prob = c(0.5, 0.5)), each = 10),
#'   x1         = rnorm(500),           # Covariate 1
#'   x2         = rbinom(500, 1, 0.4),  # Covariate 2 (binary)
#'   x3         = runif(500, 0, 10),    # Covariate 3
#'   y = rnorm(500, mean = 0, sd = 1)  # Simulated outcome variable
#' )
#'
#' # Run event study
#' event_study <- run_es(
#'   data       = df,
#'   outcome    = y,
#'   treatment  = is_treated,
#'   time       = year,
#'   timing     = 2005,
#'   lead_range = 5,              # Corresponds to years 2000-2004 (relative time: -5 to -1)
#'   lag_range  = 4,              # Corresponds to years 2006-2009 (relative time: 1 to 4)
#'   fe         = firm_id + year,
#'   cluster    = "state_id",
#'   baseline   = -1,
#'   interval   = 1
#' )
#'
#' # Run event study with covariates
#' event_study_cov <- run_es(
#'   data       = df,
#'   outcome    = y,
#'   treatment  = is_treated,
#'   time       = year,
#'   timing     = 2005,
#'   lead_range = 5,
#'   lag_range  = 4,
#'   covariates = c("x1", "x2", "x3"),  # or `x1 + x2 + x3`
#'   fe         = firm_id + year,
#'   cluster    = "state_id",
#'   baseline   = -1,
#'   interval   = 1
#' )
#'
#' @export
run_es <- function(data,
                   outcome,
                   treatment,
                   time,
                   timing,
                   lead_range,
                   lag_range,
                   covariates = NULL,
                   fe,
                   cluster = NULL,
                   baseline = -1,
                   interval = 1) {

  # ---- 0. Helper function ----
  get_term_name <- function(i) {
    # Generate term names for lead and lag variables
    if (i < 0) {
      paste0("lead", abs(i))
    } else {
      paste0("lag", i)
    }
  }

  # ---- 0.1 Handle outcome as an expression ----
  # Outcome can be a column name or an expression (e.g., log(variable)).
  outcome_expr <- rlang::enexpr(outcome)

  # Convert treatment and time to symbols
  treatment_sym <- rlang::ensym(treatment)
  time_sym <- rlang::ensym(time)

  # ---- 0.2 Check for column existence ----
  # Ensure that the specified variables exist in the data
  if (rlang::is_symbol(outcome_expr)) {
    outcome_chr <- rlang::as_string(outcome_expr)
    if (!outcome_chr %in% colnames(data)) {
      stop(
        "The specified outcome ('", outcome_chr,
        "') does not exist in the dataframe. Please specify an existing column or use a valid expression."
      )
    }
  } else if (rlang::is_call(outcome_expr)) {
    # If outcome is a call (e.g., log(variable)), skip column existence check
  } else {
    stop(
      "The specified outcome must be either a column name (symbol) or a function call (e.g., log(variable))."
    )
  }

  # Check treatment and time
  treatment_chr <- rlang::as_string(treatment_sym)
  if (!treatment_chr %in% colnames(data)) {
    stop(
      "The specified treatment ('", treatment_chr,
      "') does not exist in the dataframe. Please specify an existing column."
    )
  }

  time_chr <- rlang::as_string(time_sym)
  if (!time_chr %in% colnames(data)) {
    stop(
      "The specified time ('", time_chr,
      "') does not exist in the dataframe. Please specify an existing column."
    )
  }

  # ----0.3 Process covariates (if provided) ----

  cov_text <- ""
  if (!rlang::quo_is_null(rlang::enquo(covariates))) {
    cov_expr <- rlang::enexpr(covariates)

    if (is.character(covariates)) {
      cov_vars <- covariates
    } else {
      parse_cov_expr <- function(expr) {
        if (rlang::is_symbol(expr)) {
          return(rlang::as_string(expr))
        } else if (rlang::is_call(expr, "+")) {
          c(parse_cov_expr(expr[[2]]), parse_cov_expr(expr[[3]]))
        } else {
          stop("Invalid covariates expression. Use `x1 + x2` or `c('x1', 'x2')`.")
        }
      }

      if (rlang::is_call(cov_expr, "+") || rlang::is_symbol(cov_expr)) {
        cov_vars <- parse_cov_expr(cov_expr)
      } else {
        stop("Invalid input for covariates.")
      }
    }

    # Check variables exist
    missing_cov <- cov_vars[!cov_vars %in% colnames(data)]
    if (length(missing_cov) > 0) {
      stop("Missing covariates: ", paste(missing_cov, collapse = ", "))
    }

    cov_text <- paste(cov_vars, collapse = "+")
  }

  # ---- 0.4 Process fixed effects (fe) variable ----
  # Allow fixed effects to be specified as an additive expression (e.g., firm_id + year)
  fe_expr <- rlang::enexpr(fe)
  parse_fe_expr <- function(expr) {
    if (rlang::is_symbol(expr)) {
      return(rlang::as_string(expr))
    } else if (rlang::is_call(expr, "+")) {
      # Recursively parse '+' calls to extract all variable names
      c(parse_fe_expr(expr[[2]]), parse_fe_expr(expr[[3]]))
    } else {
      stop("Invalid fixed effects expression. Please use a formula-like expression (e.g., firm_id + year).")
    }
  }
  fe_vars <- if (rlang::is_call(fe_expr, "+") || rlang::is_symbol(fe_expr)) {
    parse_fe_expr(fe_expr)
  } else if (is.vector(fe_expr)) {
    fe_expr
  } else {
    stop("Invalid input for fe.")
  }

  # Check that the fixed effects variables exist in the data
  if (length(fe_vars) > 0) {
    missing_fes <- fe_vars[!fe_vars %in% colnames(data)]
    if (length(missing_fes) > 0) {
      stop(
        "The specified fixed effects variable(s) (",
        paste(missing_fes, collapse = ", "),
        ") do not exist in the dataframe. Please specify existing columns."
      )
    }
  }

  # ---- 0.4 Check cluster if provided ----
  if (!is.null(cluster)) {
    cluster_chr <- rlang::as_string(dplyr::ensym(cluster))
    if (!cluster_chr %in% colnames(data)) {
      stop(
        "The specified cluster ('", cluster_chr,
        "') does not exist in the dataframe. Please specify an existing column."
      )
    }
  }

  # ---- 1. Create lead and lag variables ----
  # Compute relative time based on the timing and interval
  data <- data |>
    dplyr::mutate(
      relative_time = ( !!time_sym - timing ) / interval
    )

  # Check range of relative_time and issue warnings if necessary
  min_relative_time <- min(data$relative_time, na.rm = TRUE)
  max_relative_time <- max(data$relative_time, na.rm = TRUE)

  if (lead_range > abs(min_relative_time)) {
    warning(
      "The specified lead_range (", lead_range,
      ") exceeds the available range in the data on the lead side (",
      abs(min_relative_time), ")."
    )
  }
  if (lag_range > max_relative_time) {
    warning(
      "The specified lag_range (", lag_range,
      ") exceeds the available range in the data on the lag side (",
      max_relative_time, ")."
    )
  }

  # Filter data within the specified lead and lag range
  data <- data |>
    dplyr::filter(
      dplyr::between(relative_time, -lead_range, lag_range)
    )

  # Create dummy variables for each lead and lag period
  for (i in seq(-lead_range, lag_range, by = 1)) {
    col_name <- get_term_name(i)
    data <- data |>
      dplyr::mutate(
        !!col_name := dplyr::if_else(
          !!treatment_sym == 1 & (relative_time == i),
          1,
          0
        )
      )
  }

  # ---- 2. Construct and estimate the regression model ----
  # Convert outcome_expr to text for use in formula construction
  outcome_expr_text <- rlang::expr_text(outcome_expr)

  # Construct terms for regression
  all_terms <- c(
    paste0("lead", seq(lead_range, 1)),
    paste0("lag", seq(0, lag_range))
  )
  baseline_term <- get_term_name(baseline)
  included_terms <- setdiff(all_terms, baseline_term)
  RHS_formula <- paste(included_terms, collapse = "+")

  # Add covariates
  if (cov_text != "") {
    RHS_formula <- paste0(RHS_formula, "+", cov_text)
  }

  fe_formula <- paste(fe_vars, collapse = "+")

  # Build the regression formula, e.g., "log(variable) ~ lead1+lead2+...+covariates | firm_id+year"
  model_formula_text <- paste0(
    outcome_expr_text, " ~ ", RHS_formula, " | ", fe_formula
  )
  model_formula <- stats::as.formula(model_formula_text)

  # Estimate the model using fixest::feols
  if (!is.null(cluster)) {
    model <- fixest::feols(model_formula, data = data, cluster = cluster)
  } else {
    model <- fixest::feols(model_formula, data = data)
  }

  # ---- 3. Format the results ----
  # Extract results and format them for output
  result <- broom::tidy(model)

  # Add baseline term with an estimate of 0
  baseline_row <- tibble::tibble(
    term = baseline_term,
    estimate = 0,
    std.error = 0,
    statistic = NA_real_,
    p.value = NA_real_
  )

  # Reorder results and calculate confidence intervals
  full_order <- c(
    paste0("lead", seq(lead_range, 1)),
    paste0("lag", seq(0, lag_range))
  )
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

  # Add relative_time for visualization purposes
  rel_times <- c(seq(-lead_range, -1), seq(0, lag_range)) * interval
  rel_map <- tibble::tibble(
    term = factor(full_order, levels = full_order),
    relative_time = rel_times
  )
  result <- result |>
    dplyr::left_join(rel_map, by = "term") |>
    dplyr::mutate(
      is_baseline = (term == baseline_term)
    )

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
#' @examples
#' # Simulate panel data
#' df <- tibble::tibble(
#'   firm_id = rep(1:50, each = 10),  # 50 firms over 10 years
#'   state_id = rep(sample(1:10, size = 50, replace = TRUE), each = 10),
#'   year = rep(2000:2009, times = 50),
#'   is_treated = rep(sample(c(1, 0), size = 50, replace = TRUE, prob = c(0.5, 0.5)), each = 10),
#'   y = rnorm(500, mean = 0, sd = 1)  # Simulated outcome variable
#' )
#'
#' # Run event study
#' event_study <- run_es(
#'   data       = df,
#'   outcome    = y,
#'   treatment  = is_treated,
#'   time       = year,
#'   timing     = 2005,
#'   lead_range = 5,              # Corresponds to years 2000-2004 (relative time: -5 to -1)
#'   lag_range  = 4,              # Corresponds to years 2006-2009 (relative time: 1 to 4)
#'   fe         = firm_id + year,
#'   cluster    = "state_id",
#'   baseline   = -1,
#'   interval   = 1
#' )
#'
#' # Basic plot
#' plot_es(event_study)
#'
#' # Use error bars instead of ribbon confidence intervals
#' plot_es(event_study, type = "errorbar")
#'
#' # Adjust vertical reference line
#' plot_es(event_study, type = "errorbar", vline_val = -0.5)
#'
#' # Customize axis breaks and title
#' library(ggplot2)
#' plot_es(event_study, type = "errorbar") +
#'   ggplot2::scale_x_continuous(breaks = seq(-5, 4, by = 1)) +
#'   ggplot2::ggtitle("Result of Event Study")
#'
#' @export
plot_es <- function(data,
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
                    fill = "#B25D91FF") {
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
    # If we have the "is_baseline" column, set the baseline row's std.error to NA
    # so that the error bars disappear for the baseline row.
    if ("is_baseline" %in% colnames(data)) {
      data <- data |>
        dplyr::mutate(
          std.error = dplyr::if_else(is_baseline, NA_real_, std.error),
          conf_high = estimate + 1.96 * std.error,
          conf_low  = estimate - 1.96 * std.error
        )
    } else {
      # Fallback if is_baseline doesn't exist (no changes)
      data <- data |>
        dplyr::mutate(
          conf_high = estimate + 1.96 * std.error,
          conf_low  = estimate - 1.96 * std.error
        )
    }

    base_plot <- base_plot +
      ggplot2::geom_errorbar(
        data = data,
        ggplot2::aes(ymin = conf_low, ymax = conf_high),
        color = color,
        width = barwidth,
        linewidth = linewidth
      )
  }

  return(base_plot)
}
