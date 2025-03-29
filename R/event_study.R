#' Run Event Study with Fixed Effects
#'
#' This function performs an event study using fixed effects regression based on a panel dataset.
#' It generates dummy variables for each lead and lag period relative to the treatment timing,
#' applies optional covariates and fixed effects, and estimates the model using `fixest::feols`.
#'
#' @param data A dataframe containing the panel dataset.
#' @param outcome The outcome variable, specified unquoted. You may use a raw variable name
#' (e.g., `y`) or a transformation (e.g., `log(y)`).
#' @param treatment The binary treatment indicator (unquoted). Typically equals 1 in and after the treated period, 0 otherwise.
#' @param time The time variable (unquoted). Used to calculate the relative timing.
#' @param timing The time period when the treatment occurs for the treated units.
#' @param lead_range Number of pre-treatment periods to include as leads (e.g., 5 = `lead5`, `lead4`, ..., `lead1`).
#' @param lag_range Number of post-treatment periods to include as lags (e.g., 3 = `lag0`, `lag1`, `lag2`, `lag3`).
#' @param covariates Optional covariates to include in the regression. Must be supplied as a one-sided formula (e.g., `~ x1 + x2`).
#' @param fe Fixed effects to control for unobserved heterogeneity. Must be a one-sided formula (e.g., `~ id + year`).
#' @param cluster Clustering specification for robust standard errors. Accepts either:
#' \itemize{
#'   \item a character vector of column names (e.g., `c("id", "year")`), or
#'   \item a one-sided formula (e.g., `~ id + year` or `~ id^year`).
#' }
#' Cluster variables are internally re-evaluated after filtering for the estimation window.
#' @param baseline The relative time (e.g., `-1`) to use as the reference period.
#' The corresponding dummy variable will be excluded from the regression and added manually to the results with estimate 0.
#' Must lie within the specified `lead_range` and `lag_range`. If not, an error will be thrown.
#' @param interval The interval between time periods (e.g., use `5` for 5-year spaced panel). Default is `1`.
#'
#' @return A tidy dataframe with the event study regression results, containing:
#' \itemize{
#'   \item `term`: Name of the lead or lag dummy variable.
#'   \item `estimate`: Coefficient estimate.
#'   \item `std.error`: Standard error.
#'   \item `statistic`: t-statistic.
#'   \item `p.value`: p-value.
#'   \item `conf_high`: Upper bound of 95% confidence interval.
#'   \item `conf_low`: Lower bound of 95% confidence interval.
#'   \item `relative_time`: Time scaled relative to the treatment.
#'   \item `is_baseline`: Logical indicator for the baseline term (equals `TRUE` only for the excluded dummy).
#' }
#'
#' @details
#' This function is intended for difference-in-differences or event study designs with panel data.
#' It automatically:
#' \itemize{
#'   \item computes relative time: \code{(time - timing) / interval},
#'   \item generates dummy variables for specified leads and lags,
#'   \item removes the baseline term from estimation and appends it back post-estimation,
#'   \item uses `fixest::feols()` for fast and flexible estimation.
#' }
#' Both fixed effects and clustering are fully supported.
#'
#' @examples
#' \dontrun{
#' # Assume df is a panel dataset with variables: id, year, y, treat, x1, x2, var1, var2
#'
#' # Specifying two-way clustering over var1 and var2 using a character vector:
#' run_es(
#'   data       = df,
#'   outcome    = y,
#'   treatment  = treat,
#'   time       = year,
#'   timing     = 2005,
#'   lead_range = 2,
#'   lag_range  = 2,
#'   covariates = ~ x1 + x2,
#'   fe         = ~ id + year,
#'   cluster    = c("var1", "var2"),
#'   interval   = 1
#' )
#'
#' # Specifying two-way clustering over var1 and var2 using a one-sided formula:
#' run_es(
#'   data       = df,
#'   outcome    = y,
#'   treatment  = treat,
#'   time       = year,
#'   timing     = 2005,
#'   lead_range = 2,
#'   lag_range  = 2,
#'   covariates = ~ x1 + x2,
#'   fe         = ~ id + year,
#'   cluster    = ~ var1 + var2,
#'   interval   = 1
#' )
#'
#' # Using an interaction in the clustering specification:
#' run_es(
#'   data       = df,
#'   outcome    = y,
#'   treatment  = treat,
#'   time       = year,
#'   timing     = 2005,
#'   lead_range = 2,
#'   lag_range  = 2,
#'   covariates = ~ x1 + x2,
#'   fe         = ~ id + year,
#'   cluster    = ~ var1^var2,
#'   interval   = 1
#' )
#' }
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

  # ---- 0. Helper function: generate lead/lag term name ----
  get_term_name <- function(i) {
    if (i < 0) {
      paste0("lead", abs(i))
    } else {
      paste0("lag", i)
    }
  }

  # ---- 0.1 Helper function: process cluster argument ----
  process_cluster <- function(cluster, data) {
    if (is.null(cluster)) return(NULL)

    if (inherits(cluster, "formula")) {
      cl_vars <- all.vars(cluster)
      missing_vars <- cl_vars[!cl_vars %in% colnames(data)]
      if (length(missing_vars) > 0) {
        stop("The following cluster variables from the formula are not in data: ",
             paste(missing_vars, collapse = ", "))
      }
      return(cluster)
    } else if (is.character(cluster)) {
      missing_vars <- cluster[!cluster %in% colnames(data)]
      if (length(missing_vars) > 0) {
        stop("The following cluster variables are not in data: ",
             paste(missing_vars, collapse = ", "))
      }
      return(cluster)
    } else {
      stop("Invalid type for cluster argument. Please supply either a formula (e.g., ~ var1 + var2) or a character vector (e.g., c('var1', 'var2')).")
    }
  }

  # ---- 0.2 Process outcome, treatment, time ----
  outcome_expr <- rlang::enexpr(outcome)
  treatment_sym <- rlang::ensym(treatment)
  time_sym <- rlang::ensym(time)

  if (rlang::is_symbol(outcome_expr)) {
    outcome_chr <- rlang::as_string(outcome_expr)
    if (!outcome_chr %in% colnames(data)) {
      stop("The specified outcome ('", outcome_chr, "') does not exist in the dataframe.")
    }
  } else if (!rlang::is_call(outcome_expr)) {
    stop("The specified outcome must be either a column name or a function call (e.g., log(variable)).")
  }

  treatment_chr <- rlang::as_string(treatment_sym)
  if (!treatment_chr %in% colnames(data)) {
    stop("The specified treatment ('", treatment_chr, "') does not exist in the dataframe.")
  }

  time_chr <- rlang::as_string(time_sym)
  if (!time_chr %in% colnames(data)) {
    stop("The specified time ('", time_chr, "') does not exist in the dataframe.")
  }

  # ---- 0.3 Process covariates ----
  cov_text <- ""
  if (!rlang::quo_is_null(rlang::enquo(covariates))) {
    if (!inherits(covariates, "formula")) {
      stop("Invalid input for covariates. Please supply a one-sided formula (e.g., ~ x1 + log(x2)).")
    }
    cov_rhs <- rlang::f_rhs(covariates)
    cov_text <- rlang::expr_text(cov_rhs)
    cov_vars <- all.vars(cov_rhs)
    missing_cov <- cov_vars[!cov_vars %in% colnames(data)]
    if (length(missing_cov) > 0) {
      stop("Missing covariates: ", paste(missing_cov, collapse = ", "))
    }
  }

  # ---- 0.4 Process fixed effects (fe) ----
  if (!inherits(fe, "formula")) {
    stop("Invalid input for fe. Please supply a one-sided formula (e.g., ~ id + year).")
  }
  fe_rhs <- rlang::f_rhs(fe)
  fe_vars <- all.vars(fe_rhs)
  missing_fes <- fe_vars[!fe_vars %in% colnames(data)]
  if (length(missing_fes) > 0) {
    stop("The specified fixed effects variable(s) (", paste(missing_fes, collapse = ", "), ") do not exist in the dataframe.")
  }
  fe_text <- paste(fe_vars, collapse = "+")

  # ---- 0.5 Process cluster if provided ----
  if (!is.null(cluster)) {
    cluster <- process_cluster(cluster, data)
  }

  # ---- Check that baseline is within the lead/lag range ----
  if (baseline < -lead_range || baseline > lag_range) {
    stop("The specified baseline (", baseline,
         ") is outside the range defined by lead_range (", -lead_range,
         ") and lag_range (", lag_range, ").")
  }

  # ---- 1. Create lead and lag variables ----
  data <- data |>
    dplyr::mutate(
      relative_time = ( !!time_sym - timing ) / interval
    )

  min_relative_time <- min(data$relative_time, na.rm = TRUE)
  max_relative_time <- max(data$relative_time, na.rm = TRUE)

  if (lead_range > abs(min_relative_time)) {
    warning("The specified lead_range (", lead_range,
            ") exceeds the available range in the data on the lead side (", abs(min_relative_time), ").")
  }
  if (lag_range > max_relative_time) {
    warning("The specified lag_range (", lag_range,
            ") exceeds the available range in the data on the lag side (", max_relative_time, ").")
  }

  data <- data |>
    dplyr::filter(dplyr::between(relative_time, -lead_range, lag_range))

  for (i in seq(-lead_range, lag_range, by = 1)) {
    col_name <- get_term_name(i)
    data <- data |>
      dplyr::mutate(
        !!col_name := dplyr::if_else(!!treatment_sym == 1 & (relative_time == i), 1, 0)
      )
  }

  # ---- 2. Construct and estimate the regression model ----
  outcome_expr_text <- rlang::expr_text(outcome_expr)
  all_terms <- c(
    paste0("lead", seq(lead_range, 1)),
    paste0("lag", seq(0, lag_range))
  )

  baseline_term <- get_term_name(baseline)

  included_terms <- setdiff(all_terms, baseline_term)

  RHS_formula <- paste(included_terms, collapse = "+")
  if (cov_text != "") {
    RHS_formula <- paste0(RHS_formula, "+", cov_text)
  }

  model_formula_text <- paste0(outcome_expr_text, " ~ ", RHS_formula, " | ", fe_text)
  model_formula <- stats::as.formula(model_formula_text)

  if (!is.null(cluster)) {
    model <- fixest::feols(model_formula, data = data, cluster = cluster)
  } else {
    model <- fixest::feols(model_formula, data = data)
  }

  # ---- 3. Format the results ----
  result <- broom::tidy(model)

  baseline_row <- tibble::tibble(
    term = baseline_term,
    estimate = 0,
    std.error = 0,
    statistic = NA_real_,
    p.value = NA_real_
  )

  full_order <- c(
    paste0("lead", seq(lead_range, 1)),
    paste0("lag", seq(0, lag_range))
  )
  result <- result |>
    dplyr::bind_rows(baseline_row) |>
    dplyr::mutate(term = factor(term, levels = full_order)) |>
    dplyr::arrange(term) |>
    dplyr::mutate(
      conf_high = estimate + 1.96 * std.error,
      conf_low  = estimate - 1.96 * std.error
    ) |>
    dplyr::filter(!is.na(term))

  rel_times <- c(seq(-lead_range, -1), seq(0, lag_range)) * interval
  rel_map <- tibble::tibble(
    term = factor(full_order, levels = full_order),
    relative_time = rel_times
  )
  result <- result |>
    dplyr::left_join(rel_map, by = "term") |>
    dplyr::mutate(is_baseline = (term == baseline_term))

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
#' \dontrun{
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
#' }
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
