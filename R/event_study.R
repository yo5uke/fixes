#' Run Event Study with Fixed Effects
#'
#' Performs an event study analysis using fixed effects regression on panel data.
#' The function automatically generates lead and lag dummies for each relative period around treatment, supports covariates and flexible fixed effects, and allows for clustered standard errors and observation weights.
#'
#' @param data A data frame containing the panel dataset.
#' @param outcome The outcome variable, unquoted. You can supply a raw variable (e.g., `y`) or a function call (e.g., `log(y)`).
#' @param treatment Treatment assignment indicator (unquoted). Should be binary (`0/1` or logical). Typically equals 1 in and after the treated period, 0 otherwise.
#' @param time The time variable (unquoted). Used for relative period calculation.
#' @param staggered Logical. If `TRUE`, allows each unit to have its own treatment timing (supports staggered adoption). If so, supply `timing` as a variable name. Default is `FALSE`.
#' @param timing If `staggered = FALSE`, supply a single numeric or date value (e.g., `2005` or `"2005-01-01"`). If `staggered = TRUE`, supply the unquoted variable indicating each unit's treatment time. Never-treated units (with `NA`) are allowed and automatically included as controls.
#'   If `time_transform = TRUE`, specify `timing` as an integer corresponding to the transformed index.
#' @param lead_range Number of pre-treatment periods (leads) to include (e.g., 5 creates `lead5`, ..., `lead1`). If `NULL`, automatically determined.
#' @param lag_range Number of post-treatment periods (lags) to include (e.g., 3 creates `lag0`, `lag1`, `lag2`, `lag3`). If `NULL`, automatically determined.
#' @param covariates Optional covariates for the regression. Must be a one-sided formula (e.g., `~ x1 + x2`). Default is `NULL`.
#' @param fe Fixed effects specification, as a one-sided formula (e.g., `~ id + year`). Required.
#' @param cluster Cluster-robust standard errors. Accepts a one-sided formula (e.g., `~ id`), or a character vector of column names. Default is `NULL`.
#' @param weights Optional observation weights. Accepts a one-sided formula (e.g., `~ wt`), character string, or bare variable name. Default is `NULL`.
#' @param baseline Which relative period to use as the omitted (reference) period (e.g., `-1`). This dummy is excluded from estimation and added to the results with estimate 0.
#' @param interval The interval between time periods (e.g., `1` for yearly data, `5` for 5-year periods). Default is `1`.
#' @param time_transform Logical. If `TRUE`, time is replaced by a sequential integer per unit (useful for irregular panels). Default is `FALSE`.
#' @param unit Panel unit identifier. Required if `time_transform = TRUE`. Must be an unquoted variable name (e.g., `id`).
#'
#' @return A tibble containing the event study estimates:
#' - `term`: Lead or lag dummy name (e.g., `"lead3"`, `"lag0"`)
#' - `estimate`: Coefficient estimate
#' - `std.error`: Standard error
#' - `statistic`: t-statistic
#' - `p.value`: p-value
#' - `conf_high`: Upper 95% confidence bound
#' - `conf_low`: Lower 95% confidence bound
#' - `relative_time`: Relative period (scaled by `interval`)
#' - `is_baseline`: Logical, `TRUE` only for the omitted baseline period
#'
#' @details
#' This function streamlines event study regression for panel data with flexible support for:
#'
#' - Staggered adoption: units with `NA` in `timing` are included as controls (all event dummies zero).
#' - Relative time calculation: `(time - timing) / interval`.
#' - Automatic dummy generation for specified leads and lags.
#' - Omission of the baseline period from estimation and its re-addition with estimate 0.
#' - Optional transformation of time to a unit-specific sequence (`time_transform = TRUE`), allowing for irregular or gapped panel structures.
#'
#' **Collinearity:**
#' If some covariates are perfectly collinear with fixed effects or other regressors, they are automatically dropped from the regression. A message will be displayed listing dropped variables.
#'
#' @examples
#' \dontrun{
#' # Minimal use: simple DiD with two-way fixed effects
#' run_es(
#'   data = df,
#'   outcome = y,
#'   treatment = treat,
#'   time = year,
#'   timing = 2000,
#'   lead_range = 2,
#'   lag_range = 2,
#'   fe = ~ id + year,
#'   baseline = -1
#' )
#'
#' # With weights, cluster, and covariates
#' run_es(
#'   data = df,
#'   outcome = y,
#'   treatment = treat,
#'   time = year,
#'   timing = 2000,
#'   lead_range = 2,
#'   lag_range = 3,
#'   covariates = ~ x1 + x2,
#'   fe = ~ id + year,
#'   cluster = ~ id,
#'   weights = ~ popwt,
#'   baseline = -1
#' )
#'
#' # Staggered adoption: timing is unit-specific
#' run_es(
#'   data = df,
#'   outcome = y,
#'   treatment = treat,
#'   time = year,
#'   staggered = TRUE,
#'   timing = treat_time,
#'   lead_range = 3,
#'   lag_range = 4,
#'   fe = ~ id + year,
#'   cluster = ~ id,
#'   baseline = -1
#' )
#' }
#' @importFrom stats as.formula
#' @export
run_es <- function(
    data,
    outcome,
    treatment,
    time,
    staggered = FALSE,
    timing,
    lead_range = NULL,
    lag_range = NULL,
    covariates = NULL,
    fe,
    cluster = NULL,
    weights = NULL,
    baseline = -1,
    interval = 1,
    time_transform = FALSE,
    unit = NULL
) {

  # ---- 0. Helper Functions ----
  get_term_name <- function(i) {
    if (i < 0) paste0("lead", abs(i)) else paste0("lag", i)
  }

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
      stop("Invalid type for cluster argument. Please supply either a formula or character vector.")
    }
  }

  # ---- 1. Validate Core Arguments ----
  if (!is.data.frame(data)) stop("`data` must be a data.frame.")
  if (!is.numeric(interval) || length(interval) != 1 || interval <= 0) {
    stop("`interval` must be a single positive numeric value.")
  }

  outcome_expr <- rlang::enexpr(outcome)
  treatment_sym <- rlang::ensym(treatment)
  time_sym <- rlang::ensym(time)

  # outcome: symbol or call
  if (rlang::is_symbol(outcome_expr)) {
    outcome_chr <- rlang::as_string(outcome_expr)
    if (!outcome_chr %in% colnames(data)) {
      stop("The specified outcome ('", outcome_chr, "') does not exist in the dataframe.")
    }
  } else if (!rlang::is_call(outcome_expr)) {
    stop("`outcome` must be either a column name or a function call (e.g., log(variable)).")
  }

  # treatment: column must exist and be binary (0/1 or logical)
  treatment_chr <- rlang::as_string(treatment_sym)
  if (!treatment_chr %in% colnames(data)) {
    stop("The specified treatment ('", treatment_chr, "') does not exist in the dataframe.")
  }
  if (!all(data[[treatment_chr]] %in% c(0, 1, NA)) && !is.logical(data[[treatment_chr]])) {
    stop("`treatment` must be a binary (0/1 or logical) variable.")
  }

  # time: must exist, be numeric or Date, and not missing
  time_chr <- rlang::as_string(time_sym)
  if (!time_chr %in% colnames(data)) {
    stop("The specified time ('", time_chr, "') does not exist in the dataframe.")
  }
  if (!is.numeric(data[[time_chr]]) && !inherits(data[[time_chr]], "Date")) {
    stop("The `time` variable must be either numeric or Date. Please convert it before passing to `run_es()`.")
  }
  if (any(is.na(data[[time_chr]]))) {
    stop("Missing values detected in `time`. Please impute or remove before proceeding.")
  }

  # ---- 2. Process timing for (non-)staggered ----
  if (staggered && time_transform) {
    stop("The combination of staggered = TRUE and time_transform = TRUE is not supported.")
  }
  timing_sym <- NULL
  if (staggered) {
    timing_sym <- rlang::ensym(timing)
    timing_chr <- rlang::as_string(timing_sym)
    if (!timing_chr %in% colnames(data)) {
      stop("The specified timing variable ('", timing_chr, "') does not exist in the dataframe.")
    }
    # NA in timing is now ALLOWED (never-treated units), no stop
  } else {
    timing_val <- timing
    if (is.character(timing_val)) {
      try_date <- suppressWarnings(as.Date(timing_val, format = "%Y-%m-%d"))
      if (!is.na(try_date)) {
        timing_val <- try_date
      } else {
        stop("When timing is a string, it must be a valid date (e.g., '1998-01-01').")
      }
    }
    if (inherits(timing_val, "Date")) {
      if (!inherits(data[[time_chr]], "Date")) {
        stop("When timing is a Date, time variable must also be of Date type.")
      }
      timing_val <- as.numeric(timing_val)
      data[[time_chr]] <- as.numeric(data[[time_chr]])
    }
    if (!is.numeric(timing_val)) {
      stop("When staggered = FALSE, timing must be numeric or coercible to Date.")
    }
    timing <- timing_val
  }

  # ---- 3. Process time_transform argument ----
  if (time_transform) {
    if (missing(unit)) {
      stop("When time_transform = TRUE, you must specify the `unit` argument (e.g., unit = id).")
    }
    if (interval != 1) {
      warning("When time_transform = TRUE, interval is ignored and has been set to 1.")
      interval <- 1
    }
    id_sym <- rlang::ensym(unit)
    id_chr <- rlang::as_string(id_sym)
    if (!id_chr %in% colnames(data)) {
      stop("The specified unit variable ('", id_chr, "') does not exist in the dataframe.")
    }
    data <- data |>
      dplyr::group_by(!!id_sym) |>
      dplyr::arrange(!!time_sym, .by_group = TRUE) |>
      dplyr::mutate(time_index = dplyr::row_number()) |>
      dplyr::ungroup()
    time_sym <- rlang::sym("time_index")
  }
  if (!time_transform && !is.null(unit)) {
    warning("The `unit` argument is ignored because `time_transform = FALSE`.")
  }

  # ---- 4. Validate covariates ----
  cov_text <- ""
  if (!rlang::quo_is_null(rlang::enquo(covariates))) {
    if (!inherits(covariates, "formula")) {
      stop("`covariates` must be a one-sided formula (e.g., ~ x1 + log(x2)).")
    }
    cov_rhs <- rlang::f_rhs(covariates)
    cov_text <- rlang::expr_text(cov_rhs)
    cov_vars <- all.vars(cov_rhs)
    missing_cov <- cov_vars[!cov_vars %in% colnames(data)]
    if (length(missing_cov) > 0) {
      stop("Missing covariates: ", paste(missing_cov, collapse = ", "))
    }
    if (length(cov_vars) == 0) {
      stop("No covariates specified in the formula.")
    }
  }

  # ---- 5. Validate fixed effects ----
  if (!inherits(fe, "formula")) {
    stop("`fe` must be a one-sided formula (e.g., ~ id + year).")
  }
  fe_rhs <- rlang::f_rhs(fe)
  fe_vars <- all.vars(fe_rhs)
  missing_fes <- fe_vars[!fe_vars %in% colnames(data)]
  if (length(missing_fes) > 0) {
    stop("The specified fixed effects variable(s) (", paste(missing_fes, collapse = ", "), ") do not exist in the dataframe.")
  }
  if (length(fe_vars) == 0) {
    stop("No fixed effects specified in the formula.")
  }
  fe_text <- paste(fe_vars, collapse = "+")

  # ---- 6. Validate and process cluster/weights ----
  if (!is.null(cluster)) {
    cluster <- process_cluster(cluster, data)
  }
  if (!is.null(weights)) {
    weights_expr <- rlang::enexpr(weights)
    weights_vars <- NULL
    if (is.character(weights)) {
      weights_vars <- weights
    } else if (inherits(weights, "formula")) {
      weights_vars <- all.vars(weights)
    } else if (is.symbol(weights_expr)) {
      weights_vars <- rlang::as_string(weights_expr)
    }
    if (!is.null(weights_vars)) {
      missing_weights <- weights_vars[!weights_vars %in% colnames(data)]
      if (length(missing_weights) > 0) {
        stop("The specified weights variable(s) (", paste(missing_weights, collapse = ", "), ") do not exist in the dataframe.")
      }
      if (any(is.na(data[[weights_vars[1]]]))) {
        stop("Missing values detected in weights variable.")
      }
      if (any(data[[weights_vars[1]]] < 0)) {
        stop("All weights must be non-negative.")
      }
    }
  }

  # ---- 7. Create relative_time (NA allowed for never-treated) ----
  data <- data |>
    dplyr::mutate(
      relative_time = if (staggered)
        (!!time_sym - !!timing_sym) / interval
      else
        (!!time_sym - timing) / interval
    )
  if (all(is.na(data$relative_time))) {
    stop("relative_time is NA for all observations. Check time and timing arguments.")
  }

  # ---- 8. Set lead/lag ranges automatically if needed ----
  if (is.null(lead_range) || is.null(lag_range)) {
    min_rt <- floor(min(data$relative_time, na.rm = TRUE))
    max_rt <- ceiling(max(data$relative_time, na.rm = TRUE))
    if (is.null(lead_range)) lead_range <- abs(min_rt)
    if (is.null(lag_range))  lag_range  <- max_rt
  }

  # ---- 9. Baseline and Term Range Checks ----
  if (baseline < -lead_range || baseline > lag_range) {
    stop("The specified baseline (", baseline,
         ") is outside the range defined by lead_range (", -lead_range,
         ") and lag_range (", lag_range, ").")
  }
  if (lead_range < 0 || lag_range < 0) {
    stop("lead_range and lag_range must be non-negative.")
  }

  # ---- 10. Warn if overwriting existing columns ----
  existing_terms <- vapply(seq(-lead_range, lag_range), get_term_name, character(1))
  overwritten <- existing_terms[existing_terms %in% names(data)]
  if (length(overwritten) > 0) {
    warning("The following columns already exist in the data and will be overwritten: ",
            paste(overwritten, collapse = ", "))
  }

  # ---- 11. Create lead/lag dummy variables robustly ----
  for (i in seq(-lead_range, lag_range, by = 1)) {
    col_name <- get_term_name(i)
    # NA relative_time always yields dummy = 0 (never-treated)
    data[[col_name]] <- ifelse(
      !is.na(data$relative_time) &
        as.logical(data[[treatment_chr]]) &
        abs(data$relative_time - i) < 1e-5,
      1L, 0L
    )
  }

  # ---- 12. Build Regression Formula ----
  outcome_expr_text <- rlang::expr_deparse(outcome_expr)
  all_terms <- c(
    paste0("lead", seq(lead_range, 1)),
    paste0("lag", seq(0, lag_range))
  )
  baseline_term <- get_term_name(baseline)
  included_terms <- setdiff(all_terms, baseline_term)
  if (length(included_terms) == 0) {
    stop("No event study terms are included in the model (all are set as baseline).")
  }
  RHS_formula <- paste(included_terms, collapse = "+")
  if (cov_text != "") {
    RHS_formula <- paste0(RHS_formula, "+", cov_text)
  }
  formula_string <- paste0(outcome_expr_text, " ~ ", RHS_formula, " | ", fe_text)
  model_formula <- as.formula(formula_string)

  # ---- 13. Run Regression (with/without weights) ----
  model <- tryCatch(
    {
      if (!is.null(weights)) {
        fixest::feols(model_formula, data = data, cluster = cluster, weights = rlang::enexpr(weights))
      } else {
        fixest::feols(model_formula, data = data, cluster = cluster)
      }
    },
    error = function(e) {
      stop("Model estimation failed. Check your design matrix and arguments. Error: ", e$message)
    }
  )

  # ---- 14. Collect Results ----
  result <- broom::tidy(model)

  # Add baseline row (always zero)
  baseline_row <- tibble::tibble(
    term = baseline_term,
    estimate = 0,
    std.error = 0,
    statistic = NA_real_,
    p.value = NA_real_
  )

  # Full order for result
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

  # ---- 15. Attach relative_time mapping, add meta attributes, return ----
  rel_times <- c(seq(-lead_range, -1), seq(0, lag_range)) * interval
  rel_map <- tibble::tibble(
    term = factor(full_order, levels = full_order),
    relative_time = rel_times
  )
  result <- result |>
    dplyr::left_join(rel_map, by = "term") |>
    dplyr::mutate(is_baseline = (term == baseline_term))

  # Attach meta info as attributes
  attr(result, "lead_range") <- lead_range
  attr(result, "lag_range") <- lag_range
  attr(result, "baseline") <- baseline
  attr(result, "interval") <- interval
  attr(result, "call") <- match.call()
  attr(result, "model_formula") <- formula_string
  class(result) <- c("es_result", "data.frame")

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
    ggplot2::scale_x_continuous(
      breaks = seq(floor(min(data$relative_time, na.rm = TRUE)),
                   ceiling(max(data$relative_time, na.rm = TRUE)),
                   by = 1)
    ) +
    ggplot2::labs(
      x = "Relative Time to Treatment",
      y = "Estimate and 95% Confidence Interval"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank()
    )

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
