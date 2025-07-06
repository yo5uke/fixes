#' Event Study Estimation for Panel Data
#'
#' Runs an event study regression on panel data, automatically generating lead/lag dummies around a treatment event.
#'
#' @description
#' \code{run_es()} is a user-friendly event study wrapper for panel data, supporting both classic (universal timing) and staggered (unit-varying timing) settings. It generates lead/lag dummy variables around treatment, builds the regression formula, and returns tidy results with confidence intervals for arbitrary levels.
#'
#' @section Key Features:
#' \itemize{
#'   \item One-step event study: specify outcome, treatment, time, timing, and fixed effects directly.
#'   \item Supports both universal and staggered treatment timing.
#'   \item Flexible covariate, clustering, and weighting options.
#'   \item Customizable lead/lag window and baseline normalization.
#'   \item Returns a tidy \code{data.frame} (class \code{"es_result"}) with estimates and confidence intervals.
#' }
#'
#' @param data A data.frame with panel data. Must contain all required variables.
#' @param outcome Outcome variable (unquoted variable name or expression, e.g. \code{log(y)}).
#' @param treatment Treatment indicator (unquoted; 0/1 or logical).
#' @param time Time variable (unquoted; numeric or Date).
#' @param timing Treatment timing: a numeric or Date for universal timing, or a variable (unquoted) for staggered.
#' @param fe One-sided formula for fixed effects (e.g. \code{~ id + year}).
#' @param lead_range Number of pre-treatment periods to include (default: detected from data).
#' @param lag_range Number of post-treatment periods to include (default: detected from data).
#' @param covariates One-sided formula of additional controls (optional).
#' @param cluster Cluster variable(s), as a one-sided formula or character vector (optional).
#' @param weights Observation weights (formula, character, or unquoted variable).
#' @param baseline Baseline period for normalization (default: -1, i.e., one period before treatment).
#' @param interval Numeric; interval of the time variable (default: 1).
#' @param time_transform Logical; if \code{TRUE}, converts time variable to consecutive integer within units (requires \code{unit}).
#' @param unit Panel unit variable (required if \code{time_transform = TRUE}).
#' @param staggered Logical; if \code{TRUE}, treats timing as variable (default: \code{FALSE}).
#' @param conf.level Numeric vector of confidence levels (e.g. \code{c(0.90, 0.95, 0.99)}; default: \code{0.95}).
#'
#' @return
#' A data.frame with class \code{"es_result"}, containing:
#' \describe{
#'   \item{term}{Dummy variable name (\code{leadX}, \code{lagY})}
#'   \item{estimate, std.error, statistic, p.value}{Coefficient estimates, standard errors, test statistics, and p-values}
#'   \item{conf_low_XX, conf_high_XX}{Confidence intervals at each requested level}
#'   \item{relative_time}{Period relative to treatment (0 = event)}
#'   \item{is_baseline}{Logical, is this the baseline period (estimate always 0)}
#' }
#' Attributes: \code{lead_range}, \code{lag_range}, \code{baseline}, \code{interval}, \code{call}, \code{model_formula}, \code{conf.level}.
#'
#' @details
#' The function does not require explicit formula specification; dummy variables and model formula are constructed internally.
#' Baseline normalization (reference category) can be customized. Untreated units or units never treated are supported.
#' Missing values in key variables result in an error.
#'
#' @seealso
#' \code{\link{plot_es}}
#'
#' @examples
#' \dontrun{
#' # Simulated example
#' result <- run_es(
#'   data       = panel_data,
#'   outcome    = y,
#'   treatment  = treated_1998,
#'   time       = year,
#'   timing     = 1998,
#'   fe         = ~ firm_id + year,
#'   cluster    = ~ state_id,
#'   lead_range = 3,
#'   lag_range  = 3,
#'   conf.level = c(0.90, 0.95, 0.99)
#' )
#' plot_es(result)
#' }
#'
#' @author
#' Yosuke Abe
#'
#' @importFrom stats as.formula qnorm setNames
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#'
#' @export
run_es <- function(
    data,
    outcome,
    treatment,
    time,
    timing,
    fe,
    lead_range   = NULL,
    lag_range    = NULL,
    covariates   = NULL,
    cluster      = NULL,
    weights      = NULL,
    baseline     = -1,
    interval     = 1,
    time_transform = FALSE,
    unit         = NULL,
    staggered    = FALSE,
    conf.level   = 0.95
) {
  # --- Helper: resolve column name or allow function call ---
  resolve_column <- function(expr, data, allow_call = FALSE) {
    if (rlang::is_symbol(expr)) {
      var <- rlang::as_string(expr)
      if (!var %in% names(data)) stop("Column '", var, "' not found in data.")
      return(var)
    } else if (allow_call && rlang::is_call(expr)) {
      return(rlang::expr_text(expr))
    } else if (is.character(expr) && expr %in% names(data)) {
      return(expr)
    } else {
      stop("Invalid column reference: ", rlang::expr_text(expr))
    }
  }

  stopifnot(is.data.frame(data))
  if (!is.numeric(interval) || interval <= 0) stop("interval must be positive.")

  # outcome, treatment, time: get column names or function calls
  outcome_chr   <- resolve_column(rlang::enexpr(outcome), data, allow_call = TRUE)
  treatment_chr <- resolve_column(rlang::enexpr(treatment), data)
  time_chr      <- resolve_column(rlang::enexpr(time), data)

  # timing: numeric/date (uniform) or variable (staggered)
  if (staggered) {
    timing_chr <- resolve_column(rlang::enexpr(timing), data)
    if (!timing_chr %in% names(data))
      stop("Staggered timing variable '", timing_chr, "' not found.")
  } else {
    timing_val <- timing
    # Accept both date-string and number
    if (is.character(timing_val) && !timing_val %in% names(data)) {
      try_date <- suppressWarnings(as.Date(timing_val, format = "%Y-%m-%d"))
      if (!is.na(try_date)) timing_val <- try_date
    }
    if (inherits(timing_val, "Date") && !inherits(data[[time_chr]], "Date")) {
      stop("timing is Date but time variable is not Date.")
    }
    # For Date, coerce to numeric for calculation (matching feols behavior)
    if (inherits(timing_val, "Date") && inherits(data[[time_chr]], "Date")) {
      timing_num <- as.numeric(timing_val)
      data[[time_chr]] <- as.numeric(data[[time_chr]])
      timing <- timing_num
    }
  }

  # --- Cluster argument validation (fix for test stability) ---
  if (!is.null(cluster)) {
    if (inherits(cluster, "formula")) {
      # OK
    } else if (is.character(cluster)) {
      # Single: must be colname. Vector: must be nrow(data)
      if (length(cluster) == 1) {
        if (!cluster %in% names(data)) {
          stop("Invalid type for cluster argument: character value must match a column name in data.")
        }
      } else if (length(cluster) == nrow(data)) {
        # Accept as vector (rare, but technically possible)
      } else {
        stop("Invalid type for cluster argument: character vector must be length 1 (column name) or length nrow(data).")
      }
    } else {
      stop("Invalid type for cluster argument: must be formula or character column name or vector.")
    }
  }

  # time_transform support (panel index to consecutive integers)
  if (time_transform) {
    if (missing(unit)) stop("Must specify the `unit` argument for time_transform = TRUE.")
    unit_chr <- resolve_column(rlang::enexpr(unit), data)
    data <- data %>%
      dplyr::group_by(.data[[unit_chr]]) %>%
      dplyr::arrange(.data[[time_chr]], .by_group = TRUE) %>%
      dplyr::mutate(time_index = dplyr::row_number()) %>%
      dplyr::ungroup()
    time_chr <- "time_index"
  } else {
    if (!is.null(unit)) warning("unit is ignored unless time_transform = TRUE.")
  }

  # Covariates: check as formula or NULL
  cov_text <- ""
  if (!is.null(covariates)) {
    if (!inherits(covariates, "formula"))
      stop("covariates must be a one-sided formula (e.g., ~ x1 + log(x2)).")
    cov_rhs <- rlang::f_rhs(covariates)
    cov_text <- rlang::expr_text(cov_rhs)
  }

  # Fixed effects: must be formula
  if (!inherits(fe, "formula")) stop("fe must be a one-sided formula, e.g., ~ id + year.")

  # --- Create relative_time ---
  if (staggered) {
    data$relative_time <- (data[[time_chr]] - data[[timing_chr]]) / interval
  } else {
    data$relative_time <- (data[[time_chr]] - timing) / interval
  }
  if (all(is.na(data$relative_time)))
    stop("relative_time is NA for all rows. Check your timing/time inputs.")

  # --- Automatically determine lead/lag range ---
  min_rt <- floor(min(data$relative_time, na.rm = TRUE))
  max_rt <- ceiling(max(data$relative_time, na.rm = TRUE))
  if (is.null(lead_range)) lead_range <- abs(min_rt)
  if (is.null(lag_range))  lag_range  <- max_rt

  # --- Baseline check ---
  if (baseline < -lead_range || baseline > lag_range) {
    stop("The specified baseline (", baseline,
         ") is outside the range defined by lead_range (", -lead_range,
         ") and lag_range (", lag_range, ").")
  }

  # --- Dummy term names ---
  get_term_name <- function(i) if (i < 0) paste0("lead", abs(i)) else paste0("lag", i)
  all_terms <- vapply(seq(-lead_range, lag_range), get_term_name, character(1))
  baseline_term <- get_term_name(baseline)
  included_terms <- setdiff(all_terms, baseline_term)

  # --- Overwriting warning ---
  overwritten <- all_terms[all_terms %in% names(data)]
  if (length(overwritten) > 0) {
    warning("These columns already exist and will be overwritten: ", paste(overwritten, collapse = ", "))
  }

  # --- Generate lead/lag dummies ---
  for (i in seq(-lead_range, lag_range)) {
    col_name <- get_term_name(i)
    data[[col_name]] <- ifelse(
      !is.na(data$relative_time) &
        as.logical(data[[treatment_chr]]) &
        abs(data$relative_time - i) < 1e-8,
      1L, 0L
    )
  }

  # --- Regression formula (event dummies + covariates + FE) ---
  RHS_formula <- paste(setdiff(all_terms, baseline_term), collapse = " + ")
  if (cov_text != "") RHS_formula <- paste(RHS_formula, cov_text, sep = " + ")
  formula_string <- paste0(outcome_chr, " ~ ", RHS_formula, " | ",
                           rlang::expr_text(rlang::f_rhs(fe)))
  model_formula <- as.formula(formula_string)

  # --- feols args: pass cluster/weights/covariates/fe as is ---
  model_args <- list(model_formula, data = data)
  if (!is.null(cluster)) model_args$cluster <- cluster
  if (!is.null(weights)) model_args$weights <- weights

  # --- Run regression ---
  model <- tryCatch(
    do.call(fixest::feols, model_args),
    error = function(e) {
      stop("Model estimation failed: ", e$message)
    }
  )

  # --- Collect tidy results ---
  result <- broom::tidy(model)

  # --- Baseline row: always 0 by construction ---
  baseline_row <- tibble::tibble(
    term      = baseline_term,
    estimate  = 0,
    std.error = 0,
    statistic = NA_real_,
    p.value   = NA_real_
  )

  # --- Order & rel_time mapping ---
  full_order <- all_terms
  rel_times  <- seq(-lead_range, lag_range) * interval
  rel_map <- tibble::tibble(
    term         = factor(full_order, levels = full_order),
    relative_time = rel_times
  )

  # --- Bind, arrange, add confidence intervals for each conf.level ---
  conf.level <- sort(unique(conf.level))
  zvals <- setNames(qnorm(1 - (1 - conf.level) / 2), paste0("z_", conf.level))
  result <- result %>%
    dplyr::bind_rows(baseline_row) %>%
    dplyr::mutate(term = factor(term, levels = full_order)) %>%
    dplyr::arrange(term) %>%
    dplyr::filter(!is.na(term))

  # Confidence intervals for all requested levels
  for (cl in conf.level) {
    z <- qnorm(1 - (1 - cl)/2)
    cname_lo <- paste0("conf_low_", sub("^0\\.", "", as.character(cl*100)))
    cname_hi <- paste0("conf_high_", sub("^0\\.", "", as.character(cl*100)))
    result[[cname_lo]] <- result$estimate - z * result$std.error
    result[[cname_hi]] <- result$estimate + z * result$std.error
  }

  # Attach rel_time, baseline indicator
  result <- result %>%
    dplyr::left_join(rel_map, by = "term") %>%
    dplyr::mutate(is_baseline = (term == baseline_term))

  # --- Attach meta attributes ---
  attr(result, "lead_range")    <- lead_range
  attr(result, "lag_range")     <- lag_range
  attr(result, "baseline")      <- baseline
  attr(result, "interval")      <- interval
  attr(result, "call")          <- match.call()
  attr(result, "model_formula") <- formula_string
  attr(result, "conf.level")    <- conf.level
  class(result) <- c("es_result", "data.frame")

  return(result)
}


#' Plot Event Study Results
#'
#' Visualizes event study estimates and confidence intervals produced by \code{run_es()}.
#'
#' @description
#' \code{plot_es()} produces standard event study plots, showing estimated effects and confidence intervals across event time. Supports both ribbon and errorbar styles, multiple confidence levels, and several ggplot2 themes.
#'
#' @param data Data frame returned by \code{run_es()}.
#' @param ci_level Confidence level for intervals (numeric, e.g. 0.95). Must match columns like 'conf_low_95'.
#' @param type Confidence interval style: \code{"ribbon"} (default) or \code{"errorbar"}.
#' @param vline_val X value for vertical reference line (default: 0).
#' @param vline_color Color for vertical line.
#' @param hline_val Y value for horizontal reference line (default: 0).
#' @param hline_color Color for horizontal line.
#' @param linewidth Width of estimate line or error bars.
#' @param pointsize Point size for estimates.
#' @param alpha Transparency for ribbon (default: 0.2).
#' @param barwidth Width of error bars (if type = "errorbar").
#' @param color Color for lines and points.
#' @param fill Fill color for ribbon.
#' @param theme_style ggplot2 theme: \code{"bw"} (default), \code{"minimal"}, or \code{"classic"}.
#'
#' @return
#' A ggplot2 object.
#'
#' @seealso
#' \code{\link{run_es}}
#'
#' @examples
#' \dontrun{
#' result <- run_es(...)
#' plot_es(result, ci_level = 0.95, type = "ribbon")
#' }
#'
#' @author
#' Yosuke Abe
#'
#' @export
plot_es <- function(
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
    theme_style = "bw"
) {
  # --- 1. Select confidence interval columns based on requested level ---
  ci_str <- sprintf("%.0f", ci_level * 100)
  conf_low_col <- paste0("conf_low_", ci_str)
  conf_high_col <- paste0("conf_high_", ci_str)
  if (!conf_low_col %in% names(data)) conf_low_col <- "conf_low_95"
  if (!conf_high_col %in% names(data)) conf_high_col <- "conf_high_95"

  plot_data <- data

  # --- 2. Initialize ggplot base with reference lines and points ---
  base_plot <-
    ggplot2::ggplot(
      data = plot_data,
      ggplot2::aes(x = relative_time, y = estimate, group = 1)
    ) +
    ggplot2::geom_vline(
      xintercept = vline_val, linetype = "dashed", color = vline_color
    ) +
    ggplot2::geom_hline(
      yintercept = hline_val, linetype = "dashed", color = hline_color
    ) +
    ggplot2::geom_point(
      size = pointsize, color = color
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(
        floor(min(plot_data$relative_time, na.rm = TRUE)),
        ceiling(max(plot_data$relative_time, na.rm = TRUE)), by = 1
      )
    ) +
    ggplot2::labs(
      x = "Relative Time to Treatment",
      y = sprintf("Estimate and %.0f%% Confidence Interval", ci_level * 100)
    )

  # --- 3. Add confidence intervals (ribbon or errorbar) ---
  if (type == "ribbon") {
    base_plot <- base_plot +
      ggplot2::geom_ribbon(
        ggplot2::aes(
          ymin = .data[[conf_low_col]],
          ymax = .data[[conf_high_col]]
        ),
        fill = fill, alpha = alpha
      ) +
      ggplot2::geom_line(linewidth = linewidth, color = color)
  } else if (type == "errorbar") {
    base_plot <- base_plot +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          ymin = .data[[conf_low_col]],
          ymax = .data[[conf_high_col]]
        ),
        color = color, width = barwidth, linewidth = linewidth
      )
  } else {
    stop("Invalid 'type'. Please choose 'ribbon' or 'errorbar'.")
  }

  # --- 4. Set plot theme style ---
  theme_style <- match.arg(theme_style, c("bw", "minimal", "classic"))
  if (theme_style == "bw") {
    base_plot <- base_plot + ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  } else if (theme_style == "minimal") {
    base_plot <- base_plot + ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  } else if (theme_style == "classic") {
    base_plot <- base_plot + ggplot2::theme_classic()
  }

  return(base_plot)
}
