#' Event Study Estimation for Panel Data
#'
#' Runs an event study regression on panel data, supporting both classic (universal timing) and staggered (unit-varying timing via \code{sunab}).
#' The function builds the design (lead/lag factor or \code{sunab}), estimates with \pkg{fixest}, and returns a tidy table with metadata.
#'
#' @section Key Features:
#' \itemize{
#'   \item One-step event study: specify outcome, treatment, time, timing, fixed effects, and (optionally) covariates.
#'   \item Switch between Classic (factor expansion) and Staggered-SAFE (\code{method = "sunab"}).
#'   \item Flexible clustering, weights, and VCOV choices (e.g., \code{vcov = "HC1" | "HC3" | "CR2" | "iid" ...}).
#'   \item Automatic lead/lag window detection and customizable baseline period.
#'   \item Returns an \code{"es_result"} object compatible with \code{print()} and \code{autoplot()}.
#' }
#'
#' @section Performance Optimizations:
#' The function has been optimized for computational efficiency:
#' \itemize{
#'   \item Minimal data frame copying - input data is not modified
#'   \item Single-pass metadata computation where possible
#'   \item Vectorized confidence interval calculations
#'   \item Early input validation to fail fast on invalid inputs
#'   \item Efficient time transformation using base R functions
#' }
#'
#' @param data A data.frame containing panel data.
#' @param outcome Unquoted outcome (name or expression, e.g., \code{log(y)}).
#' @param treatment Unquoted treatment indicator (0/1 or logical). Used only when \code{method = "classic"}.
#' @param time Unquoted time variable (numeric or Date).
#' @param timing For \code{classic}: a numeric/Date (universal) or a variable (unquoted) if \code{staggered = TRUE}. For \code{sunab}: an unquoted variable with adoption time.
#' @param fe One-sided fixed-effects formula, e.g., \code{~ id + year}.
#' @param lead_range,lag_range Integers for pre/post windows. If \code{NULL}, determined automatically.
#' @param covariates One-sided formula of additional controls, e.g., \code{~ x1 + log(x2)}.
#' @param cluster Cluster specification (one-sided formula like \code{~ id + year}, a single character column name, or a vector of length \code{nrow(data)}).
#' @param weights Observation weights (a name/one-sided formula or a numeric vector of length \code{nrow(data)}).
#' @param baseline Integer baseline period (default \code{-1}); used only when \code{method = "classic"}.
#' @param interval Numeric spacing of the time variable (default \code{1}; ignored internally for Dates).
#' @param time_transform Logical; if \code{TRUE}, creates consecutive integer time within unit.
#' @param unit Unit identifier variable (required when \code{time_transform = TRUE}); also used for metadata when supplied.
#' @param staggered Logical; if \code{TRUE}, \code{timing} is a variable (classic) or is used by \code{sunab}.
#' @param method Either \code{"classic"} or \code{"sunab"} (default: \code{"classic"}).
#' @param conf.level Numeric vector of confidence levels (default \code{c(0.90, 0.95, 0.99)} for interactive plotting). Specify a single value like \code{0.95} if only one level is needed.
#' @param vcov VCOV type passed to \code{fixest::vcov()} or used via \code{broom::tidy(vcov = ...)}. Default \code{"HC1"}.
#' @param vcov_args List of additional arguments forwarded to \code{fixest::vcov()}.
#'
#' @return A \code{data.frame} of class \code{"es_result"} with columns:
#' \itemize{
#'   \item \code{term}, \code{estimate}, \code{std.error}, \code{statistic}, \code{p.value}
#'   \item \code{conf_low_XX}, \code{conf_high_XX} (for each requested \code{conf.level})
#'   \item \code{relative_time} (integer; 0 = event), \code{is_baseline} (logical; classic only)
#' }
#' Attributes include: \code{lead_range}, \code{lag_range}, \code{baseline}, \code{interval}, \code{call}, \code{model_formula}, \code{conf.level},
#' \code{N}, \code{N_units}, \code{N_treated}, \code{N_nevertreated}, \code{fe}, \code{vcov_type}, \code{cluster_vars}, \code{staggered}, \code{sunab_used}.
#'
#' @importFrom stats as.formula qnorm setNames vcov
#' @importFrom dplyr %>% bind_rows mutate arrange filter left_join group_by ungroup n_distinct
#' @importFrom rlang .data
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
    baseline     = -1L,
    interval     = 1,
    time_transform = FALSE,
    unit         = NULL,
    staggered    = FALSE,
    method       = c("classic","sunab"),
    conf.level   = c(0.90, 0.95, 0.99),
    vcov         = "HC1",
    vcov_args    = list()
) {
  # ---- Helper functions (defined once) --------------------------------------
  resolve_column <- function(expr, data, allow_call = FALSE) {
    if (rlang::is_symbol(expr)) {
      var <- rlang::as_string(expr)
      if (!var %in% names(data)) stop("Column '", var, "' not found.")
      return(var)
    } else if (allow_call && rlang::is_call(expr)) {
      return(rlang::expr_text(expr))
    } else if (is.character(expr) && expr %in% names(data)) {
      return(expr)
    } else {
      stop("Invalid column/expression: ", rlang::expr_text(expr))
    }
  }

  .must_exist <- function(cols, data) {
    cols <- as.character(cols)
    miss <- setdiff(cols, names(data))
    if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "))
  }

  # Helper: Convert treatment variable to 0/1 integer
  .normalize_treatment <- function(tx, var_name) {
    if (is.logical(tx)) {
      return(as.integer(tx))
    }
    if (is.factor(tx)) {
      tx <- suppressWarnings(as.integer(as.character(tx)))
    }
    if (!is.numeric(tx)) {
      stop("`", var_name, "` must be logical or numeric 0/1.")
    }
    if (any(!tx %in% c(0, 1, NA), na.rm = FALSE)) {
      stop("`", var_name, "` must be 0/1 (or NA).")
    }
    as.integer(tx)
  }

  # Helper: Add confidence intervals (vectorized)
  .add_confidence_intervals <- function(tidy, conf_levels, estimate, std_error) {
    conf_levels <- sort(unique(conf_levels))
    n_levels <- length(conf_levels)

    # Pre-compute z-values and column names
    z_values <- stats::qnorm(1 - (1 - conf_levels) / 2)
    suffixes <- sprintf("%.0f", conf_levels * 100)
    col_names_low <- paste0("conf_low_", suffixes)
    col_names_high <- paste0("conf_high_", suffixes)

    # Vectorized CI computation
    for (i in seq_len(n_levels)) {
      tidy[[col_names_low[i]]] <- estimate - z_values[i] * std_error
      tidy[[col_names_high[i]]] <- estimate + z_values[i] * std_error
    }

    tidy
  }

  # Helper: Compute metadata in fewer passes
  .compute_metadata <- function(data, unit_chr, k_vec, timing_chr = NULL, method = "classic") {
    N_units <- if (!is.null(unit_chr)) {
      length(unique(data[[unit_chr]]))
    } else {
      NA_integer_
    }

    if (method == "sunab" && !is.null(timing_chr)) {
      N_treat <- sum(!is.na(data[[timing_chr]]))
      N_never <- if (!is.na(N_units)) N_units - N_treat else NA_integer_
    } else {
      # For classic method: count observations with non-NA event time
      N_treat <- sum(!is.na(k_vec))
      N_never <- if (!is.na(N_units) && !is.null(unit_chr)) {
        # Count units that never have non-NA event time
        units_ever_treated <- tapply(!is.na(k_vec), data[[unit_chr]], any, simplify = TRUE)
        sum(!units_ever_treated)
      } else {
        NA_integer_
      }
    }

    list(N_units = N_units, N_treated = N_treat, N_nevertreated = N_never)
  }

  # ---- Early input validation -----------------------------------------------
  method <- match.arg(method)

  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.")
  }
  if (nrow(data) == 0L) {
    stop("`data` has zero rows.")
  }
  if (!is.numeric(interval) || interval <= 0) {
    stop("`interval` must be positive numeric.")
  }
  if (!inherits(fe, "formula")) {
    stop("`fe` must be a one-sided formula, e.g., ~ id + year.")
  }
  if (!is.null(covariates) && !inherits(covariates, "formula")) {
    stop("`covariates` must be a one-sided formula (e.g., ~ x1 + log(x2)).")
  }
  if (isTRUE(time_transform) && (missing(unit) || is.null(unit))) {
    stop("`time_transform = TRUE` requires `unit` to be specified.")
  }

  # Validate cluster parameter
  if (!is.null(cluster)) {
    if (is.character(cluster)) {
      if (length(cluster) == 1L) {
        if (!cluster %in% names(data)) {
          stop("`cluster` as character must be a column in data.")
        }
      } else if (length(cluster) != nrow(data)) {
        stop("Character `cluster` must be length 1 (column name) or length nrow(data).")
      }
    } else if (!inherits(cluster, "formula")) {
      if (length(cluster) != nrow(data)) {
        stop("Vector `cluster` must be length nrow(data).")
      }
    }
  }

  # ---- Resolve core variables -----------------------------------------------
  outcome_chr <- resolve_column(rlang::enexpr(outcome), data, allow_call = TRUE)
  time_chr <- resolve_column(rlang::enexpr(time), data)

  unit_chr <- NULL
  if (!missing(unit) && !is.null(unit)) {
    unit_chr <- resolve_column(rlang::enexpr(unit), data)
  }

  # Parse formula components once
  fe_rhs_text <- rlang::expr_text(rlang::f_rhs(fe))
  cov_text <- if (!is.null(covariates)) {
    rlang::expr_text(rlang::f_rhs(covariates))
  } else {
    ""
  }

  # ---- Time transformation (if needed) --------------------------------------
  # Optimized: use base R instead of dplyr groups to avoid overhead
  time_data <- data  # Work with a copy only if transformation needed
  if (isTRUE(time_transform)) {
    # Use base R for better performance on large panels
    ord <- order(data[[unit_chr]], data[[time_chr]])
    time_vec_sorted <- data[[time_chr]][ord]
    unit_vec_sorted <- data[[unit_chr]][ord]

    # Compute dense rank within each unit using base R
    time_index <- integer(length(ord))
    prev_unit <- unit_vec_sorted[1]
    prev_time <- time_vec_sorted[1]
    rank_counter <- 1L

    time_index[1] <- rank_counter
    for (i in 2:length(ord)) {
      if (unit_vec_sorted[i] != prev_unit) {
        # New unit, reset rank
        rank_counter <- 1L
      } else if (time_vec_sorted[i] != prev_time) {
        # Same unit, different time
        rank_counter <- rank_counter + 1L
      }
      time_index[i] <- rank_counter
      prev_unit <- unit_vec_sorted[i]
      prev_time <- time_vec_sorted[i]
    }

    # Reorder back to original order
    time_index_orig <- integer(length(ord))
    time_index_orig[ord] <- time_index

    # Create modified data frame with time index
    time_data <- data
    time_data$.time_index <- time_index_orig
    time_chr <- ".time_index"
  }

  # ---- Method: sunab --------------------------------------------------------
  if (method == "sunab") {
    if (!staggered) {
      warning("`method='sunab'` is typically used with `staggered=TRUE`.")
    }

    timing_chr <- resolve_column(rlang::enexpr(timing), time_data)

    # Build formula: outcome ~ sunab(timing,time) + cov | FE
    rhs <- paste0("fixest::sunab(", timing_chr, ", ", time_chr, ")")
    if (nzchar(cov_text)) {
      rhs <- paste(rhs, cov_text, sep = " + ")
    }
    formula_string <- paste0(outcome_chr, " ~ ", rhs, " | ", fe_rhs_text)
    model_formula <- stats::as.formula(formula_string)

    # Prepare model arguments
    model_args <- list(model_formula, data = time_data)
    if (!is.null(cluster)) model_args$cluster <- cluster
    if (!is.null(weights)) model_args$weights <- weights

    # Estimate model
    model <- tryCatch(
      do.call(fixest::feols, model_args),
      error = function(e) stop("Model estimation failed: ", e$message, call. = FALSE)
    )

    # VCOV with improved error handling
    V <- tryCatch(
      vcov(model, vcov = vcov, .vcov_args = vcov_args),
      error = function(e) {
        warning("VCOV computation failed, using default: ", e$message, call. = FALSE)
        NULL
      }
    )
    tidy <- if (is.null(V)) broom::tidy(model) else broom::tidy(model, vcov = V)

    # Extract relative time from terms like "sunab::timing_var:: -2"
    tidy$relative_time <- suppressWarnings(
      as.integer(gsub(".*::(-?\\d+)$", "\\1", tidy$term))
    )
    tidy$is_baseline <- FALSE

    # Add confidence intervals (vectorized)
    tidy <- .add_confidence_intervals(tidy, conf.level, tidy$estimate, tidy$std.error)

    # Compute metadata efficiently
    metadata <- .compute_metadata(time_data, unit_chr, NULL, timing_chr, method = "sunab")

    # Attach attributes
    attr(tidy, "lead_range") <- NA_integer_
    attr(tidy, "lag_range") <- NA_integer_
    attr(tidy, "baseline") <- NA
    attr(tidy, "interval") <- interval
    attr(tidy, "call") <- match.call()
    attr(tidy, "model_formula") <- formula_string
    attr(tidy, "conf.level") <- sort(unique(conf.level))
    attr(tidy, "N") <- stats::nobs(model)
    attr(tidy, "N_units") <- metadata$N_units
    attr(tidy, "N_treated") <- metadata$N_treated
    attr(tidy, "N_nevertreated") <- metadata$N_nevertreated
    attr(tidy, "fe") <- fe_rhs_text
    attr(tidy, "vcov_type") <- vcov
    attr(tidy, "cluster_vars") <- if (inherits(cluster, "formula")) {
      rlang::expr_text(rlang::f_rhs(cluster))
    } else {
      cluster
    }
    attr(tidy, "staggered") <- staggered
    attr(tidy, "sunab_used") <- TRUE

    class(tidy) <- c("es_result", "data.frame")
    return(tidy)
  }

  # ---- Method: classic ------------------------------------------------------
  # Resolve treatment variable
  treatment_chr <- resolve_column(rlang::enexpr(treatment), time_data)
  tx <- .normalize_treatment(time_data[[treatment_chr]], treatment_chr)

  # Resolve timing
  timing_chr <- NULL
  timing_val <- NULL

  if (staggered) {
    timing_chr <- resolve_column(rlang::enexpr(timing), time_data)
    .must_exist(timing_chr, time_data)
  } else {
    timing_val <- timing
    # Handle character Date input
    if (is.character(timing_val) && !timing_val %in% names(time_data)) {
      try_date <- suppressWarnings(as.Date(timing_val, format = "%Y-%m-%d"))
      if (!is.na(try_date)) timing_val <- try_date
    }
    # Validate Date type alignment
    time_is_date <- inherits(time_data[[time_chr]], "Date")
    timing_is_date <- inherits(timing_val, "Date")
    if (time_is_date && !timing_is_date) {
      stop("`timing` must be a Date when `time` is a Date.")
    }
    if (!time_is_date && timing_is_date) {
      stop("`time` must be a Date when `timing` is a Date.")
    }
  }

  # Compute relative time (avoid storing in data frame)
  rt <- if (staggered) {
    (time_data[[time_chr]] - time_data[[timing_chr]]) / interval
  } else {
    tv <- if (inherits(time_data[[time_chr]], "Date")) {
      as.numeric(timing_val)
    } else {
      timing_val
    }
    tnum <- if (inherits(time_data[[time_chr]], "Date")) {
      as.numeric(time_data[[time_chr]])
    } else {
      time_data[[time_chr]]
    }
    (tnum - tv) / interval
  }

  if (all(is.na(rt))) {
    stop("`relative_time` is NA for all rows. Check `time` and `timing` inputs.")
  }

  # Integer event time (only for treated observations)
  k_vec <- suppressWarnings(as.integer(round(rt)))
  k_vec[!as.logical(tx)] <- NA_integer_

  # Determine lead/lag ranges (single pass with range())
  k_range <- range(k_vec, na.rm = TRUE)
  if (is.null(lead_range)) {
    lead_range <- max(0L, abs(k_range[1]))
  }
  if (is.null(lag_range)) {
    lag_range <- max(0L, k_range[2])
  }

  # Validate baseline
  if (!is.finite(baseline) || (baseline %% 1L) != 0L) {
    stop("`baseline` must be an integer.")
  }
  if (baseline < -lead_range || baseline > lag_range) {
    stop("`baseline` (", baseline, ") is outside [-lead_range, lag_range] = [",
         -lead_range, ", ", lag_range, "].")
  }

  # Create factor levels with baseline as reference (optimized: one operation)
  levels_all <- seq.int(-lead_range, lag_range)
  # Reorder levels to put baseline first
  baseline_char <- as.character(baseline)
  levels_ordered <- c(baseline_char, setdiff(as.character(levels_all), baseline_char))
  f_vec <- factor(k_vec, levels = levels_ordered)

  # Prepare data for model (minimal modification: only add factor column)
  model_data <- time_data
  model_data$..f <- f_vec

  # Build model formula
  rhs <- "..f"
  if (nzchar(cov_text)) {
    rhs <- paste(rhs, cov_text, sep = " + ")
  }
  formula_string <- paste0(outcome_chr, " ~ ", rhs, " | ", fe_rhs_text)
  model_formula <- stats::as.formula(formula_string)

  # Prepare model arguments
  model_args <- list(model_formula, data = model_data)
  if (!is.null(cluster)) model_args$cluster <- cluster
  if (!is.null(weights)) model_args$weights <- weights

  # Estimate model
  model <- tryCatch(
    do.call(fixest::feols, model_args),
    error = function(e) {
      stop("Model estimation failed: ", e$message,
           "\nHint: Check for collinearity between FE and event dummies; ",
           "reconsider `lead_range`/`lag_range` or the granularity of your FE.",
           call. = FALSE)
    }
  )

  # VCOV with improved error handling
  V <- tryCatch(
    vcov(model, vcov = vcov, .vcov_args = vcov_args),
    error = function(e) {
      warning("VCOV computation failed, using default: ", e$message, call. = FALSE)
      NULL
    }
  )
  tidy <- if (is.null(V)) broom::tidy(model) else broom::tidy(model, vcov = V)

  # Add baseline row (dropped reference category)
  baseline_row <- tibble::tibble(
    term = paste0("..f", baseline),
    estimate = 0,
    std.error = 0,
    statistic = NA_real_,
    p.value = NA_real_
  )

  # Extract relative_time from term names
  tidy$relative_time <- suppressWarnings(
    as.integer(sub("^..f", "", tidy$term))
  )

  # Combine and order results
  full_order <- paste0("..f", levels_all)
  tidy <- dplyr::bind_rows(tidy, baseline_row)
  tidy$term <- factor(tidy$term, levels = full_order)
  tidy <- tidy %>%
    dplyr::arrange(.data$term) %>%
    dplyr::filter(!is.na(.data$term))
  tidy$is_baseline <- (tidy$relative_time == baseline)

  # Add confidence intervals (vectorized)
  tidy <- .add_confidence_intervals(tidy, conf.level, tidy$estimate, tidy$std.error)

  # Compute metadata efficiently
  metadata <- .compute_metadata(model_data, unit_chr, k_vec, NULL, method = "classic")

  # Attach attributes
  attr(tidy, "lead_range") <- lead_range
  attr(tidy, "lag_range") <- lag_range
  attr(tidy, "baseline") <- baseline
  attr(tidy, "interval") <- interval
  attr(tidy, "call") <- match.call()
  attr(tidy, "model_formula") <- formula_string
  attr(tidy, "conf.level") <- sort(unique(conf.level))
  attr(tidy, "N") <- stats::nobs(model)
  attr(tidy, "N_units") <- metadata$N_units
  attr(tidy, "N_treated") <- metadata$N_treated
  attr(tidy, "N_nevertreated") <- metadata$N_nevertreated
  attr(tidy, "fe") <- fe_rhs_text
  attr(tidy, "vcov_type") <- vcov
  attr(tidy, "cluster_vars") <- if (inherits(cluster, "formula")) {
    rlang::expr_text(rlang::f_rhs(cluster))
  } else {
    cluster
  }
  attr(tidy, "staggered") <- staggered
  attr(tidy, "sunab_used") <- FALSE

  class(tidy) <- c("es_result", "data.frame")
  tidy
}
