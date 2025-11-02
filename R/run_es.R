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
#' @param data A data.frame containing panel data.
#' @param outcome Unquoted outcome (name or expression, e.g., \code{log(y)}).
#' @param treatment Unquoted treatment indicator (0/1 or logical). Used only when \code{method = "classic"}.
#' @param time Unquoted time variable (numeric or Date).
#' @param timing For \code{classic}: a numeric/Date (universal) or a variable (unquoted) if \code{staggered = TRUE}. For \code{sunab}: an unquoted variable with adoption time.
#' @param fe One-sided fixed-effects formula, e.g., \code{~ id + year}. Can be \code{NULL} for no fixed effects.
#' @param lead_range,lag_range Integers for pre/post windows. If \code{NULL}, determined automatically.
#' @param covariates One-sided formula of additional controls, e.g., \code{~ x1 + log(x2)}.
#' @param cluster Cluster specification (one-sided formula like \code{~ id + year}, a single character column name, or a vector of length \code{nrow(data)}).
#' @param weights Observation weights (a name/one-sided formula or a numeric vector of length \code{nrow(data)}).
#' @param baseline Integer baseline period (default \code{-1}); reference period excluded from results for both \code{"classic"} and \code{"sunab"} methods.
#' @param interval Numeric spacing of the time variable (default \code{1}; ignored internally for Dates).
#' @param time_transform Logical; if \code{TRUE}, creates consecutive integer time within unit.
#' @param unit Unit identifier variable (required when \code{time_transform = TRUE}); also used for metadata when supplied.
#' @param staggered Logical; if \code{TRUE}, \code{timing} is a variable (classic) or is used by \code{sunab}.
#' @param method Either \code{"classic"} or \code{"sunab"} (default: \code{"classic"}).
#' @param conf.level Numeric vector of confidence levels (default \code{0.95}).
#' @param vcov VCOV type passed to \code{fixest::vcov()} or used via \code{broom::tidy(vcov = ...)}. Default \code{"HC1"}.
#' @param vcov_args List of additional arguments forwarded to \code{fixest::vcov()}.
#'
#' @return A \code{data.frame} of class \code{"es_result"} with columns:
#' \itemize{
#'   \item \code{term}, \code{estimate}, \code{std.error}, \code{statistic}, \code{p.value}
#'   \item \code{conf_low_XX}, \code{conf_high_XX} (for each requested \code{conf.level})
#'   \item \code{relative_time} (integer; 0 = event), \code{is_baseline} (logical; marks the reference period)
#' }
#' Attributes include: \code{lead_range}, \code{lag_range}, \code{baseline}, \code{interval}, \code{call}, \code{model_formula}, \code{conf.level},
#' \code{N}, \code{N_units}, \code{N_treated}, \code{N_nevertreated}, \code{fe}, \code{vcov_type}, \code{cluster_vars}, \code{staggered}, \code{sunab_used}.
#'
#' @importFrom stats as.formula qnorm setNames vcov
#' @importFrom dplyr bind_rows mutate arrange filter left_join group_by ungroup n_distinct
#' @importFrom rlang .data
#' @importFrom utils getFromNamespace
#' @export
run_es <- function(
    data,
    outcome,
    treatment,
    time,
    timing,
    fe           = NULL,
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
    conf.level   = 0.95,
    vcov         = "HC1",
    vcov_args    = list()
) {
  method <- match.arg(method)
  stopifnot(is.data.frame(data))
  if (!is.numeric(interval) || interval <= 0) stop("`interval` must be positive.")

  # ---- helpers -------------------------------------------------------------
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

  # ---- resolve core variables ---------------------------------------------
  outcome_chr   <- resolve_column(rlang::enexpr(outcome), data, allow_call = TRUE)
  time_chr      <- resolve_column(rlang::enexpr(time), data)

  unit_chr <- NULL
  if (!missing(unit) && !is.null(unit)) {
    unit_chr <- resolve_column(rlang::enexpr(unit), data)
  }

  # time transform (dense_rank within unit)
  if (isTRUE(time_transform)) {
    if (is.null(unit_chr)) stop("`time_transform=TRUE` requires `unit`.")
    data <- data |>
      dplyr::group_by(.data[[unit_chr]]) |>
      dplyr::arrange(.data[[time_chr]], .by_group = TRUE) |>
      dplyr::mutate(.time_index = dplyr::dense_rank(.data[[time_chr]])) |>
      dplyr::ungroup()
    time_chr <- ".time_index"
  }

  # covariates text
  cov_text <- ""
  if (!is.null(covariates)) {
    if (!inherits(covariates, "formula"))
      stop("`covariates` must be a one-sided formula (e.g., ~ x1 + log(x2)).")
    cov_text <- rlang::expr_text(rlang::f_rhs(covariates))
  }

  # FE
  fe_rhs_text <- ""
  if (!is.null(fe)) {
    if (!inherits(fe, "formula")) stop("`fe` must be a one-sided formula, e.g., ~ id + year.")
    fe_rhs_text <- rlang::expr_text(rlang::f_rhs(fe))
  }

  # ---- cluster validation (feols accepts formula/character/vector) ---------
  if (!is.null(cluster)) {
    if (is.character(cluster)) {
      if (length(cluster) == 1L) {
        if (!cluster %in% names(data))
          stop("`cluster` as character must be a column in data.")
      } else if (length(cluster) != nrow(data)) {
        stop("Character `cluster` must be length 1 (column name) or length nrow(data).")
      }
    } else if (!inherits(cluster, "formula")) {
      # allow numeric vector of length nrow(data)
      if (!is.null(cluster) && length(cluster) != nrow(data))
        stop("Vector `cluster` must be length nrow(data).")
    }
  }

  # ---- method = sunab ------------------------------------------------------
  if (method == "sunab") {
    if (!staggered) warning("`method='sunab'` is typically used with `staggered=TRUE`.")
    timing_chr <- resolve_column(rlang::enexpr(timing), data)

    # Get sunab function from fixest namespace and make it available in the formula environment
    # This ensures sunab is accessible when feols evaluates the formula
    sunab_fn <- getFromNamespace("sunab", "fixest")

    # Build formula as string (simpler approach that works with the injected function)
    rhs <- paste0("sunab(", timing_chr, ", ", time_chr, ")")
    if (nzchar(cov_text)) rhs <- paste(rhs, cov_text, sep = " + ")
    if (nzchar(fe_rhs_text)) {
      formula_string <- paste0(outcome_chr, " ~ ", rhs, " | ", fe_rhs_text)
    } else {
      formula_string <- paste0(outcome_chr, " ~ ", rhs)
    }
    model_formula <- stats::as.formula(formula_string)

    # Set the formula environment to include sunab
    formula_env <- new.env(parent = environment(model_formula))
    formula_env$sunab <- sunab_fn
    environment(model_formula) <- formula_env

    model_args <- list(model_formula, data = data)
    if (!is.null(cluster)) model_args$cluster <- cluster
    if (!is.null(weights)) model_args$weights <- weights

    model <- tryCatch(do.call(fixest::feols, model_args),
                      error = function(e) stop("Model estimation failed: ", e$message))
    # vcov override
    V <- tryCatch(vcov(model, vcov = vcov, .vcov_args = vcov_args), error = function(e) NULL)
    tidy <- if (is.null(V)) broom::tidy(model) else broom::tidy(model, vcov = V)

    # extract relative time from terms like "sunab::timing_var:: -2"
    rel <- suppressWarnings(as.integer(gsub(".*::(-?\\d+)$", "\\1", tidy$term)))
    tidy$relative_time <- rel
    tidy$is_baseline   <- FALSE

    # Warn about any NA values in sunab event time terms only (not covariates)
    # Sunab terms should contain "::" (from sunab decomposition)
    terms_char <- as.character(tidy$term)
    is_sunab_term <- grepl("::", terms_char, fixed = TRUE)

    if (any(is.na(tidy$relative_time) & is_sunab_term)) {
      na_sunab_terms <- terms_char[is_sunab_term & is.na(tidy$relative_time)]
      if (length(na_sunab_terms) > 0) {
        warning("Could not extract relative_time from sunab event time terms: ",
                paste(na_sunab_terms, collapse = ", "))
      }
    }

    # Determine lead_range and lag_range
    if (is.null(lead_range)) {
      lead_range <- max(0L, abs(min(tidy$relative_time, na.rm = TRUE)))
    }
    if (is.null(lag_range)) {
      lag_range <- max(0L, max(tidy$relative_time, na.rm = TRUE))
    }

    # Filter results to specified ranges (before adding baseline)
    tidy <- tidy |>
      dplyr::filter(!is.na(.data$relative_time) &
                    .data$relative_time >= -lead_range &
                    .data$relative_time <= lag_range)

    # Add baseline row (0 estimate, 0 SE) for the dropped reference
    # Check if baseline is within the filtered range
    if (baseline >= -lead_range && baseline <= lag_range) {
      baseline_row <- tibble::tibble(
        term      = as.character(baseline),
        estimate  = 0,
        std.error = 0,
        statistic = NA_real_,
        p.value   = NA_real_,
        relative_time = baseline
      )
      # Add baseline row if it doesn't already exist
      if (!baseline %in% tidy$relative_time) {
        tidy <- dplyr::bind_rows(tidy, baseline_row)
      }
    }

    # Mark baseline rows and arrange
    tidy$is_baseline <- tidy$relative_time == baseline
    tidy <- tidy |> dplyr::arrange(.data$relative_time)

    # Update term column to show relative_time as numeric string
    tidy$term <- as.character(tidy$relative_time)

    # add CIs for requested levels
    conf.level <- sort(unique(conf.level))
    for (cl in conf.level) {
      z <- stats::qnorm(1 - (1 - cl)/2)
      suf <- sprintf("%.0f", cl*100)
      tidy[[paste0("conf_low_", suf)]]  <- tidy$estimate - z * tidy$std.error
      tidy[[paste0("conf_high_", suf)]] <- tidy$estimate + z * tidy$std.error
    }

    # metadata
    N_units <- if (!is.null(unit_chr)) dplyr::n_distinct(data[[unit_chr]]) else NA_integer_
    N_treat <- if (timing_chr %in% names(data)) sum(!is.na(data[[timing_chr]])) else NA_integer_

    attr(tidy, "lead_range")       <- lead_range
    attr(tidy, "lag_range")        <- lag_range
    attr(tidy, "baseline")         <- baseline
    attr(tidy, "interval")         <- interval
    attr(tidy, "call")             <- match.call()
    attr(tidy, "model_formula")    <- formula_string
    attr(tidy, "conf.level")       <- conf.level
    attr(tidy, "N")                <- stats::nobs(model)
    attr(tidy, "N_units")          <- N_units
    attr(tidy, "N_treated")        <- N_treat
    attr(tidy, "N_nevertreated")   <- if (!is.na(N_units)) N_units - N_treat else NA_integer_
    attr(tidy, "fe")               <- fe_rhs_text
    attr(tidy, "vcov_type")        <- vcov
    attr(tidy, "cluster_vars")     <- if (inherits(cluster,"formula")) rlang::expr_text(rlang::f_rhs(cluster)) else cluster
    attr(tidy, "staggered")        <- staggered
    attr(tidy, "sunab_used")       <- TRUE

    class(tidy) <- c("es_result","data.frame")
    return(tidy)
  }

  # ---- method = classic ----------------------------------------------------
  # resolve treatment and timing
  treatment_chr <- resolve_column(rlang::enexpr(treatment), data)
  tx <- data[[treatment_chr]]
  if (is.logical(tx)) tx <- as.integer(tx)
  if (is.factor(tx))  tx <- suppressWarnings(as.integer(as.character(tx)))
  if (!is.numeric(tx)) stop("`treatment` must be logical or numeric 0/1.")
  if (any(!tx %in% c(0,1,NA))) stop("`treatment` must be 0/1 (or NA).")
  data[[treatment_chr]] <- tx

  # timing
  timing_val <- NULL
  timing_chr <- NULL
  if (staggered) {
    timing_chr <- resolve_column(rlang::enexpr(timing), data)
  } else {
    timing_val <- timing
    if (is.character(timing_val) && !timing_val %in% names(data)) {
      try_date <- suppressWarnings(as.Date(timing_val, format = "%Y-%m-%d"))
      if (!is.na(try_date)) timing_val <- try_date
    }
    # align Date types
    if (inherits(data[[time_chr]], "Date") && !inherits(timing_val, "Date"))
      stop("`timing` must be a Date when `time` is a Date.")
    if (!inherits(data[[time_chr]], "Date") && inherits(timing_val, "Date"))
      stop("`time` must be a Date when `timing` is a Date.")
  }

  # relative_time (for range calculation and staggered case)
  if (staggered) {
    .must_exist(timing_chr, data)
    rt <- (data[[time_chr]] - data[[timing_chr]]) / interval
  } else {
    tv <- if (inherits(data[[time_chr]], "Date")) as.numeric(timing_val) else timing_val
    tnum <- if (inherits(data[[time_chr]], "Date")) as.numeric(data[[time_chr]]) else data[[time_chr]]
    rt <- (tnum - tv) / interval
  }
  data$..rt <- rt
  if (all(is.na(data$..rt))) stop("`relative_time` is NA for all rows. Check inputs.")

  # integer event time
  data$..k <- suppressWarnings(as.integer(round(data$..rt)))

  # auto ranges (based on treated units only)
  k_treated <- data$..k[as.logical(data[[treatment_chr]])]
  if (is.null(lead_range)) lead_range <- max(0L, abs(min(k_treated, na.rm = TRUE)))
  if (is.null(lag_range))  lag_range  <- max(0L, max(k_treated, na.rm = TRUE))

  # baseline check
  if (!is.finite(baseline) || (baseline %% 1L) != 0L) stop("`baseline` must be an integer.")
  if (baseline < -lead_range || baseline > lag_range)
    stop("`baseline` outside [-lead_range, lag_range].")

  # Build formula using i()
  # For staggered: use relative time (..k)
  # For non-staggered: use absolute time for i(), but will convert terms to relative time later
  if (staggered) {
    # Use i(..k, treatment, ref = baseline)
    i_formula <- paste0("fixest::i(..k, ", treatment_chr, ", ref = ", baseline, ")")
  } else {
    # Calculate the reference period based on baseline
    # E.g., if timing = 5 and baseline = -1, ref should be period 4
    if (inherits(timing_val, "Date")) {
      ref_period <- timing_val + baseline * interval
    } else {
      ref_period <- timing_val + baseline * interval
    }
    # Use i(time, treatment, ref = ref_period)
    i_formula <- paste0("fixest::i(", time_chr, ", ", treatment_chr, ", ref = ", ref_period, ")")
  }

  rhs <- i_formula
  if (nzchar(cov_text)) rhs <- paste(rhs, cov_text, sep = " + ")
  if (nzchar(fe_rhs_text)) {
    formula_string <- paste0(outcome_chr, " ~ ", rhs, " | ", fe_rhs_text)
  } else {
    formula_string <- paste0(outcome_chr, " ~ ", rhs)
  }
  model_formula  <- stats::as.formula(formula_string)

  model_args <- list(model_formula, data = data)
  if (!is.null(cluster)) model_args$cluster <- cluster
  if (!is.null(weights)) model_args$weights <- weights

  model <- tryCatch(do.call(fixest::feols, model_args),
                    error = function(e) {
                      msg <- e$message
                      stop("Model estimation failed: ", msg,
                           "\nHint: Check for collinearity between FE and event dummies; reconsider `lead_range`/`lag_range` or the granularity of your FE.")
                    })

  # vcov override
  V <- tryCatch(vcov(model, vcov = vcov, .vcov_args = vcov_args), error = function(e) NULL)
  tidy <- if (is.null(V)) broom::tidy(model) else broom::tidy(model, vcov = V)

  # Extract relative_time from i() terms - vectorized for performance
  # Format: "fixest::var::value:treatment" (3 parts) or "var::value" (2 parts)
  terms_char <- as.character(tidy$term)

  # Extract numeric values from term strings
  # Split by "::" and extract the LAST part (which contains "value:treatment" or just "value")
  parts_list <- strsplit(terms_char, "::", fixed = TRUE)
  value_parts <- sapply(parts_list, function(x) x[length(x)])

  # Extract numeric part (before any additional ":")
  numeric_parts <- sapply(strsplit(value_parts, ":", fixed = TRUE), function(x) x[1])
  time_values <- suppressWarnings(as.numeric(numeric_parts))

  # Convert to relative time
  if (staggered) {
    # For staggered, ..k is already relative time
    tidy$relative_time <- as.integer(time_values)
  } else {
    # For non-staggered, convert absolute time to relative time
    tv <- if (inherits(timing_val, "Date")) as.numeric(timing_val) else timing_val
    tidy$relative_time <- as.integer(round((time_values - tv) / interval))
  }

  # Warn about any NA values in event time terms only (not covariates)
  # Event time terms should contain "::" (from fixest::i()) or be standalone numeric-like
  is_event_term <- grepl("::", terms_char, fixed = TRUE) |
                   grepl("^[+-]?\\d+$", terms_char)

  if (any(is.na(tidy$relative_time) & is_event_term)) {
    na_event_terms <- terms_char[is_event_term & is.na(tidy$relative_time)]
    if (length(na_event_terms) > 0) {
      warning("Could not extract relative_time from event time terms: ",
              paste(na_event_terms, collapse = ", "))
    }
  }

  # Add baseline row (0 estimate, 0 SE) for the dropped reference
  baseline_row <- tibble::tibble(
    term      = as.character(baseline),
    estimate  = 0,
    std.error = 0,
    statistic = NA_real_,
    p.value   = NA_real_,
    relative_time = baseline
  )

  # Combine with baseline row
  tidy <- dplyr::bind_rows(tidy, baseline_row)
  tidy$is_baseline <- tidy$relative_time == baseline

  # Arrange by relative_time
  tidy <- tidy |> dplyr::arrange(.data$relative_time)

  # Filter results to specified ranges
  tidy <- tidy |>
    dplyr::filter(!is.na(.data$relative_time) &
                  .data$relative_time >= -lead_range &
                  .data$relative_time <= lag_range)

  # Update term column to show relative_time as requested
  tidy$term <- as.character(tidy$relative_time)

  # Add confidence intervals
  conf.level <- sort(unique(conf.level))
  for (cl in conf.level) {
    z <- stats::qnorm(1 - (1 - cl)/2)
    suf <- sprintf("%.0f", cl*100)
    tidy[[paste0("conf_low_", suf)]]  <- tidy$estimate - z * tidy$std.error
    tidy[[paste0("conf_high_", suf)]] <- tidy$estimate + z * tidy$std.error
  }

  # metadata
  N_units <- if (!is.null(unit_chr)) dplyr::n_distinct(data[[unit_chr]]) else NA_integer_
  N_treat <- sum(!is.na(data$..k))
  N_never <- if (!is.na(N_units) && !is.null(unit_chr)) {
    treated_by_unit <- tapply(!is.na(data$..k), data[[unit_chr]], any)
    sum(!treated_by_unit)
  } else NA_integer_

  attr(tidy, "lead_range")       <- lead_range
  attr(tidy, "lag_range")        <- lag_range
  attr(tidy, "baseline")         <- baseline
  attr(tidy, "interval")         <- interval
  attr(tidy, "call")             <- match.call()
  attr(tidy, "model_formula")    <- formula_string
  attr(tidy, "conf.level")       <- conf.level
  attr(tidy, "N")                <- stats::nobs(model)
  attr(tidy, "N_units")          <- N_units
  attr(tidy, "N_treated")        <- N_treat
  attr(tidy, "N_nevertreated")   <- N_never
  attr(tidy, "fe")               <- fe_rhs_text
  attr(tidy, "vcov_type")        <- vcov
  attr(tidy, "cluster_vars")     <- if (inherits(cluster,"formula")) rlang::expr_text(rlang::f_rhs(cluster)) else cluster
  attr(tidy, "staggered")        <- staggered
  attr(tidy, "sunab_used")       <- FALSE

  class(tidy) <- c("es_result","data.frame")
  tidy
}
