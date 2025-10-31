# =============================================================================
# Comprehensive Test Suite for fixes Package
# =============================================================================
# Tests cover:
#   - Core estimation (classic & sunab methods)
#   - Plotting functionality
#   - S3 methods (print, autoplot)
#   - Edge cases (Date types, expressions, time_transform, etc.)
#   - Error handling and validation
#   - Metadata and attributes
# =============================================================================

library(testthat)
library(dplyr)
library(ggplot2)

# =============================================================================
# Helper Functions
# =============================================================================

#' Check if fixest::sunab is available
has_sunab <- function() {
  "sunab" %in% getNamespaceExports("fixest")
}

#' Validate baseline behavior in results
#' @details Checks that baseline period either has estimate = 0 (if included)
#'          or is absent from results (if normalized/excluded)
validate_baseline <- function(res) {
  expect_true("is_baseline" %in% names(res))
  baseline_val <- attr(res, "baseline")

  has_baseline_row <- any(isTRUE(res$is_baseline), na.rm = TRUE)

  if (has_baseline_row) {
    # Baseline included: estimate should be 0
    baseline_rows <- res[res$is_baseline %in% TRUE, ]
    expect_true(all(baseline_rows$estimate == 0))
    expect_true(all(baseline_rows$std.error == 0))
  } else {
    # Baseline excluded: relative_time should not contain baseline value
    expect_false(baseline_val %in% res$relative_time)
  }
}

#' Validate standard es_result structure
validate_es_result <- function(res) {
  expect_s3_class(res, "es_result")
  expect_s3_class(res, "data.frame")

  # Check required columns
  required_cols <- c("term", "estimate", "std.error", "statistic",
                     "p.value", "relative_time", "is_baseline")
  expect_true(all(required_cols %in% names(res)))

  # Check confidence interval columns exist
  expect_true(any(grepl("^conf_low_", names(res))))
  expect_true(any(grepl("^conf_high_", names(res))))

  # Check attributes
  expect_true(!is.null(attr(res, "call")))
  expect_true(!is.null(attr(res, "model_formula")))
  expect_true(!is.null(attr(res, "conf.level")))
  expect_true(!is.null(attr(res, "N")))
  expect_true(!is.null(attr(res, "vcov_type")))
}

# =============================================================================
# Data Generators
# =============================================================================

#' Generate synthetic panel data with universal treatment timing
#' @param n_units Number of units (default 50)
#' @param n_periods Number of time periods (default 8)
#' @param start_time Starting time value (default 1)
#' @param treatment_effect True treatment effect (default 0.8)
#' @param seed Random seed for reproducibility
make_universal_panel <- function(n_units = 50, n_periods = 8, start_time = 1L,
                                  treatment_effect = 0.8, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  ids <- seq_len(n_units)
  times <- seq.int(start_time, start_time + n_periods - 1L)
  treatment_status <- sample(c(0L, 1L), n_units, replace = TRUE, prob = c(0.5, 0.5))
  event_time <- start_time + floor(n_periods / 2)

  tidyr::expand_grid(unit_id = ids, time = times) %>%
    arrange(unit_id, time) %>%
    mutate(
      treated = rep(treatment_status, each = n_periods),
      post = as.integer(time >= event_time),
      treated_post = treated * post,
      x1 = rnorm(n()),
      x2 = rnorm(n(), mean = 0.5),
      unit_fe = rep(rnorm(n_units, sd = 2), each = n_periods),
      time_fe = rep(rnorm(n_periods, sd = 0.5), times = n_units),
      epsilon = rnorm(n(), sd = 1),
      y = 10 + treatment_effect * treated_post + 0.3 * x1 + 0.2 * x2 +
          unit_fe + time_fe + epsilon,
      w = abs(rnorm(n(), mean = 1, sd = 0.2))  # weights
    )
}

#' Generate synthetic panel data with staggered treatment adoption
#' @param n_units Number of units (default 50)
#' @param n_periods Number of time periods (default 10)
#' @param start_time Starting time value (default 1)
#' @param p_never Proportion of never-treated units (default 0.3)
#' @param treatment_effect True treatment effect (default 0.5)
#' @param seed Random seed for reproducibility
make_staggered_panel <- function(n_units = 50, n_periods = 10, start_time = 1L,
                                  p_never = 0.3, treatment_effect = 0.5, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  ids <- seq_len(n_units)
  times <- seq.int(start_time, start_time + n_periods - 1L)

  # Create staggered adoption times
  n_adopt_periods <- 4
  adoption_times <- c(NA, seq.int(start_time + 3L, length.out = n_adopt_periods))
  probs <- c(p_never, rep((1 - p_never) / n_adopt_periods, n_adopt_periods))
  timing_by_unit <- sample(adoption_times, n_units, replace = TRUE, prob = probs)

  tidyr::expand_grid(unit_id = ids, time = times) %>%
    arrange(unit_id, time) %>%
    mutate(
      timing = rep(timing_by_unit, each = n_periods),
      ever_treated = as.integer(!is.na(timing)),
      treated_post = as.integer(!is.na(timing) & time >= timing),
      x1 = rnorm(n()),
      unit_fe = rep(rnorm(n_units, sd = 1.5), each = n_periods),
      time_fe = rep(rnorm(n_periods, sd = 0.3), times = n_units),
      epsilon = rnorm(n(), sd = 0.8),
      y = 5 + treatment_effect * treated_post + 0.2 * x1 +
          unit_fe + time_fe + epsilon,
      w = abs(rnorm(n(), mean = 1, sd = 0.15))  # weights
    )
}

# =============================================================================
# Tests: Core Functionality - Classic Method (Universal Timing)
# =============================================================================

test_that("run_es() with classic method and universal timing produces valid results", {
  data <- make_universal_panel(n_units = 60, n_periods = 8, seed = 123)
  event_time <- unique(data$time)[5]

  res <- run_es(
    data = data,
    outcome = y,
    treatment = treated,
    time = time,
    timing = event_time,
    fe = ~ unit_id,
    cluster = ~ unit_id,
    baseline = -1,
    conf.level = 0.95
  )

  validate_es_result(res)
  validate_baseline(res)

  # Check method-specific attributes
  expect_false(attr(res, "sunab_used"))
  expect_false(attr(res, "staggered"))
  expect_equal(attr(res, "baseline"), -1)
  expect_true(!is.na(attr(res, "lead_range")))
  expect_true(!is.na(attr(res, "lag_range")))
})

test_that("run_es() with classic method handles two-way fixed effects", {
  # Use staggered panel which is more suitable for two-way FE
  data <- make_staggered_panel(n_units = 50, n_periods = 10, seed = 456)

  res <- run_es(
    data = data,
    outcome = y,
    treatment = ever_treated,
    time = time,
    timing = timing,
    staggered = TRUE,
    fe = ~ unit_id + time,
    cluster = ~ unit_id,
    lead_range = 2,  # Limit range to avoid collinearity
    lag_range = 2,
    baseline = -1
  )

  validate_es_result(res)
  expect_equal(attr(res, "fe"), "unit_id + time")
})

test_that("run_es() with classic method accepts custom lead/lag ranges", {
  data <- make_universal_panel(n_periods = 10, seed = 789)  # More periods for larger window
  event_time <- unique(data$time)[6]  # Event in middle

  res <- run_es(
    data = data,
    outcome = y,
    treatment = treated,
    time = time,
    timing = event_time,
    fe = ~ unit_id,
    lead_range = 2,
    lag_range = 3,
    baseline = -1
  )

  validate_es_result(res)
  expect_equal(attr(res, "lead_range"), 2)
  expect_equal(attr(res, "lag_range"), 3)
  # Check range (na.rm = TRUE to handle any potential NAs)
  expect_true(min(res$relative_time, na.rm = TRUE) >= -2)
  expect_true(max(res$relative_time, na.rm = TRUE) <= 3)
})

test_that("run_es() with classic method auto-detects lead/lag ranges", {
  data <- make_universal_panel(n_periods = 10, seed = 111)
  event_time <- unique(data$time)[6]  # More pre-periods available

  res <- run_es(
    data = data,
    outcome = y,
    treatment = treated,
    time = time,
    timing = event_time,
    fe = ~ unit_id,
    lead_range = NULL,  # Auto-detect
    lag_range = NULL    # Auto-detect
  )

  validate_es_result(res)
  expect_true(attr(res, "lead_range") >= 3)
  expect_true(attr(res, "lag_range") >= 3)
})

# =============================================================================
# Tests: Core Functionality - Classic Method (Staggered Timing)
# =============================================================================

test_that("run_es() with classic method and staggered timing works correctly", {
  data <- make_staggered_panel(n_units = 50, n_periods = 10, seed = 222)

  res <- run_es(
    data = data,
    outcome = y,
    treatment = ever_treated,
    time = time,
    timing = timing,
    staggered = TRUE,
    fe = ~ unit_id + time,
    cluster = ~ unit_id,
    lead_range = 2,
    lag_range = 3,
    baseline = -1
  )

  validate_es_result(res)
  validate_baseline(res)
  expect_true(attr(res, "staggered"))
  expect_false(attr(res, "sunab_used"))
})

test_that("run_es() with classic staggered handles weights correctly", {
  data <- make_staggered_panel(seed = 333)

  res <- run_es(
    data = data,
    outcome = y,
    treatment = ever_treated,
    time = time,
    timing = timing,
    staggered = TRUE,
    fe = ~ unit_id + time,
    weights = ~ w,
    lead_range = 2,
    lag_range = 2
  )

  validate_es_result(res)
})

# =============================================================================
# Tests: Core Functionality - Sunab Method
# =============================================================================

test_that("run_es() with sunab method works when available", {
  skip_if_not(has_sunab(), "fixest::sunab() not available")

  data <- make_staggered_panel(n_units = 60, n_periods = 10,
                                 start_time = 2001L, seed = 444)

  res <- run_es(
    data = data,
    outcome = y,
    treatment = ever_treated,  # Not used by sunab but required param
    time = time,
    timing = timing,
    staggered = TRUE,
    method = "sunab",
    fe = ~ unit_id + time,
    cluster = ~ unit_id,
    conf.level = 0.95
  )

  validate_es_result(res)
  expect_true(attr(res, "sunab_used"))
  expect_true(attr(res, "staggered"))

  # Sunab has different baseline handling
  expect_true(is.na(attr(res, "baseline")) || attr(res, "baseline") == 0)
})

test_that("run_es() with sunab method handles weights", {
  skip_if_not(has_sunab(), "fixest::sunab() not available")

  data <- make_staggered_panel(seed = 555)

  res <- run_es(
    data = data,
    outcome = y,
    treatment = ever_treated,
    time = time,
    timing = timing,
    staggered = TRUE,
    method = "sunab",
    fe = ~ unit_id + time,
    weights = ~ w
  )

  validate_es_result(res)
  expect_true(attr(res, "sunab_used"))
})

# =============================================================================
# Tests: Plotting Functionality
# =============================================================================

test_that("plot_es() produces ggplot object with ribbon type", {
  data <- make_universal_panel(seed = 666)
  event_time <- unique(data$time)[5]

  res <- run_es(
    data = data, outcome = y, treatment = treated,
    time = time, timing = event_time, fe = ~ unit_id
  )

  p <- plot_es(res, ci_level = 0.95, type = "ribbon")

  expect_s3_class(p, "ggplot")
  # Check for ribbon geom (layer order: vline, hline, points, ribbon, line)
  expect_true(any(sapply(p$layers, function(l) inherits(l$geom, "GeomRibbon"))))
})

test_that("plot_es() produces ggplot object with errorbar type", {
  data <- make_universal_panel(seed = 777)
  event_time <- unique(data$time)[5]

  res <- run_es(
    data = data, outcome = y, treatment = treated,
    time = time, timing = event_time, fe = ~ unit_id
  )

  p <- plot_es(res, ci_level = 0.95, type = "errorbar")

  expect_s3_class(p, "ggplot")
  # Check for errorbar geom (layer order: vline, hline, points, errorbar)
  expect_true(any(sapply(p$layers, function(l) inherits(l$geom, "GeomErrorbar"))))
})

test_that("plot_es() supports different theme styles", {
  data <- make_universal_panel(seed = 888)
  event_time <- unique(data$time)[5]

  res <- run_es(
    data = data, outcome = y, treatment = treated,
    time = time, timing = event_time, fe = ~ unit_id
  )

  p_bw <- plot_es(res, theme_style = "bw")
  p_minimal <- plot_es(res, theme_style = "minimal")
  p_classic <- plot_es(res, theme_style = "classic")

  expect_s3_class(p_bw, "ggplot")
  expect_s3_class(p_minimal, "ggplot")
  expect_s3_class(p_classic, "ggplot")
})

test_that("plot_es() respects custom styling parameters", {
  data <- make_universal_panel(seed = 999)
  event_time <- unique(data$time)[5]

  res <- run_es(
    data = data, outcome = y, treatment = treated,
    time = time, timing = event_time, fe = ~ unit_id
  )

  p <- plot_es(
    res,
    ci_level = 0.95,
    type = "ribbon",
    vline_val = 0,
    hline_val = 0,
    vline_color = "red",
    hline_color = "blue",
    color = "#FF5733",
    fill = "#33FF57",
    alpha = 0.3,
    linewidth = 1.5,
    pointsize = 3
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_es() handles multiple confidence levels correctly", {
  data <- make_universal_panel(seed = 1010)
  event_time <- unique(data$time)[5]

  res <- run_es(
    data = data, outcome = y, treatment = treated,
    time = time, timing = event_time, fe = ~ unit_id,
    conf.level = c(0.90, 0.95, 0.99)
  )

  p90 <- plot_es(res, ci_level = 0.90)
  p95 <- plot_es(res, ci_level = 0.95)
  p99 <- plot_es(res, ci_level = 0.99)

  expect_s3_class(p90, "ggplot")
  expect_s3_class(p95, "ggplot")
  expect_s3_class(p99, "ggplot")
})

test_that("plot_es() falls back to 95% CI if requested level unavailable", {
  data <- make_universal_panel(seed = 1111)
  event_time <- unique(data$time)[5]

  res <- run_es(
    data = data, outcome = y, treatment = treated,
    time = time, timing = event_time, fe = ~ unit_id,
    conf.level = 0.95  # Only 95% CI computed
  )

  # Request 90% CI (not available)
  p <- plot_es(res, ci_level = 0.90)
  expect_s3_class(p, "ggplot")
})

# =============================================================================
# Tests: S3 Methods
# =============================================================================

test_that("print.es_result() displays summary correctly", {
  data <- make_universal_panel(seed = 1212)
  event_time <- unique(data$time)[5]

  res <- run_es(
    data = data, outcome = y, treatment = treated,
    time = time, timing = event_time, fe = ~ unit_id,
    cluster = ~ unit_id
  )

  # Capture printed output
  output <- capture.output(print(res))

  expect_true(length(output) > 0)
  expect_true(any(grepl("Event Study Result", output)))
  expect_true(any(grepl("N:", output)))
  expect_true(any(grepl("FE:", output)))
})

test_that("autoplot.es_result() produces ggplot", {
  data <- make_universal_panel(seed = 1313)
  event_time <- unique(data$time)[5]

  res <- run_es(
    data = data, outcome = y, treatment = treated,
    time = time, timing = event_time, fe = ~ unit_id
  )

  p <- autoplot(res)
  expect_s3_class(p, "ggplot")

  p2 <- autoplot(res, ci_level = 0.95, type = "errorbar")
  expect_s3_class(p2, "ggplot")
})

# =============================================================================
# Tests: Edge Cases and Special Features
# =============================================================================

test_that("run_es() handles Date type for time variable", {
  data <- make_universal_panel(seed = 1414)

  # Convert to Date
  data <- data %>%
    mutate(
      date = as.Date("2020-01-01") + (time - 1) * 30,
      event_date = as.Date("2020-01-01") + (5 - 1) * 30
    )

  event_date <- unique(data$event_date)[1]

  res <- run_es(
    data = data,
    outcome = y,
    treatment = treated,
    time = date,
    timing = event_date,
    fe = ~ unit_id,
    baseline = -1
  )

  validate_es_result(res)
})

test_that("run_es() handles outcome as expression", {
  data <- make_universal_panel(seed = 1515)
  event_time <- unique(data$time)[5]

  res <- run_es(
    data = data,
    outcome = log(y + 1),  # Expression
    treatment = treated,
    time = time,
    timing = event_time,
    fe = ~ unit_id
  )

  validate_es_result(res)
  expect_true(grepl("log\\(y", attr(res, "model_formula")))
})

test_that("run_es() handles covariates correctly", {
  data <- make_universal_panel(seed = 1616)
  event_time <- unique(data$time)[5]

  res <- run_es(
    data = data,
    outcome = y,
    treatment = treated,
    time = time,
    timing = event_time,
    fe = ~ unit_id,
    covariates = ~ x1 + x2
  )

  validate_es_result(res)
  expect_true(grepl("x1.*x2|x2.*x1", attr(res, "model_formula")))
})

test_that("run_es() works with time_transform=TRUE", {
  data <- make_universal_panel(n_periods = 8, seed = 1717) %>%
    mutate(time = time * 2)  # Non-consecutive time values

  event_time <- unique(data$time)[5]

  res <- run_es(
    data = data,
    outcome = y,
    treatment = treated,
    time = time,
    timing = event_time,
    fe = ~ unit_id,
    time_transform = TRUE,
    unit = "unit_id",  # Pass as character string
    baseline = -1
  )

  validate_es_result(res)
})

test_that("run_es() supports different VCOV types", {
  data <- make_universal_panel(seed = 1818)
  event_time <- unique(data$time)[5]

  res_hc1 <- run_es(
    data = data, outcome = y, treatment = treated,
    time = time, timing = event_time, fe = ~ unit_id,
    vcov = "HC1"
  )

  res_hc3 <- run_es(
    data = data, outcome = y, treatment = treated,
    time = time, timing = event_time, fe = ~ unit_id,
    vcov = "HC3"
  )

  res_iid <- run_es(
    data = data, outcome = y, treatment = treated,
    time = time, timing = event_time, fe = ~ unit_id,
    vcov = "iid"
  )

  validate_es_result(res_hc1)
  validate_es_result(res_hc3)
  validate_es_result(res_iid)

  expect_equal(attr(res_hc1, "vcov_type"), "HC1")
  expect_equal(attr(res_hc3, "vcov_type"), "HC3")
  expect_equal(attr(res_iid, "vcov_type"), "iid")
})

test_that("run_es() handles multiple confidence levels", {
  data <- make_universal_panel(seed = 1919)
  event_time <- unique(data$time)[5]

  res <- run_es(
    data = data,
    outcome = y,
    treatment = treated,
    time = time,
    timing = event_time,
    fe = ~ unit_id,
    conf.level = c(0.90, 0.95, 0.99)
  )

  validate_es_result(res)
  expect_true(all(c("conf_low_90", "conf_high_90") %in% names(res)))
  expect_true(all(c("conf_low_95", "conf_high_95") %in% names(res)))
  expect_true(all(c("conf_low_99", "conf_high_99") %in% names(res)))
  expect_equal(attr(res, "conf.level"), c(0.90, 0.95, 0.99))
})

test_that("run_es() computes metadata correctly", {
  data <- make_staggered_panel(n_units = 40, n_periods = 10, seed = 2020)

  res <- run_es(
    data = data,
    outcome = y,
    treatment = ever_treated,
    time = time,
    timing = timing,
    staggered = TRUE,
    unit = "unit_id",  # Pass as character string
    fe = ~ unit_id + time,
    cluster = ~ unit_id
  )

  validate_es_result(res)

  # Check metadata attributes
  expect_true(!is.null(attr(res, "N")))
  expect_true(!is.null(attr(res, "N_units")))
  expect_true(!is.null(attr(res, "N_treated")))
  expect_true(!is.null(attr(res, "N_nevertreated")))

  # N should match data size
  expect_true(attr(res, "N") <= nrow(data))
  expect_equal(attr(res, "N_units"), 40)
})

test_that("run_es() handles different cluster specifications", {
  data <- make_universal_panel(seed = 2121)
  event_time <- unique(data$time)[5]

  # Formula cluster
  res1 <- run_es(
    data = data, outcome = y, treatment = treated,
    time = time, timing = event_time, fe = ~ unit_id,
    cluster = ~ unit_id
  )
  validate_es_result(res1)

  # Character cluster (column name)
  res2 <- run_es(
    data = data, outcome = y, treatment = treated,
    time = time, timing = event_time, fe = ~ unit_id,
    cluster = "unit_id"
  )
  validate_es_result(res2)

  # Vector cluster
  res3 <- run_es(
    data = data, outcome = y, treatment = treated,
    time = time, timing = event_time, fe = ~ unit_id,
    cluster = data$unit_id
  )
  validate_es_result(res3)
})

test_that("run_es() handles different weight specifications", {
  data <- make_universal_panel(seed = 2222)
  event_time <- unique(data$time)[5]

  # Formula weights
  res1 <- run_es(
    data = data, outcome = y, treatment = treated,
    time = time, timing = event_time, fe = ~ unit_id,
    weights = ~ w
  )
  validate_es_result(res1)

  # Vector weights
  res2 <- run_es(
    data = data, outcome = y, treatment = treated,
    time = time, timing = event_time, fe = ~ unit_id,
    weights = data$w
  )
  validate_es_result(res2)
})

# =============================================================================
# Tests: Error Handling and Validation
# =============================================================================

test_that("run_es() errors with invalid baseline", {
  data <- make_universal_panel(seed = 2323)
  event_time <- unique(data$time)[5]

  expect_error(
    run_es(
      data = data, outcome = y, treatment = treated,
      time = time, timing = event_time, fe = ~ unit_id,
      lead_range = 2, lag_range = 2, baseline = -10
    ),
    regexp = "baseline.*outside"
  )
})

test_that("run_es() errors with invalid data input", {
  expect_error(
    run_es(
      data = "not_a_dataframe", outcome = y, treatment = treated,
      time = time, timing = 5, fe = ~ unit_id
    ),
    regexp = "data.*must be.*data.frame"
  )

  expect_error(
    run_es(
      data = data.frame(), outcome = y, treatment = treated,
      time = time, timing = 5, fe = ~ unit_id
    ),
    regexp = "zero rows"
  )
})

test_that("run_es() errors with missing required parameters", {
  data <- make_universal_panel(seed = 2424)

  expect_error(
    run_es(
      data = data, outcome = y, treatment = treated,
      time = time, timing = 5, fe = "not_a_formula"
    ),
    regexp = "fe.*must be.*formula"
  )
})

test_that("run_es() errors with invalid column names", {
  data <- make_universal_panel(seed = 2525)

  expect_error(
    run_es(
      data = data, outcome = nonexistent_col, treatment = treated,
      time = time, timing = 5, fe = ~ unit_id
    ),
    regexp = "not found"
  )
})

test_that("run_es() errors with invalid cluster specification", {
  data <- make_universal_panel(seed = 2626)
  event_time <- unique(data$time)[5]

  # Wrong length vector
  expect_error(
    run_es(
      data = data, outcome = y, treatment = treated,
      time = time, timing = event_time, fe = ~ unit_id,
      cluster = 1:10  # Wrong length
    ),
    regexp = "cluster.*length"
  )

  # Invalid column name
  expect_error(
    run_es(
      data = data, outcome = y, treatment = treated,
      time = time, timing = event_time, fe = ~ unit_id,
      cluster = "nonexistent_column"
    ),
    regexp = "cluster.*column"
  )
})

test_that("run_es() errors when time_transform=TRUE without unit", {
  data <- make_universal_panel(seed = 2727)
  event_time <- unique(data$time)[5]

  expect_error(
    run_es(
      data = data, outcome = y, treatment = treated,
      time = time, timing = event_time, fe = ~ unit_id,
      time_transform = TRUE
      # Missing unit parameter
    ),
    regexp = "time_transform.*requires.*unit"
  )
})

test_that("run_es() errors with invalid interval", {
  data <- make_universal_panel(seed = 2828)
  event_time <- unique(data$time)[5]

  expect_error(
    run_es(
      data = data, outcome = y, treatment = treated,
      time = time, timing = event_time, fe = ~ unit_id,
      interval = -1
    ),
    regexp = "interval.*positive"
  )

  expect_error(
    run_es(
      data = data, outcome = y, treatment = treated,
      time = time, timing = event_time, fe = ~ unit_id,
      interval = 0
    ),
    regexp = "interval.*positive"
  )
})

test_that("run_es() errors with invalid treatment variable", {
  data <- make_universal_panel(seed = 2929)
  data$bad_treatment <- "not_numeric"  # Invalid treatment
  event_time <- unique(data$time)[5]

  expect_error(
    run_es(
      data = data, outcome = y, treatment = bad_treatment,
      time = time, timing = event_time, fe = ~ unit_id
    ),
    regexp = "treatment.*numeric|logical"
  )
})

test_that("run_es() errors with Date type mismatch", {
  data <- make_universal_panel(seed = 3030)
  data$date_time <- as.Date("2020-01-01") + data$time  # Fix Date arithmetic
  event_time <- 5  # Numeric, but time is Date

  expect_error(
    run_es(
      data = data, outcome = y, treatment = treated,
      time = date_time, timing = event_time, fe = ~ unit_id
    ),
    regexp = "timing.*Date.*time.*Date"
  )
})

test_that("run_es() errors with invalid covariates specification", {
  data <- make_universal_panel(seed = 3131)
  event_time <- unique(data$time)[5]

  expect_error(
    run_es(
      data = data, outcome = y, treatment = treated,
      time = time, timing = event_time, fe = ~ unit_id,
      covariates = "not_a_formula"
    ),
    regexp = "covariates.*formula"
  )
})

test_that("plot_es() errors with invalid type", {
  data <- make_universal_panel(seed = 3232)
  event_time <- unique(data$time)[5]

  res <- run_es(
    data = data, outcome = y, treatment = treated,
    time = time, timing = event_time, fe = ~ unit_id
  )

  expect_error(
    plot_es(res, type = "invalid_type")
    # Just check that it errors, don't check message (may be localized)
  )
})

test_that("plot_es() errors with invalid theme_style", {
  data <- make_universal_panel(seed = 3333)
  event_time <- unique(data$time)[5]

  res <- run_es(
    data = data, outcome = y, treatment = treated,
    time = time, timing = event_time, fe = ~ unit_id
  )

  expect_error(
    plot_es(res, theme_style = "invalid_theme")
    # Just check that it errors, don't check message (may be localized)
  )
})

test_that("plot_es() warns with non-es_result object", {
  fake_data <- data.frame(
    relative_time = -2:2,
    estimate = rnorm(5),
    conf_low_95 = rnorm(5),
    conf_high_95 = rnorm(5)
  )

  expect_warning(
    plot_es(fake_data),
    regexp = "not class.*es_result"
  )
})

# =============================================================================
# Tests: Backward Compatibility and Integration
# =============================================================================

test_that("run_es() results are compatible with broom::tidy expectations", {
  data <- make_universal_panel(seed = 3434)
  event_time <- unique(data$time)[5]

  res <- run_es(
    data = data, outcome = y, treatment = treated,
    time = time, timing = event_time, fe = ~ unit_id
  )

  # Should have standard tidy columns
  expect_true("term" %in% names(res))
  expect_true("estimate" %in% names(res))
  expect_true("std.error" %in% names(res))
  expect_true("statistic" %in% names(res))
  expect_true("p.value" %in% names(res))
})

test_that("run_es() maintains consistent output structure across methods", {
  data <- make_staggered_panel(seed = 3535)

  res_classic <- run_es(
    data = data, outcome = y, treatment = ever_treated,
    time = time, timing = timing, staggered = TRUE,
    method = "classic", fe = ~ unit_id + time,
    lead_range = 2, lag_range = 2
  )

  # Check that both methods have the same core columns
  core_cols <- c("term", "estimate", "std.error", "relative_time", "is_baseline")
  expect_true(all(core_cols %in% names(res_classic)))

  if (has_sunab()) {
    res_sunab <- run_es(
      data = data, outcome = y, treatment = ever_treated,
      time = time, timing = timing, staggered = TRUE,
      method = "sunab", fe = ~ unit_id + time
    )

    expect_true(all(core_cols %in% names(res_sunab)))
  }
})

# =============================================================================
# End of Test Suite
# =============================================================================
