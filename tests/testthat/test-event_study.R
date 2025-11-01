# test-event_study.R
# Comprehensive tests for fixes::run_es / plot_es / plot_es_interactive
# Tests cover all recent bug fixes and enhancements in v0.7.1
# -------------------------------------------------------------------

library(testthat)
library(dplyr)
library(ggplot2)

# Helper functions -------------------------------------------------------------

has_sunab <- function() {
  tryCatch({
    getFromNamespace("sunab", "fixest")
    TRUE
  }, error = function(e) FALSE)
}

check_baseline_row <- function(res, expected_baseline = -1) {
  # Verify that baseline row exists with zeros
  expect_true("is_baseline" %in% names(res))
  expect_true(expected_baseline %in% res$relative_time)

  baseline_row <- res[res$relative_time == expected_baseline, ]
  expect_equal(nrow(baseline_row), 1)
  expect_equal(baseline_row$estimate, 0)
  expect_equal(baseline_row$std.error, 0)
  expect_true(baseline_row$is_baseline)
}

# Data generation utilities ----------------------------------------------------

make_universal_panel <- function(n_units = 50, T = 8, start = 1L) {
  ids   <- seq_len(n_units)
  times <- seq.int(start, start + T - 1L)
  treated_flag <- sample(c(0L, 1L), n_units, replace = TRUE)

  df <- tidyr::expand_grid(id = ids, time = times) |>
    dplyr::arrange(id, time) |>
    dplyr::mutate(
      treated_univ = rep(treated_flag, each = T),
      event_index  = start + floor(T / 2),
      is_treated   = ifelse(treated_univ == 1L & time >= event_index, 1L, 0L),
      x1 = rnorm(n()),
      u_fe = rep(rnorm(n_units, sd = 0.2), each = T),
      t_fe = rep(rnorm(T, sd = 0.2), times = n_units),
      y  = 1 + 0.8 * is_treated + 0.2 * x1 + u_fe + t_fe + rnorm(n())
    )
  df
}

make_staggered_panel <- function(n_units = 40, T = 10, start = 1L,
                                 p_never = 0.25, adopt_years = 4L) {
  ids   <- seq_len(n_units)
  times <- seq.int(start, start + T - 1L)

  adopt_pool <- c(NA, seq.int(start + 4L, length.out = adopt_years))
  probs <- c(p_never, rep((1 - p_never) / adopt_years, adopt_years))
  treat_year <- sample(adopt_pool, n_units, replace = TRUE, prob = probs)

  df <- tidyr::expand_grid(id = ids, time = times) |>
    dplyr::arrange(id, time) |>
    dplyr::mutate(
      timing      = rep(treat_year, each = T),
      treated_var = ifelse(!is.na(timing), 1L, 0L),
      is_treated  = ifelse(!is.na(timing) & time >= timing, 1L, 0L),
      x1 = rnorm(n()),
      u_fe = rep(rnorm(n_units, sd = 0.1), each = T),
      t_fe = rep(rnorm(T, sd = 0.1), times = n_units),
      y  = 1 + 0.5 * is_treated + 0.1 * x1 + u_fe + t_fe + rnorm(n()),
      w  = abs(rnorm(n(), mean = 1, sd = 0.2))
    )
  df
}

# ==============================================================================
# 1. Classic (universal timing) tests
# ==============================================================================

test_that("classic run_es: baseline row included with zeros", {
  set.seed(123)
  panel_data <- make_universal_panel(n_units = 60, T = 9, start = 1L)

  res <- run_es(
    data       = panel_data,
    outcome    = y,
    treatment  = treated_univ,
    time       = time,
    timing     = unique(panel_data$event_index)[1],
    fe         = ~ id,
    cluster    = ~ id,
    lead_range = 3,
    lag_range  = 3,
    baseline   = -1,
    conf.level = c(0.90, 0.95, 0.99)
  )

  expect_s3_class(res, "es_result")
  expect_true(all(c("term","estimate","std.error","relative_time","is_baseline") %in% names(res)))
  expect_true(all(c("conf_low_90","conf_high_90","conf_low_95","conf_high_95","conf_low_99","conf_high_99") %in% names(res)))

  # Check baseline row
  check_baseline_row(res, expected_baseline = -1)

  # Check that attributes are set correctly
  expect_equal(attr(res, "baseline"), -1)
  expect_equal(attr(res, "lead_range"), 3)
  expect_equal(attr(res, "lag_range"), 3)
})

test_that("classic run_es: lead/lag range filtering works correctly", {
  set.seed(124)
  panel_data <- make_universal_panel(n_units = 50, T = 12, start = 1L)

  res <- run_es(
    data       = panel_data,
    outcome    = y,
    treatment  = treated_univ,
    time       = time,
    timing     = 6,
    fe         = ~ id,
    lead_range = 2,
    lag_range  = 3
  )

  # Should only include relative times from -2 to 3
  expect_true(all(res$relative_time >= -2 & res$relative_time <= 3))
  expect_equal(min(res$relative_time), -2)
  expect_equal(max(res$relative_time), 3)

  # Baseline should be included
  expect_true(-1 %in% res$relative_time)
})

test_that("classic run_es: term column uses numeric format", {
  set.seed(125)
  panel_data <- make_universal_panel(n_units = 40, T = 8)

  res <- run_es(
    data       = panel_data,
    outcome    = y,
    treatment  = treated_univ,
    time       = time,
    timing     = 4,
    fe         = ~ id,
    lead_range = 2,
    lag_range  = 2
  )

  # All terms should be numeric strings
  expect_true(all(grepl("^-?\\d+$", res$term)))
  expect_false(any(grepl("::", res$term)))
})

test_that("classic run_es: custom baseline works", {
  set.seed(126)
  panel_data <- make_universal_panel(n_units = 50, T = 10)

  res <- run_es(
    data       = panel_data,
    outcome    = y,
    treatment  = treated_univ,
    time       = time,
    timing     = 5,
    fe         = ~ id,
    lead_range = 3,
    lag_range  = 3,
    baseline   = -2
  )

  check_baseline_row(res, expected_baseline = -2)
  expect_equal(attr(res, "baseline"), -2)
})

# ==============================================================================
# 2. Staggered timing (classic method) tests
# ==============================================================================

test_that("classic staggered: works with unit-varying timing", {
  set.seed(202)
  df <- make_staggered_panel(n_units = 50, T = 10, start = 1L)

  res <- run_es(
    data       = df,
    outcome    = y,
    treatment  = treated_var,
    time       = time,
    timing     = timing,
    staggered  = TRUE,
    fe         = ~ id + time,
    cluster    = ~ id,
    weights    = ~ w,
    lead_range = 2,
    lag_range  = 3,
    baseline   = -1
  )

  expect_s3_class(res, "es_result")
  expect_true(all(c("term","estimate","relative_time","is_baseline") %in% names(res)))
  check_baseline_row(res, expected_baseline = -1)
  expect_equal(attr(res, "staggered"), TRUE)
})

# ==============================================================================
# 3. sunab method tests
# ==============================================================================

test_that("sunab: baseline row added with zeros (default baseline = -1)", {
  skip_if_not(has_sunab(), "fixest::sunab() not available")

  set.seed(303)
  df <- make_staggered_panel(n_units = 60, T = 10, start = 2001L) |>
    dplyr::mutate(year = time, year_treated = timing)

  res <- run_es(
    data       = df,
    outcome    = y,
    treatment  = treated_var,
    time       = year,
    timing     = year_treated,
    staggered  = TRUE,
    method     = "sunab",
    fe         = ~ id + year,
    cluster    = ~ id
  )

  expect_s3_class(res, "es_result")
  expect_true(attr(res, "sunab_used"))

  # Check baseline row exists
  check_baseline_row(res, expected_baseline = -1)
  expect_equal(attr(res, "baseline"), -1)
})

test_that("sunab: term column uses numeric format (not 'year::-9')", {
  skip_if_not(has_sunab(), "fixest::sunab() not available")

  set.seed(304)
  df <- make_staggered_panel(n_units = 50, T = 10, start = 2001L) |>
    dplyr::mutate(year = time, year_treated = timing)

  res <- run_es(
    data       = df,
    outcome    = y,
    treatment  = treated_var,
    time       = year,
    timing     = year_treated,
    staggered  = TRUE,
    method     = "sunab",
    fe         = ~ id + year
  )

  # All terms should be clean numeric strings
  expect_true(all(grepl("^-?\\d+$", res$term)))
  expect_false(any(grepl("::", res$term)))
})

test_that("sunab: lead/lag range filtering works", {
  skip_if_not(has_sunab(), "fixest::sunab() not available")

  set.seed(305)
  df <- make_staggered_panel(n_units = 70, T = 12, start = 2000L) |>
    dplyr::mutate(year = time, year_treated = timing)

  res <- run_es(
    data       = df,
    outcome    = y,
    treatment  = treated_var,
    time       = year,
    timing     = year_treated,
    staggered  = TRUE,
    method     = "sunab",
    fe         = ~ id + year,
    lead_range = 3,
    lag_range  = 4
  )

  # Should only include times within [-3, 4]
  expect_true(all(res$relative_time >= -3 & res$relative_time <= 4))

  # Baseline should be included if within range
  expect_true(-1 %in% res$relative_time)
})

test_that("sunab: custom baseline works", {
  skip_if_not(has_sunab(), "fixest::sunab() not available")

  set.seed(306)
  df <- make_staggered_panel(n_units = 50, T = 10, start = 2001L) |>
    dplyr::mutate(year = time, year_treated = timing)

  res <- run_es(
    data       = df,
    outcome    = y,
    treatment  = treated_var,
    time       = year,
    timing     = year_treated,
    staggered  = TRUE,
    method     = "sunab",
    fe         = ~ id + year,
    baseline   = -2,
    lead_range = 4,
    lag_range  = 3
  )

  # Note: sunab may not have coefficient for -2, so baseline row should be added
  expect_true(-2 %in% res$relative_time)
  expect_equal(attr(res, "baseline"), -2)

  # Check that is_baseline marks the correct period
  expect_true(res$is_baseline[res$relative_time == -2])
})

# ==============================================================================
# 4. Plotting tests
# ==============================================================================

test_that("plot_es: produces ggplot objects (ribbon & errorbar)", {
  set.seed(401)
  panel_data <- make_universal_panel()

  res <- run_es(
    data       = panel_data,
    outcome    = y,
    treatment  = treated_univ,
    time       = time,
    timing     = unique(panel_data$event_index)[1],
    fe         = ~ id,
    lead_range = 2,
    lag_range  = 2,
    conf.level = c(0.90, 0.95)
  )

  p1 <- plot_es(res, ci_level = 0.95, type = "ribbon", theme_style = "bw")
  expect_s3_class(p1, "ggplot")

  p2 <- plot_es(res, ci_level = 0.90, type = "errorbar", theme_style = "classic")
  expect_s3_class(p2, "ggplot")
})

test_that("plot_es_interactive: works when plotly is available", {
  skip_if_not_installed("plotly")

  set.seed(402)
  panel_data <- make_universal_panel(n_units = 40, T = 8)

  res <- run_es(
    data       = panel_data,
    outcome    = y,
    treatment  = treated_univ,
    time       = time,
    timing     = 4,
    fe         = ~ id,
    lead_range = 2,
    lag_range  = 2
  )

  p <- plot_es_interactive(res, ci_level = 0.95)
  expect_s3_class(p, "plotly")
})

# ==============================================================================
# 5. time_transform tests
# ==============================================================================

test_that("run_es: time_transform works with unit identifier", {
  set.seed(501)
  df <- make_staggered_panel(n_units = 40, T = 8, start = 1L) |>
    dplyr::rename(unit_id = id)

  res <- run_es(
    data           = df,
    outcome        = y,
    treatment      = treated_var,
    time           = time,
    timing         = timing,
    staggered      = TRUE,
    time_transform = TRUE,
    unit           = "unit_id",  # Pass as character string
    fe             = ~ unit_id + time,
    cluster        = ~ unit_id,
    weights        = ~ w,
    lead_range     = 1,
    lag_range      = 2,
    baseline       = -1
  )

  expect_s3_class(res, "es_result")
  expect_true("relative_time" %in% names(res))
  check_baseline_row(res, expected_baseline = -1)
})

# ==============================================================================
# 6. Error handling tests
# ==============================================================================

test_that("run_es: baseline outside range error", {
  set.seed(601)
  panel_data <- make_universal_panel()

  expect_error(
    run_es(
      data       = panel_data,
      outcome    = y,
      treatment  = treated_univ,
      time       = time,
      timing     = unique(panel_data$event_index)[1],
      fe         = ~ id,
      lead_range = 2,
      lag_range  = 2,
      baseline   = -5
    ),
    regexp = "outside|range"
  )
})

test_that("run_es: invalid cluster length error", {
  set.seed(602)
  panel_data <- make_universal_panel()

  expect_error(
    run_es(
      data       = panel_data,
      outcome    = y,
      treatment  = treated_univ,
      time       = time,
      timing     = unique(panel_data$event_index)[1],
      fe         = ~ id,
      cluster    = 1:10
    ),
    regexp = "cluster|length"
  )
})

# ==============================================================================
# 7. S3 methods tests
# ==============================================================================

test_that("print.es_result: displays correctly", {
  set.seed(701)
  panel_data <- make_universal_panel(n_units = 30, T = 6)

  res <- run_es(
    data       = panel_data,
    outcome    = y,
    treatment  = treated_univ,
    time       = time,
    timing     = 3,
    fe         = ~ id,
    lead_range = 1,
    lag_range  = 1
  )

  expect_output(print(res), "Event Study Result")
})

test_that("autoplot.es_result: returns ggplot", {
  set.seed(702)
  panel_data <- make_universal_panel(n_units = 30, T = 6)

  res <- run_es(
    data       = panel_data,
    outcome    = y,
    treatment  = treated_univ,
    time       = time,
    timing     = 3,
    fe         = ~ id,
    lead_range = 1,
    lag_range  = 1
  )

  p <- ggplot2::autoplot(res)
  expect_s3_class(p, "ggplot")
})

# ==============================================================================
# 8. Integration test with fixest built-in data
# ==============================================================================

test_that("run_es: works with fixest::base_did", {
  skip_if_not_installed("fixest")

  data("base_did", package = "fixest")

  res <- run_es(
    data       = base_did,
    outcome    = y,
    treatment  = treat,
    time       = period,
    timing     = 5,
    fe         = ~ id + period,
    lead_range = 3,
    lag_range  = 3,
    baseline   = -1,
    cluster    = ~ id
  )

  expect_s3_class(res, "es_result")
  check_baseline_row(res, expected_baseline = -1)
  expect_equal(nrow(res), 7)  # -3 to 3
})

test_that("run_es: works with fixest::base_stagg using sunab", {
  skip_if_not(has_sunab(), "fixest::sunab() not available")
  skip_if_not_installed("fixest")

  data("base_stagg", package = "fixest")

  res <- run_es(
    data       = base_stagg,
    outcome    = y,
    treatment  = treated,
    time       = year,
    timing     = year_treated,
    fe         = ~ id + year,
    staggered  = TRUE,
    method     = "sunab",
    lead_range = 3,
    lag_range  = 3
  )

  expect_s3_class(res, "es_result")
  expect_true(-1 %in% res$relative_time)
  expect_true(all(grepl("^-?\\d+$", res$term)))
})
