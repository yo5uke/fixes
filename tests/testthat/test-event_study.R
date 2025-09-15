# test-event_study.R (revised & robust)
# -------------------------------------------------------------------
# Stable tests for fixes::run_es / plot_es
# - Tolerates both inclusion/exclusion of the baseline period
# - Avoids grid::unit name conflict by using 'unit_id'
# - Allows multiple possible error messages (implementation-agnostic)
# -------------------------------------------------------------------

library(testthat)
library(dplyr)
library(ggplot2)

# helper -----------------------------------------------------------------------
has_sunab <- function() {
  "sunab" %in% getNamespaceExports("fixest")
}

check_baseline_behavior <- function(res) {
  # If is_baseline contains TRUE, verify that estimates are zero there.
  # Otherwise, check that the baseline period is *absent* (normalized case).
  expect_true("is_baseline" %in% names(res))
  b <- attr(res, "baseline")
  has_true <- any(isTRUE(res$is_baseline), na.rm = TRUE)

  if (isTRUE(has_true)) {
    expect_true(all(res$estimate[res$is_baseline %in% TRUE] == 0))
  } else {
    # Normalized pattern: baseline row omitted
    expect_false(b %in% res$relative_time)
  }
}

# ------------------------------------------------------------------------------
# 0. Data generation utilities
# ------------------------------------------------------------------------------

make_universal_panel <- function(n_units = 50, T = 8, start = 1L) {
  # time is 1..T (safe even with time_transform=TRUE)
  ids   <- seq_len(n_units)
  times <- seq.int(start, start + T - 1L)
  treated_flag <- sample(c(0L, 1L), n_units, replace = TRUE)

  df <- tidyr::expand_grid(id = ids, time = times) |>
    arrange(id, time) |>
    mutate(
      treated_univ = rep(treated_flag, each = T),
      event_index  = start + floor(T / 2),  # universal event period
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
    arrange(id, time) |>
    mutate(
      timing      = rep(treat_year, each = T),      # adoption year (staggered)
      treated_var = ifelse(!is.na(timing), 1L, 0L), # ever treated flag
      is_treated  = ifelse(!is.na(timing) & time >= timing, 1L, 0L),
      x1 = rnorm(n()),
      u_fe = rep(rnorm(n_units, sd = 0.1), each = T),
      t_fe = rep(rnorm(T, sd = 0.1), times = n_units),
      y  = 1 + 0.5 * is_treated + 0.1 * x1 + u_fe + t_fe + rnorm(n()),
      w  = abs(rnorm(n(), mean = 1, sd = 0.2))
    )
  df
}

# ------------------------------------------------------------------------------
# 1. Classic (universal timing): include only unit FE (no time FE)
# ------------------------------------------------------------------------------

test_that("classic run_es works with baseline and returns correct structure", {
  set.seed(123)
  panel_data <- make_universal_panel(n_units = 60, T = 9, start = 1L)

  res <- run_es(
    data       = panel_data,
    outcome    = y,
    treatment  = treated_univ,
    time       = time,                 # 1..T
    timing     = unique(panel_data$event_index)[1],  # universal timing
    fe         = ~ id,                 # unit FE only, avoid collinearity
    cluster    = ~ id,
    lead_range = 2,
    lag_range  = 2,
    baseline   = -1,
    interval   = 1,
    conf.level = c(0.90, 0.95, 0.99)
  )

  expect_s3_class(res, "es_result")
  expect_true(all(c("term","estimate","std.error","relative_time","is_baseline") %in% names(res)))
  expect_true(all(c("conf_low_90","conf_high_90","conf_low_95","conf_high_95","conf_low_99","conf_high_99") %in% names(res)))
  expect_true(!is.null(attr(res, "model_formula")))
  check_baseline_behavior(res)
})

# ------------------------------------------------------------------------------
# 2. plot_es: should return ggplot objects for ribbon & errorbar types
# ------------------------------------------------------------------------------

test_that("plot_es produces ggplot objects (ribbon & errorbar)", {
  set.seed(123)
  panel_data <- make_universal_panel()

  res <- run_es(
    data       = panel_data,
    outcome    = y,
    treatment  = treated_univ,
    time       = time,
    timing     = unique(panel_data$event_index)[1],
    fe         = ~ id,
    cluster    = ~ id,
    lead_range = 2,
    lag_range  = 2,
    baseline   = -1,
    interval   = 1,
    conf.level = c(0.90, 0.95, 0.99)
  )

  p1 <- plot_es(res, ci_level = 0.95, type = "ribbon", theme_style = "bw")
  expect_s3_class(p1, "ggplot")

  p2 <- plot_es(res, ci_level = 0.90, type = "errorbar", theme_style = "classic")
  expect_s3_class(p2, "ggplot")
})

# ------------------------------------------------------------------------------
# 3. Classic (staggered timing): should work with two-way FE
# ------------------------------------------------------------------------------

test_that("classic (staggered) run_es works with two-way FE", {
  set.seed(202)
  df <- make_staggered_panel(n_units = 50, T = 10, start = 1L)

  res <- run_es(
    data       = df,
    outcome    = y,
    treatment  = treated_var,
    time       = time,
    timing     = timing,     # column with adoption time
    staggered  = TRUE,
    fe         = ~ id + time,
    cluster    = ~ id,
    weights    = ~ w,
    lead_range = 2,
    lag_range  = 3,
    baseline   = -1,
    conf.level = 0.95
  )

  expect_s3_class(res, "es_result")
  expect_true(all(c("term","estimate","relative_time","is_baseline") %in% names(res)))
  check_baseline_behavior(res)
})

# ------------------------------------------------------------------------------
# 4. sunab method (only if fixest::sunab is available)
# ------------------------------------------------------------------------------

test_that("run_es supports method = 'sunab' when available", {
  skip_if_not(has_sunab(), "fixest::sunab() not available; skipping.")

  set.seed(303)
  df <- make_staggered_panel(n_units = 60, T = 10, start = 2001L) |>
    mutate(year = time, year_treated = timing)

  res <- run_es(
    data       = df,
    outcome    = y,
    treatment  = treated_var,
    time       = year,
    timing     = year_treated,
    staggered  = TRUE,
    method     = "sunab",
    fe         = ~ id + year,
    cluster    = ~ id,
    lead_range = 3,
    lag_range  = 3,
    conf.level = 0.95
  )

  expect_s3_class(res, "es_result")
  expect_true(all(c("term","estimate") %in% names(res)))
})

# ------------------------------------------------------------------------------
# 5. time_transform + weights: safe with consecutive time × unit_id
# ------------------------------------------------------------------------------

test_that("run_es works with time_transform=TRUE and weights", {
  set.seed(404)
  df <- make_staggered_panel(n_units = 40, T = 8, start = 1L) %>%
    dplyr::rename(unit_id = id)   # avoid conflict with dplyr::id()

  res <- run_es(
    data           = df,
    outcome        = y,
    treatment      = treated_var,
    time           = time,
    timing         = timing,
    staggered      = TRUE,
    time_transform = TRUE,
    unit           = "unit_id",     # safe column name
    fe             = ~ unit_id + time,
    cluster        = ~ unit_id,
    weights        = ~ w,
    lead_range     = 1,
    lag_range      = 2,
    baseline       = -1,
    interval       = 1,
    conf.level     = 0.95
  )

  expect_s3_class(res, "es_result")
  expect_true("relative_time" %in% names(res))
})

# ------------------------------------------------------------------------------
# 6. Error handling: should return informative messages
# ------------------------------------------------------------------------------

test_that("run_es provides informative errors on invalid inputs", {
  set.seed(505)
  panel_data <- make_universal_panel()

  # baseline outside of the range
  expect_error(
    run_es(
      data       = panel_data,
      outcome    = y,
      treatment  = treated_univ,
      time       = time,
      timing     = unique(panel_data$event_index)[1],
      fe         = ~ id,
      lead_range = 1,
      lag_range  = 1,
      baseline   = -5
    ),
    regexp = "outside the range|outside.*range",
    ignore.case = TRUE
  )

  # cluster type/length error (accept multiple possible implementations)
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
    regexp = "Invalid type for cluster|cluster argument|Vector `cluster` must be length nrow\\(data\\)",
    ignore.case = TRUE
  )
})
