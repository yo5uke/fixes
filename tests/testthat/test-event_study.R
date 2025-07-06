# test-event_study.R
# ------------------------------------------------------------------------------
# Unit tests for fixes::run_es() and fixes::plot_es()
#
# These tests are intended for a development environment.
# NOTE: The tests require that the *current* (開発中の最新版) run_es/plot_es functions
# are loaded into the R session (e.g. via devtools::load_all()).
# If you have only CRAN版fixes loaded, conf.levelなど新機能のテストは失敗します。
#
# This file is meant to be placed in tests/testthat/.
# ------------------------------------------------------------------------------

library(testthat)
library(dplyr)
library(ggplot2)

# ------------------------------------------------------------------------------
# 1. Minimal synthetic panel data for basic event study
# ------------------------------------------------------------------------------

set.seed(100)
n_firms <- 50
T <- 8
firm_id <- 1:n_firms
year_seq <- 2001:2008
# 50% treated, treatment from year 2005
treated <- sample(c(0, 1), n_firms, replace = TRUE)
panel_data <- expand.grid(firm_id = firm_id, year = year_seq) %>%
  dplyr::arrange(firm_id, year) %>%
  dplyr::mutate(
    treated_2005 = rep(treated, each = T),
    is_treated = ifelse(treated_2005 == 1 & year >= 2005, 1, 0),
    x1 = rnorm(n()),
    y = 1 + 0.8 * is_treated + 0.2 * x1 +
      rnorm(n()) + rep(rnorm(n_firms, sd = 0.2), each = T) + rnorm(T, sd = 0.2)[year - 2000]
  )

# ------------------------------------------------------------------------------
# 2. Test: run_es computes event study estimates on simple panel
# ------------------------------------------------------------------------------

test_that("run_es computes event study estimates on simple panel", {
  result <- run_es(
    data       = panel_data,
    outcome    = y,
    treatment  = treated_2005,
    time       = year,
    timing     = 2005,
    fe         = ~ firm_id + year,
    cluster    = ~ firm_id,
    lead_range = 2,
    lag_range  = 2,
    baseline   = -1,
    interval   = 1,
    conf.level = c(0.90, 0.95, 0.99)
  )

  # Structure checks
  expect_s3_class(result, "es_result")
  expect_true(all(c("term", "estimate", "conf_low_95", "conf_high_95", "relative_time") %in% names(result)))
  # Baseline term should have estimate = 0
  expect_true(any(result$is_baseline))
  expect_true(all(result[result$is_baseline, ]$estimate == 0))
  # Confidence intervals present for all levels
  expect_true("conf_low_90" %in% names(result))
  expect_true("conf_low_99" %in% names(result))
  # Model formula attribute present
  expect_true(!is.null(attr(result, "model_formula")))
})

# ------------------------------------------------------------------------------
# 3. Test: plot_es generates a ggplot object for ribbon and errorbar
# ------------------------------------------------------------------------------

test_that("plot_es generates a ggplot object", {
  result <- run_es(
    data       = panel_data,
    outcome    = y,
    treatment  = treated_2005,
    time       = year,
    timing     = 2005,
    fe         = ~ firm_id + year,
    cluster    = ~ firm_id,
    lead_range = 2,
    lag_range  = 2,
    baseline   = -1,
    conf.level = c(0.90, 0.95, 0.99)
  )
  # Ribbon plot
  p1 <- plot_es(result, ci_level = 0.95, type = "ribbon", theme_style = "bw")
  expect_s3_class(p1, "ggplot")
  # Errorbar plot
  p2 <- plot_es(result, ci_level = 0.90, type = "errorbar", theme_style = "classic")
  expect_s3_class(p2, "ggplot")
})

# ------------------------------------------------------------------------------
# 4. Test: run_es handles staggered treatment and weights
# ------------------------------------------------------------------------------

test_that("run_es handles staggered treatment timing and weights", {
  set.seed(200)
  n_units <- 40
  T <- 10
  # Staggered adoption: random years between 2005-2008, NA = never treated
  treat_year <- sample(c(NA, 2005:2008), n_units, replace = TRUE, prob = c(0.25, rep(0.1875, 4)))
  df <- expand.grid(unit = 1:n_units, year = 2001:2010) %>%
    dplyr::arrange(unit, year) %>%
    dplyr::mutate(
      timing = rep(treat_year, each = T),
      is_treated = ifelse(!is.na(timing) & year >= timing, 1, 0),
      treated_var = ifelse(!is.na(timing), 1, 0),
      x1 = rnorm(n()),
      y = 1 + 0.5 * is_treated + 0.1 * x1 +
        rnorm(n()) + rep(rnorm(n_units, sd = 0.1), each = T) + rnorm(T, sd = 0.1)[year - 2000],
      w = abs(rnorm(n(), mean = 1, sd = 0.2))
    )

  result <- run_es(
    data       = df,
    outcome    = y,
    treatment  = treated_var,
    time       = year,
    timing     = timing,
    staggered  = TRUE,
    fe         = ~ unit + year,
    cluster    = ~ unit,
    weights    = ~ w,
    lead_range = 2,
    lag_range  = 3,
    baseline   = -1,
    conf.level = c(0.95)
  )

  # Checks
  expect_s3_class(result, "es_result")
  expect_true(all(c("term", "estimate", "relative_time") %in% names(result)))
  expect_true(any(result$is_baseline))
  expect_true(all(result[result$is_baseline, ]$estimate == 0))
})

# ------------------------------------------------------------------------------
# 5. Test: run_es returns error if invalid arguments
# ------------------------------------------------------------------------------

test_that("run_es gives informative errors with invalid input", {
  expect_error(
    run_es(
      data = panel_data,
      outcome = y,
      treatment = treated_2005,
      time = year,
      timing = 2005,
      fe = ~ firm_id + year,
      cluster = ~ firm_id,
      lead_range = 2,
      lag_range = 2,
      baseline = 10  # Outside possible range!
    ),
    regexp = "outside the range"
  )
  expect_error(
    run_es(
      data = panel_data,
      outcome = y,
      treatment = treated_2005,
      time = year,
      timing = 2005,
      fe = ~ firm_id + year,
      cluster = 1:10  # Invalid cluster type
    ),
    regexp = "Invalid type for cluster"
  )
})

# ------------------------------------------------------------------------------
# End of test-event_study.R
# ------------------------------------------------------------------------------
