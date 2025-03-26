library(testthat)
library(tibble)
library(dplyr)
library(fixest)
library(rlang)

test_that("run_es works with covariates provided as an additive expression", {
  # Create test data with additional covariates
  df <- tibble::tibble(
    id      = rep(1:5, each = 11),
    year    = rep(2000:2010, times = 5),
    y       = rnorm(55),
    treated = if_else(id %in% c(1, 3, 5), 1, 0),
    x1      = rnorm(55),
    x2      = rnorm(55)
  )

  # Using with() ensures that the symbols x1 and x2 are found in the evaluation environment.
  result_expr <- with(df, run_es(
    data       = df,
    outcome    = y,
    treatment  = treated,
    time       = year,
    timing     = 2005,
    lead_range = 3,
    lag_range  = 2,
    fe         = id + year,
    covariates = x1 + x2,   # additive expression
    cluster    = "id",
    interval   = 1
  ))

  expect_true("relative_time" %in% colnames(result_expr))
  expect_equal(min(result_expr$relative_time, na.rm = TRUE), -3)
  expect_equal(max(result_expr$relative_time, na.rm = TRUE), 2)
})

test_that("run_es works with covariates provided as a character vector", {
  # For interval = 5, create data with 5-year spaced 'year' so that relative_time is integer.
  df <- tibble::tibble(
    id      = rep(1:5, each = 3),
    year    = rep(c(2000, 2005, 2010), times = 5),  # 5-year spaced
    y       = rnorm(15),
    treated = if_else(id %in% c(1, 3, 5), 1, 0),
    x1      = rnorm(15),
    x2      = rnorm(15)
  )

  result_char <- run_es(
    data       = df,
    outcome    = y,
    treatment  = treated,
    time       = year,
    timing     = 2005,
    lead_range = 1,   # will keep rows with relative_time in [-1, 1]
    lag_range  = 1,
    fe         = id + year,
    covariates = c("x1", "x2"),
    cluster    = "id",
    interval   = 5
  )

  expect_true("relative_time" %in% colnames(result_char))
  expect_equal(min(result_char$relative_time, na.rm = TRUE), -5)
  expect_equal(max(result_char$relative_time, na.rm = TRUE), 5)
  expect_true("estimate" %in% colnames(result_char))
})

test_that("run_es works correctly when covariates are not specified", {
  # Create test data without covariates
  df <- tibble::tibble(
    id      = rep(1:5, each = 7),
    year    = rep(2000:2006, times = 5),
    y       = rnorm(35),
    treated = if_else(id %in% c(2, 4), 1, 0)
  )

  result_nocov <- run_es(
    data       = df,
    outcome    = y,
    treatment  = treated,
    time       = year,
    timing     = 2003,
    lead_range = 2,
    lag_range  = 2,
    fe         = id + year,
    cluster    = "id",
    interval   = 1
  )

  expect_true("term" %in% colnames(result_nocov))
  expect_true("estimate" %in% colnames(result_nocov))
  expect_true("relative_time" %in% colnames(result_nocov))
  expect_equal(min(result_nocov$relative_time, na.rm = TRUE), -2)
  expect_equal(max(result_nocov$relative_time, na.rm = TRUE), 2)
})

test_that("run_es handles outcome expressions (e.g., log(y)) and covariates correctly", {
  # Create test data ensuring y > 0 for log() and include covariates
  df2 <- tibble::tibble(
    id      = rep(1:5, each = 5),
    year    = rep(2000:2004, times = 5),
    y       = abs(rnorm(25)) + 0.1,  # ensure y > 0 for log()
    treated = if_else(id %in% c(1, 3, 5), 1, 0),
    x1      = rnorm(25),
    x2      = rnorm(25)
  )

  result_log <- run_es(
    data       = df2,
    outcome    = log(y),
    treatment  = treated,
    time       = year,
    timing     = 2002,
    lead_range = 1,
    lag_range  = 1,
    fe         = id,
    covariates = c("x1", "x2"),
    cluster    = "id",
    interval   = 1
  )

  expect_true("estimate" %in% colnames(result_log))
  expect_true("relative_time" %in% colnames(result_log))
  expect_true(any(result_log$term == "lead1"))
  expect_true(any(result_log$term == "lag0"))
})

test_that("run_es warns when lead/lag range exceeds available data", {
  df <- tibble::tibble(
    id      = rep(1:5, each = 11),
    year    = rep(2000:2010, times = 5),
    y       = rnorm(55),
    treated = if_else(id %in% c(1, 3, 5), 1, 0),
    x1      = rnorm(55),
    x2      = rnorm(55)
  )

  expect_warning(
    run_es(
      data       = df,
      outcome    = y,
      treatment  = treated,
      time       = year,
      timing     = 2005,
      lead_range = 10,
      lag_range  = 10,
      fe         = id + year,
      covariates = c("x1", "x2"),
      cluster    = "id"
    ),
    regexp = "exceeds the available range"
  )
})
