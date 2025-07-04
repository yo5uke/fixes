# ------------------------------------------------------------------------------
# Test data source note:
# The following test uses the 'castle.dta' dataset from the "Mixtape" book by
# Scott Cunningham, available at: https://github.com/scunning1975/mixtape
#
# License: MIT License
# Copyright (c) Scott Cunningham
#
# Permission is hereby granted, free of charge, to use this dataset for testing
# purposes, including within this R package, under the terms of the MIT license.
# ------------------------------------------------------------------------------

library(dplyr)

set.seed(123)

n_firms <- 1000
n_states <- 50
T <- 36

firm_id <- 1:n_firms
state_id <- sample(n_states, size = n_firms, replace = TRUE)
year_seq <- 1980:2015

fe_firm <- rnorm(n_firms, mean = 0, sd = 0.5)
fe_year <- rnorm(T, mean = 0, sd = 0.5)
error <- rnorm(n_firms * T, mean = 0, sd = 0.5)

treated_1998 <- sample(c(1, 0), size = n_firms, replace = TRUE, prob = c(0.5, 0.5))

panel_data <- tibble::tibble(
  firm_id       = rep(firm_id, each = T),
  state_id      = rep(state_id, each = T),
  year          = rep(year_seq, times = n_firms),
  fe_firm       = rep(fe_firm, each = T),
  fe_year       = rep(fe_year, times = n_firms),
  error         = error,
  treated_1998  = rep(treated_1998, each = T),
  is_treated    = ifelse(treated_1998 == 1 & rep(year_seq, times = n_firms) >= 1998, 1, 0)
) |>
  dplyr::mutate(
    x1 = rnorm(n()),
    x2 = as.numeric(firm_id %% 5),
    y = dplyr::case_when(
      is_treated == 1 ~ rnorm(n(), mean = 2, sd = 0.2) + fe_firm + fe_year + error,
      TRUE            ~ fe_firm + fe_year + error
    )
  )


test_that("Basic usage with fixed effects and clustering", {
  result <- run_es(
    data       = panel_data,
    outcome    = y,
    treatment  = treated_1998,
    time       = year,
    timing     = 1998,
    lead_range = 5,
    lag_range  = 5,
    covariates = NULL,
    fe         = ~ firm_id + year,
    cluster    = ~ state_id,
    baseline   = -1,
    interval   = 1
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("conf_low", "conf_high") %in% names(result)))
  expect_true(any(result$is_baseline))
  expect_equal(result[result$is_baseline, ]$estimate, 0)
})

test_that("Cluster specified as character vector", {
  result <- run_es(
    data       = panel_data,
    outcome    = y,
    treatment  = treated_1998,
    time       = year,
    timing     = 1998,
    lead_range = 3,
    lag_range  = 3,
    covariates = NULL,
    fe         = ~ firm_id + year,
    cluster    = c("state_id"),
    baseline   = -1,
    interval   = 1
  )

  expect_s3_class(result, "data.frame")
  expect_true("conf_low" %in% names(result))
})

test_that("Error when baseline is outside lead/lag range (too large)", {
  expect_error(
    run_es(
      data       = panel_data,
      outcome    = y,
      treatment  = treated_1998,
      time       = year,
      timing     = 1998,
      lead_range = 2,
      lag_range  = 2,
      covariates = NULL,
      fe         = ~ firm_id + year,
      cluster    = ~ state_id,
      baseline   = 6,  # lag6 is outside range
      interval   = 1
    ),
    regexp = "baseline.*outside the range"
  )
})

test_that("Error when baseline is outside lead/lag range (too small)", {
  expect_error(
    run_es(
      data       = panel_data,
      outcome    = y,
      treatment  = treated_1998,
      time       = year,
      timing     = 1998,
      lead_range = 2,
      lag_range  = 2,
      covariates = NULL,
      fe         = ~ firm_id + year,
      cluster    = ~ state_id,
      baseline   = -10,  # lead10 is outside range
      interval   = 1
    ),
    regexp = "baseline.*outside the range"
  )
})

test_that("Error when cluster input is invalid type", {
  expect_error(
    run_es(
      data       = panel_data,
      outcome    = y,
      treatment  = treated_1998,
      time       = year,
      timing     = 1998,
      lead_range = 2,
      lag_range  = 2,
      covariates = NULL,
      fe         = ~ firm_id + year,
      cluster    = 1:10,  # numeric vector (invalid)
      baseline   = -1,
      interval   = 1
    ),
    regexp = "Invalid type for cluster argument"
  )
})

test_that("run_es works with time_transform = TRUE and unit", {
  panel_data_alt <- panel_data |>
    dplyr::mutate(date = as.Date(paste0(year, "-01-01")))

  result <- run_es(
    data            = panel_data_alt,
    outcome         = y,
    treatment       = treated_1998,
    time            = date,
    timing          = 19,
    lead_range      = 3,
    lag_range       = 3,
    fe              = ~ firm_id + year,
    cluster         = ~ state_id,
    baseline        = -1,
    interval        = 1,
    time_transform  = TRUE,
    unit            = firm_id
  )

  expect_s3_class(result, "data.frame")
  expect_true("relative_time" %in% names(result))
  expect_true(any(result$is_baseline))
  expect_equal(result[result$is_baseline, ]$estimate, 0)
})

test_that("Error when time_transform = TRUE and unit is missing", {
  panel_data_alt <- panel_data |>
    dplyr::mutate(date = as.Date(paste0(year, "-01-01")))

  expect_error(
    run_es(
      data            = panel_data_alt,
      outcome         = y,
      treatment       = treated_1998,
      time            = date,
      timing          = 19,
      lead_range      = 3,
      lag_range       = 3,
      fe              = ~ firm_id + year,
      cluster         = ~ state_id,
      baseline        = -1,
      interval        = 1,
      time_transform  = TRUE
      # unit is missing
    ),
    regexp = "must specify the `unit` argument"
  )
})

test_that("Warning when unit is given but time_transform = FALSE", {
  expect_warning(
    run_es(
      data       = panel_data,
      outcome    = y,
      treatment  = treated_1998,
      time       = year,
      timing     = 1998,
      lead_range = 2,
      lag_range  = 2,
      covariates = NULL,
      fe         = ~ firm_id + year,
      cluster    = ~ state_id,
      baseline   = -1,
      interval   = 1,
      time_transform = FALSE,
      unit = firm_id
    ),
    regexp = "unit.*ignored"
  )
})


test_that("run_es works with staggered treatment timing and real covariates", {
  skip_if_offline()
  skip_on_cran()

  castle <- haven::read_dta("https://github.com/scunning1975/mixtape/raw/master/castle.dta") |>
    dplyr::mutate(treatment = ifelse(!is.na(treatment_date), 1, 0)) |>
    dplyr::select(
      state, sid, year, l_homicide, popwt, treatment, treatment_date,
      dplyr::starts_with("r20")
    )

  covariate_vars <- castle |>
    dplyr::select(dplyr::starts_with("r20")) |>
    names()

  covariate_formula <- stats::as.formula(paste("~", paste(covariate_vars, collapse = " + ")))

  result <- run_es(
    data       = castle,
    outcome    = l_homicide,
    treatment  = treatment,
    time       = year,
    staggered  = TRUE,
    timing     = treatment_date,
    covariates = covariate_formula,
    fe         = ~ state + year,
    cluster    = ~ sid,
    weights    = ~ popwt,
    baseline   = 0
  )

  expect_s3_class(result, "data.frame")
  expect_true("relative_time" %in% names(result))
  expect_true("conf_low" %in% names(result))
  expect_true("conf_high" %in% names(result))
  expect_true(any(result$is_baseline))
  expect_equal(result[result$is_baseline, ]$estimate, 0)
})

panel_data_date <- panel_data |>
  dplyr::mutate(
    date = as.Date(paste0(year, "-01-01"))
  )

testthat::test_that("run_es handles date-type time and string timing correctly", {
  result <- run_es(
    data = panel_data_date,
    outcome = y,
    treatment = is_treated,
    time = date,
    staggered = FALSE,
    timing = "1998-01-01",
    lead_range = 3,
    lag_range = 3,
    fe = ~ firm_id + date
  )

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true("relative_time" %in% colnames(result))
  testthat::expect_true(any(result$relative_time == 0))
})




