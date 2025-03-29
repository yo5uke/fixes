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


