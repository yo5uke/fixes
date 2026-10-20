# test-timing-constant.R
#
# Each unit must carry a single timing value (NA = never treated) on every
# row. Otherwise cohort membership depends on which row is read first, and
# results change with the row order of the input.

library(testthat)
library(fixes)

make_timing_data <- function() {
  set.seed(1)
  d <- expand.grid(id = 1:30, year = 1:8)
  g <- rep(c(4, 6, NA), each = 10)
  d$g <- g[d$id]
  d$y <- 0.1 * d$id + 0.2 * d$year +
    ifelse(!is.na(d$g) & d$year >= d$g, 1, 0) + rnorm(nrow(d))
  d
}

two_values <- function() {
  d <- make_timing_data()
  d$g[d$id == 1L & d$year == 1L] <- 6   # unit 1: timing 4 and 6
  d
}

na_and_value <- function() {
  d <- make_timing_data()
  d$g[d$id == 2L & d$year == 1L] <- NA  # unit 2: timing NA and 4
  d
}

for (est in c("cs", "sa", "bjs", "twm")) {
  test_that(paste0(est, ": a unit with several timing values is an error"), {
    expect_error(
      event_study(two_values(), outcome = y, time = year, timing = g,
                  unit = id, estimator = est),
      "more than one value of 'g': 1\\. .*single timing value"
    )
    expect_error(
      event_study(na_and_value(), outcome = y, time = year, timing = g,
                  unit = id, estimator = est),
      "more than one value of 'g': 2\\. .*single timing value"
    )
  })
}

test_that("att() rejects a unit with several timing values", {
  for (est in c("cs", "bjs")) {
    expect_error(
      att(two_values(), outcome = y, time = year, timing = g, unit = id,
          estimator = est),
      "single timing value"
    )
  }
})

test_that("contamination_weights() rejects a unit with several timing values", {
  expect_error(
    suppressWarnings(contamination_weights(two_values(), time = year,
                                           timing = g, unit = id)),
    "single timing value"
  )
})

test_that("unit-constant timing (with NA for never treated) passes", {
  expect_silent(
    fixes:::.assert_unit_constant_timing(make_timing_data(), "g", "id")
  )
})
