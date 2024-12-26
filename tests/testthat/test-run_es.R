test_that("run_es produces correct output with interval parameter", {
  # Create test data
  df <- tibble::tibble(
    id      = rep(1:5, each = 11),
    year    = rep(2000:2010, times = 5),
    y       = rnorm(55),
    treated = dplyr::if_else(id %in% c(1, 3, 5), 1, 0)
  )

  # Run the function with default interval (1-year steps)
  result_default <- df |>
    run_es(y, treated, year, 2005, 3, 2, c("id", "year"), "id")

  # Verify output for default interval
  expect_true("relative_time" %in% colnames(result_default))
  expect_equal(min(result_default$relative_time), -3)
  expect_equal(max(result_default$relative_time), 2)

  # Run the function with interval = 5 (5-year steps)
  result_interval <- df |>
    run_es(y, treated, year, 2005, 1, 1, c("id", "year"), "id", interval = 5)

  # Verify output for 5-year interval
  expect_true("relative_time" %in% colnames(result_interval))
  expect_equal(min(result_interval$relative_time), -5)
  expect_equal(max(result_interval$relative_time), 5)

  # Suppress warnings for out-of-range lead/lag
  expect_warning(
    run_es(df, y, treated, year, 2005, 10, 10, c("id", "year"), "id"),
    regexp = "exceeds the available range"
  )
})
