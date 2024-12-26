test_that("run_es produces correct output", {
  df <- tibble::tibble(
    id      = rep(1:5, each = 10),
    year    = rep(2000:2009, times = 5),
    y       = rnorm(50),
    treated = rep(c(0, 1), each = 25)
  )
  result <- df |>
    run_es(y, treated, year, 2005, 3, 2, c("id", "year"), "id")
  expect_true("relative_time" %in% colnames(result))
})
