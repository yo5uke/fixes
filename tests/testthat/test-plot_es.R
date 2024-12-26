test_that("plot_es creates a ggplot object", {
  df <- data.frame(
    relative_time = -5:5,
    estimate = rnorm(11),
    std.error = runif(11, 0.1, 0.3),
    conf_low = rnorm(11, mean = -0.2),
    conf_high = rnorm(11, mean = 0.2),
    term = factor(paste0("term", -5:5))
  )

  # geom_ribbon()
  p1 <- plot_es(
    data = df,
    type = "ribbon"
  )
  expect_s3_class(p1, "ggplot")

  # geom_errorbar()
  p2 <- plot_es(
    data = df,
    type = "errorbar"
  )
  expect_s3_class(p2, "ggplot")

  # Testing for invalid type arguments
  expect_error(
    plot_es(data = df, type = "invalid_type"),
    "Invalid type. Please choose 'ribbon' or 'errorbar'."
  )
})

test_that("plot_es handles missing or NA values in data gracefully", {
  # Test for cases where the data includes NA
  df <- data.frame(
    relative_time = -5:5,
    estimate = rnorm(11),
    conf_low = c(rnorm(5), NA, rnorm(5)),
    conf_high = c(rnorm(5), NA, rnorm(5)),
    term = factor(paste0("term", -5:5))
  )

  # Generate a plot with data that contains NA
  p <- plot_es(data = df, type = "ribbon")
  expect_s3_class(p, "ggplot")
})
