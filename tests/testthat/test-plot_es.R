test_that("plot_es creates a ggplot object", {
  df <- data.frame(
    relative_time = -5:5,
    estimate = rnorm(11),
    std.error = runif(11, 0.1, 0.3),
    conf_low = rnorm(11, mean = -0.2),
    conf_high = rnorm(11, mean = 0.2),
    term = factor(paste0("term", -5:5))
  )

  # Test for ribbon-style confidence intervals
  p1 <- plot_es(
    data = df,
    type = "ribbon"
  )
  expect_s3_class(p1, "ggplot")
  expect_true("GeomRibbon" %in% sapply(p1$layers, function(x) class(x$geom)[1]))

  # Test for error bar confidence intervals
  p2 <- plot_es(
    data = df,
    type = "errorbar"
  )
  expect_s3_class(p2, "ggplot")
  expect_true("GeomErrorbar" %in% sapply(p2$layers, function(x) class(x$geom)[1]))

  # Test for invalid type arguments
  expect_error(
    plot_es(data = df, type = "invalid_type"),
    "Invalid type. Please choose 'ribbon' or 'errorbar'."
  )
})

test_that("plot_es handles missing or NA values in data gracefully", {
  # Test for cases where the data includes NA values
  df <- data.frame(
    relative_time = -5:5,
    estimate = rnorm(11),
    conf_low = c(rnorm(5), NA, rnorm(5)),
    conf_high = c(rnorm(5), NA, rnorm(5)),
    term = factor(paste0("term", -5:5))
  )

  # Generate a plot with ribbon-style confidence intervals
  p <- plot_es(data = df, type = "ribbon")
  expect_s3_class(p, "ggplot")

  # Test for error bar confidence intervals when std.error contains NA
  df$std.error <- c(runif(5, 0.1, 0.3), NA, runif(5, 0.1, 0.3))
  p2 <- plot_es(data = df, type = "errorbar")
  expect_s3_class(p2, "ggplot")
})

test_that("plot_es handles custom aesthetic parameters", {
  df <- data.frame(
    relative_time = -5:5,
    estimate = rnorm(11),
    std.error = runif(11, 0.1, 0.3),
    conf_low = rnorm(11, mean = -0.2),
    conf_high = rnorm(11, mean = 0.2),
    term = factor(paste0("term", -5:5))
  )

  # Custom colors and sizes for ribbon plot
  p1 <- plot_es(
    data = df,
    type = "ribbon",
    vline_color = "blue",
    hline_color = "red",
    color = "green",
    fill = "lightgreen",
    linewidth = 2,
    pointsize = 3
  )
  expect_s3_class(p1, "ggplot")

  # Custom colors and sizes for error bar plot
  p2 <- plot_es(
    data = df,
    type = "errorbar",
    vline_color = "darkblue",
    hline_color = "darkred",
    color = "darkgreen",
    linewidth = 1.5,
    barwidth = 0.5
  )
  expect_s3_class(p2, "ggplot")
})
