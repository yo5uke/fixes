# test-plot-es.R
#
# plot.es_result(): the `time_axis` switch between event time and the
# original calendar periods, the default display and palette, and the
# guards shared by the static and interactive engines.

library(testthat)
library(fixes)

make_universal_panel <- function(seed = 11L, n_id = 40L) {
  set.seed(seed)
  df <- expand.grid(id = seq_len(n_id), year = 2005:2015)
  df$treated <- as.integer(df$id <= n_id / 2L)
  df$y <- rnorm(n_id)[df$id] + 0.1 * df$year +
    0.8 * (df$treated == 1L & df$year >= 2010) + rnorm(nrow(df), sd = 0.5)
  df
}

make_staggered_panel <- function(seed = 12L, n_id = 40L) {
  set.seed(seed)
  df <- expand.grid(id = seq_len(n_id), year = 2005:2015)
  g <- sample(c(2009, 2012, NA), n_id, replace = TRUE)
  df$gvar <- g[df$id]
  df$treated <- as.integer(!is.na(df$gvar))
  df$y <- rnorm(n_id)[df$id] + 0.1 * df$year +
    ifelse(!is.na(df$gvar) & df$year >= df$gvar, 0.7, 0) +
    rnorm(nrow(df), sd = 0.5)
  df
}

test_that("universal timing carries a calendar reference", {
  res <- event_study(make_universal_panel(), outcome = y, treatment = treated,
                     time = year, timing = 2010, fe = ~ id + year)

  expect_identical(attr(res, "ref_time"), 2010)
  expect_identical(attr(res, "time_var"), "year")
  expect_false(attr(res, "staggered"))
})

test_that("time_axis = 'calendar' relabels and rescales the x axis", {
  res <- event_study(make_universal_panel(), outcome = y, treatment = treated,
                     time = year, timing = 2010, fe = ~ id + year)

  p_rel <- plot(res)
  p_cal <- plot(res, time_axis = "calendar")

  expect_identical(p_rel$labels$x, "Relative Time to Treatment")
  expect_identical(p_cal$labels$x, "year")

  expect_identical(p_rel$data$.x, res$relative_time)
  expect_identical(p_cal$data$.x, res$relative_time + 2010)

  # The dashed reference line follows the axis in use.
  expect_identical(ggplot2::layer_data(p_rel, 1)$xintercept[1], 0)
  expect_identical(ggplot2::layer_data(p_cal, 1)$xintercept[1], 2010)
})

test_that("an explicit vline_val still wins over the axis default", {
  res <- event_study(make_universal_panel(), outcome = y, treatment = treated,
                     time = year, timing = 2010, fe = ~ id + year)

  p <- plot(res, time_axis = "calendar", vline_val = 2009.5)
  expect_identical(ggplot2::layer_data(p, 1)$xintercept[1], 2009.5)
})

test_that("calendar axis snaps Date periods onto the observed ones", {
  df <- make_universal_panel()
  df$date <- as.Date(paste0(df$year, "-01-01"))

  res <- event_study(df, outcome = y, treatment = treated, time = date,
                     timing = as.Date("2010-01-01"), interval = 365,
                     fe = ~ id + date)

  p <- plot(res, time_axis = "calendar")
  expect_s3_class(p$data$.x, "Date")
  # `interval = 365` drifts across leap years; the plotted periods must still
  # be the dates present in the data.
  expect_true(all(p$data$.x %in% df$date))
  expect_identical(p$labels$x, "date")
})

test_that("staggered adoption has no calendar axis", {
  res <- event_study(make_staggered_panel(), outcome = y, treatment = treated,
                     time = year, timing = gvar, fe = ~ id + year)

  expect_null(attr(res, "ref_time"))
  expect_error(plot(res, time_axis = "calendar"), "staggered design")
  expect_s3_class(plot(res), "ggplot")
})

test_that("a single-cohort staggered design does get a calendar axis", {
  df <- make_staggered_panel()
  df$gvar[!is.na(df$gvar)] <- 2009

  res <- event_study(df, outcome = y, treatment = treated, time = year,
                     timing = gvar, fe = ~ id + year)

  expect_identical(attr(res, "ref_time"), 2009)
  expect_identical(plot(res, time_axis = "calendar")$labels$x, "year")
})

test_that("`rel_time` results refuse the calendar axis", {
  df <- make_universal_panel()
  df$k <- ifelse(df$treated == 1L, df$year - 2010, NA_integer_)

  res <- event_study(df, outcome = y, treatment = treated, time = year,
                     rel_time = k, fe = ~ id + year)

  expect_null(attr(res, "ref_time"))
  expect_error(plot(res, time_axis = "calendar"), "no calendar reference")
})

test_that("an unavailable ci_level errors instead of silently redrawing 95%", {
  res <- event_study(make_universal_panel(), outcome = y, treatment = treated,
                     time = year, timing = 2010, fe = ~ id + year,
                     conf_level = 0.95)

  expect_error(plot(res, ci_level = 0.9), "No 90% confidence interval")
  expect_s3_class(plot(res, ci_level = 0.95), "ggplot")

  res90 <- event_study(make_universal_panel(), outcome = y,
                       treatment = treated, time = year, timing = 2010,
                       fe = ~ id + year, conf_level = c(0.90, 0.95))
  expect_identical(plot(res90, ci_level = 0.9)$labels$y,
                   "Estimate and 90% CI")
})

test_that("autoplot forwards time_axis", {
  res <- event_study(make_universal_panel(), outcome = y, treatment = treated,
                     time = year, timing = 2010, fe = ~ id + year)

  p <- ggplot2::autoplot(res, time_axis = "calendar")
  expect_identical(p$labels$x, "year")
})

test_that("the interactive plot honours time_axis", {
  skip_if_not_installed("plotly")
  res <- event_study(make_universal_panel(), outcome = y, treatment = treated,
                     time = year, timing = 2010, fe = ~ id + year)

  fig <- plot(res, interactive = TRUE, time_axis = "calendar")
  expect_s3_class(fig, "plotly")
  expect_identical(plotly::plotly_build(fig)$x$layout$xaxis$title, "year")

  expect_error(
    plot(res, interactive = TRUE, ci_level = 0.9),
    "No 90% confidence interval"
  )
})

test_that("error bars are the default display, in black", {
  res <- event_study(make_universal_panel(), outcome = y, treatment = treated,
                     time = year, timing = 2010, fe = ~ id + year)

  p <- plot(res)
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1L))
  expect_true("GeomErrorbar" %in% geoms)
  expect_false("GeomRibbon" %in% geoms)

  point <- p$layers[[which(geoms == "GeomPoint")]]
  bar <- p$layers[[which(geoms == "GeomErrorbar")]]
  expect_identical(point$aes_params$colour, "#000000")
  expect_identical(bar$aes_params$colour, "#000000")
})

test_that("errorbar_color recolours the bars alone", {
  res <- event_study(make_universal_panel(), outcome = y, treatment = treated,
                     time = year, timing = 2010, fe = ~ id + year)

  p <- plot(res, errorbar_color = "grey60")
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1L))
  expect_identical(p$layers[[which(geoms == "GeomErrorbar")]]$aes_params$colour,
                   "grey60")
  expect_identical(p$layers[[which(geoms == "GeomPoint")]]$aes_params$colour,
                   "#000000")

  # ... and follows `color` when it is not given.
  p2 <- plot(res, color = "#1B4965")
  geoms2 <- vapply(p2$layers, function(l) class(l$geom)[1], character(1L))
  expect_identical(
    p2$layers[[which(geoms2 == "GeomErrorbar")]]$aes_params$colour,
    "#1B4965"
  )
})

test_that("the ribbon display uses the package's steel-blue palette", {
  res <- event_study(make_universal_panel(), outcome = y, treatment = treated,
                     time = year, timing = 2010, fe = ~ id + year)

  p <- plot(res, type = "ribbon")
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1L))
  ribbon <- p$layers[[which(geoms == "GeomRibbon")]]

  pal <- fixes:::.fixes_palette()
  expect_identical(ribbon$aes_params$fill, pal$ribbon)
  expect_identical(ribbon$aes_params$alpha, pal$alpha)
  expect_identical(p$layers[[which(geoms == "GeomLine")]]$aes_params$colour,
                   pal$line)
  expect_identical(p$layers[[which(geoms == "GeomPoint")]]$aes_params$colour,
                   pal$line)
})
