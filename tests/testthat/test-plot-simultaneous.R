# test-plot-simultaneous.R
#
# Tests for simultaneous CI overlays in plot_es(), plot_es_interactive(),
# and plot_att_gt().

library(testthat)
library(fixes)

# ---------------------------------------------------------------------------
# Shared DGP — small panel (50 units, 11 periods, 3 cohorts + 5 never-treated)
# ---------------------------------------------------------------------------
make_sim_data <- function(seed = 42L) {
  set.seed(seed)
  n_units <- 50L
  periods <- 1995:2005

  g_vec <- c(
    rep(1998L, 15L),
    rep(2000L, 15L),
    rep(2002L, 15L),
    rep(NA_integer_, 5L)
  )

  panel <- expand.grid(id = seq_len(n_units), year = periods,
                       stringsAsFactors = FALSE)
  panel <- panel[order(panel$id, panel$year), ]
  rownames(panel) <- NULL

  panel$g     <- g_vec[panel$id]
  panel$treat <- as.integer(!is.na(panel$g) & panel$year >= panel$g)

  unit_fe <- rnorm(n_units)[panel$id]
  time_fe <- (panel$year - 1995L) * 0.1
  eps     <- rnorm(nrow(panel), sd = 0.3)
  panel$y <- unit_fe + time_fe + 1.5 * panel$treat + eps
  panel
}

# Run CS with bootstrap (B=199 for speed; seed for reproducibility)
sim_data   <- make_sim_data(seed = 42L)
boot_result <- suppressWarnings(run_es(
  data      = sim_data,
  outcome   = y,
  time      = year,
  timing    = g,
  unit      = id,
  staggered = TRUE,
  estimator = "cs",
  bootstrap = TRUE,
  B         = 199L,
  boot_seed = 1L
))

# Run CS without bootstrap (for the error-path test)
no_boot_result <- suppressWarnings(run_es(
  data      = sim_data,
  outcome   = y,
  time      = year,
  timing    = g,
  unit      = id,
  staggered = TRUE,
  estimator = "cs",
  bootstrap = FALSE
))

# ---------------------------------------------------------------------------
# Test 1 — plot_es with show_simultaneous = TRUE returns ggplot
# ---------------------------------------------------------------------------
test_that("plot_es with show_simultaneous = TRUE returns ggplot", {
  skip_if_not_installed("ggplot2")

  p <- plot_es(boot_result, show_simultaneous = TRUE)
  expect_s3_class(p, "ggplot")

  # Builds without error (catches data-lookup failures in aes)
  expect_no_error(ggplot2::ggplot_build(p))
})

# ---------------------------------------------------------------------------
# Test 2 — plot_es errors when simultaneous CI absent
# ---------------------------------------------------------------------------
test_that("plot_es errors when simultaneous CI absent", {
  skip_if_not_installed("ggplot2")

  expect_error(
    plot_es(no_boot_result, show_simultaneous = TRUE),
    regexp = "Simultaneous CIs not found",
    fixed  = TRUE
  )
})

# ---------------------------------------------------------------------------
# Test 3 — plot_es_interactive with show_simultaneous = TRUE returns plotly
# ---------------------------------------------------------------------------
test_that("plot_es_interactive with show_simultaneous = TRUE returns plotly", {
  skip_if_not_installed("plotly")

  p <- plot_es_interactive(boot_result, show_simultaneous = TRUE)
  expect_s3_class(p, "plotly")
})

# ---------------------------------------------------------------------------
# Test 4 — plot_att_gt heatmap shows simultaneous annotation when bootstrap present
# ---------------------------------------------------------------------------
test_that("plot_att_gt heatmap shows simultaneous annotation when bootstrap present", {
  skip_if_not_installed("ggplot2")

  p <- plot_att_gt(boot_result, type = "heatmap")
  expect_s3_class(p, "ggplot")

  # Subtitle should contain the simultaneous critical value phrase
  built <- ggplot2::ggplot_build(p)
  subtitle_text <- p$labels$subtitle
  expect_false(is.null(subtitle_text),
               label = "subtitle is present when bootstrap attribute exists")
  expect_true(grepl("simultaneous critical value", subtitle_text, fixed = TRUE),
              label = "subtitle contains 'simultaneous critical value'")
})

# ---------------------------------------------------------------------------
# Test 5 — plot_att_gt facet shows simultaneous ribbon when bootstrap present
# ---------------------------------------------------------------------------
test_that("plot_att_gt facet shows simultaneous ribbon when bootstrap present", {
  skip_if_not_installed("ggplot2")

  p <- plot_att_gt(boot_result, type = "facet")
  expect_s3_class(p, "ggplot")

  # Builds without error
  expect_no_error(ggplot2::ggplot_build(p))

  # A fill scale should be present (from the simultaneous CI ribbon legend)
  built    <- ggplot2::ggplot_build(p)
  has_fill <- any(vapply(built$plot$scales$scales, function(s) {
    identical(s$aesthetics, "fill") || "fill" %in% s$aesthetics
  }, logical(1L)))
  expect_true(has_fill,
              label = "facet plot has a fill scale for the simultaneous CI legend")
})
