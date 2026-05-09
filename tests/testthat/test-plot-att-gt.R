# test-plot-att-gt.R
# Tests for plot_att_gt() and autoplot.att_gt_result().

library(testthat)
library(fixes)

# ---------------------------------------------------------------------------
# Shared fixture — same DGP as test-cs.R
# ---------------------------------------------------------------------------
make_plot_data <- function(seed = 42L) {
  set.seed(seed)
  n_units <- 50L; periods <- 1995:2005
  g_vec   <- c(rep(1998L,15L), rep(2000L,15L), rep(2002L,15L),
               rep(NA_integer_,5L))
  panel   <- expand.grid(id=seq_len(n_units), year=periods,
                         stringsAsFactors=FALSE)
  panel   <- panel[order(panel$id, panel$year),]; rownames(panel) <- NULL
  panel$g     <- g_vec[panel$id]
  panel$treat <- as.integer(!is.na(panel$g) & panel$year >= panel$g)
  panel$y     <- rnorm(n_units)[panel$id] +
                 (panel$year-1995L)*0.1 + 1.5*panel$treat +
                 rnorm(nrow(panel), sd=0.3)
  panel
}

cs_result <- suppressWarnings(
  run_es(data=make_plot_data(), outcome=y, time=year, timing=g,
         unit=id, fe=~id+year, staggered=TRUE, estimator="cs")
)

# ---------------------------------------------------------------------------
# Test 1 — heatmap returns a ggplot
# ---------------------------------------------------------------------------
test_that("plot_att_gt returns a ggplot object", {
  skip_if_not_installed("ggplot2")

  p_heat <- plot_att_gt(cs_result, type = "heatmap")
  expect_s3_class(p_heat, "ggplot")

  p_facet <- plot_att_gt(cs_result, type = "facet")
  expect_s3_class(p_facet, "ggplot")
})

# ---------------------------------------------------------------------------
# Test 2 — both types render without error
# ---------------------------------------------------------------------------
test_that("plot_att_gt renders both types without error", {
  skip_if_not_installed("ggplot2")

  expect_no_error(ggplot2::ggplot_build(
    plot_att_gt(cs_result, type = "heatmap")
  ))
  expect_no_error(ggplot2::ggplot_build(
    plot_att_gt(cs_result, type = "facet")
  ))
})

# ---------------------------------------------------------------------------
# Test 3 — errors informatively on non-CS es_result
# ---------------------------------------------------------------------------
test_that("plot_att_gt errors on non-cs es_result", {
  # Construct a minimal es_result that has no att_gt attribute,
  # as would be produced by estimator = "twfe" or method = "sunab".
  fake_twfe <- structure(
    data.frame(
      term          = as.character(-1L:3L),
      estimate      = c(0, 0.1, 0.2, 1.5, 1.6),
      std.error     = c(0, 0.05, 0.05, 0.05, 0.05),
      statistic     = c(NA_real_, 2, 4, 30, 32),
      p.value       = c(NA_real_, 0.04, 1e-3, 1e-10, 1e-11),
      relative_time = -1L:3L,
      is_baseline   = c(TRUE, FALSE, FALSE, FALSE, FALSE),
      conf_low_95   = c(0, 0, 0.1, 1.4, 1.5),
      conf_high_95  = c(0, 0.2, 0.3, 1.6, 1.7),
      stringsAsFactors = FALSE
    ),
    class      = c("es_result", "data.frame"),
    lead_range = 1L,
    lag_range  = 3L,
    baseline   = -1L
    # note: no att_gt attribute — this is the condition we are testing
  )

  expect_error(
    plot_att_gt(fake_twfe),
    regexp      = "att_gt|estimator.*cs|cs.*estimator",
    ignore.case = TRUE
  )
})

# ---------------------------------------------------------------------------
# Test 4 — errors informatively on non-es_result input
# ---------------------------------------------------------------------------
test_that("plot_att_gt errors on plain data.frame without required columns", {
  bad <- data.frame(x = 1:3, y = 4:6)
  expect_error(plot_att_gt(bad), regexp = "missing columns", ignore.case = TRUE)
})

# ---------------------------------------------------------------------------
# Test 5 — att_gt_result S3 method via autoplot
# ---------------------------------------------------------------------------
test_that("autoplot.att_gt_result dispatches to plot_att_gt", {
  skip_if_not_installed("ggplot2")

  att <- attr(cs_result, "att_gt")
  class(att) <- c("att_gt_result", "data.frame")

  p <- ggplot2::autoplot(att)
  expect_s3_class(p, "ggplot")
})
