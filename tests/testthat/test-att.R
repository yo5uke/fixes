# test-att.R
#
# Test-first (TDD) specification for calc_att() — ATT aggregation.
#
# Theory anchors
# --------------
# CS (2021) Section 4:
#   "simple"    — theta = sum_g (n_g/n_treated) * mean_{t>=g} ATT(g,t)
#   "by_cohort" — theta(g) = mean_{t>=g} ATT(g,t) per cohort
#   "by_time"   — theta(t) = sum_{g<=t} w(g) * ATT(g,t), w(g) = n_g/sum_{g'<=t} n_{g'}
#
# BJS (2024):
#   "simple"    — mean(tau_hat) over all treated (i,t)
#   "by_cohort" — per-cohort mean(tau_hat)
#   "by_time"   — per-calendar-time mean(tau_hat)
#
# Reference comparisons: did::aggte() for CS; manual tau_it means for BJS.

library(testthat)
library(fixes)

# ---------------------------------------------------------------------------
# Shared simulation fixture (same design as test-cs.R / test-bjs.R)
# ---------------------------------------------------------------------------
# 50 units | periods 1995-2005 | staggered adoption in 1998, 2000, 2002
# Never-treated: 5 units (g = NA).  True ATT = 1.5 (constant/homogeneous).

make_att_data <- function(seed = 42L) {
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

att_data <- make_att_data()

# ---------------------------------------------------------------------------
# Test 1 — return type and column contract
# ---------------------------------------------------------------------------

test_that("calc_att returns att_result S3 class", {
  result <- calc_att(
    data = att_data, outcome = y, time = year, timing = g, unit = id,
    estimator = "cs", aggregation = "simple", control_group = "nevertreated"
  )
  expect_s3_class(result, "att_result")
  expect_true(is.data.frame(result))
})

test_that("calc_att simple: 1 row, correct columns, group is NA", {
  result <- calc_att(
    data = att_data, outcome = y, time = year, timing = g, unit = id,
    estimator = "cs", aggregation = "simple"
  )
  required_cols <- c("group", "estimate", "std.error", "statistic", "p.value",
                     "conf_low_95", "conf_high_95")
  expect_true(all(required_cols %in% names(result)))
  expect_equal(nrow(result), 1L)
  expect_true(is.na(result$group))
  expect_true(is.finite(result$estimate))
  expect_true(result$std.error > 0)
})

test_that("calc_att by_cohort: 3 rows (one per cohort), correct groups", {
  result <- calc_att(
    data = att_data, outcome = y, time = year, timing = g, unit = id,
    estimator = "cs", aggregation = "by_cohort"
  )
  expect_equal(nrow(result), 3L)
  expect_equal(sort(result$group), c(1998, 2000, 2002))
  expect_true(all(is.finite(result$estimate)))
  expect_true(all(result$std.error > 0))
})

test_that("calc_att by_time: multiple rows, all estimates finite", {
  result <- calc_att(
    data = att_data, outcome = y, time = year, timing = g, unit = id,
    estimator = "cs", aggregation = "by_time"
  )
  expect_true(nrow(result) > 0)
  expect_true(all(is.finite(result$estimate)))
  expect_true(all(result$std.error >= 0))
  # Calendar times should be subset of sample periods with treated units
  expect_true(all(result$group >= 1998))
})

# ---------------------------------------------------------------------------
# Test 2 — CS numerical agreement with did::aggte
# ---------------------------------------------------------------------------
# Compares calc_att(estimator="cs") against the reference did package.
# Tolerance 1e-4: our ATT(g,t) estimates match did::att_gt at 1e-6, and
# aggregation is linear, so 1e-4 has sufficient margin.

test_that("calc_att CS simple matches did::aggte(type='simple')", {
  skip_if_not_installed("did")

  did_data      <- att_data
  did_data$g    <- as.numeric(did_data$g)
  did_data$g[is.na(did_data$g)] <- 0

  ref_gt <- did::att_gt(
    yname = "y", tname = "year", idname = "id", gname = "g",
    data = did_data, control_group = "nevertreated",
    anticipation = 0L, bstrap = FALSE, cband = FALSE
  )
  ref_simple <- did::aggte(ref_gt, type = "simple", na.rm = TRUE)

  result <- calc_att(
    data = att_data, outcome = y, time = year, timing = g, unit = id,
    estimator = "cs", aggregation = "simple", control_group = "nevertreated"
  )

  # Point estimate matches did::aggte at 1e-4 (ATT(g,t) estimates agree at 1e-6;
  # aggregation uses the same exposure-weighted formula).
  # SE is NOT compared: our formula assumes independence across (g,t) pairs,
  # while did uses influence-function SEs that account for within-cohort
  # correlation. The two methods yield systematically different SEs.
  expect_equal(result$estimate, ref_simple$overall.att, tolerance = 1e-4)
  expect_true(result$std.error > 0)
})

test_that("calc_att CS by_cohort matches did::aggte(type='group')", {
  skip_if_not_installed("did")

  did_data      <- att_data
  did_data$g    <- as.numeric(did_data$g)
  did_data$g[is.na(did_data$g)] <- 0

  ref_gt <- did::att_gt(
    yname = "y", tname = "year", idname = "id", gname = "g",
    data = did_data, control_group = "nevertreated",
    anticipation = 0L, bstrap = FALSE, cband = FALSE
  )
  ref_group <- did::aggte(ref_gt, type = "group", na.rm = TRUE)

  result <- calc_att(
    data = att_data, outcome = y, time = year, timing = g, unit = id,
    estimator = "cs", aggregation = "by_cohort", control_group = "nevertreated"
  )

  common_g <- intersect(result$group, ref_group$egt)
  res_sub <- result[result$group %in% common_g, ]
  res_sub <- res_sub[order(res_sub$group), ]
  ref_sub_att <- ref_group$att.egt[ref_group$egt %in% common_g]
  ref_sub_att <- ref_sub_att[order(ref_group$egt[ref_group$egt %in% common_g])]
  ref_sub_se  <- ref_group$se.egt[ref_group$egt %in% common_g]
  ref_sub_se  <- ref_sub_se[order(ref_group$egt[ref_group$egt %in% common_g])]

  expect_equal(res_sub$estimate, ref_sub_att, tolerance = 1e-4)
  # SE not compared: did uses influence-function variance (accounts for
  # within-cohort correlation); we assume independence across (g,t) pairs.
  expect_true(all(res_sub$std.error > 0))
})

test_that("calc_att CS by_time matches did::aggte(type='calendar')", {
  skip_if_not_installed("did")

  did_data      <- att_data
  did_data$g    <- as.numeric(did_data$g)
  did_data$g[is.na(did_data$g)] <- 0

  ref_gt <- did::att_gt(
    yname = "y", tname = "year", idname = "id", gname = "g",
    data = did_data, control_group = "nevertreated",
    anticipation = 0L, bstrap = FALSE, cband = FALSE
  )
  ref_cal <- did::aggte(ref_gt, type = "calendar", na.rm = TRUE)

  result <- calc_att(
    data = att_data, outcome = y, time = year, timing = g, unit = id,
    estimator = "cs", aggregation = "by_time", control_group = "nevertreated"
  )

  common_t <- intersect(result$group, ref_cal$egt)
  res_sub <- result[result$group %in% common_t, ]
  res_sub <- res_sub[order(res_sub$group), ]
  ref_sub_att <- ref_cal$att.egt[ref_cal$egt %in% common_t]
  ref_sub_att <- ref_sub_att[order(ref_cal$egt[ref_cal$egt %in% common_t])]
  ref_sub_se  <- ref_cal$se.egt[ref_cal$egt %in% common_t]
  ref_sub_se  <- ref_sub_se[order(ref_cal$egt[ref_cal$egt %in% common_t])]

  expect_equal(res_sub$estimate, ref_sub_att, tolerance = 1e-4)
  # SE not compared: did uses influence-function variance; we use independence.
  expect_true(all(res_sub$std.error >= 0))
})

# ---------------------------------------------------------------------------
# Test 3 — BJS numerical agreement with tau_it manual computation
# ---------------------------------------------------------------------------

test_that("calc_att BJS simple matches mean(tau_it$tau_hat)", {
  es_result <- run_es(
    data = att_data, outcome = y, time = year, timing = g, unit = id,
    estimator = "bjs"
  )
  tau_it <- attr(es_result, "tau_it")
  manual_mean <- mean(tau_it$tau_hat)

  result <- calc_att(
    data = att_data, outcome = y, time = year, timing = g, unit = id,
    estimator = "bjs", aggregation = "simple"
  )

  expect_equal(result$estimate, manual_mean, tolerance = 1e-12)
})

test_that("calc_att BJS by_cohort matches per-cohort mean(tau_hat)", {
  es_result <- run_es(
    data = att_data, outcome = y, time = year, timing = g, unit = id,
    estimator = "bjs"
  )
  tau_it <- attr(es_result, "tau_it")

  result <- calc_att(
    data = att_data, outcome = y, time = year, timing = g, unit = id,
    estimator = "bjs", aggregation = "by_cohort"
  )

  for (g_val in result$group) {
    manual_g <- mean(tau_it$tau_hat[tau_it$cohort == g_val])
    res_g    <- result$estimate[result$group == g_val]
    expect_equal(res_g, manual_g, tolerance = 1e-12)
  }
})

test_that("calc_att BJS by_time matches per-calendar-time mean(tau_hat)", {
  es_result <- run_es(
    data = att_data, outcome = y, time = year, timing = g, unit = id,
    estimator = "bjs"
  )
  tau_it <- attr(es_result, "tau_it")

  result <- calc_att(
    data = att_data, outcome = y, time = year, timing = g, unit = id,
    estimator = "bjs", aggregation = "by_time"
  )

  for (t_val in result$group) {
    manual_t <- mean(tau_it$tau_hat[tau_it$time == t_val])
    res_t    <- result$estimate[result$group == t_val]
    expect_equal(res_t, manual_t, tolerance = 1e-12)
  }
})

# ---------------------------------------------------------------------------
# Test 4 — attributes are stamped correctly
# ---------------------------------------------------------------------------

test_that("calc_att attributes are set correctly for CS", {
  result <- calc_att(
    data = att_data, outcome = y, time = year, timing = g, unit = id,
    estimator = "cs", aggregation = "by_cohort", control_group = "nevertreated"
  )
  expect_equal(attr(result, "estimator"),    "cs")
  expect_equal(attr(result, "aggregation"),  "by_cohort")
  expect_equal(attr(result, "control_group"), "nevertreated")
  expect_equal(attr(result, "conf.level"),   0.95)
  expect_equal(attr(result, "N_units"),      dplyr::n_distinct(att_data$id))
  expect_true(!is.null(attr(result, "att_gt")))
})

test_that("calc_att attributes are set correctly for BJS", {
  result <- calc_att(
    data = att_data, outcome = y, time = year, timing = g, unit = id,
    estimator = "bjs", aggregation = "simple"
  )
  expect_equal(attr(result, "estimator"),   "bjs")
  expect_equal(attr(result, "aggregation"), "simple")
  expect_equal(attr(result, "conf.level"),  0.95)
  expect_true(!is.null(attr(result, "tau_it")))
})

# ---------------------------------------------------------------------------
# Test 5 — error handling
# ---------------------------------------------------------------------------

test_that("calc_att rejects unsupported estimators", {
  expect_error(
    calc_att(data = att_data, outcome = y, time = year, timing = g, unit = id,
             estimator = "sa"),
    class = "error"
  )
})

test_that("calc_att requires unit argument", {
  expect_error(
    calc_att(data = att_data, outcome = y, time = year, timing = g,
             estimator = "cs"),
    regexp = "unit"
  )
})

# ---------------------------------------------------------------------------
# Test 6 — multiple confidence levels
# ---------------------------------------------------------------------------

test_that("calc_att supports multiple conf.level values", {
  result <- calc_att(
    data = att_data, outcome = y, time = year, timing = g, unit = id,
    estimator = "cs", aggregation = "simple",
    conf.level = c(0.90, 0.95)
  )
  expected_ci_cols <- c("conf_low_90", "conf_high_90", "conf_low_95", "conf_high_95")
  expect_true(all(expected_ci_cols %in% names(result)))
  # 90% CI must be strictly narrower than 95%
  expect_true(result$conf_low_90 > result$conf_low_95)
  expect_true(result$conf_high_90 < result$conf_high_95)
})
