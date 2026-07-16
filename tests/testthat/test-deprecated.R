# test-deprecated.R
#
# The verb-style API (run_es, calc_att, run_did, plot_*, compute_*) is
# deprecated as of 1.0.0 but must (a) emit a lifecycle deprecation warning
# and (b) return output identical to the noun-style successors.

library(testthat)
library(fixes)

make_dep_panel <- function(seed = 31L) {
  set.seed(seed)
  n_id <- 40L
  df <- expand.grid(id = seq_len(n_id), year = 2001:2008)
  g <- sample(c(2004, 2006, NA), n_id, replace = TRUE, prob = c(0.4, 0.3, 0.3))
  df$gvar    <- g[df$id]
  df$treated <- as.integer(!is.na(df$gvar))
  te <- ifelse(!is.na(df$gvar) & df$year >= df$gvar, 0.7, 0)
  df$y <- rnorm(n_id)[df$id] + 0.1 * df$year + te + rnorm(nrow(df), sd = 0.5)
  df
}

strip_call <- function(x) {
  attr(x, "call") <- NULL
  x
}

# honest_sensitivity() needs its numeric Suggests (mirrors test-honest.R).
honest_solvers_ready <- function() {
  all(vapply(c("lpSolveAPI", "Rglpk", "TruncatedNormal", "Matrix", "pracma"),
             requireNamespace, logical(1L), quietly = TRUE))
}

test_that("run_es warns once and matches event_study (twfe classic)", {
  df <- make_dep_panel()

  lifecycle::expect_deprecated(
    old <- run_es(df, outcome = y, treatment = treated, time = year,
                  timing = gvar, fe = ~ id + year, cluster = ~id,
                  staggered = TRUE, conf.level = c(0.90, 0.95))
  )
  new <- event_study(df, outcome = y, treatment = treated, time = year,
                     timing = gvar, fe = ~ id + year, cluster = ~id,
                     staggered = TRUE, conf_level = c(0.90, 0.95))
  expect_identical(strip_call(old), strip_call(new))
  expect_true(is.call(attr(old, "call")))
  expect_identical(as.list(attr(old, "call"))[[1]], quote(run_es))
})

test_that("run_es matches event_study for cs (incl. bootstrap args B/alpha)", {
  df <- make_dep_panel(32L)

  old <- suppressWarnings(
    run_es(df, outcome = y, time = year, timing = gvar, unit = id,
           estimator = "cs", bootstrap = TRUE, B = 49L, alpha = 0.10,
           boot_seed = 7L)
  )
  new <- event_study(df, outcome = y, time = year, timing = gvar, unit = id,
                     estimator = "cs", bootstrap = TRUE, boot_reps = 49L,
                     boot_alpha = 0.10, boot_seed = 7L)
  expect_identical(strip_call(old), strip_call(new))
})

test_that("event_study infers staggered from a timing column", {
  df <- make_dep_panel(33L)

  inferred <- event_study(df, outcome = y, treatment = treated, time = year,
                          timing = gvar, fe = ~ id + year)
  explicit <- event_study(df, outcome = y, treatment = treated, time = year,
                          timing = gvar, fe = ~ id + year, staggered = TRUE)
  expect_identical(strip_call(inferred), strip_call(explicit))
  expect_true(attr(inferred, "staggered"))

  # scalar timing -> universal design
  uni <- event_study(df, outcome = y, treatment = treated, time = year,
                     timing = 2004, fe = ~ id + year)
  expect_false(attr(uni, "staggered"))
})

test_that("run_es(method = 'sunab') routes to the legacy fixest path", {
  skip_if_not_installed("fixest")
  df <- make_dep_panel(34L)

  old <- suppressWarnings(
    run_es(df, outcome = y, treatment = treated, time = year, timing = gvar,
           fe = ~ id + year, staggered = TRUE, method = "sunab", unit = id)
  )
  expect_s3_class(old, "es_result")
  expect_true(attr(old, "sunab_used"))
})

test_that("calc_att warns and matches att (dead args dropped silently)", {
  df <- make_dep_panel(35L)

  lifecycle::expect_deprecated(
    old <- calc_att(df, outcome = y, time = year, timing = gvar, unit = id,
                    aggregation = "by_cohort", conf.level = c(0.90, 0.95),
                    fe = ~ id + year, vcov = "HC1")
  )
  new <- att(df, outcome = y, time = year, timing = gvar, unit = id,
             aggregation = "by_cohort", conf_level = c(0.90, 0.95))
  expect_identical(old, new)
})

test_that("run_did warns and matches did", {
  df <- make_dep_panel(36L)
  df$D <- as.integer(df$treated == 1L & !is.na(df$gvar) & df$year >= df$gvar)

  lifecycle::expect_deprecated(
    old <- run_did(df, outcome = y, treatment = D, fe = ~ id + year,
                   cluster = ~id, conf.level = 0.9)
  )
  new <- did(df, outcome = y, treatment = D, fe = ~ id + year,
             cluster = ~id, conf_level = 0.9)
  expect_identical(strip_call(old), strip_call(new))
})

test_that("plot_es / plot_es_interactive / plot() agree", {
  df <- make_dep_panel(37L)
  res <- event_study(df, outcome = y, treatment = treated, time = year,
                     timing = gvar, fe = ~ id + year, staggered = TRUE)

  lifecycle::expect_deprecated(p_old <- plot_es(res))
  p_new <- plot(res)
  expect_s3_class(p_old, "ggplot")
  expect_s3_class(p_new, "ggplot")
  expect_identical(p_old$data, p_new$data)

  p_eb <- plot(res, type = "errorbar", ci_level = 0.95)
  expect_s3_class(p_eb, "ggplot")

  skip_if_not_installed("plotly")
  lifecycle::expect_deprecated(pi_old <- plot_es_interactive(res))
  pi_new <- plot(res, interactive = TRUE)
  expect_s3_class(pi_old, "plotly")
  expect_s3_class(pi_new, "plotly")
})

test_that("att_gt extractor + plot.att_gt_result replace plot_att_gt", {
  df <- make_dep_panel(38L)
  res <- event_study(df, outcome = y, time = year, timing = gvar, unit = id,
                     estimator = "cs")

  gt <- att_gt(res)
  expect_s3_class(gt, "att_gt_result")
  expect_true(all(c("g", "t", "estimate", "std_error") %in% names(gt)))

  p_new <- plot(gt)
  lifecycle::expect_deprecated(p_old <- plot_att_gt(res))
  expect_s3_class(p_new, "ggplot")
  expect_s3_class(p_old, "ggplot")

  expect_error(att_gt(data.frame(a = 1)), "No ATT\\(g,t\\) table")
})

test_that("contamination_weights renames compute_contamination_weights", {
  df <- make_dep_panel(39L)
  df2 <- df[!is.na(df$gvar) | df$id %% 2L == 0L, ]

  new <- contamination_weights(df, time = year, timing = gvar, unit = id,
                               fe = ~ id + year)
  lifecycle::expect_deprecated(
    old <- compute_contamination_weights(df, time = year, timing = gvar,
                                         unit = id, fe = ~ id + year)
  )
  expect_identical(old, new)

  p_new <- plot(new)
  lifecycle::expect_deprecated(p_old <- plot_contamination_weights(new))
  expect_s3_class(p_new, "ggplot")
  expect_s3_class(p_old, "ggplot")
})

test_that("honest_sensitivity accepts old argument names with a warning", {
  skip_if_not(honest_solvers_ready())
  df <- make_dep_panel(40L)
  res <- event_study(df, outcome = y, treatment = treated, time = year,
                     timing = gvar, fe = ~ id + year, staggered = TRUE)

  new <- honest_sensitivity(res, m_grid = c(0, 1), grid_points = 200L)
  lifecycle::expect_deprecated(
    old <- honest_sensitivity(res, Mvec = c(0, 1), gridPoints = 200L)
  )
  expect_identical(as.data.frame(old), as.data.frame(new))
})

test_that("plot.honest_result and deprecated plot_honest agree", {
  skip_if_not(honest_solvers_ready())
  df <- make_dep_panel(41L)
  res <- event_study(df, outcome = y, treatment = treated, time = year,
                     timing = gvar, fe = ~ id + year, staggered = TRUE)
  hs <- honest_sensitivity(res, m_grid = c(0, 1), grid_points = 200L)

  p_new <- plot(hs)
  lifecycle::expect_deprecated(p_old <- plot_honest(hs))
  expect_s3_class(p_new, "ggplot")
  expect_s3_class(p_old, "ggplot")
})

test_that("new tidy/glance/plot methods work on new-API results", {
  df <- make_dep_panel(42L)
  res <- event_study(df, outcome = y, time = year, timing = gvar, unit = id,
                     estimator = "cs")

  td <- broom::tidy(res)
  expect_true(all(c("term", "estimate", "std.error", "relative_time")
                  %in% names(td)))
  g <- broom::glance(res)
  expect_equal(g$estimator, "cs")
  expect_equal(g$nobs, attr(res, "N"))

  a <- att(df, outcome = y, time = year, timing = gvar, unit = id,
           aggregation = "by_cohort")
  expect_s3_class(plot(a), "ggplot")
  expect_s3_class(ggplot2::autoplot(a), "ggplot")
})
