# test-twm.R
#
# Tests for the Wooldridge (2025) Two-Way Mundlak (TWM) estimator.
#
# Theory anchors (Wooldridge 2025):
#   Procedure 5.1: POLS on cohort x calendar-time treatment cells + unit/time FE
#   Key equivalence: TWM (trends=FALSE) ≡ SA (2021) algebraically
#     (both are saturated regressions with cohort x relative-time interactions
#      and unit+time FE; TWM parameterises in calendar time, SA in relative time)
#
# Primary correctness test: numerical agreement with run_es(estimator="sa")
#   at tolerance 1e-10 (algebraic identity, not approximation).

library(testthat)
library(fixes)

# ---------------------------------------------------------------------------
# Shared simulation fixture — same as test-sa.R
# ---------------------------------------------------------------------------
make_twm_data <- function(seed = 42L) {
  set.seed(seed)
  n_units <- 50L
  periods <- 1995:2005
  g_vec <- c(rep(1998L, 15L), rep(2000L, 15L), rep(2002L, 15L),
             rep(NA_integer_, 5L))
  panel <- expand.grid(id = seq_len(n_units), year = periods,
                       stringsAsFactors = FALSE)
  panel <- panel[order(panel$id, panel$year), ]
  rownames(panel) <- NULL
  panel$g     <- g_vec[panel$id]
  panel$treat <- as.integer(!is.na(panel$g) & panel$year >= panel$g)
  unit_fe     <- rnorm(n_units)[panel$id]
  time_fe     <- (panel$year - 1995L) * 0.1
  panel$y     <- unit_fe + time_fe + 1.5 * panel$treat +
                 rnorm(nrow(panel), sd = 0.3)
  panel
}

twm_data <- make_twm_data()

# ---------------------------------------------------------------------------
# Test 1 — return type and column contract
# ---------------------------------------------------------------------------
test_that("twm estimator returns es_result object with required columns", {
  result <- run_es(
    data      = twm_data,
    outcome   = y,
    time      = year,
    timing    = g,
    unit      = id,
    fe        = ~ id + year,
    staggered = TRUE,
    estimator = "twm"
  )

  expect_s3_class(result, "es_result")
  expect_true(is.data.frame(result))

  required <- c("term", "estimate", "std.error", "statistic", "p.value",
                "relative_time", "is_baseline", "conf_low_95", "conf_high_95")
  expect_true(all(required %in% names(result)))

  non_base <- result[!result$is_baseline, ]
  expect_true(all(is.finite(non_base$estimate)))
  expect_true(all(non_base$std.error > 0))

  # Baseline row at relative_time == -1, estimate == 0
  expect_equal(sum(result$is_baseline), 1L)
  expect_equal(result$estimate[result$is_baseline], 0)
  expect_equal(result$std.error[result$is_baseline], 0)
})

# ---------------------------------------------------------------------------
# Test 2 — tau_gt attribute structure
# ---------------------------------------------------------------------------
test_that("twm tau_gt attribute has correct structure", {
  result <- run_es(
    data      = twm_data,
    outcome   = y,
    time      = year,
    timing    = g,
    unit      = id,
    fe        = ~ id + year,
    staggered = TRUE,
    estimator = "twm"
  )

  tau_gt <- attr(result, "tau_gt")
  expect_true(is.data.frame(tau_gt))
  expect_true(all(c("g", "t", "l", "estimate", "std_error") %in% names(tau_gt)))

  # l = t - g for all rows
  expect_equal(tau_gt$l, tau_gt$t - tau_gt$g)

  # No baseline rows in tau_gt (excluded from regression)
  expect_false(-1L %in% tau_gt$l)
})

# ---------------------------------------------------------------------------
# Test 3 — PRIMARY: numerical equivalence with SA (tolerance 1e-10)
#
# TWM (trends=FALSE) and SA are the same saturated CATT regression with
# unit+time FE, just parameterised differently:
#   TWM:  indicators are 1{G_i=g} * 1{t=s}    (cohort x calendar-time)
#   SA:   indicators are 1{G_i=g} * 1{t-G_i=l} (cohort x relative-time)
# After aggregation to event-study, both must give bit-for-bit identical
# estimates (algebraic identity).
# ---------------------------------------------------------------------------
test_that("twm estimates match sa estimates exactly (algebraic equivalence)", {
  res_twm <- run_es(
    data      = twm_data,
    outcome   = y,
    time      = year,
    timing    = g,
    unit      = id,
    fe        = ~ id + year,
    staggered = TRUE,
    estimator = "twm"
  )
  res_sa <- run_es(
    data      = twm_data,
    outcome   = y,
    time      = year,
    timing    = g,
    unit      = id,
    fe        = ~ id + year,
    staggered = TRUE,
    estimator = "sa"
  )

  # Align on common relative times (excluding baseline)
  common_l <- intersect(
    res_twm$relative_time[!res_twm$is_baseline],
    res_sa$relative_time[!res_sa$is_baseline]
  )
  expect_gt(length(common_l), 0L)

  twm_est <- res_twm$estimate[res_twm$relative_time %in% common_l &
                                !res_twm$is_baseline]
  sa_est  <- res_sa$estimate[res_sa$relative_time %in% common_l &
                               !res_sa$is_baseline]

  twm_est <- twm_est[order(res_twm$relative_time[res_twm$relative_time %in%
                                                    common_l & !res_twm$is_baseline])]
  sa_est  <- sa_est[order(res_sa$relative_time[res_sa$relative_time %in%
                                                 common_l & !res_sa$is_baseline])]

  expect_equal(twm_est, sa_est, tolerance = 1e-10,
               label = "TWM vs SA: point estimates")
})

# ---------------------------------------------------------------------------
# Test 4 — SE equivalence with SA (tolerance 1e-8)
# ---------------------------------------------------------------------------
test_that("twm standard errors match sa at tolerance 1e-8", {
  res_twm <- run_es(
    data      = twm_data,
    outcome   = y,
    time      = year,
    timing    = g,
    unit      = id,
    fe        = ~ id + year,
    staggered = TRUE,
    estimator = "twm"
  )
  res_sa <- run_es(
    data      = twm_data,
    outcome   = y,
    time      = year,
    timing    = g,
    unit      = id,
    fe        = ~ id + year,
    staggered = TRUE,
    estimator = "sa"
  )

  common_l <- intersect(
    res_twm$relative_time[!res_twm$is_baseline],
    res_sa$relative_time[!res_sa$is_baseline]
  )

  get_se <- function(res, ls) {
    v <- res$std.error[res$relative_time %in% ls & !res$is_baseline]
    v[order(res$relative_time[res$relative_time %in% ls & !res$is_baseline])]
  }

  expect_equal(get_se(res_twm, common_l), get_se(res_sa, common_l),
               tolerance = 1e-8, label = "TWM vs SA: standard errors")
})

# ---------------------------------------------------------------------------
# Test 5 — three-way agreement: TWM ≡ SA ≡ fixest::sunab()
# ---------------------------------------------------------------------------
test_that("twm matches fixest::sunab() at tolerance 1e-6", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("broom")

  # sunab() encodes never-treated as Inf
  ref_data       <- twm_data
  ref_data$g_ref <- ifelse(is.na(twm_data$g), Inf, twm_data$g)

  sunab_fn <- utils::getFromNamespace("sunab", "fixest")
  ref_fml  <- stats::as.formula("y ~ sunab(g_ref, year) | id + year")
  fml_env  <- new.env(parent = environment(ref_fml))
  fml_env$sunab <- sunab_fn
  environment(ref_fml) <- fml_env

  ref_model <- fixest::feols(ref_fml, data = ref_data, vcov = "HC1")
  ref_tidy  <- broom::tidy(ref_model)
  ref_rel   <- suppressWarnings(
    as.integer(gsub(".*::(-?\\d+)$", "\\1", ref_tidy$term))
  )
  ref_tidy  <- ref_tidy[!is.na(ref_rel), ]
  ref_rel   <- ref_rel[!is.na(ref_rel)]

  res_twm <- run_es(
    data      = twm_data,
    outcome   = y,
    time      = year,
    timing    = g,
    unit      = id,
    fe        = ~ id + year,
    staggered = TRUE,
    estimator = "twm"
  )

  common_l <- intersect(res_twm$relative_time[!res_twm$is_baseline], ref_rel)
  expect_gt(length(common_l), 0L)

  twm_aligned <- res_twm[res_twm$relative_time %in% common_l &
                           !res_twm$is_baseline, ]
  twm_aligned <- twm_aligned[order(twm_aligned$relative_time), ]

  ord         <- order(ref_rel[ref_rel %in% common_l])
  ref_aligned <- ref_tidy$estimate[ref_rel %in% common_l][ord]

  expect_equal(twm_aligned$estimate, ref_aligned, tolerance = 1e-6,
               label = "TWM vs sunab: point estimates")
})

# ---------------------------------------------------------------------------
# Test 6 — cohort-share weights (same anchor as SA Test 3)
# ---------------------------------------------------------------------------
test_that("twm cohort-share weights match manual calculation", {
  result <- run_es(
    data      = twm_data,
    outcome   = y,
    time      = year,
    timing    = g,
    unit      = id,
    fe        = ~ id + year,
    staggered = TRUE,
    estimator = "twm"
  )

  tau_gt <- attr(result, "tau_gt")

  # At l = +5: cohorts 1998 (g+5=2003) and 2000 (g+5=2005) are in sample,
  # cohort 2002 (g+5=2007 > 2005) is not. Both have n_g = 15, equal weights.
  tau_l5 <- tau_gt[tau_gt$l == 5L, ]
  expect_setequal(tau_l5$g, c(1998L, 2000L))

  theta_manual <- mean(tau_l5$estimate)   # equal cohort sizes => simple mean
  theta_result <- result$estimate[result$relative_time == 5L &
                                    !result$is_baseline]
  expect_equal(theta_result, theta_manual, tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# Test 7 — informative errors
# ---------------------------------------------------------------------------
test_that("twm errors informatively on missing unit column", {
  expect_error(
    run_es(data = twm_data, outcome = y, time = year, timing = g,
           staggered = TRUE, estimator = "twm"),
    regexp = "unit.*required|required.*unit",
    ignore.case = TRUE
  )
})

test_that("twm errors when trends = TRUE (not yet implemented)", {
  expect_error(
    run_es(data = twm_data, outcome = y, time = year, timing = g,
           unit = id, fe = ~ id + year, staggered = TRUE,
           estimator = "twm", trends = TRUE),
    regexp = "trends.*not yet implemented|not yet implemented.*trends",
    ignore.case = TRUE
  )
})
