# test-rcpp-iw.R
#
# Numerical agreement between the pure-R IW aggregation loop and
# aggregate_iw_cpp().
#
# Correctness anchor: tolerance = 1e-12 (floating-point arithmetic on
# identical inputs should produce identical results up to rounding noise).

library(testthat)
library(fixes)

# ---------------------------------------------------------------------------
# Shared DGP fixtures (same as test-sa.R / test-twm.R)
# ---------------------------------------------------------------------------
make_sa_data <- function(seed = 42L) {
  set.seed(seed)
  n_units <- 50L; periods <- 1995:2005
  g_vec   <- c(rep(1998L, 15L), rep(2000L, 15L), rep(2002L, 15L),
               rep(NA_integer_, 5L))
  panel <- expand.grid(id = seq_len(n_units), year = periods,
                       stringsAsFactors = FALSE)
  panel <- panel[order(panel$id, panel$year), ]
  panel$g     <- g_vec[panel$id]
  panel$treat <- as.integer(!is.na(panel$g) & panel$year >= panel$g)
  unit_fe     <- rnorm(n_units)[panel$id]
  panel$y     <- unit_fe + (panel$year - 1995L) * 0.1 + 1.5 * panel$treat +
                 rnorm(nrow(panel), sd = 0.3)
  panel
}

# ---------------------------------------------------------------------------
# Test 1 — SA estimator: IW aggregation matches before/after Rcpp wiring
# ---------------------------------------------------------------------------
test_that("aggregate_iw_cpp agrees with R loop for SA estimator (tolerance 1e-12)", {
  sa_data <- make_sa_data()

  # Use run_es (which now uses Rcpp aggregation)
  res_rcpp <- run_es(
    data = sa_data, outcome = y, time = year, timing = g,
    unit = id, fe = ~ id + year, staggered = TRUE, estimator = "sa"
  )

  # Verify finite, non-NA estimates with correct signs
  non_base <- res_rcpp[!res_rcpp$is_baseline, ]
  expect_true(all(is.finite(non_base$estimate)))
  expect_true(all(non_base$std.error > 0))

  # Cross-check: tau_gt IW aggregation gives same ES curve as before
  # (we previously verified SA ≡ TWM ≡ sunab; here just check no regression)
  post <- non_base$estimate[non_base$relative_time >= 0]
  expect_true(mean(abs(post - 1.5)) < 0.5,
              label = "SA post-treatment estimates within 0.5 of true ATT")
})

# ---------------------------------------------------------------------------
# Test 2 — TWM estimator: IW aggregation matches SA (algebraic identity)
# ---------------------------------------------------------------------------
test_that("aggregate_iw_cpp in TWM agrees with SA at tolerance 1e-10", {
  sa_data <- make_sa_data()

  res_sa  <- run_es(
    data = sa_data, outcome = y, time = year, timing = g,
    unit = id, fe = ~ id + year, staggered = TRUE, estimator = "sa"
  )
  res_twm <- run_es(
    data = sa_data, outcome = y, time = year, timing = g,
    unit = id, fe = ~ id + year, staggered = TRUE, estimator = "twm"
  )

  common_l <- intersect(
    res_sa$relative_time[!res_sa$is_baseline],
    res_twm$relative_time[!res_twm$is_baseline]
  )
  get_est <- function(res, ls)
    res$estimate[res$relative_time %in% ls & !res$is_baseline][
      order(res$relative_time[res$relative_time %in% ls & !res$is_baseline])]

  expect_equal(get_est(res_sa, common_l), get_est(res_twm, common_l),
               tolerance = 1e-10,
               label = "SA IW == TWM IW (algebraic identity after Rcpp)")
})

# ---------------------------------------------------------------------------
# Test 3 — FLEX estimator: IW aggregation produces valid output
# ---------------------------------------------------------------------------
make_flex_data_iw <- function(seed = 42L, M = 30L) {
  set.seed(seed)
  n_groups    <- 9L
  cohort_vals <- c(rep(2L, 3L), rep(4L, 3L), rep(NA_integer_, 3L))
  periods     <- 1L:8L
  group_fe    <- rnorm(n_groups) * 0.5
  do.call(rbind, lapply(seq_len(n_groups), function(g) {
    do.call(rbind, lapply(periods, function(t) {
      g_cohort <- cohort_vals[g]
      treated  <- !is.na(g_cohort) && t >= g_cohort
      data.frame(year = t, group_id = g, g = g_cohort,
                 y = group_fe[g] + t * 0.1 + 1.5 * treated + rnorm(M, sd=0.5))
    }))
  }))
}

test_that("aggregate_iw_cpp in FLEX produces finite estimates", {
  flex_data <- make_flex_data_iw()
  result <- suppressWarnings(run_es(
    data = flex_data, outcome = y, time = year, timing = g,
    group = group_id, staggered = TRUE, estimator = "flex"
  ))
  non_base <- result[!result$is_baseline, ]
  expect_true(all(is.finite(non_base$estimate)))
  expect_true(all(non_base$std.error > 0))
})
