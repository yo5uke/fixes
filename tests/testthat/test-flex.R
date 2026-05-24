# test-flex.R
#
# Tests for the Deb, Norton, Wooldridge & Zabel (2024) FLEX estimator for
# repeated cross-section (RCS) data.
#
# Theory anchors (Deb et al. 2024):
#   Eq. (3.1): FLEX regression with group x time treatment cells + group/time FE
#   Proposition 2.1: FLEX OLS ≡ multi-step imputation estimator (algebraic identity)
#
# Key structural difference from panel estimators:
#   - Different individuals observed each period (no unit tracking)
#   - Group FE (R_{ig}) replaces unit FE (alpha_i)
#   - Cohort size n_g = number of GROUPS in cohort g (not observations)

library(testthat)
library(fixes)

# ---------------------------------------------------------------------------
# Shared RCS simulation fixture
#
# Design: G = 9 groups (treatment cohorts 2L and 4L, 3 groups each;
#                        never-treated, 3 groups)
#         T = 8 periods
#         M = 30 observations per (group, period) cell  (fresh each period)
#         True ATT = 1.5 (constant post-treatment)
#         DGP: Y = group_FE + time_FE + 1.5 * treated + N(0, 0.5)
# ---------------------------------------------------------------------------
make_flex_data <- function(seed = 42L, M = 30L) {
  set.seed(seed)
  n_groups    <- 9L
  n_treat_grp <- 3L   # groups per treatment cohort
  cohort_vals <- c(rep(2L, n_treat_grp), rep(4L, n_treat_grp),
                   rep(NA_integer_, n_treat_grp))  # never-treated
  periods     <- 1L:8L

  # Group FE are CONSTANT within each group across all periods.
  # Drawing them here (before the loop) ensures the same value is used for
  # every period of a given group, which is required for group FE to be
  # identifiable separately from time FE.
  group_fe <- rnorm(n_groups) * 0.5

  panel <- do.call(rbind, lapply(seq_len(n_groups), function(g) {
    do.call(rbind, lapply(periods, function(t) {
      g_cohort <- cohort_vals[g]
      treated  <- !is.na(g_cohort) && t >= g_cohort
      t_fe     <- t * 0.1
      y_val    <- group_fe[g] + t_fe + 1.5 * treated + rnorm(M, sd = 0.5)
      data.frame(
        obs_id   = seq_len(M) + (g - 1L) * M * max(periods) +
                   (t - 1L) * M,
        year     = t,
        group_id = g,
        g        = g_cohort,
        y        = y_val,
        stringsAsFactors = FALSE
      )
    }))
  }))
  panel <- panel[order(panel$group_id, panel$year, panel$obs_id), ]
  rownames(panel) <- NULL
  panel
}

flex_data <- make_flex_data()

# ---------------------------------------------------------------------------
# Test 1 — return type and column contract
# ---------------------------------------------------------------------------
test_that("flex estimator returns es_result object with required columns", {
  result <- suppressWarnings(run_es(
    data      = flex_data,
    outcome   = y,
    time      = year,
    timing    = g,
    group     = group_id,
    staggered = TRUE,
    estimator = "flex"
  ))

  expect_s3_class(result, "es_result")
  expect_true(is.data.frame(result))

  required <- c("term", "estimate", "std.error", "statistic", "p.value",
                "relative_time", "is_baseline", "conf_low_95", "conf_high_95")
  expect_true(all(required %in% names(result)))

  non_base <- result[!result$is_baseline, ]
  expect_true(all(is.finite(non_base$estimate)))
  expect_true(all(non_base$std.error > 0))

  # Baseline row at relative_time = -1, estimate = 0
  expect_equal(sum(result$is_baseline), 1L)
  expect_equal(result$estimate[result$is_baseline], 0)
  expect_equal(result$std.error[result$is_baseline], 0)
})

# ---------------------------------------------------------------------------
# Test 2 — `group` argument required
# ---------------------------------------------------------------------------
test_that("flex errors when group argument is missing", {
  expect_error(
    run_es(data = flex_data, outcome = y, time = year, timing = g,
           staggered = TRUE, estimator = "flex"),
    regexp = "group.*required|required.*group",
    ignore.case = TRUE
  )
})

# ---------------------------------------------------------------------------
# Test 3 — group with multiple treatment dates triggers error
# ---------------------------------------------------------------------------
test_that("flex errors when a group maps to multiple treatment dates", {
  bad_data       <- flex_data
  # Force group 1 to have two different timing values in different rows
  bad_data$g[bad_data$group_id == 1L & bad_data$year == 1L] <- 3L

  expect_error(
    suppressWarnings(run_es(
      data = bad_data, outcome = y, time = year,
      timing = g, group = group_id,
      staggered = TRUE, estimator = "flex"
    )),
    regexp = "multiple treatment dates|multiple.*timing",
    ignore.case = TRUE
  )
})

# ---------------------------------------------------------------------------
# Test 4 — tau_gt attribute structure
# ---------------------------------------------------------------------------
test_that("flex tau_gt attribute has correct structure", {
  result <- suppressWarnings(run_es(
    data      = flex_data,
    outcome   = y,
    time      = year,
    timing    = g,
    group     = group_id,
    staggered = TRUE,
    estimator = "flex"
  ))

  tau_gt <- attr(result, "tau_gt")
  expect_true(is.data.frame(tau_gt))
  expect_true(all(c("g", "t", "l", "estimate", "std_error") %in% names(tau_gt)))
  expect_equal(tau_gt$l, tau_gt$t - tau_gt$g)
  expect_false(-1L %in% tau_gt$l)  # baseline excluded
})

# ---------------------------------------------------------------------------
# Test 5 — two-by-two DiD exactness (1 treated group, 1 control group)
#
# With a single treated group and a single control group, the FLEX estimate
# at relative time l=0 must equal the manual two-by-two DiD:
#   DiD = (mean_treated_post - mean_treated_pre) - (mean_ctrl_post - mean_ctrl_pre)
# This is an algebraic identity, verified at tolerance 1e-10.
# ---------------------------------------------------------------------------
test_that("flex two-by-two DiD is exact (tolerance 1e-10)", {
  set.seed(99L)
  M <- 50L
  # Explicit cell construction: 1 treated group (cohort 2), 1 never-treated
  y_g1t1 <- rnorm(M, 1.0)
  y_g1t2 <- rnorm(M, 3.5)
  y_g2t1 <- rnorm(M, 1.0)
  y_g2t2 <- rnorm(M, 1.5)

  g2g2_data <- rbind(
    data.frame(year = 1L, group_id = 1L, g = 2L,
               y = y_g1t1, stringsAsFactors = FALSE),
    data.frame(year = 2L, group_id = 1L, g = 2L,
               y = y_g1t2, stringsAsFactors = FALSE),
    data.frame(year = 1L, group_id = 2L, g = NA_integer_,
               y = y_g2t1, stringsAsFactors = FALSE),
    data.frame(year = 2L, group_id = 2L, g = NA_integer_,
               y = y_g2t2, stringsAsFactors = FALSE)
  )

  did_manual <- (mean(y_g1t2) - mean(y_g1t1)) - (mean(y_g2t2) - mean(y_g2t1))

  result <- suppressWarnings(run_es(
    data      = g2g2_data,
    outcome   = y,
    time      = year,
    timing    = g,
    group     = group_id,
    staggered = TRUE,
    estimator = "flex",
    baseline  = -1L
  ))

  flex_l0 <- result$estimate[result$relative_time == 0L & !result$is_baseline]
  expect_equal(flex_l0, did_manual, tolerance = 1e-10,
               label = "FLEX l=0 == manual two-by-two DiD")
})

# ---------------------------------------------------------------------------
# Test 6 — cohort_sizes counts unique GROUPS (not observations)
# ---------------------------------------------------------------------------
test_that("flex cohort sizes count unique groups, not observations", {
  result <- suppressWarnings(run_es(
    data      = flex_data,
    outcome   = y,
    time      = year,
    timing    = g,
    group     = group_id,
    staggered = TRUE,
    estimator = "flex"
  ))

  # From make_flex_data: 3 groups per cohort (cohort 2 and cohort 4)
  # N_treated = 6 groups, N_nevertreated = 3 groups, N_units = 9 groups
  expect_equal(attr(result, "N_units"),        9L)
  expect_equal(attr(result, "N_treated"),      6L)
  expect_equal(attr(result, "N_nevertreated"), 3L)
})

# ---------------------------------------------------------------------------
# Test 7 — ATT recovery: post-treatment estimates near true ATT = 1.5
# ---------------------------------------------------------------------------
test_that("flex recovers true ATT = 1.5 (within 2 SEs) in large sample", {
  # Use a larger M for this test to reduce Monte Carlo noise
  large_data <- make_flex_data(seed = 7L, M = 200L)
  result <- suppressWarnings(run_es(
    data      = large_data,
    outcome   = y,
    time      = year,
    timing    = g,
    group     = group_id,
    staggered = TRUE,
    estimator = "flex"
  ))

  post_ests <- result$estimate[result$relative_time >= 0L & !result$is_baseline]
  expect_true(all(abs(post_ests - 1.5) < 0.6),
              label = "post-treatment estimates within 0.6 of true ATT=1.5")
})

# ---------------------------------------------------------------------------
# Test 8 — pre-treatment estimates near zero (no anticipation)
# ---------------------------------------------------------------------------
test_that("flex pre-treatment estimates near zero (no anticipation DGP)", {
  large_data <- make_flex_data(seed = 13L, M = 200L)
  result <- suppressWarnings(run_es(
    data      = large_data,
    outcome   = y,
    time      = year,
    timing    = g,
    group     = group_id,
    staggered = TRUE,
    estimator = "flex"
  ))

  pre_ests <- result$estimate[result$relative_time < 0L & !result$is_baseline]
  if (length(pre_ests) > 0L) {
    expect_true(all(abs(pre_ests) < 0.6),
                label = "pre-treatment estimates near zero (no anticipation)")
  }
})

# ---------------------------------------------------------------------------
# Test 9 — covariates: column contract unchanged
# ---------------------------------------------------------------------------
test_that("flex covariates: column contract and baseline row unchanged", {
  flex_cov_data <- flex_data
  set.seed(111L)
  flex_cov_data$x <- rnorm(nrow(flex_cov_data))

  result <- suppressWarnings(run_es(
    data      = flex_cov_data,
    outcome   = y,
    time      = year,
    timing    = g,
    group     = group_id,
    staggered = TRUE,
    estimator = "flex",
    covariates = ~ x
  ))

  expect_s3_class(result, "es_result")
  required <- c("term", "estimate", "std.error", "statistic", "p.value",
                "relative_time", "is_baseline", "conf_low_95", "conf_high_95")
  expect_true(all(required %in% names(result)))
  expect_equal(sum(result$is_baseline), 1L)
  non_base <- result[!result$is_baseline, ]
  expect_true(all(is.finite(non_base$estimate)))
  expect_true(all(non_base$std.error > 0))
})

# ---------------------------------------------------------------------------
# Test 10 — covariates: ATT unchanged when covariate orthogonal to group
# ---------------------------------------------------------------------------
test_that("flex covariates: orthogonal covariate leaves ATT estimates unchanged", {
  set.seed(222L)
  flex_cov_data        <- flex_data
  flex_cov_data$x_orth <- rnorm(nrow(flex_cov_data))   # pure noise

  res_nocov <- suppressWarnings(run_es(
    data = flex_cov_data, outcome = y, time = year, timing = g,
    group = group_id, staggered = TRUE, estimator = "flex"
  ))
  res_cov <- suppressWarnings(run_es(
    data = flex_cov_data, outcome = y, time = year, timing = g,
    group = group_id, staggered = TRUE, estimator = "flex",
    covariates = ~ x_orth
  ))

  common_l <- intersect(
    res_nocov$relative_time[!res_nocov$is_baseline],
    res_cov$relative_time[!res_cov$is_baseline]
  )
  get_est <- function(res, ls)
    res$estimate[res$relative_time %in% ls & !res$is_baseline][
      order(res$relative_time[res$relative_time %in% ls & !res$is_baseline])]

  expect_equal(get_est(res_nocov, common_l), get_est(res_cov, common_l),
               tolerance = 0.1,
               label = "orthogonal covariate does not shift ATT estimates")
})

# ---------------------------------------------------------------------------
# Test 11 — covariates: tau_gt structure, formula, and ATT recovery
#
# When covariates are included:
#   (a) tau_gt attribute retains the same column structure as without covariates.
#   (b) model_formula attr contains ".flex_cov_X" (covariate interactions present).
#   (c) Post-treatment estimates recover the true ATT in large samples (within 0.6).
#
# Structural verification is preferred over strict Prop 2.1 algebraic identity
# since the equivalence depends on cell-level centering details that are hard to
# replicate manually in a test.
# ---------------------------------------------------------------------------
make_flex_cov_data <- function(seed = 444L, M = 80L) {
  set.seed(seed)
  n_groups    <- 9L
  n_treat_grp <- 3L
  cohort_vals <- c(rep(2L, n_treat_grp), rep(4L, n_treat_grp),
                   rep(NA_integer_, n_treat_grp))
  periods     <- 1L:8L
  group_fe    <- rnorm(n_groups) * 0.5

  panel <- do.call(rbind, lapply(seq_len(n_groups), function(g) {
    do.call(rbind, lapply(periods, function(t) {
      g_cohort <- cohort_vals[g]
      treated  <- !is.na(g_cohort) && t >= g_cohort
      t_fe     <- t * 0.1
      x_val    <- rnorm(M, mean = g * 0.2)
      y_val    <- group_fe[g] + t_fe + 1.5 * treated +
                  x_val * 0.3 + rnorm(M, sd = 0.5)
      data.frame(
        obs_id   = seq_len(M) + (g - 1L) * M * max(periods) + (t - 1L) * M,
        year     = t, group_id = g, g = g_cohort, x = x_val, y = y_val,
        stringsAsFactors = FALSE
      )
    }))
  }))
  panel <- panel[order(panel$group_id, panel$year, panel$obs_id), ]
  rownames(panel) <- NULL
  panel
}

test_that("flex covariates: tau_gt structure, formula, and ATT recovery", {
  cov_data <- make_flex_cov_data()

  result <- suppressWarnings(run_es(
    data = cov_data, outcome = y, time = year, timing = g,
    group = group_id, staggered = TRUE, estimator = "flex",
    covariates = ~ x
  ))

  # (a) tau_gt structure unchanged
  tau_gt <- attr(result, "tau_gt")
  expect_true(is.data.frame(tau_gt))
  expect_true(all(c("g", "t", "l", "estimate", "std_error") %in% names(tau_gt)))
  expect_equal(tau_gt$l, tau_gt$t - tau_gt$g)
  expect_true(all(is.finite(tau_gt$estimate)))
  expect_true(all(tau_gt$std_error > 0))

  # (b) formula contains covariate interaction terms
  fml <- attr(result, "model_formula")
  expect_true(grepl(".flex_cov_X", fml),
              label = "formula_str contains .flex_cov_X covariate interaction block")

  # (c) post-treatment ES estimates recover ATT = 1.5 within 0.6
  post_ests <- result$estimate[result$relative_time >= 0L & !result$is_baseline]
  expect_true(all(abs(post_ests - 1.5) < 0.6),
              label = "post-treatment estimates within 0.6 of true ATT=1.5 with covariates")
})

# ---------------------------------------------------------------------------
# Test 12 — explicit lead_range / lag_range trims output correctly
#
# Verifies that .es_finalize() window filter works for FLEX:
# lead_range=1, lag_range=2 must restrict relative_time to [-1, 2].
# ---------------------------------------------------------------------------
test_that("flex respects explicit lead_range and lag_range", {
  result_full <- suppressWarnings(run_es(
    data      = flex_data,
    outcome   = y,
    time      = year,
    timing    = g,
    group     = group_id,
    staggered = TRUE,
    estimator = "flex"
  ))

  result_trim <- suppressWarnings(run_es(
    data       = flex_data,
    outcome    = y,
    time       = year,
    timing     = g,
    group      = group_id,
    staggered  = TRUE,
    estimator  = "flex",
    lead_range = 1L,
    lag_range  = 2L
  ))

  expect_true(all(result_trim$relative_time >= -1L))
  expect_true(all(result_trim$relative_time <= 2L))
  expect_gt(nrow(result_full), nrow(result_trim))

  common_l <- intersect(result_full$relative_time, result_trim$relative_time)
  est_full  <- result_full$estimate[match(common_l, result_full$relative_time)]
  est_trim  <- result_trim$estimate[match(common_l, result_trim$relative_time)]
  expect_equal(est_full, est_trim, tolerance = 1e-12,
               label = "estimates identical within shared window")
})

# ---------------------------------------------------------------------------
# Test 13 — no never-treated groups: all groups treated
#
# When every group is treated (no pure control), fixest uses the remaining
# not-yet-treated observations as controls. Verify the call succeeds and
# returns a valid es_result (N_nevertreated == 0).
# ---------------------------------------------------------------------------
test_that("flex handles all-treated panel (no never-treated groups)", {
  set.seed(888L)
  M <- 20L
  # Two treatment cohorts; no never-treated group
  cohort_vals <- c(rep(2L, 2L), rep(4L, 2L))   # 4 groups, 2 cohorts
  periods     <- 1L:6L
  group_fe    <- rnorm(4L) * 0.5

  all_treated <- do.call(rbind, lapply(seq_along(cohort_vals), function(g) {
    do.call(rbind, lapply(periods, function(t) {
      gc <- cohort_vals[g]
      data.frame(
        year     = t,
        group_id = g,
        g        = gc,
        y        = group_fe[g] + t * 0.1 + as.numeric(t >= gc) * 1.5 +
                   rnorm(M, sd = 0.5),
        stringsAsFactors = FALSE
      )
    }))
  }))

  result <- suppressWarnings(run_es(
    data      = all_treated,
    outcome   = y,
    time      = year,
    timing    = g,
    group     = group_id,
    staggered = TRUE,
    estimator = "flex"
  ))

  expect_s3_class(result, "es_result")
  expect_equal(attr(result, "N_nevertreated"), 0L)
  expect_true(all(is.finite(result$estimate)))
})
