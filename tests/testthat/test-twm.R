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

# ---------------------------------------------------------------------------
# Test 9 — trends=FALSE explicit: regression guard for TWM ≡ SA equivalence
# ---------------------------------------------------------------------------
test_that("twm trends=FALSE explicit gives same result as default", {
  res_default  <- run_es(
    data = twm_data, outcome = y, time = year, timing = g,
    unit = id, fe = ~ id + year, staggered = TRUE, estimator = "twm"
  )
  res_explicit <- run_es(
    data = twm_data, outcome = y, time = year, timing = g,
    unit = id, fe = ~ id + year, staggered = TRUE,
    estimator = "twm", trends = FALSE
  )
  expect_equal(res_default$estimate,  res_explicit$estimate,  tolerance = 1e-12)
  expect_equal(res_default$std.error, res_explicit$std.error, tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# Test 10 — trends=TRUE column contract
#
# trends=TRUE uses post-treatment-only cells (Wooldridge 2025 Section 8).
# The baseline period (l = -1) is pre-treatment and therefore not in the
# output — sum(is_baseline) == 0, and all relative_time values are >= 0.
# ---------------------------------------------------------------------------
test_that("twm trends=TRUE returns es_result with required columns (post-only)", {
  result <- suppressWarnings(run_es(
    data = twm_data, outcome = y, time = year, timing = g,
    unit = id, fe = ~ id + year, staggered = TRUE,
    estimator = "twm", trends = TRUE
  ))

  expect_s3_class(result, "es_result")
  required <- c("term", "estimate", "std.error", "statistic", "p.value",
                "relative_time", "is_baseline", "conf_low_95", "conf_high_95")
  expect_true(all(required %in% names(result)))
  # Post-treatment-only: no pre-treatment estimates, no artificial baseline row
  expect_equal(sum(result$is_baseline), 0L)
  expect_true(all(result$relative_time >= 0L),
              label = "trends=TRUE: all relative_time values non-negative")
  expect_true(all(is.finite(result$estimate)))
  expect_true(all(result$std.error > 0))
})

# ---------------------------------------------------------------------------
# Test 11 — trends=TRUE reduces ATT bias under cohort-specific pre-trend DGP
#
# DGP: y_it = alpha_i + lambda_t + eta_g * (year - year_mean) * 1{G_i=g}
#           + 1.5 * w_it + eps
# trends=FALSE: eta_g term biases post estimates away from 1.5.
# trends=TRUE:  d_g*t regressors absorb eta_g*t → unbiased.
# ---------------------------------------------------------------------------
make_twm_trend_data <- function(seed = 77L, eta_1 = 0.4, eta_2 = -0.3) {
  set.seed(seed)
  periods <- 1993:2006
  n_units <- 90L
  g_vec   <- c(rep(1998L, 30L), rep(2000L, 30L), rep(NA_integer_, 30L))
  panel   <- expand.grid(id = seq_len(n_units), year = periods,
                         stringsAsFactors = FALSE)
  panel   <- panel[order(panel$id, panel$year), ]
  rownames(panel) <- NULL
  panel$g     <- g_vec[panel$id]
  panel$treat <- as.integer(!is.na(panel$g) & panel$year >= panel$g)

  eta_i   <- c(rep(eta_1, 30L), rep(eta_2, 30L), rep(0, 30L))
  unit_fe <- rnorm(n_units, sd = 0.5)[panel$id]
  time_fe <- (panel$year - 1993L) * 0.1
  trend   <- eta_i[panel$id] * (panel$year - mean(periods))
  panel$y <- unit_fe + time_fe + trend + 1.5 * panel$treat +
             rnorm(nrow(panel), sd = 0.2)
  panel
}

test_that("twm trends=TRUE reduces ATT bias under cohort-specific pre-trend DGP", {
  trend_data <- make_twm_trend_data()

  res_notrend <- run_es(
    data = trend_data, outcome = y, time = year, timing = g,
    unit = id, fe = ~ id + year, staggered = TRUE,
    estimator = "twm", trends = FALSE
  )
  res_trend <- suppressWarnings(run_es(
    data = trend_data, outcome = y, time = year, timing = g,
    unit = id, fe = ~ id + year, staggered = TRUE,
    estimator = "twm", trends = TRUE
  ))

  true_att     <- 1.5
  post_notrend <- res_notrend$estimate[res_notrend$relative_time >= 0 &
                                         !res_notrend$is_baseline]
  post_trend   <- res_trend$estimate[res_trend$relative_time >= 0 &
                                       !res_trend$is_baseline]

  bias_notrend <- sqrt(mean((post_notrend - true_att)^2))
  bias_trend   <- sqrt(mean((post_trend   - true_att)^2))

  expect_lt(bias_trend, bias_notrend,
            label = "trends=TRUE RMSE < trends=FALSE RMSE under pre-trend DGP")
})

# ---------------------------------------------------------------------------
# Test 12 — warning when cohort has fewer than 2 pre-treatment periods
# ---------------------------------------------------------------------------
test_that("twm trends=TRUE warns when cohort has < 2 pre-treatment periods", {
  set.seed(9L)
  warn_data       <- data.frame(
    id   = rep(1:10, each = 4),
    year = rep(1:4, 10),
    y    = rnorm(40),
    stringsAsFactors = FALSE
  )
  warn_data$g <- ifelse(warn_data$id <= 5L, 2L, NA_integer_)

  expect_warning(
    run_es(data = warn_data, outcome = y, time = year, timing = g,
           unit = id, fe = ~ id + year, staggered = TRUE,
           estimator = "twm", trends = TRUE),
    regexp = "fewer than 2 pre.treatment periods",
    ignore.case = TRUE
  )
})

# ---------------------------------------------------------------------------
# Test 13 — covariates: column contract unchanged
# ---------------------------------------------------------------------------
test_that("twm covariates: column contract and baseline row unchanged", {
  twm_cov_data <- twm_data
  set.seed(101L)
  twm_cov_data$x <- rnorm(nrow(twm_cov_data))

  result <- suppressWarnings(run_es(
    data = twm_cov_data, outcome = y, time = year, timing = g,
    unit = id, fe = ~ id + year, staggered = TRUE,
    estimator = "twm", covariates = ~ x
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
# Test 14 — covariates: orthogonal covariate does not shift ATT estimates
# ---------------------------------------------------------------------------
test_that("twm covariates: orthogonal covariate leaves ATT estimates unchanged", {
  set.seed(202L)
  twm_cov_data        <- twm_data
  twm_cov_data$x_orth <- rnorm(nrow(twm_cov_data))   # pure noise

  res_nocov <- run_es(
    data = twm_cov_data, outcome = y, time = year, timing = g,
    unit = id, fe = ~ id + year, staggered = TRUE, estimator = "twm"
  )
  res_cov <- suppressWarnings(run_es(
    data = twm_cov_data, outcome = y, time = year, timing = g,
    unit = id, fe = ~ id + year, staggered = TRUE,
    estimator = "twm", covariates = ~ x_orth
  ))

  common_l <- intersect(
    res_nocov$relative_time[!res_nocov$is_baseline],
    res_cov$relative_time[!res_cov$is_baseline]
  )
  get_est <- function(res, ls)
    res$estimate[res$relative_time %in% ls & !res$is_baseline][
      order(res$relative_time[res$relative_time %in% ls & !res$is_baseline])]

  expect_equal(get_est(res_nocov, common_l), get_est(res_cov, common_l),
               tolerance = 0.2,
               label = "orthogonal covariate does not shift ATT estimates by >0.2")
})

# ---------------------------------------------------------------------------
# Test 15 — covariates: reduces ATT bias under differential pre-trend DGP
#
# DGP: y_it = alpha_i + lambda_t + x_i * (t - t0) * 0.2 + 1.5 * w_it + eps
#      x_i correlated with cohort → x_i*(t-t0) creates differential pre-trends
# Without covariates: OLS conflates ATT with x_i*t term → biased.
# With covariates=~x: i(time,x) terms absorb x_i*(t-t0) → unbiased.
# ---------------------------------------------------------------------------
make_twm_cov_bias_data <- function(seed = 303L) {
  set.seed(seed)
  periods <- 1995:2005
  n_units <- 100L
  g_vec   <- c(rep(1998L, 30L), rep(2000L, 30L), rep(2002L, 30L),
               rep(NA_integer_, 10L))
  panel <- expand.grid(id = seq_len(n_units), year = periods,
                       stringsAsFactors = FALSE)
  panel <- panel[order(panel$id, panel$year), ]
  rownames(panel) <- NULL
  panel$g     <- g_vec[panel$id]
  panel$treat <- as.integer(!is.na(panel$g) & panel$year >= panel$g)

  x_vec         <- rnorm(n_units)
  x_vec[1:30]   <- x_vec[1:30]  + 1.5   # cohort 1998: high x
  x_vec[31:60]  <- x_vec[31:60] + 0.0   # cohort 2000: medium x
  x_vec[61:90]  <- x_vec[61:90] - 1.5   # cohort 2002: low x
  panel$x       <- x_vec[panel$id]

  t0    <- min(periods)
  panel$y <- (x_vec[panel$id] * 0.5 +
              rnorm(n_units, sd = 0.3)[panel$id] +
              (panel$year - t0) * 0.1 +
              panel$x * (panel$year - t0) * 0.2 +
              1.5 * panel$treat +
              rnorm(nrow(panel), sd = 0.2))
  panel
}

test_that("twm covariates: reduces ATT bias under differential pre-trend DGP", {
  cov_data <- make_twm_cov_bias_data()

  res_nocov <- run_es(
    data = cov_data, outcome = y, time = year, timing = g,
    unit = id, fe = ~ id + year, staggered = TRUE, estimator = "twm"
  )
  res_cov <- suppressWarnings(run_es(
    data = cov_data, outcome = y, time = year, timing = g,
    unit = id, fe = ~ id + year, staggered = TRUE,
    estimator = "twm", covariates = ~ x
  ))

  true_att   <- 1.5
  post_nocov <- res_nocov$estimate[res_nocov$relative_time >= 0 & !res_nocov$is_baseline]
  post_cov   <- res_cov$estimate[res_cov$relative_time >= 0 & !res_cov$is_baseline]

  bias_nocov <- sqrt(mean((post_nocov - true_att)^2))
  bias_cov   <- sqrt(mean((post_cov   - true_att)^2))

  expect_lt(bias_cov, bias_nocov,
            label = "covariates RMSE < no-covariates RMSE under differential pre-trend DGP")
})

# ---------------------------------------------------------------------------
# Test 16 — trends=TRUE with exactly 2 pre-treatment periods (boundary condition)
#
# Wooldridge (2025) requires >= 2 pre-treatment periods for trends=TRUE.
# Verify that exactly 2 periods produces valid estimates WITHOUT a warning.
# ---------------------------------------------------------------------------
test_that("twm trends=TRUE succeeds with exactly 2 pre-treatment periods", {
  set.seed(555L)
  # 10 units, periods 1:4, cohort at period 3 → exactly 2 pre-treatment periods
  exact2_data <- data.frame(
    id   = rep(1:10, each = 4),
    year = rep(1:4,  10),
    y    = rnorm(40),
    stringsAsFactors = FALSE
  )
  # Units 1-7 treated at period 3 (pre-periods: 1, 2 → exactly 2)
  # Units 8-10 never treated
  exact2_data$g <- ifelse(exact2_data$id <= 7L, 3L, NA_integer_)

  # Must NOT warn about "fewer than 2 pre-treatment periods"
  expect_no_warning(
    result <- run_es(
      data      = exact2_data,
      outcome   = y,
      time      = year,
      timing    = g,
      unit      = id,
      fe        = ~ id + year,
      staggered = TRUE,
      estimator = "twm",
      trends    = TRUE
    )
  )

  expect_s3_class(result, "es_result")
  # trends=TRUE: post-treatment only
  expect_true(all(result$relative_time >= 0L))
  expect_true(all(is.finite(result$estimate)))
})

# ---------------------------------------------------------------------------
# Test 17 — explicit lead_range / lag_range trims output correctly
#
# Verifies that the .es_finalize() window filter works for TWM:
# requesting lead_range=1, lag_range=2 must return only relative_time in [-1, 2].
# ---------------------------------------------------------------------------
test_that("twm respects explicit lead_range and lag_range", {
  result_full <- run_es(
    data      = twm_data,
    outcome   = y,
    time      = year,
    timing    = g,
    unit      = id,
    fe        = ~ id + year,
    staggered = TRUE,
    estimator = "twm"
  )

  result_trim <- run_es(
    data       = twm_data,
    outcome    = y,
    time       = year,
    timing     = g,
    unit       = id,
    fe         = ~ id + year,
    staggered  = TRUE,
    estimator  = "twm",
    lead_range = 1L,
    lag_range  = 2L
  )

  # Trimmed result must stay within [-1, 2]
  expect_true(all(result_trim$relative_time >= -1L),
              label = "no relative_time < -lead_range")
  expect_true(all(result_trim$relative_time <= 2L),
              label = "no relative_time > lag_range")

  # Full result has more rows than trimmed
  expect_gt(nrow(result_full), nrow(result_trim))

  # Where they overlap, estimates must be identical
  common_l <- intersect(result_full$relative_time, result_trim$relative_time)
  est_full  <- result_full$estimate[match(common_l, result_full$relative_time)]
  est_trim  <- result_trim$estimate[match(common_l, result_trim$relative_time)]
  expect_equal(est_full, est_trim, tolerance = 1e-12,
               label = "estimates identical within shared window")
})
