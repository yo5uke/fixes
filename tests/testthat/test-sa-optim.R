# test-sa-optim.R
#
# Agreement tests for the Strategy-C SA optimisation (i() reformulation).
#
# Both tests must pass BEFORE integration (verifying current code is correct)
# and AFTER integration (verifying the optimisation produces identical output).
#
# The external reference is fixest::sunab() — an independent SA(2021)
# implementation — at tolerance 1e-6 per CLAUDE.md testing policy.

library(testthat)
library(fixes)

# ---------------------------------------------------------------------------
# Fixture: 200 units, 10 periods, 3 cohorts + 20 never-treated
# (larger than test-sa.R to stress the IW aggregation path)
# Never-treated required so fixest::sunab() uses the same comparison group
# as .run_sa() (never-treated as control).
# ---------------------------------------------------------------------------
make_optim_data <- function(seed = 123L) {
  set.seed(seed)
  n_units <- 200L
  periods <- 2001:2010

  # Three equally-sized treated cohorts + 20 never-treated units
  g_vec <- c(
    rep(2004L, 60L),
    rep(2006L, 60L),
    rep(2008L, 60L),
    rep(NA_integer_, 20L)
  )

  panel <- expand.grid(id = seq_len(n_units), year = periods,
                       stringsAsFactors = FALSE)
  panel <- panel[order(panel$id, panel$year), ]
  rownames(panel) <- NULL

  panel$g     <- g_vec[panel$id]
  panel$treat <- as.integer(!is.na(panel$g) & panel$year >= panel$g)
  unit_fe     <- rnorm(n_units)[panel$id]
  time_fe     <- (panel$year - 2001L) * 0.2
  panel$y     <- unit_fe + time_fe + 2.0 * panel$treat +
                 rnorm(nrow(panel), sd = 0.5)
  panel
}

optim_data <- make_optim_data()

# ---------------------------------------------------------------------------
# Test 1 — optimised SA estimates match fixest::sunab() on small simulation
# ---------------------------------------------------------------------------
# fixest::sunab() is an independent SA(2021) implementation.
# Expected tolerance 1e-6 (same as CLAUDE.md agreement-test policy).
# This test must pass both before and after integrating the i() optimisation.
# ---------------------------------------------------------------------------
test_that("optimized SA matches fixest::sunab() on small simulation", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("broom")

  # Our SA implementation
  result <- run_es(
    data      = optim_data,
    outcome   = y,
    time      = year,
    timing    = g,
    unit      = id,
    fe        = ~ id + year,
    staggered = TRUE,
    estimator = "sa"
  )

  # Column contract
  expect_s3_class(result, "es_result")
  required <- c("term", "estimate", "std.error", "statistic", "p.value",
                "relative_time", "is_baseline", "conf_low_95", "conf_high_95")
  expect_true(all(required %in% names(result)))

  non_base <- result[!result$is_baseline, ]
  expect_true(all(is.finite(non_base$estimate)))
  expect_true(all(non_base$std.error > 0))
  expect_equal(sum(result$is_baseline), 1L)

  # CATT attribute preserved
  catt <- attr(result, "catt_df")
  expect_true(is.data.frame(catt))
  expect_true(all(c("g", "l", "estimate", "std_error") %in% names(catt)))
  expect_gt(nrow(catt), 0L)

  # Reference: fixest::sunab() — encode never-treated as Inf (same as test-sa.R)
  optim_ref       <- optim_data
  optim_ref$g_ref <- ifelse(is.na(optim_data$g), Inf, optim_data$g)

  sunab_fn  <- utils::getFromNamespace("sunab", "fixest")
  ref_fml   <- stats::as.formula("y ~ sunab(g_ref, year) | id + year")
  fml_env   <- new.env(parent = environment(ref_fml))
  fml_env$sunab <- sunab_fn
  environment(ref_fml) <- fml_env

  ref_model <- fixest::feols(ref_fml, data = optim_ref, vcov = "HC1")
  ref_tidy  <- broom::tidy(ref_model)
  ref_rel   <- suppressWarnings(
    as.integer(gsub(".*::(-?\\d+)$", "\\1", ref_tidy$term))
  )
  ref_tidy  <- ref_tidy[!is.na(ref_rel), ]
  ref_rel   <- ref_rel[!is.na(ref_rel)]

  common_l    <- intersect(result$relative_time[!result$is_baseline], ref_rel)
  expect_gt(length(common_l), 0L)

  res_aligned <- result[result$relative_time %in% common_l & !result$is_baseline, ]
  res_aligned <- res_aligned[order(res_aligned$relative_time), ]

  ord         <- order(ref_rel[ref_rel %in% common_l])
  ref_aligned <- ref_tidy$estimate[ref_rel %in% common_l][ord]

  # IW estimates agree with sunab() at tolerance 1e-6
  expect_equal(res_aligned$estimate, ref_aligned, tolerance = 1e-6)

  # Cohort-share weights: at l = +2, all three cohorts g+2 in [2001,2010]
  # => theta = simple mean of CATT(2004,+2), CATT(2006,+2), CATT(2008,+2)
  catt_l2    <- catt[catt$l == 2L, ]
  if (nrow(catt_l2) == 3L) {
    theta_manual <- mean(catt_l2$estimate)  # equal sizes => simple mean
    theta_result <- result$estimate[result$relative_time == 2L &
                                    !result$is_baseline]
    expect_equal(theta_result, theta_manual, tolerance = 1e-12)
  }
})

# ---------------------------------------------------------------------------
# Test 2 — optimised SA on mpdta matches fixest::sunab()
# ---------------------------------------------------------------------------
test_that("optimized SA on mpdta matches fixest::sunab() at tolerance 1e-6", {
  skip_if_not_installed("did")
  skip_if_not_installed("fixest")
  skip_if_not_installed("broom")

  data(mpdta, package = "did")
  mpdta$timing <- ifelse(mpdta$first.treat == 0L, NA_real_, mpdta$first.treat)

  result <- run_es(
    data      = mpdta,
    outcome   = lemp,
    time      = year,
    timing    = timing,
    unit      = countyreal,
    fe        = ~ countyreal + year,
    staggered = TRUE,
    estimator = "sa"
  )

  # sunab() reference — encode never-treated as Inf
  mpdta$g_ref <- ifelse(is.na(mpdta$timing), Inf, mpdta$timing)

  sunab_fn  <- utils::getFromNamespace("sunab", "fixest")
  ref_fml   <- stats::as.formula("lemp ~ sunab(g_ref, year) | countyreal + year")
  fml_env   <- new.env(parent = environment(ref_fml))
  fml_env$sunab <- sunab_fn
  environment(ref_fml) <- fml_env

  ref_model <- fixest::feols(ref_fml, data = mpdta, vcov = "HC1")
  ref_tidy  <- broom::tidy(ref_model)
  ref_rel   <- suppressWarnings(
    as.integer(gsub(".*::(-?\\d+)$", "\\1", ref_tidy$term))
  )
  ref_tidy  <- ref_tidy[!is.na(ref_rel), ]
  ref_rel   <- ref_rel[!is.na(ref_rel)]

  common_l    <- intersect(result$relative_time[!result$is_baseline], ref_rel)
  expect_gt(length(common_l), 0L)

  res_aligned <- result[result$relative_time %in% common_l & !result$is_baseline, ]
  res_aligned <- res_aligned[order(res_aligned$relative_time), ]

  ord         <- order(ref_rel[ref_rel %in% common_l])
  ref_aligned <- ref_tidy$estimate[ref_rel %in% common_l][ord]

  expect_equal(res_aligned$estimate, ref_aligned, tolerance = 1e-6)
})
