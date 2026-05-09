# test-sa.R
#
# Tests for the Sun-Abraham (2021) interaction-weighted estimator.
# Simulation fixture is identical to test-cs.R (same seed, same DGP).
#
# SA (2021) key equations tested:
#   eq. (1): saturated CATT regression with cohort x rel-time interactions
#   eq. (4): cohort-share aggregation weights
#            w(g,l) = n_g / sum_{g': g'+l in sample} n_{g'}
#
# Numerical reference: fixest::sunab() with vcov = "HC1".

library(testthat)
library(fixes)

# ---------------------------------------------------------------------------
# Shared simulation fixture (identical to test-cs.R)
# ---------------------------------------------------------------------------
make_sa_data <- function(seed = 42L) {
  set.seed(seed)
  n_units <- 50L
  periods <- 1995:2005
  g_vec   <- c(rep(1998L, 15L), rep(2000L, 15L), rep(2002L, 15L),
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

sa_data <- make_sa_data()

# ---------------------------------------------------------------------------
# Test 1 — return type and column contract
# ---------------------------------------------------------------------------
test_that("sa estimator returns es_result object", {
  result <- run_es(
    data      = sa_data,
    outcome   = y,
    time      = year,
    timing    = g,
    unit      = id,
    fe        = ~ id + year,
    staggered = TRUE,
    estimator = "sa"
  )

  expect_s3_class(result, "es_result")
  expect_true(is.data.frame(result))

  required <- c("term", "estimate", "std.error", "statistic", "p.value",
                "relative_time", "is_baseline", "conf_low_95", "conf_high_95")
  expect_true(all(required %in% names(result)))

  non_base <- result[!result$is_baseline, ]
  expect_true(all(is.finite(non_base$estimate)))
  expect_true(all(non_base$std.error > 0))

  expect_equal(sum(result$is_baseline), 1L)
  expect_equal(result$estimate[result$is_baseline], 0)
  expect_equal(result$std.error[result$is_baseline], 0)

  # CATT matrix stored as attribute
  catt <- attr(result, "catt_df")
  expect_true(is.data.frame(catt))
  expect_true(all(c("g", "l", "estimate", "std_error") %in% names(catt)))
})

# ---------------------------------------------------------------------------
# Test 2 — numerical agreement with fixest::sunab()
# ---------------------------------------------------------------------------
# fixest::sunab() implements SA (2021) eq. 4 internally.
# For a balanced panel with never-treated units, ref.c = Inf (never-treated)
# and ref.p = -1 (baseline) reproduce the same IW aggregation as .run_sa().
# Tolerance: 1e-6 per CLAUDE.md testing policy.

test_that("sa IW estimates match fixest::sunab() at tolerance 1e-6", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("broom")

  # sunab() does not accept NA cohort; encode never-treated as Inf
  sa_data_ref       <- sa_data
  sa_data_ref$g_ref <- ifelse(is.na(sa_data$g), Inf, sa_data$g)

  sunab_fn  <- utils::getFromNamespace("sunab", "fixest")
  ref_fml   <- stats::as.formula("y ~ sunab(g_ref, year) | id + year")
  fml_env   <- new.env(parent = environment(ref_fml))
  fml_env$sunab <- sunab_fn
  environment(ref_fml) <- fml_env

  ref_model <- fixest::feols(ref_fml, data = sa_data_ref, vcov = "HC1")
  ref_tidy  <- broom::tidy(ref_model)

  # sunab term names look like "g_ref::year::-3"; extract the trailing integer
  ref_rel   <- suppressWarnings(
    as.integer(gsub(".*::(-?\\d+)$", "\\1", ref_tidy$term))
  )
  ref_tidy  <- ref_tidy[!is.na(ref_rel), ]
  ref_rel   <- ref_rel[!is.na(ref_rel)]

  result <- run_es(
    data      = sa_data,
    outcome   = y,
    time      = year,
    timing    = g,
    unit      = id,
    fe        = ~ id + year,
    staggered = TRUE,
    estimator = "sa"
  )

  common_l    <- intersect(result$relative_time[!result$is_baseline], ref_rel)
  expect_gt(length(common_l), 0L)

  res_aligned <- result[result$relative_time %in% common_l & !result$is_baseline, ]
  res_aligned <- res_aligned[order(res_aligned$relative_time), ]

  ord         <- order(ref_rel[ref_rel %in% common_l])
  ref_aligned <- ref_tidy$estimate[ref_rel %in% common_l][ord]

  expect_equal(res_aligned$estimate, ref_aligned, tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# Test 3 — cohort-share weights are applied (heterogeneous cohort coverage)
# ---------------------------------------------------------------------------
# At event time l = +5, only cohorts 1998 (g+5=2003) and 2000 (g+5=2005) are
# within the 1995-2005 sample; cohort 2002 (g+5=2007) is not.
# So theta_es(+5) must be the *weighted average* of CATT(1998,+5) and
# CATT(2000,+5) with equal weights (15/30 each), NOT a simple mean of all three.

test_that("sa cohort-share weights exclude out-of-sample cohorts", {
  result <- run_es(
    data      = sa_data,
    outcome   = y,
    time      = year,
    timing    = g,
    unit      = id,
    fe        = ~ id + year,
    staggered = TRUE,
    estimator = "sa"
  )

  catt <- attr(result, "catt_df")

  # Verify l = +5 exists and comes from cohorts 1998 and 2000 only
  catt_l5 <- catt[catt$l == 5L, ]
  expect_setequal(catt_l5$g, c(1998L, 2000L))

  # Manually replicate the weight formula: n_g = 15 each, total = 30
  theta_manual <- mean(catt_l5$estimate)   # equal weights => simple mean
  theta_result <- result$estimate[result$relative_time == 5L &
                                  !result$is_baseline]
  expect_equal(theta_result, theta_manual, tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# Test 4 — informative error when timing column is missing
# ---------------------------------------------------------------------------
test_that("sa estimator errors informatively when timing column is missing", {
  bad        <- sa_data
  bad$g      <- NULL

  expect_error(
    run_es(
      data      = bad,
      outcome   = y,
      time      = year,
      timing    = g,
      unit      = id,
      fe        = ~ id + year,
      staggered = TRUE,
      estimator = "sa"
    ),
    regexp      = "g|timing|column|not found",
    ignore.case = TRUE
  )
})
