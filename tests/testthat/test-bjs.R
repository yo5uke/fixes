# test-bjs.R
#
# Test-first (TDD) specification for the Borusyak-Jaravel-Spiess (2024)
# imputation estimator.
#
# Theory anchors (BJS 2024)
# -------------------------
# Theorem 2 (3-step imputation):
#   Step 1: Y_it = alpha_i + beta_t + eps_it  on Omega_0
#   Step 2: tau_hat_it = Y_it - Y_hat_it(0) = Y_it - alpha_hat_i - beta_hat_t  on Omega_1
#   Step 3: tau_hat_h  = mean(tau_hat_it | K_it = h)
#
# Omega_0 = never-treated units + not-yet-treated observations (t < G_i).
# Omega_1 = treated observations (G_i not NA and t >= G_i).

library(testthat)
library(fixes)

# ---------------------------------------------------------------------------
# Shared simulation fixture  (identical design to CS tests)
# ---------------------------------------------------------------------------
# 50 units | periods 1995-2005 | staggered adoption in 1998, 2000, 2002
# Never-treated: 5 units (g = NA).  True ATT = 1.5 (constant/homogeneous).
# DGP: unit FE + linear time trend + 1.5 * treat + N(0, 0.3)

make_bjs_data <- function(seed = 42L) {
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

bjs_data <- make_bjs_data()

# ---------------------------------------------------------------------------
# Test 1 — return type and column contract
# ---------------------------------------------------------------------------

test_that("bjs returns es_result object", {
  result <- run_es(
    data      = bjs_data,
    outcome   = y,
    time      = year,
    timing    = g,
    unit      = id,
    staggered = TRUE,
    estimator = "bjs"
  )

  expect_s3_class(result, "es_result")
  expect_true(is.data.frame(result))

  required_cols <- c(
    "term", "estimate", "std.error", "statistic", "p.value",
    "relative_time", "is_baseline",
    "conf_low_95", "conf_high_95"
  )
  expect_true(all(required_cols %in% names(result)))

  # Exactly one baseline row at relative_time == -1
  expect_equal(sum(result$is_baseline), 1L)
  expect_equal(result$estimate[result$is_baseline], 0)
  expect_equal(result$std.error[result$is_baseline], 0)

  # Post-treatment estimates should be finite
  post <- result[result$relative_time > 0 & !result$is_baseline, ]
  expect_true(all(is.finite(post$estimate)))

  # Metadata
  expect_equal(attr(result, "N_nevertreated"), 5L)
  expect_true(attr(result, "staggered"))
})

# ---------------------------------------------------------------------------
# Test 2 — numerical agreement with didimputation::did_imputation()
# ---------------------------------------------------------------------------
# Point estimates (tau_hat_h) should match to tolerance 1e-4.
# didimputation uses gname = 0 for never-treated (did-package convention).

test_that("bjs estimates are numerically close to didimputation::did_imputation()", {
  skip_if_not_installed("didimputation")

  # didimputation convention: g = 0 for never-treated.
  # horizon=TRUE is required to get event-study output; without it the function
  # returns a single row with term="treat" (static ATT only).
  bjs_data_ref      <- bjs_data
  bjs_data_ref$g    <- as.numeric(bjs_data_ref$g)
  bjs_data_ref$g[is.na(bjs_data_ref$g)] <- 0

  ref <- didimputation::did_imputation(
    data    = bjs_data_ref,
    yname   = "y",
    idname  = "id",
    tname   = "year",
    gname   = "g",
    horizon = TRUE
  )

  result <- run_es(
    data      = bjs_data,
    outcome   = y,
    time      = year,
    timing    = g,
    unit      = id,
    staggered = TRUE,
    estimator = "bjs"
  )

  # ref$term is character integers: "0", "1", "2", ...
  ref_l <- as.integer(ref$term)
  expect_gt(length(ref_l[!is.na(ref_l)]), 0L)

  common_l <- intersect(
    result$relative_time[!result$is_baseline],
    ref_l[!is.na(ref_l)]
  )
  expect_gt(length(common_l), 0L)

  res_aligned <- result[result$relative_time %in% common_l & !result$is_baseline, ]
  res_aligned <- res_aligned[order(res_aligned$relative_time), ]

  ref_aligned <- ref[!is.na(ref_l) & ref_l %in% common_l, ]
  ref_aligned <- ref_aligned[order(as.integer(ref_aligned$term)), ]

  expect_equal(res_aligned$estimate, ref_aligned$estimate, tolerance = 1e-4)
})

# ---------------------------------------------------------------------------
# Test 3 — real-data test using mpdta
# ---------------------------------------------------------------------------
# mpdta: minimum wage and teen employment, 500 counties, years 2003-2007.
# never-treated: first.treat == 0 (did convention) -> NA (bjs convention).
# Numerical comparison restricted to post-treatment horizons.

test_that("bjs on mpdta matches didimputation to tolerance 1e-4", {
  skip_if_not_installed("did")
  skip_if_not_installed("didimputation")
  data(mpdta, package = "did")

  mpdta_bjs <- mpdta
  mpdta_bjs$first.treat[mpdta_bjs$first.treat == 0] <- NA_real_

  res <- suppressWarnings(run_es(
    data      = mpdta_bjs,
    outcome   = lemp,
    time      = year,
    unit      = countyreal,
    timing    = first.treat,
    staggered = TRUE,
    estimator = "bjs"
  ))

  # Reference: didimputation with first.treat = 0 for never-treated.
  # horizon=TRUE gives one row per event-study horizon (term = character integer).
  ref <- didimputation::did_imputation(
    data    = mpdta,
    yname   = "lemp",
    idname  = "countyreal",
    tname   = "year",
    gname   = "first.treat",
    horizon = TRUE
  )

  ref_l  <- as.integer(ref$term)
  post_l <- ref_l[!is.na(ref_l) & ref_l >= 0]
  if (length(post_l) == 0L) skip("No post-treatment horizons in didimputation output")

  res_post <- res[res$relative_time %in% post_l & !res$is_baseline, ]
  res_post <- res_post[order(res_post$relative_time), ]

  ref_post <- ref[!is.na(ref_l) & ref_l %in% post_l, ]
  ref_post <- ref_post[order(as.integer(ref_post$term)), ]

  expect_equal(nrow(res_post), nrow(ref_post))
  expect_equal(res_post$estimate, ref_post$estimate, tolerance = 1e-4)
})

# ---------------------------------------------------------------------------
# Test 4 — informative error when no untreated observations exist
# ---------------------------------------------------------------------------

test_that("bjs errors informatively when no untreated observations exist", {
  # All units treated from the very first period -> Omega_0 is empty
  all_treated <- bjs_data
  all_treated$g <- 1995L   # everyone treated from period 1

  expect_error(
    run_es(
      data      = all_treated,
      outcome   = y,
      time      = year,
      timing    = g,
      unit      = id,
      staggered = TRUE,
      estimator = "bjs"
    ),
    regexp      = "untreated|Omega_0|never.treated|not.yet",
    ignore.case = TRUE
  )
})

# ---------------------------------------------------------------------------
# Test 5 — BJS and CS produce similar estimates under homogeneous ATT
# ---------------------------------------------------------------------------
# Under constant, homogeneous treatment effects both estimators are consistent
# for the same estimand; estimates should agree within tolerance 0.1.

test_that("bjs and cs produce similar estimates under homogeneous treatment effects",
{
  res_bjs <- run_es(
    data      = bjs_data,
    outcome   = y,
    time      = year,
    timing    = g,
    unit      = id,
    staggered = TRUE,
    estimator = "bjs"
  )

  res_cs <- run_es(
    data          = bjs_data,
    outcome       = y,
    time          = year,
    timing        = g,
    unit          = id,
    staggered     = TRUE,
    estimator     = "cs",
    control_group = "nevertreated"
  )

  # Compare post-treatment estimates at common horizons
  common_h <- intersect(
    res_bjs$relative_time[res_bjs$relative_time > 0 & !res_bjs$is_baseline],
    res_cs$relative_time[ res_cs$relative_time  > 0 & !res_cs$is_baseline]
  )
  expect_gt(length(common_h), 0L)

  bjs_est <- res_bjs$estimate[res_bjs$relative_time %in% common_h & !res_bjs$is_baseline]
  cs_est  <- res_cs$estimate[ res_cs$relative_time  %in% common_h & !res_cs$is_baseline]

  bjs_est <- bjs_est[order(res_bjs$relative_time[
    res_bjs$relative_time %in% common_h & !res_bjs$is_baseline])]
  cs_est  <- cs_est[order(res_cs$relative_time[
    res_cs$relative_time  %in% common_h & !res_cs$is_baseline])]

  expect_equal(bjs_est, cs_est, tolerance = 0.1)
})
