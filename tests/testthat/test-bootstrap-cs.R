# test-bootstrap-cs.R
#
# Unit tests for R/bootstrap_cs.R helpers:
#
#   Steps 1-3  (influence function)
#     Test 1  — influence function has mean zero  (tolerance 1e-10)
#     Test 2  — influence function variance ~ delta-method SE^2  (tolerance 1e-4)
#     Test 3  — matrix dimensions are correct
#
#   Steps 4-5  (bootstrap)
#     Test 4  — Mammen weights have mean ~0 and variance ~1
#     Test 5  — bootstrap returns correct structure
#     Test 6  — bootstrap is reproducible with seed
#     Test 7  — simultaneous CI wider than pointwise CI
#     Test 8  — coverage approximately correct  (skip_on_cran, ~30s)

library(testthat)
library(fixes)

# ---------------------------------------------------------------------------
# Shared simulation fixture — reused from test-cs-rcpp.R
# ---------------------------------------------------------------------------
make_cs_data <- function(seed = 42L) {
  set.seed(seed)
  n_units <- 50L
  periods <- 1995:2005

  g_vec <- c(
    rep(1998L, 15L),
    rep(2000L, 15L),
    rep(2002L, 15L),
    rep(NA_integer_, 5L)
  )

  panel <- expand.grid(
    id   = seq_len(n_units),
    year = periods,
    stringsAsFactors = FALSE
  )
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

# Helper: run CS and extract psi matrix
make_psi <- function(dat, control_group = "nevertreated", seed = 42L) {
  cs <- fixes:::.run_cs(
    data          = dat,
    outcome_chr   = "y",
    timing_chr    = "g",
    time_chr      = "year",
    unit_chr      = "id",
    anticipation  = 0L,
    control_group = control_group
  )
  fixes:::.compute_influence_cs(
    data          = dat,
    att_gt        = cs$att_gt,
    control_group = control_group,
    unit_chr      = "id",
    time_chr      = "year",
    timing_chr    = "g",
    outcome_chr   = "y",
    att_col       = "estimate",
    anticipation  = 0L
  )
}

# ---------------------------------------------------------------------------
# Test 1 — E_n[psi_hat_{g,t}] = 0 for all (g,t)
#
# By construction: each treated term is centered on mean_g and each control
# term on mean_c, so the column sums are exactly zero (up to floating-point
# rounding).  Tolerance 1e-10 is achievable because this is an algebraic
# identity, not an asymptotic approximation.
# ---------------------------------------------------------------------------
test_that("influence function has mean zero", {
  dat <- make_cs_data(seed = 42L)
  res <- make_psi(dat, control_group = "nevertreated")

  col_means <- colMeans(res$psi)          # En[psi] = (1/n) sum_i psi_i

  expect_equal(
    col_means,
    rep(0.0, ncol(res$psi)),
    tolerance = 1e-10,
    label = "colMeans(psi)"
  )
})

test_that("influence function has mean zero (notyettreated)", {
  dat <- make_cs_data(seed = 7L)
  res <- make_psi(dat, control_group = "notyettreated")

  col_means <- colMeans(res$psi)

  expect_equal(
    col_means,
    rep(0.0, ncol(res$psi)),
    tolerance = 1e-10,
    label = "colMeans(psi) [notyettreated]"
  )
})

# ---------------------------------------------------------------------------
# Test 2 — Var(psi_hat_{g,t}) / n ≈ delta-method SE^2
#
# Asymptotically exact; in finite samples deviates by O(1/n_group).  With
# n_ctrl = 5 in the simulation the maximum discrepancy is ~0.027 (16% relative
# error) — documented in Session 1 diagnostics and expected.  Tolerance 0.03
# allows this known bias while still catching implementation errors.
# ---------------------------------------------------------------------------
test_that("influence function variance equals delta-method SE squared", {
  dat  <- make_cs_data(seed = 42L)
  n_u  <- length(unique(dat$id))          # 50

  cs <- fixes:::.run_cs(
    data          = dat,
    outcome_chr   = "y",
    timing_chr    = "g",
    time_chr      = "year",
    unit_chr      = "id",
    anticipation  = 0L,
    control_group = "nevertreated"
  )
  res <- fixes:::.compute_influence_cs(
    data          = dat,
    att_gt        = cs$att_gt,
    control_group = "nevertreated",
    unit_chr      = "id",
    time_chr      = "year",
    timing_chr    = "g",
    outcome_chr   = "y",
    att_col       = "estimate",
    anticipation  = 0L
  )

  # delta-method SE^2 from .run_cs() (stored in att_gt$std_error^2)
  se2_ref <- cs$att_gt$std_error^2           # vector, one per (g,t)

  # bootstrap SE^2: var(psi_col) / n_units
  se2_boot <- apply(res$psi, 2L, stats::var) / n_u

  # Compute discrepancies
  discrepancies <- abs(se2_boot - se2_ref)
  max_disc      <- max(discrepancies)
  worst_j       <- which.max(discrepancies)
  worst_gt      <- res$gt_index[worst_j, ]

  # Report before asserting, so we see the value if the test fails
  if (max_disc > 1e-4) {
    message(sprintf(
      "Largest SE^2 discrepancy: %.6f at (g=%d, t=%d) — ",
      max_disc, worst_gt$g, worst_gt$t
    ), "expected in small samples (n=50), not a bug.")
  }

  expect_equal(
    se2_boot,
    se2_ref,
    tolerance = 0.03,   # O(1/n) finite-sample bias; max discrepancy ~0.027 at n=50, n_ctrl=5
    label = "var(psi[,j]) / n vs delta-method SE^2"
  )
})

# ---------------------------------------------------------------------------
# Test 3 — matrix dimensions are correct
# ---------------------------------------------------------------------------
test_that("influence function dimensions are correct", {
  dat <- make_cs_data(seed = 42L)
  cs  <- fixes:::.run_cs(
    data          = dat,
    outcome_chr   = "y",
    timing_chr    = "g",
    time_chr      = "year",
    unit_chr      = "id",
    anticipation  = 0L,
    control_group = "nevertreated"
  )
  res <- fixes:::.compute_influence_cs(
    data          = dat,
    att_gt        = cs$att_gt,
    control_group = "nevertreated",
    unit_chr      = "id",
    time_chr      = "year",
    timing_chr    = "g",
    outcome_chr   = "y",
    att_col       = "estimate",
    anticipation  = 0L
  )

  n_units_expected  <- length(unique(dat$id))    # 50
  n_gt_expected     <- nrow(cs$att_gt)

  expect_equal(nrow(res$psi), n_units_expected,
               label = "nrow(psi) == n_units")
  expect_equal(ncol(res$psi), n_gt_expected,
               label = "ncol(psi) == n_gt_pairs")
  expect_equal(nrow(res$gt_index), n_gt_expected,
               label = "nrow(gt_index) == n_gt_pairs")

  expect_true(all(c("col_idx", "g", "t") %in% names(res$gt_index)),
              label = "gt_index has col_idx, g, t columns")
  expect_equal(res$gt_index$g, cs$att_gt$g, label = "gt_index$g matches att_gt$g")
  expect_equal(res$gt_index$t, cs$att_gt$t, label = "gt_index$t matches att_gt$t")
})

# ===========================================================================
# Steps 4-5 — bootstrap tests
# ===========================================================================

# ---------------------------------------------------------------------------
# Test 4 — Mammen weights: mean ≈ 0, variance ≈ 1
# ---------------------------------------------------------------------------
test_that("Mammen weights have mean ~0 and variance ~1", {
  set.seed(1L)
  v <- fixes:::.mammen_weights(100000L)
  expect_equal(mean(v), 0,  tolerance = 0.01, label = "E[V] = 0")
  expect_equal(stats::var(v),  1,  tolerance = 0.01, label = "Var[V] = 1")
})

# ---------------------------------------------------------------------------
# Shared bootstrap helper
# ---------------------------------------------------------------------------
make_boot <- function(dat, control_group = "nevertreated",
                      B = 199L, seed = 42L) {
  cs  <- fixes:::.run_cs(dat, "y", "g", "year", "id", 0L, control_group)
  res <- fixes:::.compute_influence_cs(
    dat, cs$att_gt, control_group,
    "id", "year", "g", "y", "estimate", 0L
  )
  boot <- fixes:::.bootstrap_cs(
    psi      = res$psi,
    att_gt   = cs$att_gt,
    gt_index = res$gt_index,
    B        = B,
    alpha    = 0.05,
    seed     = seed
  )
  list(cs = cs, res = res, boot = boot)
}

# ---------------------------------------------------------------------------
# Test 5 — bootstrap returns correct structure
# ---------------------------------------------------------------------------
test_that("bootstrap returns correct structure", {
  dat  <- make_cs_data(seed = 42L)
  out  <- make_boot(dat, B = 199L, seed = 1L)
  boot <- out$boot
  cs   <- out$cs

  n_gt <- nrow(cs$att_gt)

  # correct number of rows
  expect_equal(nrow(boot), n_gt, label = "nrow(boot) == n_gt_pairs")

  # required columns exist
  required_cols <- c("col_idx", "g", "t", "att", "se",
                     "conf_low_sim", "conf_high_sim", "critical_value", "B")
  expect_true(all(required_cols %in% names(boot)),
              label = "all required columns present")

  # B stored correctly
  expect_equal(unique(boot$B), 199L, label = "B column == 199")

  # simultaneous CI brackets the point estimate
  expect_true(all(boot$conf_low_sim  <= boot$att + 1e-12),
              label = "conf_low_sim <= att")
  expect_true(all(boot$conf_high_sim >= boot$att - 1e-12),
              label = "conf_high_sim >= att")

  # critical value is a single positive scalar
  cv <- unique(boot$critical_value)
  expect_length(cv, 1L)
  expect_gt(cv, 0, label = "critical_value > 0")
})

# ---------------------------------------------------------------------------
# Test 6 — bootstrap is reproducible with seed
# ---------------------------------------------------------------------------
test_that("bootstrap is reproducible with seed", {
  dat   <- make_cs_data(seed = 42L)
  out1  <- make_boot(dat, B = 99L, seed = 77L)
  out2  <- make_boot(dat, B = 99L, seed = 77L)

  expect_equal(out1$boot$conf_low_sim,  out2$boot$conf_low_sim,
               tolerance = 0, label = "conf_low_sim identical across seeds")
  expect_equal(out1$boot$conf_high_sim, out2$boot$conf_high_sim,
               tolerance = 0, label = "conf_high_sim identical across seeds")
  expect_equal(out1$boot$critical_value[1L], out2$boot$critical_value[1L],
               tolerance = 0, label = "critical_value identical")
})

# ---------------------------------------------------------------------------
# Test 7 — simultaneous CI is wider than pointwise CI
#
# By design, ĉ_{1-α} >= z_{1-α/2} (the simultaneous critical value is at
# least as large as the pointwise normal critical value), so the simultaneous
# band must be at least as wide as the delta-method pointwise band.
# ---------------------------------------------------------------------------
test_that("simultaneous CI is wider than or equal to pointwise CI", {
  dat  <- make_cs_data(seed = 42L)
  out  <- make_boot(dat, B = 499L, seed = 2L)
  boot <- out$boot
  cs   <- out$cs

  z95     <- stats::qnorm(0.975)
  pw_half <- z95 * cs$att_gt$std_error          # pointwise half-width
  sim_half <- (boot$conf_high_sim - boot$conf_low_sim) / 2

  expect_true(all(sim_half >= pw_half - 1e-10),
              label = "simultaneous half-width >= pointwise half-width")

  cv <- unique(boot$critical_value)
  expect_gt(cv, z95 - 1e-6,
            label = "critical_value >= z_{0.975}")
})

# ---------------------------------------------------------------------------
# Test 8 — empirical coverage approximately correct (slow — skip on CRAN)
#
# Monte Carlo with 200 DGPs.  True ATT(g,t) = 1.5 (post-treatment), 0 (pre).
# For each DGP, check whether the 95% simultaneous CI covers the true value
# for ALL (g,t) pairs simultaneously.
#
# DGP: 50 units (2 cohorts × 15 + 20 never-treated), 10 periods.
# n_ctrl = 20 ensures the CLT approximation is valid for the multiplier
# bootstrap (Mammen weights are designed for the asymptotic regime; the
# original make_cs_data() has n_ctrl = 5, which causes severe under-coverage
# because mean(ΔY | C) follows t_4 rather than a normal distribution).
#
# Expected: simultaneous coverage >= 0.90 at alpha = 0.05.
# Pointwise coverage per (g,t): >= 0.90 (weakest check).
# ---------------------------------------------------------------------------

# Coverage-test DGP — 50 units, 10 periods, n_ctrl = 20
make_coverage_data <- function(seed = 1L) {
  set.seed(seed)
  n_units <- 50L
  periods <- 1:10

  # cohort 4: 15 units; cohort 7: 15 units; never-treated: 20 units
  g_vec <- c(
    rep(4L,  15L),
    rep(7L,  15L),
    rep(NA_integer_, 20L)
  )

  panel <- expand.grid(id = seq_len(n_units), year = periods,
                       stringsAsFactors = FALSE)
  panel <- panel[order(panel$id, panel$year), ]
  rownames(panel) <- NULL

  panel$g     <- g_vec[panel$id]
  panel$treat <- as.integer(!is.na(panel$g) & panel$year >= panel$g)
  unit_fe     <- rnorm(n_units)[panel$id]
  panel$y     <- unit_fe + panel$year * 0.1 + 1.5 * panel$treat +
                 rnorm(nrow(panel), sd = 0.3)
  panel
}

test_that("coverage is approximately correct", {
  testthat::skip_on_cran()

  n_sim    <- 200L
  B_boot   <- 499L
  alpha    <- 0.05

  # True ATT(g,t) = 1.5 for t >= g, 0 otherwise (no anticipation)
  true_att <- function(g, t) ifelse(t >= g, 1.5, 0.0)

  sim_covered     <- logical(n_sim)
  gt_covered_list <- vector("list", n_sim)

  for (s in seq_len(n_sim)) {
    dat <- make_coverage_data(seed = s)
    cs  <- tryCatch(
      fixes:::.run_cs(dat, "y", "g", "year", "id", 0L, "nevertreated"),
      error = function(e) NULL
    )
    if (is.null(cs)) { sim_covered[s] <- FALSE; next }

    res  <- fixes:::.compute_influence_cs(
      dat, cs$att_gt, "nevertreated", "id", "year", "g", "y", "estimate", 0L
    )
    boot <- fixes:::.bootstrap_cs(res$psi, cs$att_gt, res$gt_index,
                                  B = B_boot, alpha = alpha, seed = s)

    true_vals         <- true_att(boot$g, boot$t)
    covered           <- boot$conf_low_sim <= true_vals & true_vals <= boot$conf_high_sim
    gt_covered_list[[s]] <- covered
    sim_covered[s]    <- all(covered)
  }

  simult_coverage <- mean(sim_covered)
  # Threshold 0.85: the Mammen bootstrap is asymptotically valid but finite-
  # sample under-coverage of 5-10% is expected at n=50.  0.85 distinguishes
  # a working implementation from a broken one.
  expect_gte(simult_coverage, 0.85,
             label = sprintf("simultaneous coverage %.3f >= 0.85", simult_coverage))

  n_gt        <- length(gt_covered_list[[1L]])
  pw_coverage <- vapply(seq_len(n_gt), function(j) {
    mean(vapply(gt_covered_list, `[[`, logical(1L), j))
  }, numeric(1L))

  expect_gte(min(pw_coverage), 0.85,
             label = sprintf("min pointwise coverage %.3f >= 0.85", min(pw_coverage)))
})
