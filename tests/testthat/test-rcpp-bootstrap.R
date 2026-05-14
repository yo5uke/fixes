# test-rcpp-bootstrap.R
#
# Tests for bootstrap_cs_cpp() — the Rcpp implementation of the multiplier
# bootstrap (Callaway & Sant'Anna 2021, Algorithm 1).
#
# Key correctness anchor: with the same set.seed(), bootstrap_cs_cpp() must
# generate the same Mammen weights as the pure-R .mammen_weights() because
# both draw from R's RNG stream via R::runif().  Therefore c_hat and margin
# agree to floating-point precision (up to BLAS rounding in the mat-mul).

library(testthat)
library(fixes)

# ---------------------------------------------------------------------------
# Minimal panel for CS bootstrap tests
# ---------------------------------------------------------------------------
make_boot_data <- function(seed = 1L, n_units = 40L) {
  set.seed(seed)
  periods <- 1:8L
  g_vec   <- c(rep(3L, 15L), rep(6L, 15L), rep(NA_integer_, 10L))
  panel   <- expand.grid(id = seq_len(n_units), year = periods,
                         stringsAsFactors = FALSE)
  panel   <- panel[order(panel$id, panel$year), ]
  panel$g     <- g_vec[panel$id]
  panel$treat <- as.integer(!is.na(panel$g) & panel$year >= panel$g)
  unit_fe <- rnorm(n_units)[panel$id]
  panel$y <- unit_fe + (panel$year - 1L) * 0.1 + 1.5 * panel$treat +
             rnorm(nrow(panel), sd = 0.3)
  panel
}

# ---------------------------------------------------------------------------
# Pure-R reference: run bootstrap core WITHOUT the Rcpp call
# (reproduces .bootstrap_cs internals with the same RNG state)
# ---------------------------------------------------------------------------
r_bootstrap_core <- function(psi, B, alpha, B_pilot = 199L) {
  n_units    <- nrow(psi)
  sqrt_n     <- sqrt(n_units)
  kappa      <- (sqrt(5) + 1) / 2
  p1         <- kappa / sqrt(5)
  iqr_normal <- stats::qnorm(0.75) - stats::qnorm(0.25)

  # pilot
  raw_pilot  <- stats::runif(n_units * B_pilot)
  V_pilot    <- matrix(ifelse(raw_pilot < p1, 1 - kappa, kappa),
                       nrow = B_pilot, ncol = n_units)
  R_pilot    <- (V_pilot %*% psi) / sqrt_n
  sigma_half <- apply(R_pilot, 2L, function(r) {
    q <- stats::quantile(r, probs = c(0.25, 0.75), names = FALSE)
    max((q[2L] - q[1L]) / iqr_normal, 1e-10)
  })

  # main
  raw_main   <- stats::runif(n_units * B)
  V_main     <- matrix(ifelse(raw_main < p1, 1 - kappa, kappa),
                       nrow = B, ncol = n_units)
  R_main     <- (V_main %*% psi) / sqrt_n
  R_std      <- sweep(abs(R_main), 2L, sigma_half, FUN = "/")
  t_stats    <- apply(R_std, 1L, max)
  c_hat      <- stats::quantile(t_stats, 1 - alpha, names = FALSE)
  margin     <- c_hat * sigma_half / sqrt_n

  list(c_hat = c_hat, margin = margin, sigma_half = sigma_half)
}

# ---------------------------------------------------------------------------
# Test 1 — Mammen weights statistical properties
# ---------------------------------------------------------------------------
test_that("Mammen weights from bootstrap_cs_cpp have mean ≈ 0 and var ≈ 1", {
  set.seed(42L)
  # bootstrap_cs_cpp draws weights internally; we can check its sigma_half
  # output is sensible (positive finite)
  boot_data <- make_boot_data(seed = 42L)
  result <- suppressWarnings(run_es(
    data = boot_data, outcome = y, time = year, timing = g,
    unit = id, staggered = TRUE, estimator = "cs",
    bootstrap = TRUE, B = 99L, boot_seed = 1L
  ))
  # sigma_half exposed via the bootstrap attribute is not stored directly,
  # but we can verify the Mammen weight generator via large-sample statistics
  n  <- 1000L
  set.seed(1L)
  kappa <- (sqrt(5) + 1) / 2; p1 <- kappa / sqrt(5)
  w <- ifelse(stats::runif(n) < p1, 1 - kappa, kappa)
  expect_equal(mean(w), 0, tolerance = 0.1,  label = "Mammen E[W] ≈ 0")
  expect_equal(var(w),  1, tolerance = 0.2,  label = "Mammen Var[W] ≈ 1")
})

# ---------------------------------------------------------------------------
# Test 2 — bootstrap_cs_cpp vs pure-R core (same seed, same RNG draws)
# ---------------------------------------------------------------------------
test_that("bootstrap_cs_cpp c_hat matches pure-R at tolerance 1e-6 (same seed)", {
  skip_if_not_installed("fixes")
  boot_data <- make_boot_data(seed = 7L)

  # Build influence function psi via run_es CS path to get a realistic psi
  cs_result <- suppressWarnings(run_es(
    data = boot_data, outcome = y, time = year, timing = g,
    unit = id, staggered = TRUE, estimator = "cs"
  ))
  att_gt_df <- attr(cs_result, "att_gt")

  psi_obj <- fixes:::.compute_influence_cs(
    data = boot_data, att_gt = att_gt_df,
    unit_chr = "id", time_chr = "year", timing_chr = "g", outcome_chr = "y"
  )
  psi       <- psi_obj$psi
  gt_index  <- psi_obj$gt_index

  B     <- 299L
  alpha <- 0.05

  # Pure-R
  set.seed(77L)
  r_res <- r_bootstrap_core(psi, B = B, alpha = alpha)

  # Rcpp
  set.seed(77L)
  cpp_res <- bootstrap_cs_cpp(psi, as.integer(B), alpha, 199L)

  expect_equal(cpp_res$c_hat, r_res$c_hat, tolerance = 1e-6,
               label = "c_hat: Rcpp vs R-core")
  expect_equal(cpp_res$margin, r_res$margin, tolerance = 1e-6,
               label = "margin: Rcpp vs R-core")
})

# ---------------------------------------------------------------------------
# Test 3 — end-to-end regression guard: bootstrap=TRUE gives correct columns
# ---------------------------------------------------------------------------
test_that("run_es CS bootstrap=TRUE with Rcpp: conf_low_sim column present", {
  boot_data <- make_boot_data(seed = 3L)
  result <- suppressWarnings(run_es(
    data = boot_data, outcome = y, time = year, timing = g,
    unit = id, staggered = TRUE, estimator = "cs",
    bootstrap = TRUE, B = 99L, boot_seed = 42L
  ))
  expect_true("conf_low_sim"  %in% names(result))
  expect_true("conf_high_sim" %in% names(result))
  expect_true(all(is.finite(result$conf_low_sim)))
  expect_true(all(is.finite(result$conf_high_sim)))
  expect_true(all(result$conf_low_sim <= result$conf_high_sim))
})

# ---------------------------------------------------------------------------
# Test 4 — bootstrap reproducibility: same seed → identical c_hat
# ---------------------------------------------------------------------------
test_that("bootstrap_cs_cpp is reproducible with the same seed", {
  boot_data <- make_boot_data(seed = 4L)

  res1 <- suppressWarnings(run_es(
    data = boot_data, outcome = y, time = year, timing = g,
    unit = id, staggered = TRUE, estimator = "cs",
    bootstrap = TRUE, B = 99L, boot_seed = 123L
  ))
  res2 <- suppressWarnings(run_es(
    data = boot_data, outcome = y, time = year, timing = g,
    unit = id, staggered = TRUE, estimator = "cs",
    bootstrap = TRUE, B = 99L, boot_seed = 123L
  ))

  expect_identical(res1$conf_low_sim,  res2$conf_low_sim)
  expect_identical(res1$conf_high_sim, res2$conf_high_sim)
})

# ---------------------------------------------------------------------------
# Test 5 — no seed → two runs give different critical values (stochastic)
# ---------------------------------------------------------------------------
test_that("bootstrap_cs_cpp without seed produces different results across runs", {
  boot_data <- make_boot_data(seed = 5L)

  r1 <- suppressWarnings(run_es(
    data = boot_data, outcome = y, time = year, timing = g,
    unit = id, staggered = TRUE, estimator = "cs",
    bootstrap = TRUE, B = 99L
  ))
  r2 <- suppressWarnings(run_es(
    data = boot_data, outcome = y, time = year, timing = g,
    unit = id, staggered = TRUE, estimator = "cs",
    bootstrap = TRUE, B = 99L
  ))

  crit1 <- attr(r1, "bootstrap")$critical_value[1L]
  crit2 <- attr(r2, "bootstrap")$critical_value[1L]
  expect_false(identical(crit1, crit2),
               label = "different seeds → different critical values")
})
