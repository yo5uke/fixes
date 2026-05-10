# test-cs-rcpp.R
#
# Verifies that compute_att_gt_cpp() produces bit-for-bit identical ATT(g,t)
# estimates to the pure-R nested-loop implementation inside .run_cs().
#
# This test must pass BEFORE integrating compute_att_gt_cpp() into .run_cs();
# after integration it becomes a regression guard.

library(testthat)
library(fixes)

# Reuse the same small simulation fixture from test-cs.R
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

# ---------------------------------------------------------------------------
# Helper: build the integer vectors expected by compute_att_gt_cpp()
# ---------------------------------------------------------------------------
prep_cpp_inputs <- function(dat, timing_col = "g", time_col = "year",
                             unit_col = "id", outcome_col = "y") {
  cohort_int <- as.integer(dat[[timing_col]])
  cohort_int[is.na(cohort_int)] <- 0L   # 0 = never-treated sentinel

  all_cohorts <- as.integer(sort(unique(cohort_int[cohort_int != 0L])))
  all_times   <- as.integer(sort(unique(dat[[time_col]])))

  list(
    unit_id       = as.integer(dat[[unit_col]]),
    time_id       = as.integer(dat[[time_col]]),
    outcome       = as.numeric(dat[[outcome_col]]),
    cohort        = cohort_int,
    cohorts       = all_cohorts,
    all_times     = all_times
  )
}

# ---------------------------------------------------------------------------
# Test: numerical agreement — nevertreated control group
# ---------------------------------------------------------------------------
test_that("compute_att_gt_cpp matches R loop output (nevertreated)", {
  dat <- make_cs_data(seed = 42L)

  # Reference: pure-R .run_cs() output (the nested-loop path)
  ref <- fixes:::.run_cs(
    data          = dat,
    outcome_chr   = "y",
    timing_chr    = "g",
    time_chr      = "year",
    unit_chr      = "id",
    anticipation  = 0L,
    control_group = "nevertreated"
  )
  ref_gt <- ref$att_gt[order(ref$att_gt$g, ref$att_gt$t), ]
  rownames(ref_gt) <- NULL

  # Rcpp path
  inp     <- prep_cpp_inputs(dat)
  cpp_out <- compute_att_gt_cpp(
    unit_id       = inp$unit_id,
    time_id       = inp$time_id,
    outcome       = inp$outcome,
    cohort        = inp$cohort,
    cohorts       = inp$cohorts,
    all_times     = inp$all_times,
    control_group = "nevertreated"
  )
  cpp_gt <- cpp_out[order(cpp_out$g, cpp_out$t), ]
  rownames(cpp_gt) <- NULL

  # Same number of (g, t) pairs
  expect_equal(nrow(cpp_gt), nrow(ref_gt))

  # Identical cohort and time indices
  expect_equal(cpp_gt$g, ref_gt$g)
  expect_equal(cpp_gt$t, ref_gt$t)

  # ATT estimates and SE must agree to floating-point precision
  expect_equal(cpp_gt$att, ref_gt$estimate,   tolerance = 1e-10)
  expect_equal(cpp_gt$se,  ref_gt$std_error,  tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# Test: numerical agreement — notyettreated control group
# ---------------------------------------------------------------------------
test_that("compute_att_gt_cpp matches R loop output (notyettreated)", {
  dat <- make_cs_data(seed = 7L)

  ref <- fixes:::.run_cs(
    data          = dat,
    outcome_chr   = "y",
    timing_chr    = "g",
    time_chr      = "year",
    unit_chr      = "id",
    anticipation  = 0L,
    control_group = "notyettreated"
  )
  ref_gt <- ref$att_gt[order(ref$att_gt$g, ref$att_gt$t), ]
  rownames(ref_gt) <- NULL

  inp     <- prep_cpp_inputs(dat)
  cpp_out <- compute_att_gt_cpp(
    unit_id       = inp$unit_id,
    time_id       = inp$time_id,
    outcome       = inp$outcome,
    cohort        = inp$cohort,
    cohorts       = inp$cohorts,
    all_times     = inp$all_times,
    control_group = "notyettreated"
  )
  cpp_gt <- cpp_out[order(cpp_out$g, cpp_out$t), ]
  rownames(cpp_gt) <- NULL

  expect_equal(nrow(cpp_gt), nrow(ref_gt))
  expect_equal(cpp_gt$g,   ref_gt$g)
  expect_equal(cpp_gt$t,   ref_gt$t)
  expect_equal(cpp_gt$att, ref_gt$estimate,  tolerance = 1e-10)
  expect_equal(cpp_gt$se,  ref_gt$std_error, tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# Test: edge case — single cohort, minimal panel (3 units, 4 periods)
# ---------------------------------------------------------------------------
test_that("compute_att_gt_cpp handles single-cohort minimal panel", {
  set.seed(99L)
  dat_small <- data.frame(
    id   = rep(1:5, each = 4L),
    year = rep(2000:2003, times = 5L),
    g    = c(rep(2002L, 3L * 4L), rep(NA_integer_, 2L * 4L)),
    stringsAsFactors = FALSE
  )
  dat_small$y <- rnorm(nrow(dat_small))

  ref <- fixes:::.run_cs(
    data          = dat_small,
    outcome_chr   = "y",
    timing_chr    = "g",
    time_chr      = "year",
    unit_chr      = "id",
    anticipation  = 0L,
    control_group = "nevertreated"
  )
  ref_gt <- ref$att_gt[order(ref$att_gt$g, ref$att_gt$t), ]
  rownames(ref_gt) <- NULL

  cohort_int <- as.integer(dat_small$g)
  cohort_int[is.na(cohort_int)] <- 0L
  cpp_out <- compute_att_gt_cpp(
    unit_id       = as.integer(dat_small$id),
    time_id       = as.integer(dat_small$year),
    outcome       = dat_small$y,
    cohort        = cohort_int,
    cohorts       = as.integer(sort(unique(cohort_int[cohort_int != 0L]))),
    all_times     = as.integer(sort(unique(dat_small$year))),
    control_group = "nevertreated"
  )
  cpp_gt <- cpp_out[order(cpp_out$g, cpp_out$t), ]
  rownames(cpp_gt) <- NULL

  expect_equal(nrow(cpp_gt), nrow(ref_gt))
  expect_equal(cpp_gt$att, ref_gt$estimate,  tolerance = 1e-10)
  expect_equal(cpp_gt$se,  ref_gt$std_error, tolerance = 1e-10)
})
