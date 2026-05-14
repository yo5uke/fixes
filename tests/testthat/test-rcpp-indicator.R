# test-rcpp-indicator.R
#
# Numerical agreement between the pure-R indicator matrix construction
# (nested for-loop) and build_indicator_matrix_cpp().
#
# Correctness anchor: exact integer equality (not floating-point tolerance)
# since both implementations fill 0/1 integer values deterministically.

library(testthat)
library(fixes)

# ---------------------------------------------------------------------------
# Helper: pure-R indicator matrix (copied from estimators_sa.R logic)
# ---------------------------------------------------------------------------
r_indicator_matrix <- function(cohort_id, time_id, gs_g, gs_s) {
  N <- length(cohort_id)
  K <- length(gs_g)
  mat <- matrix(0L, nrow = N, ncol = K)
  for (k in seq_len(K)) {
    g <- gs_g[k]; s <- gs_s[k]
    mat[, k] <- as.integer(!is.na(cohort_id) & cohort_id == g & time_id == s)
  }
  mat
}

# ---------------------------------------------------------------------------
# Shared small DGP fixture
# ---------------------------------------------------------------------------
make_indicator_test_data <- function(seed = 1L) {
  set.seed(seed)
  n_units <- 30L; periods <- 1:8L
  g_vec   <- c(rep(3L, 10L), rep(5L, 10L), rep(NA_integer_, 10L))
  panel   <- expand.grid(id = seq_len(n_units), year = periods,
                         stringsAsFactors = FALSE)
  panel   <- panel[order(panel$id, panel$year), ]
  panel$g <- g_vec[panel$id]
  panel
}

# ---------------------------------------------------------------------------
# Test 1 — SA data: exact agreement
# ---------------------------------------------------------------------------
test_that("rcpp indicator matrix matches R loop for SA-style data", {
  panel <- make_indicator_test_data()

  cohort_id <- as.integer(panel$g)        # NA_integer_ for never-treated
  time_id   <- as.integer(panel$year)
  cohorts   <- sort(unique(cohort_id[!is.na(cohort_id)]))
  baseline  <- -1L
  all_t     <- sort(unique(time_id))

  gs_pairs <- do.call(rbind, lapply(cohorts, function(g) {
    excl <- g + baseline
    fs   <- all_t[all_t != excl]
    data.frame(g = g, s = fs)
  }))
  gs_g <- as.integer(gs_pairs$g)
  gs_s <- as.integer(gs_pairs$s)

  r_mat   <- r_indicator_matrix(cohort_id, time_id, gs_g, gs_s)
  cpp_mat <- build_indicator_matrix_cpp(cohort_id, time_id, gs_g, gs_s)

  expect_identical(r_mat, cpp_mat,
                   label = "SA indicator matrix: R loop == Rcpp")
})

# ---------------------------------------------------------------------------
# Test 2 — FLEX data: cohort_of_obs pre-computation then shared Rcpp call
# ---------------------------------------------------------------------------
test_that("rcpp indicator matrix matches R loop for FLEX-style data", {
  set.seed(2L)
  n_groups <- 6L; periods <- 1:6L; M <- 10L
  cohort_vals <- c(rep(2L, 2L), rep(4L, 2L), rep(NA_integer_, 2L))

  panel <- do.call(rbind, lapply(seq_len(n_groups), function(g) {
    do.call(rbind, lapply(periods, function(t) {
      data.frame(obs_id = seq_len(M) + (g-1)*M*max(periods) + (t-1)*M,
                 year = t, group_id = g, g = cohort_vals[g],
                 y = rnorm(M))
    }))
  }))
  panel <- panel[order(panel$group_id, panel$year, panel$obs_id), ]

  # FLEX: group → cohort mapping
  timing_by_group <- setNames(
    tapply(panel$g, panel$group_id, function(v) unique(v[!is.na(v)])[1]),
    as.character(sort(unique(panel$group_id)))
  )
  timing_by_group[sapply(timing_by_group, is.null)] <- NA_integer_
  timing_by_group <- unlist(timing_by_group)

  # cohort_of_obs: pre-computed in R before calling Rcpp (same as FLEX estimator)
  cohort_of_obs <- as.integer(timing_by_group[as.character(panel$group_id)])
  time_id       <- as.integer(panel$year)

  cohorts  <- sort(unique(cohort_of_obs[!is.na(cohort_of_obs)]))
  baseline <- -1L
  all_t    <- sort(unique(time_id))

  gs_pairs <- do.call(rbind, lapply(cohorts, function(g) {
    excl <- g + baseline
    fs   <- all_t[all_t != excl]
    data.frame(g = g, s = fs)
  }))
  gs_g <- as.integer(gs_pairs$g)
  gs_s <- as.integer(gs_pairs$s)

  r_mat   <- r_indicator_matrix(cohort_of_obs, time_id, gs_g, gs_s)
  cpp_mat <- build_indicator_matrix_cpp(cohort_of_obs, time_id, gs_g, gs_s)

  expect_identical(r_mat, cpp_mat,
                   label = "FLEX indicator matrix: R loop == Rcpp")
})

# ---------------------------------------------------------------------------
# Test 3 — edge cases: K=1, all-never-treated, all-treated
# ---------------------------------------------------------------------------
test_that("rcpp indicator matrix handles K=1 edge case", {
  cohort_id <- c(2L, 2L, NA_integer_, NA_integer_)
  time_id   <- c(1L, 2L, 1L, 2L)
  gs_g      <- 2L
  gs_s      <- 2L

  r_mat   <- r_indicator_matrix(cohort_id, time_id, gs_g, gs_s)
  cpp_mat <- build_indicator_matrix_cpp(cohort_id, time_id, gs_g, gs_s)
  expect_identical(r_mat, cpp_mat)
})

test_that("rcpp indicator matrix: all never-treated gives all-zero matrix", {
  cohort_id <- rep(NA_integer_, 6L)
  time_id   <- rep(1:3, 2L)
  gs_g      <- c(2L, 2L)
  gs_s      <- c(2L, 3L)

  cpp_mat <- build_indicator_matrix_cpp(cohort_id, time_id, gs_g, gs_s)
  expect_true(all(cpp_mat == 0L))
})
