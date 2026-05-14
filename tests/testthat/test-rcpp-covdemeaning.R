# test-rcpp-covdemeaning.R
#
# Numerical agreement between the pure-R covariate demeaning loops and
# build_cov_interactions_cpp().
#
# Correctness anchor: tolerance = 1e-12 (identical floating-point inputs
# through the same arithmetic produce the same result).

library(testthat)
library(fixes)

# ---------------------------------------------------------------------------
# Pure-R reference implementations (mirrors estimators_twm.R / flex logic)
# ---------------------------------------------------------------------------

# TWM: centre by cohort (group_key = timing value per obs)
r_cov_int_twm <- function(cov_mat, ind_mat, timing_vec) {
  N       <- nrow(cov_mat)
  p       <- ncol(cov_mat)
  K       <- ncol(ind_mat)
  cohorts <- sort(unique(timing_vec[!is.na(timing_vec)]))

  x_centred   <- matrix(0.0, nrow = N, ncol = p)
  for (g in cohorts) {
    g_mask <- !is.na(timing_vec) & timing_vec == g
    for (j in seq_len(p))
      x_centred[g_mask, j] <- cov_mat[g_mask, j] - mean(cov_mat[g_mask, j])
  }

  out <- matrix(0.0, nrow = N, ncol = K * p)
  for (j in seq_len(p))
    for (k in seq_len(K))
      out[, (j - 1L) * K + k] <- ind_mat[, k] * x_centred[, j]
  out
}

# FLEX: centre by (cohort, time) cell
r_cov_int_flex <- function(cov_mat, ind_mat, cohort_of_obs, time_id,
                            cohorts, all_periods) {
  N <- nrow(cov_mat); p <- ncol(cov_mat); K <- ncol(ind_mat)

  x_centred <- matrix(0.0, nrow = N, ncol = p)
  for (g in cohorts) {
    g_rows <- which(!is.na(cohort_of_obs) & cohort_of_obs == g)
    for (t_val in all_periods) {
      cell <- g_rows[time_id[g_rows] == t_val]
      if (length(cell) == 0L) next
      for (j in seq_len(p))
        x_centred[cell, j] <- cov_mat[cell, j] - mean(cov_mat[cell, j])
    }
  }

  out <- matrix(0.0, nrow = N, ncol = K * p)
  for (j in seq_len(p))
    for (k in seq_len(K))
      out[, (j - 1L) * K + k] <- ind_mat[, k] * x_centred[, j]
  out
}

# ---------------------------------------------------------------------------
# Shared small panel fixture
# ---------------------------------------------------------------------------
make_covdm_data <- function(seed = 1L, p = 1L) {
  set.seed(seed)
  n_units <- 20L; periods <- 1:6L
  g_vec   <- c(rep(3L, 8L), rep(5L, 7L), rep(NA_integer_, 5L))
  panel   <- expand.grid(id = seq_len(n_units), year = periods,
                         stringsAsFactors = FALSE)
  panel   <- panel[order(panel$id, panel$year), ]
  panel$g <- g_vec[panel$id]
  cov_cols <- setNames(
    as.data.frame(matrix(rnorm(nrow(panel) * p), nrow = nrow(panel))),
    paste0("x", seq_len(p))
  )
  cbind(panel, cov_cols)
}

# ---------------------------------------------------------------------------
# Test 1 — TWM, single covariate
# ---------------------------------------------------------------------------
test_that("build_cov_interactions_cpp matches R loop for TWM (p=1)", {
  df       <- make_covdm_data(seed = 1L, p = 1L)
  cov_mat  <- as.matrix(df[, "x1", drop = FALSE])
  timing   <- as.integer(df$g)
  cohorts  <- sort(unique(timing[!is.na(timing)]))
  baseline <- -1L
  all_t    <- sort(unique(df$year))

  gs_pairs <- do.call(rbind, lapply(cohorts, function(g) {
    excl <- g + baseline
    data.frame(g = g, s = all_t[all_t != excl])
  }))
  ind_mat <- build_indicator_matrix_cpp(timing, as.integer(df$year),
                                        as.integer(gs_pairs$g),
                                        as.integer(gs_pairs$s))

  r_out   <- r_cov_int_twm(cov_mat, ind_mat, timing)
  cpp_out <- build_cov_interactions_cpp(cov_mat, ind_mat, timing)

  expect_equal(r_out, cpp_out, tolerance = 1e-12,
               label = "TWM cov interaction: R loop == Rcpp (p=1)")
})

# ---------------------------------------------------------------------------
# Test 2 — TWM, multiple covariates (p=3)
# ---------------------------------------------------------------------------
test_that("build_cov_interactions_cpp matches R loop for TWM (p=3)", {
  df       <- make_covdm_data(seed = 2L, p = 3L)
  cov_mat  <- as.matrix(df[, paste0("x", 1:3)])
  timing   <- as.integer(df$g)
  cohorts  <- sort(unique(timing[!is.na(timing)]))
  baseline <- -1L; all_t <- sort(unique(df$year))

  gs_pairs <- do.call(rbind, lapply(cohorts, function(g) {
    data.frame(g = g, s = all_t[all_t != g + baseline])
  }))
  ind_mat <- build_indicator_matrix_cpp(timing, as.integer(df$year),
                                        as.integer(gs_pairs$g),
                                        as.integer(gs_pairs$s))

  r_out   <- r_cov_int_twm(cov_mat, ind_mat, timing)
  cpp_out <- build_cov_interactions_cpp(cov_mat, ind_mat, timing)

  expect_equal(r_out, cpp_out, tolerance = 1e-12,
               label = "TWM cov interaction: R loop == Rcpp (p=3)")
})

# ---------------------------------------------------------------------------
# Test 3 — FLEX, single covariate (cell-level centering)
# ---------------------------------------------------------------------------
make_flex_covdm <- function(seed = 3L, M = 15L, p = 1L) {
  set.seed(seed)
  n_groups    <- 6L
  cohort_vals <- c(rep(2L, 2L), rep(4L, 2L), rep(NA_integer_, 2L))
  periods     <- 1L:6L
  do.call(rbind, lapply(seq_len(n_groups), function(g) {
    do.call(rbind, lapply(periods, function(t) {
      base <- data.frame(year = t, group_id = g, g = cohort_vals[g],
                         stringsAsFactors = FALSE)
      cov_cols <- setNames(
        as.data.frame(matrix(rnorm(M * p), nrow = M)),
        paste0("x", seq_len(p))
      )
      cbind(base[rep(1, M), ], cov_cols, row.names = NULL)
    }))
  }))
}

test_that("build_cov_interactions_cpp matches R loop for FLEX (p=1)", {
  df <- make_flex_covdm(seed = 3L, M = 15L, p = 1L)
  df <- df[order(df$group_id, df$year), ]

  timing_by_group <- setNames(
    tapply(df$g, df$group_id, function(v) { u <- unique(v[!is.na(v)]); if (length(u)==0) NA_integer_ else u }),
    as.character(sort(unique(df$group_id)))
  )
  cohort_of_obs <- as.integer(unlist(timing_by_group)[as.character(df$group_id)])

  cohorts  <- sort(unique(cohort_of_obs[!is.na(cohort_of_obs)]))
  all_t    <- sort(unique(df$year))
  baseline <- -1L
  gs_pairs <- do.call(rbind, lapply(cohorts, function(g) {
    data.frame(g = g, s = all_t[all_t != g + baseline])
  }))
  ind_mat <- build_indicator_matrix_cpp(cohort_of_obs, as.integer(df$year),
                                        as.integer(gs_pairs$g),
                                        as.integer(gs_pairs$s))
  cov_mat <- as.matrix(df[, "x1", drop = FALSE])

  # FLEX cell_key for Rcpp
  cohort_chr <- ifelse(is.na(cohort_of_obs), "NA", as.character(cohort_of_obs))
  cell_key   <- as.integer(factor(paste(cohort_chr, as.character(df$year), sep="_")))
  cell_key[is.na(cohort_of_obs)] <- NA_integer_

  r_out   <- r_cov_int_flex(cov_mat, ind_mat, cohort_of_obs,
                             as.integer(df$year), cohorts, all_t)
  cpp_out <- build_cov_interactions_cpp(cov_mat, ind_mat, cell_key)

  expect_equal(r_out, cpp_out, tolerance = 1e-12,
               label = "FLEX cov interaction: R loop == Rcpp (p=1)")
})

# ---------------------------------------------------------------------------
# Test 4 — FLEX, multiple covariates (p=3)
# ---------------------------------------------------------------------------
test_that("build_cov_interactions_cpp matches R loop for FLEX (p=3)", {
  df <- make_flex_covdm(seed = 4L, M = 15L, p = 3L)
  df <- df[order(df$group_id, df$year), ]

  timing_by_group <- setNames(
    tapply(df$g, df$group_id, function(v) { u <- unique(v[!is.na(v)]); if (length(u)==0) NA_integer_ else u }),
    as.character(sort(unique(df$group_id)))
  )
  cohort_of_obs <- as.integer(unlist(timing_by_group)[as.character(df$group_id)])
  cohorts  <- sort(unique(cohort_of_obs[!is.na(cohort_of_obs)]))
  all_t    <- sort(unique(df$year))
  baseline <- -1L

  gs_pairs <- do.call(rbind, lapply(cohorts, function(g) {
    data.frame(g = g, s = all_t[all_t != g + baseline])
  }))
  ind_mat <- build_indicator_matrix_cpp(cohort_of_obs, as.integer(df$year),
                                        as.integer(gs_pairs$g),
                                        as.integer(gs_pairs$s))
  cov_mat <- as.matrix(df[, paste0("x", 1:3)])

  cohort_chr <- ifelse(is.na(cohort_of_obs), "NA", as.character(cohort_of_obs))
  cell_key   <- as.integer(factor(paste(cohort_chr, as.character(df$year), sep="_")))
  cell_key[is.na(cohort_of_obs)] <- NA_integer_

  r_out   <- r_cov_int_flex(cov_mat, ind_mat, cohort_of_obs,
                             as.integer(df$year), cohorts, all_t)
  cpp_out <- build_cov_interactions_cpp(cov_mat, ind_mat, cell_key)

  expect_equal(r_out, cpp_out, tolerance = 1e-12,
               label = "FLEX cov interaction: R loop == Rcpp (p=3)")
})

# ---------------------------------------------------------------------------
# Test 5 — Never-treated rows are zero in output (not centred)
# ---------------------------------------------------------------------------
test_that("build_cov_interactions_cpp: never-treated rows remain zero", {
  df      <- make_covdm_data(seed = 5L, p = 1L)
  cov_mat <- as.matrix(df[, "x1", drop = FALSE])
  timing  <- as.integer(df$g)
  cohorts <- sort(unique(timing[!is.na(timing)]))
  all_t   <- sort(unique(df$year))
  gs_pairs <- do.call(rbind, lapply(cohorts, function(g)
    data.frame(g = g, s = all_t[all_t != g + (-1L)])))
  ind_mat <- build_indicator_matrix_cpp(timing, as.integer(df$year),
                                        as.integer(gs_pairs$g), as.integer(gs_pairs$s))

  cpp_out    <- build_cov_interactions_cpp(cov_mat, ind_mat, timing)
  never_rows <- which(is.na(df$g))

  expect_true(all(cpp_out[never_rows, ] == 0),
              label = "never-treated rows are all zero in Rcpp output")
})

# ---------------------------------------------------------------------------
# Test 6 — Output dimensions are N × (K * p)
# ---------------------------------------------------------------------------
test_that("build_cov_interactions_cpp output dimensions are N x K*p", {
  df      <- make_covdm_data(seed = 6L, p = 2L)
  cov_mat <- as.matrix(df[, c("x1", "x2")])
  timing  <- as.integer(df$g)
  cohorts <- sort(unique(timing[!is.na(timing)]))
  all_t   <- sort(unique(df$year))
  gs_pairs <- do.call(rbind, lapply(cohorts, function(g)
    data.frame(g = g, s = all_t[all_t != g + (-1L)])))
  ind_mat <- build_indicator_matrix_cpp(timing, as.integer(df$year),
                                        as.integer(gs_pairs$g), as.integer(gs_pairs$s))
  K <- ncol(ind_mat); p <- ncol(cov_mat); N <- nrow(df)

  cpp_out <- build_cov_interactions_cpp(cov_mat, ind_mat, timing)
  expect_equal(dim(cpp_out), c(N, K * p))
})
