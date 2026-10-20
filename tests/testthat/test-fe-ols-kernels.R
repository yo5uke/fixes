# test-fe-ols-kernels.R
#
# Agreement tests for the compute kernels behind .fit_fe_ols():
#   - crossprod_omp_cpp() vs base::crossprod() (values and thread-count
#     invariance — per-entry accumulation order is fixed, so results must be
#     identical across nthreads)
#   - demean_kway_cpp() thread-count invariance
#   - .chol_seq_drop() (C++ port) vs base::chol() on full-rank input and
#     its keep-first drop decisions on collinear input
#   - .fit_fe_ols() end-to-end vs feols under different fixes.nthreads

library(testthat)
library(fixes)

# ---------------------------------------------------------------------------
# crossprod kernel
# ---------------------------------------------------------------------------

test_that("crossprod_omp_cpp matches base crossprod across shapes", {
  set.seed(41)
  for (dims in list(c(100L, 7L), c(3000L, 40L), c(50L, 1L), c(8L, 12L))) {
    M <- matrix(rnorm(dims[1L] * dims[2L]), dims[1L], dims[2L])
    expect_equal(fixes:::crossprod_omp_cpp(M, 1L), unname(crossprod(M)),
                 tolerance = 1e-12)
  }
})

test_that("crossprod_omp_cpp is invariant to the thread count", {
  set.seed(42)
  M <- matrix(rnorm(5000L * 23L), 5000L, 23L)
  expect_identical(fixes:::crossprod_omp_cpp(M, 4L),
                   fixes:::crossprod_omp_cpp(M, 1L))
})

# ---------------------------------------------------------------------------
# demeaning kernel: thread-count invariance
# ---------------------------------------------------------------------------

test_that("demean_kway_cpp is invariant to the thread count", {
  set.seed(43)
  n_u <- 40L; n_t <- 12L
  df <- expand.grid(u = seq_len(n_u), t = seq_len(n_t))
  df <- df[runif(nrow(df)) > 0.2, ]
  M <- cbind(rnorm(nrow(df)), rnorm(nrow(df)), as.numeric(df$u <= 10))
  ids <- cbind(df$u - 1L, df$t - 1L)
  nl  <- c(n_u, n_t)

  d1 <- fixes:::demean_kway_cpp(M, ids, nl, w = numeric(0), tol = 1e-12,
                                max_iter = 10000L, nthreads = 1L)
  d4 <- fixes:::demean_kway_cpp(M, ids, nl, w = numeric(0), tol = 1e-12,
                                max_iter = 10000L, nthreads = 4L)
  expect_identical(d4$M, d1$M)
  expect_identical(d4$converged, d1$converged)

  w <- runif(nrow(df), 0.5, 2)
  dw1 <- fixes:::demean_kway_cpp(M, ids, nl, w = w, tol = 1e-12,
                                 max_iter = 10000L, nthreads = 1L)
  dw4 <- fixes:::demean_kway_cpp(M, ids, nl, w = w, tol = 1e-12,
                                 max_iter = 10000L, nthreads = 4L)
  expect_identical(dw4$M, dw1$M)
})

# ---------------------------------------------------------------------------
# sequential Cholesky (C++ port)
# ---------------------------------------------------------------------------

test_that(".chol_seq_drop matches base chol on full-rank input", {
  set.seed(44)
  A <- crossprod(matrix(rnorm(200L * 12L), 200L, 12L))
  cd <- fixes:::.chol_seq_drop(A)
  expect_equal(cd$kept, 1:12)
  expect_equal(cd$R, unname(chol(A)), tolerance = 1e-10)
})

test_that(".chol_seq_drop drops later duplicates and zero columns", {
  set.seed(45)
  X <- matrix(rnorm(300L * 5L), 300L, 5L)
  Xc <- cbind(X[, 1:2], X[, 1L], 0, X[, 3:5])  # col 3 dup of 1; col 4 zero
  A  <- crossprod(Xc)
  cd <- fixes:::.chol_seq_drop(A)
  expect_equal(cd$kept, c(1L, 2L, 5L, 6L, 7L))

  # solutions on the kept set match lm on the same columns
  y <- rnorm(300L)
  b <- backsolve(cd$R, forwardsolve(t(cd$R), crossprod(Xc, y)[cd$kept]))
  b_ref <- unname(coef(lm(y ~ Xc[, cd$kept] - 1)))
  expect_equal(as.numeric(b), b_ref, tolerance = 1e-8)
})

test_that(".chol_seq_drop honors d_pre for FE-absorbed columns", {
  # a column that is numerically zero relative to its pre-demeaning scale
  A <- diag(c(4, 1e-22, 9))
  cd <- fixes:::.chol_seq_drop(A, d_pre = c(4, 25, 9))
  expect_equal(cd$kept, c(1L, 3L))
})

# ---------------------------------------------------------------------------
# end-to-end: engine unchanged under different thread counts
# ---------------------------------------------------------------------------

test_that(".fit_fe_ols equals feols regardless of fixes.nthreads", {
  skip_if_not_installed("fixest")
  set.seed(46)
  df <- expand.grid(unit = 1:25, time = 1:8)
  df$x1 <- rnorm(nrow(df))
  df$x2 <- as.numeric(df$unit <= 12 & df$time >= 5)
  df$y  <- rnorm(25)[df$unit] + 0.3 * df$time + 0.5 * df$x1 + df$x2 +
    rnorm(nrow(df), sd = 0.6)
  X <- as.matrix(df[, c("x1", "x2")])

  fb <- data.frame(.y = df$y); fb$.X <- X
  fb$.f1 <- df$unit; fb$.f2 <- df$time
  m <- fixest::feols(.y ~ .X | .f1 + .f2, data = fb, cluster = fb$.f1,
                     warn = FALSE, notes = FALSE)
  V_ref <- stats::vcov(m)
  V_ref <- matrix(as.numeric(V_ref), nrow(V_ref))

  fits <- lapply(c(1L, 4L), function(nt) {
    old <- options(fixes.nthreads = nt); on.exit(options(old))
    fixes:::.fit_fe_ols(df$y, X, fe_list = list(u = df$unit, t = df$time),
                        cluster_vals = list(df$unit))
  })

  for (fit in fits) {
    expect_equal(unname(fit$coef), unname(coef(m)), tolerance = 1e-10)
    expect_equal(unname(fit$V), unname(as.matrix(V_ref)), tolerance = 1e-10)
  }
  # thread count must not change results at all
  expect_identical(fits[[1L]]$coef, fits[[2L]]$coef)
  expect_identical(fits[[1L]]$V, fits[[2L]]$V)
})

# ---------------------------------------------------------------------------
# row_no_na_cpp vs R rowSums(is.na(X)) == 0L
# ---------------------------------------------------------------------------

test_that("row_no_na_cpp matches R row-wise NA check", {
  set.seed(47)
  X_clean <- matrix(rnorm(200L * 8L), 200L, 8L)
  expect_identical(fixes:::row_no_na_cpp(X_clean), logical(0L))

  X_na <- X_clean
  X_na[c(1L, 55L, 200L), 3L] <- NA_real_
  X_na[c(55L, 120L), 7L]     <- NaN
  r_ref <- rowSums(is.na(X_na)) == 0L
  expect_identical(fixes:::row_no_na_cpp(X_na), r_ref)
})

# ---------------------------------------------------------------------------
# bind_yx_cpp vs R cbind / colSums and thread-count invariance
# ---------------------------------------------------------------------------

test_that("bind_yx_cpp matches R reference and is invariant to thread count", {
  set.seed(48)
  N <- 400L; K <- 12L
  y <- rnorm(N)
  X <- matrix(rnorm(N * K), N, K)
  w <- runif(N, 0.5, 2.0)
  keep_idx <- sort(sample(N, 320L))

  # Full sample, unweighted
  b1 <- fixes:::bind_yx_cpp(y, X, integer(0L), numeric(0L), 1L)
  b4 <- fixes:::bind_yx_cpp(y, X, integer(0L), numeric(0L), 4L)
  expect_equal(b1$M, unname(cbind(y, X)), tolerance = 1e-12)
  expect_equal(b1$d_pre, unname(colSums(X^2)), tolerance = 1e-12)
  expect_identical(b1$M, b4$M)
  expect_identical(b1$d_pre, b4$d_pre)

  # Subset + weighted
  wk <- w[keep_idx]
  bw1 <- fixes:::bind_yx_cpp(y, X, keep_idx, wk, 1L)
  bw4 <- fixes:::bind_yx_cpp(y, X, keep_idx, wk, 4L)
  Xk  <- X[keep_idx, , drop = FALSE]
  expect_equal(bw1$M, unname(cbind(y[keep_idx], Xk)), tolerance = 1e-12)
  expect_equal(bw1$d_pre, unname(colSums(Xk^2 * wk)), tolerance = 1e-12)
  expect_identical(bw1$M, bw4$M)
  expect_identical(bw1$d_pre, bw4$d_pre)
})

# ---------------------------------------------------------------------------
# demean_kway_cpp with scale_sqrt_w = TRUE vs R post-scaling
# ---------------------------------------------------------------------------

test_that("demean_kway_cpp scale_sqrt_w matches R sqrt(w) multiplication", {
  set.seed(49)
  n_u <- 30L; n_t <- 10L
  df <- expand.grid(u = seq_len(n_u), t = seq_len(n_t))
  M  <- cbind(rnorm(nrow(df)), rnorm(nrow(df)), rnorm(nrow(df)))
  ids <- cbind(df$u - 1L, df$t - 1L)
  nl  <- c(n_u, n_t)
  w   <- runif(nrow(df), 0.2, 3.0)

  d_unscaled <- fixes:::demean_kway_cpp(M, ids, nl, w = w, tol = 1e-12,
                                        max_iter = 10000L, nthreads = 1L,
                                        scale_sqrt_w = FALSE)
  d_scaled1  <- fixes:::demean_kway_cpp(M, ids, nl, w = w, tol = 1e-12,
                                        max_iter = 10000L, nthreads = 1L,
                                        scale_sqrt_w = TRUE)
  d_scaled4  <- fixes:::demean_kway_cpp(M, ids, nl, w = w, tol = 1e-12,
                                        max_iter = 10000L, nthreads = 4L,
                                        scale_sqrt_w = TRUE)
  expect_equal(d_scaled1$M, d_unscaled$M * sqrt(w), tolerance = 1e-12)
  expect_identical(d_scaled1$M, d_scaled4$M)
})

# ---------------------------------------------------------------------------
# fe_ols_meat_cpp vs R reference across vcov_mode = 0, 1, 2 and nthreads
# ---------------------------------------------------------------------------

test_that("fe_ols_meat_cpp matches R reference for iid, hetero, and cluster", {
  set.seed(50)
  N <- 500L; K <- 10L
  Md <- matrix(rnorm(N * (1L + K)), N, 1L + K)
  kept <- c(1L, 3L, 4L, 7L, 9L, 10L)
  b <- rnorm(length(kept))
  cl_i <- sample(1:25, N, replace = TRUE)
  G <- 25L

  yd   <- Md[, 1L]
  Xd_k <- Md[, kept + 1L, drop = FALSE]
  e_ref <- yd - drop(Xd_k %*% b)
  rss_ref <- sum(e_ref^2)
  tss_ref <- sum(yd^2)
  meat_het_ref <- unname(crossprod(Xd_k * e_ref))
  S_ref <- rowsum(Xd_k * e_ref, cl_i, reorder = TRUE)
  meat_cl_ref  <- unname(crossprod(S_ref))

  # Mode 0 (iid)
  m0 <- fixes:::fe_ols_meat_cpp(Md, kept, b, 0L, integer(0L), 0L, 1L)
  expect_equal(m0$e, e_ref, tolerance = 1e-12)
  expect_equal(m0$rss, rss_ref, tolerance = 1e-12)
  expect_equal(m0$tss_yd, tss_ref, tolerance = 1e-12)

  # Mode 1 (hetero) + thread invariance
  m1_1 <- fixes:::fe_ols_meat_cpp(Md, kept, b, 1L, integer(0L), 0L, 1L)
  m1_4 <- fixes:::fe_ols_meat_cpp(Md, kept, b, 1L, integer(0L), 0L, 4L)
  expect_equal(m1_1$e, e_ref, tolerance = 1e-12)
  expect_equal(m1_1$meat, meat_het_ref, tolerance = 1e-12)
  expect_identical(m1_1$meat, m1_4$meat)
  expect_identical(m1_1$e, m1_4$e)

  # Mode 2 (cluster) + thread invariance
  m2_1 <- fixes:::fe_ols_meat_cpp(Md, kept, b, 2L, cl_i, G, 1L)
  m2_4 <- fixes:::fe_ols_meat_cpp(Md, kept, b, 2L, cl_i, G, 4L)
  expect_equal(m2_1$e, e_ref, tolerance = 1e-12)
  expect_equal(m2_1$meat, meat_cl_ref, tolerance = 1e-12)
  expect_identical(m2_1$meat, m2_4$meat)
  expect_identical(m2_1$e, m2_4$e)
})

# ---------------------------------------------------------------------------
# build_twfe_design_cpp vs R loop
# ---------------------------------------------------------------------------

test_that("build_twfe_design_cpp matches pure-R dummy expansion", {
  set.seed(51)
  N <- 300L
  f_vals  <- sample(c(-3:4, NA_integer_), N, replace = TRUE)
  tr_vals <- sample(c(0, 1, NA_real_), N, replace = TRUE, prob = c(0.45, 0.45, 0.10))
  lv <- as.numeric(c(-3, -2, 0, 1, 2, 3, 4))

  r_build <- function(fv, tv, lv) {
    na_row <- is.na(fv) | is.na(tv)
    X <- matrix(0, length(fv), length(lv))
    for (j in seq_along(lv)) {
      sel <- !na_row & fv == lv[j]
      X[sel, j] <- tv[sel]
    }
    if (any(na_row)) X[na_row, ] <- NA_real_
    X
  }

  expect_equal(
    fixes:::build_twfe_design_cpp(as.numeric(f_vals), as.numeric(tr_vals), lv),
    r_build(f_vals, tr_vals, lv),
    tolerance = 1e-12
  )
})
