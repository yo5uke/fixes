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

  d1 <- fixes:::demean_kway_cpp(M, ids, nl, tol = 1e-12, max_iter = 10000L,
                                nthreads = 1L)
  d4 <- fixes:::demean_kway_cpp(M, ids, nl, tol = 1e-12, max_iter = 10000L,
                                nthreads = 4L)
  expect_identical(d4$M, d1$M)
  expect_identical(d4$converged, d1$converged)
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
