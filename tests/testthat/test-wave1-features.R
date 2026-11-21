library(testthat)
library(fixes)

make_w1_panel <- function(seed = 202L, n_id = 60L) {
  set.seed(seed)
  years <- 2001:2008
  df <- expand.grid(id = seq_len(n_id), year = years,
                    stringsAsFactors = FALSE)
  df <- df[order(df$id, df$year), ]
  rownames(df) <- NULL

  g_u <- c(rep(2004L, 20L), rep(2006L, 20L), rep(NA_integer_, n_id - 40L))
  df$g      <- g_u[df$id]
  df$D      <- as.integer(!is.na(df$g) & df$year >= df$g)
  df$region <- (df$id %% 3L) + 1L
  df$grp    <- (df$id - 1L) %/% 3L + 1L
  df$x1     <- rnorm(nrow(df))
  df$w      <- runif(nrow(df), 0.5, 2.0)

  df$y <- rnorm(n_id)[df$id] + 0.15 * (df$year - 2001L) +
    0.4 * df$x1 + 1.5 * df$D + rnorm(nrow(df), sd = 0.4)
  df
}

make_w1_flex <- function(seed = 203L, M = 10L) {
  set.seed(seed)
  cohort <- c(rep(3L, 5L), rep(5L, 5L), rep(NA_integer_, 5L))
  g_fe <- rnorm(length(cohort)) * 0.5
  rows <- lapply(seq_along(cohort), function(g) {
    do.call(rbind, lapply(1:8, function(t) {
      on <- !is.na(cohort[g]) && t >= cohort[g]
      x <- rnorm(M)
      data.frame(
        year = t, group_id = g, g = cohort[g],
        x1 = x, w = runif(M, 0.5, 2.0),
        y = g_fe[g] + 0.1 * t + 0.4 * x + 1.5 * on + rnorm(M, sd = 0.4)
      )
    }))
  })
  do.call(rbind, rows)
}

w1_df <- make_w1_panel()
w1_fx <- make_w1_flex()

# ---------------------------------------------------------------------------
# 1. Rcpp-vs-R parity for updated kernels (tolerance 1e-12)
# ---------------------------------------------------------------------------

test_that("aggregate_iw_cpp vcov and cell_weights match pure-R within 1e-12", {
  set.seed(11L)
  K <- 6L; pV <- 8L
  A <- matrix(rnorm(pV * pV), pV, pV)
  V_full <- crossprod(A)
  est <- rnorm(K)
  l_vals <- c(-2L, -2L, 0L, 0L, 1L, 1L)
  cohort_vals <- c(2004L, 2006L, 2004L, 2006L, 2004L, 2006L)
  idx_in_V <- 0:5
  unique_l <- c(-2L, 0L, 1L)
  cs_keys <- c(2004L, 2006L)
  cs_vals <- c(20L, 15L)
  cw <- c(12.3, 8.7, 14.1, 9.9, 11.2, 7.8)

  res_cpp <- fixes:::aggregate_iw_cpp(
    est, l_vals, cohort_vals, idx_in_V, V_full,
    unique_l, cs_keys, cs_vals, 2001L, 2008L,
    cell_weights = cw
  )

  # Pure-R matrix calculation
  W <- matrix(0, pV, length(unique_l))
  est_r <- numeric(length(unique_l))
  for (j in seq_along(unique_l)) {
    l <- unique_l[j]
    k <- which(l_vals == l)
    w <- cw[k] / sum(cw[k])
    est_r[j] <- sum(w * est[k])
    W[idx_in_V[k] + 1L, j] <- w
  }
  V_r <- t(W) %*% V_full %*% W
  se_r <- sqrt(diag(V_r))

  expect_equal(res_cpp$es$estimate, est_r, tolerance = 1e-12)
  expect_equal(res_cpp$es$std_error, se_r, tolerance = 1e-12)
  expect_equal(res_cpp$vcov, V_r, tolerance = 1e-12)
})

test_that("build_cov_interactions_cpp weighted centering matches pure-R within 1e-12", {
  set.seed(12L)
  N <- 40L; p <- 2L; K <- 3L
  cov_mat <- matrix(rnorm(N * p), N, p)
  ind_mat <- matrix(0L, N, K)
  for (i in seq_len(N)) ind_mat[i, (i %% K) + 1L] <- 1L
  g_key <- rep(c(1L, 2L, NA_integer_, 1L), length.out = N)
  wt <- runif(N, 0.2, 3.0)

  cpp <- fixes:::build_cov_interactions_cpp(cov_mat, ind_mat, g_key, wt = wt)

  xc <- matrix(0, N, p)
  for (g in unique(g_key[!is.na(g_key)])) {
    r <- which(g_key == g)
    for (j in seq_len(p)) {
      mu <- sum(wt[r] * cov_mat[r, j]) / sum(wt[r])
      xc[r, j] <- cov_mat[r, j] - mu
    }
  }
  ref <- matrix(0, N, K * p)
  for (j in seq_len(p)) {
    for (k in seq_len(K)) {
      ref[, (j - 1L) * K + k] <- ind_mat[, k] * xc[, j]
    }
  }
  expect_equal(cpp, ref, tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# 2. es_vcov on all estimators + honest_sensitivity() integration
# ---------------------------------------------------------------------------

test_that("all estimators attach valid es_vcov matching std.error", {
  skip_if_not_installed("fixest")

  # CS, SA, TWM, BJS (with pretrends)
  res_list <- list(
    cs  = event_study(y ~ 1 | id + year, w1_df, time = year, timing = g, estimator = "cs"),
    sa  = event_study(y ~ 1 | id + year, w1_df, time = year, timing = g, estimator = "sa", cluster = ~ id),
    twm = event_study(y ~ 1 | id + year, w1_df, time = year, timing = g, estimator = "twm", cluster = ~ id),
    bjs = event_study(y ~ 1 | id + year, w1_df, time = year, timing = g, estimator = "bjs", pretrends = TRUE),
    flex = suppressWarnings(event_study(y ~ 1, w1_fx, time = year, timing = g, group = group_id, estimator = "flex"))
  )

  for (nm in names(res_list)) {
    res <- res_list[[nm]]
    V <- attr(res, "es_vcov")
    nb <- res[!res$is_baseline, ]
    expect_false(is.null(V), info = nm)
    expect_true(isSymmetric(V, tol = 1e-12), info = nm)
    expect_equal(rownames(V), as.character(nb$relative_time), info = nm)
    expect_equal(unname(sqrt(diag(V))), nb$std.error, tolerance = 1e-10, info = nm)
  }

  # SA es_vcov matches W' vcov(fixest::sunab) W and aggregated SEs
  sa_df <- w1_df; sa_df$g[is.na(sa_df$g)] <- 10000L
  f_sa  <- fixest::feols(y ~ sunab(g, year) | id + year, data = sa_df, cluster = ~ id)
  s_sa  <- summary(f_sa, agg = "period")
  ct_sa <- fixest::coeftable(s_sa)
  V_sa  <- attr(res_list$sa, "es_vcov")
  ref_l <- as.integer(sub("^year::", "", rownames(ct_sa)))
  ord_r <- order(ref_l)
  expect_equal(unname(sqrt(diag(V_sa))), unname(ct_sa[ord_r, "Std. Error"]),
               tolerance = 1e-6)

  V_raw <- unclass(vcov(f_sa))
  cn    <- rownames(V_raw)
  c_l   <- as.integer(sub("^year::(-?[0-9]+):cohort::.*$", "\\1", cn))
  c_g   <- as.integer(sub("^.*:cohort::([0-9]+)$", "\\1", cn))
  sz    <- table(w1_df$g[w1_df$year == 2001L & !is.na(w1_df$g)])
  u_l   <- sort(unique(c_l))
  W_ref <- matrix(0, length(cn), length(u_l))
  for (j in seq_along(u_l)) {
    k <- which(c_l == u_l[j])
    w <- as.numeric(sz[as.character(c_g[k])])
    W_ref[k, j] <- w / sum(w)
  }
  expect_equal(unname(V_sa), unname(t(W_ref) %*% V_raw %*% W_ref),
               tolerance = 1e-6)
})

test_that("honest_sensitivity() runs directly on es_result from cs, sa, bjs, twm, flex", {
  skip_if_not_installed("lpSolveAPI")
  skip_if_not_installed("Rglpk")
  skip_if_not_installed("TruncatedNormal")
  skip_if_not_installed("Matrix")
  skip_if_not_installed("pracma")

  res_cs  <- event_study(y ~ 1 | id + year, w1_df, time = year, timing = g, estimator = "cs")
  res_sa  <- event_study(y ~ 1 | id + year, w1_df, time = year, timing = g, estimator = "sa", cluster = ~ id)
  res_twm <- event_study(y ~ 1 | id + year, w1_df, time = year, timing = g, estimator = "twm", cluster = ~ id)
  res_bjs <- event_study(y ~ 1 | id + year, w1_df, time = year, timing = g, estimator = "bjs", pretrends = TRUE)
  res_flx <- suppressWarnings(event_study(y ~ 1, w1_fx, time = year, timing = g, group = group_id, estimator = "flex"))

  for (r in list(res_cs, res_sa, res_twm, res_bjs, res_flx)) {
    h <- honest_sensitivity(r, m_grid = c(0, 0.5, 1), grid_points = 100L)
    expect_s3_class(h, "honest_result")
    expect_true(all(is.finite(h$lb)))
    expect_true(all(is.finite(h$ub)))
  }
})

# ---------------------------------------------------------------------------
# 3. SA with covariates and weights vs fixest::sunab()
# ---------------------------------------------------------------------------

test_that("SA with covariates and weights matches fixest::sunab() within 1e-6", {
  skip_if_not_installed("fixest")

  sa_df <- w1_df; sa_df$g[is.na(sa_df$g)] <- 10000L
  ref <- fixest::feols(
    y ~ sunab(g, year) + x1 + factor(region) | id + year,
    data = sa_df, weights = ~ w, cluster = ~ id
  )
  res <- event_study(
    y ~ x1 + factor(region) | id + year, data = w1_df,
    time = year, timing = g, estimator = "sa",
    weights = ~ w, cluster = ~ id
  )

  nb <- res[!res$is_baseline, ]
  ct <- fixest::coeftable(ref)
  sa_rows <- grep("^year::", rownames(ct))
  ref_l   <- as.integer(sub("^year::", "", rownames(ct)[sa_rows]))
  i1 <- match(ref_l, nb$relative_time)
  expect_equal(nb$estimate[i1], unname(ct[sa_rows, "Estimate"]), tolerance = 1e-6)
  expect_equal(nb$std.error[i1], unname(ct[sa_rows, "Std. Error"]), tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# 4. TWM and FLEX with weights
# ---------------------------------------------------------------------------

test_that("TWM and FLEX support weights; weighted TWM matches weighted SA", {
  sa_w  <- event_study(y ~ 1 | id + year, w1_df, time = year, timing = g,
                       estimator = "sa", weights = ~ w, cluster = ~ id)
  twm_w <- event_study(y ~ 1 | id + year, w1_df, time = year, timing = g,
                       estimator = "twm", weights = ~ w, cluster = ~ id)
  expect_equal(twm_w$estimate, sa_w$estimate, tolerance = 1e-8)
  expect_equal(twm_w$std.error, sa_w$std.error, tolerance = 1e-8)

  twm_cw <- event_study(y ~ x1 | id + year, w1_df, time = year, timing = g,
                        estimator = "twm", weights = ~ w, cluster = ~ id)
  expect_true(all(is.finite(twm_cw$estimate)))
  expect_true(all(twm_cw$std.error[!twm_cw$is_baseline] > 0))

  flx_cw <- suppressWarnings(event_study(
    y ~ x1, w1_fx, time = year, timing = g, group = group_id,
    estimator = "flex", weights = ~ w, cluster = ~ group_id
  ))
  expect_true(all(is.finite(flx_cw$estimate)))
  expect_true(all(flx_cw$std.error[!flx_cw$is_baseline] > 0))
})

# ---------------------------------------------------------------------------
# 5. BJS pretrends and att(bjs, cluster) vs didimputation
# ---------------------------------------------------------------------------

test_that("BJS pretrends = TRUE and explicit horizons match didimputation within 1e-6", {
  skip_if_not_installed("didimputation")

  did_df <- w1_df; did_df$g <- as.numeric(did_df$g); did_df$g[is.na(did_df$g)] <- 0

  # pretrends = TRUE with covariates
  ref_all <- didimputation::did_imputation(
    did_df, yname = "y", gname = "g", tname = "year", idname = "id",
    first_stage = ~ x1 | id + year, horizon = TRUE, pretrends = TRUE
  )
  res_all <- event_study(
    y ~ x1 | id + year, data = w1_df, time = year, timing = g,
    estimator = "bjs", pretrends = TRUE
  )
  nb <- res_all[!res_all$is_baseline, ]
  common <- intersect(nb$relative_time, as.integer(ref_all$term))
  i1 <- match(common, nb$relative_time)
  i2 <- match(as.character(common), ref_all$term)
  expect_equal(nb$estimate[i1], ref_all$estimate[i2], tolerance = 1e-6)
  expect_equal(nb$std.error[i1], ref_all$std.error[i2], tolerance = 1e-6)

  # pretrends = -4:-2
  ref_sub <- didimputation::did_imputation(
    did_df, yname = "y", gname = "g", tname = "year", idname = "id",
    first_stage = ~ x1 | id + year, horizon = TRUE, pretrends = -4:-2
  )
  res_sub <- event_study(
    y ~ x1 | id + year, data = w1_df, time = year, timing = g,
    estimator = "bjs", pretrends = -4:-2
  )
  nb_sub <- res_sub[!res_sub$is_baseline, ]
  common_s <- intersect(nb_sub$relative_time, as.integer(ref_sub$term))
  i1s <- match(common_s, nb_sub$relative_time)
  i2s <- match(as.character(common_s), ref_sub$term)
  expect_equal(nb_sub$estimate[i1s], ref_sub$estimate[i2s], tolerance = 1e-6)
  expect_equal(nb_sub$std.error[i1s], ref_sub$std.error[i2s], tolerance = 1e-6)
})

test_that("att(estimator = 'bjs', cluster = ...) matches didimputation within 1e-6", {
  skip_if_not_installed("didimputation")

  did_df <- w1_df; did_df$g <- as.numeric(did_df$g); did_df$g[is.na(did_df$g)] <- 0
  ref <- didimputation::did_imputation(
    did_df, yname = "y", gname = "g", tname = "year", idname = "id",
    first_stage = ~ x1 | id + year, cluster_var = "grp"
  )
  res <- att(y ~ x1 | id + year, data = w1_df, time = year, timing = g,
             estimator = "bjs", cluster = ~ grp)
  expect_equal(res$estimate, ref$estimate, tolerance = 1e-6)
  expect_equal(res$std.error, ref$std.error, tolerance = 1e-6)
})
