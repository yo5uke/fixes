library(testthat)
library(fixes)

make_cov_panel <- function(seed = 101L, n_id = 80L) {
  set.seed(seed)
  years <- 2001:2008
  df <- expand.grid(id = seq_len(n_id), year = years,
                    stringsAsFactors = FALSE)
  df <- df[order(df$id, df$year), ]
  rownames(df) <- NULL

  g_u <- sample(c(2004L, 2006L, NA_integer_), n_id, replace = TRUE,
                prob = c(0.35, 0.35, 0.30))
  df$g       <- g_u[df$id]
  df$treated <- as.integer(!is.na(df$g))
  df$D       <- as.integer(!is.na(df$g) & df$year >= df$g)
  df$region  <- (df$id %% 3L) + 1L
  df$x_const <- rnorm(n_id)[df$id]
  df$x1      <- 0.4 * df$x_const + rnorm(nrow(df))
  df$x2      <- rnorm(nrow(df))

  unit_fe <- rnorm(n_id)[df$id]
  time_fe <- 0.15 * (df$year - 2001L)
  df$y <- unit_fe + time_fe +
    0.5 * df$x_const * (df$year - 2001L) +
    0.4 * df$x1 - 0.3 * df$x2 +
    1.5 * df$D + rnorm(nrow(df), sd = 0.4)
  df
}

# Pure-R reference for compute_cs_cov_cpp
pure_r_cs_cov <- function(Y_mat, unit_timing, cohorts, all_times,
                          X_base_list, control_group, anticipation,
                          est_method) {
  n_units <- nrow(Y_mat)
  never_only <- control_group == "nevertreated"
  out_g <- numeric(); out_t <- numeric()
  out_att <- numeric(); out_se <- numeric()
  psi_list <- list()

  for (gi in seq_along(cohorts)) {
    g <- cohorts[gi]
    base_t <- g - 1 - anticipation
    base_ti <- match(base_t, all_times)
    if (is.na(base_ti) || is.null(X_base_list[[gi]])) next
    X_all <- X_base_list[[gi]]
    is_tr <- !is.na(unit_timing) & unit_timing == g
    if (!any(is_tr)) next

    for (ti in seq_along(all_times)) {
      t <- all_times[ti]
      if (t == base_t) next
      cutoff <- max(t + anticipation, g - 1)
      is_ct <- if (never_only) {
        is.na(unit_timing)
      } else {
        (is.na(unit_timing) | unit_timing > cutoff) & (is.na(unit_timing) | unit_timing != g)
      }
      keep <- is_tr | is_ct
      if (!any(is_ct)) next

      dy <- Y_mat[keep, ti] - Y_mat[keep, base_ti]
      if (any(!is.finite(dy))) next
      D <- as.numeric(is_tr[keep])
      C <- 1 - D
      X <- X_all[keep, , drop = FALSE]
      n_sub <- length(D)
      w1 <- D / mean(D)

      if (est_method %in% c("reg", "trad")) {
        Xc <- X[C == 1, , drop = FALSE]
        beta <- solve(crossprod(Xc), crossprod(Xc, dy[C == 1]))
        r <- dy - as.numeric(X %*% beta)
      } else {
        r <- dy
      }

      if (est_method %in% c("ipw", "trad")) {
        ps <- stats::glm.fit(
          X, D, family = stats::binomial(),
          control = list(epsilon = 1e-14, maxit = 100L)
        )$fitted.values
        ps <- pmin(ps, 1 - 1e-6)
        trim <- ifelse(C == 1, ps < 0.995, TRUE)
        w0_raw <- trim * C * ps / (1 - ps)
        w0 <- w0_raw / mean(w0_raw)
        H <- crossprod(X, X * (ps * (1 - ps))) / n_sub
      }

      if (est_method == "reg") {
        att <- mean(w1 * r)
        M1 <- colMeans(w1 * X)
        u1 <- solve(crossprod(Xc) / n_sub, M1)
        psi_sub <- w1 * (r - att) - C * r * as.numeric(X %*% u1)
      } else if (est_method == "ipw") {
        eta1 <- mean(w1 * dy); eta0 <- mean(w0 * dy)
        att <- eta1 - eta0
        M2 <- colMeans(w0 * (dy - eta0) * X)
        u2 <- solve(H, M2)
        psi_sub <- w1 * (dy - eta1) - w0 * (dy - eta0) -
          (D - ps) * as.numeric(X %*% u2)
      } else {
        eta1 <- mean(w1 * r); eta0 <- mean(w0 * r)
        att <- eta1 - eta0
        M1 <- colMeans((w1 - w0) * X)
        u1 <- solve(crossprod(Xc) / n_sub, M1)
        M2 <- colMeans(w0 * (r - eta0) * X)
        u2 <- solve(H, M2)
        psi_sub <- w1 * (r - eta1) - w0 * (r - eta0) -
          C * r * as.numeric(X %*% u1) -
          (D - ps) * as.numeric(X %*% u2)
      }

      psi <- rep(0, n_units)
      psi[keep] <- psi_sub * (n_units / n_sub)
      se <- sqrt(sum(psi^2)) / n_units

      out_g <- c(out_g, g); out_t <- c(out_t, t)
      out_att <- c(out_att, att); out_se <- c(out_se, se)
      psi_list[[length(psi_list) + 1L]] <- psi
    }
  }
  list(g = out_g, t = out_t, att = out_att, se = out_se,
       psi = do.call(cbind, psi_list))
}

cov_df <- make_cov_panel()

# ---------------------------------------------------------------------------
# 1. Rcpp vs pure-R parity for compute_cs_cov_cpp
# ---------------------------------------------------------------------------

test_that("compute_cs_cov_cpp matches pure-R implementation within 1e-12", {
  u_all <- sort(unique(cov_df$id))
  all_t <- sort(unique(cov_df$year))
  cohorts <- sort(unique(cov_df$g[!is.na(cov_df$g)]))
  Y_mat <- matrix(NA_real_, length(u_all), length(all_t))
  Y_mat[cbind(match(cov_df$id, u_all), match(cov_df$year, all_t))] <- cov_df$y
  unit_timing <- cov_df$g[match(u_all, cov_df$id)]
  X_base_list <- lapply(cohorts, function(g) {
    sub <- cov_df[cov_df$year == (g - 1L), ]
    sub <- sub[match(u_all, sub$id), ]
    cbind(1, sub$x1, sub$x2)
  })

  for (cg in c("nevertreated", "notyettreated")) {
    for (m in c("trad", "reg", "ipw")) {
      cpp <- fixes:::compute_cs_cov_cpp(Y_mat, unit_timing, cohorts, all_t,
                                        X_base_list, cg, 0L, m)
      ref <- pure_r_cs_cov(Y_mat, unit_timing, cohorts, all_t,
                           X_base_list, cg, 0L, m)
      expect_equal(cpp$g, ref$g)
      expect_equal(cpp$t, ref$t)
      expect_equal(cpp$att, ref$att, tolerance = 1e-12)
      expect_equal(cpp$se, ref$se, tolerance = 1e-12)
      expect_equal(cpp$psi, ref$psi, tolerance = 1e-12)
    }
  }
})

# ---------------------------------------------------------------------------
# 2. CS with covariates vs did::att_gt() + did::aggte()
# ---------------------------------------------------------------------------

test_that("CS att() and event_study() with covariates match did package", {
  skip_if_not_installed("did")

  did_df <- cov_df
  did_df$g <- as.numeric(did_df$g)
  did_df$g[is.na(did_df$g)] <- 0

  for (cg in c("nevertreated", "notyettreated")) {
    for (antic in c(0L, 1L)) {
      for (m in c("trad", "reg", "ipw")) {
        did_m <- c(trad = "dr", reg = "reg", ipw = "ipw")[[m]]
        ref_gt <- suppressWarnings(did::att_gt(
          yname = "y", tname = "year", idname = "id", gname = "g",
          xformla = ~ x_const + x1 + x2, data = did_df,
          control_group = cg, anticipation = antic,
          est_method = did_m, base_period = "universal",
          bstrap = FALSE, cband = FALSE
        ))

        for (agg in c("simple", "by_cohort", "by_time")) {
          did_type <- switch(agg, simple = "simple", by_cohort = "group",
                             by_time = "calendar")
          ref_agg <- did::aggte(ref_gt, type = did_type, bstrap = FALSE)
          res <- att(y ~ x_const + x1 + x2 | id + year, data = cov_df,
                     time = year, timing = g, estimator = "cs",
                     aggregation = agg, control_group = cg,
                     anticipation = antic, est_method = m)
          if (agg == "simple") {
            expect_equal(res$estimate, ref_agg$overall.att, tolerance = 1e-6)
            expect_equal(res$std.error, ref_agg$overall.se, tolerance = 1e-6)
          } else {
            ord <- order(ref_agg$egt)
            expect_equal(res$group, ref_agg$egt[ord])
            expect_equal(res$estimate, ref_agg$att.egt[ord], tolerance = 1e-6)
            expect_equal(res$std.error, ref_agg$se.egt[ord], tolerance = 1e-6)
          }
        }

        if (antic == 0L && m == "trad") {
          ref_dyn <- did::aggte(ref_gt, type = "dynamic", bstrap = FALSE)
          es <- event_study(y ~ x_const + x1 + x2 | id + year, data = cov_df,
                            time = year, timing = g, estimator = "cs",
                            control_group = cg, est_method = m)
          es_post <- es[!es$is_baseline & es$relative_time >= 0L, ]
          common_l <- intersect(es_post$relative_time, ref_dyn$egt)
          i1 <- match(common_l, es_post$relative_time)
          i2 <- match(common_l, ref_dyn$egt)
          expect_equal(es_post$estimate[i1], ref_dyn$att.egt[i2], tolerance = 1e-6)
          expect_equal(es_post$std.error[i1], ref_dyn$se.egt[i2], tolerance = 1e-6)
        }
      }
    }
  }

  # Time-invariant covariates match did across all pre and post horizons
  ref_ti <- did::att_gt(
    yname = "y", tname = "year", idname = "id", gname = "g",
    xformla = ~ x_const, data = did_df, base_period = "universal",
    bstrap = FALSE, cband = FALSE
  )
  ref_ti_dyn <- did::aggte(ref_ti, type = "dynamic", bstrap = FALSE)
  es_ti <- event_study(y ~ x_const | id + year, data = cov_df,
                       time = year, timing = g, estimator = "cs")
  es_ti_nb <- es_ti[!es_ti$is_baseline, ]
  common_ti <- intersect(es_ti_nb$relative_time, ref_ti_dyn$egt)
  expect_equal(es_ti_nb$estimate[match(common_ti, es_ti_nb$relative_time)],
               ref_ti_dyn$att.egt[match(common_ti, ref_ti_dyn$egt)],
               tolerance = 1e-6)
  expect_equal(es_ti_nb$std.error[match(common_ti, es_ti_nb$relative_time)],
               ref_ti_dyn$se.egt[match(common_ti, ref_ti_dyn$egt)],
               tolerance = 1e-6)
})

test_that("CS handles bootstrap, expressions, missing and collinear covariates", {
  skip_if_not_installed("did")

  # Expressions and factors
  d <- cov_df
  d$ax1 <- abs(d$x1)
  d$r2 <- as.integer(d$region == 2L)
  d$r3 <- as.integer(d$region == 3L)
  a_expr <- att(y ~ abs(x1) + factor(region) | id + year, data = d,
                time = year, timing = g, estimator = "cs")
  a_cols <- att(y ~ ax1 + r2 + r3 | id + year, data = d,
                time = year, timing = g, estimator = "cs")
  expect_equal(a_expr$estimate, a_cols$estimate, tolerance = 1e-12)
  expect_equal(a_expr$std.error, a_cols$std.error, tolerance = 1e-12)

  # Bootstrap simultaneous bands with covariates
  es_boot <- event_study(y ~ x1 + x2 | id + year, data = cov_df,
                         time = year, timing = g, estimator = "cs",
                         bootstrap = TRUE, boot_reps = 99L, boot_seed = 7L)
  expect_true(all(c("conf_low_sim", "conf_high_sim") %in% names(es_boot)))
  expect_false(is.null(attr(es_boot, "bootstrap")))
  expect_true(all(is.finite(es_boot$conf_low_sim)))

  # Collinear covariates match non-collinear covariates
  d$x_dup <- 2 * d$x1
  a_base <- att(y ~ x1 + x2 | id + year, data = d, time = year, timing = g,
                estimator = "cs")
  a_collin <- att(y ~ x1 + x2 + x_dup | id + year, data = d,
                  time = year, timing = g, estimator = "cs")
  expect_equal(a_collin$estimate, a_base$estimate, tolerance = 1e-8)
  expect_equal(a_collin$std.error, a_base$std.error, tolerance = 1e-8)

  # Missing covariate drops affected units when balancing
  d_na <- cov_df
  d_na$x1[d_na$id == 1L & d_na$year == 2003L] <- NA_real_
  expect_warning(
    a_na <- att(y ~ x1 + x2 | id + year, data = d_na,
                time = year, timing = g, estimator = "cs"),
    "Dropped"
  )
  did_na <- d_na
  did_na$g <- as.numeric(did_na$g); did_na$g[is.na(did_na$g)] <- 0
  ref_na <- suppressWarnings(did::att_gt(
    yname = "y", tname = "year", idname = "id", gname = "g",
    xformla = ~ x1 + x2, data = did_na, base_period = "universal",
    bstrap = FALSE, cband = FALSE
  ))
  ref_na_s <- did::aggte(ref_na, type = "simple", bstrap = FALSE)
  expect_equal(a_na$estimate, ref_na_s$overall.att, tolerance = 1e-6)
  expect_equal(a_na$std.error, ref_na_s$overall.se, tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# 3. BJS with covariates vs didimputation::did_imputation()
# ---------------------------------------------------------------------------

test_that("BJS att() and event_study() with covariates match didimputation", {
  skip_if_not_installed("didimputation")

  did_df <- cov_df
  did_df$g <- as.numeric(did_df$g)
  did_df$g[is.na(did_df$g)] <- 0

  # att() simple
  ref_s <- didimputation::did_imputation(
    data = did_df, yname = "y", gname = "g", tname = "year", idname = "id",
    first_stage = ~ x1 + x2 | id + year
  )
  res_s <- att(y ~ x1 + x2 | id + year, data = cov_df,
               time = year, timing = g, estimator = "bjs",
               aggregation = "simple")
  expect_equal(res_s$estimate, ref_s$estimate, tolerance = 1e-6)
  expect_equal(res_s$std.error, ref_s$std.error, tolerance = 1e-6)

  # att() by_cohort
  res_c <- att(y ~ x1 + x2 | id + year, data = cov_df,
               time = year, timing = g, estimator = "bjs",
               aggregation = "by_cohort")
  expect_equal(nrow(res_c), 2L)
  expect_true(all(is.finite(res_c$estimate)))
  expect_true(all(res_c$std.error > 0))

  # event_study() horizon
  ref_h <- didimputation::did_imputation(
    data = did_df, yname = "y", gname = "g", tname = "year", idname = "id",
    first_stage = ~ x1 + x2 | id + year, horizon = TRUE
  )
  res_h <- event_study(y ~ x1 + x2 | id + year, data = cov_df,
                       time = year, timing = g, estimator = "bjs")
  res_h_nb <- res_h[!res_h$is_baseline, ]
  common <- intersect(res_h_nb$relative_time, as.integer(ref_h$term))
  i1 <- match(common, res_h_nb$relative_time)
  i2 <- match(as.character(common), ref_h$term)
  expect_equal(res_h_nb$estimate[i1], ref_h$estimate[i2], tolerance = 1e-6)
  expect_equal(res_h_nb$std.error[i1], ref_h$std.error[i2], tolerance = 1e-6)
})

test_that("BJS handles singletons, missing/collinear covariates, expressions, and custom cluster", {
  skip_if_not_installed("didimputation")

  # Singleton unit in Omega_0 (first treated at 2002, only 2001 untreated)
  d_sing <- cov_df
  d_sing$g[d_sing$id == 1L] <- 2002L
  d_sing$D <- as.integer(!is.na(d_sing$g) & d_sing$year >= d_sing$g)
  did_sing <- d_sing
  did_sing$g <- as.numeric(did_sing$g); did_sing$g[is.na(did_sing$g)] <- 0
  ref_sing <- didimputation::did_imputation(
    data = did_sing, yname = "y", gname = "g", tname = "year", idname = "id",
    first_stage = ~ x1 + x2 | id + year
  )
  res_sing <- att(y ~ x1 + x2 | id + year, data = d_sing,
                  time = year, timing = g, estimator = "bjs")
  expect_equal(res_sing$estimate, ref_sing$estimate, tolerance = 1e-6)
  expect_equal(res_sing$std.error, ref_sing$std.error, tolerance = 1e-6)

  # Missing covariate in Omega_0 and Omega_1
  d_na <- cov_df
  d_na$x1[5L] <- NA_real_
  d_na$x2[which(d_na$D == 1L)[1L]] <- NA_real_
  did_na <- d_na
  did_na$g <- as.numeric(did_na$g); did_na$g[is.na(did_na$g)] <- 0
  ref_na <- suppressWarnings(didimputation::did_imputation(
    data = did_na, yname = "y", gname = "g", tname = "year", idname = "id",
    first_stage = ~ x1 + x2 | id + year
  ))
  res_na <- suppressWarnings(att(y ~ x1 + x2 | id + year, data = d_na,
                                 time = year, timing = g, estimator = "bjs"))
  expect_equal(res_na$estimate, ref_na$estimate, tolerance = 1e-6)
  expect_equal(res_na$std.error, ref_na$std.error, tolerance = 1e-6)

  # Purely time-invariant / collinear covariate absorbed by unit FE
  a_no_cov <- att(y ~ 1 | id + year, data = cov_df,
                  time = year, timing = g, estimator = "bjs")
  a_const <- att(y ~ x_const | id + year, data = cov_df,
                 time = year, timing = g, estimator = "bjs")
  expect_equal(a_const$estimate, a_no_cov$estimate, tolerance = 1e-10)
  expect_equal(a_const$std.error, a_no_cov$std.error, tolerance = 1e-10)

  # Expressions and factors
  d_expr <- cov_df
  d_expr$ax1 <- abs(d_expr$x1)
  d_expr$r2 <- as.integer(d_expr$region == 2L)
  d_expr$r3 <- as.integer(d_expr$region == 3L)
  b_expr <- att(y ~ abs(x1) + factor(region) | id + year, data = d_expr,
                time = year, timing = g, estimator = "bjs")
  b_cols <- att(y ~ ax1 + r2 + r3 | id + year, data = d_expr,
                time = year, timing = g, estimator = "bjs")
  expect_equal(b_expr$estimate, b_cols$estimate, tolerance = 1e-12)
  expect_equal(b_expr$std.error, b_cols$std.error, tolerance = 1e-12)

  # Custom cluster in event_study()
  d_cl <- cov_df
  d_cl$grp <- (d_cl$id - 1L) %/% 4L + 1L
  did_cl <- d_cl
  did_cl$g <- as.numeric(did_cl$g); did_cl$g[is.na(did_cl$g)] <- 0
  ref_cl <- didimputation::did_imputation(
    data = did_cl, yname = "y", gname = "g", tname = "year", idname = "id",
    first_stage = ~ x1 + x2 | id + year, horizon = TRUE, cluster_var = "grp"
  )
  res_cl <- event_study(y ~ x1 + x2 | id + year, data = d_cl,
                        time = year, timing = g, estimator = "bjs",
                        cluster = ~ grp)
  res_cl_nb <- res_cl[!res_cl$is_baseline, ]
  common <- intersect(res_cl_nb$relative_time, as.integer(ref_cl$term))
  i1 <- match(common, res_cl_nb$relative_time)
  i2 <- match(as.character(common), ref_cl$term)
  expect_equal(res_cl_nb$std.error[i1], ref_cl$std.error[i2], tolerance = 1e-6)
})

test_that("treated-group dummy with scalar timing works with covariates in att()", {
  d_u <- cov_df[is.na(cov_df$g) | cov_df$g == 2005L, ]
  # Build clean universal-timing subset
  set.seed(77L)
  du <- expand.grid(id = 1:50, year = 2001:2008)
  tr <- rep(c(1L, 0L), each = 25L)
  du$treated <- tr[du$id]
  du$x1 <- rnorm(nrow(du))
  du$D  <- as.integer(du$treated == 1L & du$year >= 2005L)
  du$y  <- rnorm(50)[du$id] + 0.1 * du$year + 0.5 * du$x1 + 1.2 * du$D + rnorm(nrow(du), sd = 0.4)

  for (est in c("cs", "bjs")) {
    a_v <- att(y ~ treated + x1 | id + year, data = du,
               time = year, timing = 2005L, estimator = est)
    a_d <- att(y ~ D + x1 | id + year, data = du,
               time = year, estimator = est)
    expect_equal(a_v$estimate, a_d$estimate, tolerance = 1e-12)
    expect_equal(a_v$std.error, a_d$std.error, tolerance = 1e-12)
  }
})
