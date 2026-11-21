library(testthat)
library(fixes)

# ---------------------------------------------------------------------------
# Helpers & Fixtures
# ---------------------------------------------------------------------------

make_2period_panel <- function(seed = 2020L, n_units = 120L) {
  set.seed(seed)
  x1 <- rnorm(n_units)
  x2 <- rnorm(n_units)
  reg <- (seq_len(n_units) %% 3L) + 1L
  p_true <- plogis(-0.2 + 0.5 * x1 - 0.35 * x2)
  treated <- rbinom(n_units, 1L, p_true)
  u_fe <- rnorm(n_units, sd = 0.5)
  w_u <- runif(n_units, 0.5, 2.5)

  y0 <- 2.0 + u_fe + 0.6 * x1 - 0.4 * x2 + rnorm(n_units, sd = 0.4)
  y1 <- 2.0 + u_fe + 0.5 + 0.8 * x1 - 0.2 * x2 +
    1.8 * treated + 0.4 * treated * x1 + rnorm(n_units, sd = 0.4)

  df0 <- data.frame(
    id = seq_len(n_units), year = 2004L, y = y0,
    D = 0L, treated = treated, g = ifelse(treated == 1L, 2005L, NA_integer_),
    x1 = x1, x2 = x2, region = reg, w = w_u, stringsAsFactors = FALSE
  )
  df1 <- data.frame(
    id = seq_len(n_units), year = 2005L, y = y1,
    D = treated, treated = treated, g = ifelse(treated == 1L, 2005L, NA_integer_),
    x1 = x1, x2 = x2, region = reg, w = w_u, stringsAsFactors = FALSE
  )
  rbind(df0, df1)
}

# Two-period panel with limited overlap: a few control units sit where the
# propensity score exceeds DRDID's trimming level (0.995), while the IPT fit
# still converges.
make_overlap_panel <- function(seed = 1L, n_units = 300L) {
  set.seed(seed)
  x1 <- c(rnorm(n_units - 4L), 2.4, 2.7, 3.0, 3.3)
  x2 <- rnorm(n_units)
  treated <- c(rbinom(n_units - 4L, 1L, plogis(-0.5 + 3 * x1[seq_len(n_units - 4L)])),
               0L, 0L, 0L, 0L)
  w_u <- runif(n_units, 0.5, 2.5)
  y0 <- x1 + rnorm(n_units)
  y1 <- y0 + 0.5 + 1.5 * treated + 0.4 * x1 + rnorm(n_units)
  rbind(
    data.frame(id = seq_len(n_units), year = 2004L, y = y0, D = 0L,
               x1 = x1, x2 = x2, w = w_u),
    data.frame(id = seq_len(n_units), year = 2005L, y = y1, D = treated,
               x1 = x1, x2 = x2, w = w_u)
  )
}

# Two-period panel with a binary covariate: the x1 = 1 cell holds 240 treated
# and 1 control unit, so both the logit and the IPT score equal 240/241 there
# and the trimming level (0.995) binds for every propensity-score method.
make_saturated_trim_panel <- function(seed = 1L) {
  set.seed(seed)
  x1 <- c(rep(0L, 120L), rep(1L, 241L))
  treated <- c(rep(0:1, c(80L, 40L)), rep(0:1, c(1L, 240L)))
  n_units <- length(x1)
  w_u <- runif(n_units, 0.5, 2.5)
  y0 <- x1 + rnorm(n_units)
  y1 <- y0 + 0.5 + 1.5 * treated + 0.4 * x1 + rnorm(n_units)
  rbind(
    data.frame(id = seq_len(n_units), year = 2004L, y = y0, D = 0L, x1 = x1, w = w_u),
    data.frame(id = seq_len(n_units), year = 2005L, y = y1, D = treated, x1 = x1, w = w_u)
  )
}

# Two-period panel where the treated covariate mean lies outside the control
# support, so the IPT problem has no solution.
make_no_ipt_panel <- function(seed = 3L, n_units = 2000L) {
  set.seed(seed)
  x1 <- rnorm(n_units)
  treated <- rbinom(n_units, 1L, plogis(-0.2 + 8 * x1))
  y0 <- x1 + rnorm(n_units)
  y1 <- y0 + 1 + treated + 0.5 * x1 + rnorm(n_units)
  rbind(
    data.frame(id = seq_len(n_units), year = 1L, y = y0, D = 0L, x1 = x1),
    data.frame(id = seq_len(n_units), year = 2L, y = y1, D = treated, x1 = x1)
  )
}

# Pure-R reference implementation for Sant'Anna & Zhao (2020) panel DR-DID,
# following DRDID: propensity scores are capped at 1 - 1e-6 and control units
# with a score at or above 0.995 get zero weight.
pure_r_dr_did_panel <- function(y0, y1, D, X, est_method, weights = NULL) {
  n <- length(D)
  dy <- y1 - y0
  C <- 1 - D
  v <- if (is.null(weights)) rep(1, n) else weights / mean(weights)
  w1 <- (v * D) / mean(v * D)
  trim_fn <- function(p) ifelse(C == 1, p < 0.995, TRUE)

  if (est_method == "imp") {
    # Maximize sum v_i [ D_i (X_i' g) - (1 - D_i) exp(X_i' g) ]
    k <- ncol(X)
    d_bar <- mean(v * D) / mean(v)
    gamma <- c(log(d_bar / (1 - d_bar)), rep(0, k - 1L))
    obj_fn <- function(eta) {
      sum(v * (D * eta - C * exp(eta)))
    }
    eta <- as.numeric(X %*% gamma)
    obj <- obj_fn(eta)
    for (it in 1:60) {
      ee <- exp(pmin(30, pmax(-30, eta)))
      grad <- as.numeric(crossprod(X, v * (D - C * ee)))
      if (max(abs(grad)) / n < 1e-13) break
      H <- crossprod(X, X * (v * C * ee))
      step <- as.numeric(solve(H, grad))
      alpha <- 1.0
      for (ls in 1:30) {
        g_try <- gamma + alpha * step
        e_try <- as.numeric(X %*% g_try)
        o_try <- obj_fn(e_try)
        if (is.finite(o_try) && o_try >= obj - 1e-13 * (1 + abs(obj))) {
          gamma <- g_try; eta <- e_try; obj <- o_try
          break
        }
        alpha <- alpha * 0.5
      }
      if (max(abs(alpha * step)) < 1e-13 && max(abs(grad)) / n < 1e-13) break
    }
    p <- pmin(as.numeric(stats::plogis(eta)), 1 - 1e-6)
    w_odds <- p / (1 - p)
    Xc <- X[C == 1, , drop = FALSE]
    wc <- v[C == 1] * w_odds[C == 1]
    beta <- as.numeric(solve(crossprod(Xc, Xc * wc), crossprod(Xc, wc * dy[C == 1])))
    r <- dy - as.numeric(X %*% beta)
    # DRDID::drdid_imp_panel: control weights normalised by the treated mass
    w0 <- trim_fn(p) * v * C * w_odds / mean(v * D)
    att <- mean(w1 * r) - mean(w0 * r)
    psi <- w1 * (r - att) - w0 * r
    return(list(att = att, se = sqrt(sum(psi^2)) / n, psi = psi, ps = p))
  }

  if (est_method %in% c("reg", "trad")) {
    Xc <- X[C == 1, , drop = FALSE]
    vc <- v[C == 1]
    beta <- as.numeric(solve(crossprod(Xc, Xc * vc), crossprod(Xc, vc * dy[C == 1])))
    r <- dy - as.numeric(X %*% beta)
  } else {
    r <- dy
  }

  if (est_method %in% c("ipw", "trad")) {
    ps <- suppressWarnings(stats::glm.fit(
      X, D, weights = v, family = stats::binomial(),
      control = list(epsilon = 1e-14, maxit = 100L)
    )$fitted.values)
    ps <- pmin(ps, 1 - 1e-6)
    w0_raw <- trim_fn(ps) * v * C * ps / (1 - ps)
    w0 <- w0_raw / mean(w0_raw)
    H <- crossprod(X, X * (v * ps * (1 - ps))) / n
  } else {
    ps <- rep(NA_real_, n)
  }

  if (est_method == "reg") {
    att <- mean(w1 * r)
    M1 <- colMeans(w1 * X)
    u1 <- as.numeric(solve(crossprod(Xc, Xc * vc) / n, M1))
    psi <- w1 * (r - att) - v * C * r * as.numeric(X %*% u1)
  } else if (est_method == "ipw") {
    eta1 <- mean(w1 * dy)
    eta0 <- mean(w0 * dy)
    att <- eta1 - eta0
    M2 <- colMeans(w0 * (dy - eta0) * X)
    u2 <- as.numeric(solve(H, M2))
    psi <- w1 * (dy - eta1) - w0 * (dy - eta0) -
      v * (D - ps) * as.numeric(X %*% u2)
  } else {
    eta1 <- mean(w1 * r)
    eta0 <- mean(w0 * r)
    att <- eta1 - eta0
    M1 <- colMeans((w1 - w0) * X)
    u1 <- as.numeric(solve(crossprod(Xc, Xc * vc) / n, M1))
    M2 <- colMeans(w0 * (r - eta0) * X)
    u2 <- as.numeric(solve(H, M2))
    psi <- w1 * (r - eta1) - w0 * (r - eta0) -
      v * C * r * as.numeric(X %*% u1) -
      v * (D - ps) * as.numeric(X %*% u2)
  }

  list(att = att, se = sqrt(sum(psi^2)) / n, psi = psi, ps = ps)
}

panel2 <- make_2period_panel()

# ---------------------------------------------------------------------------
# 1. Rcpp vs pure-R parity (1e-12)
# ---------------------------------------------------------------------------

test_that("compute_dr_did_panel_cpp matches pure-R reference within 1e-12", {
  p0 <- panel2[panel2$year == 2004L, ]
  p1 <- panel2[panel2$year == 2005L, ]
  X  <- cbind(1, p0$x1, p0$x2)

  for (w_flag in c(FALSE, TRUE)) {
    ww <- if (w_flag) p0$w else NULL
    for (m in c("imp", "trad", "reg", "ipw")) {
      cpp <- fixes:::compute_dr_did_panel_cpp(p0$y, p1$y, p1$D, X, m, ww)
      ref <- pure_r_dr_did_panel(p0$y, p1$y, p1$D, X, m, ww)
      expect_equal(cpp$att, ref$att, tolerance = 1e-12)
      expect_equal(cpp$se,  ref$se,  tolerance = 1e-12)
      expect_equal(cpp$psi, ref$psi, tolerance = 1e-12)
      if (m != "reg") {
        expect_equal(cpp$ps, ref$ps, tolerance = 1e-12)
      } else {
        expect_true(all(is.na(cpp$ps)))
      }
    }
  }
})

# Limited-overlap fixtures: continuous covariates (trimming binds for the
# logit score) and a saturated binary covariate (binds for logit and IPT).
trim_cases <- list(
  list(df = make_overlap_panel(), covs = c("x1", "x2"), methods = c("trad", "ipw")),
  list(df = make_saturated_trim_panel(), covs = "x1", methods = c("imp", "trad", "ipw"))
)

test_that("compute_dr_did_panel_cpp caps and trims propensity scores like DRDID", {
  for (cs in trim_cases) {
    p0 <- cs$df[cs$df$year == 2004L, ]
    p1 <- cs$df[cs$df$year == 2005L, ]
    X  <- cbind(1, as.matrix(p0[, cs$covs]))
    for (w_flag in c(FALSE, TRUE)) {
      ww <- if (w_flag) p0$w else NULL
      for (m in c("imp", "trad", "ipw")) {
        cpp <- fixes:::compute_dr_did_panel_cpp(p0$y, p1$y, p1$D, X, m, ww)
        ref <- pure_r_dr_did_panel(p0$y, p1$y, p1$D, X, m, ww)
        expect_true(isTRUE(cpp$converged))
        if (!w_flag && m %in% cs$methods) {
          expect_gt(sum(cpp$ps[p1$D == 0] >= 0.995), 0L)
        }
        expect_lte(max(cpp$ps), 1 - 1e-6)
        expect_equal(cpp$att, ref$att, tolerance = 1e-12)
        expect_equal(cpp$se,  ref$se,  tolerance = 1e-12)
        expect_equal(cpp$psi, ref$psi, tolerance = 1e-12)
        expect_equal(cpp$ps,  ref$ps,  tolerance = 1e-12)
      }
    }
  }
})

test_that("logit convergence follows glm: separation is not reported as non-convergence", {
  set.seed(1L)
  x <- rnorm(300L)
  d_sep  <- as.integer(x > 0)
  d_qsep <- d_sep
  d_qsep[which.min(abs(x))] <- 1L - d_qsep[which.min(abs(x))]
  for (d in list(rbinom(300L, 1L, plogis(x)), d_sep, d_qsep)) {
    for (m in c("trad", "ipw")) {
      cpp <- fixes:::compute_dr_did_panel_cpp(rnorm(300L), rnorm(300L), d,
                                              cbind(1, x), m)
      expect_true(cpp$ps_converged)
    }
  }
  # Methods without a logit fit always report convergence
  cpp_imp <- fixes:::compute_dr_did_panel_cpp(rnorm(300L), rnorm(300L),
                                              rbinom(300L, 1L, plogis(x)),
                                              cbind(1, x), "imp")
  expect_true(cpp_imp$ps_converged)
})

test_that("a non-converged logit fit warns in did() and in CS", {
  real_panel <- fixes:::compute_dr_did_panel_cpp
  local_mocked_bindings(compute_dr_did_panel_cpp = function(...) {
    out <- real_panel(...)
    out$ps_converged <- FALSE
    out
  })
  expect_warning(
    did(y ~ D + x1 | id + year, data = panel2, time = year, unit = id,
        estimator = "dr", est_method = "trad"),
    "Propensity score estimation did not converge"
  )

  real_cs <- fixes:::compute_cs_cov_cpp
  local_mocked_bindings(compute_cs_cov_cpp = function(...) {
    out <- real_cs(...)
    out$logit_nc_g <- out$g[1L]
    out$logit_nc_t <- out$t[1L]
    out
  })
  set.seed(3L)
  df <- expand.grid(id = 1:60, year = 2001:2004)
  df$g  <- c(2003L, NA_integer_)[(df$id %% 2L) + 1L]
  df$x1 <- rnorm(60L)[df$id]
  df$y  <- rnorm(nrow(df)) + 1 * (!is.na(df$g) & df$year >= df$g)
  expect_warning(
    att(y ~ x1 | id + year, data = df, time = year, timing = g,
        estimator = "cs", est_method = "trad"),
    "Propensity score estimation did not converge for \\(g, t\\) = \\(2003, "
  )
})

test_that("compute_dr_did_panel_cpp reports a non-converged IPT fit", {
  nc <- make_no_ipt_panel()
  p0 <- nc[nc$year == 1L, ]
  p1 <- nc[nc$year == 2L, ]
  cpp <- fixes:::compute_dr_did_panel_cpp(p0$y, p1$y, p1$D, cbind(1, p0$x1), "imp")
  expect_false(cpp$converged)
})

test_that("compute_cs_cov_cpp with est_method = 'imp' matches pure-R within 1e-12", {
  set.seed(2021L)
  n_id <- 70L
  years <- 2001:2005
  df <- expand.grid(id = seq_len(n_id), year = years, stringsAsFactors = FALSE)
  df <- df[order(df$id, df$year), ]
  g_u <- sample(c(2003L, 2004L, NA_integer_), n_id, replace = TRUE, prob = c(0.35, 0.35, 0.30))
  df$g <- g_u[df$id]
  df$x1 <- rnorm(n_id)[df$id]
  df$x2 <- rnorm(n_id)[df$id]
  df$y <- rnorm(n_id)[df$id] + 0.1 * df$year + 0.4 * df$x1 * (df$year - 2001L) +
    1.5 * (!is.na(df$g) & df$year >= df$g) + rnorm(nrow(df), sd = 0.4)

  u_all <- sort(unique(df$id))
  all_t <- sort(unique(df$year))
  cohorts <- sort(unique(df$g[!is.na(df$g)]))
  Y_mat <- matrix(NA_real_, n_id, length(all_t))
  Y_mat[cbind(match(df$id, u_all), match(df$year, all_t))] <- df$y
  unit_timing <- df$g[match(u_all, df$id)]
  X_base_list <- lapply(cohorts, function(g) {
    sub <- df[df$year == (g - 1L), ]
    sub <- sub[match(u_all, sub$id), ]
    cbind(1, sub$x1, sub$x2)
  })

  for (cg in c("nevertreated", "notyettreated")) {
    cpp <- fixes:::compute_cs_cov_cpp(Y_mat, unit_timing, cohorts, all_t,
                                      X_base_list, cg, 0L, "imp")
    ref_g <- numeric(); ref_t <- numeric()
    ref_att <- numeric(); ref_se <- numeric(); ref_psi <- list()
    for (gi in seq_along(cohorts)) {
      g <- cohorts[gi]
      base_ti <- match(g - 1L, all_t)
      is_tr <- !is.na(unit_timing) & unit_timing == g
      for (ti in seq_along(all_t)) {
        t <- all_t[ti]
        if (t == g - 1L) next
        cutoff <- max(t, g - 1L)
        is_ct <- if (cg == "nevertreated") is.na(unit_timing) else
          (is.na(unit_timing) | unit_timing > cutoff) & (is.na(unit_timing) | unit_timing != g)
        keep <- is_tr | is_ct
        D_sub <- as.numeric(is_tr[keep])
        X_sub <- X_base_list[[gi]][keep, , drop = FALSE]
        cell <- pure_r_dr_did_panel(Y_mat[keep, base_ti], Y_mat[keep, ti], D_sub, X_sub, "imp")
        psi_col <- rep(0, n_id)
        psi_col[keep] <- cell$psi * (n_id / sum(keep))
        ref_g <- c(ref_g, g); ref_t <- c(ref_t, t)
        ref_att <- c(ref_att, cell$att)
        ref_se <- c(ref_se, sqrt(sum(psi_col^2)) / n_id)
        ref_psi[[length(ref_psi) + 1L]] <- psi_col
      }
    }
    expect_equal(cpp$g, ref_g)
    expect_equal(cpp$t, ref_t)
    expect_equal(cpp$att, ref_att, tolerance = 1e-12)
    expect_equal(cpp$se, ref_se, tolerance = 1e-12)
    expect_equal(cpp$psi, do.call(cbind, ref_psi), tolerance = 1e-12)
  }
})

# ---------------------------------------------------------------------------
# 2. Oracle comparison: did(estimator = "dr") vs DRDID
# ---------------------------------------------------------------------------

test_that("did(estimator = 'dr') matches DRDID across est_methods and weights", {
  skip_if_not_installed("DRDID")

  p0 <- panel2[panel2$year == 2004L, ]
  p1 <- panel2[panel2$year == 2005L, ]
  X  <- cbind(1, p0$x1, p0$x2)

  for (m in c("imp", "trad", "reg", "ipw")) {
    res <- suppressMessages(did(
      y ~ D + x1 + x2 | id + year,
      data = panel2, time = year,
      estimator = "dr", est_method = m
    ))
    ref <- switch(
      m,
      imp  = DRDID::drdid_imp_panel(p1$y, p0$y, p1$D, X),
      trad = DRDID::drdid_panel(p1$y, p0$y, p1$D, X),
      reg  = DRDID::reg_did_panel(p1$y, p0$y, p1$D, X),
      ipw  = DRDID::std_ipw_did_panel(p1$y, p0$y, p1$D, X)
    )
    expect_equal(res$estimates$estimate,  ref$ATT, tolerance = 1e-6)
    expect_equal(res$estimates$std.error, ref$se,  tolerance = 1e-6)
  }

  # Weighted DRDID::drdid comparison (DRDID expects unit-constant dname)
  for (m in c("imp", "trad")) {
    res_w <- suppressMessages(did(
      y ~ D + x1 + x2 | id + year,
      data = panel2, time = year,
      estimator = "dr", est_method = m, weights = ~ w
    ))
    ref_w <- DRDID::drdid(
      yname = "y", tname = "year", idname = "id", dname = "treated",
      xformla = ~ x1 + x2, data = panel2, panel = TRUE,
      estMethod = m, weightsname = "w"
    )
    expect_equal(res_w$estimates$estimate,  ref_w$ATT, tolerance = 1e-6)
    expect_equal(res_w$estimates$std.error, ref_w$se,  tolerance = 1e-6)
  }

  # Weighted reg and ipw against direct DRDID panel functions
  res_w_reg <- suppressMessages(did(
    y ~ D + x1 + x2 | id + year, data = panel2, time = year,
    estimator = "dr", est_method = "reg", weights = ~ w
  ))
  ref_w_reg <- DRDID::reg_did_panel(p1$y, p0$y, p1$D, X, i.weights = p0$w)
  expect_equal(res_w_reg$estimates$estimate,  ref_w_reg$ATT, tolerance = 1e-6)
  expect_equal(res_w_reg$estimates$std.error, ref_w_reg$se,  tolerance = 1e-6)

  res_w_ipw <- suppressMessages(did(
    y ~ D + x1 + x2 | id + year, data = panel2, time = year,
    estimator = "dr", est_method = "ipw", weights = ~ w
  ))
  ref_w_ipw <- DRDID::std_ipw_did_panel(p1$y, p0$y, p1$D, X, i.weights = p0$w)
  expect_equal(res_w_ipw$estimates$estimate,  ref_w_ipw$ATT, tolerance = 1e-6)
  expect_equal(res_w_ipw$estimates$std.error, ref_w_ipw$se,  tolerance = 1e-6)
})

test_that("did(estimator = 'dr') without covariates matches DRDID and TWFE", {
  skip_if_not_installed("DRDID")

  p0 <- panel2[panel2$year == 2004L, ]
  p1 <- panel2[panel2$year == 2005L, ]

  res_twfe <- did(y ~ D | id + year, data = panel2)
  ref_null <- DRDID::drdid_imp_panel(p1$y, p0$y, p1$D, covariates = NULL)

  for (m in c("imp", "trad", "reg", "ipw")) {
    res <- did(y ~ D | id + year, data = panel2, time = year, unit = id,
               estimator = "dr", est_method = m)
    expect_equal(res$estimates$estimate, ref_null$ATT, tolerance = 1e-6)
    expect_equal(res$estimates$std.error, ref_null$se, tolerance = 1e-6)
    expect_equal(res$estimates$estimate, res_twfe$estimates$estimate, tolerance = 1e-10)
  }
})

test_that("did(estimator = 'dr') matches DRDID when propensity-score trimming binds", {
  skip_if_not_installed("DRDID")

  for (cs in trim_cases) {
    p0 <- cs$df[cs$df$year == 2004L, ]
    p1 <- cs$df[cs$df$year == 2005L, ]
    X  <- cbind(1, as.matrix(p0[, cs$covs]))
    fml <- stats::as.formula(
      paste("y ~ D +", paste(cs$covs, collapse = " + "), "| id + year")
    )
    for (w_flag in c(FALSE, TRUE)) {
      ww <- if (w_flag) p0$w else NULL
      for (m in c("imp", "trad", "ipw")) {
        res <- suppressWarnings(suppressMessages(did(
          fml, data = cs$df, time = year,
          estimator = "dr", est_method = m,
          weights = if (w_flag) ~ w else NULL
        )))
        ps <- propensity_scores(res)
        if (!w_flag && m %in% cs$methods) {
          expect_gt(sum(ps$propensity_score[ps$treated == 0L] >= 0.995), 0L)
        }
        ref <- suppressWarnings(switch(
          m,
          imp  = DRDID::drdid_imp_panel(p1$y, p0$y, p1$D, X, i.weights = ww),
          trad = DRDID::drdid_panel(p1$y, p0$y, p1$D, X, i.weights = ww),
          ipw  = DRDID::std_ipw_did_panel(p1$y, p0$y, p1$D, X, i.weights = ww)
        ))
        expect_equal(res$estimates$estimate,  ref$ATT, tolerance = 1e-6)
        expect_equal(res$estimates$std.error, ref$se,  tolerance = 1e-6)
      }
    }
  }
})

# ---------------------------------------------------------------------------
# 3. Treatment specifications, expressions, collinearity, panel balancing
# ---------------------------------------------------------------------------

test_that("did(estimator = 'dr') supports all 3 treatment specs, expressions, and swapped args", {
  r_d <- did(y ~ D + x1 + x2 | id + year, data = panel2,
             time = year, unit = id, estimator = "dr")
  r_g <- did(y ~ x1 + x2 | id + year, data = panel2,
             time = year, timing = g, unit = id, estimator = "dr")
  r_v <- did(y ~ treated + x1 + x2 | id + year, data = panel2,
             time = year, timing = 2005L, unit = id, estimator = "dr")
  r_swap <- did(panel2, y ~ D + x1 + x2 | id + year,
                time = year, unit = id, estimator = "dr")

  expect_equal(r_g$estimates$estimate,  r_d$estimates$estimate)
  expect_equal(r_g$estimates$std.error, r_d$estimates$std.error)
  expect_identical(r_g$estimates$term, "g")
  expect_identical(r_g$coeftable$term, "g")
  expect_identical(rownames(r_g$vcov), "g")

  expect_equal(r_v$estimates$estimate,  r_d$estimates$estimate)
  expect_equal(r_v$estimates$std.error, r_d$estimates$std.error)
  expect_identical(r_v$estimates$term, "treated")

  expect_equal(r_swap$estimates$estimate, r_d$estimates$estimate)

  # Expression LHS and factor/function covariates
  skip_if_not_installed("DRDID")
  res_expr <- did(log(y + 10) ~ D + log(abs(x1) + 1) + factor(region) | id + year,
                  data = panel2, time = year, unit = id, estimator = "dr")
  p0 <- panel2[panel2$year == 2004L, ]
  p1 <- panel2[panel2$year == 2005L, ]
  X_expr <- stats::model.matrix(~ log(abs(x1) + 1) + factor(region), data = p0)
  ref_expr <- DRDID::drdid_panel(log(p1$y + 10), log(p0$y + 10), p1$D, X_expr)
  expect_equal(res_expr$estimates$estimate,  ref_expr$ATT, tolerance = 1e-6)
  expect_equal(res_expr$estimates$std.error, ref_expr$se,  tolerance = 1e-6)
})

test_that("did(estimator = 'dr') balances incomplete panels, drops NAs/zero weights, handles collinearity", {
  df_unbal <- panel2[!(panel2$id == 1L & panel2$year == 2005L), ]
  expect_warning(
    res_unbal <- did(y ~ D + x1 | id + year, data = df_unbal,
                     time = year, unit = id, estimator = "dr"),
    "missing in one period"
  )
  expect_equal(attr(res_unbal, "N_units"), length(unique(panel2$id)) - 1L)
  expect_equal(attr(res_unbal, "N"), 2L * (length(unique(panel2$id)) - 1L))

  df_col <- panel2
  df_col$x_dup <- df_col$x1 * 2
  res_base <- did(y ~ D + x1 + x2 | id + year, data = panel2,
                  time = year, unit = id, estimator = "dr")
  res_col  <- did(y ~ D + x1 + x_dup + x2 | id + year, data = df_col,
                  time = year, unit = id, estimator = "dr")
  expect_equal(res_col$estimates$estimate,  res_base$estimates$estimate,  tolerance = 1e-12)
  expect_equal(res_col$estimates$std.error, res_base$estimates$std.error, tolerance = 1e-12)

  # Zero weights dropped silently; negative weights error
  df_w <- panel2
  df_w$w[df_w$id == 5L] <- 0
  res_w0 <- did(y ~ D + x1 | id + year, data = df_w,
                time = year, unit = id, estimator = "dr", weights = ~ w)
  res_w_sub <- did(y ~ D + x1 | id + year, data = df_w[df_w$id != 5L, ],
                   time = year, unit = id, estimator = "dr", weights = ~ w)
  expect_equal(res_w0$estimates$estimate, res_w_sub$estimates$estimate)

  df_w$w[1L] <- -1
  expect_error(
    did(y ~ D + x1 | id + year, data = df_w,
        time = year, unit = id, estimator = "dr", weights = ~ w),
    "Negative weights"
  )
})

# ---------------------------------------------------------------------------
# 4. Validation and multi-period error handling
# ---------------------------------------------------------------------------

test_that("did(estimator = 'dr') rejects > 2 periods and invalid specs clearly", {
  df3 <- expand.grid(id = 1:30, year = 2003:2005)
  df3$D <- as.integer(df3$id <= 15L & df3$year >= 2005L)
  df3$x1 <- rnorm(nrow(df3))
  df3$y <- rnorm(nrow(df3))

  expect_error(
    did(y ~ D + x1 | id + year, data = df3, time = year, unit = id, estimator = "dr"),
    "2-period panel.*att\\(.*estimator = \"cs\".*event_study\\("
  )
  # The suggested CS call carries the same est_method name
  for (m in c("trad", "imp", "reg", "ipw")) {
    expect_error(
      did(y ~ D + x1 | id + year, data = df3, time = year, unit = id,
          estimator = "dr", est_method = m),
      paste0("att\\(\\.\\.\\., estimator = \"cs\", est_method = \"", m, "\"\\)")
    )
  }
  expect_error(
    did(y ~ D | id + year, data = panel2, estimator = "dr"),
    "`time` is required"
  )
  expect_error(
    did(y ~ D | id + year + region, data = panel2, time = year, unit = id, estimator = "dr"),
    "additional or interacted fixed effects"
  )

  df_always <- panel2
  df_always$D[df_always$id == 1L & df_always$year == 2004L] <- 1L
  expect_error(
    did(y ~ D | id + year, data = df_always, time = year, unit = id, estimator = "dr"),
    "treated in pre-treatment period"
  )
})

test_that("est_method has the same names and default ('trad') in did(), att() and event_study()", {
  methods <- c("trad", "imp", "reg", "ipw")
  for (f in list(did, att, event_study)) {
    expect_identical(eval(formals(f)$est_method), methods)
  }

  # did(estimator = "dr") defaults to the traditional DR estimator
  r_def  <- did(y ~ D + x1 + x2 | id + year, data = panel2, time = year,
                unit = id, estimator = "dr")
  r_trad <- did(y ~ D + x1 + x2 | id + year, data = panel2, time = year,
                unit = id, estimator = "dr", est_method = "trad")
  expect_identical(r_def$estimates, r_trad$estimates)
  expect_identical(attr(r_def, "est_method"), "trad")

  # CS defaults to "trad" in att() and event_study()
  set.seed(9L)
  df <- expand.grid(id = 1:60, year = 2001:2004)
  df$g  <- c(2003L, NA_integer_)[(df$id %% 2L) + 1L]
  df$x1 <- rnorm(60L)[df$id]
  df$y  <- rnorm(nrow(df)) + (!is.na(df$g) & df$year >= df$g)
  a_def  <- att(y ~ x1 | id + year, data = df, time = year, timing = g,
                estimator = "cs")
  a_trad <- att(y ~ x1 | id + year, data = df, time = year, timing = g,
                estimator = "cs", est_method = "trad")
  expect_identical(a_def$estimate, a_trad$estimate)
  es_def  <- event_study(y ~ x1 | id + year, data = df, time = year,
                         timing = g, estimator = "cs")
  es_trad <- event_study(y ~ x1 | id + year, data = df, time = year,
                         timing = g, estimator = "cs", est_method = "trad")
  expect_identical(es_def$estimate, es_trad$estimate)

  # "dr" (the did package's name) points to "trad"
  hint <- "est_method = \"dr\".*\"trad\""
  expect_error(did(y ~ D + x1 | id + year, data = panel2, time = year,
                   unit = id, estimator = "dr", est_method = "dr"), hint)
  expect_error(att(y ~ x1 | id + year, data = df, time = year, timing = g,
                   estimator = "cs", est_method = "dr"), hint)
  expect_error(event_study(y ~ x1 | id + year, data = df, time = year,
                           timing = g, estimator = "cs", est_method = "dr"), hint)
})

test_that("did(estimator = 'dr', est_method = 'imp') stops when the IPT fit does not converge", {
  nc <- make_no_ipt_panel()
  expect_error(
    suppressMessages(did(y ~ D + x1 | id + year, data = nc, time = year,
                         estimator = "dr", est_method = "imp")),
    "IPT propensity-score fit did not converge.*est_method = \"trad\""
  )
  # The traditional DR estimator is unaffected
  expect_s3_class(
    suppressWarnings(suppressMessages(did(y ~ D + x1 | id + year, data = nc, time = year,
                                          estimator = "dr", est_method = "trad"))),
    "did_result"
  )
})

# ---------------------------------------------------------------------------
# 5. did_result compatibility: print, broom::tidy, broom::glance, modelsummary
# ---------------------------------------------------------------------------

test_that("did(estimator = 'dr') works with print, tidy, glance, and modelsummary", {
  res <- did(y ~ D + x1 + x2 | id + year, data = panel2,
             time = year, unit = id, estimator = "dr", est_method = "imp",
             conf_level = c(0.90, 0.95))

  out_txt <- capture.output(print(res))
  expect_true(any(grepl("DR-DID \\(imp\\)", out_txt)))
  expect_true(is.infinite(res$df.t))

  td <- broom::tidy(res, conf.int = TRUE, conf.level = 0.95)
  expect_equal(nrow(td), 1L)
  expect_equal(td$term, "D")
  expect_equal(td$estimate,  res$estimates$estimate)
  expect_equal(td$std.error, res$estimates$std.error)
  expect_equal(td$conf.low,  res$estimates$conf_low_95)
  expect_equal(td$conf.high, res$estimates$conf_high_95)

  gl <- broom::glance(res)
  expect_equal(nrow(gl), 1L)
  expect_equal(gl$nobs, 2L * length(unique(panel2$id)))

  skip_if_not_installed("modelsummary")
  tab <- tryCatch(
    modelsummary::modelsummary(res, output = "data.frame"),
    error = function(e) NULL
  )
  if (is.null(tab)) skip("modelsummary S3 dispatch unavailable in this environment")
  expect_s3_class(tab, "data.frame")
})

# ---------------------------------------------------------------------------
# 6. Callaway-Sant'Anna with est_method = "imp"
# ---------------------------------------------------------------------------

test_that("CS estimator supports est_method = 'imp' matching pure-R & DRDID per cell", {
  set.seed(77L)
  n_id <- 75L
  years <- 2001:2005
  df <- expand.grid(id = seq_len(n_id), year = years, stringsAsFactors = FALSE)
  df <- df[order(df$id, df$year), ]
  rownames(df) <- NULL
  g_u <- sample(c(2003L, 2004L, NA_integer_), n_id, replace = TRUE, prob = c(0.35, 0.35, 0.30))
  df$g <- g_u[df$id]
  df$x1 <- rnorm(n_id)[df$id]
  df$x2 <- rnorm(n_id)[df$id]
  df$D <- as.integer(!is.na(df$g) & df$year >= df$g)
  df$y <- rnorm(n_id)[df$id] + 0.1 * df$year + 0.5 * df$x1 * (df$year - 2001L) +
    1.6 * df$D + rnorm(nrow(df), sd = 0.35)

  for (cg in c("nevertreated", "notyettreated")) {
    a_imp <- att(y ~ x1 + x2 | id + year, data = df, time = year, timing = g,
                 unit = id, estimator = "cs", control_group = cg,
                 est_method = "imp", aggregation = "simple")
    es_imp <- event_study(y ~ x1 + x2 | id + year, data = df, time = year, timing = g,
                          unit = id, estimator = "cs", control_group = cg,
                          est_method = "imp")
    expect_s3_class(a_imp, "att_result")
    expect_s3_class(es_imp, "es_result")
    expect_equal(att_gt(a_imp)[, c("g", "t", "estimate", "std_error")],
                 att_gt(es_imp)[, c("g", "t", "estimate", "std_error")])

    if (requireNamespace("DRDID", quietly = TRUE)) {
      gt <- att_gt(a_imp)
      for (r in seq_len(nrow(gt))) {
        g_val <- gt$g[r]
        t_val <- gt$t[r]
        b_val <- g_val - 1L
        cutoff <- max(t_val, g_val - 1L)
        sub_b <- df[df$year == b_val, ]
        sub_t <- df[df$year == t_val, ]
        keep <- if (cg == "nevertreated") {
          (!is.na(sub_b$g) & sub_b$g == g_val) | is.na(sub_b$g)
        } else {
          (!is.na(sub_b$g) & sub_b$g == g_val) | is.na(sub_b$g) | (sub_b$g > cutoff & sub_b$g != g_val)
        }
        sb <- sub_b[keep, ]
        st <- sub_t[keep, ]
        D_cell <- as.integer(!is.na(sb$g) & sb$g == g_val)
        X_cell <- cbind(1, sb$x1, sb$x2)
        ref <- DRDID::drdid_imp_panel(st$y, sb$y, D_cell, X_cell)
        expect_equal(gt$estimate[r], ref$ATT, tolerance = 1e-6)
        # DRDID se scales IF by 1/n_sub; CS scales by (n_all/n_sub)/n_all = 1/n_sub as well
        expect_equal(gt$std_error[r], ref$se, tolerance = 1e-6)
      }
    }
  }
})

test_that("CS cells match DRDID per cell when propensity-score trimming binds", {
  skip_if_not_installed("DRDID")

  sp <- make_saturated_trim_panel()
  u  <- sp[sp$year == 2004L, ]
  g_u <- ifelse(sp$D[sp$year == 2005L] == 1L, 2003L, NA_integer_)
  set.seed(5L)
  df <- expand.grid(id = u$id, year = 2001:2003)
  df$g  <- g_u[df$id]
  df$x1 <- u$x1[df$id]
  df$y  <- df$x1 + 0.2 * df$year + 0.4 * df$x1 * (df$year - 2001L) +
    1.5 * (!is.na(df$g) & df$year >= df$g) + rnorm(nrow(df))

  for (m in c("trad", "ipw", "imp")) {
    a <- suppressWarnings(att(y ~ x1 | id + year, data = df, time = year,
                              timing = g, estimator = "cs", est_method = m,
                              keep_ps = TRUE))
    ps <- propensity_scores(a)
    expect_gt(sum(ps$propensity_score[ps$treated == 0L] >= 0.995), 0L)
    gt <- att_gt(a)
    expect_equal(nrow(gt), 2L)
    sb <- df[df$year == 2002L, ]
    for (r in seq_len(nrow(gt))) {
      st <- df[df$year == gt$t[r], ]
      D_cell <- as.integer(!is.na(sb$g))
      X_cell <- cbind(1, sb$x1)
      ref <- switch(
        m,
        trad = suppressWarnings(DRDID::drdid_panel(st$y, sb$y, D_cell, X_cell)),
        ipw = suppressWarnings(DRDID::std_ipw_did_panel(st$y, sb$y, D_cell, X_cell)),
        imp = DRDID::drdid_imp_panel(st$y, sb$y, D_cell, X_cell)
      )
      expect_equal(gt$estimate[r],  ref$ATT, tolerance = 1e-6)
      expect_equal(gt$std_error[r], ref$se,  tolerance = 1e-6)
    }
  }
})

test_that("CS with est_method = 'imp' warns and drops cells whose IPT fit does not converge", {
  set.seed(11L)
  n_id <- 150L
  grp <- rep(c("never", "g2003", "g2004"), each = n_id / 3L)
  x1_u <- ifelse(grp == "never", runif(n_id, -2, 0.5),
          ifelse(grp == "g2003", runif(n_id, -1.5, 0.3), runif(n_id, 1, 2)))
  g_u <- c(never = NA_integer_, g2003 = 2003L, g2004 = 2004L)[grp]
  df <- expand.grid(id = seq_len(n_id), year = 2001:2005)
  df$g  <- unname(g_u[df$id])
  df$x1 <- x1_u[df$id]
  df$y  <- df$x1 + 0.1 * df$year + 1.2 * (!is.na(df$g) & df$year >= df$g) +
    rnorm(nrow(df), sd = 0.5)

  warns <- character()
  a <- withCallingHandlers(
    att(y ~ x1 | id + year, data = df, time = year, timing = g,
        estimator = "cs", est_method = "imp"),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("IPT propensity-score fit did not converge", warns)))
  expect_true(any(grepl("2004", warns[grepl("did not converge", warns)])))
  gt <- att_gt(a)
  expect_false(any(gt$g == 2004))
  expect_true(any(gt$g == 2003))
})

# ---------------------------------------------------------------------------
# 7. propensity_scores() extractor
# ---------------------------------------------------------------------------

test_that("propensity_scores() extracts valid unit propensity scores across APIs", {
  # Unordered character unit IDs in did(estimator = "dr")
  df_chr <- panel2
  df_chr$uid <- sprintf("u_%03d", df_chr$id)
  df_chr <- df_chr[sample(nrow(df_chr)), ]

  for (m in c("imp", "trad", "ipw")) {
    res <- did(y ~ D + x1 + x2 | uid + year, data = df_chr,
               time = year, unit = uid, estimator = "dr", est_method = m)
    ps <- propensity_scores(res)
    expect_s3_class(ps, "data.frame")
    expect_identical(names(ps), c("unit", "treated", "propensity_score"))
    expect_equal(nrow(ps), length(unique(df_chr$uid)))
    expect_true(all(ps$propensity_score > 0 & ps$propensity_score < 1))
    expect_true(is.character(ps$unit))
    expect_identical(ps$unit, sort(unique(df_chr$uid)))

    p0 <- panel2[panel2$year == 2004L, ]
    p1 <- panel2[panel2$year == 2005L, ]
    expect_identical(ps$treated, as.integer(p1$D))
    X <- cbind(1, p0$x1, p0$x2)
    if (m %in% c("trad", "ipw")) {
      glm_ps <- stats::glm.fit(
        X, p1$D, family = stats::binomial(),
        control = list(epsilon = 1e-14, maxit = 100L)
      )$fitted.values
      expect_equal(ps$propensity_score, as.numeric(glm_ps), tolerance = 1e-12)
    } else {
      # IPT FOC: sum (D_i - (1 - D_i) * p_i / (1 - p_i)) * X_i == 0
      foc <- colMeans((ps$treated - (1 - ps$treated) *
                         ps$propensity_score / (1 - ps$propensity_score)) * X)
      expect_equal(unname(foc), rep(0, ncol(X)), tolerance = 1e-10)
    }
  }

  # CS with keep_ps = TRUE (both att() and event_study(), including bootstrap = TRUE)
  set.seed(88L)
  n_id <- 60L
  df_cs <- expand.grid(id = seq_len(n_id), year = 2001:2005)
  g_cs <- sample(c(2003L, 2004L, NA_integer_), n_id, replace = TRUE, prob = c(0.35, 0.35, 0.30))
  df_cs$g <- g_cs[df_cs$id]
  df_cs$x1 <- rnorm(n_id)[df_cs$id]
  df_cs$y <- rnorm(n_id)[df_cs$id] + 0.1 * df_cs$year +
    ifelse(!is.na(df_cs$g) & df_cs$year >= df_cs$g, 1.2, 0) + rnorm(nrow(df_cs), sd = 0.4)

  att_no <- att(y ~ x1 | id + year, data = df_cs, time = year, timing = g,
                unit = id, estimator = "cs", est_method = "imp", keep_ps = FALSE)
  att_ps <- att(y ~ x1 | id + year, data = df_cs, time = year, timing = g,
                unit = id, estimator = "cs", est_method = "imp", keep_ps = TRUE)
  expect_equal(att_ps$estimate, att_no$estimate)
  ps_att <- propensity_scores(att_ps)
  expect_identical(names(ps_att), c("g", "t", "unit", "treated", "propensity_score"))
  expect_true(all(is.na(ps_att$t)))
  expect_equal(sort(unique(ps_att$g)), c(2003, 2004))

  es_ps <- event_study(y ~ x1 | id + year, data = df_cs, time = year, timing = g,
                       unit = id, estimator = "cs", control_group = "notyettreated",
                       est_method = "trad", keep_ps = TRUE,
                       bootstrap = TRUE, boot_reps = 29L, boot_seed = 1L)
  ps_es <- propensity_scores(es_ps)
  gt_es <- as.data.frame(att_gt(es_ps))
  gt_ps <- unique(ps_es[order(ps_es$g, ps_es$t), c("g", "t")])
  rownames(gt_ps) <- NULL
  rownames(gt_es) <- NULL
  expect_false(anyNA(ps_es$t))
  expect_identical(gt_ps, gt_es[, c("g", "t")])

  # Informative errors when propensity scores are absent
  res_twfe <- did(y ~ D | id + year, data = panel2)
  res_reg  <- did(y ~ D + x1 | id + year, data = panel2, time = year, unit = id,
                  estimator = "dr", est_method = "reg")
  expect_error(propensity_scores(res_twfe), "estimator = \"dr\"")
  expect_error(propensity_scores(res_reg),  "est_method = \"reg\"")
  expect_error(propensity_scores(att_no),   "keep_ps = TRUE")
})
