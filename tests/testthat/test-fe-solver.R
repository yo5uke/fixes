# test-fe-solver.R
#
# Agreement tests for the internal 2-way fixed-effects solver
# (.solve_fe_2way / solve_fe_2way_cpp) against fixest::feols() + fixef(),
# which it replaces in BJS Step 1.
#
# The solver must replicate fixest's estimation-sample handling:
#   - rows with NA outcome are dropped
#   - observations whose unit or time FE level appears exactly once are
#     removed iteratively before estimation (their FE is not returned)
#
# Only the sums alpha_i + beta_t are identified (normalization-free), so all
# comparisons are on fitted values alpha[u] + beta[t], never on raw FE values.

library(testthat)
library(fixes)

# Fit y ~ 0 | unit + time with fixest and return a lookup for alpha_u + beta_t
fixest_fe_fit <- function(df) {
  m  <- fixest::feols(y ~ 0 | unit + time, data = df,
                      warn = FALSE, notes = FALSE)
  fe <- fixest::fixef(m)
  list(
    model = m,
    sum   = function(u, t) {
      unname(fe$unit[as.character(u)] + fe$time[as.character(t)])
    }
  )
}

solver_fe_fit <- function(df) {
  s <- fixes:::.solve_fe_2way(df$y, df$unit, df$time)
  list(
    fit = s,
    sum = function(u, t) {
      unname(s$alpha[as.character(u)] + s$beta[as.character(t)])
    }
  )
}

# ---------------------------------------------------------------------------
# Test 1 — balanced panel: fitted values match fixest
# ---------------------------------------------------------------------------

test_that("solver matches fixest on a balanced panel", {
  skip_if_not_installed("fixest")
  set.seed(1)
  df <- expand.grid(unit = sprintf("u%02d", 1:20), time = 2001:2010,
                    stringsAsFactors = FALSE)
  df$y <- rnorm(20)[match(df$unit, unique(df$unit))] +
    0.3 * (df$time - 2000) + rnorm(nrow(df), sd = 0.5)

  ref <- fixest_fe_fit(df)
  new <- solver_fe_fit(df)

  expect_equal(new$sum(df$unit, df$time), ref$sum(df$unit, df$time),
               tolerance = 1e-6)
  expect_equal(new$fit$n_kept, stats::nobs(ref$model))
})

# ---------------------------------------------------------------------------
# Test 2 — unbalanced panel: fitted values match fixest
# ---------------------------------------------------------------------------

test_that("solver matches fixest on an unbalanced panel", {
  skip_if_not_installed("fixest")
  set.seed(2)
  df <- expand.grid(unit = 1:30, time = 1:12)
  df$y <- rnorm(30)[df$unit] + 0.2 * df$time + rnorm(nrow(df), sd = 0.4)
  df <- df[-sample(nrow(df), 60L), ]   # drop ~17% of rows

  ref <- fixest_fe_fit(df)
  new <- solver_fe_fit(df)

  expect_equal(new$sum(df$unit, df$time), ref$sum(df$unit, df$time),
               tolerance = 1e-6)
  expect_equal(new$fit$n_kept, stats::nobs(ref$model))
})

# ---------------------------------------------------------------------------
# Test 3 — singleton unit is dropped exactly like fixest
# ---------------------------------------------------------------------------

test_that("solver drops singleton FE levels like fixest", {
  skip_if_not_installed("fixest")
  df <- data.frame(
    unit = c("A", "A", "A", "B", "B", "B", "S"),
    time = c(1, 2, 3, 1, 2, 3, 2),
    y    = c(1.0, 2.0, 3.0, 1.5, 2.5, 3.6, 9.0)
  )

  ref <- fixest_fe_fit(df)
  s   <- fixes:::.solve_fe_2way(df$y, df$unit, df$time)

  # fixest drops the singleton obs: nobs = 6; the solver must agree
  expect_equal(s$n_kept, stats::nobs(ref$model))
  # the singleton unit has no estimated FE (NA on lookup)
  expect_true(is.na(s$alpha["S"]))
  # fitted values on the kept sample match
  kept <- df$unit != "S"
  expect_equal(
    unname(s$alpha[df$unit[kept]] + s$beta[as.character(df$time[kept])]),
    ref$sum(df$unit[kept], df$time[kept]),
    tolerance = 1e-6
  )
})

# ---------------------------------------------------------------------------
# Test 4 — iterative (chained) singleton removal matches fixest
# ---------------------------------------------------------------------------
# Dropping unit-singleton "S" leaves time 2 with a single obs (B@2), which
# must then be dropped as a time singleton, leaving 4 observations.

test_that("solver removes chained singletons iteratively like fixest", {
  skip_if_not_installed("fixest")
  df <- data.frame(
    unit = c("A", "A", "A", "B", "B", "B", "S"),
    time = c(1, 2, 3, 1, 2, 3, 2),
    y    = c(1.0, NA, 3.0, 1.5, 2.5, 3.6, 9.0)
  )

  ref <- fixest_fe_fit(df)
  s   <- fixes:::.solve_fe_2way(df$y, df$unit, df$time)

  expect_equal(s$n_kept, stats::nobs(ref$model))  # 4 obs survive
  expect_true(is.na(s$alpha["S"]))
  expect_false("2" %in% names(s$beta))            # time 2 dropped entirely

  kept <- df$unit %in% c("A", "B") & df$time %in% c(1, 3) & !is.na(df$y)
  expect_equal(
    unname(s$alpha[df$unit[kept]] + s$beta[as.character(df$time[kept])]),
    ref$sum(df$unit[kept], df$time[kept]),
    tolerance = 1e-6
  )
})

# ---------------------------------------------------------------------------
# Test 5 — large sparse panel: agreement and convergence
# ---------------------------------------------------------------------------

test_that("solver matches fixest on a large sparse unbalanced panel", {
  skip_if_not_installed("fixest")
  set.seed(5)
  df <- expand.grid(unit = 1:300, time = 1:40)
  df$y <- rnorm(300, sd = 2)[df$unit] + cumsum(rnorm(40, sd = 0.3))[df$time] +
    rnorm(nrow(df), sd = 1)
  df <- df[runif(nrow(df)) > 0.30, ]   # 30% missing at random

  ref <- fixest_fe_fit(df)
  new <- solver_fe_fit(df)

  expect_true(new$fit$converged)
  expect_equal(new$sum(df$unit, df$time), ref$sum(df$unit, df$time),
               tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# Test 6 — out-of-sample imputation lookup matches fixest predict()
# ---------------------------------------------------------------------------
# BJS Step 2 imputes alpha[u] + beta[t] for (u, t) pairs NOT in the
# estimation sample (treated observations). Verify against predict().

test_that("out-of-sample alpha + beta lookups match fixest predict()", {
  skip_if_not_installed("fixest")
  set.seed(6)
  df <- expand.grid(unit = 1:25, time = 1:10)
  df$y <- rnorm(25)[df$unit] + 0.5 * df$time + rnorm(nrow(df), sd = 0.3)

  # estimation sample: drop "treated" cells (unit <= 10, time >= 6)
  est  <- df[!(df$unit <= 10 & df$time >= 6), ]
  hold <- df[  df$unit <= 10 & df$time >= 6, ]

  m  <- fixest::feols(y ~ 0 | unit + time, data = est,
                      warn = FALSE, notes = FALSE)
  s  <- fixes:::.solve_fe_2way(est$y, est$unit, est$time)

  pred_ref <- stats::predict(m, newdata = hold)
  pred_new <- unname(s$alpha[as.character(hold$unit)] +
                       s$beta[as.character(hold$time)])

  expect_equal(pred_new, pred_ref, tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# Test 7 — end-to-end: BJS estimates unchanged vs the v0.11 fixest pipeline
# ---------------------------------------------------------------------------
# Replicates the pre-v0.12 Step 1-3 (feols + fixef + singleton patch +
# horizon means) inline and requires run_es(estimator = "bjs") to reproduce
# it. This pins the solver swap to the previous behavior.

bjs_reference_fixest <- function(data, outcome, timing, time, unit) {
  is_tr  <- !is.na(data[[timing]]) & data[[time]] >= data[[timing]]
  omega0 <- data[!is_tr, ]
  omega1 <- data[is_tr, ]

  m <- fixest::feols(
    stats::as.formula(paste0(outcome, " ~ 0 | ", unit, " + ", time)),
    data = omega0, warn = FALSE, notes = FALSE
  )
  fe    <- fixest::fixef(m)
  alpha <- fe[[unit]]
  beta  <- fe[[time]]

  u_keys <- as.character(omega1[[unit]])
  t_keys <- as.character(omega1[[time]])
  o0u    <- as.character(omega0[[unit]])
  o0t    <- as.character(omega0[[time]])

  sing <- unique(u_keys[is.na(alpha[u_keys])])
  if (length(sing) > 0L) {
    sel <- o0u %in% sing
    if (any(sel)) {
      r0  <- omega0[[outcome]][sel] - beta[o0t[sel]]
      mns <- tapply(r0, o0u[sel], mean, na.rm = TRUE)
      alpha[names(mns)] <- mns
    }
  }

  tau <- omega1[[outcome]] - unname(alpha[u_keys] + beta[t_keys])
  h   <- as.integer(omega1[[time]] - omega1[[timing]])
  ok  <- !is.na(tau)
  tapply(tau[ok], h[ok], mean)
}

test_that("bjs end-to-end estimates match the fixest-based v0.11 pipeline", {
  skip_if_not_installed("fixest")
  set.seed(42)
  n_units <- 50L
  periods <- 1995:2005
  g_vec   <- c(rep(1998L, 15L), rep(2000L, 15L), rep(2002L, 15L),
               rep(NA_integer_, 5L))

  panel <- expand.grid(id = seq_len(n_units), year = periods)
  panel <- panel[order(panel$id, panel$year), ]
  panel$g     <- g_vec[panel$id]
  panel$treat <- as.integer(!is.na(panel$g) & panel$year >= panel$g)
  panel$y     <- rnorm(n_units)[panel$id] + (panel$year - 1995L) * 0.1 +
    1.5 * panel$treat + rnorm(nrow(panel), sd = 0.3)

  ref <- bjs_reference_fixest(panel, "y", "g", "year", "id")

  res <- run_es(
    data      = panel,
    outcome   = y,
    time      = year,
    timing    = g,
    unit      = id,
    staggered = TRUE,
    estimator = "bjs"
  )

  post <- res[!res$is_baseline & res$relative_time >= 0, ]
  post <- post[order(post$relative_time), ]
  ref  <- ref[as.character(post$relative_time)]

  expect_equal(post$estimate, as.numeric(ref), tolerance = 1e-6)
})

test_that("bjs end-to-end matches the v0.11 pipeline on an unbalanced panel", {
  skip_if_not_installed("fixest")
  set.seed(43)
  n_units <- 40L
  periods <- 1:12
  g_vec   <- c(rep(5L, 12L), rep(8L, 12L), rep(NA_integer_, 16L))

  panel <- expand.grid(id = seq_len(n_units), time = periods)
  panel$g <- g_vec[panel$id]
  panel$y <- rnorm(n_units)[panel$id] + 0.25 * panel$time +
    2 * as.integer(!is.na(panel$g) & panel$time >= panel$g) +
    rnorm(nrow(panel), sd = 0.5)
  panel <- panel[-sample(nrow(panel), 70L), ]   # ~15% missing

  ref <- bjs_reference_fixest(panel, "y", "g", "time", "id")

  res <- suppressWarnings(run_es(
    data      = panel,
    outcome   = y,
    time      = time,
    timing    = g,
    unit      = id,
    staggered = TRUE,
    estimator = "bjs"
  ))

  post <- res[!res$is_baseline & res$relative_time >= 0, ]
  post <- post[order(post$relative_time), ]
  ref  <- ref[as.character(post$relative_time)]

  expect_equal(post$estimate, as.numeric(ref), tolerance = 1e-6)
})

test_that("bjs end-to-end matches the v0.11 pipeline on mpdta", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("did")
  data(mpdta, package = "did", envir = environment())

  mpdta_bjs <- mpdta
  mpdta_bjs$first.treat[mpdta_bjs$first.treat == 0] <- NA_real_

  ref <- bjs_reference_fixest(mpdta_bjs, "lemp", "first.treat",
                              "year", "countyreal")

  res <- suppressWarnings(run_es(
    data      = mpdta_bjs,
    outcome   = lemp,
    time      = year,
    unit      = countyreal,
    timing    = first.treat,
    staggered = TRUE,
    estimator = "bjs"
  ))

  post <- res[!res$is_baseline & res$relative_time >= 0, ]
  post <- post[order(post$relative_time), ]
  ref  <- ref[as.character(post$relative_time)]

  expect_equal(post$estimate, as.numeric(ref), tolerance = 1e-6)
})
