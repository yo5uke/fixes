# test-fe-ols.R
#
# Agreement tests for the internal FE-OLS engine (.fit_fe_ols) against
# fixest::feols(), which it replaces in the SA, TWM, and FLEX estimators and
# in the contamination-weight auxiliary regressions.
#
# The engine must reproduce, to numerical agreement:
#   - within-OLS coefficients after k-way FE demeaning
#   - the full coefficient VCOV for iid / hetero (HC1) / one-way cluster,
#     including fixest's default small-sample corrections
#     (ssc: adj = TRUE, fixef.K = "nested", cluster.adj = TRUE)
#   - collinearity handling (keep-first, drop-later; dropped terms absent
#     from coef/tidy), NA-row dropping, and singleton FE-level removal
#   - nobs on the estimation sample

library(testthat)
library(fixes)

# Reference: feols with X as a matrix column, replicating the exact call
# pattern used by the estimators (incl. .model_vcov_full precedence).
feols_ref <- function(y, X, fe_df = NULL, cluster = NULL,
                      vcov_type = "HC1", vcov_args = list()) {
  fb <- data.frame(.y = y)
  fb$.X <- X
  fe_terms <- character(0)
  if (!is.null(fe_df)) {
    for (nm in names(fe_df)) {
      fb[[nm]] <- fe_df[[nm]]
      fe_terms <- c(fe_terms, nm)
    }
  }
  fml <- stats::as.formula(
    if (length(fe_terms) > 0L)
      paste0(".y ~ .X | ", paste(fe_terms, collapse = " + "))
    else ".y ~ .X"
  )
  args <- list(fml, data = fb, warn = FALSE, notes = FALSE)
  if (!is.null(cluster)) args$cluster <- cluster
  m <- do.call(fixest::feols, args)

  V <- if (!is.null(cluster) && identical(vcov_type, "HC1")) {
    stats::vcov(m)
  } else {
    tryCatch(stats::vcov(m, vcov = vcov_type, .vcov_args = vcov_args),
             error = function(e) stats::vcov(m))
  }

  fixn <- function(nm) {
    if (ncol(X) == 1L) ifelse(nm == ".X", colnames(X), nm)
    else ifelse(startsWith(nm, ".X"), substring(nm, 3L), nm)
  }
  cf <- stats::coef(m)
  names(cf) <- fixn(names(cf))
  V <- matrix(as.numeric(V), nrow = nrow(V),
              dimnames = list(fixn(rownames(V)), fixn(colnames(V))))
  list(coef = cf, V = V, nobs = stats::nobs(m))
}

# Engine call with the same interface
engine_fit <- function(y, X, fe_df = NULL, cluster = NULL,
                       vcov_type = "HC1", vcov_args = list()) {
  fe_list <- if (is.null(fe_df)) list() else as.list(fe_df)
  cl <- if (is.null(cluster)) NULL else list(cluster)
  fixes:::.fit_fe_ols(y, X, fe_list = fe_list, cluster_vals = cl,
                      vcov_type = vcov_type, vcov_args = vcov_args)
}

expect_engine_matches <- function(y, X, fe_df = NULL, cluster = NULL,
                                  vcov_type = "HC1", vcov_args = list(),
                                  tol = 1e-8) {
  ref <- feols_ref(y, X, fe_df, cluster, vcov_type, vcov_args)
  fit <- engine_fit(y, X, fe_df, cluster, vcov_type, vcov_args)

  expect_equal(names(fit$coef), names(ref$coef))
  expect_equal(fit$coef, ref$coef, tolerance = tol)
  expect_equal(rownames(fit$V), rownames(ref$V))
  expect_equal(fit$V, ref$V, tolerance = tol)
  expect_equal(fit$nobs, ref$nobs)
  expect_equal(fit$tidy$term, names(ref$coef))
  expect_equal(fit$tidy$std.error, unname(sqrt(diag(ref$V))), tolerance = tol)
  invisible(fit)
}

# Shared DGP: unbalanced panel with indicator-style and continuous regressors
make_panel <- function(seed = 7L, n_u = 12L, n_t = 6L) {
  set.seed(seed)
  df <- expand.grid(unit = seq_len(n_u), time = seq_len(n_t))
  df$grp <- ((df$unit - 1L) %% 4L) + 1L
  df$x1  <- rnorm(nrow(df))
  df$x2  <- as.numeric(df$unit <= n_u / 2 & df$time >= n_t - 2L)
  df$y   <- rnorm(n_u)[df$unit] + 0.3 * df$time + 0.5 * df$x1 +
    1.2 * df$x2 + rnorm(nrow(df), sd = 0.7)
  df[-c(5L, 20L, 33L), ]
}

df <- make_panel()
X  <- as.matrix(df[, c("x1", "x2")])
fe2 <- df[, c("unit", "time")]

# ---------------------------------------------------------------------------
# VCOV agreement across types and FE counts
# ---------------------------------------------------------------------------

test_that("engine matches feols: 2-way FE, iid / hetero / HC1", {
  expect_engine_matches(df$y, X, fe2, vcov_type = "iid")
  expect_engine_matches(df$y, X, fe2, vcov_type = "hetero")
  expect_engine_matches(df$y, X, fe2, vcov_type = "HC1")
})

test_that("engine matches feols: one-way cluster (nested and non-nested)", {
  # unit clusters: unit FE nested
  expect_engine_matches(df$y, X, fe2, cluster = df$unit)
  # time clusters: time FE nested
  expect_engine_matches(df$y, X, fe2, cluster = df$time)
  # coarser grouping of units: unit FE nested in grp
  expect_engine_matches(df$y, X, fe2, cluster = df$grp)
  # obs-level random groups: nests nothing
  set.seed(8)
  rgrp <- sample(1:5, nrow(df), replace = TRUE)
  expect_engine_matches(df$y, X, fe2, cluster = rgrp)
})

test_that("engine matches feols: cluster with explicit iid vcov_type wins", {
  expect_engine_matches(df$y, X, fe2, cluster = df$unit, vcov_type = "iid")
})

test_that("engine matches feols: 1 FE, 3 FE, and no FE (intercept)", {
  expect_engine_matches(df$y, X, df[, "unit", drop = FALSE])
  expect_engine_matches(df$y, X, df[, "unit", drop = FALSE],
                        cluster = df$unit)
  df3 <- df
  df3$blk <- ((df3$unit - 1L) %/% 4L) + 1L
  expect_engine_matches(df3$y, X, df3[, c("unit", "time", "blk")])
  expect_engine_matches(df3$y, X, df3[, c("unit", "time", "blk")],
                        cluster = df3$unit)
  # no FE: feols adds an intercept
  expect_engine_matches(df$y, X, fe_df = NULL)
  expect_engine_matches(df$y, X, fe_df = NULL, cluster = df$unit)
  expect_engine_matches(df$y, X, fe_df = NULL, vcov_type = "hetero")
})

# ---------------------------------------------------------------------------
# Collinearity: keep-first / drop-later, matching feols
# ---------------------------------------------------------------------------

test_that("engine drops later collinear and all-zero columns like feols", {
  Xc <- cbind(a = df$x1, b = df$x1, z = 0, c = df$x2)  # b duplicates a; z zero
  ref <- feols_ref(df$y, Xc, fe2)
  fit <- engine_fit(df$y, Xc, fe2)

  expect_equal(names(fit$coef), c("a", "c"))
  expect_equal(names(fit$coef), names(ref$coef))
  expect_equal(fit$coef, ref$coef, tolerance = 1e-8)
  expect_equal(fit$V, ref$V, tolerance = 1e-8)
  expect_false(any(c("b", "z") %in% fit$tidy$term))
})

test_that("engine drops columns absorbed by the FEs like feols", {
  # unit-constant column is absorbed by the unit FE
  Xa <- cbind(x1 = df$x1, ufix = as.numeric(df$unit == 3L))
  # ufix varies over units — but a column constant WITHIN units:
  Xa <- cbind(x1 = df$x1, uconst = rnorm(12)[df$unit])
  ref <- feols_ref(df$y, Xa, fe2)
  fit <- engine_fit(df$y, Xa, fe2)
  expect_equal(names(fit$coef), names(ref$coef))
  expect_equal(fit$coef, ref$coef, tolerance = 1e-8)
  expect_equal(fit$V, ref$V, tolerance = 1e-8)
})

# ---------------------------------------------------------------------------
# Sample handling: NA rows, singleton FE levels, K = 1
# ---------------------------------------------------------------------------

test_that("engine drops NA rows like feols", {
  y2 <- df$y; y2[c(3L, 10L)] <- NA
  X2 <- X;    X2[15L, 1L]    <- NA
  expect_engine_matches(y2, X2, fe2)
  expect_engine_matches(y2, X2, fe2, cluster = df$unit)
})

test_that("engine removes singleton FE observations like feols", {
  df_s <- rbind(df, data.frame(unit = 99L, time = 3L, grp = 1L,
                               x1 = 0.5, x2 = 0, y = 5))
  X_s  <- as.matrix(df_s[, c("x1", "x2")])
  fe_s <- df_s[, c("unit", "time")]
  fit  <- expect_engine_matches(df_s$y, X_s, fe_s)
  expect_equal(fit$nobs, nrow(df))   # singleton row dropped
  expect_engine_matches(df_s$y, X_s, df_s[, "unit", drop = FALSE])
})

test_that("engine handles a single-column X like feols", {
  X1 <- X[, "x1", drop = FALSE]
  expect_engine_matches(df$y, X1, fe2)
  expect_engine_matches(df$y, X1, fe2, cluster = df$unit)
})

# ---------------------------------------------------------------------------
# Fallback paths must also match feols exactly
# ---------------------------------------------------------------------------

test_that("engine falls back to fixest for non-empty vcov_args and matches", {
  va <- list(ssc = fixest::ssc(adj = FALSE))
  ref <- feols_ref(df$y, X, fe2, vcov_type = "iid", vcov_args = va)
  fit <- engine_fit(df$y, X, fe2, vcov_type = "iid", vcov_args = va)
  expect_equal(fit$coef, ref$coef, tolerance = 1e-10)
  expect_equal(fit$V, ref$V, tolerance = 1e-10)
})

test_that("engine falls back to fixest for multiway cluster and matches", {
  ref_m <- {
    fb <- data.frame(.y = df$y); fb$.X <- X
    fb$.f1 <- df$unit; fb$.f2 <- df$time
    m <- fixest::feols(.y ~ .X | .f1 + .f2, data = fb,
                       cluster = list(df$unit, df$time),
                       warn = FALSE, notes = FALSE)
    V <- stats::vcov(m)
    matrix(as.numeric(V), nrow = nrow(V),
           dimnames = lapply(dimnames(V), function(nm) substring(nm, 3L)))
  }
  fit <- fixes:::.fit_fe_ols(df$y, X, fe_list = as.list(fe2),
                             cluster_vals = list(df$unit, df$time))
  expect_equal(fit$V, ref_m, tolerance = 1e-10)
})

test_that("engine falls back to fixest for unsupported vcov strings", {
  # "twoway" errors in vcov() without more info -> .model_vcov_full falls
  # back to the model default; the engine must reproduce that chain.
  ref <- feols_ref(df$y, X, fe2, cluster = df$unit, vcov_type = "nonsense")
  fit <- engine_fit(df$y, X, fe2, cluster = df$unit, vcov_type = "nonsense")
  expect_equal(fit$coef, ref$coef, tolerance = 1e-10)
  expect_equal(fit$V, ref$V, tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# i() dummy expansion equals fixest's i() terms
# ---------------------------------------------------------------------------
# TWM/FLEX covariate paths replace i(time, x, ref) / i(group, x, ref) in the
# formula with .expand_i_dummies(); the expanded columns must reproduce
# fixest's interaction regressors exactly (values, order, and names).

test_that(".expand_i_dummies reproduces fixest i(f, x, ref) coefficients", {
  set.seed(21)
  dd <- expand.grid(unit = 1:20, year = 2001:2008)
  dd$x <- rnorm(20)[dd$unit] + 0.1 * dd$year
  dd$y <- rnorm(20)[dd$unit] + 0.2 * dd$year + 0.7 * dd$x +
    rnorm(nrow(dd), sd = 0.5)

  m_i <- fixest::feols(y ~ i(year, x, ref = 2003) | unit + year,
                       data = dd, warn = FALSE, notes = FALSE)

  Xe <- fixes:::.expand_i_dummies(dd$year, dd$x, 2003, "year", "x")
  dd$.Xe <- Xe
  m_e <- fixest::feols(y ~ .Xe | unit + year,
                       data = dd, warn = FALSE, notes = FALSE)

  expect_equal(unname(coef(m_e)), unname(coef(m_i)), tolerance = 1e-10)
  expect_equal(unname(sqrt(diag(vcov(m_e)))), unname(sqrt(diag(vcov(m_i)))),
               tolerance = 1e-10)
  # names follow the fixest i() convention
  expect_equal(colnames(Xe), names(coef(m_i)))
})

test_that(".expand_i_dummies handles an unobserved ref and character levels", {
  set.seed(22)
  dd <- expand.grid(grp = c("a", "b", "c"), t = 1:6)
  dd$x <- rnorm(nrow(dd))
  dd$y <- rnorm(nrow(dd)) + (dd$grp == "b") * dd$x

  # ref level not present in the data: all levels are expanded
  Xe <- fixes:::.expand_i_dummies(dd$grp, dd$x, "zzz", "grp", "x")
  expect_equal(colnames(Xe), paste0("grp::", c("a", "b", "c"), ":x"))

  m_i <- fixest::feols(y ~ i(grp, x, ref = "a") | t,
                       data = dd, warn = FALSE, notes = FALSE)
  Xe2 <- fixes:::.expand_i_dummies(dd$grp, dd$x, "a", "grp", "x")
  dd$.Xe2 <- Xe2
  m_e <- fixest::feols(y ~ .Xe2 | t, data = dd, warn = FALSE, notes = FALSE)
  expect_equal(unname(coef(m_e)), unname(coef(m_i)), tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# Larger randomized agreement sweep
# ---------------------------------------------------------------------------

test_that("engine matches feols on a larger randomized design", {
  set.seed(11)
  n_u <- 60L; n_t <- 10L
  dl <- expand.grid(unit = seq_len(n_u), time = seq_len(n_t))
  dl <- dl[runif(nrow(dl)) > 0.15, ]
  K  <- 25L
  Xl <- matrix(0, nrow(dl), K)
  for (k in seq_len(K)) {
    gsel <- sample(n_u, 8L); tsel <- sample(n_t, 3L)
    Xl[dl$unit %in% gsel & dl$time %in% tsel, k] <- 1
  }
  colnames(Xl) <- paste0(".ind__", seq_len(K))
  dl$y <- rnorm(n_u)[dl$unit] + 0.2 * dl$time +
    drop(Xl %*% runif(K, -1, 1)) + rnorm(nrow(dl), sd = 0.5)

  fel <- dl[, c("unit", "time")]
  expect_engine_matches(dl$y, Xl, fel, tol = 1e-6)
  expect_engine_matches(dl$y, Xl, fel, cluster = dl$unit, tol = 1e-6)
  expect_engine_matches(dl$y, Xl, fel, vcov_type = "hetero", tol = 1e-6)
})
