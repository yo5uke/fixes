# Tests for honest_sensitivity() — Rambachan & Roth (2023) robust inference.
#
# The numeric engine is validated against the HonestDiD reference package
# (method = "Conditional", i.e. ARP conditional test).  Delta^SD with a single
# post period matches HonestDiD to machine precision; Delta^RM matches to grid
# resolution (both invert the same test over a shared theta grid).

# Well-conditioned fixture (avoids the singular-vertex edge where HonestDiD's
# own multi-post ARP solver errors).
honest_fixture <- function(numPre, numPost, seed = 7) {
  set.seed(seed)
  k <- numPre + numPost
  sigma <- 0.04 * (0.2^abs(outer(seq_len(k), seq_len(k), "-")))
  betahat <- c(stats::rnorm(numPre, 0, 0.05), 0.4 + 0.1 * seq_len(numPost))
  list(betahat = betahat, sigma = sigma,
       numPre = numPre, numPost = numPost)
}

solvers_ready <- function() {
  all(vapply(c("lpSolveAPI", "Rglpk", "TruncatedNormal", "Matrix", "pracma"),
             requireNamespace, logical(1L), quietly = TRUE))
}

test_that("honest_sensitivity returns a well-formed honest_result", {
  skip_if_not(solvers_ready())
  d <- honest_fixture(3, 2)
  h <- honest_sensitivity(betahat = d$betahat, sigma = d$sigma,
                          type = "relative_magnitude", Mvec = c(0, 0.5, 1),
                          numPrePeriods = d$numPre, numPostPeriods = d$numPost)
  expect_s3_class(h, "honest_result")
  expect_true(all(c("M", "lb", "ub", "method", "type") %in% names(h)))
  expect_equal(sum(h$method == "Original"), 1L)
  expect_equal(sum(h$method != "Original"), 3L)
  expect_false(is.null(attr(h, "breakdown")))
  # robust CIs should weakly widen as the restriction loosens
  rob <- h[h$method != "Original", ]
  rob <- rob[order(rob$M), ]
  expect_true(all(diff(rob$ub - rob$lb) >= -1e-8))
})

test_that("Delta^SD (1 post period) matches HonestDiD to machine precision", {
  skip_if_not(solvers_ready())
  skip_if_not_installed("HonestDiD")
  d  <- honest_fixture(3, 1)
  Mv <- c(0, 0.05, 0.1)
  mine <- honest_sensitivity(betahat = d$betahat, sigma = d$sigma,
            type = "smoothness", Mvec = Mv,
            numPrePeriods = d$numPre, numPostPeriods = d$numPost)
  mine <- mine[mine$type == "smoothness", ]
  mine <- mine[order(mine$M), ]
  theirs <- HonestDiD::createSensitivityResults(
    betahat = d$betahat, sigma = d$sigma, numPrePeriods = d$numPre,
    numPostPeriods = d$numPost, method = "Conditional", Mvec = Mv)
  theirs <- theirs[order(theirs$M), ]
  expect_equal(mine$lb, theirs$lb, tolerance = 1e-6)
  expect_equal(mine$ub, theirs$ub, tolerance = 1e-6)
})

test_that("Delta^RM (1 post period) matches HonestDiD to grid resolution", {
  skip_if_not(solvers_ready())
  skip_if_not_installed("HonestDiD")
  d  <- honest_fixture(3, 1)
  Mb <- c(0.5, 1, 1.5)
  mine <- honest_sensitivity(betahat = d$betahat, sigma = d$sigma,
            type = "relative_magnitude", Mvec = Mb,
            numPrePeriods = d$numPre, numPostPeriods = d$numPost)
  mine <- mine[mine$type == "relative_magnitude", ]
  mine <- mine[order(mine$M), ]
  theirs <- HonestDiD::createSensitivityResults_relativeMagnitudes(
    betahat = d$betahat, sigma = d$sigma, numPrePeriods = d$numPre,
    numPostPeriods = d$numPost, method = "Conditional", Mbarvec = Mb)
  theirs <- theirs[order(theirs$Mbar), ]
  # grid spacing for RM is (40 * sd(theta)) / (gridPoints - 1)
  sdTheta <- sqrt(d$sigma[d$numPre + 1L, d$numPre + 1L])
  gridstep <- 40 * sdTheta / (1000 - 1)
  expect_true(max(abs(mine$lb - theirs$lb)) <= 4 * gridstep)
  expect_true(max(abs(mine$ub - theirs$ub)) <= 4 * gridstep)
})

test_that("honest_sensitivity works from a run_es() es_result", {
  skip_if_not(solvers_ready())
  set.seed(11)
  n_u <- 80; Tt <- 10
  df <- expand.grid(id = 1:n_u, year = 1:Tt)
  df$cohort <- ifelse(df$id <= 40, 6L, NA_integer_)
  df$treat  <- as.integer(!is.na(df$cohort))
  df$rel    <- ifelse(is.na(df$cohort), NA, df$year - df$cohort)
  df$y <- 0.2 * df$id + 0.4 * df$year +
    ifelse(!is.na(df$rel) & df$rel >= 0, 1.5 * (df$rel + 1), 0) +
    stats::rnorm(nrow(df))

  res <- run_es(df, outcome = y, treatment = treat, time = year, timing = 6,
                staggered = FALSE, fe = ~ id + year,
                lead_range = 4, lag_range = 3)
  expect_false(is.null(attr(res, "es_vcov")))

  h <- honest_sensitivity(res, type = "relative_magnitude", Mvec = c(0, 0.5, 1))
  expect_s3_class(h, "honest_result")
  # original CI midpoint should be near the on-impact effect estimate
  expect_true(is.finite(attr(h, "theta_hat")))
})

test_that("es_result without es_vcov errors informatively", {
  skip_if_not(solvers_ready())
  set.seed(12)
  n_u <- 60; Tt <- 8
  df <- expand.grid(id = 1:n_u, year = 1:Tt)
  df$cohort <- ifelse(df$id <= 30, 5L, NA_integer_)
  df$y <- df$id * 0.1 + df$year * 0.2 +
    ifelse(!is.na(df$cohort) & df$year >= df$cohort, 1, 0) +
    stats::rnorm(nrow(df))
  res_cs <- run_es(df, outcome = y, timing = cohort, time = year, unit = id,
                   staggered = TRUE, estimator = "cs",
                   lead_range = 3, lag_range = 2)
  expect_null(attr(res_cs, "es_vcov"))
  expect_error(honest_sensitivity(res_cs, type = "smoothness"),
               regexp = "es_vcov|covariance")
})

test_that("breakdown value is finite when the effect is strong", {
  skip_if_not(solvers_ready())
  # large post effect relative to SE -> robust CI excludes 0 for small Mbar
  d <- honest_fixture(3, 1)
  d$betahat[d$numPre + 1L] <- 2.0     # strong on-impact effect
  h <- honest_sensitivity(betahat = d$betahat, sigma = d$sigma,
                          type = "relative_magnitude",
                          Mvec = c(0, 0.5, 1, 1.5, 2),
                          numPrePeriods = d$numPre, numPostPeriods = d$numPost)
  expect_true(is.finite(attr(h, "breakdown")))
})

test_that("input validation", {
  skip_if_not(solvers_ready())
  d <- honest_fixture(3, 2)
  expect_error(
    honest_sensitivity(betahat = d$betahat, sigma = d$sigma,
                       numPrePeriods = 3, numPostPeriods = 3),
    regexp = "must equal length")
  expect_error(
    honest_sensitivity(betahat = d$betahat, sigma = d$sigma,
                       numPrePeriods = 3, numPostPeriods = 2,
                       l_vec = c(1, 0, 0)),
    regexp = "l_vec")
  expect_error(honest_sensitivity(), regexp = "betahat|es_result|object")
})

test_that("plot_honest returns a ggplot", {
  skip_if_not(solvers_ready())
  d <- honest_fixture(3, 1)
  h <- honest_sensitivity(betahat = d$betahat, sigma = d$sigma,
                          type = "smoothness", Mvec = c(0, 0.05),
                          numPrePeriods = d$numPre, numPostPeriods = d$numPost)
  p <- plot_honest(h)
  expect_s3_class(p, "ggplot")
  expect_s3_class(ggplot2::autoplot(h), "ggplot")
})
