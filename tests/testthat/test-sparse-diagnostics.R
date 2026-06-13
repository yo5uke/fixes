# test-sparse-diagnostics.R
#
# Diagnostics for silent failures in sparse / unbalanced panels:
#   - CS names + stores cohorts dropped for an unobserved base period
#   - SA warns when the saturated design is degenerate
#   - BJS exposes the count of un-imputable treated observations

library(testthat)
library(fixes)

# ---------------------------------------------------------------------------
# (3a) CS dropped-cohort diagnostic
# ---------------------------------------------------------------------------

test_that("CS names and stores cohorts whose base period (g-1) is unobserved", {
  set.seed(3)
  units  <- 1:80
  years  <- 2000:2012
  cohort <- c(rep(2005L, 20), rep(2007L, 20), rep(2010L, 20),
              rep(NA_integer_, 20))
  coh_of <- setNames(cohort, units)

  df <- expand.grid(id = units, year = years)
  df$cohort <- coh_of[as.character(df$id)]
  df$y <- rnorm(nrow(df)) +
    ifelse(!is.na(df$cohort) & df$year >= df$cohort, 1, 0)

  # Remove cohort-2007 units' base-period (2006) rows only.  2006 still exists
  # globally (other units observe it), so this is the silent per-cohort case.
  df2 <- df[!(df$cohort %in% 2007L & df$year == 2006L), ]

  expect_warning(
    res <- run_es(df2, outcome = y, time = year, timing = cohort,
                  unit = id, estimator = "cs"),
    "2007"
  )
  expect_true(2007 %in% attr(res, "dropped_cohorts"))
  # Surviving cohorts still produce estimates.
  expect_true(nrow(res) > 0L)
})

test_that("CS reports an empty dropped_cohorts on a clean balanced panel", {
  set.seed(4)
  units  <- 1:60
  years  <- 2000:2010
  cohort <- c(rep(2004L, 20), rep(2007L, 20), rep(NA_integer_, 20))
  coh_of <- setNames(cohort, units)
  df <- expand.grid(id = units, year = years)
  df$cohort <- coh_of[as.character(df$id)]
  df$y <- rnorm(nrow(df)) +
    ifelse(!is.na(df$cohort) & df$year >= df$cohort, 1, 0)

  res <- run_es(df, outcome = y, time = year, timing = cohort,
                unit = id, estimator = "cs")
  expect_length(attr(res, "dropped_cohorts"), 0L)
})

# ---------------------------------------------------------------------------
# (3b) SA degeneracy warning
# ---------------------------------------------------------------------------

test_that("SA warns on a degenerate (over-saturated) design", {
  # Sparse, irregularly-observed monthly panel (only ~half the unit-periods
  # observed): most cohort x relative-time interactions become collinear with
  # the unit/time FEs and fixest drops them, mirroring the motivating case.
  set.seed(11)
  units   <- 1:6
  periods <- 1:12
  coh_of  <- setNames(c(3L, 5L, 7L, 9L, NA_integer_, NA_integer_), units)
  df <- expand.grid(id = units, period = periods)
  df$cohort <- coh_of[as.character(df$id)]
  df$y <- rnorm(nrow(df))
  df <- df[runif(nrow(df)) < 0.5, ]            # drop ~half the observations

  ws <- character(0)
  withCallingHandlers(
    run_es(df, outcome = y, time = period, timing = cohort, unit = id,
           estimator = "sa", fe = ~ id + period),
    warning = function(w) {
      ws <<- c(ws, conditionMessage(w)); invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("degenerate", ws)))
})

test_that("SA does not warn 'degenerate' on a healthy panel", {
  set.seed(6)
  units  <- 1:60
  years  <- 2000:2010
  cohort <- c(rep(2004L, 20), rep(2007L, 20), rep(NA_integer_, 20))
  coh_of <- setNames(cohort, units)
  df <- expand.grid(id = units, year = years)
  df$cohort <- coh_of[as.character(df$id)]
  df$y <- rnorm(nrow(df)) +
    ifelse(!is.na(df$cohort) & df$year >= df$cohort, 1, 0)

  ws <- character(0)
  withCallingHandlers(
    run_es(df, outcome = y, time = year, timing = cohort, unit = id,
           estimator = "sa", fe = ~ id + year),
    warning = function(w) {
      ws <<- c(ws, conditionMessage(w)); invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("degenerate", ws)))
})

# ---------------------------------------------------------------------------
# (3c) BJS un-imputable count exposed
# ---------------------------------------------------------------------------

test_that("BJS exposes n_unimputed for treated obs with no pre-period", {
  set.seed(8)
  units  <- 1:40
  years  <- 2000:2010
  cohort <- rep(NA_integer_, 40)
  cohort[2:20]  <- 2005L          # normal treated cohort (has pre-periods)
  cohort[1]     <- 2000L          # adopts at the first period -> no pre-period
  coh_of <- setNames(cohort, units)
  df <- expand.grid(id = units, year = years)
  df$cohort <- coh_of[as.character(df$id)]
  df$y <- rnorm(nrow(df)) +
    ifelse(!is.na(df$cohort) & df$year >= df$cohort, 1, 0)

  res <- suppressWarnings(
    run_es(df, outcome = y, time = year, timing = cohort, unit = id,
           estimator = "bjs")
  )
  # Unit 1 contributes 11 un-imputable treated observations (2000-2010).
  expect_equal(attr(res, "n_unimputed"), 11L)
  expect_true(attr(res, "n_treated_obs") >= 11L)
})
