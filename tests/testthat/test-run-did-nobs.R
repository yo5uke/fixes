# test-run-did-nobs.R
#
# run_did() must report the *estimation* sample (after feols drops NA rows),
# matching nobs(model) and run_es(), not nrow(data).

library(testthat)
library(fixes)

make_na_data <- function(seed = 1L, n_na = 100L) {
  set.seed(seed)
  df <- expand.grid(id = 1:50, year = 2000:2010)
  df <- df[order(df$id, df$year), ]
  df$D <- as.integer(df$id <= 25 & df$year >= 2005)
  unit_fe <- rnorm(50)[df$id]
  df$y <- unit_fe + (df$year - 2000) * 0.1 + 0.3 * df$D + rnorm(nrow(df), sd = 0.5)
  df$y[sample(nrow(df), n_na)] <- NA  # inject missingness
  df
}

test_that("run_did N equals nobs(model), not nrow(data), under missingness", {
  df  <- make_na_data()
  res <- run_did(df, outcome = y, treatment = D, fe = ~ id + year,
                 unit = id, time = year)

  expect_lt(attr(res, "N"), nrow(df))                 # NA rows were dropped
  expect_equal(attr(res, "N"), stats::nobs(res$model))
  expect_equal(attr(res, "N"), broom::glance(res)$nobs)
})

test_that("run_did N_treated counts treated obs in the estimation sample only", {
  df  <- make_na_data()
  res <- run_did(df, outcome = y, treatment = D, fe = ~ id + year)

  # Reconstruct the fitted sample the same way run_did does.
  used <- seq_len(nrow(df))
  for (s in res$model$obs_selection) used <- used[s]
  expect_equal(attr(res, "N_treated"), sum(df$D[used] == 1L, na.rm = TRUE))
  expect_lt(attr(res, "N_treated"), sum(df$D, na.rm = TRUE))  # < full treated
})

test_that("run_did and run_es report the same N on the same data/spec", {
  df  <- make_na_data()
  did <- run_did(df, outcome = y, treatment = D, fe = ~ id + year)

  # run_es classic, non-staggered: timing 2005, treatment = unit-level group.
  df$treat_unit <- as.integer(df$id <= 25)
  es <- suppressWarnings(
    run_es(df, outcome = y, treatment = treat_unit, time = year,
           timing = 2005, fe = ~ id + year, unit = id)
  )
  expect_equal(attr(did, "N"), attr(es, "N"))
})

test_that("with no missingness N still equals nrow (no behaviour change)", {
  df  <- make_na_data(n_na = 0L)
  res <- run_did(df, outcome = y, treatment = D, fe = ~ id + year, unit = id)
  expect_equal(attr(res, "N"), nrow(df))
  expect_equal(attr(res, "N_treated"), sum(df$D))
})
