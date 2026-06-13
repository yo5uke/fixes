# test-rel-time.R
#
# A pre-built event-time column passed via `rel_time` must be used verbatim and
# agree with a direct feols(y ~ i(rel_time, treatment, ref) | fe) at machine
# precision.

library(testthat)
library(fixes)

make_stagger <- function(seed = 7L) {
  set.seed(seed)
  units   <- 1:60
  years   <- 2000:2012
  cohorts <- c(rep(2004L, 15L), rep(2007L, 15L), rep(2010L, 15L),
               rep(NA_integer_, 15L))            # last 15 never-treated
  coh_of  <- setNames(cohorts, units)

  df <- expand.grid(id = units, year = years)
  df <- df[order(df$id, df$year), ]
  df$cohort <- coh_of[as.character(df$id)]
  df$treat  <- as.integer(!is.na(df$cohort))     # unit-level treated indicator
  df$rel    <- df$year - df$cohort               # NA for never-treated

  unit_fe <- rnorm(length(units))[df$id]
  eff     <- ifelse(!is.na(df$rel) & df$rel >= 0, 0.4 * (df$rel + 1), 0)
  df$y <- unit_fe + (df$year - 2000) * 0.1 + eff + rnorm(nrow(df), sd = 0.5)
  df
}

test_that("rel_time matches direct feols i(rel, treat, ref) to machine eps", {
  df <- make_stagger()

  res <- run_es(df, outcome = y, treatment = treat, time = year,
                rel_time = rel, fe = ~ id + year, unit = id, baseline = -1L)

  # Reference: replicate run_es's NA-control patching (NA -> baseline) so the
  # never-treated rows stay in the sample but contribute zero (treat = 0).
  ref_df      <- df
  ref_df$k    <- ref_df$rel
  ref_df$k[is.na(ref_df$k)] <- -1L
  ref <- fixest::feols(y ~ i(k, treat, ref = -1) | id + year,
                       data = ref_df, vcov = "HC1")
  rt  <- broom::tidy(ref)
  rt$relative_time <- as.integer(sub(".*::(-?\\d+):.*", "\\1", rt$term))

  m <- merge(res[!res$is_baseline, c("relative_time", "estimate", "std.error")],
             rt[, c("relative_time", "estimate", "std.error")],
             by = "relative_time", suffixes = c(".pkg", ".ref"))
  expect_equal(nrow(m), nrow(rt))
  expect_equal(m$estimate.pkg,  m$estimate.ref,  tolerance = 1e-10)
  expect_equal(m$std.error.pkg, m$std.error.ref, tolerance = 1e-10)
})

test_that("rel_time normalises the time=event_time, timing=0 idiom", {
  df <- make_stagger()
  # Give controls a defined rel value so the legacy hack keeps them too
  # (otherwise the hack silently drops NA-rel control rows).
  df$rel_full <- ifelse(is.na(df$rel), -1L, df$rel)

  res_new  <- run_es(df, outcome = y, treatment = treat, time = year,
                     rel_time = rel_full, fe = ~ id + year, baseline = -1L)
  res_hack <- run_es(df, outcome = y, treatment = treat, time = rel_full,
                     timing = 0L, fe = ~ id + year, baseline = -1L)

  expect_equal(res_new$estimate,  res_hack$estimate,  tolerance = 1e-10)
  expect_equal(res_new$std.error, res_hack$std.error, tolerance = 1e-10)
})

test_that("rel_time guards against incompatible options", {
  df <- make_stagger()
  expect_error(
    run_es(df, outcome = y, treatment = treat, time = year, rel_time = rel,
           unit = id, estimator = "cs", timing = cohort),
    "rel_time.*twfe"
  )
  expect_error(
    run_es(df, outcome = y, treatment = treat, time = year, rel_time = rel,
           timing = cohort, staggered = TRUE, fe = ~ id + year),
    "staggered"
  )
  expect_error(
    run_es(df, outcome = y, treatment = treat, time = year, rel_time = rel,
           unit = id, time_transform = TRUE, fe = ~ id + year),
    "time_transform"
  )
})

test_that("rel_time keeps never-treated controls in the estimation sample", {
  df <- make_stagger()
  res <- run_es(df, outcome = y, treatment = treat, time = year,
                rel_time = rel, fe = ~ id + year, unit = id)
  # All rows are retained (controls patched, not dropped).
  expect_equal(attr(res, "N"), nrow(df))
})
