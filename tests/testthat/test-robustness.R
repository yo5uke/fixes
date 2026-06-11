# ==============================================================================
# Robustness regression tests (v0.11.2 fixes)
#
# Covers the silent-corruption bugs fixed in 0.11.2:
#   1. CS with character/factor unit IDs (as.integer() destroyed them)
#   2. CS with a cohort at timing == 0 (collided with the never-treated
#      sentinel and leaked treated units into the control group)
#   3. classic staggered TWFE dropping all never-treated rows (..k = NA)
#   4. sunab dropping all never-treated rows (fixest::sunab has no NA cohort)
#   5. informative error for non-consecutive time grids (CS)
#   6. integer-valued time/timing validation
# ==============================================================================

make_robust_panel <- function(n_units = 60L, periods = 2001:2010, seed = 1L) {
  set.seed(seed)
  timing_by_unit <- sample(c(2005L, 2007L, NA), n_units, replace = TRUE)
  df <- data.frame(
    id   = rep(seq_len(n_units), each = length(periods)),
    year = rep(periods, times = n_units)
  )
  df$timing  <- timing_by_unit[df$id]
  rel        <- ifelse(is.na(df$timing), NA, df$year - df$timing)
  df$y       <- rnorm(nrow(df)) + ifelse(!is.na(rel) & rel >= 0, 1, 0)
  df$treated <- as.integer(!is.na(df$timing))
  df
}

test_that("cs: character and factor unit IDs give identical results to integer IDs", {
  df <- make_robust_panel()

  r_int <- run_es(df, outcome = y, time = year, timing = timing,
                  estimator = "cs", unit = id)

  df_chr <- df
  df_chr$id <- sprintf("firm_%03d", df$id)
  r_chr <- run_es(df_chr, outcome = y, time = year, timing = timing,
                  estimator = "cs", unit = id)

  df_fct <- df
  df_fct$id <- factor(sprintf("firm_%03d", df$id))
  r_fct <- run_es(df_fct, outcome = y, time = year, timing = timing,
                  estimator = "cs", unit = id)

  expect_equal(r_chr$estimate,  r_int$estimate,  tolerance = 1e-12)
  expect_equal(r_chr$std.error, r_int$std.error, tolerance = 1e-12)
  expect_equal(r_fct$estimate,  r_int$estimate,  tolerance = 1e-12)
})

test_that("cs: results are invariant to a common shift of time and timing (cohort at 0)", {
  df <- make_robust_panel()
  r_ref <- run_es(df, outcome = y, time = year, timing = timing,
                  estimator = "cs", unit = id)

  # shift so one cohort lands exactly on 0 and times go negative
  df0 <- df
  df0$year   <- df0$year   - 2005L
  df0$timing <- df0$timing - 2005L
  r0 <- run_es(df0, outcome = y, time = year, timing = timing,
               estimator = "cs", unit = id)

  expect_equal(nrow(r0), nrow(r_ref))
  expect_equal(r0$estimate,  r_ref$estimate,  tolerance = 1e-12)
  expect_equal(r0$std.error, r_ref$std.error, tolerance = 1e-12)

  # att_gt g/t must be reported on the (shifted) input scale
  att0 <- attr(r0, "att_gt")
  expect_true(all(att0$g %in% (c(2005L, 2007L) - 2005L)))
})

test_that("classic staggered TWFE keeps never-treated rows in the sample", {
  df <- make_robust_panel()
  expect_true(any(is.na(df$timing)))

  res <- run_es(df, outcome = y, treatment = treated, time = year,
                timing = timing, fe = ~ id + year,
                staggered = TRUE, unit = id)

  expect_equal(attr(res, "N"), nrow(df))
  # metadata still reflects the original NA pattern
  expect_equal(attr(res, "N_treated"), sum(!is.na(df$timing)))
  expect_equal(
    attr(res, "N_nevertreated"),
    length(unique(df$id[is.na(df$timing)]))
  )
})

test_that("sunab keeps never-treated rows and matches fixest base_stagg convention", {
  skip_if_not(
    is.function(tryCatch(getFromNamespace("sunab", "fixest"),
                         error = function(e) NULL)),
    "fixest::sunab() not available"
  )
  df <- make_robust_panel()

  res <- run_es(df, outcome = y, time = year, timing = timing,
                fe = ~ id + year, staggered = TRUE,
                method = "sunab", unit = id)
  expect_equal(attr(res, "N"), nrow(df))

  # reference: direct fixest call with never-treated recoded out of range
  df2 <- df
  df2$timing[is.na(df2$timing)] <- max(df2$year) + 10000
  m   <- fixest::feols(y ~ sunab(timing, year) | id + year, data = df2)
  ref <- broom::tidy(m)
  ref$rel <- as.integer(gsub(".*::(-?\\d+)$", "\\1", ref$term))

  cmp <- merge(
    as.data.frame(res)[!res$is_baseline, c("relative_time", "estimate")],
    ref[, c("rel", "estimate")],
    by.x = "relative_time", by.y = "rel"
  )
  expect_gt(nrow(cmp), 0L)
  expect_equal(cmp$estimate.x, cmp$estimate.y, tolerance = 1e-10)
})

test_that("cs: non-consecutive time grid raises an informative error", {
  df <- make_robust_panel()
  df$year   <- 2000L + (df$year - 2001L) * 5L
  df$timing <- 2000L + (df$timing - 2001L) * 5L

  expect_error(
    run_es(df, outcome = y, time = year, timing = timing,
           estimator = "cs", unit = id),
    "time grid"
  )
})

test_that("cs/sa: fractional time or timing values are rejected, not truncated", {
  df <- make_robust_panel()
  df_frac <- df
  df_frac$year <- df_frac$year + 0.5

  expect_error(
    run_es(df_frac, outcome = y, time = year, timing = timing,
           estimator = "cs", unit = id),
    "integer-valued"
  )
  expect_error(
    suppressWarnings(
      run_es(df_frac, outcome = y, time = year, timing = timing,
             fe = ~ id + year, estimator = "sa", unit = id)
    ),
    "integer-valued"
  )
})

test_that("cs: cohorts whose base period is missing are skipped with a warning", {
  df <- make_robust_panel(periods = 2001:2010)
  # add a cohort treated at the first observed period: base period 2000 missing
  df$timing[df$id <= 5L] <- 2001L

  expect_warning(
    run_es(df, outcome = y, time = year, timing = timing,
           estimator = "cs", unit = id),
    "base period"
  )
})

test_that("bjs: singleton pre-period units are imputed (vectorised patch)", {
  df <- make_robust_panel(periods = 2001:2010)
  # unit 1: treated in 2002 with exactly one pre-treatment observation
  df$timing[df$id == 1L] <- 2002L

  res <- run_es(df, outcome = y, time = year, timing = timing,
                estimator = "bjs", unit = id)
  tau <- attr(res, "tau_it")
  expect_true(all(is.finite(tau$tau_hat[tau$unit == 1L])))
})
