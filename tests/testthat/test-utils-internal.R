# Unit tests for the shared internal helpers in R/utils-internal.R.
# These lock in the behaviour relied on by run_es(), calc_att(), run_did(),
# and the five estimator backends after the v0.11 consolidation refactor.

test_that(".resolve_col handles symbols, character, and expressions", {
  df <- data.frame(y = 1:3, x1 = 4:6)
  expect_equal(fixes:::.resolve_col(quote(y), df), "y")
  expect_equal(fixes:::.resolve_col("x1", df), "x1")
  expect_equal(fixes:::.resolve_col(quote(log(y)), df, allow_call = TRUE),
               "log(y)")
  expect_error(fixes:::.resolve_col(quote(zzz), df), regexp = "not found")
  expect_error(fixes:::.resolve_col(quote(log(y)), df, allow_call = FALSE),
               regexp = "Invalid")
})

test_that(".add_ci_columns reproduces the normal-approximation CI", {
  df <- data.frame(estimate = c(1, -2), std_error = c(0.5, 0.25))
  out <- fixes:::.add_ci_columns(df, conf.level = c(0.90, 0.95))
  z90 <- stats::qnorm(0.95); z95 <- stats::qnorm(0.975)
  expect_equal(out$conf_low_90,  df$estimate - z90 * df$std_error)
  expect_equal(out$conf_high_95, df$estimate + z95 * df$std_error)
  # custom column names
  df2 <- data.frame(estimate = 1, std.error = 0.1)
  out2 <- fixes:::.add_ci_columns(df2, 0.95, se_col = "std.error")
  expect_equal(out2$conf_high_95, 1 + z95 * 0.1)
})

test_that(".validate_panel_cols flags missing columns and non-numeric time", {
  df <- data.frame(y = 1:3, g = c(2, NA, 2), t = 1:3, id = 1:3)
  expect_silent(fixes:::.validate_panel_cols(df, c("y", "g", "t", "id"), "t"))
  expect_error(fixes:::.validate_panel_cols(df, c("y", "nope"), "t"),
               regexp = "not found")
  df$t <- as.character(df$t)
  expect_error(fixes:::.validate_panel_cols(df, c("y", "t"), "t"),
               regexp = "must be numeric")
})

test_that(".compute_cohort_sizes counts unique units per cohort", {
  df <- data.frame(
    id     = c(1, 1, 2, 2, 3, 3, 4, 4),
    timing = c(2, 2, 2, 2, 3, 3, NA, NA)
  )
  cs <- fixes:::.compute_cohort_sizes(df, "timing", "id", c(2, 3))
  expect_equal(unname(cs), c(2L, 1L))
  expect_equal(names(cs), c("2", "3"))
})

test_that(".build_es_vcov orders by relative time and labels dimnames", {
  V <- matrix(c(4, 1, 0,
                1, 9, 2,
                0, 2, 16), 3, 3,
              dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  out <- fixes:::.build_es_vcov(V, terms = c("c", "a", "b"),
                                rel_times = c(2L, -2L, 1L))
  expect_equal(rownames(out), c("-2", "1", "2"))
  expect_equal(diag(out), c(`-2` = 4, `1` = 9, `2` = 16))
  expect_null(fixes:::.build_es_vcov(NULL, "a", 1L))
  # terms not in V are dropped
  expect_null(fixes:::.build_es_vcov(V, "zzz", 1L))
})
