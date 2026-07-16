# test-did.R
#
# Test-first (TDD) specification for run_did() — basic TWFE DiD estimator.
#
# Model: y_it = alpha_i + alpha_t + delta * D_it + eps_it
#   D_it = binary treatment indicator (pre-built by the user)
#   delta = the DiD estimate (ATT under parallel trends)

library(testthat)
library(fixes)

# ---------------------------------------------------------------------------
# Shared simulation fixture
# 60 units | periods 2000-2010 | 30 treated units | treatment starts 2006
# Never-treated: 30 units.  True ATT = 2.0.
# ---------------------------------------------------------------------------

make_did_data <- function(seed = 99L) {
  set.seed(seed)
  n_units <- 60L
  periods <- 2000:2010

  panel <- expand.grid(id = seq_len(n_units), year = periods,
                       stringsAsFactors = FALSE)
  panel <- panel[order(panel$id, panel$year), ]
  rownames(panel) <- NULL

  treat_units <- seq_len(30L)
  panel$D <- as.integer(panel$id %in% treat_units & panel$year >= 2006L)

  unit_fe <- rnorm(n_units)[panel$id]
  time_fe <- (panel$year - 2000L) * 0.1
  panel$y <- unit_fe + time_fe + 2.0 * panel$D + rnorm(nrow(panel), sd = 0.5)
  panel$x1 <- rnorm(nrow(panel))
  panel
}

did_data <- make_did_data()

# ---------------------------------------------------------------------------
# 1. Return type
# ---------------------------------------------------------------------------

test_that("run_did returns did_result S3 class that is a list", {
  res <- run_did(did_data, outcome = y, treatment = D,
                 fe = ~ id + year)
  expect_s3_class(res, "did_result")
  expect_true(is.list(res))
  expect_true("estimates" %in% names(res))
  expect_true("coeftable" %in% names(res))
  expect_true("vcov"      %in% names(res))
  expect_true("fit_stats" %in% names(res))
})

# ---------------------------------------------------------------------------
# 2. estimates column contract
# ---------------------------------------------------------------------------

test_that("estimates data.frame has required columns and exactly one row", {
  res <- run_did(did_data, outcome = y, treatment = D,
                 fe = ~ id + year)
  est <- res$estimates
  expect_s3_class(est, "data.frame")
  expect_equal(nrow(est), 1L)
  expected_cols <- c("term", "estimate", "std.error", "statistic", "p.value",
                     "conf_low_95", "conf_high_95")
  for (col in expected_cols) {
    expect_true(col %in% names(est), info = paste("missing column:", col))
  }
})

# ---------------------------------------------------------------------------
# 3. Attributes
# ---------------------------------------------------------------------------

test_that("did_result carries required attributes", {
  res <- run_did(did_data, outcome = y, treatment = D,
                 fe = ~ id + year, unit = id, time = year)
  expect_equal(attr(res, "N"),         nrow(did_data))
  expect_equal(attr(res, "N_units"),   60L)
  expect_equal(attr(res, "N_treated"), sum(did_data$D))
  expect_equal(attr(res, "vcov_type"), "HC1")
  expect_null(attr(res, "cluster_vars"))
  expect_equal(attr(res, "conf.level"), 0.95)
  expect_equal(attr(res, "outcome"),   "y")
  expect_equal(attr(res, "treatment"), "D")
  expect_true(nzchar(attr(res, "fe")))
})

# ---------------------------------------------------------------------------
# 4. Explicit fe formula
# ---------------------------------------------------------------------------

test_that("explicit fe formula is stored as character RHS", {
  res <- run_did(did_data, outcome = y, treatment = D,
                 fe = ~ id + year)
  expect_equal(attr(res, "fe"), "id + year")
})

# ---------------------------------------------------------------------------
# 5. FE auto-inference from unit + time
# ---------------------------------------------------------------------------

test_that("fe auto-inferred from unit+time matches explicit fe", {
  res_explicit <- run_did(did_data, outcome = y, treatment = D,
                          fe = ~ id + year)
  res_inferred <- run_did(did_data, outcome = y, treatment = D,
                          unit = id, time = year)
  expect_equal(attr(res_inferred, "fe"), "id + year")
  expect_equal(res_inferred$estimates$estimate,
               res_explicit$estimates$estimate,
               tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# 6. Numerical agreement with direct feols
# ---------------------------------------------------------------------------

test_that("estimate agrees with direct fixest::feols at machine precision", {
  skip_if_not_installed("fixest")
  res <- run_did(did_data, outcome = y, treatment = D,
                 fe = ~ id + year)
  ref <- fixest::feols(y ~ D | id + year, data = did_data,
                       vcov = "HC1")
  ref_tidy <- broom::tidy(ref)
  expect_equal(res$estimates$estimate,
               ref_tidy$estimate[ref_tidy$term == "D"],
               tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# 7. Outcome expression
# ---------------------------------------------------------------------------

test_that("outcome can be an expression like log(y + 3)", {
  res <- suppressWarnings(run_did(did_data, outcome = log(y + 3), treatment = D,
                                  fe = ~ id + year))
  expect_s3_class(res, "did_result")
  expect_equal(attr(res, "outcome"), "log(y + 3)")
})

# ---------------------------------------------------------------------------
# 8. Covariates
# ---------------------------------------------------------------------------

test_that("covariates argument works and treatment term is still present", {
  res <- run_did(did_data, outcome = y, treatment = D,
                 fe = ~ id + year, covariates = ~ x1)
  expect_s3_class(res, "did_result")
  expect_equal(nrow(res$estimates), 1L)
  expect_equal(res$estimates$term, "D")
  full_tidy <- broom::tidy(res)
  expect_true("D"  %in% full_tidy$term)
  expect_true("x1" %in% full_tidy$term)
})

# ---------------------------------------------------------------------------
# 9. Multiple conf.level
# ---------------------------------------------------------------------------

test_that("conf.level vector generates multiple CI pairs, all correct", {
  res <- run_did(did_data, outcome = y, treatment = D,
                 fe = ~ id + year, conf.level = c(0.90, 0.95))
  est <- res$estimates
  for (col in c("conf_low_90", "conf_high_90", "conf_low_95", "conf_high_95")) {
    expect_true(col %in% names(est), info = paste("missing:", col))
  }
  expect_lt(est$conf_high_90 - est$conf_low_90,
            est$conf_high_95 - est$conf_low_95)
  expect_equal(attr(res, "conf.level"), c(0.90, 0.95))
})

# ---------------------------------------------------------------------------
# 10. tidy.did_result
# ---------------------------------------------------------------------------

test_that("tidy.did_result returns broom-compatible data.frame", {
  res <- run_did(did_data, outcome = y, treatment = D,
                 fe = ~ id + year)
  td <- broom::tidy(res)
  expect_s3_class(td, "data.frame")
  for (col in c("term", "estimate", "std.error", "statistic", "p.value")) {
    expect_true(col %in% names(td), info = paste("missing tidy column:", col))
  }
  treatment_row <- td[td$term == "D", ]
  expect_equal(as.numeric(treatment_row$estimate),
               res$estimates$estimate,
               tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# 11. glance.did_result
# ---------------------------------------------------------------------------

test_that("glance.did_result returns single-row data.frame with nobs", {
  res <- run_did(did_data, outcome = y, treatment = D,
                 fe = ~ id + year)
  gl <- broom::glance(res)
  expect_s3_class(gl, "data.frame")
  expect_equal(nrow(gl), 1L)
  expect_true("nobs" %in% names(gl))
  expect_equal(gl$nobs, nrow(did_data))
})

# ---------------------------------------------------------------------------
# 12. print.did_result
# ---------------------------------------------------------------------------

test_that("print.did_result outputs DiD Estimation header", {
  res <- run_did(did_data, outcome = y, treatment = D,
                 fe = ~ id + year)
  expect_output(print(res), "DiD Estimation")
  expect_output(print(res), "TWFE")
})

# ---------------------------------------------------------------------------
# 13. Error: treatment absorbed by FE
# ---------------------------------------------------------------------------

test_that("absorption error is informative when treatment is collinear with FE", {
  # D is constant within each unit (never changes) — collinear with unit FE
  # when treatment doesn't vary within units after controlling for FE
  # Use a simpler case: absorb treatment with a unit-level FE directly.
  # We create a treatment variable that is unit-constant (no within-unit variation).
  d2 <- did_data
  d2$D_unit_const <- as.integer(d2$id <= 30L)  # perfectly absorbed by id FE
  expect_error(
    run_did(d2, outcome = y, treatment = D_unit_const, fe = ~ id + year),
    "absorbed"
  )
})

# ---------------------------------------------------------------------------
# 14. Error: unknown column
# ---------------------------------------------------------------------------

test_that("run_did errors clearly when outcome or treatment column not found", {
  expect_error(run_did(did_data, outcome = zzz_no_such, treatment = D,
                       fe = ~ id + year),
               "not found")
  expect_error(run_did(did_data, outcome = y, treatment = zzz_no_such,
                       fe = ~ id + year),
               "not found")
})

# ---------------------------------------------------------------------------
# 15. modelsummary smoke test
# ---------------------------------------------------------------------------

test_that("modelsummary(did_result) returns data.frame (smoke test)", {
  skip_if_not_installed("modelsummary")
  res <- run_did(did_data, outcome = y, treatment = D,
                 fe = ~ id + year)
  tab <- modelsummary::modelsummary(res, output = "data.frame")
  expect_s3_class(tab, "data.frame")
  expect_true(nrow(tab) > 0L)
})

# ---------------------------------------------------------------------------
# 16. timing parameter: auto-construction of D_it
# ---------------------------------------------------------------------------

test_that("timing option constructs D_it from group indicator + time", {
  # Add group indicator (unit-constant)
  d2 <- did_data
  d2$treated_group <- as.integer(d2$id <= 30L)

  res_timing <- run_did(d2, outcome = y, treatment = treated_group,
                        time = year, timing = 2006L,
                        fe = ~ id + year)
  res_prebuilt <- run_did(d2, outcome = y, treatment = D,
                          fe = ~ id + year)

  expect_s3_class(res_timing, "did_result")
  # Point estimates must match exactly
  expect_equal(res_timing$estimates$estimate,
               res_prebuilt$estimates$estimate,
               tolerance = 1e-12)
  # Term label in estimates reflects the original group variable
  expect_equal(res_timing$estimates$term, "treated_group")
  # timing attribute stored
  expect_equal(attr(res_timing, "timing"), 2006L)
  # treatment attribute reflects group variable
  expect_equal(attr(res_timing, "treatment"), "treated_group")
})

test_that("timing option requires time argument", {
  d2 <- did_data
  d2$treated_group <- as.integer(d2$id <= 30L)
  expect_error(
    run_did(d2, outcome = y, treatment = treated_group, timing = 2006L,
            fe = ~ id + year),
    "time.*must be provided"
  )
})

test_that("timing option works with fe auto-inference", {
  d2 <- did_data
  d2$treated_group <- as.integer(d2$id <= 30L)
  res <- run_did(d2, outcome = y, treatment = treated_group,
                 unit = id, time = year, timing = 2006L)
  expect_s3_class(res, "did_result")
  expect_equal(attr(res, "fe"), "id + year")
})

# ---------------------------------------------------------------------------
# 17. cluster-robust SE: vcov_type metadata
# ---------------------------------------------------------------------------

test_that("vcov_type is 'cluster' when cluster is specified with default vcov", {
  res <- run_did(did_data, outcome = y, treatment = D,
                 fe = ~ id + year, cluster = ~ id)
  expect_equal(attr(res, "vcov_type"), "cluster")
  expect_equal(attr(res, "cluster_vars"), "id")
  # SEs with clustering differ from HC1
  res_hc1 <- run_did(did_data, outcome = y, treatment = D,
                     fe = ~ id + year)
  expect_false(isTRUE(all.equal(
    res$estimates$std.error,
    res_hc1$estimates$std.error
  )))
})

test_that("vcov_type reflects explicit vcov when cluster is also specified", {
  res <- run_did(did_data, outcome = y, treatment = D,
                 fe = ~ id + year, cluster = ~ id, vcov = "iid")
  expect_equal(attr(res, "vcov_type"), "iid")
})

# ---------------------------------------------------------------------------
# Oracle: tidy/glance reproduce broom on a direct feols fit (post-migration)
# ---------------------------------------------------------------------------

test_that("tidy(did_result) matches broom::tidy(feols) incl. conf.int", {
  skip_if_not_installed("fixest")
  df <- make_did_data()
  df$x2 <- runif(nrow(df), 1, 3)
  df$grp <- factor(sample(letters[1:3], nrow(df), replace = TRUE))

  res <- run_did(df, outcome = y, treatment = D,
                 covariates = ~ x1 + log(x2) + grp, fe = ~ id + year,
                 cluster = ~ id)
  ref <- fixest::feols(y ~ D + x1 + log(x2) + grp | id + year,
                       data = df, cluster = ~id)
  td  <- broom::tidy(res, conf.int = TRUE)
  tdr <- broom::tidy(ref, conf.int = TRUE)

  expect_equal(td$term, tdr$term)
  expect_equal(td$estimate, as.numeric(tdr$estimate), tolerance = 1e-8)
  expect_equal(td$std.error, as.numeric(tdr$std.error), tolerance = 1e-8)
  expect_equal(td$statistic, as.numeric(tdr$statistic), tolerance = 1e-8)
  expect_equal(td$p.value, as.numeric(tdr$p.value), tolerance = 1e-8)
  expect_equal(td$conf.low, as.numeric(tdr$conf.low), tolerance = 1e-8)
  expect_equal(td$conf.high, as.numeric(tdr$conf.high), tolerance = 1e-8)
})

test_that("glance(did_result) matches broom::glance(feols)", {
  skip_if_not_installed("fixest")
  df <- make_did_data()
  df$w <- runif(nrow(df), 0.5, 2)

  # FE + weights
  res <- run_did(df, outcome = y, treatment = D, fe = ~ id + year,
                 weights = ~w)
  ref <- fixest::feols(y ~ D | id + year, data = df, weights = ~w)
  g  <- broom::glance(res)
  gr <- broom::glance(ref)
  for (nm in c("r.squared", "adj.r.squared", "within.r.squared", "sigma",
               "nobs", "AIC", "BIC", "logLik")) {
    expect_equal(as.numeric(g[[nm]]), as.numeric(gr[[nm]]),
                 tolerance = 1e-8, info = nm)
  }

  # pooled OLS (no FE)
  res_p <- suppressMessages(run_did(df, outcome = y, treatment = D))
  ref_p <- fixest::feols(y ~ D, data = df)
  g_p  <- broom::glance(res_p)
  gr_p <- broom::glance(ref_p)
  expect_equal(g_p$r.squared, as.numeric(gr_p$r.squared), tolerance = 1e-8)
  expect_true(is.na(g_p$within.r.squared))
  expect_equal(g_p$AIC, as.numeric(gr_p$AIC), tolerance = 1e-8)
  expect_equal(res_p$coeftable$term[1], "(Intercept)")
})

test_that("run_did weighted estimates match feols weights", {
  skip_if_not_installed("fixest")
  df <- make_did_data()
  df$w <- runif(nrow(df), 0.5, 2)
  res <- run_did(df, outcome = y, treatment = D, fe = ~ id + year,
                 weights = ~w, cluster = ~id)
  ref <- fixest::feols(y ~ D | id + year, data = df, weights = ~w,
                       cluster = ~id)
  tdr <- broom::tidy(ref)
  expect_equal(res$estimates$estimate,
               as.numeric(tdr$estimate[tdr$term == "D"]), tolerance = 1e-8)
  expect_equal(res$estimates$std.error,
               as.numeric(tdr$std.error[tdr$term == "D"]), tolerance = 1e-8)
  expect_equal(res$estimates$p.value,
               as.numeric(tdr$p.value[tdr$term == "D"]), tolerance = 1e-8)
})
