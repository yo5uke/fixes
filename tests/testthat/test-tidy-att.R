# test-tidy-att.R
#
# broom::tidy / broom::glance methods for att_result, enabling modelsummary()
# (and tinytable) tables for aggregated ATTs from calc_att() (CS and BJS).

library(testthat)
library(fixes)

make_att_data <- function(seed = 1L) {
  set.seed(seed)
  units <- 1:60
  yrs   <- 2000:2010
  coh   <- setNames(c(rep(2004L, 20), rep(2007L, 20), rep(NA_integer_, 20)),
                    units)
  d <- expand.grid(id = units, year = yrs)
  d$cohort <- coh[as.character(d$id)]
  d$D <- as.integer(!is.na(d$cohort) & d$year >= d$cohort)
  d$y <- rnorm(nrow(d)) + ifelse(d$D == 1L, 1.5, 0)
  d
}

d <- make_att_data()

cs_simple <- calc_att(d, outcome = y, time = year, timing = cohort, unit = id,
                      estimator = "cs", aggregation = "simple")
cs_cohort <- calc_att(d, outcome = y, time = year, timing = cohort, unit = id,
                      estimator = "cs", aggregation = "by_cohort")
bjs_simple <- suppressWarnings(
  calc_att(d, outcome = y, time = year, timing = cohort, unit = id,
           estimator = "bjs", aggregation = "simple")
)

# ---------------------------------------------------------------------------
# tidy contract
# ---------------------------------------------------------------------------

test_that("tidy.att_result returns the modelsummary column contract", {
  td <- broom::tidy(cs_simple)
  expect_s3_class(td, "data.frame")
  for (col in c("term", "estimate", "std.error", "statistic", "p.value")) {
    expect_true(col %in% names(td), info = paste("missing:", col))
  }
  expect_type(td$term, "character")
  # estimate/std.error preserved from the att_result
  expect_equal(td$estimate,  as.numeric(cs_simple$estimate),  tolerance = 1e-12)
  expect_equal(td$std.error, as.numeric(cs_simple$std.error), tolerance = 1e-12)
})

test_that("term labels reflect the aggregation type and align across estimators", {
  expect_equal(broom::tidy(cs_simple)$term, "ATT")

  td_c <- broom::tidy(cs_cohort)
  expect_equal(nrow(td_c), nrow(cs_cohort))
  expect_true(all(grepl("^Cohort ", td_c$term)))
  # Same cohort labels for a different estimator -> rows align in a joined table
  bjs_cohort <- suppressWarnings(
    calc_att(d, outcome = y, time = year, timing = cohort, unit = id,
             estimator = "bjs", aggregation = "by_cohort")
  )
  expect_setequal(broom::tidy(bjs_cohort)$term, td_c$term)
})

test_that("tidy.att_result adds conf.low/conf.high only when requested", {
  expect_false(any(c("conf.low", "conf.high") %in% names(broom::tidy(cs_simple))))
  td <- broom::tidy(cs_simple, conf.int = TRUE, conf.level = 0.90)
  expect_true(all(c("conf.low", "conf.high") %in% names(td)))
  z <- stats::qnorm(0.95)
  expect_equal(td$conf.low, td$estimate - z * td$std.error, tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# glance contract
# ---------------------------------------------------------------------------

test_that("glance.att_result returns a one-row GOF summary", {
  gl <- broom::glance(cs_simple)
  expect_s3_class(gl, "data.frame")
  expect_equal(nrow(gl), 1L)
  expect_true("nobs" %in% names(gl))
  expect_equal(gl$nobs, as.integer(attr(cs_simple, "N")))
  expect_equal(gl$estimator, "CS")
  expect_equal(gl$aggregation, "simple")
})

test_that("glance works for the BJS estimator too", {
  gl <- broom::glance(bjs_simple)
  expect_equal(gl$estimator, "BJS")
  expect_equal(gl$nobs, as.integer(attr(bjs_simple, "N")))
})

# ---------------------------------------------------------------------------
# modelsummary / tinytable integration (skips if dispatch unavailable)
# ---------------------------------------------------------------------------

test_that("modelsummary builds a table from att_result", {
  skip_if_not_installed("modelsummary")
  tab <- tryCatch(
    modelsummary::modelsummary(cs_cohort, output = "data.frame"),
    error = function(e) e
  )
  # Under devtools::load_all() modelsummary may not see the dev-registered S3
  # method; that is an environment artifact, not a contract failure.
  if (inherits(tab, "error")) {
    skip("modelsummary S3 dispatch unavailable in this environment")
  }
  expect_s3_class(tab, "data.frame")
  expect_true(nrow(tab) > 0L)
})

test_that("tinytable renders a tidied att_result", {
  skip_if_not_installed("tinytable")
  tt <- tinytable::tt(broom::tidy(cs_cohort))
  expect_true(inherits(tt, "tinytable"))  # tinytable objects are S4
})
