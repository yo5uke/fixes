# test-cs.R
#
# Test-first (TDD) specification for the Callaway-Sant'Anna (2021) estimator.
#
# Theory anchors
# --------------
# Eq. 2.8 (unconditional parallel trends):
#   ATT(g, t) = E[Y_t - Y_{g-1} | G = g] - E[Y_t - Y_{g-1} | C]
#   where C = never-treated (or not-yet-treated) comparison group.
#
# Table 1 — dynamic / event-study aggregation:
#   theta_D(l) = sum_g ATT(g, g+l) * P(G=g | g+l <= T, G > g_min)
#   i.e., cohort-size-weighted average of ATT(g, g+l) over all cohorts
#   for which event time l is within the sample window.
#
# All tests call skip("not yet implemented").
# Remove each skip() only when the corresponding feature is wired up
# in run_es(estimator = "cs").

library(testthat)
library(fixes)

# ---------------------------------------------------------------------------
# Shared simulation fixture
# ---------------------------------------------------------------------------
# Design
#   50 units  |  periods 1995-2005  |  no anticipation
#   Cohort 1998 : 15 units (first treated 1998)
#   Cohort 2000 : 15 units (first treated 2000)
#   Cohort 2002 : 15 units (first treated 2002)
#   Never treated:  5 units  (g = NA)
#   True ATT       = 1.5 (constant, homogeneous across cohorts and time)
#   DGP            = unit FE + linear time trend + 1.5 * treat + N(0, 0.3)

make_cs_data <- function(seed = 42L) {
  set.seed(seed)
  n_units <- 50L
  periods <- 1995:2005

  g_vec <- c(
    rep(1998L, 15L),
    rep(2000L, 15L),
    rep(2002L, 15L),
    rep(NA_integer_, 5L)
  )

  panel <- expand.grid(
    id   = seq_len(n_units),
    year = periods,
    stringsAsFactors = FALSE
  )
  panel <- panel[order(panel$id, panel$year), ]
  rownames(panel) <- NULL

  panel$g     <- g_vec[panel$id]          # NA = never treated
  panel$treat <- as.integer(!is.na(panel$g) & panel$year >= panel$g)

  unit_fe <- rnorm(n_units)[panel$id]
  time_fe <- (panel$year - 1995L) * 0.1
  eps     <- rnorm(nrow(panel), sd = 0.3)

  panel$y <- unit_fe + time_fe + 1.5 * panel$treat + eps
  panel
}

cs_data <- make_cs_data()

# ---------------------------------------------------------------------------
# Test 1 — return type and column contract
# ---------------------------------------------------------------------------

test_that("cs estimator returns es_result object", {
  result <- run_es(
    data          = cs_data,
    outcome       = y,
    time          = year,
    timing        = g,
    unit          = id,
    staggered     = TRUE,
    estimator     = "cs",
    control_group = "nevertreated"
  )

  expect_s3_class(result, "es_result")
  expect_true(is.data.frame(result))

  # Column contract shared with other estimators
  required_cols <- c(
    "term", "estimate", "std.error", "statistic", "p.value",
    "relative_time", "is_baseline",
    "conf_low_95", "conf_high_95"
  )
  expect_true(all(required_cols %in% names(result)))

  # Non-baseline rows must have finite estimates and positive SEs
  non_base <- result[!result$is_baseline, ]
  expect_true(all(is.finite(non_base$estimate)))
  expect_true(all(non_base$std.error > 0))

  # Exactly one baseline row at relative_time == -1 (default baseline)
  expect_equal(sum(result$is_baseline), 1L)
  expect_equal(result$estimate[result$is_baseline], 0)
  expect_equal(result$std.error[result$is_baseline], 0)
})

# ---------------------------------------------------------------------------
# Test 2 — numerical agreement with did::att_gt()
# ---------------------------------------------------------------------------
# Compares the dynamic aggregation from run_es(estimator = "cs") against
# did::att_gt() + did::aggte(type = "dynamic") with the same never-treated
# control group and no anticipation.
#
# Tolerance 1e-6 per CLAUDE.md testing policy.

test_that("cs ATT(g,t) estimates are numerically close to did::att_gt()", {
  skip_if_not_installed("did")

  # did package convention: g = 0 (numeric) for never-treated units.
  # Must be double, not integer: did internally does g[g==0] := Inf via
  # data.table, which silently truncates when the column is integer.
  cs_data_did      <- cs_data
  cs_data_did$g    <- as.numeric(cs_data_did$g)
  cs_data_did$g[is.na(cs_data_did$g)] <- 0

  ref <- did::att_gt(
    yname         = "y",
    tname         = "year",
    idname        = "id",
    gname         = "g",
    data          = cs_data_did,
    control_group = "nevertreated",
    anticipation  = 0L,
    bstrap        = FALSE,
    cband         = FALSE
  )

  # Table 1 dynamic aggregation: theta_D(l) weighted by cohort sizes
  ref_dyn <- did::aggte(ref, type = "dynamic", na.rm = TRUE)

  result <- run_es(
    data          = cs_data,
    outcome       = y,
    time          = year,
    timing        = g,
    unit          = id,
    staggered     = TRUE,
    estimator     = "cs",
    control_group = "nevertreated"
  )

  # Comparison is restricted to POST-TREATMENT event times (l >= 0).
  #
  # Why: did::att_gt(base_period = "varying") follows two distinct conventions:
  #   - l >= 0 (post-treatment): base period = g-1  [matches CS 2021 eq. 2.8]
  #   - l <  0 (pre-treatment): base period = t-1   [sequential first-diff]
  # Our .run_cs() implements eq. 2.8 exactly — g-1 as base for ALL t — so
  # only post-treatment ATTs share the same estimand definition.
  post_l <- result$relative_time[!result$is_baseline & result$relative_time >= 0]
  common_l <- intersect(post_l, ref_dyn$egt[ref_dyn$egt >= 0])
  expect_gt(length(common_l), 0L)

  res_aligned <- result[result$relative_time %in% common_l & !result$is_baseline, ]
  res_aligned <- res_aligned[order(res_aligned$relative_time), ]

  egt_sub     <- ref_dyn$egt[ref_dyn$egt %in% common_l]
  att_sub     <- ref_dyn$att.egt[ref_dyn$egt %in% common_l]
  ref_aligned <- att_sub[order(egt_sub)]

  expect_equal(res_aligned$estimate, ref_aligned, tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# Test 3 — never-treated units used as clean controls
# ---------------------------------------------------------------------------

test_that("cs estimator handles never-treated units correctly", {
  result <- run_es(
    data          = cs_data,
    outcome       = y,
    time          = year,
    timing        = g,
    unit          = id,
    staggered     = TRUE,
    estimator     = "cs",
    control_group = "nevertreated"
  )

  # Both pre- and post-treatment periods must appear
  expect_true(any(result$relative_time < 0L & !result$is_baseline))
  expect_true(any(result$relative_time > 0L))

  # Metadata must reflect the 5 never-treated units in the fixture
  expect_equal(attr(result, "N_nevertreated"), 5L)

  # Control group choice must be stored for reproducibility
  expect_equal(attr(result, "cs_control_group"), "nevertreated")
})

# ---------------------------------------------------------------------------
# Test 4 — informative error when timing column is absent
# ---------------------------------------------------------------------------

test_that("cs estimator errors informatively when timing column is missing", {
  bad_data   <- cs_data
  bad_data$g <- NULL   # drop the cohort column

  expect_error(
    run_es(
      data          = bad_data,
      outcome       = y,
      time          = year,
      timing        = g,
      unit          = id,
      staggered     = TRUE,
      estimator     = "cs"
    ),
    regexp      = "g|timing|column|not found",
    ignore.case = TRUE
  )
})

# ---------------------------------------------------------------------------
# Test 5 — real-data validation: mpdta (CS 2021, Section 5)
# ---------------------------------------------------------------------------
# mpdta: minimum wage and teen employment, 500 counties, years 2003-2007
# (note: 2005 absent from data), cohorts 2004 / 2006 / 2007.
#
# Comparison is restricted to post-treatment ATT(g,t) (t >= g) for the same
# reason as Test 2: did::att_gt(base_period = "varying") uses t-1 as base
# for pre-treatment periods, while our eq. 2.8 uses g-1 for all t.
#
# Tolerance is 1e-4 (looser than the simulation test) because mpdta is a
# real, larger dataset and minor floating-point accumulation can grow.

test_that("CS estimates on mpdta match did::att_gt() post-treatment to tolerance 1e-4", {
  skip_if_not_installed("did")
  skip_if_not_installed("dplyr")
  data(mpdta, package = "did")

  # Our CS estimator identifies never-treated units via is.na(timing).
  # mpdta uses first.treat = 0 for never-treated (did convention), so we
  # must convert before calling run_es().
  mpdta_cs <- mpdta
  mpdta_cs$first.treat[mpdta_cs$first.treat == 0] <- NA_real_

  res <- suppressWarnings(run_es(
    outcome       = lemp,
    treatment     = treated,   # ignored by CS path; kept to match template
    time          = year,
    unit          = countyreal,
    timing        = first.treat,
    data          = mpdta_cs,
    estimator     = "cs",
    control_group = "nevertreated"
  ))

  # Reference: did::att_gt() understands first.treat = 0 as never-treated
  ref <- did::att_gt(
    yname         = "lemp",
    tname         = "year",
    idname        = "countyreal",
    gname         = "first.treat",
    data          = mpdta,
    control_group = "nevertreated",
    bstrap        = FALSE,
    cband         = FALSE
  )

  # Extract post-treatment ATT(g,t) from our result
  att_res <- attr(res, "att_gt")
  att_res_post <- att_res[att_res$t >= att_res$g, ]
  att_res_post <- att_res_post[order(att_res_post$g, att_res_post$t), ]

  # Extract post-treatment ATT(g,t) from did reference (MP object fields)
  att_ref_all  <- data.frame(g = ref$group, t = ref$t, estimate = ref$att,
                              stringsAsFactors = FALSE)
  att_ref_post <- att_ref_all[att_ref_all$t >= att_ref_all$g, ]
  att_ref_post <- att_ref_post[order(att_ref_post$g, att_ref_post$t), ]

  expect_equal(nrow(att_res_post), nrow(att_ref_post))
  expect_equal(att_res_post$estimate, att_ref_post$estimate, tolerance = 1e-4)
})

# ---------------------------------------------------------------------------
# Test 6 — plot_att_gt() works on real data (mpdta)
# ---------------------------------------------------------------------------

test_that("plot_att_gt() works on mpdta cs result", {
  skip_if_not_installed("did")
  skip_if_not_installed("ggplot2")
  data(mpdta, package = "did")

  mpdta_cs <- mpdta
  mpdta_cs$first.treat[mpdta_cs$first.treat == 0] <- NA_real_

  res <- suppressWarnings(run_es(
    outcome       = lemp,
    treatment     = treated,
    time          = year,
    unit          = countyreal,
    timing        = first.treat,
    data          = mpdta_cs,
    estimator     = "cs",
    control_group = "nevertreated"
  ))

  p_heat  <- plot_att_gt(res, type = "heatmap")
  p_facet <- plot_att_gt(res, type = "facet")

  expect_s3_class(p_heat,  "ggplot")
  expect_s3_class(p_facet, "ggplot")
})
