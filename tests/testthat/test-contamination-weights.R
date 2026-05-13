# test-contamination-weights.R
#
# Tests for compute_contamination_weights() and plot_contamination_weights().
#
# Theory anchors (SA 2021)
# -------------------------
# Eq. (20): mu_l = sum_{(e, l')} omega^l_{e,l'} * CATT_{e,l'}
# Eq. (12): auxiliary OVB regression (response = cohort-specific indicator,
#           regressors = cohort-aggregated indicators + two-way FEs)
#
# Diagnostic properties tested (SA 2021, Proposition 1):
#   (i)  sum_e omega^l_{e,l}   = 1  (own-period cohort weights sum to 1)
#   (ii) sum_e omega^l_{e,l'}  = 0  for l' in g_incl, l' != l
#   (iii) OVB identity: mu_l = sum_{(e,l')} omega^l_{e,l'} * CATT_{e,l'}
#         (exact algebraic identity up to floating-point)

library(testthat)
library(fixes)

# ---------------------------------------------------------------------------
# Shared simulation fixture (same DGP as test-sa.R)
# ---------------------------------------------------------------------------
make_cw_data <- function(seed = 42L) {
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
    id = seq_len(n_units),
    year = periods,
    stringsAsFactors = FALSE
  )
  panel <- panel[order(panel$id, panel$year), ]
  rownames(panel) <- NULL
  panel$g <- g_vec[panel$id]
  panel$treat <- as.integer(!is.na(panel$g) & panel$year >= panel$g)
  unit_fe <- stats::rnorm(n_units)[panel$id]
  time_fe <- (panel$year - 1995L) * 0.1
  panel$y <- unit_fe +
    time_fe +
    1.5 * panel$treat +
    stats::rnorm(nrow(panel), sd = 0.3)
  panel
}

cw_data <- make_cw_data()

# ---------------------------------------------------------------------------
# Test 1 — return type and column contract
# ---------------------------------------------------------------------------
test_that("compute_contamination_weights returns sa_contamination_weights", {
  cw <- compute_contamination_weights(
    data = cw_data,
    time = year,
    timing = g,
    unit = id,
    fe = ~ id + year,
    baseline = -1L
  )

  expect_s3_class(cw, "sa_contamination_weights")
  expect_s3_class(cw, "data.frame")

  required_cols <- c(
    "catt_cohort",
    "catt_period",
    "twfe_period",
    "weight",
    "is_own"
  )
  expect_true(all(required_cols %in% names(cw)))

  # All weights should be finite (no NA expected for a well-specified panel)
  expect_true(
    all(is.finite(cw$weight)),
    info = "All weights should be finite for this panel"
  )

  # Attributes
  expect_equal(attr(cw, "baseline"), -1L)
  expect_equal(attr(cw, "cohorts"), c(1998L, 2000L, 2002L))
  expect_true(is.integer(attr(cw, "cohort_sizes")))
  expect_true(is.numeric(attr(cw, "incl_periods")))
})

# ---------------------------------------------------------------------------
# Test 2 — correct dimensions
# ---------------------------------------------------------------------------
test_that("weight matrix has correct number of rows", {
  cw <- compute_contamination_weights(
    data = cw_data,
    time = year,
    timing = g,
    unit = id,
    fe = ~ id + year,
    baseline = -1L
  )

  # With 3 cohorts and periods 1995-2005 (11 calendar periods):
  # Z_mat covers ALL (g, l) pairs including baseline (Gemini fix):
  # - Cohort 1998: l in {-3,-2,-1, 0,...,7} = 11 pairs
  # - Cohort 2000: l in {-5,..., 5}          = 11 pairs
  # - Cohort 2002: l in {-7,..., 3}          = 11 pairs
  # Total CATTs (K_z) = 33   [includes baseline l=-1 rows]
  # Unique included periods (K_x) = -7,...,-2, 0,...,7 = 14  (excl baseline -1)
  # Total rows = K_z * K_x = 33 * 14 = 462

  cohorts <- attr(cw, "cohorts")
  incl_periods <- attr(cw, "incl_periods")

  # Check that all TWFE periods (incl_periods) appear for each CATT pair
  catt_pairs <- unique(cw[, c("catt_cohort", "catt_period")])
  for (i in seq_len(nrow(catt_pairs))) {
    sub <- cw[
      cw$catt_cohort == catt_pairs$catt_cohort[i] &
        cw$catt_period == catt_pairs$catt_period[i],
    ]
    expect_equal(
      nrow(sub),
      length(incl_periods),
      info = paste0(
        "CATT(g=",
        catt_pairs$catt_cohort[i],
        ", l=",
        catt_pairs$catt_period[i],
        ")"
      )
    )
  }
})

# ---------------------------------------------------------------------------
# Test 3 — Property (i): own-period cohort weights sum to 1
# ---------------------------------------------------------------------------
test_that("own-period weights sum to 1 for each TWFE period (Prop 1-i)", {
  cw <- compute_contamination_weights(
    data = cw_data,
    time = year,
    timing = g,
    unit = id,
    fe = ~ id + year,
    baseline = -1L
  )

  # For each TWFE period l, sum weights where catt_period == l (is_own == TRUE)
  incl_periods <- attr(cw, "incl_periods")

  for (l in incl_periods) {
    own_rows <- cw[cw$twfe_period == l & cw$is_own, ]
    if (nrow(own_rows) == 0L) {
      next
    } # no cohort observed at this period
    own_sum <- sum(own_rows$weight, na.rm = TRUE)
    expect_equal(
      own_sum,
      1.0,
      tolerance = 1e-4,
      info = paste0("own-weight sum for TWFE period l=", l)
    )
  }
})

# ---------------------------------------------------------------------------
# Test 4 — Property (ii): cross-period (included) cohort weights sum to 0
# ---------------------------------------------------------------------------
test_that("cross-period included weights sum to 0 (Prop 1-ii)", {
  cw <- compute_contamination_weights(
    data = cw_data,
    time = year,
    timing = g,
    unit = id,
    fe = ~ id + year,
    baseline = -1L
  )

  incl_periods <- attr(cw, "incl_periods")

  for (l_twfe in incl_periods) {
    for (l_catt in incl_periods) {
      if (l_catt == l_twfe) {
        next
      } # skip own-period (already tested above)
      cross_rows <- cw[cw$twfe_period == l_twfe & cw$catt_period == l_catt, ]
      if (nrow(cross_rows) == 0L) {
        next
      }
      cross_sum <- sum(cross_rows$weight, na.rm = TRUE)
      expect_equal(
        cross_sum,
        0.0,
        tolerance = 1e-4,
        info = paste0(
          "cross-weight sum: TWFE l=",
          l_twfe,
          " <- CATT period l=",
          l_catt
        )
      )
    }
  }
})

# ---------------------------------------------------------------------------
# Test 5 — Property (iii): OVB decomposition identity
# ---------------------------------------------------------------------------
# mu_l (TWFE) = sum_{(e, l')} omega^l_{e,l'} * CATT_{e,l'}
# This is an exact algebraic identity (up to floating-point).
test_that("OVB decomposition: TWFE = sum(weight * CATT) (Eq 20)", {
  # ---- SA CATT estimates ---------------------------------------------------
  sa_res <- run_es(
    data = cw_data,
    outcome = y,
    time = year,
    timing = g,
    unit = id,
    fe = ~ id + year,
    staggered = TRUE,
    estimator = "sa",
    baseline = -1L
  )
  catt_df <- attr(sa_res, "catt_df")

  # ---- Contamination weights -----------------------------------------------
  cw <- compute_contamination_weights(
    data = cw_data,
    time = year,
    timing = g,
    unit = id,
    fe = ~ id + year,
    baseline = -1L
  )

  # ---- Compute OVB prediction: sum_{(e,l')} omega * CATT ------------------
  cw_merged <- merge(
    cw,
    catt_df[, c("g", "l", "estimate")],
    by.x = c("catt_cohort", "catt_period"),
    by.y = c("g", "l"),
    all.x = TRUE
  )
  cw_merged$contribution <- cw_merged$weight * cw_merged$estimate

  ovb_pred <- tapply(
    cw_merged$contribution,
    cw_merged$twfe_period,
    sum,
    na.rm = TRUE
  )

  # ---- TWFE estimates from the SA result (rel_time x estimate) -------------
  # Use the SA result as reference since it shares the same FE specification
  # as the contamination weights (same cohort-aggregated relative time indicators
  # come from the same data structure).
  # We compare against the SA IW estimates, which should satisfy the OVB
  # identity when the weights are correctly computed.
  sa_est <- setNames(
    sa_res$estimate[!sa_res$is_baseline],
    sa_res$relative_time[!sa_res$is_baseline]
  )

  # The OVB identity holds for the CLASSIC TWFE (cohort-aggregated), not the
  # IW estimator.  We verify by running a pure TWFE regression ourselves.
  # Build cohort-aggregated relative time variable.
  tmp <- cw_data
  tmp$rel_t <- tmp$year - tmp$g # NA for never-treated
  all_incl <- attr(cw, "incl_periods")

  # For each included period, build the cohort-aggregated dummy
  for (l in all_incl) {
    col <- paste0("D_", if (l < 0L) paste0("neg", -l) else as.character(l))
    tmp[[col]] <- as.integer(!is.na(tmp$rel_t) & tmp$rel_t == l)
  }
  dummy_cols <- paste0(
    "D_",
    ifelse(all_incl < 0L, paste0("neg", -all_incl), as.character(all_incl))
  )

  twfe_fml <- stats::as.formula(
    paste0("y ~ ", paste(dummy_cols, collapse = " + "), " | id + year")
  )
  twfe_model <- fixest::feols(twfe_fml, data = tmp, warn = FALSE, notes = FALSE)
  mu_l <- stats::coef(twfe_model)

  # Match names to incl_periods
  twfe_est <- setNames(
    vapply(
      dummy_cols,
      function(d) {
        if (d %in% names(mu_l)) mu_l[[d]] else NA_real_
      },
      numeric(1L)
    ),
    all_incl
  )

  # Compare OVB prediction with actual TWFE estimates
  common_l <- intersect(names(ovb_pred), names(twfe_est))
  for (l in common_l) {
    if (is.na(twfe_est[[l]]) || is.na(ovb_pred[[l]])) {
      next
    }
    expect_equal(
      as.numeric(ovb_pred[[l]]),
      as.numeric(twfe_est[[l]]),
      tolerance = 1e-4,
      info = paste0("OVB identity at TWFE period l=", l)
    )
  }
})

# ---------------------------------------------------------------------------
# Test 6 — is_own flag
# ---------------------------------------------------------------------------
test_that("is_own correctly flags own-period rows", {
  cw <- compute_contamination_weights(
    data = cw_data,
    time = year,
    timing = g,
    unit = id,
    fe = ~ id + year,
    baseline = -1L
  )

  expect_true(all(cw$is_own == (cw$catt_period == cw$twfe_period)))
})

# ---------------------------------------------------------------------------
# Test 7 — print method runs without error
# ---------------------------------------------------------------------------
test_that("print.sa_contamination_weights runs without error", {
  cw <- compute_contamination_weights(
    data = cw_data,
    time = year,
    timing = g,
    unit = id,
    fe = ~ id + year,
    baseline = -1L
  )
  expect_output(print(cw), "SA Contamination Weights")
  expect_output(print(cw), "own_weight_sum")
})

# ---------------------------------------------------------------------------
# Test 8 — plot function returns a ggplot
# ---------------------------------------------------------------------------
test_that("plot_contamination_weights returns a ggplot object", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("scales")

  cw <- compute_contamination_weights(
    data = cw_data,
    time = year,
    timing = g,
    unit = id,
    fe = ~ id + year,
    baseline = -1L
  )

  p <- plot_contamination_weights(cw)
  expect_s3_class(p, "ggplot")

  # Themes
  p_min <- plot_contamination_weights(cw, theme = "minimal")
  expect_s3_class(p_min, "ggplot")

  p_vals <- plot_contamination_weights(cw, show_values = TRUE)
  expect_s3_class(p_vals, "ggplot")
})

# ---------------------------------------------------------------------------
# Test 9 — error on missing columns
# ---------------------------------------------------------------------------
test_that("compute_contamination_weights errors on missing columns", {
  expect_error(
    compute_contamination_weights(
      data = cw_data,
      time = year,
      timing = nonexistent_col,
      unit = id,
      fe = ~ id + year
    ),
    regexp = "not found"
  )
})

# ---------------------------------------------------------------------------
# Test 10 — single-cohort data: own-period weights still sum to 1
# ---------------------------------------------------------------------------
test_that("single-cohort data: own-period weights still sum to 1", {
  set.seed(1L)
  n <- 30L
  periods <- 2000:2008
  single <- expand.grid(
    id = seq_len(n),
    year = periods,
    stringsAsFactors = FALSE
  )
  single <- single[order(single$id, single$year), ]
  single$g <- ifelse(single$id <= 20L, 2004L, NA_integer_)
  single$treat <- as.integer(!is.na(single$g) & single$year >= single$g)
  single$y <- stats::rnorm(nrow(single)) + single$treat * 2

  cw <- suppressMessages(
    compute_contamination_weights(
      data = single,
      time = year,
      timing = g,
      unit = id,
      fe = ~ id + year,
      baseline = -1L
    )
  )

  incl_periods <- attr(cw, "incl_periods")
  for (l in incl_periods) {
    own_rows <- cw[cw$twfe_period == l & cw$is_own, ]
    if (nrow(own_rows) == 0L) {
      next
    }
    expect_equal(
      sum(own_rows$weight, na.rm = TRUE),
      1.0,
      tolerance = 1e-4,
      info = paste0("single-cohort own-weight sum at l=", l)
    )
  }
})

# ---------------------------------------------------------------------------
# Test 11 — baseline period appears in catt_period (Gemini fix validation)
# ---------------------------------------------------------------------------
# Z_mat must cover ALL (g, l) pairs including the excluded baseline, so that
# contamination weights from the baseline normalisation are captured.
# SA (2021) Section 3.6 / Proposition 4: the weights from the excluded period
# sum to -1 across cohorts (forcing mechanism of the normalisation).
test_that("catt_period contains baseline period rows (Z_mat covers all periods)", {
  cw <- compute_contamination_weights(
    data = cw_data,
    time = year,
    timing = g,
    unit = id,
    fe = ~ id + year,
    baseline = -1L
  )

  # The baseline period (l = -1) must appear as a catt_period
  expect_true(
    -1L %in% cw$catt_period,
    label = "baseline catt_period = -1 present in output"
  )

  # For each TWFE included period l'', the baseline contamination weights
  # sum to a NEGATIVE value (typically -1 in the aggregate, but depends on
  # the number of cohorts observed at each horizon).
  incl_periods <- attr(cw, "incl_periods")
  for (l_twfe in incl_periods) {
    baseline_rows <- cw[cw$twfe_period == l_twfe & cw$catt_period == -1L, ]
    if (nrow(baseline_rows) == 0L) next
    baseline_sum <- sum(baseline_rows$weight, na.rm = TRUE)
    # Baseline contamination sums to negative (normalization forcing)
    expect_lt(
      baseline_sum, 0,
      label = paste0("baseline catt_period weight sum < 0 at TWFE l=", l_twfe)
    )
  }
})
