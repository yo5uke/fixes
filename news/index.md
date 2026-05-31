# Changelog

## fixes 0.11.1 (2026-05-30)

### Improvements

- **[`run_es()`](https://yo5uke.com/fixes/reference/run_es.md) —
  diagnostic warning for ambiguous `timing = NA`** (classic TWFE,
  `staggered = TRUE` only): a
  [`warning()`](https://rdrr.io/r/base/warning.html) is now emitted when
  any unit has `treatment = 1` in at least one row but `timing = NA`.
  Such units are silently absorbed as never-treated controls — correct
  when `NA` is intentional, but dangerous when `NA` represents genuinely
  missing treatment timing. The warning names up to 5 affected unit IDs
  (when `unit` is supplied) for quick diagnosis.

- **`@param timing` documentation clarified**: all staggered estimators
  (cs, sa, bjs, twm, flex, classic with `staggered = TRUE`) share the
  same `NA = never treated` convention as
  [`did::att_gt()`](https://bcallaway11.github.io/did/reference/att_gt.html)
  and
  [`fixest::sunab()`](https://lrberge.github.io/fixest/reference/sunab.html).
  The roxygen documentation now states this explicitly and notes the
  risk of silent misclassification when `NA` represents missing data
  rather than “never treated”.

------------------------------------------------------------------------

## fixes 0.11.0 (2026-05-24)

### New Features

- **[`honest_sensitivity()`](https://yo5uke.com/fixes/reference/honest_sensitivity.md)
  — honest robust inference (Rambachan & Roth 2023)**
  ([R/honest_did.R](https://yo5uke.com/fixes/news/R/honest_did.R)):
  sensitivity analysis for event-study / DiD designs when parallel
  trends may be violated. Instead of assuming parallel trends holds
  exactly, it reports confidence sets for a post-treatment effect under
  progressively weaker restrictions on the difference in trends, and a
  *breakdown value* (the largest restriction at which the effect is
  still significant):

  ``` r

  res <- run_es(df, outcome = y, treatment = treat, time = year, timing = 6,
                fe = ~ id + year)
  h <- honest_sensitivity(res, type = "relative_magnitude",
                          Mvec = c(0, 0.5, 1, 1.5, 2))
  plot_honest(h)
  ```

  - Restriction families: `"relative_magnitude"` () and `"smoothness"`
    ().
  - Inference via the Andrews-Roth-Pakes (ARP) **conditional**
    moment-inequality test — a pure-R reimplementation (no dependency on
    the HonestDiD package).
  - Works directly from a
    [`run_es()`](https://yo5uke.com/fixes/reference/run_es.md)
    `es_result` (estimator `"twfe"`: classic or `method = "sunab"`,
    which now carry the event-study coefficient covariance via the new
    `es_vcov` attribute), or from a raw `betahat` / `sigma` pair for any
    estimator.
  - [`plot_honest()`](https://yo5uke.com/fixes/reference/plot_honest.md)
    / `autoplot()` draw the “top-down” sensitivity plot;
    `print.honest_result()` reports the breakdown value.
  - Validated against the **HonestDiD** reference package (method
    `"Conditional"`): (single post period) matches to machine precision;
    matches to grid resolution. Heavy numeric helpers (`lpSolveAPI`,
    `Rglpk`, `TruncatedNormal`, `Matrix`, `pracma`) are in `Suggests`
    and gated with
    [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html).

### Internal / Refactor

- **Estimator-core consolidation** (behaviour-preserving): factored the
  boilerplate duplicated across
  [`run_es()`](https://yo5uke.com/fixes/reference/run_es.md),
  [`calc_att()`](https://yo5uke.com/fixes/reference/calc_att.md),
  [`run_did()`](https://yo5uke.com/fixes/reference/run_did.md), and the
  five estimator backends into shared helpers in
  [R/utils-internal.R](https://yo5uke.com/fixes/news/R/utils-internal.R)
  — `.resolve_col()` (NSE column resolution), `.add_ci_columns()` (CI
  construction), `.validate_panel_cols()`, `.compute_cohort_sizes()`,
  `.model_vcov_full()`, and `.aggregate_iw()`. The
  [`calc_att()`](https://yo5uke.com/fixes/reference/calc_att.md)
  aggregation loops were vectorised. All existing tests pass unchanged.

- **[`run_did()`](https://yo5uke.com/fixes/reference/run_did.md) — basic
  TWFE DiD estimator**
  ([R/run_did.R](https://yo5uke.com/fixes/news/R/run_did.R)): New
  function for classic two-way fixed-effects difference-in-differences.
  Accepts a pre-built binary treatment indicator `D_it` and returns a
  `did_result` S3 object with full `modelsummary` / `tinytable`
  compatibility:

  ``` r

  df$D <- as.integer(df$treated & df$year >= 2006)
  result <- run_did(df, outcome = y, treatment = D, fe = ~ id + year)
  print(result)
  modelsummary::modelsummary(result)   # tinytable output (modelsummary >= 2.0)
  ```

  - NSE API consistent with
    [`run_es()`](https://yo5uke.com/fixes/reference/run_es.md) and
    [`calc_att()`](https://yo5uke.com/fixes/reference/calc_att.md) —
    specify variables by name, not by formula construction.
  - `fe` auto-inferred as `~ unit + time` when both `unit` and `time`
    are supplied and `fe = NULL`.
  - [`tidy.did_result()`](https://yo5uke.com/fixes/reference/tidy.did_result.md)
    delegates to
    [`broom::tidy.fixest()`](https://broom.tidymodels.org/reference/tidy.fixest.html)
    for full regression table (all regressors);
    [`glance.did_result()`](https://yo5uke.com/fixes/reference/glance.did_result.md)
    delegates to
    [`broom::glance.fixest()`](https://broom.tidymodels.org/reference/glance.fixest.html)
    for model-level stats (`within.r.squared`, `nobs`, `AIC`, etc.).
  - `print.did_result()` shows a clean header: estimator, sample sizes,
    FE spec, VCOV type.
  - Outcome expressions like `log(y)` supported.
  - `modelsummary` and `tinytable` added to Suggests.

- **[`calc_att()`](https://yo5uke.com/fixes/reference/calc_att.md) — ATT
  aggregation function**
  ([R/calc_att.R](https://yo5uke.com/fixes/news/R/calc_att.R)): New
  function that separates “calculating ATT” from “running an event
  study”. While
  [`run_es()`](https://yo5uke.com/fixes/reference/run_es.md) produces a
  full dynamic event-study curve (effects by relative time),
  [`calc_att()`](https://yo5uke.com/fixes/reference/calc_att.md) yields
  aggregated ATT estimates with a clear, minimal interface:

  ``` r

  calc_att(data, outcome = y, time = year, timing = g, unit = id,
           estimator = "cs",
           aggregation = "simple")     # overall ATT
  calc_att(..., aggregation = "by_cohort")  # ATT per cohort
  calc_att(..., aggregation = "by_time")    # ATT per calendar period
  ```

  - **Estimators**: `"cs"` (Callaway-Sant’Anna 2021) and `"bjs"`
    (Borusyak et al. 2024). SA, TWM, FLEX are event-study-only
    estimators and are not supported (use
    [`run_es()`](https://yo5uke.com/fixes/reference/run_es.md)).
  - **Aggregation formulas (CS)** match
    [`did::aggte()`](https://bcallaway11.github.io/did/reference/aggte.html)
    for point estimates:
    - `"simple"`: exposure-weighted average over all post-treatment
      (g,t) pairs.
    - `"by_cohort"`: mean ATT(g,t) over post-treatment periods per
      cohort.
    - `"by_time"`: cohort-size-weighted ATT per calendar period.
  - **SEs**: independence assumption (diagonal delta-method).
    Systematically smaller than
    [`did::aggte`](https://bcallaway11.github.io/did/reference/aggte.html)
    SEs, which use influence-function variance accounting for
    within-cohort time correlation. Cluster-robust SE for BJS
    aggregations planned for v0.12.
  - **Returns** an `att_result` S3 object (data.frame subclass) with
    columns `group`, `estimate`, `std.error`, `statistic`, `p.value`,
    `conf_low_XX`/`conf_high_XX`. Attributes: `aggregation`,
    `estimator`, `N`, `N_units`, `N_treated`, `control_group` (CS only),
    `att_gt` (raw CS ATT(g,t) table), `tau_it` (BJS unit-time effects
    table).
  - **`print.att_result()`** S3 method provides a concise summary.

### Testing

- **48 new tests** in `tests/testthat/test-att.R`:
  - Return type and column contract for all aggregation types
  - CS point estimates match
    [`did::aggte()`](https://bcallaway11.github.io/did/reference/aggte.html)
    at tolerance 1e-4
  - BJS estimates match manual `mean(tau_it$tau_hat)` at tolerance 1e-12
  - Multiple `conf.level` support verified

------------------------------------------------------------------------

## fixes 0.10.1 (2026-05-14)

### Internal

- **`run_es.R` refactoring**: Extracted `.make_tidy()` and
  `.es_finalize()` private helpers that centralise the repeated
  tidy-construction / baseline-row / range-filter / attr-stamping
  pattern that was previously duplicated across the `cs`, `sa`, `bjs`,
  `twm`, and `flex` branches (~360 lines removed). No API change; all
  593 tests pass.
- **profvis benchmark harness**: Added `inst/profile/benchmark.R` with
  `profile_estimator()` and `benchmark_all()` utilities. Run
  interactively to generate flamegraph HTML and wall-clock summary
  across estimators before starting Phase 3 Rcpp work.
- **CLAUDE.md documentation**: Added `src/compute_att_gt.cpp` (shipped
  in v0.8.1 but missing from the Rcpp Acceleration Roadmap), and updated
  Package Structure to list all 13 R source files, 5 Rcpp files, and 16
  test files that now exist.
- **Edge-case tests** (19 new, 593 total):
  - `test-twm.R` Test 16 — `trends=TRUE` with exactly 2 pre-treatment
    periods succeeds without warning (boundary condition of the ≥ 2
    requirement).
  - `test-twm.R` Test 17 — explicit `lead_range`/`lag_range` correctly
    trims TWM output and preserves estimates in the shared window
    (exercises `.es_finalize()` filter).
  - `test-flex.R` Test 12 — explicit `lead_range`/`lag_range` trims FLEX
    output correctly.
  - `test-flex.R` Test 13 — FLEX succeeds with an all-treated panel (no
    never-treated groups; `N_nevertreated == 0`).
  - `test-sa.R` — explicit `lead_range`/`lag_range` trims SA output and
    preserves estimates within the shared window.

------------------------------------------------------------------------

## fixes 0.10.0 (2026-05-14)

### New Features

- `run_es(estimator = "twm", trends = TRUE)`: Cohort-specific linear
  trend detrending (Wooldridge 2025, Section 8). Adds `d_g * t`
  regressors so each cohort’s counterfactual trend can deviate linearly
  from the common time trend. Implemented as post-treatment-only cells +
  trend columns; requires \>= 2 pre-treatment periods per cohort. Output
  shows `relative_time >= 0` only (pre-trend testing is deferred to the
  no-trend model).
- `run_es(estimator = "twm", covariates = ~ x1 + x2)`: Full
  Wooldridge (2025) Procedure 5.1 covariate interactions. Adds
  treatment-cell × cohort-demeaned covariate terms
  (`ẋ_{ig} = x_i - x̄_g`) and `i(time, x_j)` conditional parallel-trends
  controls (Eqs. 5.2-5.3).
- `run_es(estimator = "flex", covariates = ~ x1 + x2)`: Full Deb et
  al. (2024) Eq. 3.1 covariate interactions for RCS data. Cell-level
  centering `X_{i,t} - X̄_{g,t}` (Eq. 2.11) with `i(time, x_j)` and
  `i(group, x_j)` conditional PT controls.

### Performance (Rcpp Phase 2)

- `src/indicator_matrix.cpp` — `build_indicator_matrix_cpp()`: Replaces
  the pure-R nested for-loop that fills the 0/1 indicator matrix in the
  SA, TWM, and FLEX estimators. Single shared Rcpp utility, O(N×K)
  integer fill with no R allocation overhead per column.
- `src/iw_aggregation.cpp` — `aggregate_iw_cpp()`: Replaces the R
  event-time aggregation loop and the `t(w) %*% V_sub %*% w`
  quadratic-form VCOV step in SA, TWM, and FLEX. Uses RcppArmadillo
  `arma::mat::submat()` for O(1) VCOV block extraction and
  `arma::as_scalar(w.t() * V_sub * w)` for the quadratic form.
- `src/cov_demeaning.cpp` — `build_cov_interactions_cpp()`: Replaces the
  pure-R covariate demeaning loops in TWM (cohort-level centering) and
  FLEX (group×time cell-level centering) with a single
  `std::unordered_map`-based O(N) grouping pass followed by column-wise
  mean subtraction, then builds the N × (K×p) treatment-cell ×
  centred-covariate interaction matrix in C++.
- `src/bootstrap_cs.cpp` — `bootstrap_cs_cpp()`: Replaces the R-level
  `apply`/`sweep` calls in the CS multiplier bootstrap with
  RcppArmadillo DGEMM for both the pilot and main stages. Uses
  `R::runif()` for Mammen weight generation (column-major fill order
  matches R’s [`matrix()`](https://rdrr.io/r/base/matrix.html)) so
  results are numerically identical to the pure-R path under the same
  [`set.seed()`](https://rdrr.io/r/base/Random.html).
- Added `src/Makevars` and `src/Makevars.win` linking
  `$(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)` for cross-platform
  RcppArmadillo support.
- `RcppArmadillo` added to `LinkingTo` in DESCRIPTION.

### Internal

- 574 tests passing (47 new over v0.9.0: trends 4, TWM-covariates 3,
  FLEX-covariates 3, Rcpp-indicator 4, Rcpp-IW 3, Rcpp-covdemeaning 6,
  Rcpp-bootstrap 12, plus sub-assertions).

------------------------------------------------------------------------

## fixes 0.9.0 (2026-05-14)

### New Features

- `run_es(estimator = "twm")`: Wooldridge (2025) Two-Way Mundlak (TWM)
  estimator for panel data. Implements Procedure 5.1 via POLS on cohort
  x calendar-time treatment cells with two-way FE. Algebraically
  identical to `estimator = "sa"` in the no-covariate base case
  (verified numerically at tolerance 1e-10). The `trends = TRUE`
  extension (cohort-specific linear trend detrending, Wooldridge 2025
  Section 8) is reserved for v0.9.1.
- `run_es(estimator = "flex", group = <group_id>)`: Deb, Norton,
  Wooldridge & Zabel (2024) FLEX estimator for **repeated cross-section
  (RCS)** data. Uses group x calendar-time OLS with group+time FE (no
  unit FE). Algebraically equivalent to the multi-step imputation
  estimator (Proposition 2.1). New `group` argument identifies the
  treatment group (R\_{ig}) each individual belongs to. Default
  clustering is by group. Covariate interactions deferred to v0.9.1.
- Reference PDFs converted to TXT: `papers/Deb et al. (2024).txt`,
  `papers/Woodridge (2025).txt`.

### Internal

- 517 tests passing (41 new: TWM 21, FLEX 20).
- Both new estimators: pre-allocated integer indicator matrix
  construction (same strategy as SA optimisation)
  - feols() + quadratic-form VCOV. No new dependencies.

------------------------------------------------------------------------

## fixes 0.8.1 (2026-05-10)

CRAN release: 2026-05-10

### New Features

- `run_es(estimator = "cs")`: Callaway-Sant’Anna (2021) estimator.
  Computes group-time ATT(g,t) via the unconditional DiD estimand (eq.
  2.8) with never-treated or not-yet-treated control groups. Supports
  `control_group = "nevertreated"` (default) or `"notyettreated"`. The
  full ATT(g,t) matrix is stored as the `att_gt` attribute of the
  result.
- `run_es(estimator = "sa")`: Sun-Abraham (2021) interaction-weighted
  estimator. Aggregates cohort x relative-time interactions by cohort
  share weights. Numerically identical to
  [`fixest::sunab()`](https://lrberge.github.io/fixest/reference/sunab.html)
  to machine precision.
- `run_es(estimator = "bjs")`: Borusyak, Jaravel & Spiess (2024)
  imputation estimator. Fits TWFE on untreated observations only,
  imputes counterfactuals for treated observations, and averages
  treatment effects by horizon. Handles singleton unit fixed effects via
  closed-form recovery.
- `run_es(bootstrap = TRUE)`: Multiplier bootstrap for simultaneous
  confidence bands (Algorithm 1, Callaway & Sant’Anna 2021). Adds
  `conf_low_sim` / `conf_high_sim` columns to the result and stores the
  (g,t)-level bootstrap object as `attr(result, "bootstrap")`.
  Controlled by `B` (draws, default 999) and `boot_seed` arguments.
  Available only when `estimator = "cs"`.
- [`plot_att_gt()`](https://yo5uke.com/fixes/reference/plot_att_gt.md):
  Visualize the full ATT(g,t) matrix from CS results as a heatmap
  (`type = "heatmap"`) or cohort-faceted time series (`type = "facet"`).
  Requires `estimator = "cs"` result as input. When `bootstrap = TRUE`
  was used, the heatmap adds a subtitle with the simultaneous critical
  value and open-diamond markers for simultaneously significant cells;
  the facet adds a lighter simultaneous CI ribbon.
- `plot_es(show_simultaneous = TRUE)`: Overlays the simultaneous
  bootstrap CI (lighter band, alpha 0.15) alongside the pointwise CI
  (alpha 0.3) with a two-entry legend. Errors informatively if bootstrap
  was not run.
- `plot_es_interactive(show_simultaneous = TRUE)`: Adds a second lighter
  ribbon trace for the simultaneous CI and extends the hover tooltip
  with simultaneous CI bounds.

### Bug Fixes

- [`run_es()`](https://yo5uke.com/fixes/reference/run_es.md) bootstrap
  path: [`base::merge()`](https://rdrr.io/r/base/merge.html) silently
  dropped all custom attributes (e.g. `att_gt`, `N_units`, `lead_range`)
  from the `es_result` object. Attributes are now saved and restored
  around the merge, so
  [`plot_att_gt()`](https://yo5uke.com/fixes/reference/plot_att_gt.md)
  works correctly after a `bootstrap = TRUE` run.

### Internal

- Added `did` and `didimputation` to Suggests for numerical agreement
  tests against reference implementations.
- 195 tests passing across all estimators, visualization functions, and
  bootstrap routines.

------------------------------------------------------------------------

## fixes 0.7.2 (2026-05-02)

CRAN release: 2026-05-02

### Bug Fixes

- **Clustered standard errors no longer silently overridden by HC1:**
  - Fixed a bug where specifying `cluster` in
    [`run_es()`](https://yo5uke.com/fixes/reference/run_es.md) did not
    produce clustered standard errors
  - The default `vcov = "HC1"` was being applied after model estimation,
    overriding the clustered SE from `feols()`
  - Now, when `cluster` is specified and `vcov` is left at its default
    (`"HC1"`), the model’s clustered standard errors are used
  - To explicitly request HC1 SEs even when `cluster` is set, pass
    `vcov = "HC1"` together with `cluster = NULL`
  - Applies to both `classic` and `sunab` methods
  - This resolves discrepancies between
    [`run_es()`](https://yo5uke.com/fixes/reference/run_es.md) results
    and equivalent direct
    [`fixest::feols()`](https://lrberge.github.io/fixest/reference/feols.html)
    calls

------------------------------------------------------------------------

## fixes 0.7.1 (2025-11-02)

### Bug Fixes

- **Lead/lag range filtering now enforced:**
  - Fixed bug where `lead_range` and `lag_range` parameters were
    ignored, causing all estimated coefficients to be returned
    regardless of specified ranges
  - Results now correctly filtered to only include coefficients within
    `[-lead_range, lag_range]`
  - Applies to both `classic` and `sunab` methods
- **sunab() namespace error resolved:**
  - Fixed `'sunab' is not an exported object from 'namespace:fixest'`
    error when using `method = "sunab"`
  - Formula environment now correctly configured to access `sunab`
    function from fixest package
  - sunab() calls now work identically to direct fixest::feols() usage

### Enhancements

- **Baseline row now included in sunab results:**
  - The `baseline` parameter (default: -1) now applies to both `classic`
    and `sunab` methods
  - Baseline period added to results with `estimate = 0`,
    `std.error = 0`, and `is_baseline = TRUE`
  - Provides consistent behavior across both estimation methods
  - Updated documentation to clarify that `baseline` is used for both
    methods
- **Improved term column formatting:**
  - Term values now display as clean numeric strings (e.g., `"-9"`,
    `"0"`, `"3"`)
  - Removed complex fixest-specific notation (e.g., `"year::-9"`) for
    better readability
  - Consistent formatting across both classic and sunab methods
- **Modernized code with native pipe operator:**
  - Replaced tidyverse pipe (`%>%`) with base R pipe (`|>`) throughout
    the package
  - Requires R \>= 4.1.0 (already enforced in DESCRIPTION)
  - Cleaner code with no functional changes

### Testing

- **Comprehensive test suite overhaul:**
  - Added 25+ test cases covering all bug fixes and enhancements
  - Separate test suites for classic, staggered, and sunab methods
  - Tests for baseline row inclusion, term formatting, and range
    filtering
  - Integration tests with fixest built-in datasets (base_did,
    base_stagg)
  - Tests use native pipe operator (`|>`)

### Compatibility

- Fully backward compatible with previous versions
- All changes maintain existing API and functionality
- Enhanced robustness and user experience

------------------------------------------------------------------------

## fixes 0.7.0 (2025-11-01)

### Bug Fixes

- **Critical fix for relative_time calculation in non-staggered
  designs:**
  - Fixed bug where `relative_time` was NA for all coefficients except
    baseline when using non-staggered treatment timing
  - The reference period in
    [`fixest::i()`](https://lrberge.github.io/fixest/reference/i.html)
    now correctly corresponds to the `baseline` parameter
  - For example, with `timing = 5` and `baseline = -1`, period 4 (not
    period 5) is now used as the reference
  - This ensures proper event study estimation with the intended
    baseline period

### New Features

- **Interactive plotting with plotly:**
  - New function
    [`plot_es_interactive()`](https://yo5uke.com/fixes/reference/plot_es_interactive.md)
    for creating interactive event study plots
  - Hover tooltips display: relative time, point estimate, confidence
    intervals, standard error, and p-value
  - Supports customizable confidence levels, colors, and styling options
  - Optional ribbon or point-only display modes
  - Fully compatible with `es_result` objects from
    [`run_es()`](https://yo5uke.com/fixes/reference/run_es.md)

### Performance Improvements

- **Optimized relative_time extraction:**
  - Vectorized term parsing replaces previous
    [`sapply()`](https://rdrr.io/r/base/lapply.html) approach for
    significant performance gains
  - Faster processing of large event study results
- **Enhanced error reporting:**
  - More informative warnings when term parsing fails
  - Better debugging information for malformed coefficient names

### Compatibility

- Fully backward compatible with previous versions
- Requires R \>= 4.1.0
- Interactive plotting requires `plotly` package (optional, suggested
  dependency)

------------------------------------------------------------------------

## fixes 0.5.0 (2025-07-06)

CRAN release: 2025-07-07

### New Features

- **Multiple Confidence Intervals:**  
  [`run_es()`](https://yo5uke.com/fixes/reference/run_es.md) and
  [`plot_es()`](https://yo5uke.com/fixes/reference/plot_es.md) now
  support returning and visualizing multiple confidence levels (e.g.,
  90%, 95%, 99%) in a single analysis.
- **Enhanced Plotting Options:**  
  [`plot_es()`](https://yo5uke.com/fixes/reference/plot_es.md) adds
  `ci_level` selection, more theme options (`theme_style`), and improved
  ribbon/error bar display.
- **Expanded Error Handling:**  
  More informative error reporting for all argument validation and model
  mis-specification.
- **Documentation Overhaul:**  
  Updated README, vignette, and roxygen docs to reflect all new features
  and usage patterns.

### Improvements

- **Input Validation:**  
  Stricter checks for all arguments and clearer, friendlier error
  messages.
- **Staggered Timing:**  
  More robust handling of partial or missing treatment timing.
- **Code Refactoring:**  
  Internal codebase cleaned for maintainability and reliability.

### Bug Fixes

- Fixed: Confidence intervals always correctly labeled and included in
  output.
- Fixed: Dummy variables not created for untreated/never-treated units
  in staggered settings.
- Fixed: CRAN warnings by declaring all global variables used in tidy
  evaluation.

### Compatibility

- Fully backward compatible with previous versions.
- Minimum required R version remains 4.1.0.

## fixes 0.4.1 (2025-06-30)

CRAN release: 2025-06-30

### New Features

- **Support for untreated units (`NA` in timing):**
  - Units with missing `timing` are now retained as never-treated
    controls (staggered only).
- **Flexible `weights` input:**
  - Accepts formulas (`~ popwt`), bare names (`popwt`), or character
    strings (`"popwt"`).
- **Improved `cluster` input handling:**
  - Now supports formula, character vector, and bare names consistently.

### Improvements

- Better validation and error messages across all inputs.
- Warning added if `unit` is supplied without `time_transform = TRUE`.
- Documentation switched to Markdown-style lists for better readability.

### Bug Fixes

- Ensured untreated units don’t receive event dummies.
- Improved internal dummy creation and baseline handling.

### Compatibility

- Fully backward compatible with previous versions.

## fixes 0.4.0 (May 25, 2025)

CRAN release: 2025-05-25

### New Features

- Added support for **staggered treatment timing**:
  - New `staggered = TRUE` option allows `timing` to vary by unit (e.g.,
    treatment year column).
  - Units with `NA` in `timing` are safely retained as untreated.
- Added support for **observation weights**:
  - Use the `weights` argument (e.g., `~ popwt`) to run weighted
    regressions.
- **Automatic lead/lag range detection**:
  - If `lead_range` or `lag_range` is `NULL`, the function computes the
    maximum feasible range from the data.

### Improvements

- Safer handling of collinearity and untreated units.
- Warning is issued when `unit` is specified without
  `time_transform = TRUE`.
- Input validation and internal structure cleaned up for better
  robustness.

### Compatibility

- Backward compatible with previous versions.

## fixes 0.3.1 (May 18, 2025)

### Improvements

- Improved default x-axis scaling in
  [`plot_es()`](https://yo5uke.com/fixes/reference/plot_es.md):
  - The x-axis now uses
    [`ggplot2::scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
    with integer breaks spaced by 1, aligned to the `relative_time`
    range.
  - This provides more readable and consistent event time labeling in
    plots.

## fixes 0.3.0 (May 18, 2025)

### New Features

- Added support for irregular and non-numeric time variables such as
  `Date`:
  - Introduced `time_transform = TRUE` to automatically convert the
    `time` variable into a unit-level sequential index (1, 2, 3, …) for
    event study estimation.
  - Added `unit` argument to specify the panel unit identifier required
    when `time_transform = TRUE`.
  - This enables seamless analysis of panel data with monthly,
    quarterly, or irregular time formats.

### Improvements

- Updated input validation:
  - Now accepts `Date` class in the `time` variable and converts it
    automatically to numeric if `time_transform = FALSE`.
  - Improved error messages and warnings for better user feedback when
    `unit` is missing or `time` is of unsupported type.
- Enhanced documentation:
  - Updated `@examples` in the function documentation to include
    `Date`-based examples.
  - Extended vignette to illustrate `time_transform` usage.
  - Expanded `README.md` to describe irregular time handling and
    demonstrate new use cases.
- Strengthened test coverage:
  - Added new unit tests for `time_transform`, `unit` handling, and
    `Date` conversion edge cases.

### Compatibility

- No breaking changes.
- This is a backward-compatible **minor release** introducing
  significant new functionality for irregular time formats.

------------------------------------------------------------------------

## fixes 0.2.1 (May 11, 2025)

CRAN release: 2025-05-10

### Minor Improvements

- Added a warning when lead/lag dummy variable names (e.g., `lead1`,
  `lag0`) already exist in the dataset to prevent accidental
  overwriting.
- Added a warning when filtered data (based on `lead_range`,
  `lag_range`, and `interval`) has fewer than 10 rows, helping users
  identify overly narrow estimation windows.
- Improved handling of the `treatment` variable: it is now coerced to
  logical using [`as.logical()`](https://rdrr.io/r/base/logical.html) to
  support both binary numeric (`0/1`) and logical (`TRUE/FALSE`)
  formats.
- Fixed internal bug in model formula construction:
  - Previously, fixed effects specified via the `fe` argument (e.g.,
    `~ id + year`) were combined using `model_formula | fe_text`, which
    caused evaluation errors during tests.
  - Now, the full model formula is safely constructed as a string and
    parsed with [`as.formula()`](https://rdrr.io/r/stats/formula.html)
    to ensure compatibility with
    [`fixest::feols()`](https://lrberge.github.io/fixest/reference/feols.html).

### Compatibility

- No breaking changes. This is a backward-compatible patch release with
  internal robustness improvements and enhanced error handling.

## fixes 0.2.0 (March 29, 2025)

CRAN release: 2025-04-23

### Major Features

- **Support for covariates in
  [`run_es()`](https://yo5uke.com/fixes/reference/run_es.md)**:
  - Covariates must now be specified as a **one-sided formula** (e.g.,
    `~ x1 + x2`).
- **Fixed Effects and Clustering Interface Updated**:
  - `fe` and `cluster` arguments must now be specified using a
    **one-sided formula** (e.g., `~ id + year`).
  - Character vector input for `cluster` is still accepted.
  - Improved internal handling and validation of fixed effects and
    clustering variables.
- Improved error messages for invalid or missing variable names.

## fixes 0.1.0 (March 17, 2025)

CRAN release: 2025-03-18

### Major Changes

- `fe_var` argument now supports additive notation (`firm_id + year`)
  instead of character vectors.
- Improved [`plot_es()`](https://yo5uke.com/fixes/reference/plot_es.md)
  efficiency and documentation.

### Minor Improvements

- Fixed cluster variable handling to correctly reference column names.
- Updated test cases to support new `fe` notation.
- Improved package documentation.

## fixes 0.0.2 (Enhancements & Fixes)

This version introduced several enhancements and refinements to improve
usability and maintainability.

### Improvements

- Refactored variable name handling:
  - `outcome_var`, `treated_var`, and `time_var` are now processed using
    [`rlang::ensym()`](https://rlang.r-lib.org/reference/defusing-advanced.html)
    for better robustness.
  - `fe_var` and `cluster_var` handling improved for more reliable
    column referencing.
- More informative **error messages** when variables are missing in the
  dataset.
- Enhanced **baseline term handling** in regression models to prevent
  incorrect factor levels.
- Improved [`plot_es()`](https://yo5uke.com/fixes/reference/plot_es.md)
  function:
  - Added validation checks to ensure required columns (`relative_time`,
    `estimate`, etc.) are present.
  - Adjusted confidence interval calculations to avoid missing values in
    error bars.

### Fixes

- Addressed an issue where `baseline` handling could lead to incorrect
  sorting of lead/lag terms.
- Resolved a minor inconsistency in fixed effects variable name parsing.

## fixes 0.0.1 (Initial Release)

This is the first release of the `fixes` package, providing tools for
estimating and visualizing event study models with fixed effects.

### Features

- **[`run_es()`](https://yo5uke.com/fixes/reference/run_es.md)**: A
  function to estimate event study models using
  [`fixest::feols()`](https://lrberge.github.io/fixest/reference/feols.html),
  generating lead and lag variables automatically.
  - Supports **fixed effects** (`fe_var` as character vector).
  - Allows **clustered standard errors** via `cluster_var`.
  - Handles **time scaling** through the `interval` argument.
- **[`plot_es()`](https://yo5uke.com/fixes/reference/plot_es.md)**: A
  function to visualize event study results with ggplot2.
  - Supports **ribbon-style confidence intervals** (`type = "ribbon"`,
    default).
  - Allows **error bar visualization** (`type = "errorbar"`).
  - Customizable plot elements including colors, line styles, and
    reference lines.

### Initial Implementation

- **Fixed effects regression model** using
  [`fixest::feols()`](https://lrberge.github.io/fixest/reference/feols.html).
- **Automated creation of lead/lag dummy variables** based on treatment
  timing.
- **Baseline period exclusion** to avoid multicollinearity.
- **Support for custom time intervals (`interval` argument)**.

### Limitations in 0.0.1

- Fixed effects must be specified as a **character vector
  (`c("firm_id", "year")`)**.
- Clustered standard errors require variable names as **character
  strings (`"state_id"`)**.
- No direct support for additive notation in `fe_var`.
