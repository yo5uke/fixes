## Test environments

- Local: Windows 11 x64 (build 26200), R 4.6.0
- GitHub Actions CI:
  - macOS (R release)
  - Windows (R release)
  - Ubuntu (R devel, release, oldrel-1)

## R CMD check results

0 errors | 0 warnings | 0 notes

`devtools::check()` output (local, Windows 11, R 4.6.0):

```
Status: OK
0 errors ✔ | 0 warnings ✔ | 0 notes ✔
```

Two INFO messages appeared during the check (not warnings or notes):

- **Packages suggested but not available for checking: `plotly`,
  `didimputation`, `modelsummary`.**  
  These are in `Suggests` and are gated throughout the package with
  `skip_if_not_installed()` in tests and `requireNamespace(..., quietly = TRUE)`
  in production code. All functionality that depends on them degrades gracefully
  when the package is absent.

- **Package unavailable to check Rd xrefs: `modelsummary`.**  
  The `\link[modelsummary]{modelsummary}` cross-reference in `run_did()`'s
  documentation could not be resolved because `modelsummary` was not installed
  in the check environment. The reference is correct; the link simply renders as
  plain text when the package is absent.

## Downstream dependencies

None (new package, not yet on CRAN).

## New in this submission (0.11.1)

This document covers changes since v0.8.1, the last version for which
`cran-comments.md` was updated.

### New estimators and functions (v0.9.0 – v0.11.0)

- **`run_es(estimator = "twm")`**: Wooldridge (2025) Two-Way Mundlak
  estimator. Supports `covariates =` (Procedure 5.1 covariate interactions)
  and `trends = TRUE` (cohort-specific linear detrending, Section 8).
- **`run_es(estimator = "flex", group = <id>)`**: Deb, Norton, Wooldridge &
  Zabel (2024) FLEX estimator for repeated cross-section data.
- **`honest_sensitivity()`**: Robust post-estimation inference under
  violations of parallel trends (Rambachan & Roth 2023). Implements the
  Andrews-Roth-Pakes conditional moment-inequality test for the
  `Delta^RM` and `Delta^SD` restriction families. Pure-R reimplementation
  — no dependency on the `HonestDiD` package (which is in Suggests for
  numerical agreement tests only).
- **`calc_att()`**: Aggregated ATT estimation (overall, by cohort, by
  calendar time) for CS and BJS estimators. Separates "ATT calculation"
  from "event study estimation" at the API level.
- **`run_did()`**: Basic TWFE DiD with `broom`-compatible `tidy()` /
  `glance()` S3 methods for `modelsummary` / `tinytable` output.
- **`plot_honest()`** / **`autoplot.honest_result()`**: Sensitivity plot
  for `honest_sensitivity()` output.

### Rcpp acceleration (v0.10.0)

Four new Rcpp / RcppArmadillo routines were added to reduce estimation time
for large panels:

- `src/indicator_matrix.cpp`: O(N×K) integer fill for the treatment-cell
  indicator matrix shared by SA, TWM, and FLEX.
- `src/iw_aggregation.cpp`: Interaction-weighted aggregation with
  quadratic-form VCOV via `arma::as_scalar(w.t() * V_sub * w)`.
- `src/cov_demeaning.cpp`: Group-level covariate centering + interaction
  matrix for TWM and FLEX covariate paths.
- `src/bootstrap_cs.cpp`: Mammen multiplier bootstrap via RcppArmadillo
  DGEMM. Column-major fill order matches `R::matrix()` for RNG
  reproducibility under the same `set.seed()`.

### Improvements (v0.11.1)

- **Diagnostic warning for ambiguous `timing = NA`** in
  `run_es(estimator = "twfe", staggered = TRUE)`: a `warning()` is now
  emitted when any unit has `treatment = 1` in at least one row but
  `timing = NA`. `NA` is the standard "never treated" sentinel (matching
  `did::att_gt()` and `fixest::sunab()` conventions), but a unit that
  appears treated yet has no timing information is almost certainly a data
  error. The warning names up to 5 affected unit IDs for quick diagnosis.
- **`@param timing` documentation** now explicitly states the `NA = never
  treated` convention for all staggered estimators.
