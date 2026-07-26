# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`fixes` is an R package for staggered difference-in-differences (DiD) estimation and visualization. Version 1.0.0 introduced a noun-based API; the pre-1.0 verb-based API is deprecated but numerically identical and fully functional (`R/deprecated.R`).

**Core functions:** `event_study()` (dynamic effects by relative time, all estimators), `att()` (aggregated ATT — overall/by-cohort/by-time, CS & BJS), `did()` (TWFE with `tidy`/`glance`, modelsummary-compatible), `att_gt()` (CS ATT(g,t) table), `contamination_weights()` (SA diagnostics), `honest_sensitivity()` (Rambachan-Roth, ARP-conditional). Every result class has a `plot()` S3 method (`interactive = TRUE` → plotly); `tidy`/`glance`/`autoplot` exist for `es_result`, `att_result`, `did_result`.

**Design principle:** `event_study()` for the full dynamic curve, `att()` for scalar/per-cohort/per-time ATT, `did()` for a single DiD coefficient. Estimators are selected via `estimator = "twfe"|"cs"|"sa"|"bjs"|"twm"|"flex"`.

**Code layout:** one file per concern — `R/estimators_<name>.R` implements each estimator's `.run_<name>()`, `R/plot_*.R` the plotting impl, `R/tidy-*_result.R` the broom methods, `R/fe_ols.R`/`R/fe_solver.R` the internal engine. `src/` mirrors this for C++ kernels (see `.claude/rules/rcpp.md`).

## API contract

- **Never change `event_study()`'s signature**, even as internal implementations evolve.
- **Never remove or change the behavior of the deprecated wrappers** (`R/deprecated.R`) — they must stay contractually identical to their successors (enforced by `tests/testthat/test-deprecated.R`).
- The `es_result` structure must stay consistent across all estimators, and `plot.es_result()` must accept results from any of them.
- `staggered = NULL` (default) is inferred from `timing`: a column name → staggered, a scalar → universal. Explicit values override.
- New-API arguments use snake_case (`conf_level`, `boot_reps`, `boot_alpha`, `m_grid`, `grid_points`, `n_pre_periods`, `n_post_periods`).
- `did_result` has no raw model object — use `$coeftable`, `$vcov`, `$fit_stats`, `$df.t`.

## Dependency architecture

fixest is in **Suggests, not Imports** — default estimation runs on the internal FE-OLS engine, not fixest (see `.claude/rules/engine.md` for the engine internals and exactly which code paths still require fixest). CI has a **no-fixest job** (hard deps only); every test touching fixest needs `skip_if_not_installed("fixest")`.

## Known limitations

- Large panels (~1M rows): the classic event-study path is ~2.5x fixest wall time (R-level matrix copies dominate; compute kernels are <20% of it).
- BJS aggregation in `att()` uses naive independence SE, not cluster-robust.
- `honest_sensitivity()` covers ARP-conditional only; FLCI/hybrid methods are not implemented.
- Synthetic DiD and a Bayesian event study are not implemented.

## Key commands

```r
devtools::load_all()   # build + load
devtools::test()       # run tests
testthat::test_file("tests/testthat/test-event_study.R")
devtools::check()      # R CMD check
devtools::document()   # roxygen
```

Tests set `options(lifecycle_verbosity = "quiet")` (`tests/testthat/setup-lifecycle.R`); deprecation itself is tested via `lifecycle::expect_deprecated()` in `test-deprecated.R`.

CI runs R-CMD-check on macOS/Windows (release) and Ubuntu (devel/release/oldrel-1), plus the no-fixest job.

## Testing policy

- **Write tests before implementing any new estimator.**
- Numerical agreement tests against reference implementations at tolerance `1e-6` (engine-level oracle tests at `1e-8`): TWFE/`did()` vs `fixest::feols`, CS vs `did::att_gt()`, SA vs `fixest::sunab()`, BJS vs `didimputation`.
- Old-vs-new API equivalence uses `expect_identical` after stripping the `call` attribute (`test-deprecated.R`).
- Every test using fixest/did/didimputation/modelsummary/plotly needs `skip_if_not_installed()`.
- Never break existing tests.

## Timing, VCOV, and confidence intervals

- Regular time: numeric time column + `interval`. Irregular time: `time_transform = TRUE` + `unit` (then `timing` must be an index, not the original time value).
- Universal treatment: scalar `timing` (or `staggered = FALSE`). Staggered adoption: `timing` names a column of unit-specific times, `NA` = never-treated (or `staggered = TRUE`).
- Default vcov is "HC1"; cluster + default vcov → CRV1 automatically. The internal engine covers iid/hetero/HC1 + one-way cluster only; anything else needs fixest.
- p-values are Student-t: df = N − K_full, or G − 1 under one-way clustering (matches fixest/broom).
- `conf_level` accepts a vector → `conf_low_90`/`conf_high_90`/... columns (normal quantiles for event-study CIs; `tidy.did_result(conf.int=)` uses the stored t df).

## Common gotchas

1. Fixed-effects formula must be one-sided (`~ id + year`), not two-sided.
2. Baseline range must be within `[-lead_range, lag_range]`.
3. `time_transform = TRUE` cannot combine with `staggered = TRUE`.
4. Avoid `unit` as a data column name — conflicts with ggplot2.
5. Zero/NA weights silently drop rows in the FE-OLS engine; negative weights error.
6. README.md is generated — edit `README.Rmd` and knit manually; never edit `README.md` directly.

## Papers

Reference papers live in `papers/`. **Always run `/read papers/<file>.pdf` before implementing or modifying an estimator** — see `.claude/rules/estimators.md` for the paper-to-estimator map and estimator-specific implementation notes.
