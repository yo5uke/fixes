---
trigger: always_on
---

# Project context

The full spec lives in `@/CLAUDE.md` (project overview, API contract, dependency architecture, testing policy, timing/vcov conventions, common gotchas, commit message format) and in `@/.claude/rules/engine.md`, `@/.claude/rules/estimators.md`, `@/.claude/rules/rcpp.md` (engine internals, estimator-specific math, Rcpp conventions). Treat `CLAUDE.md` as the source of truth — this file is a pointer into it, not a copy, so it can't drift out of date.

Condensed orientation, for quick reference:

- `fixes` is an R package for staggered difference-in-differences (DiD) estimation and visualization. The 1.0.0 noun API (`event_study()`, `att()`, `did()`, `att_gt()`, `contamination_weights()`, `honest_sensitivity()`) is current; the pre-1.0 verb API in `R/deprecated.R` is deprecated but must stay numerically and contractually identical to its successors — never remove or change its behavior.
- Never change `event_study()`'s signature, even as internal implementations evolve.
- fixest is in Suggests, not Imports — default estimation runs on the internal FE-OLS engine (`R/fe_ols.R`, `R/fe_solver.R`), and a CI "no-fixest" job enforces this. Any new fixest touchpoint must be gated behind `.require_fixest()`.
- Numerical-agreement tests are the correctness bar for estimators: TWFE/`did()` vs `fixest::feols`, CS vs `did::att_gt()`, SA vs `fixest::sunab()`, BJS vs `didimputation`, at tolerance `1e-6` (engine-level oracle tests at `1e-8`).
- `README.md` is generated from `README.Rmd` — never edit `README.md` directly.

When reviewing a change to an estimator's math, check the relevant paper in `papers/` against `@/.claude/rules/estimators.md`'s paper-to-estimator map before judging correctness — formulas for weights, aggregation, and identification must match the paper, not memory or intuition.
