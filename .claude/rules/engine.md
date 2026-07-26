---
paths:
  - "R/fe_ols.R"
  - "R/fe_solver.R"
  - "R/estimators_twfe.R"
  - "R/did.R"
---

# Internal FE-OLS engine

`R/fe_ols.R` `.fit_fe_ols()`:

- k-way demeaning via `demean_kway_cpp` (weighted WLS supported).
- Keep-first sequential-Cholesky collinearity drop.
- VCOV: iid/HC1/one-way-CRV1, matching fixest's ssc. Student-t inference (df = N − K_full; G − 1 under clustering).
- Fit statistics match `broom::glance.fixest`: weighted RSS/TSS R²s, Gaussian logLik on **unweighted** residuals, AIC/BIC param count = coefs + FE dof.
- Returns `used` (estimation-sample indices). Zero/NA weights drop rows; negative weights error.

`R/fe_solver.R` `.solve_fe_2way()`: 2-way FE solver used by BJS.

fixest is required only for: `run_es(method = "sunab")` legacy path, multiway clustering, non-empty `vcov_args`, vcov types beyond iid/hetero/HC1, and FE/cluster specs beyond plain columns (e.g. `~ id^year`). Gate any new fixest touchpoint with `.require_fixest()` (R/utils-internal.R) — the CI no-fixest job builds with hard deps only.
