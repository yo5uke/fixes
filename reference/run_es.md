# Event Study Estimation for Panel Data

Runs an event study regression on panel data, supporting both classic
(universal timing) and staggered (unit-varying timing via `sunab`). The
function builds the design (lead/lag factor or `sunab`), estimates with
[`fixest::feols()`](https://lrberge.github.io/fixest/reference/feols.html),
and returns a tidy table with metadata.

## Usage

``` r
run_es(
  data,
  outcome,
  treatment = NULL,
  time,
  timing,
  fe = NULL,
  lead_range = NULL,
  lag_range = NULL,
  covariates = NULL,
  cluster = NULL,
  weights = NULL,
  baseline = -1L,
  interval = 1,
  time_transform = FALSE,
  unit = NULL,
  staggered = FALSE,
  method = c("classic", "sunab"),
  estimator = c("twfe", "cs", "sa", "bjs", "twm", "flex"),
  control_group = c("nevertreated", "notyettreated"),
  anticipation = 0L,
  conf.level = 0.95,
  vcov = "HC1",
  vcov_args = list(),
  bootstrap = FALSE,
  B = 999L,
  alpha = 0.05,
  boot_seed = NULL,
  group = NULL,
  trends = FALSE
)
```

## Arguments

- data:

  A data.frame containing panel data.

- outcome:

  Unquoted outcome (name or expression, e.g., `log(y)`).

- treatment:

  Unquoted treatment indicator (0/1 or logical). Used only when
  `method = "classic"`.

- time:

  Unquoted time variable (numeric or Date).

- timing:

  For `classic`: a numeric/Date (universal) or a variable (unquoted) if
  `staggered = TRUE`. For `sunab`: an unquoted variable with adoption
  time. For `estimator = "cs"`: unquoted column giving each unit's first
  treatment period (`NA` = never treated). **Convention for staggered
  estimators (cs, sa, bjs, twm, flex, classic with
  `staggered = TRUE`):** `NA` in the `timing` column means the unit is
  *never treated* and will be used as a control. This follows the same
  convention as
  [`did::att_gt()`](https://bcallaway11.github.io/did/reference/att_gt.html),
  [`fixest::sunab()`](https://lrberge.github.io/fixest/reference/sunab.html),
  and `didimputation`. If `NA` instead represents *missing* treatment
  timing for an otherwise treated unit, those observations will be
  silently absorbed into the control group, which is almost certainly
  wrong. For `estimator = "twfe"` with `staggered = TRUE`, a warning is
  emitted when units with `treatment = 1` also have `timing = NA`.

- fe:

  One-sided fixed-effects formula, e.g., `~ id + year`. Can be `NULL`
  for no fixed effects. Ignored when `estimator = "cs"`.

- lead_range, lag_range:

  Integers for pre/post windows. If `NULL`, determined automatically.

- covariates:

  One-sided formula of additional controls, e.g., `~ x1 + log(x2)`.

- cluster:

  Cluster specification (one-sided formula like `~ id + year`, a single
  character column name, or a vector of length `nrow(data)`).

- weights:

  Observation weights (a name/one-sided formula or a numeric vector of
  length `nrow(data)`).

- baseline:

  Integer baseline period (default `-1`); reference period excluded from
  results for both `"classic"` and `"sunab"` methods.

- interval:

  Numeric spacing of the time variable (default `1`; ignored internally
  for Dates).

- time_transform:

  Logical; if `TRUE`, creates consecutive integer time within unit.

- unit:

  Unit identifier variable (required when `estimator = "cs"` or
  `time_transform = TRUE`); also used for metadata when supplied.

- staggered:

  Logical; if `TRUE`, `timing` is a variable (classic) or is used by
  `sunab`.

- method:

  Either `"classic"` or `"sunab"` (default: `"classic"`).

- estimator:

  Estimation strategy: `"twfe"` (default, existing fixest-based paths),
  `"cs"` for the Callaway-Sant'Anna (2021) group-time ATT estimator, or
  `"sa"` for the Sun-Abraham (2021) interaction-weighted estimator.

- control_group:

  For `estimator = "cs"`: comparison group, either `"nevertreated"`
  (default) or `"notyettreated"`.

- anticipation:

  For `estimator = "cs"`: number of anticipation periods before
  treatment (non-negative integer, default `0L`).

- conf.level:

  Numeric vector of confidence levels (default `0.95`).

- vcov:

  VCOV type passed to `fixest::vcov()` or used via
  `broom::tidy(vcov = ...)`. Default `"HC1"`.

- vcov_args:

  List of additional arguments forwarded to `fixest::vcov()`.

- bootstrap:

  Logical; if `TRUE` and `estimator = "cs"`, compute simultaneous
  confidence bands via the multiplier bootstrap (Algorithm 1, Callaway
  and Sant'Anna 2021). Adds `conf_low_sim` and `conf_high_sim` columns
  to the result and stores the full (g,t)-level bootstrap object as
  `attr(result, "bootstrap")`. Default `FALSE`.

- B:

  Integer number of bootstrap draws (default `999`). Used only when
  `bootstrap = TRUE` and `estimator = "cs"`.

- alpha:

  Significance level for the simultaneous band (default `0.05`). Note:
  this is independent of `conf.level`, which governs the pointwise
  delta-method CIs.

- boot_seed:

  Integer seed for the bootstrap RNG; `NULL` (default) does not set a
  seed. Pass an integer for reproducible results.

- group:

  Unquoted group identifier for `estimator = "flex"` only. Identifies
  which treatment group (cohort) each observation belongs to in a
  repeated cross-section design (\\R\_{ig}\\ in Deb et al. 2024). Each
  group must map to exactly one value of `timing` (or `NA` for
  never-treated groups). Not used by other estimators.

- trends:

  Logical; for `estimator = "twm"` only. When `TRUE`, adds
  cohort-specific linear trend regressors \\d_g \cdot t\\ to the
  Procedure 5.1 regression (Wooldridge 2025, Section 8), allowing each
  cohort's counterfactual trend to deviate linearly from the common time
  trend. Requires at least 2 pre-treatment periods per cohort. Default
  `FALSE`.

## Value

A `data.frame` of class `"es_result"` with columns:

- `term`, `estimate`, `std.error`, `statistic`, `p.value`

- `conf_low_XX`, `conf_high_XX` (for each requested `conf.level`)

- `relative_time` (integer; 0 = event), `is_baseline` (logical; marks
  the reference period)

Attributes include: `lead_range`, `lag_range`, `baseline`, `interval`,
`call`, `model_formula`, `conf.level`, `N`, `N_units`, `N_treated`,
`N_nevertreated`, `fe`, `vcov_type`, `cluster_vars`, `staggered`,
`sunab_used`.

## Key Features

- One-step event study: specify outcome, treatment, time, timing, fixed
  effects, and (optionally) covariates.

- Switch between Classic (factor expansion) and Staggered-SAFE
  (`method = "sunab"`).

- Flexible clustering, weights, and VCOV choices (e.g.,
  `vcov = "HC1" | "HC3" | "CR2" | "iid" ...`).

- Automatic lead/lag window detection and customizable baseline period.

- Returns an `"es_result"` object compatible with
  [`print()`](https://rdrr.io/r/base/print.html) and `autoplot()`.
