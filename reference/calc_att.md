# Compute ATT Aggregations for Staggered Adoption Designs

Estimates average treatment effects on the treated (ATT) using either
the Callaway-Sant'Anna (2021) or Borusyak-Jaravel-Spiess (2024)
estimator, aggregated to a single summary effect (`"simple"`),
per-cohort effects (`"by_cohort"`), or per calendar-time effects
(`"by_time"`).

## Usage

``` r
calc_att(
  data,
  outcome,
  treatment = NULL,
  time,
  timing,
  fe = NULL,
  covariates = NULL,
  cluster = NULL,
  weights = NULL,
  interval = 1,
  time_transform = FALSE,
  unit = NULL,
  estimator = c("cs", "bjs"),
  aggregation = c("simple", "by_cohort", "by_time"),
  control_group = c("nevertreated", "notyettreated"),
  anticipation = 0L,
  conf.level = 0.95,
  vcov = "HC1",
  vcov_args = list()
)
```

## Arguments

- data:

  A data.frame containing panel data.

- outcome:

  Unquoted outcome variable (name or expression, e.g., `log(y)`).

- treatment:

  Unused; reserved for future use.

- time:

  Unquoted calendar time variable (numeric).

- timing:

  Unquoted column giving each unit's first treatment period (`NA` =
  never treated).

- fe:

  Ignored (CS and BJS absorb fixed effects internally).

- covariates:

  Ignored; reserved for future use.

- cluster:

  Ignored; reserved for future use.

- weights:

  Ignored; reserved for future use.

- interval:

  Numeric time spacing (default `1`; informational only).

- time_transform:

  Logical; if `TRUE`, creates consecutive integer time within unit via
  [`dplyr::dense_rank()`](https://dplyr.tidyverse.org/reference/row_number.html).
  Requires `unit`.

- unit:

  Unquoted unit identifier (required).

- estimator:

  Estimation strategy: `"cs"` (Callaway-Sant'Anna 2021, default) or
  `"bjs"` (Borusyak-Jaravel-Spiess 2024).

- aggregation:

  Aggregation type: `"simple"` (overall ATT, default), `"by_cohort"`
  (one ATT per treatment cohort), or `"by_time"` (one ATT per calendar
  time period).

- control_group:

  For `estimator = "cs"`: comparison group, `"nevertreated"` (default)
  or `"notyettreated"`.

- anticipation:

  For `estimator = "cs"`: number of anticipation periods before
  treatment (non-negative integer, default `0L`).

- conf.level:

  Numeric confidence level(s) (default `0.95`). Multiple levels are
  supported, e.g., `c(0.90, 0.95)`.

- vcov:

  Ignored (SE is analytical for CS; approximate for BJS).

- vcov_args:

  Ignored.

## Value

A `data.frame` of class `"att_result"` with columns:

- `group`:

  Cohort or calendar time (`NA` for `"simple"`).

- `estimate`:

  ATT point estimate.

- `std.error`:

  Standard error.

- `statistic`:

  t-statistic (`estimate / std.error`).

- `p.value`:

  Two-sided p-value (normal approximation).

- `conf_low_XX`, `conf_high_XX`:

  CI bounds for each `conf.level`.

Attributes: `aggregation`, `estimator`, `conf.level`, `N`, `N_units`,
`N_treated`, `N_nevertreated`, `control_group` (CS only), `att_gt` (CS
raw ATT(g,t) table), `tau_it` (BJS unit-time effects table).

## Details

This function complements
[`run_es()`](https://yo5uke.com/fixes/reference/run_es.md): use
[`run_es()`](https://yo5uke.com/fixes/reference/run_es.md) when you want
a full event-study curve (dynamic effects by relative time), and
`calc_att()` when you want aggregated ATT estimates that collapse the
time dimension.

## Aggregation formulas (CS estimator)

- **simple**: \\\theta = \sum_g (n_g/n\_{treated}) \cdot
  \overline{ATT(g,\cdot)}\\ where \\\overline{ATT(g,\cdot)}\\ is the
  mean over post-treatment periods.

- **by_cohort**: \\\theta(g) = \overline{ATT(g,\cdot)}\\ per cohort.

- **by_time**: \\\theta(t) = \sum\_{g \le t} w(g,t) \cdot ATT(g,t)\\
  with \\w(g,t) = n_g / \sum\_{g' \le t} n\_{g'}\\.

## Standard errors (BJS estimator)

BJS SEs are approximate (naive sample variance of unit-time effects).
Cluster-robust SE for BJS aggregations is planned for v0.12.

## See also

[`run_es()`](https://yo5uke.com/fixes/reference/run_es.md) for
event-study (dynamic) estimates.
