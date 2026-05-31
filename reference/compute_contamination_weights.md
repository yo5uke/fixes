# Contamination Weights for TWFE Event-Study Coefficients (Sun-Abraham 2021)

Estimates the contamination weights \\\omega^{\ell''}\_{e,\ell}\\ that
decompose each TWFE event-study coefficient \\\hat\mu\_{\ell''}\\ into a
linear combination of cohort-specific ATTs (CATTs):
\$\$\hat\mu\_{\ell''} = \sum\_{(e,\ell)} \omega^{\ell''}\_{e,\ell} \cdot
\widehat{CATT}\_{e,\ell}\$\$ (Sun and Abraham 2021, Equation 20).

The weights are obtained via the OVB auxiliary regression (Eq. 12): for
each cohort-period CATT cell \\(e, \ell)\\, regress the cohort-specific
indicator \\1\\E_i = e\\\cdot 1\\t-E_i = \ell\\\\ on all
cohort-aggregated relative-time indicators \\D^{\ell''}\_{i,t}\\ and
two-way fixed effects. The resulting regression coefficient on
\\D^{\ell''}\_{i,t}\\ is \\\omega^{\ell''}\_{e,\ell}\\.

## Usage

``` r
compute_contamination_weights(
  data,
  time,
  timing,
  unit,
  fe = NULL,
  baseline = -1L
)
```

## Arguments

- data:

  A data.frame with one row per unit-period (balanced panel).

- time:

  Unquoted name of the calendar time variable (numeric).

- timing:

  Unquoted name of the first-treatment-period variable; `NA` marks
  never-treated units.

- unit:

  Unquoted name of the unit identifier.

- fe:

  One-sided fixed-effects formula, e.g. `~ id + year`. When `NULL`
  (default), falls back to `~ <unit> + <time>` with a warning.

- baseline:

  Integer reference (baseline) period excluded from the TWFE
  specification (default `-1L`). Must match the `baseline` argument used
  in [`run_es()`](https://yo5uke.com/fixes/reference/run_es.md).

## Value

An object of class `c("sa_contamination_weights", "data.frame")` with
one row per `(catt_cohort, catt_period, twfe_period)` triple, and
columns:

- `catt_cohort`:

  Cohort \\e\\ (first treatment period).

- `catt_period`:

  Relative event time \\\ell\\ of the CATT.

- `twfe_period`:

  Relative event time \\\ell''\\ of the TWFE coefficient being
  decomposed.

- `weight`:

  Contamination weight \\\omega^{\ell''}\_{e,\ell}\\.

- `is_own`:

  Logical; `TRUE` when `catt_period == twfe_period`.

Attributes: `baseline`, `cohorts`, `cohort_sizes`, `incl_periods`.

## Interpretation

- **Own-period cell** (`catt_period == twfe_period`):
  \\\omega^{\ell}\_{{e,\ell}}\\ represents the weight the TWFE estimator
  places on \\CATT\_{e,\ell}\\. Under the SA IW estimator these equal
  the cohort-size weights \\n_e / \sum n\_{e'}\\.

- **Cross-period cell** (`catt_period != twfe_period`): Any non-zero
  weight indicates contamination: the TWFE coefficient
  \\\hat\mu\_{\ell''}\\ also picks up treatment effects from period
  \\\ell \ne \ell''\\.

- **Verification**: the OVB identity (property iii) holds exactly, so
  \\\hat\mu\_{\ell''} = \sum\_{(e,\ell)} \omega^{\ell''}\_{e,\ell} \cdot
  \widehat{CATT}\_{e,\ell}\\ up to floating-point precision.

## References

Sun, L. and Abraham, S. (2021). Estimating dynamic treatment effects in
event studies with heterogeneous treatment effects. *Journal of
Econometrics*, 225(2), 175–199.

## See also

[`plot_contamination_weights()`](https://yo5uke.com/fixes/reference/plot_contamination_weights.md),
[`run_es()`](https://yo5uke.com/fixes/reference/run_es.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Estimate contamination weights
cw <- compute_contamination_weights(
  data     = panel_data,
  time     = year,
  timing   = first_treat,
  unit     = id,
  fe       = ~ id + year,
  baseline = -1L
)
print(cw)
plot_contamination_weights(cw)
} # }
```
