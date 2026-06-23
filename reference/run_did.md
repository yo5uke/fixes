# Run a basic two-way fixed-effects DiD model

Estimates a classic difference-in-differences model of the form
`outcome ~ D_it | fe` using
[`fixest::feols()`](https://lrberge.github.io/fixest/reference/feols.html).

There are two ways to supply the treatment indicator:

**Option A — pre-built `D_it`** (maximum flexibility):

    df$D <- as.integer(df$treated & df$year >= 2006)
    run_did(df, outcome = y, treatment = D, fe = ~ id + year)

**Option B — timing-based construction** (convenience; consistent with
[`run_es()`](https://yo5uke.com/fixes/reference/run_es.md) and
[`calc_att()`](https://yo5uke.com/fixes/reference/calc_att.md)):

    run_did(df, outcome = y, treatment = treated, time = year, timing = 2006,
            fe = ~ id + year)

Here `treatment` is a binary group indicator (1 = treated unit, 0 =
control), `time` is the calendar-time variable, and `timing` is the
scalar treatment onset period. Internally
`D_it = treatment * (time >= timing)` is constructed automatically. For
staggered-adoption settings use
[`calc_att()`](https://yo5uke.com/fixes/reference/calc_att.md); for
dynamic event-study estimates use
[`run_es()`](https://yo5uke.com/fixes/reference/run_es.md).

## Usage

``` r
run_did(
  data,
  outcome,
  treatment,
  timing = NULL,
  fe = NULL,
  unit = NULL,
  time = NULL,
  covariates = NULL,
  cluster = NULL,
  weights = NULL,
  conf.level = 0.95,
  vcov = "HC1",
  vcov_args = list()
)
```

## Arguments

- data:

  A data.frame (panel format).

- outcome:

  Unquoted outcome variable or expression (e.g., `log(y)`).

- treatment:

  Unquoted column name. When `timing = NULL` (default): a pre-built
  binary `D_it` indicator (1 = treated unit-time, 0 = otherwise). When
  `timing` is provided: a binary group indicator (1 = treated unit, 0 =
  control unit; constant within units).

- timing:

  Numeric scalar. When provided, `D_it` is constructed as
  `treatment * (time >= timing)`. Requires `time` to be specified.
  Default `NULL` (user supplies pre-built `D_it` via `treatment`).

- fe:

  One-sided formula specifying fixed effects, e.g. `~ id + year`. If
  `NULL` and both `unit` and `time` are supplied, `fe` is auto-inferred
  as `~ unit + time`. If `NULL` and neither is supplied, a pooled OLS
  model is estimated (with a message).

- unit:

  Unquoted unit identifier column (for metadata and `fe`
  auto-inference).

- time:

  Unquoted time variable column. Used for (a) `fe` auto-inference
  and (b) `D_it` construction when `timing` is provided.

- covariates:

  One-sided formula of additional controls, e.g. `~ x1 + x2`.

- cluster:

  Clustering specification: a one-sided formula (`~ id`), a single
  character column name, or a numeric vector of length `nrow(data)`.
  When `cluster` is specified and `vcov` is the default `"HC1"`,
  cluster-robust standard errors are used automatically.

- weights:

  Observation weights (formula or numeric vector).

- conf.level:

  Confidence level(s) for CIs. Scalar or vector (e.g., `c(0.90, 0.95)`).
  Default `0.95`.

- vcov:

  VCOV type string passed to `fixest::vcov()`. Default `"HC1"`. Ignored
  in favour of cluster-robust SE when `cluster` is supplied and `vcov`
  is left at its default `"HC1"`.

- vcov_args:

  Named list of additional arguments forwarded to `fixest::vcov()`.

## Value

A `did_result` object (named list) with elements:

- `estimates`:

  Data frame with the treatment coefficient: `term`, `estimate`,
  `std.error`, `statistic`, `p.value`, and `conf_low_XX`/`conf_high_XX`
  for each `conf.level` entry.

- `model`:

  The underlying `fixest` model object.

Attributes: `call`, `formula_str`, `outcome`, `treatment`, `timing`,
`fe`, `vcov_type`, `cluster_vars`, `conf.level`, `N`, `N_units`,
`N_treated`, `unit`, `time`. `N`, `N_units`, and `N_treated` describe
the *estimation* sample (after
[`fixest::feols()`](https://lrberge.github.io/fixest/reference/feols.html)
drops rows with missing values), matching `nobs(model)` and
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html),
not `nrow(data)`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Option A: pre-built D_it
df$D <- as.integer(df$treated & df$year >= 2006)
res <- run_did(df, outcome = y, treatment = D, fe = ~ id + year)

# Option B: timing-based construction
res <- run_did(df, outcome = y, treatment = treated, time = year,
               timing = 2006, fe = ~ id + year)

# Cluster-robust SE
res <- run_did(df, outcome = y, treatment = D, fe = ~ id + year,
               cluster = ~ id)

print(res)
broom::tidy(res)
broom::glance(res)
# modelsummary::modelsummary(res)
} # }
```
