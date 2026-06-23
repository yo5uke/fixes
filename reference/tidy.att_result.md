# Tidy an `att_result` object

Returns a tidy data frame of the aggregated ATT estimates from a
[`calc_att()`](https://yo5uke.com/fixes/reference/calc_att.md) result,
in the column format expected by
[`modelsummary::modelsummary()`](https://modelsummary.com/man/modelsummary.html)
(and therefore renderable with
[`tinytable::tt()`](https://vincentarelbundock.github.io/tinytable/man/tt.html)).
This makes it easy to put overall, by-cohort, or by-time ATTs into a
publication table, and to stack several estimators (e.g.
[`run_did()`](https://yo5uke.com/fixes/reference/run_did.md) TWFE, CS,
and BJS) side by side in one table.

The `term` column is derived from the aggregation type so that rows
align across models:

- `aggregation = "simple"` -\> `"ATT"`

- `aggregation = "by_cohort"` -\> `"Cohort <g>"`

- `aggregation = "by_time"` -\> `"Time <t>"`

## Usage

``` r
# S3 method for class 'att_result'
tidy(x, conf.int = FALSE, conf.level = 0.95, ...)
```

## Arguments

- x:

  An `att_result` object returned by
  [`calc_att()`](https://yo5uke.com/fixes/reference/calc_att.md).

- conf.int:

  Logical; add `conf.low`/`conf.high` columns? Default `FALSE`.

- conf.level:

  Confidence level for `conf.int` (normal approximation). Default
  `0.95`.

- ...:

  Unused; for S3 generic compatibility.

## Value

A data frame with columns `term`, `estimate`, `std.error`, `statistic`,
`p.value` (and optionally `conf.low`, `conf.high`).

## Examples

``` r
if (FALSE) { # \dontrun{
att <- calc_att(df, outcome = y, time = year, timing = g, unit = id,
                estimator = "cs", aggregation = "by_cohort")
broom::tidy(att)

# Compare estimators in one table:
twfe <- run_did(df, outcome = y, treatment = D, fe = ~ id + year)
cs   <- calc_att(df, outcome = y, time = year, timing = g, unit = id,
                 estimator = "cs")
bjs  <- calc_att(df, outcome = y, time = year, timing = g, unit = id,
                 estimator = "bjs")
modelsummary::modelsummary(list(TWFE = twfe, CS = cs, BJS = bjs))
} # }
```
