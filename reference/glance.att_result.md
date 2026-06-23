# Glance at an `att_result` object

Returns a one-row summary of an
[`calc_att()`](https://yo5uke.com/fixes/reference/calc_att.md) result
for the goodness-of-fit block of a
[`modelsummary::modelsummary()`](https://modelsummary.com/man/modelsummary.html)
table.

## Usage

``` r
glance.att_result(x, ...)
```

## Arguments

- x:

  An `att_result` object returned by
  [`calc_att()`](https://yo5uke.com/fixes/reference/calc_att.md).

- ...:

  Unused; for S3 generic compatibility.

## Value

A one-row data frame with `nobs`, `n.units`, `n.treated`, `estimator`,
and `aggregation`.

## Examples

``` r
if (FALSE) { # \dontrun{
att <- calc_att(df, outcome = y, time = year, timing = g, unit = id)
broom::glance(att)
} # }
```
