# Glance at a `did_result` object

Returns a single-row summary of model-level statistics from a
[`run_did()`](https://yo5uke.com/fixes/reference/run_did.md) result.
Delegates to
[`broom::glance.fixest()`](https://broom.tidymodels.org/reference/glance.fixest.html)
which provides `nobs`, `r.squared`, `adj.r.squared`, `within.r.squared`,
`AIC`, `BIC`, and related statistics.

## Usage

``` r
glance.did_result(x, ...)
```

## Arguments

- x:

  A `did_result` object returned by
  [`run_did()`](https://yo5uke.com/fixes/reference/run_did.md).

- ...:

  Additional arguments passed to
  [`broom::glance.fixest()`](https://broom.tidymodels.org/reference/glance.fixest.html).

## Value

A one-row data frame of model-level statistics.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- run_did(df, outcome = y, treatment = D, fe = ~ id + year)
broom::glance(res)
} # }
```
