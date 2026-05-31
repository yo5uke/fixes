# Tidy a `did_result` object

Returns a tidy data frame of model coefficients from a
[`run_did()`](https://yo5uke.com/fixes/reference/run_did.md) result.
Delegates to
[`broom::tidy.fixest()`](https://broom.tidymodels.org/reference/tidy.fixest.html)
on the underlying `fixest` model so that all regressors (treatment and
covariates) appear in the output — the format expected by
[`modelsummary::modelsummary()`](https://modelsummary.com/man/modelsummary.html).

## Usage

``` r
# S3 method for class 'did_result'
tidy(x, conf.int = FALSE, conf.level = 0.95, ...)
```

## Arguments

- x:

  A `did_result` object returned by
  [`run_did()`](https://yo5uke.com/fixes/reference/run_did.md).

- conf.int:

  Logical; add `conf.low`/`conf.high` columns? Default `FALSE`.

- conf.level:

  Confidence level for `conf.int`. Default `0.95`.

- ...:

  Additional arguments passed to
  [`broom::tidy.fixest()`](https://broom.tidymodels.org/reference/tidy.fixest.html).

## Value

A data frame with columns `term`, `estimate`, `std.error`, `statistic`,
`p.value` (and optionally `conf.low`, `conf.high`).

## Examples

``` r
if (FALSE) { # \dontrun{
res <- run_did(df, outcome = y, treatment = D, fe = ~ id + year)
broom::tidy(res)
broom::tidy(res, conf.int = TRUE)
} # }
```
