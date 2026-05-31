# Autoplot for event-study results

S3 method that plots an `es_result` (from
[`run_es()`](https://yo5uke.com/fixes/reference/run_es.md)). It forwards
arguments to
[`plot_es()`](https://yo5uke.com/fixes/reference/plot_es.md).

## Usage

``` r
# S3 method for class 'es_result'
autoplot(object, ci_level = 0.95, type = "ribbon", ...)
```

## Arguments

- object:

  An `es_result` returned by
  [`run_es()`](https://yo5uke.com/fixes/reference/run_es.md).

- ci_level:

  Confidence level (numeric, e.g., 0.95). Passed to
  [`plot_es()`](https://yo5uke.com/fixes/reference/plot_es.md).

- type:

  Plot type: `"ribbon"` (default) or `"errorbar"`. Passed to
  [`plot_es()`](https://yo5uke.com/fixes/reference/plot_es.md).

- ...:

  Additional arguments forwarded to
  [`plot_es()`](https://yo5uke.com/fixes/reference/plot_es.md).

## Value

A `ggplot` object.

## Examples

``` r
# res <- run_es(...)
# ggplot2::autoplot(res, ci_level = 0.95, type = "ribbon")
```
