# Plot a honest sensitivity analysis

Visualises the output of
[`honest_sensitivity()`](https://yo5uke.com/fixes/reference/honest_sensitivity.md):
robust confidence intervals for the target effect under progressively
weaker parallel-trends restrictions (increasing \\M\\ or \\\bar M\\),
alongside the original confidence interval that assumes parallel trends
holds exactly. This is the "top-down" sensitivity plot of Rambachan and
Roth (2023).

## Usage

``` r
plot_honest(x, ...)

# S3 method for class 'honest_result'
autoplot(object, ...)
```

## Arguments

- x:

  A `honest_result` object from
  [`honest_sensitivity()`](https://yo5uke.com/fixes/reference/honest_sensitivity.md).

- ...:

  Unused.

- object:

  A `honest_result` object.

## Value

A `ggplot` object.

## See also

[`honest_sensitivity()`](https://yo5uke.com/fixes/reference/honest_sensitivity.md)
