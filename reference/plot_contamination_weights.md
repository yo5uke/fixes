# Plot Contamination Weights as a Tile Heatmap

Creates a ggplot2 tile heatmap of the contamination weights returned by
[`compute_contamination_weights()`](https://yo5uke.com/fixes/reference/compute_contamination_weights.md).

Each cell at position (`twfe_period`, `catt_label`) shows the weight
\\\omega^{\ell''}\_{e,\ell}\\: how much of \\CATT\_{e,\ell}\\ leaks into
the TWFE coefficient \\\hat\mu\_{\ell''}\\.

- **Diagonal cells** (`catt_period == twfe_period`): own-period weights
  (sum across cohorts should be \\\approx 1\\).

- **Off-diagonal cells**: cross-period contamination (ideally close to
  zero under treatment effect homogeneity).

## Usage

``` r
plot_contamination_weights(
  x,
  limit_abs = NULL,
  midpoint = 0,
  low = "#2166AC",
  mid = "white",
  high = "#B2182B",
  theme = c("bw", "minimal", "classic"),
  show_values = FALSE,
  value_digits = 2L
)
```

## Arguments

- x:

  An `sa_contamination_weights` object from
  [`compute_contamination_weights()`](https://yo5uke.com/fixes/reference/compute_contamination_weights.md).

- limit_abs:

  Numeric; symmetric colour scale limit `[-limit, limit]`. Defaults to
  the maximum absolute weight (rounded up to one decimal).

- midpoint:

  Numeric; midpoint of the diverging colour scale (default 0).

- low:

  Colour for negative weights (default `"#2166AC"`).

- mid:

  Colour for zero weight (default `"white"`).

- high:

  Colour for positive weights (default `"#B2182B"`).

- theme:

  Character; `"bw"` (default), `"minimal"`, or `"classic"`.

- show_values:

  Logical; overlay weight values in each tile (default `FALSE`).

- value_digits:

  Integer; decimal digits when `show_values = TRUE` (default `2L`).

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## See also

[`compute_contamination_weights()`](https://yo5uke.com/fixes/reference/compute_contamination_weights.md)
