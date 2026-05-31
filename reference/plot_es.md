# Plot event-study results with ribbons or error bars

Plot event-study results with ribbons or error bars

## Usage

``` r
plot_es(
  data,
  ci_level = 0.95,
  type = "ribbon",
  vline_val = 0,
  vline_color = "#000",
  hline_val = 0,
  hline_color = "#000",
  linewidth = 1,
  pointsize = 2,
  alpha = 0.2,
  barwidth = 0.2,
  color = "#B25D91FF",
  fill = "#B25D91FF",
  theme_style = "bw",
  show_simultaneous = FALSE
)
```

## Arguments

- data:

  An object of class `es_result` returned by
  [`run_es()`](https://yo5uke.com/fixes/reference/run_es.md).

- ci_level:

  Confidence level to display (e.g., 0.95).

- type:

  One of `"ribbon"` (default) or `"errorbar"`.

- vline_val, hline_val:

  Numeric locations for vertical/horizontal reference lines (default 0).

- vline_color, hline_color:

  Colors for reference lines.

- linewidth, pointsize, alpha, barwidth:

  Styling parameters for lines/points/bands/bars.

- color, fill:

  Optional, override line/point color and ribbon fill.

- theme_style:

  One of `"bw"`, `"minimal"`, or `"classic"` for ggplot theme.

- show_simultaneous:

  Logical; if `TRUE`, overlays the simultaneous bootstrap CI (lighter
  band, alpha 0.15) alongside the standard pointwise CI (alpha 0.3),
  with a legend distinguishing the two bands. Requires
  `bootstrap = TRUE` in the originating
  [`run_es()`](https://yo5uke.com/fixes/reference/run_es.md) call.
  Default `FALSE`.

## Value

A `ggplot` object.

## Examples

``` r
# Assuming `res <- run_es(...)`
# p <- plot_es(res, ci_level = 0.95, type = "ribbon")
# print(p)
```
