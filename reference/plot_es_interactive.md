# Interactive event-study plot with hover details

Creates an interactive plotly visualization of event study results with
hover-over displays showing coefficients, confidence intervals, and
other details.

## Usage

``` r
plot_es_interactive(
  data,
  ci_level = 0.95,
  vline_val = 0,
  hline_val = 0,
  vline_color = "#000",
  hline_color = "#000",
  color = "#B25D91FF",
  fill = "#B25D91FF",
  alpha = 0.2,
  linewidth = 2,
  markersize = 8,
  show_ribbon = TRUE,
  show_simultaneous = FALSE,
  height = NULL,
  width = NULL
)
```

## Arguments

- data:

  An object of class `es_result` returned by
  [`run_es()`](https://yo5uke.com/fixes/reference/run_es.md).

- ci_level:

  Confidence level to display (e.g., 0.95). Default is 0.95.

- vline_val:

  Numeric location for vertical reference line (default 0).

- hline_val:

  Numeric location for horizontal reference line (default 0).

- vline_color:

  Color for vertical reference line (default "#000").

- hline_color:

  Color for horizontal reference line (default "#000").

- color:

  Point and line color (default "#B25D91FF").

- fill:

  Ribbon/band fill color (default "#B25D91FF").

- alpha:

  Ribbon transparency (default 0.2).

- linewidth:

  Line width (default 2).

- markersize:

  Marker size (default 8).

- show_ribbon:

  Logical; if TRUE, shows confidence interval as a ribbon band (default
  TRUE).

- show_simultaneous:

  Logical; if `TRUE`, overlays a second (lighter) ribbon for the
  simultaneous bootstrap CI and extends the hover tooltip with
  simultaneous CI bounds. Requires `bootstrap = TRUE` in the originating
  [`run_es()`](https://yo5uke.com/fixes/reference/run_es.md) call.
  Default `FALSE`.

- height:

  Plot height in pixels (default NULL for auto).

- width:

  Plot width in pixels (default NULL for auto).

## Value

A `plotly` object that can be displayed interactively.

## Details

The hover tooltip displays:

- Relative time to treatment

- Point estimate (coefficient)

- Confidence interval bounds

- Standard error

- P-value

- Simultaneous CI bounds (when `show_simultaneous = TRUE`)

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming res <- run_es(...)
plot_es_interactive(res)
plot_es_interactive(res, ci_level = 0.99, show_ribbon = FALSE)
plot_es_interactive(res, show_simultaneous = TRUE)
} # }
```
