# Plot the ATT(g,t) matrix from a Callaway-Sant'Anna event study

Visualises the full cohort-by-period ATT(g,t) matrix stored in the
`att_gt` attribute of an `es_result` object produced by
`run_es(estimator = "cs")`. Two display styles are available:

- `"heatmap"`: a tile plot with calendar time \\t\\ on the x-axis and
  cohort \\g\\ on the y-axis, colour-filled by the point estimate. Cells
  whose pointwise confidence interval excludes zero are marked with a
  filled dot; cells that are simultaneously significant (when bootstrap
  data are available) are additionally marked with an open diamond.

- `"facet"`: one panel per cohort showing ATT(g,t) over calendar time
  \\t\\ with a pointwise confidence ribbon, mirroring the style of
  [`plot_es()`](https://yo5uke.com/fixes/reference/plot_es.md). A
  lighter simultaneous CI ribbon is overlaid when bootstrap data are
  available.

Both types draw a vertical dashed line at \\t = g\\ (treatment onset)
for each cohort.

## Usage

``` r
plot_att_gt(
  x,
  type = c("heatmap", "facet"),
  ci_level = 0.95,
  zero_line = TRUE,
  theme = c("bw", "minimal", "classic"),
  color = "#B25D91FF",
  fill = "#B25D91FF",
  alpha = 0.2
)

# S3 method for class 'att_gt_result'
autoplot(object, ...)
```

## Arguments

- x:

  An `es_result` object returned by `run_es(estimator = "cs")`, or an
  `att_gt_result` data frame produced by extracting `attr(x, "att_gt")`
  and giving it class `"att_gt_result"`.

- type:

  `"heatmap"` (default) or `"facet"`.

- ci_level:

  Confidence level for pointwise intervals (default 0.95).

- zero_line:

  Logical; draw a horizontal reference line at zero in the `"facet"`
  display (default `TRUE`).

- theme:

  One of `"bw"` (default), `"minimal"`, or `"classic"`.

- color:

  Line and point colour used in the `"facet"` display (default
  `"#B25D91FF"`, matching
  [`plot_es()`](https://yo5uke.com/fixes/reference/plot_es.md)).

- fill:

  Ribbon fill colour in the `"facet"` display (default `"#B25D91FF"`).

- alpha:

  Ribbon transparency in the `"facet"` display (default `0.2`).

- object:

  An `att_gt_result` object (extracted from an `es_result` via
  `attr(result, "att_gt")`).

- ...:

  Passed to `plot_att_gt`.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Bootstrap annotation

When `attr(x, "bootstrap")` is present (i.e.,
[`run_es()`](https://yo5uke.com/fixes/reference/run_es.md) was called
with `bootstrap = TRUE`), both plot types add simultaneous inference
overlays sourced from the (g,t)-level bootstrap object.

## See also

[`plot_es()`](https://yo5uke.com/fixes/reference/plot_es.md),
[`run_es()`](https://yo5uke.com/fixes/reference/run_es.md)

## Examples

``` r
if (FALSE) { # \dontrun{
cs_result <- run_es(data = mydata, outcome = y, time = year,
                    timing = g, unit = id, fe = ~id + year,
                    staggered = TRUE, estimator = "cs")
plot_att_gt(cs_result)
plot_att_gt(cs_result, type = "facet")
} # }
```
