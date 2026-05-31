# Package index

## Estimation

Estimate event-study curves, aggregated ATT, and basic two-way FE DiD.

- [`run_es()`](https://yo5uke.com/fixes/reference/run_es.md) : Event
  Study Estimation for Panel Data
- [`calc_att()`](https://yo5uke.com/fixes/reference/calc_att.md) :
  Compute ATT Aggregations for Staggered Adoption Designs
- [`run_did()`](https://yo5uke.com/fixes/reference/run_did.md) : Run a
  basic two-way fixed-effects DiD model

## Sensitivity analysis

Robust inference under violations of parallel trends.

- [`honest_sensitivity()`](https://yo5uke.com/fixes/reference/honest_sensitivity.md)
  : Honest sensitivity analysis for parallel-trends violations

## Visualization

Plot event studies, ATT(g,t) matrices, and sensitivity results.

- [`plot_es()`](https://yo5uke.com/fixes/reference/plot_es.md) : Plot
  event-study results with ribbons or error bars
- [`plot_es_interactive()`](https://yo5uke.com/fixes/reference/plot_es_interactive.md)
  : Interactive event-study plot with hover details
- [`plot_att_gt()`](https://yo5uke.com/fixes/reference/plot_att_gt.md)
  [`autoplot(`*`<att_gt_result>`*`)`](https://yo5uke.com/fixes/reference/plot_att_gt.md)
  : Plot the ATT(g,t) matrix from a Callaway-Sant'Anna event study
- [`plot_honest()`](https://yo5uke.com/fixes/reference/plot_honest.md)
  [`autoplot(`*`<honest_result>`*`)`](https://yo5uke.com/fixes/reference/plot_honest.md)
  : Plot a honest sensitivity analysis
- [`plot_contamination_weights()`](https://yo5uke.com/fixes/reference/plot_contamination_weights.md)
  : Plot Contamination Weights as a Tile Heatmap

## Diagnostics

Contamination-weight diagnostics for the Sun-Abraham estimator.

- [`compute_contamination_weights()`](https://yo5uke.com/fixes/reference/compute_contamination_weights.md)
  : Contamination Weights for TWFE Event-Study Coefficients (Sun-Abraham
  2021)

## S3 methods

broom and ggplot2 integration for result objects.

- [`autoplot(`*`<es_result>`*`)`](https://yo5uke.com/fixes/reference/autoplot.es_result.md)
  : Autoplot for event-study results

- [`tidy(`*`<did_result>`*`)`](https://yo5uke.com/fixes/reference/tidy.did_result.md)
  :

  Tidy a `did_result` object

- [`glance.did_result()`](https://yo5uke.com/fixes/reference/glance.did_result.md)
  :

  Glance at a `did_result` object
