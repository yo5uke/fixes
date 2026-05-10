## R CMD check results
0 errors | 0 warnings | 0 notes

## Notes
None.

## New in this submission (0.8.1)

### Estimators
- `run_es(estimator = "cs")`: Callaway & Sant'Anna (2021) group-time ATT
  estimator with never-treated or not-yet-treated control groups.
- `run_es(estimator = "sa")`: Sun & Abraham (2021) interaction-weighted
  estimator. Numerically identical to `fixest::sunab()`.
- `run_es(estimator = "bjs")`: Borusyak, Jaravel & Spiess (2024) imputation
  estimator.

### Bootstrap simultaneous confidence bands
- `run_es(bootstrap = TRUE)`: Multiplier bootstrap (Algorithm 1, Callaway &
  Sant'Anna 2021) for simultaneous CIs over the full event-study curve.
- `plot_es(show_simultaneous = TRUE)`: Overlays simultaneous and pointwise
  CI bands with a two-entry legend.
- `plot_es_interactive(show_simultaneous = TRUE)`: Adds simultaneous CI ribbon
  to the interactive plotly plot.

### Visualization
- `plot_att_gt()`: New function to visualise the full ATT(g,t) matrix as a
  heatmap or cohort-faceted time series. Bootstrap-aware: adds simultaneous
  critical value to the subtitle and marks simultaneously significant cells
  with open-diamond markers (heatmap) or a lighter ribbon (facet).

### Bug fixes
- `run_es()` bootstrap path: `base::merge()` was silently dropping all custom
  attributes from the result. Fixed by saving and restoring attributes around
  the merge.

## Test environments
- macOS (GitHub Actions, R release)
- Windows (GitHub Actions, R release)
- Ubuntu (GitHub Actions, R devel, release, oldrel-1)
- Local: Windows 11 / WSL2, R 4.5.2

## Downstream dependencies
None (new package).
