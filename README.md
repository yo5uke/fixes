
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fixes <a><img src="man/figures/fixes_logo_blue.png" align="right" height="138" width="120" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/fixes)](https://CRAN.R-project.org/package=fixes)
[![R-CMD-check](https://github.com/yo5uke/fixes/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yo5uke/fixes/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

> **Current version:** 0.6.0 (development)

> **Note**  
> By default, the `fixes` package assumes time is a regularly spaced
> numeric variable (e.g., year = 1995, 1996, …).  
> If your time variable is irregular or non-numeric (e.g., `Date` type),
> set `time_transform = TRUE` to automatically convert it to a
> sequential index within each unit.  
> For unit-specific treatment timing, set `staggered = TRUE`.

The `fixes` package is designed for convenient event study analysis and
plotting, particularly useful for visualizing parallel trends and
dynamic effects in two-way fixed effects (TWFE)
difference-in-differences (DID) research.

**Key Functions:**

1.  `run_es()` — Takes a data frame, generates lead/lag dummies, and
    fits the event study regression. Supports fixed effects, covariates,
    clustering, staggered timing, weights, custom baseline, and multiple
    confidence intervals.
2.  `plot_es()` — Plots event study results using `ggplot2` with
    flexible options: ribbon or error bars, choice of CI level, and
    theme customization.

## Installation

Install from CRAN:

``` r
install.packages("fixes")
```

Or with **pak**:

``` r
pak::pak("fixes")
```

For the latest development version from GitHub:

``` r
pak::pak("yo5uke/fixes")
```

## How to use

First, load the library.

``` r
library(fixes)
```

### Data frame requirements

`run_es()` expects a panel data frame with at least:

- Unit identifier (e.g., individual, firm, region)
- Treatment indicator (0/1 or TRUE/FALSE)
- Time variable (numeric or `Date`)
- Outcome variable (continuous)

For **staggered adoption** (`staggered = TRUE`), include a variable
specifying unit-specific treatment timing (e.g., “treatment_year”).

#### Example data

Widely used panel datasets include:

- `did::sim_dt()`: simulated panel for DiD tutorials
- `fixest::base_stagg`: a built-in dataset for staggered adoption

``` r
df1 <- fixest::base_did      # Basic DiD
df2 <- fixest::base_stagg    # Staggered treatment
```

|          y |         x1 |  id | period | post | treat |
|-----------:|-----------:|----:|-------:|-----:|------:|
|  2.8753063 |  0.5365377 |   1 |      1 |    0 |     1 |
|  1.8606527 | -3.0431894 |   1 |      2 |    0 |     1 |
|  0.0941652 |  5.5768439 |   1 |      3 |    0 |     1 |
|  3.7814749 | -2.8300587 |   1 |      4 |    0 |     1 |
| -2.5581996 | -5.0443544 |   1 |      5 |    0 |     1 |
|  1.7287324 | -0.6363849 |   1 |      6 |    1 |     1 |

|  | id | year | year_treated | time_to_treatment | treated | treatment_effect_true | x1 | y |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|
| 2 | 90 | 1 | 2 | -1 | 1 | 0 | -1.0947021 | 0.0172297 |
| 3 | 89 | 1 | 3 | -2 | 1 | 0 | -3.7100676 | -4.5808453 |
| 4 | 88 | 1 | 4 | -3 | 1 | 0 | 2.5274402 | 2.7381717 |
| 5 | 87 | 1 | 5 | -4 | 1 | 0 | -0.7204263 | -0.6510307 |
| 6 | 86 | 1 | 6 | -5 | 1 | 0 | -3.6711678 | -5.3338166 |
| 7 | 85 | 1 | 7 | -6 | 1 | 0 | -0.3152137 | 0.4956263 |

### `run_es()`

The main event study function. All key arguments below:

| Argument | Description |
|----|----|
| `data` | Data frame to be used. |
| `outcome` | Outcome variable. Can be specified as a raw variable or a transformation (e.g., `log(y)`). Provide it unquoted. |
| `treatment` | Dummy variable indicating the treated units. Provide it unquoted. Accepts both `0/1` and `TRUE/FALSE`. |
| `time` | Time variable. Provide it unquoted. |
| `timing` | The time at which the treatment occurs. If `staggered = FALSE`, this should be a scalar (e.g., `2005`). If `staggered = TRUE`, provide a variable (column) indicating the treatment time for each unit. |
| `fe` | Fixed effects to control for unobserved heterogeneity. **Must be a one-sided formula** (e.g., `~ id + year`). |
| `lead_range` | Number of pre-treatment periods to include (e.g., 3 = `lead3`, `lead2`, `lead1`). Default is `NULL`, which automatically uses the maximum available lead range. |
| `lag_range` | Number of post-treatment periods to include (e.g., 2 = `lag0` (the treatment period), `lag1`, `lag2`). Default is `NULL`, which automatically uses the maximum available lag range. |
| `covariates` | Additional covariates to include in the regression. **Must be a one-sided formula** (e.g., `~ x1 + x2`). |
| `cluster` | Specifies clustering for standard errors. Can be a **character vector** (e.g., `c("id", "year")`) or a **formula** (e.g., `~ id + year`, `~ id^year`). |
| `weights` | Optional weights to be used in the regression. Provide as a one-sided formula (e.g., `~ weight`). |
| `baseline` | Relative time value to be used as the reference category. The corresponding dummy is excluded from the regression. **Must be within the specified lead/lag range.** |
| `interval` | Time interval between observations (e.g., `1` for yearly data, `5` for 5-year intervals). |
| `time_transform` | Logical. If `TRUE`, converts the `time` variable into a sequential index (1, 2, 3, …) within each unit. Useful for irregular time (e.g., Date). Default is `FALSE`. |
| `unit` | Required if `time_transform = TRUE`. Specifies the panel unit identifier (e.g., `firm_id`). |
| `staggered` | Logical. If `TRUE`, allows for unit-specific treatment timing (staggered adoption). Default is `FALSE`. |
| `conf.level` | Numeric vector of confidence levels (e.g., `c(0.90, 0.95, 0.99)`; default: `0.95`). |

#### Example: basic event study

``` r
event_study <- run_es(
  data       = df1,
  outcome    = y,
  treatment  = treat,
  time       = period,
  timing     = 6,
  fe         = ~ id + period,
  lead_range = 5,
  lag_range  = 4,
  cluster    = ~ id,
  baseline   = -1,
  interval   = 1,
  conf.level = c(0.90, 0.95, 0.99)
)
```

- `fe` must be a one-sided formula (e.g., `~ firm_id + year`).
- `cluster` can be a one-sided formula or a character vector.

#### With covariates

``` r
event_study <- run_es(
  data       = df1,
  outcome    = y,
  treatment  = treat,
  time       = period,
  timing     = 6,
  fe         = ~ id + period,
  lead_range = 5,
  lag_range  = 4,
  covariates = ~ cov1 + cov2 + cov3,
  cluster    = ~ id,
  baseline   = -1,
  interval   = 1
)
```

#### Using irregular time data (`Date`), with `time_transform`

``` r
df_alt <- df1 |>
  dplyr::mutate(
    year = rep(2001:2010, times = 108),
    date = as.Date(paste0(year, "-01-01"))
  )

event_study_alt <- run_es(
  data           = df_alt,
  outcome        = y,
  treatment      = treat,
  time           = date,
  timing         = 9,  # Use index, not the original Date
  fe             = ~ id + period,
  lead_range     = 3,
  lag_range      = 3,
  cluster        = ~ id,
  baseline       = -1,
  time_transform = TRUE,
  unit           = id
)
```

> **Note:**  
> When `time_transform = TRUE`, specify `timing` as an index (e.g., 9 =
> 9th observation in unit).  
> Currently, `time_transform = TRUE` *cannot* be combined with
> `staggered = TRUE` (future versions may support this).

### `plot_es()`

`plot_es()` visualizes results using `ggplot2`. By default, it plots a
ribbon for the 95% CI, but supports error bars, CI level selection, and
multiple themes.

| Argument    | Description                                 |
|-------------|---------------------------------------------|
| data        | Data frame from `run_es()`                  |
| ci_level    | Confidence interval (default: 0.95)         |
| type        | “ribbon” (default) or “errorbar”            |
| vline_val   | X for vertical line (default: 0)            |
| vline_color | Color for vline (default: “\#000”)          |
| hline_val   | Y for horizontal line (default: 0)          |
| hline_color | Color for hline (default: “\#000”)          |
| linewidth   | Line width (default: 1)                     |
| pointsize   | Point size (default: 2)                     |
| alpha       | Ribbon transparency (default: 0.2)          |
| barwidth    | Errorbar width (default: 0.2)               |
| color       | Point/line color (default: “\#B25D91FF”)    |
| fill        | Ribbon color (default: “\#B25D91FF”)        |
| theme_style | Theme: “bw” (default), “minimal”, “classic” |

#### Example usage

``` r
plot_es(event_study)
plot_es(event_study, type = "errorbar")
plot_es(event_study, type = "ribbon", ci_level = 0.9, theme_style = "minimal")
plot_es(event_study, type = "errorbar", ci_level = 0.99) + ggplot2::ggtitle("Event Study, 99% CI")
```

Further customization with `ggplot2` is fully supported:

``` r
plot_es(event_study, type = "errorbar") + 
  ggplot2::scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
  ggplot2::ggtitle("Result of Event Study")
```

## Planned Features

- Support for `staggered = TRUE` with `time_transform = TRUE`
- Allow `timing` to accept original time values (e.g., `Date`), not just
  index

## Debugging and Contributions

If you find an issue or want to contribute, please use the [GitHub
Issues page](https://github.com/yo5uke/fixes/issues).

------------------------------------------------------------------------

Happy analyzing!🥂
