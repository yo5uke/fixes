# Honest sensitivity analysis for parallel-trends violations

Robust inference and sensitivity analysis for event-study / DiD designs
following Rambachan and Roth (2023). Instead of assuming parallel trends
holds exactly, it asks how large a violation of parallel trends would
have to be before the causal conclusion changes, returning confidence
sets for a post-treatment effect under a sequence of restrictions on the
possible difference in trends.

Two restriction families are supported:

- `"relative_magnitude"` (\\\Delta^{RM}(\bar M)\\): post-treatment
  violations are at most \\\bar M\\ times the largest pre-treatment
  violation (Rambachan & Roth 2023, Section 2.4.1).

- `"smoothness"` (\\\Delta^{SD}(M)\\): the difference in trends deviates
  from linearity by at most \\M\\ per period (Section 2.4.3).

Inference uses the Andrews-Roth-Pakes (ARP) conditional
moment-inequality test (Section 3.2.1), which is uniformly valid and
recommended for general restriction sets.

## Usage

``` r
honest_sensitivity(
  object = NULL,
  type = c("relative_magnitude", "smoothness"),
  Mvec = NULL,
  l_vec = NULL,
  alpha = 0.05,
  gridPoints = 1000L,
  betahat = NULL,
  sigma = NULL,
  numPrePeriods = NULL,
  numPostPeriods = NULL
)
```

## Arguments

- object:

  An `es_result` from
  [`run_es()`](https://yo5uke.com/fixes/reference/run_es.md) (with
  `estimator = "twfe"`, i.e. classic or `method = "sunab"`, which carry
  the event-study coefficient covariance). Alternatively, supply
  `betahat` and `sigma` directly (see below) for any estimator.

- type:

  Restriction family: `"relative_magnitude"` (default) or
  `"smoothness"`.

- Mvec:

  Numeric vector of restriction parameters. For `"relative_magnitude"`
  these are \\\bar M\\ values (default `seq(0, 2, by = 0.5)`); for
  `"smoothness"` these are \\M\\ values (default a data-driven sequence
  from 0 to the largest pre-period SD).

- l_vec:

  Numeric weight vector over post-treatment periods defining the target
  \\\theta = l'\tau\_{post}\\. Defaults to the first post-treatment
  period.

- alpha:

  Significance level (default `0.05` for 95% confidence sets).

- gridPoints:

  Number of grid points for test inversion (default `1000`).

- betahat:

  Optional event-study coefficient vector (pre then post, excluding the
  reference period), ordered by relative time. Required when `object`
  does not carry an `es_vcov` attribute.

- sigma:

  Optional covariance matrix of `betahat`.

- numPrePeriods, numPostPeriods:

  Optional integer counts; inferred from `object` or from
  `betahat`/`l_vec` when omitted.

## Value

A `data.frame` of class `"honest_result"` with one row per restriction
value plus the original (parallel-trends) confidence interval, with
columns `M`, `lb`, `ub`, `method`, and `type`. The breakdown value
(largest restriction at which the robust CI still excludes 0) is stored
in `attr(., "breakdown")`.

## References

Rambachan, A. and Roth, J. (2023). A More Credible Approach to Parallel
Trends. *Review of Economic Studies*, 90(5), 2555-2591.

## See also

[`run_es()`](https://yo5uke.com/fixes/reference/run_es.md),
[`plot_honest()`](https://yo5uke.com/fixes/reference/plot_honest.md)
