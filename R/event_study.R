# --------------------------------------------------------------------------- #
#  Internal helpers (not exported)                                             #
# --------------------------------------------------------------------------- #

# Convert a .run_*() output's `es` data.frame to the es_result tidy format.
# `es` must have: relative_time, estimate, std_error,
#  conf_low_XX / conf_high_XX for each level in conf.level.
.make_tidy <- function(es, conf.level) {
  statistic <- ifelse(es$std_error > 0, es$estimate / es$std_error, NA_real_)
  tidy <- data.frame(
    term = as.character(es$relative_time),
    estimate = es$estimate,
    std.error = es$std_error,
    statistic = statistic,
    p.value = ifelse(
      !is.na(statistic),
      2 * stats::pnorm(-abs(statistic)),
      NA_real_
    ),
    relative_time = as.integer(es$relative_time),
    is_baseline = FALSE,
    stringsAsFactors = FALSE
  )
  for (cl in conf.level) {
    suf <- sprintf("%.0f", cl * 100)
    tidy[[paste0("conf_low_", suf)]] <- es[[paste0("conf_low_", suf)]]
    tidy[[paste0("conf_high_", suf)]] <- es[[paste0("conf_high_", suf)]]
  }
  tidy
}

# Append a zero baseline row, sort by relative_time, enforce the
# [−lead_range, lag_range] window, and stamp all attributes + class.
# meta: named list — every element is set as an attribute on the result.
# bjs_lead: use BJS-specific lead_range formula (−min of all rows, not just
#   non-baseline rows) so the baseline row survives the window filter when
#   BJS produces no pre-treatment estimates.
.es_finalize <- function(
  tidy,
  baseline,
  conf.level,
  lead_range,
  lag_range,
  meta,
  bjs_lead = FALSE
) {
  baseline <- as.integer(baseline)
  if (!(baseline %in% tidy$relative_time)) {
    bl_row <- data.frame(
      term = as.character(baseline),
      estimate = 0,
      std.error = 0,
      statistic = NA_real_,
      p.value = NA_real_,
      relative_time = baseline,
      is_baseline = TRUE,
      stringsAsFactors = FALSE
    )
    for (cl in conf.level) {
      suf <- sprintf("%.0f", cl * 100)
      bl_row[[paste0("conf_low_", suf)]] <- 0
      bl_row[[paste0("conf_high_", suf)]] <- 0
    }
    tidy <- rbind(tidy, bl_row)
    tidy$is_baseline <- tidy$relative_time == baseline
  } else {
    tidy$is_baseline <- FALSE
  }
  tidy <- tidy[order(tidy$relative_time), ]
  rownames(tidy) <- NULL

  if (bjs_lead) {
    if (is.null(lead_range)) {
      lead_range <- max(0L, -min(tidy$relative_time, na.rm = TRUE))
    }
  } else {
    non_base <- tidy$relative_time[!tidy$is_baseline]
    if (is.null(lead_range)) {
      lead_range <- max(0L, abs(min(non_base)))
    }
    if (is.null(lag_range)) lag_range <- max(0L, max(non_base))
  }
  if (is.null(lag_range)) {
    non_base <- tidy$relative_time[!tidy$is_baseline]
    lag_range <- max(0L, max(non_base))
  }

  tidy <- tidy[
    !is.na(tidy$relative_time) &
      tidy$relative_time >= -lead_range &
      tidy$relative_time <= lag_range,
  ]

  attr(tidy, "lead_range") <- lead_range
  attr(tidy, "lag_range") <- lag_range
  attr(tidy, "baseline") <- baseline
  attr(tidy, "conf.level") <- conf.level
  for (nm in names(meta)) {
    attr(tidy, nm) <- meta[[nm]]
  }

  class(tidy) <- c("es_result", "data.frame")
  tidy
}

.trim_es_vcov <- function(V, tidy) {
  if (is.null(V)) return(NULL)
  nb <- tidy[!tidy$is_baseline, , drop = FALSE]
  if (nrow(nb) == 0L) return(NULL)
  nm <- as.character(nb$relative_time)
  if (!all(nm %in% rownames(V))) return(NULL)
  V[nm, nm, drop = FALSE]
}

# Calendar period that relative time 0 refers to, or NULL when the design has
# no single one (staggered adoption with several cohorts, or a pre-built
# `rel_time` column). Used by the plots' `time_axis = "calendar"` option.
.es_ref_time <- function(x) {
  if (is.null(x)) return(NULL)
  u <- unique(x[!is.na(x)])
  if (length(u) == 1L) u else NULL
}

# Extract cluster variable name(s) as a character scalar/vector from the
# `cluster` argument (formula or character).
.cluster_vars_chr <- function(cluster) {
  if (inherits(cluster, "formula")) {
    rlang::expr_text(rlang::f_rhs(cluster))
  } else {
    cluster
  }
}

# --------------------------------------------------------------------------- #

#' Event study estimation for panel data
#'
#' Estimates dynamic treatment effects by relative time (an event-study
#' curve) on panel data. Supports the classic TWFE design (universal or
#' staggered timing) plus modern staggered-robust estimators —
#' Callaway-Sant'Anna, Sun-Abraham, Borusyak-Jaravel-Spiess, Wooldridge
#' two-way Mundlak, and Deb et al. FLEX — selected via `estimator=`.
#' Estimation runs on the package's internal fixed-effects OLS engine;
#' results are returned as a tidy table with rich metadata.
#'
#' The model is a formula, `outcome ~ treatment + covariates | fixed effects`,
#' as in `fixest::feols()`; the same formula works in [did()] and [att()].
#'
#' `event_study()` supersedes [run_es()] as of fixes 1.0.0.
#'
#' @section Specifying treatment:
#' Describe treatment in whichever way matches your data:
#'
#' \tabular{ll}{
#'   \strong{Your data has} \tab \strong{Write} \cr
#'   D (1 for treated units in post-treatment periods) \tab `y ~ D + x | id + year, time = year` \cr
#'   each unit's first treated period (`NA` = never treated) \tab `y ~ x | id + year, time = year, timing = g` \cr
#'   a treated-group dummy and one treatment date \tab `y ~ treated + x | id + year, time = year, timing = 2005` \cr
#' }
#'
#' - **D column** (no `timing`): the first right-hand-side term is D. A
#'   unit's treatment starts in the first period with `D = 1` and must stay
#'   on from then on (staggered adoption). Rows with a missing D are dropped.
#'   A single start date across units gives the classic universal design,
#'   several give the staggered one. With `time_transform = TRUE` the start
#'   is read on the time index, so no index arithmetic is needed.
#' - **Timing column** (`timing = g`): `g` defines treatment, so the
#'   right-hand side holds only covariates (write `y ~ 1 | id + year` when
#'   there are none). D or the treated-group dummy must not appear in the
#'   formula.
#' - **Treated-group dummy and a date** (`timing = 2005`): the first term is
#'   the treated-group dummy (constant within units), and treatment starts at
#'   `timing` for every treated unit.
#'
#' A pre-built event-time column goes in `rel_time`, with the treated-group
#' dummy as the first term: `y ~ treated | id + year, time = year,
#' rel_time = ttt`.
#'
#' @section Panel identifiers:
#' `time` is required. `unit` is taken from the fixed effects when they are
#' plain columns made of `time` and exactly one other column (`| id + year`
#' with `time = year`, in either order); a message then reports the unit and
#' time used. Otherwise pass `unit`: it is needed for a D column and for
#' `estimator = "cs"`, `"sa"`, `"bjs"` and `"twm"`. `print()` shows the
#' identifiers of every result.
#'
#' @section Fixed effects and covariates by estimator:
#' `"twfe"`, `"sa"` and `"twm"` use the fixed effects as written. `"cs"`,
#' `"bjs"` and `"flex"` absorb the unit (group) and time effects themselves:
#' those fixed effects are accepted silently, any others are ignored with a
#' warning. Covariates are supported by all six estimators (`"twfe"`, `"cs"`,
#' `"sa"`, `"bjs"`, `"twm"`, and `"flex"`), and may be expressions such as
#' `log(x)` or `factor(region)`.
#'
#' @details
#' **How relative (event) time is defined.** `event_study()` can build the
#' event-time axis in three different ways, and in sparse or unbalanced panels
#' (e.g. monthly surveys observed only a few times a year) they do **not**
#' coincide:
#'
#' \tabular{lll}{
#'   \strong{Method} \tab \strong{Definition} \tab \strong{How to invoke} \cr
#'   Calendar difference (default) \tab `(time - timing) / interval` \tab a timing column, a scalar `timing`, or a D column \cr
#'   Within-unit rank \tab `dense_rank(time)` within each unit \tab `time_transform = TRUE` \cr
#'   Pre-built event time \tab supplied column, used verbatim \tab `rel_time = <col>` \cr
#' }
#'
#' In a balanced panel all three agree.  When observation spacing is irregular
#' they diverge: for a unit observed at periods `{1, 5, 9, 20}` treated at 20,
#' the calendar difference puts period 9 at `9 - 20 = -11`, whereas a global
#' observed-period grid (as used by `paneltools`/`fect`'s `get.cohort()`) places
#' it two observed waves before treatment (`-2`).  To reproduce an analysis built
#' on such a grid, pass that grid's event-time column via `rel_time` so the
#' design matches `fixest::feols(y ~ i(rel_time, treatment, ref) | fe)` exactly.
#' `att()` uses the same calendar-difference convention as the default here.
#' With a D column, a unit's start is the first period in which D = 1 is
#' observed.
#'
#' **Missing values and unbalanced panels.** With `estimator = "cs"`, rows
#' with a missing or non-finite outcome, time, or unit are dropped, and the
#' panel is then balanced by dropping every unit not observed in all periods,
#' as [did::att_gt()] does by default (`allow_unbalanced_panel = FALSE`).
#' Both steps warn, and `N`, `N_units`, `N_treated` and `N_nevertreated`
#' describe the balanced sample. The regression-based estimators drop rows
#' with missing values from the fit as `fixest::feols()` does; with
#' `estimator = "sa"` or `"twm"` the cohort shares used to aggregate
#' cohort-specific effects count only units that remain in the estimation
#' sample (Sun and Abraham 2021, Section 4.1).
#'
#' @section Key Features:
#' - One formula for the model (outcome, treatment, covariates, fixed
#'   effects), shared with [did()] and [att()].
#' - Six estimators behind one interface: `"twfe"`, `"cs"`, `"sa"`, `"bjs"`, `"twm"`, `"flex"`.
#' - Flexible clustering, weights, and VCOV choices (e.g., `vcov = "HC1" | "iid" | "hetero" ...`).
#' - Automatic lead/lag window detection and customizable baseline period.
#' - Returns an `"es_result"` object compatible with `plot()`, `print()`, and `autoplot()`.
#'
#' @param formula A model formula
#'   `outcome ~ treatment + covariates | fixed effects`. The left-hand side
#'   may be an expression (e.g. `log(y)`); the part after `|` is optional.
#'   What the first right-hand-side term means depends on `timing`; see
#'   **Specifying treatment**. Fixed effects such as `id^year` require the
#'   optional \{fixest\} package. All variables must be columns of `data`.
#' @param data A data.frame containing panel data. `formula` and `data` may
#'   come in either order, so `df |> event_study(y ~ D | id + year, time =
#'   year)` works.
#' @param time Unquoted time variable (numeric or Date).
#' @param timing Treatment timing; see **Specifying treatment**. Either an
#'   unquoted column giving each unit's first treatment period (`NA` = never
#'   treated) or a single treatment date (numeric or Date). Omit it when the
#'   formula holds a D column. A timing column must have the same value on
#'   every row of a unit; `"cs"`, `"sa"`, `"bjs"` and `"twm"` stop with an
#'   error naming the offending units otherwise.
#'   **`NA` means never treated:** this follows `did::att_gt()`,
#'   `fixest::sunab()`, and `didimputation`. If `NA` instead represents
#'   *missing* treatment timing for an otherwise treated unit, those
#'   observations are silently absorbed into the control group, which is
#'   almost certainly wrong. A D column avoids the coding question.
#' @param unit Unquoted unit identifier. Taken from the fixed effects when
#'   possible; see **Panel identifiers**.
#' @param lead_range,lag_range Integers for pre/post windows. If `NULL`, determined automatically.
#' @param cluster Cluster specification (one-sided formula like `~ id + year`, a single character column name, or a vector of length `nrow(data)`).
#' @param weights Observation weights (a name/one-sided formula or a numeric vector of length `nrow(data)`).
#' @param baseline Integer baseline period (default `-1`); the reference period excluded from the estimation results.
#' @param interval Numeric spacing of the time variable (default `1`; ignored internally for Dates).
#' @param time_transform Logical; if `TRUE`, creates consecutive integer time
#'   within unit. A timing column or value is then read as an index; a D
#'   column needs no adjustment.
#' @param rel_time Optional unquoted column holding a **pre-built event time**
#'   (time relative to treatment), e.g. the `Time_to_Treatment` produced by
#'   `paneltools`/`fect`'s `get.cohort()`.  When supplied, `event_study()` uses
#'   this column verbatim as the event-study factor — `i(rel_time, treatment,
#'   ref = baseline)` — instead of computing relative time internally from
#'   `time` and `timing`; the first right-hand-side term is then the
#'   treated-group dummy.  Never-treated / control rows should be `NA`.
#'   Requires `estimator = "twfe"`; cannot be combined with `timing`,
#'   `staggered = TRUE` or `time_transform = TRUE`.  See **Details** for how
#'   this differs from the calendar-difference and `time_transform`
#'   conventions.
#' @param staggered Logical or `NULL` (default). Only consulted by
#'   `estimator = "twfe"`: `TRUE` means a staggered design, `FALSE` a single
#'   universal treatment date. With the default `NULL` the design follows
#'   from how treatment is given (a timing column is staggered, a single
#'   date is universal, a D column is universal when every unit starts in
#'   the same period). Pass an explicit value to override the inference.
#' @param estimator Estimation strategy: `"twfe"` (default) for the classic
#'   TWFE event study, `"cs"` (Callaway-Sant'Anna 2021), `"sa"` (Sun-Abraham
#'   2021 interaction-weighted), `"bjs"` (Borusyak-Jaravel-Spiess 2024
#'   imputation), `"twm"` (Wooldridge two-way Mundlak), or `"flex"`
#'   (Deb et al. 2024, repeated cross-sections).
#' @param control_group For `estimator = "cs"`: comparison group, either
#'   `"nevertreated"` (default) or `"notyettreated"`.
#' @param anticipation For `estimator = "cs"`: number of anticipation periods
#'   before treatment (non-negative integer, default `0L`).
#' @param est_method For `estimator = "cs"` with covariates: estimation method,
#'   `"trad"` (traditional doubly robust, default; `"dr"` in the did package),
#'   `"imp"` (Sant'Anna & Zhao 2020 improved doubly robust with IPT propensity
#'   score), `"reg"` (outcome regression), or `"ipw"` (inverse probability
#'   weighting), as in [did()]. Ignored when no covariates are supplied. As in
#'   the DRDID package, propensity scores are
#'   capped at `1 - 1e-6` and control units with a score of at least 0.995 get
#'   zero weight; with `"imp"`, ATT(g,t) cells whose IPT fit does not converge
#'   are dropped with a warning.
#' @param keep_ps Logical; for `estimator = "cs"` with covariates and
#'   `est_method` in `"trad"`, `"imp"`, or `"ipw"`, store per-unit propensity
#'   scores so [propensity_scores()] can extract them. Default `FALSE`.
#' @param conf_level Numeric vector of confidence levels (default `0.95`).
#' @param vcov VCOV type: `"HC1"` (default), `"iid"`, or `"hetero"` on the
#'   internal engine; other types are delegated to `fixest::vcov()` when the
#'   optional \{fixest\} package is installed.
#' @param vcov_args List of additional arguments forwarded to `fixest::vcov()`
#'   (requires \{fixest\}).
#' @param bootstrap Logical; if `TRUE` and `estimator = "cs"`, compute
#'   simultaneous confidence bands via the multiplier bootstrap (Algorithm 1,
#'   Callaway and Sant'Anna 2021).  Adds `conf_low_sim` and
#'   `conf_high_sim` columns to the result and stores the full (g,t)-level
#'   bootstrap object as `attr(result, "bootstrap")`.  Default `FALSE`.
#' @param boot_reps Integer number of bootstrap draws (default `999`).  Used
#'   only when `bootstrap = TRUE` and `estimator = "cs"`.
#' @param boot_alpha Significance level for the simultaneous band (default
#'   `0.05`).  Note: this is independent of `conf_level`, which governs the
#'   pointwise delta-method CIs.
#' @param boot_seed Integer seed for the bootstrap RNG; `NULL` (default)
#'   does not set a seed.  Pass an integer for reproducible results.
#' @param group Unquoted group identifier for `estimator = "flex"` only.
#'   Identifies which treatment group (cohort) each observation belongs to in a
#'   repeated cross-section design (\eqn{R_{ig}} in Deb et al. 2024).  Each
#'   group must map to exactly one value of `timing` (or `NA` for
#'   never-treated groups).  With a D column, each group's start is the first
#'   period with `D = 1`.  Not used by other estimators.
#' @param trends Logical; for `estimator = "twm"` only.  When `TRUE`,
#'   adds cohort-specific linear trend regressors \eqn{d_g \cdot t} to the
#'   Procedure 5.1 regression (Wooldridge 2025, Section 8), allowing each
#'   cohort's counterfactual trend to deviate linearly from the common time
#'   trend.  Requires at least 2 pre-treatment periods per cohort.
#'   Default `FALSE`.
#' @param pretrends Logical or negative integer vector; for `estimator = "bjs"`
#'   only. When `TRUE`, estimates pre-treatment period coefficients on
#'   untreated observations (Borusyak et al. 2024, Section 4.4) excluding
#'   `baseline`; when a negative integer vector (e.g. `-4:-2`), estimates those
#'   specific horizons. Default `FALSE`.
#' @param outcome,treatment,fe,covariates `r lifecycle::badge("deprecated")`
#'   The model arguments of the older interface: the unquoted outcome, the
#'   treated-group dummy (used only by `estimator = "twfe"`), a one-sided
#'   fixed-effects formula, and a one-sided covariate formula. They still
#'   work in fixes 1.x, with a deprecation warning that shows the equivalent
#'   formula call; fixes 2.0.0 is planned to accept only `formula`. Name them
#'   when you use them: the second argument is now `data`.
#'
#' @return A `data.frame` of class `"es_result"` with columns:
#' - `term`, `estimate`, `std.error`, `statistic`, `p.value`
#' - `conf_low_XX`, `conf_high_XX` (for each requested `conf.level`)
#' - `relative_time` (integer; 0 = event), `is_baseline` (logical; marks the reference period)
#'
#' Attributes include: `lead_range`, `lag_range`, `baseline`, `interval`, `call`, `model_formula`, `conf.level`,
#' `N`, `N_units`, `N_treated`, `N_nevertreated`, `fe`, `vcov_type`, `cluster_vars`, `staggered`, `sunab_used`,
#' `unit_var`, `time_var`.
#'
#' @seealso [plot.es_result()] for visualization, [att()] for aggregated ATT
#'   estimation, [propensity_scores()] for extracting propensity scores, and
#'   [run_es()] for the deprecated verb-style predecessor.
#'
#' @examples
#' \dontrun{
#' # D column: 1 for treated units in post-treatment periods
#' res <- event_study(y ~ D | id + year, data = df, time = year)
#' plot(res)
#'
#' # Timing column (NA = never treated), Callaway-Sant'Anna
#' res <- event_study(y ~ 1 | id + year, data = df, time = year, timing = g,
#'                    estimator = "cs")
#'
#' # Treated-group dummy and a single treatment date, clustered by unit
#' res <- event_study(y ~ treated + x1 | id + year, data = df, time = year,
#'                    timing = 2005, cluster = ~ id)
#' }
#'
#' @importFrom stats as.formula qnorm setNames vcov
#' @importFrom dplyr bind_rows mutate arrange filter left_join group_by ungroup n_distinct
#' @importFrom rlang .data
#' @importFrom utils getFromNamespace
#' @export
event_study <- function(
  formula,
  data,
  time,
  timing = NULL,
  unit = NULL,
  estimator = c("twfe", "cs", "sa", "bjs", "twm", "flex"),
  cluster = NULL,
  weights = NULL,
  baseline = -1L,
  lead_range = NULL,
  lag_range = NULL,
  conf_level = 0.95,
  vcov = "HC1",
  vcov_args = list(),
  control_group = c("nevertreated", "notyettreated"),
  anticipation = 0L,
  est_method = c("trad", "imp", "reg", "ipw"),
  keep_ps = FALSE,
  bootstrap = FALSE,
  boot_reps = 999L,
  boot_alpha = 0.05,
  boot_seed = NULL,
  group = NULL,
  trends = FALSE,
  pretrends = FALSE,
  interval = 1,
  time_transform = FALSE,
  rel_time = NULL,
  staggered = NULL,
  outcome,
  treatment = NULL,
  fe = NULL,
  covariates = NULL
) {
  estimator <- match.arg(estimator)
  cl  <- match.call()
  env <- parent.frame()
  fd  <- .formula_and_data(rlang::enquo(formula), rlang::enquo(data),
                           "event_study")
  model_args <- c("outcome", "treatment", "fe", "covariates")

  if (is.null(fd$formula)) {
    names(cl)[names(cl) == "formula"] <- "data"
    .deprecate_model_args(
      "event_study", "`outcome`, `treatment`, `fe` and `covariates`",
      .legacy_formula_call("event_study", cl, fd$data, env, estimator), env
    )
    return(.eval_impl(cl, "event_study", .event_study_impl, env))
  }
  if (any(model_args %in% names(cl))) {
    stop("Give the model either as `formula` or through `outcome`, ",
         "`treatment`, `fe` and `covariates`, not both.", call. = FALSE)
  }

  data <- fd$data
  p <- .parse_model_formula(fd$formula, data, "event_study")
  timing_s <- .timing_spec(cl$timing, data, env)
  has_rel  <- !is.null(cl$rel_time) && !identical(cl$rel_time, quote(NULL))
  if (has_rel && timing_s$kind != "none") {
    stop("`rel_time` already encodes event time relative to treatment; ",
         "drop `timing`.", call. = FALSE)
  }
  mode_d <- timing_s$kind == "none" && !has_rel

  .require_time(cl$time)
  time_chr  <- .resolve_col(cl$time, data)
  group_chr <- .col_arg(cl$group, data)
  is_flex   <- estimator == "flex"
  panel <- if (is_flex) {
    list(unit = .col_arg(cl$unit, data), time = time_chr)
  } else {
    .resolve_panel(time_chr, .col_arg(cl$unit, data), p$fe,
                   need_unit = mode_d ||
                     estimator %in% c("cs", "sa", "bjs", "twm"))
  }
  id_chr <- if (is_flex) group_chr else panel$unit
  if (is_flex && is.null(group_chr)) {
    stop("`group` is required when `estimator = 'flex'`.", call. = FALSE)
  }

  tr <- .formula_treatment(
    p, timing_s, has_rel, data, id_chr, time_chr, "event_study",
    derive_cohort = !(estimator == "twfe" && timing_s$kind == "value"),
    time_transform = time_transform
  )
  data <- tr$data
  covariates <- if (estimator == "twfe") {
    .covariate_formula(tr$covariates, p$env)
  } else {
    .formula_covariates(tr$covariates, estimator, p$env)
  }
  fe <- .formula_fe(p$fe, estimator,
                    keep = c(if (is_flex) group_chr else panel$unit, time_chr))

  treatment_chr <- NULL
  timing_arg    <- NULL
  if (estimator == "twfe" && tr$mode %in% c("rel_time", "value")) {
    treatment_chr <- tr$term
    if (tr$mode == "value") timing_arg <- timing_s$value
  } else if (estimator == "twfe") {
    treatment_chr <- .fresh_col(data, ".fixes_treated")
    data[[treatment_chr]] <- as.integer(!is.na(tr$cohort))
    starts <- unique(tr$cohort[!is.na(tr$cohort)])
    if (tr$mode == "cohort") {
      timing_arg <- timing_s$name
    } else if (length(starts) == 1L && !isTRUE(staggered)) {
      timing_arg <- starts
      staggered  <- FALSE
    } else {
      if (isFALSE(staggered)) {
        stop("`", tr$term, "` starts in ", length(starts), " different ",
             "periods; `staggered = FALSE` needs a single treatment date.",
             call. = FALSE)
      }
      timing_arg <- .fresh_col(data, ".fixes_timing")
      data[[timing_arg]] <- tr$cohort
      staggered <- TRUE
    }
  } else if (tr$mode == "cohort") {
    timing_arg <- timing_s$name
  } else if (tr$mode != "rel_time") {
    timing_arg <- .fresh_col(data, ".fixes_timing")
    data[[timing_arg]] <- tr$cohort
  }

  new <- cl[!names(cl) %in% c("formula", model_args, "time", "timing", "unit",
                              "staggered", "estimator")]
  new$outcome    <- p$lhs
  new$treatment  <- treatment_chr
  new$time       <- time_chr
  new$timing     <- timing_arg
  new$unit       <- panel$unit
  new$fe         <- fe
  new$covariates <- covariates
  new$staggered  <- staggered
  new$estimator  <- estimator

  res <- .eval_impl(new, "event_study", .event_study_impl, env, data = data)
  attr(res, "call") <- .user_call(cl, fd$swapped)
  res
}

# The estimation behind event_study(), with the argument interface of
# run_es(); run_es() and the legacy event_study() arguments call it directly.
.event_study_impl <- function(
  data,
  outcome,
  treatment = NULL,
  time,
  timing,
  fe = NULL,
  lead_range = NULL,
  lag_range = NULL,
  covariates = NULL,
  cluster = NULL,
  weights = NULL,
  baseline = -1L,
  interval = 1,
  time_transform = FALSE,
  rel_time = NULL,
  unit = NULL,
  staggered = NULL,
  estimator = c("twfe", "cs", "sa", "bjs", "twm", "flex"),
  control_group = c("nevertreated", "notyettreated"),
  anticipation = 0L,
  est_method = c("trad", "imp", "reg", "ipw"),
  keep_ps = FALSE,
  conf_level = 0.95,
  vcov = "HC1",
  vcov_args = list(),
  bootstrap = FALSE,
  boot_reps = 999L,
  boot_alpha = 0.05,
  boot_seed = NULL,
  group = NULL,
  trends = FALSE,
  pretrends = FALSE
) {
  estimator <- match.arg(estimator)
  stopifnot(is.data.frame(data))
  if (!is.numeric(interval) || interval <= 0) {
    stop("`interval` must be positive.")
  }

  # Internal names kept from the verb-style API so the estimator branches
  # below stay unchanged.
  conf.level <- conf_level
  B <- boot_reps
  alpha <- boot_alpha

  # ---- staggered inference ---------------------------------------------------
  # Only the twfe estimator distinguishes universal from staggered timing.
  # Default NULL: `timing` naming a column of `data` means staggered;
  # anything else (scalar value, expression) means universal timing.
  if (is.null(staggered)) {
    timing_expr <- rlang::enexpr(timing)
    staggered <- !rlang::is_missing(timing_expr) &&
      ((rlang::is_symbol(timing_expr) &&
          rlang::as_string(timing_expr) %in% names(data)) ||
         (is.character(timing_expr) && length(timing_expr) == 1L &&
            timing_expr %in% names(data)))
  }
  staggered <- isTRUE(staggered)

  # ---- helpers -------------------------------------------------------------
  # NSE column resolution is provided by the shared `.resolve_col()` helper in
  # R/utils-internal.R; alias it locally so the call sites below stay unchanged.
  resolve_column <- .resolve_col
  .must_exist <- function(cols, data) {
    cols <- as.character(cols)
    miss <- setdiff(cols, names(data))
    if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "))
  }

  # ---- resolve core variables ---------------------------------------------
  outcome_chr <- resolve_column(rlang::enexpr(outcome), data, allow_call = TRUE)
  time_chr <- resolve_column(rlang::enexpr(time), data)
  # Kept for plot axis labels: `time_transform` overwrites `time_chr`.
  time_var <- time_chr

  unit_chr <- NULL
  unit_expr <- rlang::enexpr(unit)
  if (!is.null(unit_expr)) {
    unit_chr <- resolve_column(unit_expr, data)
  }

  # ---- rel_time: pre-built event time supplied directly --------------------
  # When the caller already holds an event-time column (e.g. the
  # `Time_to_Treatment` produced by paneltools/fect's get.cohort, or any
  # cross-tool construction), `rel_time` uses it verbatim as the i() factor,
  # bypassing run_es()'s own (time - timing)/interval relative-time arithmetic.
  # This normalises the previous `time = event_time, timing = 0` idiom.
  rel_time_expr <- rlang::enexpr(rel_time)
  use_rel_time  <- !is.null(rel_time_expr) &&
    !identical(rel_time_expr, quote(NULL))
  if (use_rel_time) {
    if (estimator != "twfe")
      stop("`rel_time` is only supported for `estimator = \"twfe\"` ",
           "(the classic event-study path).")
    if (isTRUE(staggered))
      stop("`rel_time` cannot be combined with `staggered = TRUE`: ",
           "it already encodes event time relative to treatment.")
    if (isTRUE(time_transform))
      stop("`rel_time` cannot be combined with `time_transform = TRUE`.")
  }

  # time transform (dense_rank within unit)
  if (isTRUE(time_transform)) {
    if (is.null(unit_chr)) {
      stop("`time_transform=TRUE` requires `unit`.")
    }
    data <- data |>
      dplyr::group_by(.data[[unit_chr]]) |>
      dplyr::arrange(.data[[time_chr]], .by_group = TRUE) |>
      dplyr::mutate(.time_index = dplyr::dense_rank(.data[[time_chr]])) |>
      dplyr::ungroup()
    time_chr <- ".time_index"
  }

  # covariates text
  cov_text <- ""
  if (!is.null(covariates)) {
    if (!inherits(covariates, "formula")) {
      stop("`covariates` must be a one-sided formula (e.g., ~ x1 + log(x2)).")
    }
    cov_text <- rlang::expr_text(rlang::f_rhs(covariates))
  }

  # FE
  fe_rhs_text <- ""
  if (!is.null(fe)) {
    if (!inherits(fe, "formula")) {
      stop("`fe` must be a one-sided formula, e.g., ~ id + year.")
    }
    fe_rhs_text <- rlang::expr_text(rlang::f_rhs(fe))
  }

  # ---- cluster validation (feols accepts formula/character/vector) ---------
  if (!is.null(cluster)) {
    if (is.character(cluster)) {
      if (length(cluster) == 1L) {
        if (!cluster %in% names(data)) {
          stop("`cluster` as character must be a column in data.")
        }
      } else if (length(cluster) != nrow(data)) {
        stop(
          "Character `cluster` must be length 1 (column name) or length nrow(data)."
        )
      }
    } else if (!inherits(cluster, "formula")) {
      # allow numeric vector of length nrow(data)
      if (!is.null(cluster) && length(cluster) != nrow(data)) {
        stop("Vector `cluster` must be length nrow(data).")
      }
    }
  }

  # ---- estimator = cs -------------------------------------------------------
  if (estimator == "cs") {
    if (!missing(fe) && !is.null(fe)) {
      warning(
        "`fe` is ignored when `estimator = 'cs'`. ",
        "The CS estimator absorbs unit and time effects via first-differencing."
      )
    }

    timing_chr <- resolve_column(rlang::enexpr(timing), data)
    time_chr2 <- time_chr
    if (is.null(unit_chr)) {
      stop("`unit` is required when `estimator = 'cs'`.")
    }

    control_group <- match.arg(control_group)
    est_method    <- .match_est_method(est_method)
    conf.level <- sort(unique(conf.level))

    cs_out <- .run_cs(
      data = data,
      outcome_chr = outcome_chr,
      timing_chr = timing_chr,
      time_chr = time_chr2,
      unit_chr = unit_chr,
      anticipation = anticipation,
      control_group = control_group,
      conf.level = conf.level,
      covariates = covariates,
      est_method = est_method,
      keep_ps = isTRUE(keep_ps)
    )

    n_units <- cs_out$n_units
    tidy <- .make_tidy(cs_out$es, conf.level)
    tidy <- .es_finalize(
      tidy,
      baseline,
      conf.level,
      lead_range,
      lag_range,
      meta = list(
        interval = interval,
        ref_time = .es_ref_time(cs_out$data[[timing_chr]]),
        time_levels = sort(unique(cs_out$data[[time_chr]])),
        time_var = time_var,
        unit_var = unit_chr,
        time_transform = isTRUE(time_transform),
        call = match.call(),
        model_formula = "cs",
        N = cs_out$n_obs,
        N_units = n_units,
        N_treated = n_units - cs_out$n_never,
        N_nevertreated = cs_out$n_never,
        fe = "",
        vcov_type = "analytic",
        cluster_vars = NULL,
        staggered = TRUE,
        sunab_used = FALSE,
        cs_control_group = control_group,
        att_gt = cs_out$att_gt,
        dropped_cohorts = cs_out$dropped_cohorts
      )
    )

    # ---- Multiplier bootstrap (Algorithm 1, CS 2021) -------------------------
    if (isTRUE(bootstrap)) {
      # 1. Unit-level influence functions at the (g,t) level
      psi_gt <- if (!is.null(cs_out$if_ctx$psi_gt)) {
        cs_out$if_ctx$psi_gt
      } else {
        .cs_psi_cols(cs_out$if_ctx, cs_out$att_gt$g, cs_out$att_gt$t)
      }
      gt_index <- data.frame(
        col_idx = seq_len(nrow(cs_out$att_gt)),
        g = cs_out$att_gt$g,
        t = cs_out$att_gt$t,
        stringsAsFactors = FALSE
      )

      # 2. Event-study influence functions, already built for the SEs
      es_idx <- data.frame(
        col_idx = seq_len(nrow(cs_out$es)),
        relative_time = cs_out$es$relative_time,
        estimate = cs_out$es$estimate,
        std_error = cs_out$es$std_error,
        stringsAsFactors = FALSE
      )

      # 3. Bootstrap at the event-study level → simultaneous CI over all ℓ
      boot_es <- .bootstrap_cs(
        psi = cs_out$psi_es,
        att_gt = es_idx,
        gt_index = es_idx,
        B = as.integer(B),
        alpha = alpha,
        seed = boot_seed
      )

      # 4. Merge simultaneous CI into tidy on relative_time.
      # Save and restore custom attributes: base::merge() drops them.
      saved_attrs <- attributes(tidy)
      saved_attrs <- saved_attrs[
        !names(saved_attrs) %in% c("names", "row.names", "class")
      ]

      boot_cols <- boot_es[, c(
        "relative_time",
        "conf_low_sim",
        "conf_high_sim"
      )]
      tidy <- merge(
        tidy,
        boot_cols,
        by = "relative_time",
        all.x = TRUE,
        sort = FALSE
      )
      tidy <- tidy[order(tidy$relative_time), ]
      rownames(tidy) <- NULL

      for (.attr_nm in names(saved_attrs)) {
        attr(tidy, .attr_nm) <- saved_attrs[[.attr_nm]]
      }
      rm(.attr_nm)

      # Baseline row: by convention ATT = 0, so simultaneous CI = [0, 0]
      tidy$conf_low_sim[tidy$is_baseline] <- 0
      tidy$conf_high_sim[tidy$is_baseline] <- 0

      # 5. Bootstrap at the (g,t) level — stored as attribute
      boot_gt <- .bootstrap_cs(
        psi = psi_gt,
        att_gt = cs_out$att_gt,
        gt_index = gt_index,
        B = as.integer(B),
        alpha = alpha,
        seed = boot_seed
      )
      attr(tidy, "bootstrap") <- boot_gt
      attr(tidy, "boot_alpha") <- alpha
    }

    class(tidy) <- c("es_result", "data.frame")
    attr(tidy, "estimator") <- estimator
    attr(tidy, "es_vcov")   <- .trim_es_vcov(cs_out$es_vcov, tidy)
    if (!is.null(cs_out$propensity_scores)) {
      attr(tidy, "propensity_scores") <- cs_out$propensity_scores
    }
    return(tidy)
  }

  # ---- estimator = sa -------------------------------------------------------
  if (estimator == "sa") {
    timing_chr <- resolve_column(rlang::enexpr(timing), data)
    if (is.null(unit_chr)) {
      stop("`unit` is required when `estimator = 'sa'`.")
    }

    fe_str <- if (nzchar(fe_rhs_text)) {
      fe_rhs_text
    } else {
      warning(
        "`fe` not specified for SA estimator; defaulting to `~ ",
        unit_chr,
        " + ",
        time_chr,
        "`."
      )
      paste(unit_chr, time_chr, sep = " + ")
    }

    cov_sa <- .covariate_columns(covariates, data)
    conf.level <- sort(unique(conf.level))

    sa_out <- .run_sa(
      data = cov_sa$data,
      outcome_chr = outcome_chr,
      timing_chr = timing_chr,
      time_chr = time_chr,
      unit_chr = unit_chr,
      fe_str = fe_str,
      baseline = baseline,
      cluster = cluster,
      vcov_type = vcov,
      vcov_args = vcov_args,
      conf.level = conf.level,
      covariate_chrs = cov_sa$names,
      weights = weights
    )

    n_units <- length(unique(data[[unit_chr]]))
    tidy <- .make_tidy(sa_out$es, conf.level)
    tidy <- .es_finalize(
      tidy,
      baseline,
      conf.level,
      lead_range,
      lag_range,
      meta = list(
        interval = interval,
        ref_time = .es_ref_time(data[[timing_chr]]),
        time_levels = sort(unique(data[[time_chr]])),
        time_var = time_var,
        unit_var = unit_chr,
        time_transform = isTRUE(time_transform),
        call = match.call(),
        model_formula = sa_out$formula_str,
        N = sa_out$n_obs,
        N_units = n_units,
        N_treated = n_units - sa_out$n_never,
        N_nevertreated = sa_out$n_never,
        fe = fe_str,
        vcov_type = vcov,
        cluster_vars = .cluster_vars_chr(cluster),
        staggered = TRUE,
        sunab_used = FALSE,
        catt_df = sa_out$catt_df
      )
    )

    attr(tidy, "estimator") <- estimator
    attr(tidy, "es_vcov")   <- .trim_es_vcov(sa_out$es_vcov, tidy)
    return(tidy)
  }

  # ---- estimator = bjs -------------------------------------------------------
  if (estimator == "bjs") {
    timing_chr <- resolve_column(rlang::enexpr(timing), data)
    if (is.null(unit_chr)) {
      stop("`unit` is required when `estimator = 'bjs'`.")
    }

    conf.level <- sort(unique(conf.level))

    clustervars_chr <- if (!is.null(cluster)) {
      if (inherits(cluster, "formula")) {
        all.vars(rlang::f_rhs(cluster))
      } else if (is.character(cluster) && length(cluster) == 1L) {
        cluster
      } else {
        NULL
      }
    } else {
      NULL
    }

    bjs_out <- .run_bjs(
      data = data,
      outcome_chr = outcome_chr,
      timing_chr = timing_chr,
      time_chr = time_chr,
      unit_chr = unit_chr,
      clustervars = clustervars_chr,
      conf.level = conf.level,
      covariates = covariates,
      pretrends = pretrends,
      baseline = baseline
    )

    n_units <- length(unique(data[[unit_chr]]))
    tidy <- .make_tidy(bjs_out$es, conf.level)
    tidy <- .es_finalize(
      tidy,
      baseline,
      conf.level,
      lead_range,
      lag_range,
      meta = list(
        interval = interval,
        ref_time = .es_ref_time(data[[timing_chr]]),
        time_levels = sort(unique(data[[time_chr]])),
        time_var = time_var,
        unit_var = unit_chr,
        time_transform = isTRUE(time_transform),
        call = match.call(),
        model_formula = "bjs",
        N = nrow(data),
        N_units = n_units,
        N_treated = n_units - bjs_out$n_never,
        N_nevertreated = bjs_out$n_never,
        fe = paste(unit_chr, time_chr, sep = " + "),
        vcov_type = "bjs_conservative",
        cluster_vars = clustervars_chr,
        staggered = TRUE,
        sunab_used = FALSE,
        tau_it = bjs_out$tau_it,
        n_unimputed = bjs_out$n_unimputed,
        n_treated_obs = bjs_out$n_treated_obs
      ),
      bjs_lead = TRUE
    )

    attr(tidy, "estimator") <- estimator
    attr(tidy, "es_vcov")   <- .trim_es_vcov(bjs_out$es_vcov, tidy)
    return(tidy)
  }

  # ---- estimator = twm -------------------------------------------------------
  if (estimator == "twm") {
    timing_chr <- resolve_column(rlang::enexpr(timing), data)
    if (is.null(unit_chr)) {
      stop("`unit` is required when `estimator = 'twm'`.")
    }

    fe_str <- if (nzchar(fe_rhs_text)) {
      fe_rhs_text
    } else {
      warning(
        "`fe` not specified for TWM estimator; defaulting to `~ ",
        unit_chr,
        " + ",
        time_chr,
        "`."
      )
      paste(unit_chr, time_chr, sep = " + ")
    }

    cov_twm <- .covariate_columns(covariates, data)
    conf.level <- sort(unique(conf.level))

    twm_out <- .run_twm(
      data = cov_twm$data,
      outcome_chr = outcome_chr,
      timing_chr = timing_chr,
      time_chr = time_chr,
      unit_chr = unit_chr,
      fe_str = fe_str,
      baseline = baseline,
      trends = isTRUE(trends),
      covariate_chrs = cov_twm$names,
      cluster = cluster,
      vcov_type = vcov,
      vcov_args = vcov_args,
      conf.level = conf.level,
      weights = weights
    )

    n_units <- length(unique(data[[unit_chr]]))
    tidy <- .make_tidy(twm_out$es, conf.level)
    tidy <- .es_finalize(
      tidy,
      baseline,
      conf.level,
      lead_range,
      lag_range,
      meta = list(
        interval = interval,
        ref_time = .es_ref_time(data[[timing_chr]]),
        time_levels = sort(unique(data[[time_chr]])),
        time_var = time_var,
        unit_var = unit_chr,
        time_transform = isTRUE(time_transform),
        call = match.call(),
        model_formula = twm_out$formula_str,
        N = twm_out$n_obs,
        N_units = n_units,
        N_treated = n_units - twm_out$n_never,
        N_nevertreated = twm_out$n_never,
        fe = fe_str,
        vcov_type = vcov,
        cluster_vars = .cluster_vars_chr(cluster),
        staggered = TRUE,
        sunab_used = FALSE,
        tau_gt = twm_out$tau_gt
      )
    )

    attr(tidy, "estimator") <- estimator
    attr(tidy, "es_vcov")   <- .trim_es_vcov(twm_out$es_vcov, tidy)
    return(tidy)
  }

  # ---- estimator = flex -------------------------------------------------------
  if (estimator == "flex") {
    group_expr <- rlang::enexpr(group)
    if (is.null(group_expr)) {
      stop("`group` is required when `estimator = 'flex'`.")
    }
    group_chr <- resolve_column(group_expr, data)

    timing_chr <- resolve_column(rlang::enexpr(timing), data)

    if (!is.null(fe) && !is.null(fe)) {
      warning(
        "`fe` is ignored when `estimator = 'flex'`; ",
        "group and time fixed effects are used automatically."
      )
    }

    cov_flex <- .covariate_columns(covariates, data)
    conf.level <- sort(unique(conf.level))

    flex_out <- .run_flex(
      data = cov_flex$data,
      outcome_chr = outcome_chr,
      timing_chr = timing_chr,
      time_chr = time_chr,
      group_chr = group_chr,
      baseline = baseline,
      covariate_chrs = cov_flex$names,
      cluster = cluster,
      vcov_type = vcov,
      vcov_args = vcov_args,
      conf.level = conf.level,
      weights = weights
    )

    n_groups <- length(unique(data[[group_chr]]))
    cluster_vars_flex <- if (inherits(cluster, "formula")) {
      rlang::expr_text(rlang::f_rhs(cluster))
    } else if (!is.null(cluster)) {
      cluster
    } else {
      group_chr
    }

    tidy <- .make_tidy(flex_out$es, conf.level)
    tidy <- .es_finalize(
      tidy,
      baseline,
      conf.level,
      lead_range,
      lag_range,
      meta = list(
        interval = interval,
        ref_time = .es_ref_time(data[[timing_chr]]),
        time_levels = sort(unique(data[[time_chr]])),
        time_var = time_var,
        unit_var = group_chr,
        time_transform = isTRUE(time_transform),
        call = match.call(),
        model_formula = flex_out$formula_str,
        N = flex_out$n_obs,
        N_units = n_groups,
        N_treated = n_groups - flex_out$n_never_groups,
        N_nevertreated = flex_out$n_never_groups,
        fe = paste(group_chr, time_chr, sep = " + "),
        vcov_type = vcov,
        cluster_vars = cluster_vars_flex,
        staggered = TRUE,
        sunab_used = FALSE,
        tau_gt = flex_out$tau_gt
      )
    )

    attr(tidy, "estimator") <- estimator
    attr(tidy, "es_vcov")   <- .trim_es_vcov(flex_out$es_vcov, tidy)
    return(tidy)
  }

  # ---- estimator = twfe (classic event-study design) ------------------------
  # resolve treatment and timing
  treatment_chr <- resolve_column(rlang::enexpr(treatment), data)
  tx <- data[[treatment_chr]]
  if (is.logical(tx)) {
    tx <- as.integer(tx)
  }
  if (is.factor(tx)) {
    tx <- suppressWarnings(as.integer(as.character(tx)))
  }
  if (!is.numeric(tx)) {
    stop("`treatment` must be logical or numeric 0/1.")
  }
  if (any(!tx %in% c(0, 1, NA))) {
    stop("`treatment` must be 0/1 (or NA).")
  }

  # rel_time and staggered both build the event-study factor (..k) from a
  # per-row relative-time value and use i(..k, treatment, ref = baseline);
  # non-staggered classic instead interacts absolute time with treatment.
  staggered_design <- staggered || use_rel_time

  # timing
  timing_val   <- NULL
  timing_chr   <- NULL
  rel_time_chr <- NULL
  if (use_rel_time) {
    rel_time_chr <- resolve_column(rel_time_expr, data)
    if (!is.numeric(data[[rel_time_chr]])) {
      stop("`rel_time` must be a numeric/integer event-time column ",
           "(time relative to treatment; controls may be NA).")
    }
  } else if (staggered) {
    timing_chr <- resolve_column(rlang::enexpr(timing), data)

    # Warn when a unit has treatment = 1 in some rows but timing = NA.
    # NA is the "never treated" sentinel, so such units are silently used as
    # controls — correct if intentional, dangerous if NA means missing timing.
    treated_na_timing <- tx == 1L & is.na(data[[timing_chr]])
    if (any(treated_na_timing, na.rm = TRUE)) {
      if (!is.null(unit_chr)) {
        problem_units <- unique(data[[unit_chr]][treated_na_timing])
        n_prob <- length(problem_units)
        unit_str <- if (n_prob <= 5L) {
          paste(problem_units, collapse = ", ")
        } else {
          paste(c(utils::head(problem_units, 5L), "..."), collapse = ", ")
        }
        warning(
          n_prob, " unit(s) have `treatment = 1` in at least one row but `timing = NA`: ",
          unit_str, ".\n",
          "These units are treated as never-treated controls. ",
          "If NA means 'never treated', this is correct. ",
          "If NA represents missing treatment timing, remove or impute these rows.",
          call. = FALSE
        )
      } else {
        n_rows <- sum(treated_na_timing, na.rm = TRUE)
        warning(
          n_rows, " row(s) have `treatment = 1` but `timing = NA`. ",
          "These rows are treated as never-treated controls. ",
          "If NA means 'never treated', this is correct. ",
          "If NA represents missing treatment timing, remove or impute these rows.",
          call. = FALSE
        )
      }
    }
  } else {
    timing_val <- timing
    if (is.character(timing_val) && !timing_val %in% names(data)) {
      try_date <- suppressWarnings(as.Date(timing_val, format = "%Y-%m-%d"))
      if (!is.na(try_date)) timing_val <- try_date
    }
    # align Date types
    if (inherits(data[[time_chr]], "Date") && !inherits(timing_val, "Date")) {
      stop("`timing` must be a Date when `time` is a Date.")
    }
    if (!inherits(data[[time_chr]], "Date") && inherits(timing_val, "Date")) {
      stop("`time` must be a Date when `timing` is a Date.")
    }
  }

  # relative_time (for range calculation and staggered case)
  if (use_rel_time) {
    # Pre-built event time is used verbatim (NA on never-treated/control rows).
    rt <- data[[rel_time_chr]]
  } else if (staggered) {
    .must_exist(timing_chr, data)
    rt <- (data[[time_chr]] - data[[timing_chr]]) / interval
  } else {
    tv <- if (inherits(data[[time_chr]], "Date")) {
      as.numeric(timing_val)
    } else {
      timing_val
    }
    tnum <- if (inherits(data[[time_chr]], "Date")) {
      as.numeric(data[[time_chr]])
    } else {
      data[[time_chr]]
    }
    rt <- (tnum - tv) / interval
  }
  if (all(is.na(rt))) {
    stop("`relative_time` is NA for all rows. Check inputs.")
  }

  # integer event time
  k_vec <- suppressWarnings(as.integer(round(rt)))

  # auto ranges (based on treated units only)
  k_treated <- k_vec[tx == 1L & !is.na(tx)]
  if (is.null(lead_range)) {
    lead_range <- max(0L, abs(min(k_treated, na.rm = TRUE)))
  }
  if (is.null(lag_range)) {
    lag_range <- max(0L, max(k_treated, na.rm = TRUE))
  }

  # baseline check
  if (!is.finite(baseline) || (baseline %% 1L) != 0L) {
    stop("`baseline` must be an integer.")
  }
  if (baseline < -lead_range || baseline > lag_range) {
    stop("`baseline` outside [-lead_range, lag_range].")
  }

  # N_treated/N_nevertreated metadata must reflect the original NA pattern,
  # captured here before never-treated rows are patched below.
  ..k_known <- !is.na(k_vec)

  # Keep never-treated controls in the estimation sample.  With staggered
  # timing their ..k is NA (timing = NA), and feols drops every row with an
  # NA regressor — silently excluding the whole control group.  The i()
  # dummies are interacted with `treatment` (0 on these rows), so any non-NA
  # filler produces all-zero dummies; the baseline value is used so the rows
  # fall in the already-excluded reference level.
  if (staggered_design) {
    k_vec[is.na(k_vec)] <- as.integer(baseline)
  }

  # The fixest-style formula string is kept as metadata for reproducibility
  # (attr "model_formula"), although estimation now runs on the internal
  # FE-OLS engine.
  if (staggered_design) {
    i_formula <- paste0(
      "fixest::i(..k, ", treatment_chr, ", ref = ", baseline, ")"
    )
  } else {
    # Reference period from the baseline, e.g. timing = 5, baseline = -1
    # puts the reference at period 4.
    ref_period <- timing_val + baseline * interval
    i_formula <- paste0(
      "fixest::i(", time_chr, ", ", treatment_chr, ", ref = ", ref_period, ")"
    )
  }

  rhs <- i_formula
  if (nzchar(cov_text)) {
    rhs <- paste(rhs, cov_text, sep = " + ")
  }
  if (nzchar(fe_rhs_text)) {
    formula_string <- paste0(outcome_chr, " ~ ", rhs, " | ", fe_rhs_text)
  } else {
    formula_string <- paste0(outcome_chr, " ~ ", rhs)
  }

  fit_c <- .run_twfe_classic(
    data, outcome_chr, treatment_chr, time_chr,
    fe_rhs_text, cov_text, cluster, weights,
    staggered_design, timing_val, interval,
    baseline, vcov, vcov_args, k_vec = k_vec, tx_vec = tx
  )
  tidy <- fit_c$tidy

  # Full coefficient VCOV (same cluster/vcov precedence used to build `tidy`),
  # retained for honest_sensitivity() downstream.
  V_full_es <- fit_c$V_full

  # Add baseline row (0 estimate, 0 SE) for the dropped reference
  baseline_row <- tibble::tibble(
    term = as.character(baseline),
    estimate = 0,
    std.error = 0,
    statistic = NA_real_,
    p.value = NA_real_,
    relative_time = baseline
  )

  # Combine with baseline row
  tidy <- dplyr::bind_rows(tidy, baseline_row)
  tidy$is_baseline <- tidy$relative_time == baseline

  # Arrange by relative_time
  tidy <- tidy |> dplyr::arrange(.data$relative_time)

  # Filter results to specified ranges
  tidy <- tidy |>
    dplyr::filter(
      !is.na(.data$relative_time) &
        .data$relative_time >= -lead_range &
        .data$relative_time <= lag_range
    )

  # Event-study coefficient VCOV (ordered by relative time) — built from the
  # original coefficient term names before they are relabelled below.
  es_vcov <- .build_es_vcov(
    V_full_es,
    tidy$term[!tidy$is_baseline],
    tidy$relative_time[!tidy$is_baseline]
  )

  # Update term column to show relative_time as requested
  tidy$term <- as.character(tidy$relative_time)

  # Add confidence intervals
  conf.level <- sort(unique(conf.level))
  for (cl in conf.level) {
    z <- stats::qnorm(1 - (1 - cl) / 2)
    suf <- sprintf("%.0f", cl * 100)
    tidy[[paste0("conf_low_", suf)]] <- tidy$estimate - z * tidy$std.error
    tidy[[paste0("conf_high_", suf)]] <- tidy$estimate + z * tidy$std.error
  }

  # metadata
  # A unit counts as treated when it has treatment = 1 on a row with a known
  # event time; staggered units with timing = NA are controls.
  treated_row <- ..k_known & !is.na(tx) & tx == 1L
  if (!is.null(unit_chr)) {
    uv <- data[[unit_chr]]
    N_units <- length(unique(uv))
    N_treat <- length(unique(uv[treated_row]))
    N_never <- length(setdiff(unique(uv), unique(uv[treated_row])))
  } else {
    N_units <- N_treat <- N_never <- NA_integer_
  }

  attr(tidy, "lead_range") <- lead_range
  attr(tidy, "lag_range") <- lag_range
  attr(tidy, "baseline") <- baseline
  attr(tidy, "interval") <- interval
  attr(tidy, "ref_time") <- if (staggered) {
    .es_ref_time(data[[timing_chr]])
  } else if (!use_rel_time) {
    timing_val
  } else {
    NULL
  }
  attr(tidy, "time_levels") <- sort(unique(data[[time_chr]]))
  attr(tidy, "time_var") <- time_var
  attr(tidy, "unit_var") <- unit_chr
  attr(tidy, "time_transform") <- isTRUE(time_transform)
  attr(tidy, "call") <- match.call()
  attr(tidy, "model_formula") <- formula_string
  attr(tidy, "conf.level") <- conf.level
  attr(tidy, "N") <- fit_c$nobs
  attr(tidy, "N_units") <- N_units
  attr(tidy, "N_treated") <- N_treat
  attr(tidy, "N_nevertreated") <- N_never
  attr(tidy, "fe") <- fe_rhs_text
  attr(tidy, "vcov_type") <- if (!is.null(cluster) && identical(vcov, "HC1")) "cluster" else vcov
  attr(tidy, "cluster_vars") <- if (inherits(cluster, "formula")) {
    rlang::expr_text(rlang::f_rhs(cluster))
  } else {
    cluster
  }
  attr(tidy, "staggered") <- staggered
  attr(tidy, "sunab_used") <- FALSE
  attr(tidy, "es_vcov") <- es_vcov
  attr(tidy, "estimator") <- estimator

  rownames(tidy) <- NULL
  class(tidy) <- c("es_result", "data.frame")
  tidy
}
