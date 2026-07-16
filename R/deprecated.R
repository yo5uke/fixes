# Deprecated verb-style API (pre-1.0.0), kept as thin wrappers over the
# noun-style successors. Each wrapper rewrites its own call (so unquoted
# NSE arguments pass through unevaluated), renames modernized arguments,
# and restores the original `call` attribute for byte-level fidelity with
# pre-1.0.0 results.

#' Deprecated: run an event study
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `run_es()` was renamed to [event_study()] in fixes 1.0.0. It remains
#' fully functional (including `method = "sunab"`, which is superseded by
#' `estimator = "sa"`), but new code should use [event_study()].
#'
#' @param data,outcome,treatment,time,timing,fe,lead_range,lag_range Passed
#'   to [event_study()].
#' @param covariates,cluster,weights,baseline,interval,time_transform Passed
#'   to [event_study()].
#' @param rel_time,unit,staggered,estimator,control_group,anticipation Passed
#'   to [event_study()].
#' @param vcov,vcov_args,bootstrap,boot_seed,group,trends Passed to
#'   [event_study()].
#' @param method Either `"classic"` (default) or `"sunab"`. The `"sunab"`
#'   path (via `fixest::sunab()`, requires the optional \{fixest\} package)
#'   is superseded by `estimator = "sa"` and has no equivalent argument in
#'   [event_study()].
#' @param conf.level Renamed to `conf_level` in [event_study()].
#' @param B Renamed to `boot_reps` in [event_study()].
#' @param alpha Renamed to `boot_alpha` in [event_study()].
#'
#' @return An `es_result` object; see [event_study()].
#' @seealso [event_study()]
#' @keywords internal
#' @export
run_es <- function(
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
  staggered = FALSE,
  method = c("classic", "sunab"),
  estimator = c("twfe", "cs", "sa", "bjs", "twm", "flex"),
  control_group = c("nevertreated", "notyettreated"),
  anticipation = 0L,
  conf.level = 0.95,
  vcov = "HC1",
  vcov_args = list(),
  bootstrap = FALSE,
  B = 999L,
  alpha = 0.05,
  boot_seed = NULL,
  group = NULL,
  trends = FALSE
) {
  method <- match.arg(method)
  estimator <- match.arg(estimator)
  use_sunab <- identical(estimator, "twfe") && identical(method, "sunab")

  lifecycle::deprecate_warn(
    "1.0.0", "run_es()", "event_study()",
    details = if (use_sunab) {
      'Note: `method = "sunab"` is superseded by `estimator = "sa"`.'
    }
  )

  cl <- match.call()
  cl$method <- NULL

  if (use_sunab) {
    rl <- rlang::enexpr(rel_time)
    if (!is.null(rl) && !identical(rl, quote(NULL))) {
      stop("`rel_time` is not supported with `method = \"sunab\"`; ",
           "pass the pre-built event time with the default classic method.")
    }
    keep <- c("", "data", "outcome", "treatment", "time", "timing", "fe",
              "lead_range", "lag_range", "covariates", "cluster", "weights",
              "baseline", "interval", "time_transform", "unit", "conf.level",
              "vcov", "vcov_args")
    cl <- cl[names(cl) %in% keep]
    cl$staggered <- staggered
    cl[[1L]] <- .run_es_sunab_legacy
  } else {
    nm <- names(cl)
    nm[nm == "conf.level"] <- "conf_level"
    nm[nm == "B"] <- "boot_reps"
    nm[nm == "alpha"] <- "boot_alpha"
    names(cl) <- nm
    # run_es defaulted to a universal design; suppress event_study()'s
    # column-name inference unless the caller set `staggered` explicitly.
    cl$staggered <- staggered
    cl[[1L]] <- quote(fixes::event_study)
  }

  out <- eval(cl, parent.frame())
  attr(out, "call") <- match.call()
  out
}

#' Deprecated: calculate aggregated ATT
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `calc_att()` was renamed to [att()] in fixes 1.0.0. The arguments
#' `treatment`, `fe`, `covariates`, `cluster`, `weights`, `vcov`, and
#' `vcov_args` were never used and have no equivalent in [att()].
#'
#' @param data,outcome,time,timing,unit Passed to [att()].
#' @param estimator,aggregation,control_group,anticipation Passed to [att()].
#' @param interval,time_transform Passed to [att()].
#' @param conf.level Renamed to `conf_level` in [att()].
#' @param treatment,fe,covariates,cluster,weights,vcov,vcov_args Ignored
#'   (reserved arguments of the old interface; accepted and dropped).
#'
#' @return An `att_result` object; see [att()].
#' @seealso [att()]
#' @keywords internal
#' @export
calc_att <- function(
  data,
  outcome,
  treatment  = NULL,
  time,
  timing,
  fe         = NULL,
  covariates = NULL,
  cluster    = NULL,
  weights    = NULL,
  interval   = 1,
  time_transform = FALSE,
  unit       = NULL,
  estimator  = c("cs", "bjs"),
  aggregation = c("simple", "by_cohort", "by_time"),
  control_group = c("nevertreated", "notyettreated"),
  anticipation  = 0L,
  conf.level    = 0.95,
  vcov          = "HC1",
  vcov_args     = list()
) {
  lifecycle::deprecate_warn("1.0.0", "calc_att()", "att()")

  unit_expr <- rlang::enexpr(unit)
  if (rlang::is_missing(unit_expr) || is.null(unit_expr)) {
    stop("`unit` is required for `calc_att()`.")
  }

  cl <- match.call()
  for (a in intersect(c("treatment", "fe", "covariates", "cluster",
                        "weights", "vcov", "vcov_args"), names(cl))) {
    cl[[a]] <- NULL
  }
  names(cl)[names(cl) == "conf.level"] <- "conf_level"
  cl[[1L]] <- quote(fixes::att)
  eval(cl, parent.frame())
}

#' Deprecated: run a basic TWFE DiD model
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `run_did()` was renamed to [did()] in fixes 1.0.0.
#'
#' @param data,outcome,treatment,timing,fe,unit,time Passed to [did()].
#' @param covariates,cluster,weights,vcov,vcov_args Passed to [did()].
#' @param conf.level Renamed to `conf_level` in [did()].
#'
#' @return A `did_result` object; see [did()].
#' @seealso [did()]
#' @keywords internal
#' @export
run_did <- function(
  data,
  outcome,
  treatment,
  timing      = NULL,
  fe          = NULL,
  unit        = NULL,
  time        = NULL,
  covariates  = NULL,
  cluster     = NULL,
  weights     = NULL,
  conf.level  = 0.95,
  vcov        = "HC1",
  vcov_args   = list()
) {
  lifecycle::deprecate_warn("1.0.0", "run_did()", "did()")
  cl <- match.call()
  names(cl)[names(cl) == "conf.level"] <- "conf_level"
  cl[[1L]] <- quote(fixes::did)
  out <- eval(cl, parent.frame())
  attr(out, "call") <- match.call()
  out
}

#' Deprecated: plot an event-study result
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `plot_es()` is deprecated as of fixes 1.0.0: call
#' [plot()][plot.es_result] on the result directly.
#'
#' @param data An `es_result` object.
#' @param ... Passed to the plotting engine; see [plot.es_result()].
#'
#' @return A `ggplot` object.
#' @seealso [plot.es_result()]
#' @keywords internal
#' @export
plot_es <- function(data, ...) {
  lifecycle::deprecate_warn("1.0.0", "plot_es()", "plot()")
  .plot_es_impl(data, ...)
}

#' Deprecated: interactive event-study plot
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `plot_es_interactive()` is deprecated as of fixes 1.0.0: use
#' `plot(x, interactive = TRUE)` instead; see [plot.es_result()].
#'
#' @param data An `es_result` object.
#' @param ... Passed to the plotting engine; see [plot.es_result()].
#'
#' @return A `plotly` object.
#' @seealso [plot.es_result()]
#' @keywords internal
#' @export
plot_es_interactive <- function(data, ...) {
  lifecycle::deprecate_warn(
    "1.0.0", "plot_es_interactive()", "plot()",
    details = "Use plot(x, interactive = TRUE)."
  )
  .plot_es_interactive_impl(data, ...)
}

#' Deprecated: plot the ATT(g,t) matrix
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `plot_att_gt()` is deprecated as of fixes 1.0.0: extract the table with
#' [att_gt()] and call [plot()][plot.att_gt_result] on it.
#'
#' @param x An `es_result` from a CS fit, or an `att_gt_result`.
#' @param ... Passed to the plotting engine; see [plot.att_gt_result()].
#'
#' @return A `ggplot` object.
#' @seealso [att_gt()], [plot.att_gt_result()]
#' @keywords internal
#' @export
plot_att_gt <- function(x, ...) {
  lifecycle::deprecate_warn(
    "1.0.0", "plot_att_gt()", "plot()",
    details = "Use plot(att_gt(x))."
  )
  .plot_att_gt_impl(x, ...)
}

#' Deprecated: plot a honest sensitivity analysis
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `plot_honest()` is deprecated as of fixes 1.0.0: call
#' [plot()][plot.honest_result] on the result directly.
#'
#' @param x A `honest_result` object.
#' @param ... Passed to the plotting engine; see [plot.honest_result()].
#'
#' @return A `ggplot` object.
#' @seealso [plot.honest_result()]
#' @keywords internal
#' @export
plot_honest <- function(x, ...) {
  lifecycle::deprecate_warn("1.0.0", "plot_honest()", "plot()")
  .plot_honest_impl(x, ...)
}

#' Deprecated: compute SA contamination weights
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `compute_contamination_weights()` was renamed to
#' [contamination_weights()] in fixes 1.0.0.
#'
#' @param data,time,timing,unit,fe,baseline Passed to
#'   [contamination_weights()].
#'
#' @return An `sa_contamination_weights` object; see
#'   [contamination_weights()].
#' @seealso [contamination_weights()]
#' @keywords internal
#' @export
compute_contamination_weights <- function(
  data,
  time,
  timing,
  unit,
  fe = NULL,
  baseline = -1L
) {
  lifecycle::deprecate_warn(
    "1.0.0", "compute_contamination_weights()", "contamination_weights()"
  )
  cl <- match.call()
  cl[[1L]] <- quote(fixes::contamination_weights)
  eval(cl, parent.frame())
}

#' Deprecated: plot contamination weights
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `plot_contamination_weights()` is deprecated as of fixes 1.0.0: call
#' [plot()][plot.sa_contamination_weights] on the result directly.
#'
#' @param x An `sa_contamination_weights` object.
#' @param ... Passed to the plotting engine; see
#'   [plot.sa_contamination_weights()].
#'
#' @return A `ggplot` object.
#' @seealso [plot.sa_contamination_weights()]
#' @keywords internal
#' @export
plot_contamination_weights <- function(x, ...) {
  lifecycle::deprecate_warn(
    "1.0.0", "plot_contamination_weights()", "plot()"
  )
  plot.sa_contamination_weights(x, ...)
}
