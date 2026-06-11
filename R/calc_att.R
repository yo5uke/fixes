# --------------------------------------------------------------------------- #
#  Internal helpers (not exported)                                             #
# --------------------------------------------------------------------------- #

# Aggregate CS att_gt to a summary data.frame.
# Returns data.frame with columns: group, estimate, std_error.
# Formula:
#   simple:    theta = sum_g (n_g/n_treated) * mean_{t>=g} ATT(g,t)
#              SE^2  = sum_g (n_g/n_treated)^2 * sum_{t>=g} SE^2(g,t) / |T_g|^2
#   by_cohort: theta(g) = mean_{t>=g} ATT(g,t)
#              SE(g)^2  = sum_{t>=g} SE^2(g,t) / |T_g|^2
#   by_time:   theta(t) = sum_{g<=t} w(g,t)*ATT(g,t), w(g,t)=n_g/sum_{g'<=t}n_{g'}
#              SE(t)^2  = sum_{g<=t} w(g,t)^2 * SE^2(ATT(g,t))
# All three assume independence across cohorts (CS 2021).
.agg_cs <- function(cs_out, aggregation, conf.level) {
  att_gt       <- cs_out$att_gt
  cohort_sizes <- cs_out$cohort_sizes

  post <- att_gt[att_gt$t >= att_gt$g, ]   # relative_time >= 0

  if (aggregation == "simple") {
    # Weight each (g,t) pair by n_g (cohort size), then normalize.
    # This gives more weight to cohorts with more post-treatment periods,
    # matching did::aggte(type="simple") (CS 2021, weighted ATT across groups).
    w_vec     <- cohort_sizes[as.character(post$g)]
    total_w   <- sum(w_vec)
    w_norm    <- w_vec / total_w
    theta     <- sum(w_norm * post$estimate)
    var_theta <- sum(w_norm^2 * post$std_error^2)   # independence assumption
    result_df <- data.frame(
      group     = NA_real_,
      estimate  = theta,
      std_error = sqrt(var_theta),
      stringsAsFactors = FALSE
    )

  } else if (aggregation == "by_cohort") {
    # theta(g) = mean_{t>=g} ATT(g,t); SE(g) = sqrt(sum SE^2) / |T_g|
    g_lev <- sort(unique(post$g))
    g_f   <- factor(post$g, levels = g_lev)
    n_t_g <- tapply(post$std_error, g_f, length)
    result_df <- data.frame(
      group     = g_lev,
      estimate  = as.numeric(tapply(post$estimate, g_f, mean)),
      std_error = as.numeric(tapply(post$std_error, g_f, function(s) sqrt(sum(s^2)))) /
                  as.numeric(n_t_g),
      stringsAsFactors = FALSE
    )

  } else {  # by_time
    # theta(t) = sum_{g<=t} w(g,t) ATT(g,t), w(g,t) = n_g / sum_{g'<=t} n_{g'}
    t_lev     <- sort(unique(post$t))
    rows_list <- lapply(t_lev, function(t_val) {
      rows_t <- post[post$t == t_val, ]
      sz_t   <- cohort_sizes[as.character(rows_t$g)]
      w_t    <- sz_t / sum(sz_t)
      data.frame(
        group     = t_val,
        estimate  = sum(w_t * rows_t$estimate),
        std_error = sqrt(sum(w_t^2 * rows_t$std_error^2)),
        stringsAsFactors = FALSE
      )
    })
    result_df <- do.call(rbind, rows_list)
  }

  rownames(result_df) <- NULL
  result_df
}

# Aggregate BJS tau_it to a summary data.frame.
# SEs are approximate (naive sample SE of the unit-time effects).
.agg_bjs <- function(bjs_out, aggregation) {
  tau_it <- bjs_out$tau_it   # all treated (i,t) obs; tau_hat = Y_it - Y_hat_it(0)

  # Naive sample SE of a cell of unit-time effects: sd/sqrt(n) (0 if singleton).
  .cell_se <- function(x) {
    n <- length(x)
    if (n > 1L) stats::sd(x) / sqrt(n) else 0
  }

  if (aggregation == "simple") {
    tau <- tau_it$tau_hat
    result_df <- data.frame(
      group     = NA_real_,
      estimate  = mean(tau),
      std_error = .cell_se(tau),
      stringsAsFactors = FALSE
    )

  } else {  # by_cohort or by_time: group by cohort or calendar time
    key   <- if (aggregation == "by_cohort") tau_it$cohort else tau_it$time
    lev   <- sort(unique(key))
    key_f <- factor(key, levels = lev)
    result_df <- data.frame(
      group     = lev,
      estimate  = as.numeric(tapply(tau_it$tau_hat, key_f, mean)),
      std_error = as.numeric(tapply(tau_it$tau_hat, key_f, .cell_se)),
      stringsAsFactors = FALSE
    )
  }

  rownames(result_df) <- NULL
  result_df
}

# Rename std_error -> std.error, add t-stat, p-value, and CI columns.
.finalize_att <- function(df, conf.level) {
  names(df)[names(df) == "std_error"] <- "std.error"
  df$statistic <- ifelse(
    df$std.error > 0,
    df$estimate / df$std.error,
    NA_real_
  )
  df$p.value <- ifelse(
    !is.na(df$statistic),
    2 * stats::pnorm(-abs(df$statistic)),
    NA_real_
  )
  .add_ci_columns(df, sort(unique(conf.level)), se_col = "std.error")
}

# --------------------------------------------------------------------------- #

#' Compute ATT Aggregations for Staggered Adoption Designs
#'
#' Estimates average treatment effects on the treated (ATT) using either the
#' Callaway-Sant'Anna (2021) or Borusyak-Jaravel-Spiess (2024) estimator,
#' aggregated to a single summary effect (`"simple"`), per-cohort effects
#' (`"by_cohort"`), or per calendar-time effects (`"by_time"`).
#'
#' This function complements [run_es()]: use `run_es()` when you want a full
#' event-study curve (dynamic effects by relative time), and `calc_att()` when
#' you want aggregated ATT estimates that collapse the time dimension.
#'
#' @section Aggregation formulas (CS estimator):
#' - **simple**: \eqn{\theta = \sum_g (n_g/n_{treated}) \cdot \overline{ATT(g,\cdot)}}
#'   where \eqn{\overline{ATT(g,\cdot)}} is the mean over post-treatment periods.
#' - **by_cohort**: \eqn{\theta(g) = \overline{ATT(g,\cdot)}} per cohort.
#' - **by_time**: \eqn{\theta(t) = \sum_{g \le t} w(g,t) \cdot ATT(g,t)}
#'   with \eqn{w(g,t) = n_g / \sum_{g' \le t} n_{g'}}.
#'
#' @section Standard errors (BJS estimator):
#' BJS SEs are approximate (naive sample variance of unit-time effects).
#' Cluster-robust SEs for BJS aggregations are planned for a future release.
#'
#' @param data A data.frame containing panel data.
#' @param outcome Unquoted outcome variable (name or expression, e.g., `log(y)`).
#' @param treatment Unused; reserved for future use.
#' @param time Unquoted calendar time variable (numeric).
#' @param timing Unquoted column giving each unit's first treatment period
#'   (`NA` = never treated).
#' @param fe Ignored (CS and BJS absorb fixed effects internally).
#' @param covariates Ignored; reserved for future use.
#' @param cluster Ignored; reserved for future use.
#' @param weights Ignored; reserved for future use.
#' @param interval Numeric time spacing (default `1`; informational only).
#' @param time_transform Logical; if `TRUE`, creates consecutive integer time
#'   within unit via `dplyr::dense_rank()`. Requires `unit`.
#' @param unit Unquoted unit identifier (required).
#' @param estimator Estimation strategy: `"cs"` (Callaway-Sant'Anna 2021,
#'   default) or `"bjs"` (Borusyak-Jaravel-Spiess 2024).
#' @param aggregation Aggregation type: `"simple"` (overall ATT, default),
#'   `"by_cohort"` (one ATT per treatment cohort), or `"by_time"` (one ATT
#'   per calendar time period).
#' @param control_group For `estimator = "cs"`: comparison group,
#'   `"nevertreated"` (default) or `"notyettreated"`.
#' @param anticipation For `estimator = "cs"`: number of anticipation periods
#'   before treatment (non-negative integer, default `0L`).
#' @param conf.level Numeric confidence level(s) (default `0.95`). Multiple
#'   levels are supported, e.g., `c(0.90, 0.95)`.
#' @param vcov Ignored (SE is analytical for CS; approximate for BJS).
#' @param vcov_args Ignored.
#'
#' @return A `data.frame` of class `"att_result"` with columns:
#' \describe{
#'   \item{`group`}{Cohort or calendar time (`NA` for `"simple"`).}
#'   \item{`estimate`}{ATT point estimate.}
#'   \item{`std.error`}{Standard error.}
#'   \item{`statistic`}{t-statistic (`estimate / std.error`).}
#'   \item{`p.value`}{Two-sided p-value (normal approximation).}
#'   \item{`conf_low_XX`, `conf_high_XX`}{CI bounds for each `conf.level`.}
#' }
#'
#' Attributes: `aggregation`, `estimator`, `conf.level`, `N`, `N_units`,
#' `N_treated`, `N_nevertreated`, `control_group` (CS only), `att_gt` (CS
#' raw ATT(g,t) table), `tau_it` (BJS unit-time effects table).
#'
#' @seealso [run_es()] for event-study (dynamic) estimates.
#' @importFrom stats qnorm pnorm sd
#' @importFrom dplyr group_by mutate arrange dense_rank ungroup n_distinct
#' @importFrom rlang .data
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
  estimator     <- match.arg(estimator)
  aggregation   <- match.arg(aggregation)
  control_group <- match.arg(control_group)
  conf.level    <- sort(unique(conf.level))

  stopifnot(is.data.frame(data))
  if (!is.numeric(interval) || interval <= 0) {
    stop("`interval` must be positive.")
  }

  # ---- resolve column names --------------------------------------------------
  # NSE column resolution uses the shared `.resolve_col()` helper
  # (R/utils-internal.R); aliased locally so the call sites below stay unchanged.
  resolve_column <- .resolve_col

  outcome_chr <- resolve_column(rlang::enexpr(outcome), data, allow_call = TRUE)
  time_chr    <- resolve_column(rlang::enexpr(time),    data)
  timing_chr  <- resolve_column(rlang::enexpr(timing),  data)

  unit_expr <- rlang::enexpr(unit)
  if (is.null(unit_expr)) stop("`unit` is required for `calc_att()`.")
  unit_chr <- resolve_column(unit_expr, data)

  if (isTRUE(time_transform)) {
    data <- data |>
      dplyr::group_by(.data[[unit_chr]]) |>
      dplyr::arrange(.data[[time_chr]], .by_group = TRUE) |>
      dplyr::mutate(.time_index = dplyr::dense_rank(.data[[time_chr]])) |>
      dplyr::ungroup()
    time_chr <- ".time_index"
  }

  # ---- dispatch to estimator -------------------------------------------------
  if (estimator == "cs") {
    cs_out     <- .run_cs(data, outcome_chr, timing_chr, time_chr, unit_chr,
                          anticipation, control_group, conf.level)
    agg_df     <- .agg_cs(cs_out, aggregation, conf.level)
    n_treated  <- sum(cs_out$cohort_sizes)
    n_never    <- cs_out$n_never
    extra_attrs <- list(control_group = control_group,
                        att_gt        = cs_out$att_gt)

  } else {  # bjs
    bjs_out    <- .run_bjs(data, outcome_chr, timing_chr, time_chr, unit_chr,
                           clustervars = NULL, conf.level)
    agg_df     <- .agg_bjs(bjs_out, aggregation)
    n_treated  <- length(unique(bjs_out$tau_it$unit))
    n_never    <- bjs_out$n_never
    extra_attrs <- list(tau_it = bjs_out$tau_it)
  }

  agg_df <- .finalize_att(agg_df, conf.level)
  rownames(agg_df) <- NULL

  # ---- stamp class and attributes --------------------------------------------
  n_units <- dplyr::n_distinct(data[[unit_chr]])

  attr(agg_df, "aggregation")    <- aggregation
  attr(agg_df, "estimator")      <- estimator
  attr(agg_df, "conf.level")     <- conf.level
  attr(agg_df, "N")              <- nrow(data)
  attr(agg_df, "N_units")        <- n_units
  attr(agg_df, "N_treated")      <- n_treated
  attr(agg_df, "N_nevertreated") <- n_never
  for (nm in names(extra_attrs)) attr(agg_df, nm) <- extra_attrs[[nm]]

  class(agg_df) <- c("att_result", "data.frame")
  agg_df
}

#' @export
print.att_result <- function(x, digits = 3L, ...) {
  estimator   <- attr(x, "estimator")
  aggregation <- attr(x, "aggregation")
  N           <- attr(x, "N")
  N_units     <- attr(x, "N_units")
  N_treated   <- attr(x, "N_treated")

  agg_label <- switch(aggregation,
    simple    = "Simple (overall)",
    by_cohort = "By cohort",
    by_time   = "By calendar time"
  )
  cat(sprintf(
    "ATT Estimation  [estimator: %s | aggregation: %s]\n",
    toupper(estimator), agg_label
  ))
  cat(sprintf("N = %d obs | %d units | %d treated\n\n", N, N_units, N_treated))

  display <- as.data.frame(x)
  print(display, digits = digits, ...)
  invisible(x)
}
