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

#' Event Study Estimation for Panel Data
#'
#' Runs an event study regression on panel data, supporting both classic (universal timing) and staggered (unit-varying timing via `sunab`).
#' The function builds the design (lead/lag factor or `sunab`), estimates with [fixest::feols()], and returns a tidy table with metadata.
#'
#' @section Key Features:
#' - One-step event study: specify outcome, treatment, time, timing, fixed effects, and (optionally) covariates.
#' - Switch between Classic (factor expansion) and Staggered-SAFE (`method = "sunab"`).
#' - Flexible clustering, weights, and VCOV choices (e.g., `vcov = "HC1" | "HC3" | "CR2" | "iid" ...`).
#' - Automatic lead/lag window detection and customizable baseline period.
#' - Returns an `"es_result"` object compatible with `print()` and `autoplot()`.
#'
#' @param data A data.frame containing panel data.
#' @param outcome Unquoted outcome (name or expression, e.g., `log(y)`).
#' @param treatment Unquoted treatment indicator (0/1 or logical). Used only when `method = "classic"`.
#' @param time Unquoted time variable (numeric or Date).
#' @param timing For `classic`: a numeric/Date (universal) or a variable (unquoted) if `staggered = TRUE`. For `sunab`: an unquoted variable with adoption time. For `estimator = "cs"`: unquoted column giving each unit's first treatment period (`NA` = never treated).
#' @param fe One-sided fixed-effects formula, e.g., `~ id + year`. Can be `NULL` for no fixed effects. Ignored when `estimator = "cs"`.
#' @param lead_range,lag_range Integers for pre/post windows. If `NULL`, determined automatically.
#' @param covariates One-sided formula of additional controls, e.g., `~ x1 + log(x2)`.
#' @param cluster Cluster specification (one-sided formula like `~ id + year`, a single character column name, or a vector of length `nrow(data)`).
#' @param weights Observation weights (a name/one-sided formula or a numeric vector of length `nrow(data)`).
#' @param baseline Integer baseline period (default `-1`); reference period excluded from results for both `"classic"` and `"sunab"` methods.
#' @param interval Numeric spacing of the time variable (default `1`; ignored internally for Dates).
#' @param time_transform Logical; if `TRUE`, creates consecutive integer time within unit.
#' @param unit Unit identifier variable (required when `estimator = "cs"` or `time_transform = TRUE`); also used for metadata when supplied.
#' @param staggered Logical; if `TRUE`, `timing` is a variable (classic) or is used by `sunab`.
#' @param method Either `"classic"` or `"sunab"` (default: `"classic"`).
#' @param estimator Estimation strategy: `"twfe"` (default, existing fixest-based paths),
#'   `"cs"` for the Callaway-Sant'Anna (2021) group-time ATT estimator, or
#'   `"sa"` for the Sun-Abraham (2021) interaction-weighted estimator.
#' @param control_group For `estimator = "cs"`: comparison group, either
#'   `"nevertreated"` (default) or `"notyettreated"`.
#' @param anticipation For `estimator = "cs"`: number of anticipation periods
#'   before treatment (non-negative integer, default `0L`).
#' @param conf.level Numeric vector of confidence levels (default `0.95`).
#' @param vcov VCOV type passed to `fixest::vcov()` or used via `broom::tidy(vcov = ...)`. Default `"HC1"`.
#' @param vcov_args List of additional arguments forwarded to `fixest::vcov()`.
#' @param bootstrap Logical; if `TRUE` and `estimator = "cs"`, compute
#'   simultaneous confidence bands via the multiplier bootstrap (Algorithm 1,
#'   Callaway and Sant'Anna 2021).  Adds `conf_low_sim` and
#'   `conf_high_sim` columns to the result and stores the full (g,t)-level
#'   bootstrap object as `attr(result, "bootstrap")`.  Default `FALSE`.
#' @param B Integer number of bootstrap draws (default `999`).  Used only
#'   when `bootstrap = TRUE` and `estimator = "cs"`.
#' @param alpha Significance level for the simultaneous band (default `0.05`).
#'   Note: this is independent of `conf.level`, which governs the pointwise
#'   delta-method CIs.
#' @param boot_seed Integer seed for the bootstrap RNG; `NULL` (default)
#'   does not set a seed.  Pass an integer for reproducible results.
#' @param group Unquoted group identifier for `estimator = "flex"` only.
#'   Identifies which treatment group (cohort) each observation belongs to in a
#'   repeated cross-section design (\eqn{R_{ig}} in Deb et al. 2024).  Each
#'   group must map to exactly one value of `timing` (or `NA` for
#'   never-treated groups).  Not used by other estimators.
#' @param trends Logical; for `estimator = "twm"` only.  When `TRUE`,
#'   adds cohort-specific linear trend regressors \eqn{d_g \cdot t} to the
#'   Procedure 5.1 regression (Wooldridge 2025, Section 8), allowing each
#'   cohort's counterfactual trend to deviate linearly from the common time
#'   trend.  Requires at least 2 pre-treatment periods per cohort.
#'   Default `FALSE`.
#'
#' @return A `data.frame` of class `"es_result"` with columns:
#' - `term`, `estimate`, `std.error`, `statistic`, `p.value`
#' - `conf_low_XX`, `conf_high_XX` (for each requested `conf.level`)
#' - `relative_time` (integer; 0 = event), `is_baseline` (logical; marks the reference period)
#'
#' Attributes include: `lead_range`, `lag_range`, `baseline`, `interval`, `call`, `model_formula`, `conf.level`,
#' `N`, `N_units`, `N_treated`, `N_nevertreated`, `fe`, `vcov_type`, `cluster_vars`, `staggered`, `sunab_used`.
#'
#' @importFrom stats as.formula qnorm setNames vcov
#' @importFrom dplyr bind_rows mutate arrange filter left_join group_by ungroup n_distinct
#' @importFrom rlang .data
#' @importFrom utils getFromNamespace
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
  stopifnot(is.data.frame(data))
  if (!is.numeric(interval) || interval <= 0) {
    stop("`interval` must be positive.")
  }

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

  unit_chr <- NULL
  unit_expr <- rlang::enexpr(unit)
  if (!is.null(unit_expr)) {
    unit_chr <- resolve_column(unit_expr, data)
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
    conf.level <- sort(unique(conf.level))

    cs_out <- .run_cs(
      data = data,
      outcome_chr = outcome_chr,
      timing_chr = timing_chr,
      time_chr = time_chr2,
      unit_chr = unit_chr,
      anticipation = anticipation,
      control_group = control_group,
      conf.level = conf.level
    )

    n_units <- dplyr::n_distinct(data[[unit_chr]])
    tidy <- .make_tidy(cs_out$es, conf.level)
    tidy <- .es_finalize(
      tidy,
      baseline,
      conf.level,
      lead_range,
      lag_range,
      meta = list(
        interval = interval,
        call = match.call(),
        model_formula = "cs",
        N = nrow(data),
        N_units = n_units,
        N_treated = n_units - cs_out$n_never,
        N_nevertreated = cs_out$n_never,
        fe = "",
        vcov_type = "analytic",
        cluster_vars = NULL,
        staggered = TRUE,
        sunab_used = FALSE,
        cs_control_group = control_group,
        att_gt = cs_out$att_gt
      )
    )

    # ---- Multiplier bootstrap (Algorithm 1, CS 2021) -------------------------
    if (isTRUE(bootstrap)) {
      # 1. Unit-level influence functions at the (g,t) level
      infl <- .compute_influence_cs(
        data = data,
        att_gt = cs_out$att_gt,
        control_group = control_group,
        unit_chr = unit_chr,
        time_chr = time_chr2,
        timing_chr = timing_chr,
        outcome_chr = outcome_chr,
        att_col = "estimate",
        anticipation = anticipation
      )

      # 2. Aggregate psi to event-study level using cohort-size weights
      es_agg <- .aggregate_psi_es(
        psi_gt = infl$psi,
        gt_index = infl$gt_index,
        att_gt = cs_out$att_gt,
        cohort_sizes = cs_out$cohort_sizes,
        att_es = cs_out$es[, c("relative_time", "estimate", "std_error")]
      )

      # 3. Bootstrap at the event-study level → simultaneous CI over all ℓ
      boot_es <- .bootstrap_cs(
        psi = es_agg$psi_es,
        att_gt = es_agg$gt_es_idx,
        gt_index = es_agg$gt_es_idx,
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
        psi = infl$psi,
        att_gt = cs_out$att_gt,
        gt_index = infl$gt_index,
        B = as.integer(B),
        alpha = alpha,
        seed = boot_seed
      )
      attr(tidy, "bootstrap") <- boot_gt
      attr(tidy, "boot_alpha") <- alpha
    }

    class(tidy) <- c("es_result", "data.frame")
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

    conf.level <- sort(unique(conf.level))

    sa_out <- .run_sa(
      data = data,
      outcome_chr = outcome_chr,
      timing_chr = timing_chr,
      time_chr = time_chr,
      unit_chr = unit_chr,
      fe_str = fe_str,
      baseline = baseline,
      cluster = cluster,
      vcov_type = vcov,
      vcov_args = vcov_args,
      conf.level = conf.level
    )

    n_units <- dplyr::n_distinct(data[[unit_chr]])
    tidy <- .make_tidy(sa_out$es, conf.level)
    tidy <- .es_finalize(
      tidy,
      baseline,
      conf.level,
      lead_range,
      lag_range,
      meta = list(
        interval = interval,
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
      conf.level = conf.level
    )

    n_units <- dplyr::n_distinct(data[[unit_chr]])
    tidy <- .make_tidy(bjs_out$es, conf.level)
    tidy <- .es_finalize(
      tidy,
      baseline,
      conf.level,
      lead_range,
      lag_range,
      meta = list(
        interval = interval,
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
        tau_it = bjs_out$tau_it
      ),
      bjs_lead = TRUE
    )

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

    cov_chrs_twm <- if (!is.null(covariates)) all.vars(covariates) else NULL
    conf.level <- sort(unique(conf.level))

    twm_out <- .run_twm(
      data = data,
      outcome_chr = outcome_chr,
      timing_chr = timing_chr,
      time_chr = time_chr,
      unit_chr = unit_chr,
      fe_str = fe_str,
      baseline = baseline,
      trends = isTRUE(trends),
      covariate_chrs = cov_chrs_twm,
      cluster = cluster,
      vcov_type = vcov,
      vcov_args = vcov_args,
      conf.level = conf.level
    )

    n_units <- dplyr::n_distinct(data[[unit_chr]])
    tidy <- .make_tidy(twm_out$es, conf.level)
    tidy <- .es_finalize(
      tidy,
      baseline,
      conf.level,
      lead_range,
      lag_range,
      meta = list(
        interval = interval,
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

    cov_chrs_flex <- if (!is.null(covariates)) all.vars(covariates) else NULL
    conf.level <- sort(unique(conf.level))

    flex_out <- .run_flex(
      data = data,
      outcome_chr = outcome_chr,
      timing_chr = timing_chr,
      time_chr = time_chr,
      group_chr = group_chr,
      baseline = baseline,
      covariate_chrs = cov_chrs_flex,
      cluster = cluster,
      vcov_type = vcov,
      vcov_args = vcov_args,
      conf.level = conf.level
    )

    n_groups <- dplyr::n_distinct(data[[group_chr]])
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

    return(tidy)
  }

  # ---- method = sunab ------------------------------------------------------
  if (method == "sunab") {
    if (!staggered) {
      warning("`method='sunab'` is typically used with `staggered=TRUE`.")
    }
    timing_chr <- resolve_column(rlang::enexpr(timing), data)

    # Get sunab function from fixest namespace and make it available in the formula environment
    # This ensures sunab is accessible when feols evaluates the formula
    sunab_fn <- getFromNamespace("sunab", "fixest")

    # Build formula as string (simpler approach that works with the injected function)
    rhs <- paste0("sunab(", timing_chr, ", ", time_chr, ")")
    if (nzchar(cov_text)) {
      rhs <- paste(rhs, cov_text, sep = " + ")
    }
    if (nzchar(fe_rhs_text)) {
      formula_string <- paste0(outcome_chr, " ~ ", rhs, " | ", fe_rhs_text)
    } else {
      formula_string <- paste0(outcome_chr, " ~ ", rhs)
    }
    model_formula <- stats::as.formula(formula_string)

    # Set the formula environment to include sunab
    formula_env <- new.env(parent = environment(model_formula))
    formula_env$sunab <- sunab_fn
    environment(model_formula) <- formula_env

    model_args <- list(model_formula, data = data)
    if (!is.null(cluster)) {
      model_args$cluster <- cluster
    }
    if (!is.null(weights)) {
      model_args$weights <- weights
    }

    model <- tryCatch(do.call(fixest::feols, model_args), error = function(e) {
      stop("Model estimation failed: ", e$message)
    })
    # vcov: when cluster is specified and vcov is the default "HC1", use the
    # model's clustered SE rather than silently overriding it with HC1.
    if (!is.null(cluster) && identical(vcov, "HC1")) {
      tidy <- broom::tidy(model)
    } else {
      V <- tryCatch(
        vcov(model, vcov = vcov, .vcov_args = vcov_args),
        error = function(e) NULL
      )
      tidy <- if (is.null(V)) {
        broom::tidy(model)
      } else {
        broom::tidy(model, vcov = V)
      }
    }

    # Full coefficient VCOV, retained for honest_sensitivity() downstream.
    V_full_es <- .model_vcov_full(model, vcov, cluster, vcov_args)

    # extract relative time from terms like "sunab::timing_var:: -2"
    rel <- suppressWarnings(as.integer(gsub(".*::(-?\\d+)$", "\\1", tidy$term)))
    tidy$relative_time <- rel
    tidy$is_baseline <- FALSE

    # Warn about any NA values in sunab event time terms only (not covariates)
    # Sunab terms should contain "::" (from sunab decomposition)
    terms_char <- as.character(tidy$term)
    is_sunab_term <- grepl("::", terms_char, fixed = TRUE)

    if (any(is.na(tidy$relative_time) & is_sunab_term)) {
      na_sunab_terms <- terms_char[is_sunab_term & is.na(tidy$relative_time)]
      if (length(na_sunab_terms) > 0) {
        warning(
          "Could not extract relative_time from sunab event time terms: ",
          paste(na_sunab_terms, collapse = ", ")
        )
      }
    }

    # Determine lead_range and lag_range
    if (is.null(lead_range)) {
      lead_range <- max(0L, abs(min(tidy$relative_time, na.rm = TRUE)))
    }
    if (is.null(lag_range)) {
      lag_range <- max(0L, max(tidy$relative_time, na.rm = TRUE))
    }

    # Filter results to specified ranges (before adding baseline)
    tidy <- tidy |>
      dplyr::filter(
        !is.na(.data$relative_time) &
          .data$relative_time >= -lead_range &
          .data$relative_time <= lag_range
      )

    # Add baseline row (0 estimate, 0 SE) for the dropped reference
    # Check if baseline is within the filtered range
    if (baseline >= -lead_range && baseline <= lag_range) {
      baseline_row <- tibble::tibble(
        term = as.character(baseline),
        estimate = 0,
        std.error = 0,
        statistic = NA_real_,
        p.value = NA_real_,
        relative_time = baseline
      )
      # Add baseline row if it doesn't already exist
      if (!baseline %in% tidy$relative_time) {
        tidy <- dplyr::bind_rows(tidy, baseline_row)
      }
    }

    # Mark baseline rows and arrange
    tidy$is_baseline <- tidy$relative_time == baseline
    tidy <- tidy |> dplyr::arrange(.data$relative_time)

    # Event-study coefficient VCOV (ordered by relative time) — built from the
    # original coefficient term names before they are relabelled below.
    es_vcov <- .build_es_vcov(
      V_full_es,
      tidy$term[!tidy$is_baseline],
      tidy$relative_time[!tidy$is_baseline]
    )

    # Update term column to show relative_time as numeric string
    tidy$term <- as.character(tidy$relative_time)

    # add CIs for requested levels
    conf.level <- sort(unique(conf.level))
    for (cl in conf.level) {
      z <- stats::qnorm(1 - (1 - cl) / 2)
      suf <- sprintf("%.0f", cl * 100)
      tidy[[paste0("conf_low_", suf)]] <- tidy$estimate - z * tidy$std.error
      tidy[[paste0("conf_high_", suf)]] <- tidy$estimate + z * tidy$std.error
    }

    # metadata
    N_units <- if (!is.null(unit_chr)) {
      dplyr::n_distinct(data[[unit_chr]])
    } else {
      NA_integer_
    }
    N_treat <- if (timing_chr %in% names(data)) {
      sum(!is.na(data[[timing_chr]]))
    } else {
      NA_integer_
    }

    attr(tidy, "lead_range") <- lead_range
    attr(tidy, "lag_range") <- lag_range
    attr(tidy, "baseline") <- baseline
    attr(tidy, "interval") <- interval
    attr(tidy, "call") <- match.call()
    attr(tidy, "model_formula") <- formula_string
    attr(tidy, "conf.level") <- conf.level
    attr(tidy, "N") <- stats::nobs(model)
    attr(tidy, "N_units") <- N_units
    attr(tidy, "N_treated") <- N_treat
    attr(tidy, "N_nevertreated") <- if (!is.na(N_units)) {
      N_units - N_treat
    } else {
      NA_integer_
    }
    attr(tidy, "fe") <- fe_rhs_text
    attr(tidy, "vcov_type") <- if (!is.null(cluster) && identical(vcov, "HC1")) "cluster" else vcov
    attr(tidy, "cluster_vars") <- if (inherits(cluster, "formula")) {
      rlang::expr_text(rlang::f_rhs(cluster))
    } else {
      cluster
    }
    attr(tidy, "staggered") <- staggered
    attr(tidy, "sunab_used") <- TRUE
    attr(tidy, "es_vcov") <- es_vcov

    class(tidy) <- c("es_result", "data.frame")
    return(tidy)
  }

  # ---- method = classic ----------------------------------------------------
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
  data[[treatment_chr]] <- tx

  # timing
  timing_val <- NULL
  timing_chr <- NULL
  if (staggered) {
    timing_chr <- resolve_column(rlang::enexpr(timing), data)
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
  if (staggered) {
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
  data$..rt <- rt
  if (all(is.na(data$..rt))) {
    stop("`relative_time` is NA for all rows. Check inputs.")
  }

  # integer event time
  data$..k <- suppressWarnings(as.integer(round(data$..rt)))

  # auto ranges (based on treated units only)
  k_treated <- data$..k[as.logical(data[[treatment_chr]])]
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

  # Build formula using i()
  # For staggered: use relative time (..k)
  # For non-staggered: use absolute time for i(), but will convert terms to relative time later
  if (staggered) {
    # Use i(..k, treatment, ref = baseline)
    i_formula <- paste0(
      "fixest::i(..k, ",
      treatment_chr,
      ", ref = ",
      baseline,
      ")"
    )
  } else {
    # Calculate the reference period based on baseline
    # E.g., if timing = 5 and baseline = -1, ref should be period 4
    if (inherits(timing_val, "Date")) {
      ref_period <- timing_val + baseline * interval
    } else {
      ref_period <- timing_val + baseline * interval
    }
    # Use i(time, treatment, ref = ref_period)
    i_formula <- paste0(
      "fixest::i(",
      time_chr,
      ", ",
      treatment_chr,
      ", ref = ",
      ref_period,
      ")"
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
  model_formula <- stats::as.formula(formula_string)

  model_args <- list(model_formula, data = data)
  if (!is.null(cluster)) {
    model_args$cluster <- cluster
  }
  if (!is.null(weights)) {
    model_args$weights <- weights
  }

  model <- tryCatch(do.call(fixest::feols, model_args), error = function(e) {
    msg <- e$message
    stop(
      "Model estimation failed: ",
      msg,
      "\nHint: Check for collinearity between FE and event dummies; reconsider `lead_range`/`lag_range` or the granularity of your FE."
    )
  })

  # vcov: when cluster is specified and vcov is the default "HC1", use the
  # model's clustered SE rather than silently overriding it with HC1.
  if (!is.null(cluster) && identical(vcov, "HC1")) {
    tidy <- broom::tidy(model)
  } else {
    V <- tryCatch(
      vcov(model, vcov = vcov, .vcov_args = vcov_args),
      error = function(e) NULL
    )
    tidy <- if (is.null(V)) broom::tidy(model) else broom::tidy(model, vcov = V)
  }

  # Full coefficient VCOV (same cluster/vcov precedence used to build `tidy`),
  # retained for honest_sensitivity() downstream.
  V_full_es <- .model_vcov_full(model, vcov, cluster, vcov_args)

  # Extract relative_time from i() terms - vectorized for performance
  # Format: "fixest::var::value:treatment" (3 parts) or "var::value" (2 parts)
  terms_char <- as.character(tidy$term)

  # Extract numeric values from term strings
  # Split by "::" and extract the LAST part (which contains "value:treatment" or just "value")
  parts_list <- strsplit(terms_char, "::", fixed = TRUE)
  value_parts <- sapply(parts_list, function(x) x[length(x)])

  # Extract numeric part (before any additional ":")
  numeric_parts <- sapply(
    strsplit(value_parts, ":", fixed = TRUE),
    function(x) x[1]
  )
  time_values <- suppressWarnings(as.numeric(numeric_parts))

  # Convert to relative time
  if (staggered) {
    # For staggered, ..k is already relative time
    tidy$relative_time <- as.integer(time_values)
  } else {
    # For non-staggered, convert absolute time to relative time
    tv <- if (inherits(timing_val, "Date")) {
      as.numeric(timing_val)
    } else {
      timing_val
    }
    tidy$relative_time <- as.integer(round((time_values - tv) / interval))
  }

  # Warn about any NA values in event time terms only (not covariates)
  # Event time terms should contain "::" (from fixest::i()) or be standalone numeric-like
  is_event_term <- grepl("::", terms_char, fixed = TRUE) |
    grepl("^[+-]?\\d+$", terms_char)

  if (any(is.na(tidy$relative_time) & is_event_term)) {
    na_event_terms <- terms_char[is_event_term & is.na(tidy$relative_time)]
    if (length(na_event_terms) > 0) {
      warning(
        "Could not extract relative_time from event time terms: ",
        paste(na_event_terms, collapse = ", ")
      )
    }
  }

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
  N_units <- if (!is.null(unit_chr)) {
    dplyr::n_distinct(data[[unit_chr]])
  } else {
    NA_integer_
  }
  N_treat <- sum(!is.na(data$..k))
  N_never <- if (!is.na(N_units) && !is.null(unit_chr)) {
    treated_by_unit <- tapply(!is.na(data$..k), data[[unit_chr]], any)
    sum(!treated_by_unit)
  } else {
    NA_integer_
  }

  attr(tidy, "lead_range") <- lead_range
  attr(tidy, "lag_range") <- lag_range
  attr(tidy, "baseline") <- baseline
  attr(tidy, "interval") <- interval
  attr(tidy, "call") <- match.call()
  attr(tidy, "model_formula") <- formula_string
  attr(tidy, "conf.level") <- conf.level
  attr(tidy, "N") <- stats::nobs(model)
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

  class(tidy) <- c("es_result", "data.frame")
  tidy
}
