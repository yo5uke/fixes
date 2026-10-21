# --------------------------------------------------------------------------- #
#  Internal helpers (not exported)                                             #
# --------------------------------------------------------------------------- #

# NSE column resolution is provided by the shared `.resolve_col()` helper in
# R/utils-internal.R.

# Extract cluster variable name(s) from formula/character/NULL.
.cluster_vars_did <- function(cluster) {
  if (is.null(cluster)) return(NULL)
  if (inherits(cluster, "formula")) {
    return(rlang::expr_text(rlang::f_rhs(cluster)))
  }
  if (is.character(cluster) && length(cluster) == 1L) {
    return(cluster)
  }
  NULL
}

# --------------------------------------------------------------------------- #
#  Main function                                                               #
# --------------------------------------------------------------------------- #

#' Basic two-way fixed-effects DiD estimation
#'
#' @description
#' Estimates a classic difference-in-differences model of the form
#' `outcome ~ D + covariates | fixed effects` on the package's internal
#' fixed-effects OLS engine (numerically equivalent to `fixest::feols()`).
#' The formula is the one used by [event_study()] and [att()]: swap the
#' function to get effects by event time or aggregated staggered ATTs.
#'
#' `did()` supersedes [run_did()] as of fixes 1.0.0.
#'
#' Treatment is given in one of three ways, as in [event_study()]:
#'
#' ```r
#' # D: 1 for treated units in post-treatment periods
#' did(y ~ D | id + year, data = df)
#'
#' # each unit's first treated period (NA = never treated)
#' did(y ~ 1 | id + year, data = df, time = year, timing = g)
#'
#' # a treated-group dummy and one treatment date
#' did(y ~ treated | id + year, data = df, time = year, timing = 2006)
#' ```
#' With `timing`, `D = treatment * (time >= timing)` is built internally.
#' For staggered-adoption settings see [att()]; for dynamic effects see
#' [event_study()].
#'
#' @param formula A model formula
#'   `outcome ~ treatment + covariates | fixed effects`. The left-hand side
#'   may be an expression (e.g. `log(y)`); without `|` the model has no
#'   fixed effects. The first right-hand-side term is D without `timing`,
#'   the treated-group dummy with a single `timing` date, and absent with a
#'   timing column (see **Description**). All variables must be columns of
#'   `data`.
#' @param data A data.frame (panel format). `formula` and `data` may come in
#'   either order.
#' @param time Unquoted time variable. Required with `timing`; with plain
#'   fixed effects such as `| id + year` it also lets `did()` report the
#'   unit (see [event_study()], **Panel identifiers**).
#' @param timing Treatment timing: an unquoted column giving each unit's
#'   first treatment period (`NA` = never treated), or a single numeric
#'   treatment date. Default `NULL`: the formula holds D.
#' @param unit Unquoted unit identifier column (for metadata). Taken from
#'   the fixed effects when possible.
#' @param cluster Clustering specification: a one-sided formula (`~ id`),
#'   a single character column name, or a numeric vector of length `nrow(data)`.
#'   When `cluster` is specified and `vcov` is the default `"HC1"`, cluster-robust
#'   standard errors are used automatically.
#' @param weights Observation weights (formula or numeric vector).
#' @param conf_level Confidence level(s) for CIs.  Scalar or vector
#'   (e.g., `c(0.90, 0.95)`).  Default `0.95`.
#' @param vcov VCOV type string passed to `fixest::vcov()`.  Default `"HC1"`.
#'   Ignored in favour of cluster-robust SE when `cluster` is supplied and
#'   `vcov` is left at its default `"HC1"`.
#' @param vcov_args Named list of additional arguments forwarded to
#'   `fixest::vcov()`.
#' @param outcome,treatment,fe,covariates `r lifecycle::badge("deprecated")`
#'   The model arguments of the older interface: the unquoted outcome, the
#'   treatment column (D, or the treated-group dummy when `timing` is
#'   given), a one-sided fixed-effects formula (`NULL` with both `unit` and
#'   `time` gives `~ unit + time`), and a one-sided covariate formula. They
#'   still work in fixes 1.x, with a deprecation warning that shows the
#'   equivalent formula call; fixes 2.0.0 is planned to accept only
#'   `formula`. Name them when you use them: the second argument is now
#'   `data`.
#'
#' @return A `did_result` object (named list) with elements:
#'   \describe{
#'     \item{`estimates`}{Data frame with the treatment coefficient:
#'       `term`, `estimate`, `std.error`, `statistic`, `p.value`, and
#'       `conf_low_XX`/`conf_high_XX` for each `conf.level` entry.}
#'     \item{`coeftable`}{Data frame with all kept coefficients (treatment
#'       and covariates): `term`, `estimate`, `std.error`, `statistic`,
#'       `p.value`.}
#'     \item{`vcov`}{Full coefficient covariance matrix.}
#'     \item{`fit_stats`}{Model-level statistics (nobs, R-squareds, sigma,
#'       logLik, AIC, BIC) used by [broom::glance()].}
#'     \item{`df.t`}{Degrees of freedom of the t inference.}
#'   }
#'   Attributes: `call`, `formula_str`, `outcome`, `treatment`, `timing`,
#'   `fe`, `vcov_type`, `cluster_vars`, `conf.level`, `N`, `N_units`,
#'   `N_treated`, `unit`, `time` (and `timing_var` with a timing column).
#'   `N`, `N_units`, and `N_treated` describe the *estimation* sample (after
#'   rows with missing values are dropped), matching `broom::glance()`, not
#'   `nrow(data)`.
#'
#' @examples
#' \dontrun{
#' res <- did(y ~ D | id + year, data = df)
#'
#' # Built from a treated-group dummy and the treatment date
#' res <- did(y ~ treated | id + year, data = df, time = year, timing = 2006)
#'
#' # Covariates and cluster-robust SE
#' res <- did(y ~ D + x1 | id + year, data = df, cluster = ~ id)
#'
#' print(res)
#' broom::tidy(res)
#' broom::glance(res)
#' # modelsummary::modelsummary(res)
#' }
#'
#' @seealso [event_study()] for dynamic effects, [att()] for staggered
#'   aggregation, and [run_did()] for the deprecated verb-style predecessor.
#'
#' @importFrom rlang enexpr is_symbol as_string is_call expr_text f_rhs f_lhs
#' @importFrom broom tidy
#' @importFrom stats qnorm nobs as.formula vcov
#' @export
did <- function(
  formula,
  data,
  time       = NULL,
  timing     = NULL,
  unit       = NULL,
  cluster    = NULL,
  weights    = NULL,
  conf_level = 0.95,
  vcov       = "HC1",
  vcov_args  = list(),
  outcome,
  treatment,
  fe         = NULL,
  covariates = NULL
) {
  cl  <- match.call()
  env <- parent.frame()
  fd  <- .formula_and_data(rlang::enquo(formula), rlang::enquo(data), "did")
  model_args <- c("outcome", "treatment", "fe", "covariates")

  if (is.null(fd$formula)) {
    names(cl)[names(cl) == "formula"] <- "data"
    .deprecate_model_args(
      "did", "`outcome`, `treatment`, `fe` and `covariates`",
      .legacy_formula_call("did", cl, fd$data, env), env
    )
    return(.eval_impl(cl, "did", .did_impl, env))
  }
  if (any(model_args %in% names(cl))) {
    stop("Give the model either as `formula` or through `outcome`, ",
         "`treatment`, `fe` and `covariates`, not both.", call. = FALSE)
  }

  data <- fd$data
  p <- .parse_model_formula(fd$formula, data, "did")
  timing_s <- .timing_spec(cl$timing, data, env)
  time_chr <- .col_arg(cl$time, data)
  if (timing_s$kind != "none") .require_time(time_chr)
  panel <- .resolve_panel(time_chr, .col_arg(cl$unit, data), p$fe,
                          need_unit = FALSE)

  tr <- .formula_treatment(p, timing_s, FALSE, data, panel$unit, time_chr,
                           "did", derive_cohort = FALSE)
  treatment_chr <- tr$term
  timing_arg    <- if (tr$mode == "value") timing_s$value
  if (tr$mode == "cohort") {
    treatment_chr <- .fresh_col(data, ".did_D_it")
    g <- data[[timing_s$name]]
    data[[treatment_chr]] <- as.integer(!is.na(g) & data[[time_chr]] >= g)
  }

  new <- cl[!names(cl) %in% c("formula", model_args, "time", "timing", "unit")]
  new$outcome    <- p$lhs
  new$treatment  <- treatment_chr
  new$timing     <- timing_arg
  new$fe         <- p$fe
  new$unit       <- panel$unit
  new$time       <- time_chr
  new$covariates <- .covariate_formula(tr$covariates, p$env)
  new$.auto_fe   <- FALSE

  res <- .eval_impl(new, "did", .did_impl, env, data = data)
  if (tr$mode == "cohort") {
    res$estimates$term <- timing_s$name
    attr(res, "treatment")  <- timing_s$name
    attr(res, "timing_var") <- timing_s$name
  }
  attr(res, "call") <- .user_call(cl, fd$swapped)
  res
}

# The estimation behind did(), with the argument interface of run_did();
# run_did() and the legacy did() arguments call it directly. `.auto_fe`
# builds `~ unit + time` when `fe` is NULL (off for formulas, where no `|`
# means no fixed effects).
.did_impl <- function(
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
  conf_level  = 0.95,
  vcov        = "HC1",
  vcov_args   = list(),
  .auto_fe    = TRUE
) {
  # ---- input validation -------------------------------------------------------
  if (!is.data.frame(data)) stop("`data` must be a data.frame.")
  if (!is.numeric(conf_level) || any(conf_level <= 0) || any(conf_level >= 1)) {
    stop("`conf_level` must be numeric in (0, 1).")
  }
  conf.level <- sort(unique(conf_level))

  # ---- resolve column names (NSE) --------------------------------------------
  outcome_chr   <- .resolve_col(rlang::enexpr(outcome),   data, allow_call = TRUE)
  treatment_chr <- .resolve_col(rlang::enexpr(treatment), data, allow_call = FALSE)

  unit_expr <- rlang::enexpr(unit)
  unit_chr  <- if (!is.null(unit_expr) && !identical(unit_expr, quote(NULL))) {
    .resolve_col(unit_expr, data)
  } else NULL

  time_expr <- rlang::enexpr(time)
  time_chr  <- if (!is.null(time_expr) && !identical(time_expr, quote(NULL))) {
    .resolve_col(time_expr, data)
  } else NULL

  # ---- timing: auto-construct D_it -------------------------------------------
  # group_chr is set to the original treatment group column when timing is used,
  # so the estimates output labels correctly.
  group_chr <- NULL
  treat_vec <- NULL

  if (!is.null(timing)) {
    # --- validate timing argument
    if (!is.numeric(timing) || length(timing) != 1L) {
      stop("`timing` must be a single numeric value (treatment onset period).")
    }
    if (is.null(time_chr)) {
      stop("`time` must be provided when `timing` is specified.")
    }
    # --- validate treatment as a group indicator (binary, typically unit-constant)
    treat_g <- data[[treatment_chr]]
    if (is.logical(treat_g)) {
      treat_g <- as.integer(treat_g)
    } else if (!is.numeric(treat_g)) {
      stop("When `timing` is specified, `treatment` must be a binary (0/1) group indicator.")
    }
    uvals_g <- sort(unique(treat_g[!is.na(treat_g)]))
    if (!all(uvals_g %in% c(0L, 1L))) {
      stop("When `timing` is specified, `treatment` must be a binary (0/1) group indicator.")
    }
    # --- construct D_it = treatment_group * (time >= timing)
    d_temp <- ".did_D_it"
    while (d_temp %in% names(data)) d_temp <- paste0(d_temp, "_")
    treat_vec <- as.integer(treat_g == 1L & data[[time_chr]] >= timing)
    group_chr     <- treatment_chr   # remember original name for output
    treatment_chr <- d_temp          # regression uses the constructed D_it

  } else {
    # --- validate pre-built treatment indicator (D_it)
    treat_col <- data[[treatment_chr]]
    if (is.logical(treat_col)) {
      treat_vec <- as.integer(treat_col)
    } else if (!is.numeric(treat_col)) {
      stop("`treatment` column must be 0/1 numeric or logical.")
    } else {
      uvals <- sort(unique(treat_col[!is.na(treat_col)]))
      if (!all(uvals %in% c(0L, 1L))) {
        stop("`treatment` column must contain only 0 and 1 (binary indicator).")
      }
      treat_vec <- treat_col
    }
  }

  # ---- build fe_rhs_text -----------------------------------------------------
  if (!is.null(fe)) {
    if (!inherits(fe, "formula")) {
      stop("`fe` must be a one-sided formula, e.g., ~ id + year.")
    }
    if (!is.null(rlang::f_lhs(fe))) {
      stop("`fe` must be a one-sided formula (no LHS). Use e.g., ~ id + year.")
    }
    fe_rhs_text <- rlang::expr_text(rlang::f_rhs(fe))
  } else if (.auto_fe && !is.null(unit_chr) && !is.null(time_chr)) {
    fe_rhs_text <- paste(unit_chr, time_chr, sep = " + ")
  } else if (.auto_fe && (!is.null(unit_chr) || !is.null(time_chr))) {
    stop("Both `unit` and `time` must be provided to auto-construct `fe`.")
  } else {
    if (.auto_fe) message("No `fe` specified; estimating pooled OLS.")
    fe_rhs_text <- ""
  }

  # ---- build covariates text -------------------------------------------------
  cov_text <- ""
  if (!is.null(covariates)) {
    if (!inherits(covariates, "formula")) {
      stop("`covariates` must be a one-sided formula (e.g., ~ x1 + log(x2)).")
    }
    cov_text <- rlang::expr_text(rlang::f_rhs(covariates))
  }

  # ---- validate cluster argument ---------------------------------------------
  if (!is.null(cluster)) {
    if (is.character(cluster)) {
      if (length(cluster) == 1L) {
        if (!cluster %in% names(data)) {
          stop("`cluster` as character must be a column name in `data`.")
        }
      } else if (length(cluster) != nrow(data)) {
        stop("Character `cluster` must be length 1 (column name) or length nrow(data).")
      }
    } else if (!inherits(cluster, "formula")) {
      if (length(cluster) != nrow(data)) {
        stop("Vector `cluster` must have length nrow(data).")
      }
    }
  }

  # ---- construct model formula (metadata + fixest formula fallback) ----------
  rhs <- treatment_chr
  if (nzchar(cov_text)) rhs <- paste(rhs, cov_text, sep = " + ")
  if (nzchar(fe_rhs_text)) {
    formula_str <- paste0(outcome_chr, " ~ ", rhs, " | ", fe_rhs_text)
  } else {
    formula_str <- paste0(outcome_chr, " ~ ", rhs)
  }

  # ---- estimate on the internal FE-OLS engine ---------------------------------
  # The engine reproduces feols estimation-sample handling and its default
  # VCOV precedence: cluster with the default "HC1" gives cluster-robust SEs;
  # an explicit vcov type applies regardless of cluster. FE or cluster
  # specifications that are not plain columns keep the fixest formula path.
  X <- matrix(as.numeric(treat_vec), ncol = 1L,
              dimnames = list(NULL, treatment_chr))
  if (nzchar(cov_text)) X <- cbind(X, .build_cov_matrix(cov_text, data))

  y_vals  <- rlang::eval_tidy(rlang::parse_expr(outcome_chr), data)
  fe_vars <- .parse_fe_list(fe_rhs_text, data)
  cl_vals <- .resolve_cluster_vals(cluster, data)

  fit <- tryCatch({
    if (isFALSE(fe_vars) || isFALSE(cl_vals)) {
      data$.did_X <- X
      fml <- if (nzchar(fe_rhs_text)) {
        paste0(outcome_chr, " ~ .did_X | ", fe_rhs_text)
      } else {
        paste0(outcome_chr, " ~ .did_X")
      }
      f <- .fit_fe_ols_formula(data, fml, cluster, vcov, vcov_args,
                               weights = weights, keep_stats = TRUE)
      .strip_mat_prefix(f, ".did_X", colnames(X))
    } else {
      .fit_fe_ols(y_vals, X, fe_list = fe_vars, cluster_vals = cl_vals,
                  vcov_type = vcov, vcov_args = vcov_args,
                  weights = .resolve_weights_vec(weights, data),
                  keep_stats = TRUE)
    }
  }, error = function(e) {
    msg <- conditionMessage(e)
    if (grepl("collinear", msg, ignore.case = TRUE)) {
      stop(
        "Treatment variable was absorbed by fixed effects or is perfectly ",
        "collinear.\n", msg
      )
    }
    stop("DiD model failed: ", msg)
  })

  tidy_coef <- fit$tidy

  # ---- isolate treatment row --------------------------------------------------
  treat_idx <- which(tidy_coef$term == treatment_chr)
  if (length(treat_idx) == 0L) {
    stop(
      "Treatment variable was absorbed by fixed effects or is perfectly ",
      "collinear with other regressors.\n",
      "Hint: ensure D_it has within-unit (or within-time) variation after ",
      "partialling out the specified fixed effects."
    )
  }
  # Reconstruct as a plain data.frame to strip any tibble / vctrs attributes.
  # When timing was used, label the term with the original group variable name.
  term_label <- if (!is.null(group_chr)) group_chr else as.character(tidy_coef$term[treat_idx])
  treat_rows <- data.frame(
    term      = term_label,
    estimate  = as.numeric(tidy_coef$estimate[treat_idx]),
    std.error = as.numeric(tidy_coef$std.error[treat_idx]),
    statistic = as.numeric(tidy_coef$statistic[treat_idx]),
    p.value   = as.numeric(tidy_coef$p.value[treat_idx]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  # ---- add CI columns --------------------------------------------------------
  treat_rows <- .add_ci_columns(treat_rows, conf.level, se_col = "std.error")
  rownames(treat_rows) <- NULL

  # ---- metadata --------------------------------------------------------------
  # Report the *estimation* sample, not nrow(data): rows with NA in any model
  # variable (and singleton FE levels) are dropped, mirroring feols. The
  # engine returns the fitted-sample row indices directly.
  used_idx <- fit$used

  N         <- fit$nobs
  all_used  <- (N == nrow(data))
  N_units   <- if (!is.null(unit_chr)) {
    length(unique(if (all_used) data[[unit_chr]] else data[[unit_chr]][used_idx]))
  } else NA_integer_
  N_treated <- sum((if (all_used) treat_vec else treat_vec[used_idx]) == 1L, na.rm = TRUE)

  # vcov_type: when cluster is used with default vcov, the actual SE is
  # cluster-robust; reflect that in the metadata rather than showing "HC1".
  vcov_type_display <- if (!is.null(cluster) && identical(vcov, "HC1")) "cluster" else vcov

  # ---- build result ----------------------------------------------------------
  result <- list(
    estimates = treat_rows,
    coeftable = fit$tidy,
    vcov      = fit$V,
    fit_stats = fit$stats,
    df.t      = fit$df.t
  )
  attr(result, "call")         <- match.call()
  attr(result, "formula_str")  <- formula_str
  attr(result, "outcome")      <- outcome_chr
  attr(result, "treatment")    <- if (!is.null(group_chr)) group_chr else treatment_chr
  attr(result, "timing")       <- timing
  attr(result, "fe")           <- fe_rhs_text
  attr(result, "vcov_type")    <- vcov_type_display
  attr(result, "cluster_vars") <- .cluster_vars_did(cluster)
  attr(result, "conf.level")   <- conf.level
  attr(result, "N")            <- N
  attr(result, "N_units")      <- N_units
  attr(result, "N_treated")    <- N_treated
  attr(result, "unit")         <- unit_chr
  attr(result, "time")         <- time_chr
  class(result) <- "did_result"
  result
}

# --------------------------------------------------------------------------- #
#  S3 method: print                                                            #
# --------------------------------------------------------------------------- #

#' @export
print.did_result <- function(x, digits = 3L, ...) {
  fe      <- attr(x, "fe")
  vc      <- attr(x, "vcov_type")
  cl      <- attr(x, "cluster_vars")
  N       <- attr(x, "N")
  N_units <- attr(x, "N_units")
  N_treat <- attr(x, "N_treated")
  timing  <- attr(x, "timing")

  cat("DiD Estimation  [TWFE]\n")
  if (!is.na(N_units)) {
    cat(sprintf("N = %d obs | %d units | %d treated obs\n", N, N_units, N_treat))
  } else {
    cat(sprintf("N = %d obs | %d treated obs\n", N, N_treat))
  }
  unit_v <- attr(x, "unit")
  time_v <- attr(x, "time")
  if (!is.null(unit_v) || !is.null(time_v)) {
    cat(sprintf("Panel: unit = %s | time = %s\n",
                if (is.null(unit_v)) "-" else unit_v,
                if (is.null(time_v)) "-" else time_v))
  }
  if (!is.null(timing)) {
    cat(sprintf("Timing: %s (post = time >= %.10g)\n",
                attr(x, "treatment"), timing))
  } else if (!is.null(attr(x, "timing_var"))) {
    cat(sprintf("Timing: post = %s >= %s\n", time_v, attr(x, "timing_var")))
  }
  cat("FE:", if (nzchar(fe)) fe else "(none)", "\n")
  cat("VCOV:", vc, "| Cluster:",
      paste(if (is.null(cl)) "-" else cl, collapse = " + "), "\n\n")

  display_cols <- intersect(
    c("term", "estimate", "std.error", "statistic", "p.value"),
    names(x$estimates)
  )
  print(x$estimates[, display_cols, drop = FALSE], digits = digits, ...)
  invisible(x)
}
