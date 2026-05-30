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

#' Run a basic two-way fixed-effects DiD model
#'
#' @description
#' Estimates a classic difference-in-differences model of the form
#' `outcome ~ D_it | fe` using [fixest::feols()].
#'
#' There are two ways to supply the treatment indicator:
#'
#' **Option A — pre-built `D_it`** (maximum flexibility):
#' ```r
#' df$D <- as.integer(df$treated & df$year >= 2006)
#' run_did(df, outcome = y, treatment = D, fe = ~ id + year)
#' ```
#'
#' **Option B — timing-based construction** (convenience; consistent with
#' `run_es()` and `calc_att()`):
#' ```r
#' run_did(df, outcome = y, treatment = treated, time = year, timing = 2006,
#'         fe = ~ id + year)
#' ```
#' Here `treatment` is a binary group indicator (1 = treated unit, 0 = control),
#' `time` is the calendar-time variable, and `timing` is the scalar treatment
#' onset period.  Internally `D_it = treatment * (time >= timing)` is constructed
#' automatically.  For staggered-adoption settings use [calc_att()]; for dynamic
#' event-study estimates use [run_es()].
#'
#' @param data A data.frame (panel format).
#' @param outcome Unquoted outcome variable or expression (e.g., `log(y)`).
#' @param treatment Unquoted column name.  When `timing = NULL` (default): a
#'   pre-built binary `D_it` indicator (1 = treated unit-time, 0 = otherwise).
#'   When `timing` is provided: a binary group indicator (1 = treated unit,
#'   0 = control unit; constant within units).
#' @param timing Numeric scalar.  When provided, `D_it` is constructed as
#'   `treatment * (time >= timing)`.  Requires `time` to be specified.
#'   Default `NULL` (user supplies pre-built `D_it` via `treatment`).
#' @param fe One-sided formula specifying fixed effects, e.g. `~ id + year`.
#'   If `NULL` and both `unit` and `time` are supplied, `fe` is auto-inferred
#'   as `~ unit + time`.  If `NULL` and neither is supplied, a pooled OLS model
#'   is estimated (with a message).
#' @param unit Unquoted unit identifier column (for metadata and `fe`
#'   auto-inference).
#' @param time Unquoted time variable column.  Used for (a) `fe` auto-inference
#'   and (b) `D_it` construction when `timing` is provided.
#' @param covariates One-sided formula of additional controls, e.g. `~ x1 + x2`.
#' @param cluster Clustering specification: a one-sided formula (`~ id`),
#'   a single character column name, or a numeric vector of length `nrow(data)`.
#'   When `cluster` is specified and `vcov` is the default `"HC1"`, cluster-robust
#'   standard errors are used automatically.
#' @param weights Observation weights (formula or numeric vector).
#' @param conf.level Confidence level(s) for CIs.  Scalar or vector
#'   (e.g., `c(0.90, 0.95)`).  Default `0.95`.
#' @param vcov VCOV type string passed to `fixest::vcov()`.  Default `"HC1"`.
#'   Ignored in favour of cluster-robust SE when `cluster` is supplied and
#'   `vcov` is left at its default `"HC1"`.
#' @param vcov_args Named list of additional arguments forwarded to
#'   `fixest::vcov()`.
#'
#' @return A `did_result` object (named list) with elements:
#'   \describe{
#'     \item{`estimates`}{Data frame with the treatment coefficient:
#'       `term`, `estimate`, `std.error`, `statistic`, `p.value`, and
#'       `conf_low_XX`/`conf_high_XX` for each `conf.level` entry.}
#'     \item{`model`}{The underlying `fixest` model object.}
#'   }
#'   Attributes: `call`, `formula_str`, `outcome`, `treatment`, `timing`,
#'   `fe`, `vcov_type`, `cluster_vars`, `conf.level`, `N`, `N_units`,
#'   `N_treated`, `unit`, `time`.
#'
#' @examples
#' \dontrun{
#' # Option A: pre-built D_it
#' df$D <- as.integer(df$treated & df$year >= 2006)
#' res <- run_did(df, outcome = y, treatment = D, fe = ~ id + year)
#'
#' # Option B: timing-based construction
#' res <- run_did(df, outcome = y, treatment = treated, time = year,
#'                timing = 2006, fe = ~ id + year)
#'
#' # Cluster-robust SE
#' res <- run_did(df, outcome = y, treatment = D, fe = ~ id + year,
#'                cluster = ~ id)
#'
#' print(res)
#' broom::tidy(res)
#' broom::glance(res)
#' # modelsummary::modelsummary(res)
#' }
#'
#' @importFrom rlang enexpr is_symbol as_string is_call expr_text f_rhs f_lhs
#' @importFrom fixest feols
#' @importFrom broom tidy
#' @importFrom stats qnorm nobs as.formula vcov
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
  # ---- input validation -------------------------------------------------------
  if (!is.data.frame(data)) stop("`data` must be a data.frame.")
  if (!is.numeric(conf.level) || any(conf.level <= 0) || any(conf.level >= 1)) {
    stop("`conf.level` must be numeric in (0, 1).")
  }
  conf.level <- sort(unique(conf.level))

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
      data[[treatment_chr]] <- as.integer(treat_g)
      treat_g <- data[[treatment_chr]]
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
    data[[d_temp]] <- as.integer(
      data[[treatment_chr]] == 1L & data[[time_chr]] >= timing
    )
    group_chr     <- treatment_chr   # remember original name for output
    treatment_chr <- d_temp          # regression uses the constructed D_it

  } else {
    # --- validate pre-built treatment indicator (D_it)
    treat_col <- data[[treatment_chr]]
    if (is.logical(treat_col)) {
      data[[treatment_chr]] <- as.integer(treat_col)
    } else if (!is.numeric(treat_col)) {
      stop("`treatment` column must be 0/1 numeric or logical.")
    } else {
      uvals <- sort(unique(treat_col[!is.na(treat_col)]))
      if (!all(uvals %in% c(0L, 1L))) {
        stop("`treatment` column must contain only 0 and 1 (binary indicator).")
      }
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
  } else if (!is.null(unit_chr) && !is.null(time_chr)) {
    fe_rhs_text <- paste(unit_chr, time_chr, sep = " + ")
  } else if (!is.null(unit_chr) || !is.null(time_chr)) {
    stop("Both `unit` and `time` must be provided to auto-construct `fe`.")
  } else {
    message("No `fe` specified; estimating pooled OLS.")
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

  # ---- construct feols formula -----------------------------------------------
  rhs <- treatment_chr
  if (nzchar(cov_text)) rhs <- paste(rhs, cov_text, sep = " + ")
  if (nzchar(fe_rhs_text)) {
    formula_str <- paste0(outcome_chr, " ~ ", rhs, " | ", fe_rhs_text)
  } else {
    formula_str <- paste0(outcome_chr, " ~ ", rhs)
  }
  model_formula <- stats::as.formula(formula_str)

  # ---- call feols -------------------------------------------------------------
  model_args <- list(model_formula, data = data)
  if (!is.null(cluster)) model_args$cluster <- cluster
  if (!is.null(weights)) model_args$weights <- weights

  model <- tryCatch(
    do.call(fixest::feols, model_args),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("collinear", msg, ignore.case = TRUE)) {
        stop(
          "Treatment variable was absorbed by fixed effects or is perfectly ",
          "collinear.\n", msg
        )
      }
      stop("DiD model failed: ", msg)
    }
  )

  # ---- extract VCOV-adjusted tidy coefficients --------------------------------
  # When cluster is supplied and the user left vcov at its default "HC1", feols
  # already computed cluster-robust SEs.  Use them directly rather than
  # overriding with HC1.  When vcov was explicitly changed (e.g., "iid"),
  # apply it regardless of cluster.
  if (!is.null(cluster) && identical(vcov, "HC1")) {
    tidy_coef <- broom::tidy(model)
  } else {
    V <- tryCatch(
      stats::vcov(model, vcov = vcov, .vcov_args = vcov_args),
      error = function(e) NULL
    )
    tidy_coef <- if (is.null(V)) broom::tidy(model) else broom::tidy(model, vcov = V)
  }

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
  N         <- nrow(data)
  N_units   <- if (!is.null(unit_chr)) length(unique(data[[unit_chr]])) else NA_integer_
  N_treated <- sum(data[[treatment_chr]] == 1L, na.rm = TRUE)

  # vcov_type: when cluster is used with default vcov, the actual SE is
  # cluster-robust; reflect that in the metadata rather than showing "HC1".
  vcov_type_display <- if (!is.null(cluster) && identical(vcov, "HC1")) "cluster" else vcov

  # ---- build result ----------------------------------------------------------
  result <- list(estimates = treat_rows, model = model)
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
  if (!is.null(timing)) {
    cat(sprintf("Timing: %s (post = time >= %.10g)\n",
                attr(x, "treatment"), timing))
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
