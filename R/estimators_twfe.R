# Classic TWFE event-study estimation on the internal FE-OLS engine.
#
# .run_twfe_classic() reproduces the previous fixest-based classic path of
# run_es(): the event-study design i(f, treatment, ref) is expanded into
# explicit interaction dummies (event columns first, covariates after, so
# keep-first collinearity dropping matches the feols formula order), the
# within-OLS fit and VCOV come from .fit_fe_ols(), and relative time is
# read directly off the dummy levels instead of being re-parsed from
# coefficient-name strings.
#
# Fixed-effects specifications that are not plain column sums (e.g.
# ~ id^year) and cluster specifications that cannot be resolved to value
# vectors keep the previous behavior through the fixest formula fallback.

# Expand the classic event-study design into a named dummy matrix plus the
# relative time of each column.
#
# f_vals is the i() factor (event time ..k for staggered designs, calendar
# time for universal timing); rows with NA in f_vals or treatment are set to
# NA so the engine drops them exactly like feols drops NA i() rows.
.build_twfe_design <- function(f_vals, tr_vals, ref, f_name, treatment_chr,
                               staggered_design, timing_val, interval) {
  lv <- sort(unique(f_vals))            # sort() drops NA
  lv <- lv[!(lv %in% ref)]
  if (length(lv) == 0L)
    stop("Model estimation failed: no event-time levels remain outside ",
         "the reference period.")

  X_ev <- matrix(0, length(f_vals), length(lv))
  for (j in seq_along(lv)) {
    sel <- !is.na(f_vals) & f_vals == lv[j]
    X_ev[sel, j] <- tr_vals[sel]
  }
  X_ev[is.na(f_vals) | is.na(tr_vals), ] <- NA_real_
  colnames(X_ev) <- paste0(f_name, "::", lv, ":", treatment_chr)

  rel_time <- if (staggered_design) {
    as.integer(lv)
  } else {
    as.integer(round((as.numeric(lv) - as.numeric(timing_val)) / interval))
  }

  list(X = X_ev, rel_time = rel_time)
}

# Covariate columns expanded with model.matrix conventions (factor dummies
# named as in feols); NA rows are preserved so the engine's estimation-sample
# handling sees them.
.build_cov_matrix <- function(cov_text, data) {
  mf <- stats::model.frame(stats::reformulate(cov_text), data,
                           na.action = stats::na.pass)
  cm <- stats::model.matrix(attr(mf, "terms"), mf)
  cm[, colnames(cm) != "(Intercept)", drop = FALSE]
}

# Resolve run_es-style weights (one-sided formula or numeric vector) to a
# numeric vector aligned with data rows; NULL passes through.
.resolve_weights_vec <- function(weights, data) {
  if (is.null(weights)) return(NULL)
  if (inherits(weights, "formula")) {
    return(as.numeric(eval(rlang::f_rhs(weights), data,
                           environment(weights))))
  }
  if (is.numeric(weights)) {
    if (length(weights) != nrow(data))
      stop("`weights` must have length nrow(data).")
    return(as.numeric(weights))
  }
  stop("`weights` must be a one-sided formula or a numeric vector.")
}

# Estimate the classic TWFE event study. Returns
# list(tidy, V_full, nobs, engine): `tidy` has term / estimate / std.error /
# statistic / p.value / relative_time (NA on covariate rows), with `term`
# still holding the coefficient names so the caller can build es_vcov before
# relabelling.
.run_twfe_classic <- function(data, outcome_chr, treatment_chr, time_chr,
                              fe_rhs_text, cov_text, cluster, weights,
                              staggered_design, timing_val, interval,
                              baseline, vcov, vcov_args) {
  if (staggered_design) {
    f_vals <- data$..k
    ref    <- as.integer(baseline)
    f_name <- "..k"
  } else {
    f_vals <- data[[time_chr]]
    ref    <- timing_val + baseline * interval
    f_name <- time_chr
  }

  des <- .build_twfe_design(f_vals, data[[treatment_chr]], ref, f_name,
                            treatment_chr, staggered_design, timing_val,
                            interval)
  X <- des$X
  if (nzchar(cov_text)) X <- cbind(X, .build_cov_matrix(cov_text, data))

  y_vals  <- rlang::eval_tidy(rlang::parse_expr(outcome_chr), data)
  fe_vars <- .parse_fe_list(fe_rhs_text, data)
  cl_vals <- .resolve_cluster_vals(cluster, data)

  fit <- tryCatch({
    if (isFALSE(fe_vars) || isFALSE(cl_vals)) {
      data$.es_X <- X
      fml <- if (nzchar(fe_rhs_text)) {
        paste0(outcome_chr, " ~ .es_X | ", fe_rhs_text)
      } else {
        paste0(outcome_chr, " ~ .es_X")
      }
      f <- .fit_fe_ols_formula(data, fml, cluster, vcov, vcov_args,
                               weights = weights)
      .strip_mat_prefix(f, ".es_X", colnames(X))
    } else {
      .fit_fe_ols(y_vals, X, fe_list = fe_vars, cluster_vals = cl_vals,
                  vcov_type = vcov, vcov_args = vcov_args,
                  weights = .resolve_weights_vec(weights, data))
    }
  }, error = function(e) {
    stop(
      "Model estimation failed: ", conditionMessage(e),
      "\nHint: Check for collinearity between FE and event dummies; ",
      "reconsider `lead_range`/`lag_range` or the granularity of your FE."
    )
  })

  td <- fit$tidy
  idx <- match(td$term, colnames(des$X))
  td$relative_time <- NA_integer_
  hit <- !is.na(idx)
  td$relative_time[hit] <- des$rel_time[idx[hit]]

  list(tidy = td, V_full = fit$V, nobs = fit$nobs, engine = fit$engine)
}
