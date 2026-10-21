# Formula interface shared by event_study(), did() and att().
#
# A model is written `outcome ~ treatment + covariates | fixed effects`.
# Treatment is given in exactly one of three ways:
#   - no `timing`:            the first right-hand-side term is D (1 for treated
#                             units in post-treatment periods);
#   - `timing` is a column:   the treatment timing (NA = never treated) defines
#                             treatment; every right-hand-side term is a
#                             covariate;
#   - `timing` is a value:    the first term is the treated-group dummy.

# ---- formula / data order ---------------------------------------------------

# Sort out the first two arguments. Returns list(formula, data, swapped);
# `formula` is NULL for a legacy call (model given through `outcome` etc.).
.formula_and_data <- function(formula_q, data_q, fn) {
  first <- if (rlang::quo_is_missing(formula_q)) NULL else
    tryCatch(rlang::eval_tidy(formula_q), error = function(e) e)

  if (inherits(first, "formula")) {
    if (rlang::quo_is_missing(data_q)) {
      stop("`data` is missing.", call. = FALSE)
    }
    return(list(formula = first, data = rlang::eval_tidy(data_q),
                swapped = FALSE))
  }

  if (is.null(first) || is.data.frame(first)) {
    if (rlang::quo_is_missing(data_q)) {
      return(list(formula = NULL, data = first, swapped = FALSE))
    }
    second <- tryCatch(rlang::eval_tidy(data_q), error = function(e) e)
    if (is.data.frame(first) && inherits(second, "formula")) {
      return(list(formula = second, data = first, swapped = TRUE))
    }
    if (is.null(first) && is.data.frame(second)) {
      return(list(formula = NULL, data = second, swapped = FALSE))
    }
  }

  stop(
    "`", fn, "()` takes a model formula first, e.g. ", fn,
    "(y ~ D | id + year, data = df", if (fn != "did") ", time = year", ").\n",
    "Arguments of the older interface must be named (outcome = y, ...).",
    call. = FALSE
  )
}

# The user's call with `formula` and `data` in their proper places.
.user_call <- function(cl, swapped) {
  if (swapped) {
    f <- cl$data
    cl$data <- cl$formula
    cl$formula <- f
  }
  cl
}

# Evaluate a rewritten call to an internal implementation, bound under the
# public function's name so errors read as calls to it.
.eval_impl <- function(cl, fn, impl, env, data = NULL) {
  e <- rlang::env(env)
  e[[fn]] <- impl
  if (!is.null(data)) {
    e$.fixes_data <- data
    cl$data <- quote(.fixes_data)
  }
  cl[[1L]] <- as.name(fn)
  eval(cl, e)
}

# ---- parsing ----------------------------------------------------------------

.split_plus <- function(e) {
  if (rlang::is_call(e, "+", n = 2L)) {
    return(c(.split_plus(e[[2L]]), .split_plus(e[[3L]])))
  }
  if (rlang::is_call(e, "+", n = 1L)) return(.split_plus(e[[2L]]))
  if (rlang::is_call(e, "-", n = 2L) &&
      (identical(e[[3L]], 1) || identical(e[[3L]], 0))) {
    return(.split_plus(e[[2L]]))
  }
  list(e)
}

.join_plus <- function(terms) Reduce(function(a, b) call("+", a, b), terms)

.check_formula_term <- function(term, fn) {
  if (identical(term, quote(.))) {
    stop("`.` (all remaining columns) is not supported in the formula; ",
         "list the covariates explicitly.", call. = FALSE)
  }
  if (rlang::is_call(term, "-")) {
    stop("`-` terms are not supported in the formula.", call. = FALSE)
  }
  if (is.numeric(term)) {
    stop("Numeric constants other than 1 and 0 are not supported in the ",
         "formula.", call. = FALSE)
  }
  fns <- setdiff(all.names(term), all.vars(term))
  if ("i" %in% fns) {
    if (fn == "event_study") {
      stop("event_study() builds the event-time dummies itself; put the ",
           "treatment in the formula instead of `i()`.", call. = FALSE)
    }
    stop("`i()` is not supported in ", fn, "(); use event_study() for ",
         "effects by event time.", call. = FALSE)
  }
  if ("sunab" %in% fns) {
    stop("`sunab()` is not supported; use event_study(estimator = \"sa\").",
         call. = FALSE)
  }
  if (any(c("sw", "csw", "sw0", "csw0", "mvsw") %in% fns)) {
    stop("Stepwise specifications (`sw()`, `csw()`) are not supported.",
         call. = FALSE)
  }
  if ("~" %in% fns) {
    stop("Only one `~` is allowed in the formula.", call. = FALSE)
  }
  if (any(startsWith(all.vars(term), ".."))) {
    stop("`..` variables are not supported in the formula.", call. = FALSE)
  }
  invisible(NULL)
}

# Parse `lhs ~ terms | fe`. Returns list(lhs, terms, fe) with `fe` a
# one-sided formula (or NULL).
.parse_model_formula <- function(formula, data, fn) {
  if (length(formula) != 3L) {
    stop("The model formula must be two-sided, e.g. y ~ D | id + year.",
         call. = FALSE)
  }
  lhs <- formula[[2L]]
  rhs <- formula[[3L]]
  fe  <- NULL
  if (rlang::is_call(rhs, "|")) {
    if (rlang::is_call(rhs[[2L]], "|") || rlang::is_call(rhs[[3L]], "|")) {
      stop("Only one `|` is allowed: ",
           "outcome ~ treatment + covariates | fixed effects.", call. = FALSE)
    }
    fe  <- rlang::new_formula(NULL, rhs[[3L]], env = environment(formula))
    rhs <- rhs[[2L]]
  }

  terms <- .split_plus(rhs)
  terms <- terms[!vapply(terms, function(t) {
    is.numeric(t) && length(t) == 1L && t %in% c(0, 1)
  }, logical(1L))]
  for (t in terms) .check_formula_term(t, fn)

  vars <- unique(c(all.vars(lhs), unlist(lapply(terms, all.vars)),
                   if (!is.null(fe)) all.vars(fe)))
  miss <- setdiff(vars, names(data))
  if (length(miss)) {
    stop("Column(s) not found in `data`: ",
         paste0("`", miss, "`", collapse = ", "), ".", call. = FALSE)
  }
  list(lhs = lhs, terms = terms, fe = fe, env = environment(formula))
}

# One-sided covariate formula from a list of terms (NULL when empty).
.covariate_formula <- function(terms, env) {
  if (!length(terms)) return(NULL)
  rlang::new_formula(NULL, .join_plus(terms), env = env)
}

# ---- panel identifiers ------------------------------------------------------

# NSE column argument (symbol or string) as a column name, or NULL.
.col_arg <- function(expr, data) {
  if (is.null(expr) || identical(expr, quote(NULL))) return(NULL)
  .resolve_col(expr, data)
}

# Fill in `unit` from the fixed effects when they are plain columns made of
# `time` and exactly one other column, and report what is used.
.resolve_panel <- function(time_chr, unit_chr, fe, need_unit) {
  inferred <- FALSE
  if (is.null(unit_chr) && !is.null(time_chr) && !is.null(fe)) {
    fe_terms <- .split_plus(rlang::f_rhs(fe))
    if (all(vapply(fe_terms, rlang::is_symbol, logical(1L)))) {
      nm <- vapply(fe_terms, rlang::as_string, character(1L))
      other <- setdiff(nm, time_chr)
      if (time_chr %in% nm && length(other) == 1L) {
        unit_chr <- other
        inferred <- TRUE
      }
    }
  }
  if (need_unit && is.null(unit_chr)) {
    stop(
      if (is.null(fe)) {
        "Pass `unit =` to name the column identifying the units (e.g. unit = id)."
      } else {
        paste0(
          "Could not tell which fixed effect identifies the units; pass ",
          "`unit =` (e.g. unit = id). The unit is read from the fixed ",
          "effects only when they are plain columns: `time` and exactly one ",
          "other."
        )
      },
      call. = FALSE
    )
  }
  if (inferred) {
    message("Panel: unit = ", unit_chr, ", time = ", time_chr,
            " (unit taken from the fixed effects; set `unit` to override).")
  }
  list(unit = unit_chr, time = time_chr)
}

.require_time <- function(time_chr) {
  if (is.null(time_chr)) {
    stop("`time` is required: name the column holding the time period, ",
         "e.g. time = year.", call. = FALSE)
  }
}

# ---- treatment --------------------------------------------------------------

# Classify `timing`: list(kind = "none" | "column" | "value", name, value).
.timing_spec <- function(expr, data, env) {
  if (is.null(expr) || identical(expr, quote(NULL))) {
    return(list(kind = "none"))
  }
  if (rlang::is_symbol(expr) && rlang::as_string(expr) %in% names(data)) {
    return(list(kind = "column", name = rlang::as_string(expr)))
  }
  if (is.character(expr) && length(expr) == 1L && expr %in% names(data)) {
    return(list(kind = "column", name = expr))
  }
  val <- tryCatch(eval(expr, env), error = function(e) {
    stop("`timing` must be a column of `data` or a single treatment date; ",
         "`", rlang::expr_text(expr), "` is neither.", call. = FALSE)
  })
  if (is.null(val)) return(list(kind = "none"))
  if (length(val) != 1L) {
    stop("`timing` must be a column of `data` or a single treatment date.",
         call. = FALSE)
  }
  list(kind = "value", value = val)
}

.fresh_col <- function(data, base) {
  nm <- base
  while (nm %in% names(data)) nm <- paste0(nm, "_")
  nm
}

.as_binary <- function(v, name) {
  if (is.logical(v)) v <- as.integer(v)
  if (!is.numeric(v) || any(!v %in% c(0, 1, NA))) {
    stop("`", name, "` must be a 0/1 (or logical) treatment indicator.",
         call. = FALSE)
  }
  v
}

.varies_within <- function(v, id) {
  ok <- !is.na(v) & !is.na(id)
  if (!any(ok)) return(FALSE)
  u  <- .dense_codes(id[ok])
  vv <- v[ok]
  any(vv != vv[match(seq_len(max(u)), u)][u])
}

# Dense rank of `t` within each `u` (the index used by time_transform = TRUE).
.within_unit_rank <- function(u, t) {
  out <- rep(NA_integer_, length(t))
  ok  <- which(!is.na(u) & !is.na(t))
  if (!length(ok)) return(out)
  uc <- .dense_codes(u)[ok]
  tn <- as.numeric(t)[ok]
  o  <- order(uc, tn)
  uo <- uc[o]
  to <- tn[o]
  n  <- length(o)
  new_unit <- c(TRUE, uo[-1L] != uo[-n])
  new_val  <- new_unit | c(TRUE, to[-1L] != to[-n])
  cs <- cumsum(new_val)
  out[ok[o]] <- as.integer(cs - cs[new_unit][cumsum(new_unit)] + 1L)
  out
}

# Treatment start of each row's unit read off a D column: the first period
# with D = 1 (NA for never-treated units). Treatment must stay on once it
# starts.
.cohort_from_d <- function(d, id, t, d_name, fn) {
  ok <- !is.na(id) & !is.na(t)
  uc <- .dense_codes(id)
  tn <- as.numeric(t)

  on <- which(ok & d == 1L)
  if (!length(on)) {
    stop("`", d_name, "` is never 1: the data contain no treated ",
         "observations.", call. = FALSE)
  }
  nu <- max(uc, na.rm = TRUE)
  on <- on[order(uc[on], tn[on])]
  first_on <- on[!duplicated(uc[on])]
  start_row <- rep(NA_integer_, nu)
  start_row[uc[first_on]] <- first_on
  start_t <- rep(NA_real_, nu)
  start_t[uc[first_on]] <- tn[first_on]

  off <- ok & d == 0L & !is.na(start_t[uc]) & tn > start_t[uc]
  off[is.na(off)] <- FALSE
  if (any(off)) {
    bad <- unique(id[off])
    shown <- paste(utils::head(bad, 5L), collapse = ", ")
    if (length(bad) > 5L) shown <- paste0(shown, ", ...")
    stop("`", d_name, "` switches back from 1 to 0 within unit(s) ", shown,
         ". ", fn, "() assumes treatment stays on once it starts ",
         "(staggered adoption).", call. = FALSE)
  }

  obs <- which(ok)
  obs <- obs[order(uc[obs], tn[obs])]
  first_obs <- obs[!duplicated(uc[obs])]
  first_t <- rep(NA_real_, nu)
  first_t[uc[first_obs]] <- tn[first_obs]

  always <- !is.na(start_t) & start_t == first_t
  if (all(always[!is.na(start_t)])) {
    stop("`", d_name, "` never switches from 0 to 1 within a unit, so it ",
         "looks like a treated-group dummy rather than D (1 for treated ",
         "units in post-treatment periods). Pass the treatment date as ",
         "`timing` (e.g. timing = 2005), or use a D column.", call. = FALSE)
  }
  if (any(always)) {
    warning(sum(always), " unit(s) have `", d_name, "` = 1 from their first ",
            "observed period; their treatment start is taken to be that ",
            "period.", call. = FALSE)
  }
  t[start_row[uc]]
}

# A timing column defines treatment by itself, so D or the treated-group
# dummy must not slip into the formula as covariates.
.reject_treatment_terms <- function(terms, data, timing_chr, time_chr) {
  g <- data[[timing_chr]]
  d_ref <- if (!is.null(time_chr)) {
    as.integer(!is.na(g) & data[[time_chr]] >= g)
  }
  g_ref <- as.integer(!is.na(g))
  same <- function(v, ref) {
    if (is.null(ref)) return(FALSE)
    if (is.logical(v)) v <- as.integer(v)
    if (!is.numeric(v)) return(FALSE)
    ok <- !is.na(v) & !is.na(ref)
    any(ok) && all(v[ok] == ref[ok])
  }
  for (term in terms) {
    if (!rlang::is_symbol(term)) next
    nm <- rlang::as_string(term)
    v  <- data[[nm]]
    if (same(v, d_ref) || same(v, g_ref)) {
      stop("Treatment is defined by `timing` (`", timing_chr, "`) here, so `",
           nm, "` must not appear in the formula (it would enter as a ",
           "covariate). Drop `", nm, "`, or drop `timing` if `", nm,
           "` is the treatment indicator D.", call. = FALSE)
    }
  }
  invisible(NULL)
}

# Read the treatment specification off the parsed formula. Returns
# list(mode, term, covariates, data, cohort): `term` is the treatment
# column (D or the treated-group dummy), `cohort` the per-row treatment
# start (NA = never treated) when `derive_cohort` is TRUE.
.formula_treatment <- function(p, timing, has_rel_time, data, id_chr,
                               time_chr, fn, derive_cohort = TRUE,
                               time_transform = FALSE) {
  mode <- if (has_rel_time) "rel_time" else
    switch(timing$kind, none = "d", column = "cohort", value = "value")
  terms <- p$terms
  out <- list(mode = mode, term = NULL, covariates = terms, data = data,
              cohort = NULL)

  if (mode == "cohort") {
    .reject_treatment_terms(terms, data, timing$name, time_chr)
    out$cohort <- data[[timing$name]]
    return(out)
  }

  if (!length(terms)) {
    stop(switch(mode,
      d = paste0(
        "Put the treatment indicator D (1 for treated units in ",
        "post-treatment periods) first on the right-hand side, e.g. ",
        "y ~ D | id + year. With a treatment-timing column instead, pass ",
        "it as `timing`."
      ),
      paste0(
        "Put the treated-group dummy first on the right-hand side, e.g. ",
        "y ~ treated | id + year."
      )
    ), call. = FALSE)
  }
  if (!rlang::is_symbol(terms[[1L]])) {
    stop("The treatment term must be a column name; got `",
         rlang::expr_text(terms[[1L]]), "`.", call. = FALSE)
  }
  term <- rlang::as_string(terms[[1L]])
  out$term <- term
  out$covariates <- terms[-1L]

  if (mode == "rel_time") return(out)

  if (mode == "value") {
    g01 <- .as_binary(data[[term]], term)
    if (!is.null(id_chr) && .varies_within(g01, data[[id_chr]])) {
      stop("`", term, "` varies within units, so it looks like a treatment ",
           "indicator D (1 for treated units in post-treatment periods) ",
           "rather than a treated-group dummy. If it is D, drop `timing`.",
           call. = FALSE)
    }
    if (derive_cohort) {
      if (anyNA(g01)) {
        stop("`", term, "` has missing values.", call. = FALSE)
      }
      co <- rep(timing$value, nrow(data))
      co[g01 != 1L] <- NA
      out$cohort <- co
    }
    return(out)
  }

  # mode == "d"
  d01 <- .as_binary(data[[term]], term)
  if (derive_cohort) {
    if (anyNA(d01)) {
      warning("Dropped ", sum(is.na(d01)), " row(s) with a missing `", term,
              "`.", call. = FALSE)
      keep <- !is.na(d01)
      data <- data[keep, , drop = FALSE]
      d01  <- d01[keep]
      out$data <- data
    }
    t_vals <- data[[time_chr]]
    if (isTRUE(time_transform)) {
      t_vals <- .within_unit_rank(data[[id_chr]], t_vals)
    }
    out$cohort <- .cohort_from_d(d01, data[[id_chr]], t_vals, term, fn)
  }
  out
}

# ---- estimator-specific checks ------------------------------------------------

.formula_covariates <- function(terms, estimator, env) {
  if (!length(terms)) return(NULL)
  labels <- vapply(terms, rlang::expr_text, character(1L))
  shown  <- paste0("`", labels, "`", collapse = ", ")
  if (estimator %in% c("cs", "sa", "bjs")) {
    stop("estimator = \"", estimator, "\" does not support covariates; ",
         "remove ", shown, " from the formula.", call. = FALSE)
  }
  .covariate_formula(terms, env)
}

# cs, bjs and flex absorb the panel effects themselves: fixed effects within
# them are accepted silently, anything else is ignored with a warning.
.formula_fe <- function(fe, estimator, keep) {
  if (is.null(fe) || !estimator %in% c("cs", "bjs", "flex")) return(fe)
  extra <- setdiff(all.vars(fe), keep)
  if (length(extra)) {
    warning("Fixed effects other than ", paste0("`", keep, "`",
                                                collapse = " and "),
            " are ignored by estimator = \"", estimator, "\": ",
            paste0("`", extra, "`", collapse = ", "), ".", call. = FALSE)
  }
  NULL
}

# ---- deprecation of the legacy model arguments --------------------------------

.deprecate_model_args <- function(fn, args, suggestion, user_env) {
  details <- c(i = paste0(
    "From fixes 2.0.0, ", fn, "() is planned to accept only a model formula ",
    "(current version: ", utils::packageVersion("fixes"), ")."
  ))
  if (!is.null(suggestion)) {
    details <- c(details, i = paste0("Equivalent call: ", suggestion))
  }
  lifecycle::deprecate_soft(
    "1.0.0",
    I(paste0("Specifying the model with ", args, " in `", fn, "()`")),
    details = details,
    user_env = user_env
  )
}

# The formula call equivalent to a legacy call `cl` (NULL if it cannot be
# built).
.legacy_formula_call <- function(fn, cl, data, env, estimator = NULL) {
  tryCatch(.build_formula_call(fn, cl, data, env, estimator),
           error = function(e) NULL)
}

.build_formula_call <- function(fn, cl, data, env, estimator) {
  lhs <- cl$outcome
  if (is.null(lhs)) return(NULL)
  tr <- cl$treatment
  if (identical(tr, quote(NULL))) tr <- NULL

  fe  <- if (!is.null(cl$fe)) eval(cl$fe, env)
  cov <- if (!is.null(cl$covariates)) eval(cl$covariates, env)
  covs <- if (inherits(cov, "formula")) .split_plus(rlang::f_rhs(cov)) else
    list()
  timing  <- .timing_spec(cl$timing, data, env)
  has_rel <- !is.null(cl$rel_time) && !identical(cl$rel_time, quote(NULL))

  keep_tr <- switch(fn,
    event_study = !is.null(tr) &&
      (has_rel || (identical(estimator, "twfe") && timing$kind == "value")),
    did = TRUE,
    FALSE
  )
  rhs_terms <- c(if (keep_tr) list(tr), covs)
  rhs <- if (length(rhs_terms)) .join_plus(rhs_terms) else 1

  fe_rhs <- if (inherits(fe, "formula")) rlang::f_rhs(fe)
  if (is.null(fe_rhs) && fn == "did" && !is.null(cl$unit) &&
      !is.null(cl$time)) {
    fe_rhs <- call("+", cl$unit, cl$time)
  }
  fml <- if (is.null(fe_rhs)) call("~", lhs, rhs) else
    call("~", lhs, call("|", rhs, fe_rhs))

  rest <- as.list(cl)[-1L]
  rest <- rest[!names(rest) %in%
                 c("outcome", "treatment", "fe", "covariates", "data",
                   "formula")]
  if (!is.null(rest$staggered)) {
    s <- eval(rest$staggered, env)
    if ((isTRUE(s) && timing$kind == "column") ||
        (isFALSE(s) && timing$kind == "value")) {
      rest$staggered <- NULL
    }
  }
  lead <- intersect(c("time", "timing", "unit"), names(rest))
  rest <- c(rest[lead], rest[setdiff(names(rest), lead)])

  new <- as.call(c(list(as.name(fn), fml, data = cl$data), rest))
  paste(trimws(deparse(new, width.cutoff = 500L)), collapse = " ")
}
