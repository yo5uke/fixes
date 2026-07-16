# Legacy Sun-Abraham path via fixest::sunab(), reachable only through the
# deprecated run_es(method = "sunab"). New code uses estimator = "sa" (the
# package's own interaction-weighted implementation, validated against
# fixest::sunab() to 1e-6). Kept verbatim so existing scripts reproduce
# their previous numbers exactly; requires the optional {fixest} package.
#
# The prologue duplicates the input handling that run_es()/event_study()
# performed before dispatching into this branch.
.run_es_sunab_legacy <- function(
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
  conf.level = 0.95,
  vcov = "HC1",
  vcov_args = list()
) {
  .require_fixest(
    '`method = "sunab"`',
    paste0('use `estimator = "sa"`, the built-in Sun-Abraham ',
           "interaction-weighted estimator")
  )
  stopifnot(is.data.frame(data))
  if (!is.numeric(interval) || interval <= 0) {
    stop("`interval` must be positive.")
  }

  outcome_chr <- .resolve_col(rlang::enexpr(outcome), data, allow_call = TRUE)
  time_chr <- .resolve_col(rlang::enexpr(time), data)

  unit_chr <- NULL
  unit_expr <- rlang::enexpr(unit)
  if (!is.null(unit_expr) && !identical(unit_expr, quote(NULL))) {
    unit_chr <- .resolve_col(unit_expr, data)
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

  cov_text <- ""
  if (!is.null(covariates)) {
    if (!inherits(covariates, "formula")) {
      stop("`covariates` must be a one-sided formula (e.g., ~ x1 + log(x2)).")
    }
    cov_text <- rlang::expr_text(rlang::f_rhs(covariates))
  }

  fe_rhs_text <- ""
  if (!is.null(fe)) {
    if (!inherits(fe, "formula")) {
      stop("`fe` must be a one-sided formula, e.g., ~ id + year.")
    }
    fe_rhs_text <- rlang::expr_text(rlang::f_rhs(fe))
  }

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
      if (!is.null(cluster) && length(cluster) != nrow(data)) {
        stop("Vector `cluster` must be length nrow(data).")
      }
    }
  }

  if (!staggered) {
    warning("`method='sunab'` is typically used with `staggered=TRUE`.")
  }
  timing_chr <- .resolve_col(rlang::enexpr(timing), data)

  # fixest::sunab() has no NA-cohort convention: rows with an NA cohort are
  # dropped from the estimation sample, silently discarding the entire
  # never-treated control group.  Recode never-treated units to a cohort far
  # beyond the sample (the convention used by fixest::base_stagg), which
  # sunab treats as never-treated controls.
  n_na_timing_rows <- sum(is.na(data[[timing_chr]]))
  if (n_na_timing_rows > 0L) {
    never_code <- max(data[[time_chr]], na.rm = TRUE) + 10000
    data[[timing_chr]][is.na(data[[timing_chr]])] <- never_code
  }

  # Get sunab from the fixest namespace and make it available in the formula
  # environment so feols can evaluate the formula.
  sunab_fn <- getFromNamespace("sunab", "fixest")

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
      stats::vcov(model, vcov = vcov, .vcov_args = vcov_args),
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
  if (baseline >= -lead_range && baseline <= lag_range) {
    baseline_row <- tibble::tibble(
      term = as.character(baseline),
      estimate = 0,
      std.error = 0,
      statistic = NA_real_,
      p.value = NA_real_,
      relative_time = baseline
    )
    if (!baseline %in% tidy$relative_time) {
      tidy <- dplyr::bind_rows(tidy, baseline_row)
    }
  }

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
    # never-treated rows were recoded to `never_code` above, so count
    # them via the pre-recode NA tally.
    nrow(data) - n_na_timing_rows
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
  tidy
}
