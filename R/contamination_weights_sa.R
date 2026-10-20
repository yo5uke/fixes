# ============================================================================
# contamination_weights_sa.R
#
# Diagnostic tools for quantifying the contamination of TWFE event-study
# coefficients by treatment effects from other periods / cohorts, following
# Sun and Abraham (2021) Section 3.5-3.6.
#
# Theory
# ------
# In a standard TWFE "fully dynamic" event-study regression:
#   Y_it = alpha_i + lambda_t + sum_{l in g_incl} mu_l * D^l_{i,t} + eps
#
# where D^l_{i,t} = 1{t - E_i = l} is the cohort-AGGREGATED relative-time
# indicator, the OVB formula decomposes each mu_l as a linear combination of
# the cohort-specific ATTs (CATTs):
#
#   mu_{l''} = sum_{(e, l)} omega^{l''}_{e,l} * CATT_{e,l}    ... Eq (20)
#
# The contamination weights omega^{l''}_{e,l} are obtained by regressing
# each cohort-specific indicator Z_{e,l}(i,t) = 1{E_i=e} * 1{t-E_i=l} on
# all cohort-AGGREGATED included indicators X_{l''}(i,t) = D^{l''}_{i,t},
# plus unit and time fixed effects (OVB auxiliary regression):
#
#   Z_{e,l}(i,t) = alpha_i + lambda_t + sum_{l''} omega^{l''}_{e,l} *
#                  X_{l''}(i,t) + u_{i,t}                       ... Eq (12)
#
# Key diagnostic properties (SA 2021, Proposition 1):
#   (i)  sum_e omega^l_{e,l}   = 1  (own-period cohort weights sum to 1)
#   (ii) sum_e omega^l_{e,l'}  = 0  for l' in g_incl, l' != l
#        (cross-period contamination sums to zero under homogeneity)
#   (iii) OVB identity: mu_l'' = sum_{(e,l)} omega^{l''}_{e,l} * CATT_{e,l}
#
# Public functions exported:
#   compute_contamination_weights()   -- estimate and return weight matrix
#   plot_contamination_weights()      -- ggplot2 tile heatmap
# ============================================================================

# Multi-response within-OLS solver for contamination_weights().
# Demeans [X | Z] jointly against the fixed effects in a single OpenMP pass,
# applies keep-first Cholesky dropping to X'X, and solves for all K_z response
# columns simultaneously.
.fit_multi_fe_ols <- function(Z, X, fe_list, collin_tol = 1e-10) {
  Q <- length(fe_list)
  n <- nrow(X)
  storage.mode(X) <- "double"
  storage.mode(Z) <- "double"

  ok <- rep(TRUE, n)
  x_ok <- row_no_na_cpp(X)
  if (length(x_ok) > 0L) ok <- ok & x_ok
  z_ok <- row_no_na_cpp(Z)
  if (length(z_ok) > 0L) ok <- ok & z_ok
  for (f in fe_list) if (anyNA(f)) ok <- ok & !is.na(f)

  if (Q > 0L) {
    fe_code <- lapply(fe_list, .dense_codes)
    repeat {
      idx <- which(ok)
      keep <- rep(TRUE, length(idx))
      for (q in seq_len(Q)) {
        cq  <- fe_code[[q]][idx]
        cnt <- tabulate(cq)
        keep <- keep & cnt[cq] != 1L
      }
      if (all(keep)) break
      ok[idx[!keep]] <- FALSE
    }
  }

  N <- sum(ok)
  if (N == 0L) stop("No observations remain after removing NA rows and singletons.")

  all_ok <- (N == n)
  Xk <- if (all_ok) X else X[ok, , drop = FALSE]
  Zk <- if (all_ok) Z else Z[ok, , drop = FALSE]
  nt <- .fes_nthreads()

  if (Q > 0L) {
    fe_f <- lapply(fe_code, function(code) {
      if (all_ok) code else .dense_codes(code[ok])
    })
    n_lev  <- vapply(fe_f, max, integer(1L))
    fe_ids <- matrix(0L, nrow = N, ncol = Q)
    for (q in seq_len(Q)) fe_ids[, q] <- fe_f[[q]] - 1L

    dm <- demean_kway_cpp(cbind(Xk, Zk), fe_ids, n_lev,
                          w = numeric(0), tol = 1e-12,
                          max_iter = 10000L, nthreads = nt)
    Md <- dm$M
    K_x <- ncol(Xk)
    x_names <- colnames(Xk)
    d_pre <- colSums(Xk^2)
  } else {
    Xd0 <- cbind(`(Intercept)` = 1, Xk)
    K_x <- ncol(Xd0)
    Md  <- cbind(Xd0, Zk)
    x_names <- colnames(Xd0)
    d_pre <- NULL
  }

  CP  <- crossprod_omp_cpp(Md, nt)
  XtX <- CP[seq_len(K_x), seq_len(K_x), drop = FALSE]
  XtZ <- CP[seq_len(K_x), (K_x + 1L):ncol(Md), drop = FALSE]
  if (is.null(d_pre)) d_pre <- diag(XtX)

  cd   <- .chol_seq_drop(XtX, d_pre = d_pre, tol = collin_tol)
  kept <- cd$kept
  if (length(kept) == 0L) stop("All regressors were dropped as collinear.")

  Rk <- cd$R
  B  <- backsolve(Rk, forwardsolve(t(Rk), XtZ[kept, , drop = FALSE]))
  rownames(B) <- x_names[kept]
  B
}

#' Contamination Weights for TWFE Event-Study Coefficients (Sun-Abraham 2021)
#'
#' @description
#' Estimates the contamination weights \eqn{\omega^{\ell''}_{e,\ell}} that
#' decompose each TWFE event-study coefficient \eqn{\hat\mu_{\ell''}} into a
#' linear combination of cohort-specific ATTs (CATTs):
#' \deqn{\hat\mu_{\ell''} = \sum_{(e,\ell)} \omega^{\ell''}_{e,\ell} \cdot
#'   \widehat{CATT}_{e,\ell}}
#' (Sun and Abraham 2021, Equation 20).
#'
#' The weights are obtained via the OVB auxiliary regression (Eq. 12): for
#' each cohort-period CATT cell \eqn{(e, \ell)}, regress the cohort-specific
#' indicator \eqn{1\{E_i = e\}\cdot 1\{t-E_i = \ell\}} on all cohort-aggregated
#' relative-time indicators \eqn{D^{\ell''}_{i,t}} and two-way fixed effects.
#' The resulting regression coefficient on \eqn{D^{\ell''}_{i,t}} is
#' \eqn{\omega^{\ell''}_{e,\ell}}.
#'
#' @section Interpretation:
#' - **Own-period cell** (`catt_period == twfe_period`):
#'   \eqn{\omega^{\ell}_{{e,\ell}}} represents the weight the TWFE estimator
#'   places on \eqn{CATT_{e,\ell}}. Under the SA IW estimator these equal
#'   the cohort-size weights \eqn{n_e / \sum n_{e'}}.
#' - **Cross-period cell** (`catt_period != twfe_period`):
#'   Any non-zero weight indicates contamination: the TWFE coefficient
#'   \eqn{\hat\mu_{\ell''}} also picks up treatment effects from period
#'   \eqn{\ell \ne \ell''}.
#' - **Verification**: the OVB identity (property iii) holds
#'   exactly, so \eqn{\hat\mu_{\ell''} = \sum_{(e,\ell)}
#'   \omega^{\ell''}_{e,\ell} \cdot \widehat{CATT}_{e,\ell}} up to
#'   floating-point precision.
#'
#' @param data A data.frame with one row per unit-period (balanced panel).
#' @param time Unquoted name of the calendar time variable (numeric).
#' @param timing Unquoted name of the first-treatment-period variable;
#'   `NA` marks never-treated units.
#' @param unit Unquoted name of the unit identifier.
#' @param fe One-sided fixed-effects formula, e.g. `~ id + year`.
#'   When `NULL` (default), falls back to `~ <unit> + <time>`
#'   with a warning.
#' @param baseline Integer reference (baseline) period excluded from the TWFE
#'   specification (default `-1L`).  Must match the `baseline`
#'   argument used in [event_study()].
#'
#' @return An object of class `c("sa_contamination_weights", "data.frame")`
#'   with one row per `(catt_cohort, catt_period, twfe_period)` triple,
#'   and columns:
#' \describe{
#'   \item{`catt_cohort`}{Cohort \eqn{e} (first treatment period).}
#'   \item{`catt_period`}{Relative event time \eqn{\ell} of the CATT.}
#'   \item{`twfe_period`}{Relative event time \eqn{\ell''} of the TWFE
#'     coefficient being decomposed.}
#'   \item{`weight`}{Contamination weight \eqn{\omega^{\ell''}_{e,\ell}}.}
#'   \item{`is_own`}{Logical; `TRUE` when `catt_period == twfe_period`.}
#' }
#' Attributes: `baseline`, `cohorts`, `cohort_sizes`, `incl_periods`.
#'
#' @seealso [plot.sa_contamination_weights()], [event_study()], and
#'   [compute_contamination_weights()] for the deprecated predecessor.
#'
#' @references
#' Sun, L. and Abraham, S. (2021). Estimating dynamic treatment effects in
#' event studies with heterogeneous treatment effects.
#' *Journal of Econometrics*, 225(2), 175--199.
#'
#' @examples
#' \dontrun{
#' # Estimate contamination weights
#' cw <- contamination_weights(
#'   data     = panel_data,
#'   time     = year,
#'   timing   = first_treat,
#'   unit     = id,
#'   fe       = ~ id + year,
#'   baseline = -1L
#' )
#' print(cw)
#' plot(cw)
#' }
#'
#' @export
contamination_weights <- function(
  data,
  time,
  timing,
  unit,
  fe = NULL,
  baseline = -1L
) {
  baseline <- as.integer(baseline)

  # ---- NSE: resolve column names -------------------------------------------
  .resolve_cw <- function(expr, data) {
    if (rlang::is_symbol(expr)) {
      var <- rlang::as_string(expr)
      if (!var %in% names(data)) {
        stop("Column '", var, "' not found in data.")
      }
      return(var)
    } else if (
      is.character(expr) && length(expr) == 1L && expr %in% names(data)
    ) {
      return(expr)
    } else {
      stop("Invalid column specification: ", rlang::expr_text(expr))
    }
  }

  time_chr <- .resolve_cw(rlang::enexpr(time), data)
  timing_chr <- .resolve_cw(rlang::enexpr(timing), data)
  unit_chr <- .resolve_cw(rlang::enexpr(unit), data)

  # ---- FE string -----------------------------------------------------------
  fe_rhs_text <- if (!is.null(fe)) {
    if (!inherits(fe, "formula")) {
      stop("`fe` must be a one-sided formula, e.g., ~ id + year.")
    }
    rlang::expr_text(rlang::f_rhs(fe))
  } else {
    warning(
      "`fe` not specified; defaulting to `~ ",
      unit_chr,
      " + ",
      time_chr,
      "`."
    )
    paste(unit_chr, time_chr, sep = " + ")
  }

  # ---- Validate -----------------------------------------------------------
  if (!is.numeric(data[[time_chr]])) {
    stop("'", time_chr, "' must be numeric.")
  }

  # ---- Bookkeeping --------------------------------------------------------
  all_periods <- sort(unique(data[[time_chr]]))

  timing_vec <- data[[timing_chr]]
  cohorts <- sort(unique(timing_vec[!is.na(timing_vec)]))

  if (length(cohorts) == 0L) {
    stop("No treated units found: all '", timing_chr, "' values are NA.")
  }
  if (length(cohorts) == 1L) {
    message(
      "Only one cohort found. Contamination weights reflect ",
      "cross-period contamination only (no cross-cohort variation)."
    )
  }

  reltime_vec <- data[[time_chr]] - timing_vec # NA for never-treated

  cohort_sizes <- .compute_cohort_sizes(data, timing_chr, unit_chr, cohorts)

  # ---- Build (cohort, l) index for ALL observed periods -------------------
  # gl_all: all (g, l) pairs within the observed window.
  gl_all <- do.call(
    rbind,
    lapply(cohorts, function(g) {
      fl <- all_periods - g
      if (length(fl) == 0L) {
        return(NULL)
      }
      data.frame(g = g, l = fl, stringsAsFactors = FALSE)
    })
  )

  if (is.null(gl_all) || nrow(gl_all) == 0L) {
    stop("No (cohort, period) pairs found. Check the data structure.")
  }

  K_z <- nrow(gl_all)
  N <- nrow(data)

  # ---- Build cohort-SPECIFIC indicator matrix Z ---------------------------
  # Z[, k] = 1{E_i = g_k} * 1{t - E_i = l_k}  (never-treated rows = 0)
  # Column naming follows .run_sa() convention: ".z__<g>__<l>"
  Z_colnames <- character(K_z)
  k <- 0L
  for (g in cohorts) {
    for (j in which(gl_all$g == g)) {
      l <- gl_all$l[j]
      k <- k + 1L
      Z_colnames[k] <- paste0(
        ".z__",
        g,
        "__",
        if (l < 0L) paste0("neg", -l) else as.character(l)
      )
    }
  }
  Z_mat <- build_indicator_matrix_cpp(
    cohort_id = as.integer(timing_vec),
    time_id   = as.integer(reltime_vec),
    gs_g      = as.integer(gl_all$g),
    gs_s      = as.integer(gl_all$l)
  )
  colnames(Z_mat) <- Z_colnames

  # ---- Build cohort-AGGREGATED indicator matrix X -------------------------
  # X[, j] = D^{l_j}_{i,t} = 1{t - E_i = l_j}  (sum over cohorts; 0 for
  # never-treated).  These are the regressors of the short TWFE regression.
  incl_periods <- sort(unique(gl_all$l[gl_all$l != baseline]))
  K_x <- length(incl_periods)

  X_colnames <- paste0(
    ".x__",
    ifelse(incl_periods < 0L, paste0("neg", -incl_periods), as.character(incl_periods))
  )
  X_mat <- build_indicator_matrix_cpp(
    cohort_id = ifelse(is.na(reltime_vec), NA_integer_, 1L),
    time_id   = as.integer(reltime_vec),
    gs_g      = rep(1L, K_x),
    gs_s      = as.integer(incl_periods)
  )
  colnames(X_mat) <- X_colnames

  # ---- Auxiliary OVB regressions ------------------------------------------
  # For each CATT cell (e, l) in g_incl, run:
  #   Z[i,t; e,l] = alpha_i + lambda_t + sum_{l''} omega^{l''}_{e,l} *
  #                 X[i,t; l''] + u_{i,t}
  # The coefficient on X[, l''] is omega^{l''}_{e,l}: how much of CATT_{e,l}
  # leaks into the TWFE estimate mu_{l''}.
  # All K_z regressions share the same regressor matrix X and fixed effects;
  # only the response changes. On the internal engine we demean [X | Z] and
  # solve all K_z right-hand sides in a single pass.

  X_aux <- X_mat
  colnames(X_aux) <- .mat_coef_names(".aux_X", X_colnames)
  fe_vars_aux <- .parse_fe_list(fe_rhs_text, data)
  if (isFALSE(fe_vars_aux)) {
    .require_fixest(
      paste0("This fixed-effects specification (beyond plain columns, ",
             "e.g. `~ id^year`)"),
      "use plain column fixed effects"
    )
    wdata <- data
    wdata$.aux_X <- X_mat
    fml_str <- if (nzchar(fe_rhs_text)) {
      paste0(".aux_resp ~ .aux_X | ", fe_rhs_text)
    } else {
      ".aux_resp ~ .aux_X"
    }
    fml <- stats::as.formula(fml_str)

    weight_rows <- vector("list", K_z * K_x)
    row_idx <- 0L
    for (k_z in seq_len(K_z)) {
      g_k <- gl_all$g[k_z]
      l_k <- gl_all$l[k_z]
      coefs <- tryCatch({
        wdata$.aux_resp <- Z_mat[, k_z]
        stats::coef(fixest::feols(fml, data = wdata,
                                  warn = FALSE, notes = FALSE))
      }, error = function(e) {
        warning("Auxiliary regression for CATT(g=", g_k, ", l=", l_k,
                ") failed: ", e$message, "; weights set to NA.")
        NULL
      })
      for (j in seq_len(K_x)) {
        l_twfe <- incl_periods[j]
        if (is.null(coefs)) {
          w_val <- NA_real_
        } else {
          cname <- paste0(".aux_X", X_colnames[j])
          w_val <- if (cname %in% names(coefs)) coefs[[cname]] else NA_real_
        }
        row_idx <- row_idx + 1L
        weight_rows[[row_idx]] <- data.frame(
          catt_cohort = g_k, catt_period = l_k, twfe_period = l_twfe,
          weight = w_val, stringsAsFactors = FALSE
        )
      }
    }
    weights_df <- do.call(rbind, weight_rows[seq_len(row_idx)])
  } else {
    B_kept <- tryCatch(
      .fit_multi_fe_ols(Z_mat, X_aux, fe_list = fe_vars_aux),
      error = function(e) {
        warning("Auxiliary regressions failed: ", e$message,
                "; weights set to NA.")
        NULL
      }
    )
    W_full <- matrix(NA_real_, nrow = K_x, ncol = K_z)
    if (!is.null(B_kept)) {
      cnames <- if (K_x == 1L) ".aux_X" else paste0(".aux_X", X_colnames)
      m_idx  <- match(rownames(B_kept), cnames)
      hit    <- !is.na(m_idx)
      W_full[m_idx[hit], ] <- B_kept[hit, , drop = FALSE]
    }
    weights_df <- data.frame(
      catt_cohort = rep(gl_all$g, each = K_x),
      catt_period = rep(gl_all$l, each = K_x),
      twfe_period = rep(incl_periods, times = K_z),
      weight      = as.numeric(W_full),
      stringsAsFactors = FALSE
    )
  }
  weights_df <- weights_df[
    order(
      weights_df$twfe_period,
      weights_df$catt_cohort,
      weights_df$catt_period
    ),
  ]
  rownames(weights_df) <- NULL
  weights_df$is_own <- weights_df$catt_period == weights_df$twfe_period

  structure(
    weights_df,
    class = c("sa_contamination_weights", "data.frame"),
    baseline = baseline,
    cohorts = cohorts,
    cohort_sizes = cohort_sizes,
    incl_periods = incl_periods
  )
}


# ---------------------------------------------------------------------------
# print.sa_contamination_weights
# ---------------------------------------------------------------------------

#' @export
#' @noRd
print.sa_contamination_weights <- function(x, digits = 3L, ...) {
  cohorts <- attr(x, "cohorts")
  incl_periods <- attr(x, "incl_periods")
  baseline <- attr(x, "baseline")
  cohort_sizes <- attr(x, "cohort_sizes")

  cat("SA Contamination Weights\n")
  cat(rep("-", 50L), "\n", sep = "")
  cat("Cohorts        :", paste(cohorts, collapse = ", "), "\n")
  cat(
    "Cohort sizes   :",
    paste(paste0(cohorts, "(", cohort_sizes, ")"), collapse = ", "),
    "\n"
  )
  cat("Baseline period:", baseline, "\n")
  cat("Included periods:", paste(incl_periods, collapse = ", "), "\n\n")

  # Per-TWFE-period: own-weight sum and max cross-period |weight|
  twfe_periods <- sort(unique(x$twfe_period))
  summ <- do.call(
    rbind,
    lapply(twfe_periods, function(l) {
      sub <- x[x$twfe_period == l, ]
      own_sum <- sum(sub$weight[sub$is_own], na.rm = TRUE)
      cross_max <- max(abs(sub$weight[!sub$is_own]), na.rm = TRUE)
      data.frame(
        twfe_period = l,
        own_weight_sum = round(own_sum, digits),
        max_cross_abs = round(cross_max, digits)
      )
    })
  )

  cat("Summary per TWFE period (own-weight sum should be ~1):\n")
  print(summ, row.names = FALSE)
  invisible(x)
}


# ---------------------------------------------------------------------------
# plot_contamination_weights
# ---------------------------------------------------------------------------

#' Plot contamination weights as a tile heatmap
#'
#' @description
#' Base `plot()` method for the contamination weights returned by
#' [contamination_weights()]: a ggplot2 tile heatmap.
#'
#' Each cell at position (`twfe_period`, `catt_label`) shows the
#' weight \eqn{\omega^{\ell''}_{e,\ell}}: how much of \eqn{CATT_{e,\ell}}
#' leaks into the TWFE coefficient \eqn{\hat\mu_{\ell''}}.
#'
#' - **Diagonal cells** (`catt_period == twfe_period`): own-period
#'   weights (sum across cohorts should be \eqn{\approx 1}).
#' - **Off-diagonal cells**: cross-period contamination (ideally
#'   close to zero under treatment effect homogeneity).
#'
#' @param x An `sa_contamination_weights` object from
#'   [contamination_weights()].
#' @param limit_abs Numeric; symmetric colour scale limit `[-limit, limit]`.
#'   Defaults to the maximum absolute weight (rounded up to one decimal).
#' @param midpoint Numeric; midpoint of the diverging colour scale (default 0).
#' @param low Colour for negative weights (default `"#2166AC"`).
#' @param mid Colour for zero weight (default `"white"`).
#' @param high Colour for positive weights (default `"#B2182B"`).
#' @param theme Character; `"bw"` (default), `"minimal"`, or `"classic"`.
#' @param show_values Logical; overlay weight values in each tile (default
#'   `FALSE`).
#' @param value_digits Integer; decimal digits when `show_values = TRUE`
#'   (default `2L`).
#' @param ... Unused.
#'
#' @return A [ggplot2::ggplot()] object.
#'
#' @seealso [contamination_weights()]
#'
#' @importFrom rlang .data
#' @export
plot.sa_contamination_weights <- function(
  x,
  limit_abs = NULL,
  midpoint = 0,
  low = "#2166AC",
  mid = "white",
  high = "#B2182B",
  theme = c("bw", "minimal", "classic"),
  show_values = FALSE,
  value_digits = 2L,
  ...
) {
  if (!inherits(x, "sa_contamination_weights")) {
    stop(
      "`x` must be an `sa_contamination_weights` object from ",
      "`contamination_weights()`."
    )
  }

  theme <- match.arg(theme)

  # ---- Prepare label for y-axis (cohort: catt_period) ---------------------
  plot_df <- x
  plot_df$catt_label <- paste0(
    "g=",
    plot_df$catt_cohort,
    ", l=",
    ifelse(
      plot_df$catt_period < 0,
      as.character(plot_df$catt_period),
      paste0("+", plot_df$catt_period)
    )
  )

  # Order y-axis: first by cohort, then by catt_period (ascending)
  gl_order <- unique(plot_df[
    order(plot_df$catt_cohort, plot_df$catt_period),
    c("catt_cohort", "catt_period", "catt_label")
  ])
  plot_df$catt_label <- factor(
    plot_df$catt_label,
    levels = rev(gl_order$catt_label)
  )

  # ---- Colour scale limits ------------------------------------------------
  max_abs <- max(abs(plot_df$weight), na.rm = TRUE)
  if (is.null(limit_abs)) {
    limit_abs <- ceiling(max_abs * 10) / 10 # round up to 1 decimal
    limit_abs <- max(limit_abs, 0.1) # at least 0.1
  }

  # ---- Build ggplot -------------------------------------------------------
  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$twfe_period,
      y = .data$catt_label,
      fill = .data$weight
    )
  ) +
    ggplot2::geom_tile(colour = "grey70", linewidth = 0.3) +
    ggplot2::scale_fill_gradient2(
      low = low,
      mid = mid,
      high = high,
      midpoint = midpoint,
      limits = c(-limit_abs, limit_abs),
      oob = scales::squish,
      name = "Weight"
    ) +
    ggplot2::geom_vline(
      xintercept = 0 - 0.5,
      linetype = "dashed",
      colour = "grey40",
      linewidth = 0.4
    ) +
    ggplot2::scale_x_continuous(
      breaks = attr(x, "incl_periods"),
      expand = ggplot2::expansion(add = 0.5)
    ) +
    ggplot2::labs(
      x = "TWFE relative event time (l'')",
      y = "CATT cell (g, l)",
      title = "Contamination weights for TWFE event-study coefficients",
      subtitle = paste0(
        "Each cell shows the weight of CATT(g, l) ",
        "in the TWFE estimate for event time l''"
      )
    )

  # Optional: overlay weight values
  if (show_values) {
    fmt <- paste0("%.", value_digits, "f")
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = sprintf(fmt, .data$weight)),
        size = 2.8,
        colour = ifelse(
          abs(plot_df$weight) > limit_abs * 0.6,
          "white",
          "grey20"
        )
      )
  }

  # Apply theme
  base_theme <- switch(
    theme,
    bw = ggplot2::theme_bw(),
    minimal = ggplot2::theme_minimal(),
    classic = ggplot2::theme_classic()
  )
  p <- p +
    base_theme +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 8),
      axis.text.x = ggplot2::element_text(size = 9),
      panel.grid = ggplot2::element_blank(),
      legend.position = "right"
    )

  p
}
