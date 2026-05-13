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

# ---------------------------------------------------------------------------
# compute_contamination_weights
# ---------------------------------------------------------------------------

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
#' \itemize{
#'   \item \strong{Own-period cell} (\code{catt_period == twfe_period}):
#'     \eqn{\omega^{\ell}_{{e,\ell}}} represents the weight the TWFE estimator
#'     places on \eqn{CATT_{e,\ell}}. Under the SA IW estimator these equal
#'     the cohort-size weights \eqn{n_e / \sum n_{e'}}.
#'   \item \strong{Cross-period cell} (\code{catt_period != twfe_period}):
#'     Any non-zero weight indicates contamination: the TWFE coefficient
#'     \eqn{\hat\mu_{\ell''}} also picks up treatment effects from period
#'     \eqn{\ell \ne \ell''}.
#'   \item \strong{Verification}: the OVB identity (property iii) holds
#'     exactly, so \eqn{\hat\mu_{\ell''} = \sum_{(e,\ell)}
#'     \omega^{\ell''}_{e,\ell} \cdot \widehat{CATT}_{e,\ell}} up to
#'     floating-point precision.
#' }
#'
#' @param data A data.frame with one row per unit-period (balanced panel).
#' @param time Unquoted name of the calendar time variable (numeric).
#' @param timing Unquoted name of the first-treatment-period variable;
#'   \code{NA} marks never-treated units.
#' @param unit Unquoted name of the unit identifier.
#' @param fe One-sided fixed-effects formula, e.g. \code{~ id + year}.
#'   When \code{NULL} (default), falls back to \code{~ <unit> + <time>}
#'   with a warning.
#' @param baseline Integer reference (baseline) period excluded from the TWFE
#'   specification (default \code{-1L}).  Must match the \code{baseline}
#'   argument used in \code{\link{run_es}}.
#'
#' @return An object of class \code{c("sa_contamination_weights",
#'   "data.frame")} with one row per \code{(catt_cohort, catt_period,
#'   twfe_period)} triple, and columns:
#' \describe{
#'   \item{\code{catt_cohort}}{Cohort \eqn{e} (first treatment period).}
#'   \item{\code{catt_period}}{Relative event time \eqn{\ell} of the CATT.}
#'   \item{\code{twfe_period}}{Relative event time \eqn{\ell''} of the TWFE
#'     coefficient being decomposed.}
#'   \item{\code{weight}}{Contamination weight
#'     \eqn{\omega^{\ell''}_{e,\ell}}.}
#'   \item{\code{is_own}}{Logical; \code{TRUE} when
#'     \code{catt_period == twfe_period}.}
#' }
#' Attributes: \code{baseline}, \code{cohorts}, \code{cohort_sizes},
#' \code{incl_periods}.
#'
#' @seealso \code{\link{plot_contamination_weights}}, \code{\link{run_es}}
#'
#' @references
#' Sun, L. and Abraham, S. (2021). Estimating dynamic treatment effects in
#' event studies with heterogeneous treatment effects.
#' \emph{Journal of Econometrics}, 225(2), 175--199.
#'
#' @examples
#' \dontrun{
#' # Estimate contamination weights
#' cw <- compute_contamination_weights(
#'   data     = panel_data,
#'   time     = year,
#'   timing   = first_treat,
#'   unit     = id,
#'   fe       = ~ id + year,
#'   baseline = -1L
#' )
#' print(cw)
#' plot_contamination_weights(cw)
#' }
#'
#' @export
compute_contamination_weights <- function(
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
  data <- data[order(data[[unit_chr]], data[[time_chr]]), ]
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

  cohort_sizes <- vapply(
    cohorts,
    function(g) {
      length(unique(data[[unit_chr]][!is.na(timing_vec) & timing_vec == g]))
    },
    integer(1L)
  )
  names(cohort_sizes) <- as.character(cohorts)

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
  Z_mat <- matrix(0L, nrow = N, ncol = K_z)
  Z_colnames <- character(K_z)
  k <- 0L
  for (g in cohorts) {
    g_mask <- !is.na(timing_vec) & timing_vec == g
    for (j in which(gl_all$g == g)) {
      l <- gl_all$l[j]
      k <- k + 1L
      Z_colnames[k] <- paste0(
        ".z__",
        g,
        "__",
        if (l < 0L) {
          paste0("neg", -l)
        } else {
          as.character(l)
        }
      )
      Z_mat[, k] <- as.integer(g_mask & !is.na(reltime_vec) & reltime_vec == l)
    }
  }
  colnames(Z_mat) <- Z_colnames

  # ---- Build cohort-AGGREGATED indicator matrix X -------------------------
  # X[, j] = D^{l_j}_{i,t} = 1{t - E_i = l_j}  (sum over cohorts; 0 for
  # never-treated).  These are the regressors of the short TWFE regression.
  incl_periods <- sort(unique(gl_all$l[gl_all$l != baseline]))
  K_x <- length(incl_periods)

  X_mat <- matrix(0L, nrow = N, ncol = K_x)
  X_colnames <- character(K_x)
  for (j in seq_len(K_x)) {
    l_j <- incl_periods[j]
    X_colnames[j] <- paste0(
      ".x__",
      if (l_j < 0L) {
        paste0("neg", -l_j)
      } else {
        as.character(l_j)
      }
    )
    X_mat[, j] <- as.integer(!is.na(reltime_vec) & reltime_vec == l_j)
  }
  colnames(X_mat) <- X_colnames

  # ---- Auxiliary OVB regressions ------------------------------------------
  # For each CATT cell (e, l) in g_incl, run:
  #   Z[i,t; e,l] = alpha_i + lambda_t + sum_{l''} omega^{l''}_{e,l} *
  #                 X[i,t; l''] + u_{i,t}
  # The coefficient on X[, l''] is omega^{l''}_{e,l}: how much of CATT_{e,l}
  # leaks into the TWFE estimate mu_{l''}.
  # All K_z regressions share the same regressor matrix X; only the response
  # changes.  We store X as a matrix column ".aux_X" in a working copy of data
  # so fixest reads all K_x aggregated columns as a single matrix block.

  wdata <- data # working copy (avoids modifying user df)
  wdata$.aux_X <- X_mat # fixed across loop iterations

  weight_rows <- vector("list", K_z * K_x)
  row_idx <- 0L

  fml_str <- if (nzchar(fe_rhs_text)) {
    paste0(".aux_resp ~ .aux_X | ", fe_rhs_text)
  } else {
    ".aux_resp ~ .aux_X"
  }
  fml <- stats::as.formula(fml_str)

  for (k_z in seq_len(K_z)) {
    g_k <- gl_all$g[k_z]
    l_k <- gl_all$l[k_z]

    wdata$.aux_resp <- Z_mat[, k_z]

    model <- tryCatch(
      fixest::feols(fml, data = wdata, warn = FALSE, notes = FALSE),
      error = function(e) {
        warning(
          "Auxiliary regression for CATT(g=",
          g_k,
          ", l=",
          l_k,
          ") failed: ",
          e$message,
          "; weights set to NA."
        )
        NULL
      }
    )

    coefs <- if (!is.null(model)) stats::coef(model) else NULL

    for (j in seq_len(K_x)) {
      l_twfe <- incl_periods[j]
      if (is.null(coefs)) {
        w_val <- NA_real_
      } else {
        # feols prepends the matrix-column name ".aux_X" to each column name
        cname <- paste0(".aux_X", X_colnames[j])
        w_val <- if (cname %in% names(coefs)) coefs[[cname]] else NA_real_
      }
      row_idx <- row_idx + 1L
      weight_rows[[row_idx]] <- data.frame(
        catt_cohort = g_k,
        catt_period = l_k,
        twfe_period = l_twfe,
        weight = w_val,
        stringsAsFactors = FALSE
      )
    }
  }

  weights_df <- do.call(rbind, weight_rows[seq_len(row_idx)])
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

#' Plot Contamination Weights as a Tile Heatmap
#'
#' @description
#' Creates a ggplot2 tile heatmap of the contamination weights returned by
#' \code{\link{compute_contamination_weights}}.
#'
#' Each cell at position (\code{twfe_period}, \code{catt_label}) shows the
#' weight \eqn{\omega^{\ell''}_{e,\ell}}: how much of \eqn{CATT_{e,\ell}}
#' leaks into the TWFE coefficient \eqn{\hat\mu_{\ell''}}.
#' \itemize{
#'   \item \strong{Diagonal cells} (\code{catt_period == twfe_period}): own-period
#'     weights (sum across cohorts should be \eqn{\approx 1}).
#'   \item \strong{Off-diagonal cells}: cross-period contamination (ideally
#'     close to zero under treatment effect homogeneity).
#' }
#'
#' @param x An \code{sa_contamination_weights} object from
#'   \code{\link{compute_contamination_weights}}.
#' @param limit_abs Numeric; symmetric colour scale limit \code{[-limit, limit]}.
#'   Defaults to the maximum absolute weight (rounded up to one decimal).
#' @param midpoint Numeric; midpoint of the diverging colour scale (default 0).
#' @param low Colour for negative weights (default \code{"#2166AC"}).
#' @param mid Colour for zero weight (default \code{"white"}).
#' @param high Colour for positive weights (default \code{"#B2182B"}).
#' @param theme Character; \code{"bw"} (default), \code{"minimal"}, or
#'   \code{"classic"}.
#' @param show_values Logical; overlay weight values in each tile (default
#'   \code{FALSE}).
#' @param value_digits Integer; decimal digits when \code{show_values = TRUE}
#'   (default \code{2L}).
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @seealso \code{\link{compute_contamination_weights}}
#'
#' @importFrom rlang .data
#' @export
plot_contamination_weights <- function(
  x,
  limit_abs = NULL,
  midpoint = 0,
  low = "#2166AC",
  mid = "white",
  high = "#B2182B",
  theme = c("bw", "minimal", "classic"),
  show_values = FALSE,
  value_digits = 2L
) {
  if (!inherits(x, "sa_contamination_weights")) {
    stop(
      "`x` must be an `sa_contamination_weights` object from ",
      "`compute_contamination_weights()`."
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
      x = "TWFE relative event time (\u2113'')",
      y = "CATT cell (g, \u2113)",
      title = "Contamination weights for TWFE event-study coefficients",
      subtitle = paste0(
        "Each cell shows \u03c9\u1d35\u1d31\u1d1e\u2099\u1d40\u1d32\u1d31 ",
        "[= weight on CATT(g,\u2113) in TWFE estimate \u03bc\u1d35\u1d31\u1d1e\u2099\u2099]"
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
