#' Borusyak, Jaravel, Spiess (2024) Imputation Estimator
#'
#' @description
#' Implements the 3-step imputation estimator from Borusyak, Jaravel, and Spiess
#' (2024) for staggered adoption designs (Theorem 2).
#'
#' \strong{Step 1} estimates unit and time fixed effects on the set of
#' "untreated" observations Ω₀ (never-treated units ∪ not-yet-treated
#' observations, i.e. \eqn{t < G_i} for eventually-treated units):
#' \deqn{Y_{it} = \alpha_i + \beta_t + \varepsilon_{it},\quad (i,t)\in\Omega_0}
#'
#' \strong{Step 2} imputes the counterfactual for each treated observation
#' (i,t) ∈ Ω₁ (where \eqn{G_i} is not \code{NA} and \eqn{t \ge G_i}):
#' \deqn{\hat{Y}_{it}(0) = \hat\alpha_i + \hat\beta_t,\qquad
#'       \hat\tau_{it} = Y_{it} - \hat{Y}_{it}(0)}
#'
#' \strong{Step 3} aggregates to an event-study curve at horizon \eqn{h}:
#' \deqn{\hat\tau_h = \frac{1}{|\Omega_{1,h}|}
#'   \sum_{(i,t):\,K_{it}=h} \hat\tau_{it}}
#'
#' Standard errors follow Theorem 3 / equations (7)--(8). The estimator is
#' \eqn{\sum v_{it} Y_{it}} over all observations, where the weights on
#' untreated observations project the treated weights on the Step 1 fixed
#' effects. Treated residuals are \eqn{\hat\tau_{it}} minus the
#' \eqn{v^2}-weighted mean of its cohort \eqn{\times} horizon cell (eq. 8),
#' untreated residuals come from Step 1, and the cluster scores
#' \eqn{\sum_t v_{it}\tilde\varepsilon_{it}} form the sandwich (eq. 7).
#'
#' @param data data.frame with one row per unit-period (balanced panel).
#' @param outcome_chr Column name (character) for the outcome variable.
#' @param timing_chr Column name (character) for the first treatment period;
#'   \code{NA} marks never-treated units.
#' @param time_chr Column name (character) for calendar time (numeric).
#' @param unit_chr Column name (character) for the unit identifier.
#' @param clustervars Character vector of clustering variable name(s), or
#'   \code{NULL} to cluster by unit (default).
#' @param conf.level Numeric confidence level(s) for CIs (default 0.95).
#'
#' @return A list with:
#' \describe{
#'   \item{\code{es}}{data.frame: \code{relative_time}, \code{estimate},
#'     \code{std_error}, \code{conf_low_XX}/\code{conf_high_XX} per level.}
#'   \item{\code{tau_it}}{data.frame of unit-time treatment effects:
#'     \code{unit}, \code{time}, \code{cohort}, \code{relative_time},
#'     \code{tau_hat}.}
#'   \item{\code{n_never}}{Number of never-treated units.}
#'   \item{\code{cohorts}}{Sorted numeric vector of cohort values.}
#'   \item{\code{n_obs_omega0}}{Number of observations used in Step 1.}
#'   \item{\code{n_unimputed}}{Number of treated observations whose
#'     counterfactual could not be imputed (missing FE level) and were
#'     therefore excluded from aggregation.}
#'   \item{\code{n_treated_obs}}{Size of \eqn{\Omega_1} (treated observations)
#'     before the unimputable rows were dropped.}
#'   \item{\code{vcov_ctx}}{Inputs for \code{.bjs_se()}, the standard error of
#'     any weighted average of \code{tau_it$tau_hat}.}
#' }
#' @noRd
.run_bjs <- function(data,
                     outcome_chr,
                     timing_chr,
                     time_chr,
                     unit_chr,
                     clustervars = NULL,
                     conf.level  = 0.95) {

  # ---- Validate inputs -------------------------------------------------------
  .validate_panel_cols(data, c(outcome_chr, timing_chr, time_chr, unit_chr), time_chr)
  .assert_unit_constant_timing(data, timing_chr, unit_chr)

  # ---- Bookkeeping -----------------------------------------------------------
  data <- data[order(data[[unit_chr]], data[[time_chr]]), ]

  cohorts <- sort(unique(data[[timing_chr]][!is.na(data[[timing_chr]])]))
  if (length(cohorts) == 0L)
    stop("No treated units found: all values of '", timing_chr, "' are NA.")

  never_units <- unique(data[[unit_chr]][is.na(data[[timing_chr]])])
  n_never     <- length(never_units)

  # ---- Partition into Ω₀ and Ω₁ (BJS 2024, Section 2) ----------------------
  # Ω₀ = never-treated units (all periods) + not-yet-treated observations
  #      (t < G_i for eventually-treated units i)
  # Ω₁ = treated observations: G_i not NA and t >= G_i
  is_treated_obs <- !is.na(data[[timing_chr]]) &
                    data[[time_chr]] >= data[[timing_chr]]
  omega0 <- data[!is_treated_obs, ]
  omega1 <- data[ is_treated_obs, ]

  if (nrow(omega0) == 0L)
    stop("No untreated observations (Omega_0) found. The BJS imputation ",
         "estimator requires never-treated or not-yet-treated units to identify ",
         "the counterfactual. Ensure `timing` is NA for never-treated units and ",
         "that eventually-treated units have at least one pre-treatment period.")

  if (nrow(omega1) == 0L)
    stop("No treated observations (Omega_1) found. Check that `timing` specifies ",
         "the first treatment period and that observations exist at or after it.")

  # ---- Step 1: Estimate FEs on Ω₀ (Theorem 2, Step 1) ----------------------
  # Y_it = alpha_i + beta_t + eps_it,  (i,t) in Omega_0
  # Estimated with the internal alternating-projections solver; it drops NA
  # outcomes and singleton FE levels the same way fixest::feols() does.
  step1_fit <- tryCatch(
    .solve_fe_2way(omega0[[outcome_chr]], omega0[[unit_chr]],
                   omega0[[time_chr]]),
    error = function(e)
      stop("BJS Step 1: FE regression on untreated observations failed: ",
           e$message)
  )

  # ---- Step 2: Impute Y_hat_it(0) and compute tau_hat_it (Theorem 2, Step 2)
  # Y_hat_it(0) = alpha_hat_i + beta_hat_t  via named lookup
  alpha_hat <- step1_fit$alpha   # named: unit_id  -> unit FE
  beta_hat  <- step1_fit$beta    # named: time_val -> time FE

  # The solver does not estimate unit FEs for "singleton" units (those with
  # exactly 1 observation in omega0).  Their FE is closed-form from
  # Theorem 2: alpha_hat_i = Y_{i,t} - beta_hat_t.  We patch those entries
  # before the imputation lookup below.
  u_keys <- as.character(omega1[[unit_chr]])
  t_keys <- as.character(omega1[[time_chr]])

  singleton_u <- unique(u_keys[is.na(alpha_hat[u_keys])])
  if (length(singleton_u) > 0L) {
    omega0_u_chr <- as.character(omega0[[unit_chr]])
    sel <- omega0_u_chr %in% singleton_u
    if (any(sel)) {
      omega0_t_chr <- as.character(omega0[[time_chr]][sel])
      resid0 <- omega0[[outcome_chr]][sel] - beta_hat[omega0_t_chr]
      means  <- tapply(resid0, omega0_u_chr[sel], mean, na.rm = TRUE)
      alpha_hat[names(means)] <- means
    }
  }

  y_hat0 <- alpha_hat[u_keys] + beta_hat[t_keys]
  names(y_hat0) <- NULL

  n_na <- sum(is.na(y_hat0))
  if (n_na == nrow(omega1))
    stop("All imputed counterfactuals are NA. This usually means treated units ",
         "have no pre-treatment observations or some time periods are only ",
         "observed in the treated state. Check the panel structure.")
  if (n_na > 0L)
    warning(sprintf("%d treated observation(s) (%.1f%%) could not be imputed ",
                    n_na, 100 * n_na / nrow(omega1)),
            "due to missing FE levels; excluded from aggregation.")

  # Number of treated observations that could not be imputed (missing FE level),
  # retained for the result so callers can audit how much of Omega_1 was dropped.
  n_unimputed <- n_na
  n_omega1    <- nrow(omega1)

  omega1$.y_hat0  <- y_hat0
  omega1$.tau_hat <- omega1[[outcome_chr]] - omega1$.y_hat0
  omega1$.rel_t   <- as.integer(omega1[[time_chr]] - omega1[[timing_chr]])
  omega1          <- omega1[!is.na(omega1$.tau_hat), ]

  if (nrow(omega1) == 0L)
    stop("No valid treated observations remain after counterfactual imputation.")

  # ---- Step 3: Aggregate by event-study horizon (Theorem 2, Step 3) ---------
  # tau_hat_h = (1 / |Omega_{1,h}|) * sum_{(i,t): K_it = h} tau_hat_it
  horizons <- sort(unique(omega1$.rel_t))
  es_rows  <- list()

  # Integer codes of units, times and clusters over Omega_0 (rows with an
  # outcome) and Omega_1; the cluster defaults to the unit.
  ok0   <- is.finite(omega0[[outcome_chr]])
  u_raw <- omega0[[unit_chr]][ok0]
  t_raw <- omega0[[time_chr]][ok0]
  u_lev <- unique(u_raw)
  t_lev <- unique(t_raw)
  u0 <- match(u_raw, u_lev)
  t0 <- match(t_raw, t_lev)
  u1 <- match(omega1[[unit_chr]], u_lev)
  t1 <- match(omega1[[time_chr]], t_lev)

  if (is.null(clustervars) || length(clustervars) == 0L) {
    cl0 <- u0
    cl1 <- u1
  } else {
    clust_raw <- function(d) {
      if (length(clustervars) == 1L) return(d[[clustervars]])
      do.call(paste, c(lapply(clustervars, function(v) as.character(d[[v]])),
                       list(sep = "__")))
    }
    cl_all <- .dense_codes(c(clust_raw(omega0)[ok0], clust_raw(omega1)))
    cl0 <- cl_all[seq_along(u0)]
    cl1 <- cl_all[length(u0) + seq_along(u1)]
  }

  eps0 <- omega0[[outcome_chr]][ok0] -
    alpha_hat[as.character(u_raw)] - beta_hat[as.character(t_raw)]
  eps0[is.na(eps0)] <- 0   # rows absorbed by a singleton fixed effect

  # Cohort x horizon cells (eq. 8)
  g_idx <- match(omega1[[timing_chr]], cohorts)
  cell1 <- .dense_codes(g_idx + (omega1$.rel_t - min(omega1$.rel_t)) * length(cohorts))

  vcov_ctx <- list(
    tau = omega1$.tau_hat, cell1 = cell1,
    u0 = u0, t0 = t0, u1 = u1, t1 = t1,
    cl0 = cl0, cl1 = cl1, eps0 = eps0,
    n_unit = length(u_lev), n_time = length(t_lev),
    n_cl = max(cl0, cl1)
  )

  rel_t_all <- omega1$.rel_t
  tau_all   <- omega1$.tau_hat
  wts <- lapply(horizons, function(h) {
    idx <- which(rel_t_all == h)
    list(idx = idx, w = rep(1 / length(idx), length(idx)))
  })
  se_h <- .bjs_se(vcov_ctx, wts)
  for (j in seq_along(horizons)) {
    es_rows[[j]] <- data.frame(
      relative_time = horizons[j],
      estimate      = mean(tau_all[wts[[j]]$idx]),
      std_error     = se_h[j],
      stringsAsFactors = FALSE
    )
  }

  if (length(es_rows) == 0L)
    stop("BJS event-study aggregation produced no estimates.")

  es           <- do.call(rbind, es_rows)
  es           <- es[order(es$relative_time), ]
  rownames(es) <- NULL

  # ---- Confidence intervals -------------------------------------------------
  conf.level <- sort(unique(conf.level))
  es <- .add_ci_columns(es, conf.level, se_col = "std_error")

  # ---- tau_it table for downstream use --------------------------------------
  tau_it <- data.frame(
    unit          = omega1[[unit_chr]],
    time          = omega1[[time_chr]],
    cohort        = omega1[[timing_chr]],
    relative_time = omega1$.rel_t,
    tau_hat       = omega1$.tau_hat,
    stringsAsFactors = FALSE
  )

  list(
    es           = es,
    tau_it       = tau_it,
    n_never      = n_never,
    cohorts      = cohorts,
    n_obs_omega0 = nrow(omega0),
    n_unimputed  = n_unimputed,
    n_treated_obs = n_omega1,
    vcov_ctx     = vcov_ctx
  )
}

# Standard errors of the aggregates sum_{it} w_it * tau_hat_it (BJS 2024,
# Theorem 3, eqs. 7-8). `wts` is a list of list(idx, w): rows of tau_it and
# their weights. The weights on untreated observations are
# v_it = -(z_i + z_t) with (A0'A0) z = A1'w, A0 and A1 the unit and time
# dummies of Omega_0 and Omega_1. The side with more levels is eliminated, and
# the remaining system is solved with a pseudo-inverse computed once (or by
# Gauss-Seidel sweeps when it has more than `dense_max` levels).
.bjs_se <- function(ctx, wts, dense_max = 1000L) {
  elim_unit <- ctx$n_time <= ctx$n_unit
  if (elim_unit) {
    e0 <- ctx$u0; k0 <- ctx$t0; e1 <- ctx$u1; k1 <- ctx$t1
    n_e <- ctx$n_unit; n_k <- ctx$n_time
  } else {
    e0 <- ctx$t0; k0 <- ctx$u0; e1 <- ctx$t1; k1 <- ctx$u1
    n_e <- ctx$n_time; n_k <- ctx$n_unit
  }
  cnt_e <- tabulate(e0, n_e)
  n_w   <- length(wts)

  # Treated side, one column per aggregate: cluster scores of the residuals
  # from the v^2-weighted cohort x horizon cell means (eq. 8), and A1'w.
  score <- matrix(0, ctx$n_cl, n_w)
  C_e   <- matrix(0, n_e, n_w)
  C_k   <- matrix(0, n_k, n_w)
  for (j in seq_len(n_w)) {
    i <- wts[[j]]$idx
    w <- wts[[j]]$w

    cell <- .dense_codes(ctx$cell1[i])
    w2   <- w^2
    n_cell <- max(cell)
    tau_tilde <- sum_by_code_cpp(cell, w2 * ctx$tau[i], n_cell) /
      sum_by_code_cpp(cell, w2, n_cell)
    eps1 <- ctx$tau[i] - tau_tilde[cell]

    score[, j] <- sum_by_code_cpp(ctx$cl1[i], w * eps1, ctx$n_cl)
    C_e[, j]   <- sum_by_code_cpp(e1[i], w, n_e)
    C_k[, j]   <- sum_by_code_cpp(k1[i], w, n_k)
  }

  if (n_k <= dense_max) {
    eig  <- eigen(fe_schur_cpp(e0 - 1L, k0 - 1L, n_e, n_k), symmetric = TRUE)
    keep <- eig$values > max(eig$values) * 1e-10
    M_pinv <- eig$vectors[, keep, drop = FALSE] %*%
      (t(eig$vectors[, keep, drop = FALSE]) / eig$values[keep])
    Z_k <- M_pinv %*% (C_k - sum_rows_by_code_cpp(k0, e0, C_e / cnt_e, n_k))
    Z_e <- (C_e - sum_rows_by_code_cpp(e0, k0, Z_k, n_e)) / cnt_e
  } else {
    Z_e <- C_e
    Z_k <- C_k
    for (j in seq_len(n_w)) {
      z <- solve_fe_normal_cpp(e0 - 1L, k0 - 1L, C_e[, j], C_k[, j], n_e, n_k,
                               1e-12, 100000L)
      if (!z$converged)
        warning("BJS variance: the projection of the treated weights did ",
                "not converge.", call. = FALSE)
      Z_e[, j] <- z$z_unit
      Z_k[, j] <- z$z_time
    }
  }

  # Untreated side: sum_t v_it * eps_it with v_it = -(z_e + z_k).
  score <- score -
    sum_rows_by_code_cpp(ctx$cl0, e0, Z_e, ctx$n_cl, ctx$eps0) -
    sum_rows_by_code_cpp(ctx$cl0, k0, Z_k, ctx$n_cl, ctx$eps0)
  sqrt(colSums(score^2))
}
