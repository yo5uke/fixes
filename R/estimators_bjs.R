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
.sum_mat_by_code <- function(code, X, n) {
  m <- ncol(X)
  out <- matrix(0, nrow = n, ncol = m)
  for (j in seq_len(m)) out[, j] <- sum_by_code_cpp(code, X[, j], n)
  out
}

.run_bjs <- function(data,
                     outcome_chr,
                     timing_chr,
                     time_chr,
                     unit_chr,
                     clustervars  = NULL,
                     conf.level   = 0.95,
                     covariates   = NULL,
                     aggregate_es = TRUE,
                     pretrends    = FALSE,
                     baseline     = -1L) {

  # ---- Validate inputs -------------------------------------------------------
  .validate_panel_cols(data, c(outcome_chr, timing_chr, time_chr, unit_chr), time_chr)
  .assert_unit_constant_timing(data, timing_chr, unit_chr)

  cov_res   <- .covariate_columns(covariates, data)
  data      <- cov_res$data
  cov_names <- cov_res$names

  # ---- Bookkeeping -----------------------------------------------------------
  timing_vec <- data[[timing_chr]]
  cohorts    <- sort(unique(timing_vec[!is.na(timing_vec)]))
  if (length(cohorts) == 0L)
    stop("No treated units found: all values of '", timing_chr, "' are NA.")

  never_units <- unique(data[[unit_chr]][is.na(timing_vec)])
  n_never     <- length(never_units)

  # ---- Partition into Ω₀ and Ω₁ (BJS 2024, Section 2) ----------------------
  # Ω₀ = never-treated units (all periods) + not-yet-treated observations
  #      (t < G_i for eventually-treated units i)
  # Ω₁ = treated observations: G_i not NA and t >= G_i
  is_treated_obs <- !is.na(timing_vec) &
                    data[[time_chr]] >= timing_vec
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

  # ---- Step 1: Estimate FEs (+ covariates) on Ω₀ (Theorem 2, eq. 5) --------
  y0_all <- as.numeric(omega0[[outcome_chr]])
  ok0    <- is.finite(y0_all)
  has_cov <- length(cov_names) > 0L
  if (has_cov) {
    X0_all <- matrix(0, nrow = nrow(omega0), ncol = length(cov_names))
    for (j in seq_along(cov_names)) {
      v <- as.numeric(omega0[[cov_names[j]]])
      X0_all[, j] <- v
      ok0 <- ok0 & is.finite(v)
    }
  }
  if (!any(ok0))
    stop("BJS Step 1: FE regression on untreated observations failed: ",
         "no non-NA observations.")

  y0    <- y0_all[ok0]
  u_raw <- omega0[[unit_chr]][ok0]
  t_raw <- omega0[[time_chr]][ok0]
  u_lev <- unique(u_raw)
  t_lev <- unique(t_raw)
  u0    <- match(u_raw, u_lev)
  t0    <- match(t_raw, t_lev)
  n_unit <- length(u_lev)
  n_time <- length(t_lev)

  keep_ns <- rep(TRUE, length(y0))
  repeat {
    idx <- which(keep_ns)
    if (length(idx) == 0L) break
    cu <- tabulate(u0[idx], n_unit)
    ct <- tabulate(t0[idx], n_time)
    ok_step <- (cu[u0[idx]] > 1L) & (ct[t0[idx]] > 1L)
    if (all(ok_step)) break
    keep_ns[idx[!ok_step]] <- FALSE
  }
  if (!any(keep_ns))
    stop("BJS Step 1: FE regression on untreated observations failed: ",
         "all observations are singletons.")

  idx_ns   <- which(keep_ns)
  u0_ns    <- u0[idx_ns]
  t0_ns    <- t0[idx_ns]
  u_active <- which(tabulate(u0_ns, n_unit) > 0L)
  t_active <- which(tabulate(t0_ns, n_time) > 0L)
  u0_dense <- match(u0_ns, u_active)
  t0_dense <- match(t0_ns, t_active)

  kept_x    <- integer(0L)
  delta_hat <- numeric(0L)
  R_chol    <- NULL
  X0_tilde  <- NULL
  X0_kept   <- NULL
  y0_adj    <- y0

  if (has_cov) {
    X0_ok <- X0_all[ok0, , drop = FALSE]
    nt    <- .fes_nthreads()
    fe_ids <- cbind(u0_dense - 1L, t0_dense - 1L)
    storage.mode(fe_ids) <- "integer"
    n_lev  <- c(length(u_active), length(t_active))
    k_idx  <- if (length(idx_ns) == length(y0)) integer(0L) else idx_ns

    byx <- bind_yx_cpp(y0, X0_ok, k_idx, numeric(0), nt)
    dm  <- demean_kway_cpp(byx$M, fe_ids, n_lev,
                           w = numeric(0), tol = 1e-12,
                           max_iter = 10000L, nthreads = nt,
                           scale_sqrt_w = FALSE)
    if (!dm$converged)
      warning("BJS Step 1: covariate demeaning did not fully converge.",
              call. = FALSE)

    CP  <- crossprod_omp_cpp(dm$M, nt)
    XtX <- CP[-1L, -1L, drop = FALSE]
    Xty <- CP[-1L, 1L]
    cd  <- chol_seq_drop_cpp(XtX, byx$d_pre, 1e-10)
    kept_x <- cd$kept

    if (length(kept_x) > 0L) {
      R_chol    <- cd$R
      delta_hat <- drop(backsolve(R_chol, forwardsolve(t(R_chol), Xty[kept_x])))
      X0_kept   <- X0_ok[, kept_x, drop = FALSE]
      y0_adj    <- y0 - as.numeric(X0_kept %*% delta_hat)
      X0_tilde  <- matrix(0, nrow = length(y0), ncol = length(kept_x))
      X0_tilde[idx_ns, ] <- dm$M[, 1L + kept_x, drop = FALSE]
    }
  }

  fe_fit <- solve_fe_2way_cpp(y0_adj[idx_ns], u0_dense - 1L, t0_dense - 1L,
                              length(u_active), length(t_active), 1e-12, 100000L)
  if (!fe_fit$converged)
    warning("BJS Step 1: 2-way FE solver did not fully converge.", call. = FALSE)

  alpha_hat <- rep(NA_real_, n_unit)
  beta_hat  <- rep(NA_real_, n_time)
  alpha_hat[u_active] <- fe_fit$alpha
  beta_hat[t_active]  <- fe_fit$beta

  # Patch singleton unit FEs (and any singleton time FEs) in closed form
  if (anyNA(alpha_hat)) {
    sel <- is.na(alpha_hat[u0]) & !is.na(beta_hat[t0])
    if (any(sel)) {
      r_sel <- y0_adj[sel] - beta_hat[t0[sel]]
      u_sel <- u0[sel]
      cnt_s <- tabulate(u_sel, n_unit)
      sum_s <- sum_by_code_cpp(u_sel, r_sel, n_unit)
      upd   <- cnt_s > 0L & is.na(alpha_hat)
      alpha_hat[upd] <- sum_s[upd] / cnt_s[upd]
    }
  }
  if (anyNA(beta_hat)) {
    sel <- is.na(beta_hat[t0]) & !is.na(alpha_hat[u0])
    if (any(sel)) {
      r_sel <- y0_adj[sel] - alpha_hat[u0[sel]]
      t_sel <- t0[sel]
      cnt_s <- tabulate(t_sel, n_time)
      sum_s <- sum_by_code_cpp(t_sel, r_sel, n_time)
      upd   <- cnt_s > 0L & is.na(beta_hat)
      beta_hat[upd] <- sum_s[upd] / cnt_s[upd]
    }
  }

  eps0 <- y0_adj - alpha_hat[u0] - beta_hat[t0]
  eps0[!keep_ns] <- 0

  # ---- Step 2: Impute Y_hat_it(0) and compute tau_hat_it (Theorem 2, Step 2)
  u1_raw <- match(omega1[[unit_chr]], u_lev)
  t1_raw <- match(omega1[[time_chr]], t_lev)
  y_hat0 <- alpha_hat[u1_raw] + beta_hat[t1_raw]
  if (length(kept_x) > 0L) {
    X1_kept <- matrix(0, nrow = nrow(omega1), ncol = length(kept_x))
    for (j in seq_along(kept_x)) {
      X1_kept[, j] <- as.numeric(omega1[[cov_names[kept_x[j]]]])
    }
    y_hat0 <- y_hat0 + as.numeric(X1_kept %*% delta_hat)
  } else {
    X1_kept <- NULL
  }

  n_na <- sum(is.na(y_hat0))
  if (n_na == nrow(omega1))
    stop("All imputed counterfactuals are NA. This usually means treated units ",
         "have no pre-treatment observations or some time periods are only ",
         "observed in the treated state. Check the panel structure.")
  if (n_na > 0L)
    warning(sprintf("%d treated observation(s) (%.1f%%) could not be imputed ",
                    n_na, 100 * n_na / nrow(omega1)),
            "due to missing FE levels; excluded from aggregation.")

  # Number of treated observations that could not be imputed,
  # retained for the result so callers can audit how much of Omega_1 was dropped.
  n_unimputed <- n_na
  n_omega1    <- nrow(omega1)

  tau_hat_all <- as.numeric(omega1[[outcome_chr]]) - y_hat0
  valid1      <- is.finite(tau_hat_all)
  if (!any(valid1))
    stop("No valid treated observations remain after counterfactual imputation.")

  omega1 <- omega1[valid1, , drop = FALSE]
  u1     <- u1_raw[valid1]
  t1     <- t1_raw[valid1]
  if (!is.null(X1_kept)) X1_kept <- X1_kept[valid1, , drop = FALSE]

  omega1$.y_hat0  <- y_hat0[valid1]
  omega1$.tau_hat <- tau_hat_all[valid1]
  omega1$.rel_t   <- as.integer(omega1[[time_chr]] - omega1[[timing_chr]])

  # ---- Step 3: Variance context (+ event-study aggregation) ----------------
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
  n_cl <- max(cl0, cl1)

  # Cohort x horizon cells (eq. 8)
  g_idx <- match(omega1[[timing_chr]], cohorts)
  cell1 <- .dense_codes(g_idx + (omega1$.rel_t - min(omega1$.rel_t)) * length(cohorts))

  S_X_cl <- if (!is.null(X0_tilde)) .sum_mat_by_code(cl0, X0_tilde * eps0, n_cl) else NULL

  vcov_ctx <- list(
    tau = omega1$.tau_hat, cell1 = cell1,
    u0 = u0, t0 = t0, u1 = u1, t1 = t1,
    cl0 = cl0, cl1 = cl1, eps0 = eps0,
    n_unit = n_unit, n_time = n_time, n_cl = n_cl,
    X0 = X0_kept, X1 = X1_kept, R_chol = R_chol, S_X_cl = S_X_cl
  )

  es <- NULL
  es_vcov <- NULL
  if (isTRUE(aggregate_es)) {
    horizons  <- sort(unique(omega1$.rel_t))
    rel_t_all <- omega1$.rel_t
    tau_all   <- omega1$.tau_hat
    wts <- lapply(horizons, function(h) {
      idx <- which(rel_t_all == h)
      list(idx = idx, w = rep(1 / length(idx), length(idx)))
    })
    se_res <- .bjs_se(vcov_ctx, wts, return_vcov = TRUE)
    es_post <- data.frame(
      relative_time = horizons,
      estimate      = vapply(wts, function(x) mean(tau_all[x$idx]), numeric(1L)),
      std_error     = se_res$se,
      stringsAsFactors = FALSE
    )
    V_post <- se_res$vcov

    if (!isFALSE(pretrends) && !is.null(pretrends)) {
      rel0 <- omega0[[time_chr]] - omega0[[timing_chr]]
      if (isTRUE(pretrends)) {
        K_pre <- sort(unique(as.integer(rel0[!is.na(rel0) & rel0 < 0L])))
        K_pre <- setdiff(K_pre, as.integer(baseline))
      } else if (is.numeric(pretrends)) {
        K_pre <- sort(unique(as.integer(pretrends)))
        if (any(K_pre >= 0L))
          stop("`pretrends` horizons must be negative integers.", call. = FALSE)
      } else {
        stop("`pretrends` must be TRUE, FALSE, or a negative integer vector.",
             call. = FALSE)
      }

      if (length(K_pre) > 0L) {
        pre_names <- paste0(".bjs_pre__", K_pre)
        X_pre <- matrix(0, nrow = nrow(omega0), ncol = length(K_pre))
        for (j in seq_along(K_pre)) {
          X_pre[!is.na(rel0) & rel0 == K_pre[j], j] <- 1
        }
        keep_pre_cols <- colSums(X_pre) > 0
        if (any(keep_pre_cols)) {
          K_pre     <- K_pre[keep_pre_cols]
          pre_names <- pre_names[keep_pre_cols]
          X_pre     <- X_pre[, keep_pre_cols, drop = FALSE]
          colnames(X_pre) <- pre_names

          X_pre_design <- if (has_cov) cbind(X_pre, X0_all) else X_pre
          cl_pre <- if (is.null(clustervars) || length(clustervars) == 0L) {
            list(omega0[[unit_chr]])
          } else if (length(clustervars) == 1L) {
            list(omega0[[clustervars]])
          } else {
            list(do.call(paste, c(lapply(clustervars, function(v) as.character(omega0[[v]])),
                                  list(sep = "__"))))
          }

          pre_fit <- .fit_fe_ols(
            y = y0_all, X = X_pre_design,
            fe_list = list(omega0[[unit_chr]], omega0[[time_chr]]),
            cluster_vals = cl_pre, vcov_type = "HC1"
          )

          hit <- match(pre_fit$tidy$term, pre_names)
          is_pre_term <- !is.na(hit)
          if (any(is_pre_term)) {
            kept_terms <- pre_fit$tidy$term[is_pre_term]
            kept_k     <- K_pre[hit[is_pre_term]]
            es_pre <- data.frame(
              relative_time = kept_k,
              estimate      = pre_fit$tidy$estimate[is_pre_term],
              std_error     = pre_fit$tidy$std.error[is_pre_term],
              stringsAsFactors = FALSE
            )
            V_pre <- pre_fit$V[kept_terms, kept_terms, drop = FALSE]

            es <- rbind(es_pre, es_post)
            n_pre  <- nrow(es_pre)
            n_post <- nrow(es_post)
            V_comb <- matrix(0, nrow = n_pre + n_post, ncol = n_pre + n_post)
            V_comb[seq_len(n_pre), seq_len(n_pre)] <- V_pre
            V_comb[n_pre + seq_len(n_post), n_pre + seq_len(n_post)] <- V_post

            ord <- order(es$relative_time)
            es  <- es[ord, , drop = FALSE]
            es_vcov <- V_comb[ord, ord, drop = FALSE]
          }
        }
      }
    }

    if (is.null(es)) {
      ord     <- order(es_post$relative_time)
      es      <- es_post[ord, , drop = FALSE]
      es_vcov <- V_post[ord, ord, drop = FALSE]
    }
    rownames(es) <- NULL
    nm_es <- as.character(es$relative_time)
    dimnames(es_vcov) <- list(nm_es, nm_es)

    conf.level <- sort(unique(conf.level))
    es <- .add_ci_columns(es, conf.level, se_col = "std_error")
  }

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
    es            = es,
    es_vcov       = es_vcov,
    tau_it        = tau_it,
    n_never       = n_never,
    cohorts       = cohorts,
    n_obs_omega0  = nrow(omega0),
    n_unimputed   = n_unimputed,
    n_treated_obs = n_omega1,
    vcov_ctx      = vcov_ctx
  )
}

# Standard errors of the aggregates sum_{it} w_it * tau_hat_it (BJS 2024,
# Theorem 3, eqs. 7-8). `wts` is a list of list(idx, w): rows of tau_it and
# their weights. Without covariates, the weights on untreated observations are
# v_it = -(z_i + z_t) with (A0'A0) z = A1'w, A0 and A1 the unit and time
# dummies of Omega_0 and Omega_1. With covariates X, the FWL Schur complement
# gives v_0 = -A0 z_A^(0) - X0_tilde z_X where
# (X0_tilde' X0_tilde) z_X = X1' w - X0' A0 z_A^(0).
.bjs_se <- function(ctx, wts, dense_max = 1000L, return_vcov = FALSE) {
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
  has_x <- !is.null(ctx$X0) && ncol(ctx$X0) > 0L

  # Treated side, one column per aggregate: cluster scores of the residuals
  # from the v^2-weighted cohort x horizon cell means (eq. 8), and A1'w.
  score <- matrix(0, ctx$n_cl, n_w)
  C_e   <- matrix(0, n_e, n_w)
  C_k   <- matrix(0, n_k, n_w)
  if (has_x) C_x <- matrix(0, ncol(ctx$X0), n_w)
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
    if (has_x) C_x[, j] <- crossprod(ctx$X1[i, , drop = FALSE], w)
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

  # Untreated side: sum_t v_it * eps_it with v_it = -(z_e + z_k) [- X0_tilde z_X].
  score <- score -
    sum_rows_by_code_cpp(ctx$cl0, e0, Z_e, ctx$n_cl, ctx$eps0) -
    sum_rows_by_code_cpp(ctx$cl0, k0, Z_k, ctx$n_cl, ctx$eps0)
  if (has_x) {
    X0_by_e <- .sum_mat_by_code(e0, ctx$X0, n_e)
    X0_by_k <- .sum_mat_by_code(k0, ctx$X0, n_k)
    rhs_x   <- C_x - crossprod(X0_by_e, Z_e) - crossprod(X0_by_k, Z_k)
    Z_X     <- backsolve(ctx$R_chol, forwardsolve(t(ctx$R_chol), rhs_x))
    score   <- score - ctx$S_X_cl %*% Z_X
  }
  se <- sqrt(colSums(score^2))
  if (isTRUE(return_vcov)) {
    return(list(se = se, vcov = crossprod(score)))
  }
  se
}
