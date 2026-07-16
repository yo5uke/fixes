# Internal FE-OLS engine: y ~ X | fe_1 + ... + fe_Q with feols-compatible
# estimation-sample handling, collinearity dropping, and VCOV.
#
# .fit_fe_ols() estimates in pure R/Rcpp (k-way demeaning via
# demean_kway_cpp(), within-OLS, and iid / hetero (HC1) / one-way-cluster
# VCOV with fixest's default small-sample corrections). Requests it cannot
# reproduce exactly — multiway clustering, non-empty `vcov_args`, or vcov
# types outside iid/hetero/HC1/White — are delegated to fixest::feols() so
# results stay identical to the previous implementation in every case.
#
# Sample handling replicated from feols:
#   - rows with NA in y, X, FE, or cluster values are dropped
#   - observations whose level in any FE dimension appears exactly once are
#     removed iteratively (fixef.rm = "perfect" default)
# Collinearity handling replicated from feols: columns are processed left to
# right and a column collinear with previously kept ones is dropped
# (keep-first); dropped columns are absent from coef/V/tidy.
#
# VCOV small-sample corrections (fixest default ssc: adj = TRUE,
# fixef.K = "nested", cluster.adj = TRUE), with
# K = #kept coefficients + [Q > 0] * (1 + sum over counted FE dims of
# (levels - 1)); FE dims nested in the cluster variable are not counted:
#   - iid:     V = RSS / (N - K) * (X'X)^-1
#   - hetero:  V = sandwich * N / (N - K)
#   - cluster: V = sandwich_G * G/(G-1) * (N-1)/(N-K)
#
# Observation weights (WLS) follow feols: rows with NA or zero weight are
# dropped, negative weights error, group means in the demeaning become
# weighted means, and [y | X] rows are scaled by sqrt(w) afterwards so the
# same crossprod/score code yields X'WX and w-weighted scores.
#
# Inference matches feols/broom: Student-t with df = G - 1 under one-way
# clustering, df = N - K (coefficients + all FE dof) otherwise.
#
# Returns list(coef, V, tidy(term/estimate/std.error/statistic/p.value),
# nobs, df.t, dropped, used, engine) and, with keep_stats = TRUE, a $stats
# list (nobs, r.squared, adj.r.squared, within.r.squared, sigma, logLik,
# AIC, BIC) matching fixest's fitstat conventions (weighted RSS/TSS for the
# R-squareds and sigma; plain Gaussian log-likelihood on unweighted
# residuals; AIC/BIC parameter count = coefficients + FE dof).

.fit_fe_ols <- function(y, X, fe_list = list(), cluster_vals = NULL,
                        vcov_type = "HC1", vcov_args = list(),
                        collin_tol = 1e-10, weights = NULL,
                        keep_stats = FALSE) {
  if (is.null(colnames(X)))
    stop("FE-OLS: X must have column names.")

  vt_ok <- is.character(vcov_type) && length(vcov_type) == 1L &&
    tolower(vcov_type) %in% c("iid", "hetero", "hc1", "white")
  if (length(vcov_args) > 0L || !vt_ok ||
      (!is.null(cluster_vals) && length(cluster_vals) > 1L))
    return(.fit_fe_ols_fixest(y, X, fe_list, cluster_vals,
                              vcov_type, vcov_args,
                              weights = weights, keep_stats = keep_stats))

  Q <- length(fe_list)
  n <- length(y)
  cl <- if (is.null(cluster_vals)) NULL else cluster_vals[[1L]]

  w <- weights
  if (!is.null(w)) {
    w <- as.numeric(w)
    if (length(w) != n)
      stop("FE-OLS: `weights` must have length ", n, ".")
    if (any(w < 0, na.rm = TRUE))
      stop("FE-OLS: negative weights are not allowed.")
  }

  # ---- Estimation sample: NA rows, then iterative singleton removal ---------
  ok <- !is.na(y)
  if (ncol(X) > 0L) ok <- ok & (rowSums(is.na(X)) == 0L)
  for (f in fe_list) ok <- ok & !is.na(f)
  if (!is.null(cl)) ok <- ok & !is.na(cl)
  if (!is.null(w)) ok <- ok & !is.na(w) & w > 0

  # Grouping is done on dense integer codes (hash-based match) rather than
  # character conversion: only group identity matters downstream, never the
  # level ordering, and integer tabulation is far cheaper at panel scale.
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
  if (N == 0L)
    stop("FE-OLS: no observations remain after removing NA rows and ",
         "singleton fixed-effect levels.")

  all_ok <- all(ok)
  yk <- if (all_ok) as.numeric(y) else as.numeric(y[ok])
  Xk <- if (all_ok) X else X[ok, , drop = FALSE]
  storage.mode(Xk) <- "double"
  wk <- if (is.null(w)) NULL else if (all_ok) w else w[ok]
  sw <- if (is.null(wk)) NULL else sqrt(wk)

  nt <- .fes_nthreads()

  # ---- Demeaning (k-way alternating projections) -----------------------------
  # With weights, demeaning subtracts weighted group means and the demeaned
  # [y | X] rows are then scaled by sqrt(w), so the crossprod/score code
  # below computes the WLS quantities unchanged. `Md` holds [y | X]; the X
  # block is only ever indexed by column, never materialized separately.
  if (Q > 0L) {
    fe_f  <- lapply(fe_code, function(code) {
      .dense_codes(if (all_ok) code else code[ok])
    })
    n_lev <- vapply(fe_f, max, integer(1L))
    fe_ids <- matrix(0L, nrow = N, ncol = Q)
    for (q in seq_len(Q)) fe_ids[, q] <- fe_f[[q]] - 1L

    dm <- demean_kway_cpp(cbind(yk, Xk), fe_ids, n_lev,
                          w = if (is.null(wk)) numeric(0) else wk,
                          tol = 1e-12, max_iter = 10000L, nthreads = nt)
    if (!dm$converged)
      warning("FE-OLS: demeaning did not fully converge; ",
              "results may be imprecise.")
    Md <- dm$M
    if (!is.null(sw)) Md <- Md * sw
    yd <- Md[, 1L]
    x_names <- colnames(Xk)
  } else {
    Xd0 <- cbind(`(Intercept)` = 1, Xk)
    if (is.null(sw)) {
      yd <- yk
    } else {
      yd <- yk * sw
      Xd0 <- Xd0 * sw
    }
    Md <- cbind(yd, Xd0)
    x_names <- colnames(Xd0)
    n_lev <- integer(0L)
    fe_f  <- list()
  }

  # ---- Within-OLS with keep-first collinearity dropping ----------------------
  # One symmetric cross product over [y | X] yields both X'X and X'y.
  CP  <- crossprod_omp_cpp(Md, nt)
  XtX <- CP[-1L, -1L, drop = FALSE]
  Xty <- CP[-1L, 1L]

  # Columns absorbed by the FEs are detected against their pre-demeaning
  # scale (their demeaned diagonal is numerically zero relative to it).
  d_pre <- if (Q > 0L) {
    if (is.null(wk)) colSums(Xk^2) else colSums(wk * Xk^2)
  } else {
    diag(XtX)
  }

  cd   <- .chol_seq_drop(XtX, d_pre = d_pre, tol = collin_tol)
  kept <- cd$kept
  if (length(kept) == 0L)
    stop("FE-OLS: all regressors were dropped as collinear ",
         "with the fixed effects.")

  Rk    <- cd$R
  b     <- drop(backsolve(Rk, forwardsolve(t(Rk), Xty[kept])))
  bread <- chol2inv(Rk)
  Xd_k  <- Md[, kept + 1L, drop = FALSE]
  e     <- yd - drop(Xd_k %*% b)

  cn <- x_names[kept]
  names(b) <- cn
  K_kept <- length(kept)

  fe_df_term <- function(counted) {
    if (Q == 0L) return(0L)
    1L + sum((n_lev - 1L)[counted])
  }

  K_full <- K_kept + fe_df_term(rep(TRUE, Q))

  use_cluster <- !is.null(cl) && identical(vcov_type, "HC1")
  if (use_cluster) {
    cl_i <- .dense_codes(cl[ok])
    G    <- max(cl_i)
    # An FE dimension is nested in the cluster variable when every level
    # maps to a single cluster.
    nested <- vapply(seq_len(Q), function(q) {
      fq <- fe_f[[q]]
      m  <- integer(n_lev[q])
      m[fq] <- cl_i
      all(m[fq] == cl_i)
    }, logical(1L))
    K_eff <- K_kept + fe_df_term(!nested)
    S     <- rowsum(Xd_k * e, cl_i)
    V <- bread %*% crossprod(S) %*% bread *
      (G / (G - 1)) * ((N - 1) / (N - K_eff))
    df_t <- G - 1
  } else {
    K_eff <- K_full
    if (tolower(vcov_type) == "iid") {
      V <- bread * sum(e^2) / (N - K_eff)
    } else {
      V <- bread %*% crossprod_omp_cpp(Xd_k * e, nt) %*% bread * N / (N - K_eff)
    }
    df_t <- N - K_full
  }
  dimnames(V) <- list(cn, cn)

  se_v   <- unname(sqrt(diag(V)))
  stat_v <- unname(b) / se_v
  tidy <- data.frame(term = cn, estimate = unname(b),
                     std.error = sqrt(diag(V)),
                     statistic = stat_v,
                     p.value   = 2 * stats::pt(-abs(stat_v), df_t),
                     stringsAsFactors = FALSE)

  fit <- list(coef = b, V = V, tidy = tidy, nobs = N, df.t = df_t,
              dropped = setdiff(x_names, cn),
              used = unname(which(ok)), engine = "internal")
  if (keep_stats) {
    rss_w  <- sum(e^2)
    rss_un <- if (is.null(wk)) rss_w else sum(e^2 / wk)
    wv     <- if (is.null(wk)) rep(1, N) else wk
    ybar   <- sum(wv * yk) / sum(wv)
    tss_w  <- sum(wv * (yk - ybar)^2)
    ll     <- -N / 2 * (log(2 * pi * rss_un / N) + 1)
    fit$stats <- list(
      nobs             = N,
      r.squared        = 1 - rss_w / tss_w,
      adj.r.squared    = 1 - (rss_w / (N - K_full)) / (tss_w / (N - 1)),
      within.r.squared = if (Q > 0L) 1 - rss_w / sum(yd^2) else NA_real_,
      sigma            = sqrt(rss_w / (N - K_full)),
      logLik           = ll,
      AIC              = -2 * ll + 2 * K_full,
      BIC              = -2 * ll + log(N) * K_full
    )
  }
  fit
}

# Strip fixest_vcov (or any other) class/attributes down to a plain numeric
# matrix so the engine's V contract is uniform across code paths.
.plain_matrix <- function(V) {
  matrix(as.numeric(V), nrow = nrow(V), dimnames = dimnames(V))
}

# Dense 1-based integer codes for a grouping vector (hash-based match).
# NA values stay NA. Level order is arbitrary; downstream code relies only
# on group identity, never on ordering.
.dense_codes <- function(v) {
  u <- unique(v)
  u <- u[!is.na(u)]
  match(v, u)
}

# Sequential Cholesky with keep-first column dropping (chol_seq_drop_cpp).
# Processes the columns of XtX in order; a column is dropped (matching
# feols's collinearity choice) when either its diagonal is numerically zero
# relative to its pre-demeaning scale `d_pre` (absorbed by the FEs) or its
# residual diagonal after projecting on the previously kept columns falls
# below tol * its own diagonal (collinear with kept columns).
.chol_seq_drop <- function(XtX, d_pre = diag(XtX), tol = 1e-10) {
  chol_seq_drop_cpp(XtX, as.numeric(d_pre), tol)
}

# Thread count for the OpenMP kernels: the `fixes.nthreads` option wins;
# under R CMD check the CRAN core limit is respected; otherwise half the
# available cores.
.fes_nthreads <- function() {
  opt <- getOption("fixes.nthreads")
  if (!is.null(opt)) return(max(1L, as.integer(opt)[1L]))
  if (nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_"))) return(2L)
  nc <- tryCatch(parallel::detectCores(logical = TRUE),
                 error = function(e) NA_integer_)
  if (is.na(nc) || nc < 2L) return(1L)
  nc %/% 2L
}

# Shared post-fit extraction for the fixest delegation paths: t-based
# inference columns (using the model's own t degrees of freedom, matching
# what broom::tidy(<fixest>) reports), the estimation-sample row indices,
# and optional fit statistics in the engine's $stats layout.
.fes_fixest_extras <- function(model, n_input, keep_stats) {
  df_t <- tryCatch(fixest::degrees_freedom(model, "t"),
                   error = function(e) NA_real_)

  used <- seq_len(n_input)
  os <- model$obs_selection
  if (!is.null(os)) for (s in os) used <- used[s]

  stats_out <- NULL
  if (keep_stats) {
    fs <- fixest::fitstat(model, ~ r2 + ar2 + wr2)
    stats_out <- list(
      nobs             = stats::nobs(model),
      r.squared        = as.numeric(fs$r2),
      adj.r.squared    = as.numeric(fs$ar2),
      within.r.squared = as.numeric(fs$wr2),
      sigma            = stats::sigma(model),
      logLik           = as.numeric(stats::logLik(model)),
      AIC              = stats::AIC(model),
      BIC              = stats::BIC(model)
    )
  }

  list(df_t = df_t, used = used, stats = stats_out)
}

# t-based statistic/p.value columns for a coefficient table; falls back to
# the normal approximation when no t dof is available.
.fes_stat_cols <- function(tidy, df_t) {
  stat_v <- tidy$estimate / tidy$std.error
  tidy$statistic <- stat_v
  tidy$p.value <- if (is.na(df_t)) 2 * stats::pnorm(-abs(stat_v)) else
    2 * stats::pt(-abs(stat_v), df_t)
  tidy
}

# fixest delegation for requests the internal path does not cover
# (multiway cluster, non-empty vcov_args, exotic vcov types). Reproduces the
# exact previous behavior: feols estimation + .model_vcov_full() precedence.
.fit_fe_ols_fixest <- function(y, X, fe_list, cluster_vals,
                               vcov_type, vcov_args,
                               weights = NULL, keep_stats = FALSE) {
  .require_fixest(
    paste0("This variance specification (multiway clustering, non-empty ",
           "`vcov_args`, or a vcov type other than iid/hetero/HC1)"),
    "use a supported specification"
  )
  fb <- data.frame(.y = y)
  fb$.X <- X
  fe_terms <- character(0L)
  for (q in seq_along(fe_list)) {
    nm <- paste0(".fe", q)
    fb[[nm]] <- fe_list[[q]]
    fe_terms <- c(fe_terms, nm)
  }
  fml <- stats::as.formula(
    if (length(fe_terms) > 0L)
      paste0(".y ~ .X | ", paste(fe_terms, collapse = " + "))
    else ".y ~ .X"
  )
  args <- list(fml, data = fb)
  if (!is.null(cluster_vals))
    args$cluster <- if (length(cluster_vals) == 1L) cluster_vals[[1L]] else
      cluster_vals
  if (!is.null(weights)) args$weights <- as.numeric(weights)

  model <- do.call(fixest::feols, args)
  V <- .plain_matrix(.model_vcov_full(
    model, vcov_type,
    cluster   = if (is.null(cluster_vals)) NULL else "supplied",
    vcov_args = vcov_args
  ))

  # feols names matrix-column coefficients "<matrix>.<col>" (".X" alone when
  # the matrix has a single column); map back to the caller's column names.
  fixn <- function(nm) {
    if (ncol(X) == 1L) ifelse(nm == ".X", colnames(X), nm)
    else ifelse(startsWith(nm, ".X"), substring(nm, 3L), nm)
  }
  cf <- stats::coef(model)
  names(cf) <- fixn(names(cf))
  dimnames(V) <- list(fixn(rownames(V)), fixn(colnames(V)))

  extras <- .fes_fixest_extras(model, length(y), keep_stats)
  tidy <- data.frame(term = names(cf), estimate = unname(cf),
                     std.error = unname(sqrt(diag(V))),
                     stringsAsFactors = FALSE)
  tidy <- .fes_stat_cols(tidy, extras$df_t)

  fit <- list(coef = cf, V = V, tidy = tidy, nobs = stats::nobs(model),
              df.t = extras$df_t,
              dropped = setdiff(colnames(X), names(cf)),
              used = extras$used, engine = "fixest")
  fit$stats <- extras$stats
  fit
}

# Resolve a user cluster specification (formula / character column names /
# value vector / list / data.frame) against `data` into a list of value
# vectors. Returns NULL when no cluster, FALSE when the spec cannot be
# resolved (callers then keep the fixest formula path).
.resolve_cluster_vals <- function(cluster, data) {
  n <- nrow(data)
  if (is.null(cluster)) return(NULL)
  if (inherits(cluster, "formula")) {
    vars <- all.vars(cluster)
    if (length(vars) == 0L || !all(vars %in% names(data))) return(FALSE)
    return(lapply(vars, function(v) data[[v]]))
  }
  if (is.data.frame(cluster)) {
    if (nrow(cluster) == n) return(as.list(cluster))
    return(FALSE)
  }
  if (is.list(cluster)) {
    if (length(cluster) > 0L &&
        all(vapply(cluster, length, integer(1L)) == n))
      return(cluster)
    return(FALSE)
  }
  if (is.character(cluster) && all(cluster %in% names(data)) &&
      length(cluster) < n)
    return(lapply(cluster, function(v) data[[v]]))
  if (length(cluster) == n) return(list(cluster))
  FALSE
}

# Parse a fixed-effects RHS string of plain column names ("id + year") into
# a named list of value vectors. Returns FALSE for anything more complex
# (e.g. "id^year"), in which case callers keep the fixest formula path.
.parse_fe_list <- function(fe_str, data) {
  if (is.null(fe_str) || !nzchar(fe_str)) return(list())
  vars <- trimws(strsplit(fe_str, "+", fixed = TRUE)[[1L]])
  if (length(vars) == 0L || any(!nzchar(vars)) ||
      !all(vars %in% names(data)))
    return(FALSE)
  out <- lapply(vars, function(v) data[[v]])
  names(out) <- vars
  out
}

# feols coefficient names for a matrix regressor block: "<prefix><col>" per
# column, or the bare prefix when the matrix has a single column.
.mat_coef_names <- function(prefix, cols) {
  if (length(cols) == 1L) prefix else paste0(prefix, cols)
}

# Undo feols's matrix-column naming on a fit object: map "<prefix><col>"
# (bare prefix for a single-column matrix) back to the matrix's own column
# names in coef, V, and tidy.
.strip_mat_prefix <- function(fit, prefix, cols) {
  fixn <- function(nm) {
    if (length(cols) == 1L) {
      ifelse(nm == prefix, cols, nm)
    } else {
      ifelse(startsWith(nm, prefix), substring(nm, nchar(prefix) + 1L), nm)
    }
  }
  names(fit$coef) <- fixn(names(fit$coef))
  dimnames(fit$V) <- list(fixn(rownames(fit$V)), fixn(colnames(fit$V)))
  fit$tidy$term   <- fixn(fit$tidy$term)
  fit
}

# Expand fixest-style i(f, x, ref) interaction dummies: one column
# x * 1(f == v) per level v of f (ascending, excluding ref), named
# "<f_name>::<v>:<x_name>" like fixest's i() coefficients.
.expand_i_dummies <- function(f_vals, x_vals, ref, f_name, x_name) {
  lv <- sort(unique(f_vals))
  lv <- lv[!(lv %in% ref)]
  M  <- matrix(0, length(f_vals), length(lv))
  for (j in seq_along(lv)) {
    sel <- f_vals == lv[j]
    M[sel, j] <- x_vals[sel]
  }
  colnames(M) <- paste0(f_name, "::", lv, ":", x_name)
  M
}

# fixest formula-path fit used when the FE string or cluster specification
# cannot be resolved into plain columns. `formula_str` must reference
# matrix columns already stored in `data` (e.g. data$.sa_X), exactly like
# the pre-engine implementation.
.fit_fe_ols_formula <- function(data, formula_str, cluster,
                                vcov_type, vcov_args,
                                weights = NULL, keep_stats = FALSE) {
  .require_fixest(
    paste0("This fixed-effects or cluster specification (beyond plain ",
           "columns, e.g. `~ id^year`)"),
    "use plain column fixed effects / clustering"
  )
  model_args <- list(stats::as.formula(formula_str), data = data)
  if (!is.null(cluster)) model_args$cluster <- cluster
  if (!is.null(weights)) model_args$weights <- weights
  model <- do.call(fixest::feols, model_args)

  V  <- .plain_matrix(.model_vcov_full(model, vcov_type, cluster, vcov_args))
  cf <- stats::coef(model)

  extras <- .fes_fixest_extras(model, nrow(data), keep_stats)
  tidy <- data.frame(term = names(cf), estimate = unname(cf),
                     std.error = unname(sqrt(diag(V))),
                     stringsAsFactors = FALSE)
  tidy <- .fes_stat_cols(tidy, extras$df_t)

  fit <- list(coef = cf, V = V, tidy = tidy, nobs = stats::nobs(model),
              df.t = extras$df_t, dropped = character(0L),
              used = extras$used, engine = "fixest-formula")
  fit$stats <- extras$stats
  fit
}
