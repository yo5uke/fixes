# Internal 2-way fixed-effects solver: Y = alpha_unit + beta_time + eps.
#
# Replaces fixest::feols(y ~ 0 | unit + time) + fixest::fixef() in BJS
# Step 1. To keep the estimation sample identical to fixest's
# (fixef.rm = "perfect" default), rows with an NA outcome are dropped and
# observations whose unit or time level appears exactly once are removed
# iteratively (dropping one singleton can create another); removed levels
# have no estimated FE and yield NA on lookup, exactly like fixef().
#
# Only the sums alpha_i + beta_t are identified — no normalization is
# applied, so alpha and beta must always be used jointly. With a
# disconnected estimation sample the sums are only comparable within a
# connected component (the same caveat applies to fixef()).
#
# Returns a list:
#   alpha     : named numeric, names = as.character(unit) levels kept
#   beta      : named numeric, names = as.character(time) levels kept
#   resid     : y - alpha - beta per input row (NA for rows not kept)
#   n_kept    : rows used in estimation
#   converged : logical
.solve_fe_2way <- function(y, unit, time, tol = 1e-12, max_iter = 100000L) {
  keep <- !is.na(y) & !is.na(unit) & !is.na(time)
  u_code <- .dense_codes(unit)
  t_code <- .dense_codes(time)

  repeat {
    idx <- which(keep)
    if (length(idx) == 0L) break
    cu <- tabulate(u_code[idx])
    ct <- tabulate(t_code[idx])
    drop_idx <- cu[u_code[idx]] == 1L | ct[t_code[idx]] == 1L
    if (!any(drop_idx)) break
    keep[idx[drop_idx]] <- FALSE
  }

  if (!any(keep))
    stop("2-way FE solver: no observations remain after removing NA rows ",
         "and singleton fixed-effect levels.")

  # The sweeps never sum across unit levels, so their order does not matter;
  # time levels are kept in sorted order.
  fu <- .chr_factor(unit[keep], sort = FALSE)
  ft <- .chr_factor(time[keep])
  y_kept <- as.numeric(y[keep])

  fit <- solve_fe_2way_cpp(
    y        = y_kept,
    unit_id  = fu$code - 1L,
    time_id  = ft$code - 1L,
    n_unit   = length(fu$levels),
    n_time   = length(ft$levels),
    tol      = tol,
    max_iter = as.integer(max_iter)
  )

  if (!fit$converged)
    warning("2-way FE solver did not converge within ", max_iter,
            " iterations (last relative change: ",
            signif(fit$last_delta, 3), ").")

  alpha <- fit$alpha
  beta  <- fit$beta
  resid <- rep(NA_real_, length(y))
  resid[keep] <- y_kept - alpha[fu$code] - beta[ft$code]
  names(alpha) <- fu$levels
  names(beta)  <- ft$levels

  list(
    alpha     = alpha,
    beta      = beta,
    resid     = resid,
    n_kept    = sum(keep),
    converged = fit$converged
  )
}

# Codes and levels of factor(as.character(x)), converting only the distinct
# values of x to character. With sort = FALSE the levels keep their order of
# first appearance.
.chr_factor <- function(x, sort = TRUE) {
  ux  <- unique(x)
  cx  <- as.character(ux)
  lev <- unique(cx)
  if (sort) lev <- sort(lev)
  list(code = match(cx, lev)[match(x, ux)], levels = lev)
}

# v[as.character(x)] without names, converting only the distinct values of x.
.chr_lookup <- function(x, v) {
  ux <- unique(x)
  unname(v)[match(as.character(ux), names(v))][match(x, ux)]
}
