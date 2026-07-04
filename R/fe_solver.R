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
#   n_kept    : rows used in estimation
#   converged : logical
.solve_fe_2way <- function(y, unit, time, tol = 1e-12, max_iter = 100000L) {
  u_chr <- as.character(unit)
  t_chr <- as.character(time)
  keep  <- !is.na(y) & !is.na(unit) & !is.na(time)

  repeat {
    cu   <- table(u_chr[keep])
    ct   <- table(t_chr[keep])
    drop <- keep & (u_chr %in% names(cu)[cu == 1L] |
                      t_chr %in% names(ct)[ct == 1L])
    if (!any(drop)) break
    keep <- keep & !drop
  }

  if (!any(keep))
    stop("2-way FE solver: no observations remain after removing NA rows ",
         "and singleton fixed-effect levels.")

  fu <- factor(u_chr[keep])
  ft <- factor(t_chr[keep])

  fit <- solve_fe_2way_cpp(
    y        = as.numeric(y[keep]),
    unit_id  = as.integer(fu) - 1L,
    time_id  = as.integer(ft) - 1L,
    n_unit   = nlevels(fu),
    n_time   = nlevels(ft),
    tol      = tol,
    max_iter = as.integer(max_iter)
  )

  if (!fit$converged)
    warning("2-way FE solver did not converge within ", max_iter,
            " iterations (last relative change: ",
            signif(fit$last_delta, 3), ").")

  alpha <- fit$alpha
  beta  <- fit$beta
  names(alpha) <- levels(fu)
  names(beta)  <- levels(ft)

  list(
    alpha     = alpha,
    beta      = beta,
    n_kept    = sum(keep),
    converged = fit$converged
  )
}
