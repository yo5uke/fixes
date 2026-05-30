# --------------------------------------------------------------------------- #
#  Honest sensitivity analysis (Rambachan & Roth 2023)                          #
#                                                                               #
#  Pure-R implementation of robust inference for event-study / DiD designs      #
#  when parallel trends may be violated.  Given event-study coefficients        #
#  beta = (beta_pre, beta_post) with covariance Sigma, the reduced-form         #
#  parameter decomposes as beta = tau + delta with tau_pre = 0 (no             #
#  anticipation) and delta a difference-in-trends bias.  Imposing               #
#  delta in Delta partially identifies theta = l' tau_post; we return           #
#  uniformly-valid confidence sets via the Andrews-Roth-Pakes (ARP)             #
#  conditional moment-inequality test (Rambachan & Roth 2023, Section 3).       #
#                                                                               #
#  Restriction families:                                                        #
#    * "smoothness"          Delta^SD(M):   |(d_{t+1}-d_t)-(d_t-d_{t-1})| <= M   #
#    * "relative_magnitude"  Delta^RM(Mbar): post second-differences bounded     #
#                            by Mbar x max pre-period first difference           #
#                            (finite union of polyhedra over the location s      #
#                            and sign of the binding pre-period difference).     #
#                                                                               #
#  Numerical engine reimplemented from the algorithms in Rambachan & Roth       #
#  (2023) and Andrews, Roth & Pakes (2023); validated against the HonestDiD     #
#  reference package (method = "Conditional") in tests/testthat/test-honest.R.  #
#                                                                               #
#  Heavy numeric helpers (LP / truncated normal) live in Suggests and are       #
#  gated with requireNamespace(): lpSolveAPI, Rglpk, TruncatedNormal, Matrix,   #
#  pracma.                                                                       #
# --------------------------------------------------------------------------- #

# ---- dependency gate -------------------------------------------------------

.honest_require <- function() {
  needed <- c("lpSolveAPI", "Rglpk", "TruncatedNormal", "Matrix", "pracma")
  miss   <- needed[!vapply(needed, requireNamespace, logical(1L),
                           quietly = TRUE)]
  if (length(miss) > 0L) {
    stop("honest_sensitivity() requires the following package(s): ",
         paste(miss, collapse = ", "),
         ".\nInstall them with install.packages(c(",
         paste0("\"", miss, "\"", collapse = ", "), ")).",
         call. = FALSE)
  }
}

# ---- restriction matrices (Rambachan & Roth 2023, Section 2.4) -------------

# Delta^SD(M): |second difference| <= M, as a polyhedron A delta <= d.
.honest_A_SD <- function(numPrePeriods, numPostPeriods) {
  k <- numPrePeriods + numPostPeriods
  Atilde <- matrix(0, nrow = k - 1L, ncol = k + 1L)
  for (r in seq_len(k - 1L)) Atilde[r, r:(r + 2L)] <- c(1, -2, 1)
  Atilde <- Atilde[, -(numPrePeriods + 1L), drop = FALSE]   # drop period 0
  rbind(Atilde, -Atilde)
}

.honest_d_SD <- function(numPrePeriods, numPostPeriods, M) {
  rep(M, nrow(.honest_A_SD(numPrePeriods, numPostPeriods)))
}

# Delta^RM(Mbar) component polyhedron for a fixed location s (the pre-period
# first difference assumed maximal) and sign (max_positive).
.honest_A_RM <- function(numPrePeriods, numPostPeriods, Mbar = 1, s,
                         max_positive = TRUE) {
  k <- numPrePeriods + numPostPeriods
  Atilde <- matrix(0, nrow = k, ncol = k + 1L)
  for (r in seq_len(k)) Atilde[r, r:(r + 1L)] <- c(-1, 1)
  v_max_dif <- matrix(0, nrow = 1L, ncol = k + 1L)
  v_max_dif[(numPrePeriods + s):(numPrePeriods + 1L + s)] <- c(-1, 1)
  if (!max_positive) v_max_dif <- -v_max_dif
  A_UB <- rbind(
    matrix(rep(v_max_dif, numPrePeriods), nrow = numPrePeriods, byrow = TRUE),
    matrix(rep(Mbar * v_max_dif, numPostPeriods), nrow = numPostPeriods,
           byrow = TRUE)
  )
  A <- rbind(Atilde - A_UB, -Atilde - A_UB)
  zerorows <- apply(A, 1L, function(x) sum(x * x)) <= 1e-10
  A <- A[!zerorows, , drop = FALSE]
  A[, -(numPrePeriods + 1L), drop = FALSE]   # drop period 0 column
}

.honest_d_RM <- function(numPrePeriods, numPostPeriods) {
  A <- .honest_A_RM(numPrePeriods, numPostPeriods, Mbar = 0, s = 0)
  rep(0, nrow(A))
}

# ---- basis / selection / change-of-basis helpers ---------------------------

.honest_basis <- function(index, size) {
  v <- matrix(0, nrow = size, ncol = 1L)
  v[index] <- 1
  v
}

.honest_selection_rows <- function(selection, size) {
  m <- matrix(0, nrow = length(selection), ncol = size)
  m[seq_along(selection), selection] <- diag(length(selection))
  m
}

# Gamma: invertible matrix with l' in the first row (change of basis for the
# nuisance representation).  Reimplements HonestDiD::.construct_Gamma.
.honest_construct_Gamma <- function(l) {
  barT <- length(l)
  B    <- cbind(l, diag(barT))
  rrefB <- pracma::rref(B)
  leading <- vapply(seq_len(nrow(rrefB)), function(i) {
    row <- rrefB[i, ]
    nz  <- which(abs(row) > 1e-10)
    if (length(nz) == 0L) NA_real_ else nz[1L]
  }, numeric(1L))
  Gamma <- t(B[, leading, drop = FALSE])
  if (abs(det(Gamma)) < .Machine$double.eps)
    stop("Failed to construct change-of-basis matrix Gamma.")
  Gamma
}

# ---- truncated-normal quantile ---------------------------------------------

# (1 - alpha-equivalent) quantile of N(mu, sd) truncated to [l, u].
.honest_norminvp <- function(p, l, u, mu = 0, sd = 1) {
  ln <- (l - mu) / sd
  un <- (u - mu) / sd
  q  <- TruncatedNormal::norminvp(p, ln, un)
  mu + q * sd
}

# ---- ID-set bounds via LP (Rambachan & Roth 2023, Lemma 2.1) ----------------

# Generic identified-set endpoints for theta = l' tau_post given a restriction
# polyhedron (A_ineq, d_ineq) and the pre-period equality delta_pre = beta_pre.
.honest_idset <- function(A_ineq, d_ineq, betahat, l_vec,
                          numPrePeriods, numPostPeriods) {
  k      <- numPrePeriods + numPostPeriods
  fDelta <- c(rep(0, numPrePeriods), l_vec)
  preEq  <- cbind(diag(numPrePeriods),
                  matrix(0, nrow = numPrePeriods, ncol = numPostPeriods))
  mat    <- rbind(A_ineq, preEq)
  rhs    <- c(d_ineq, betahat[seq_len(numPrePeriods)])
  dir    <- c(rep("<=", nrow(A_ineq)), rep("==", numPrePeriods))
  bounds <- list(lower = list(ind = seq_len(k), val = rep(-Inf, k)),
                 upper = list(ind = seq_len(k), val = rep(Inf, k)))
  rmax <- Rglpk::Rglpk_solve_LP(obj = fDelta, mat = mat, dir = dir, rhs = rhs,
                                bounds = bounds, max = TRUE)
  rmin <- Rglpk::Rglpk_solve_LP(obj = fDelta, mat = mat, dir = dir, rhs = rhs,
                                bounds = bounds, max = FALSE)
  base_post <- as.numeric(crossprod(l_vec,
                          betahat[(numPrePeriods + 1L):k]))
  if (rmax$status != 0 && rmin$status != 0) {
    return(c(lb = base_post, ub = base_post))
  }
  c(lb = base_post - rmax$optimum, ub = base_post - rmin$optimum)
}

# ---- ARP conditional test: no-nuisance / max-moment form -------------------
# (Rambachan & Roth 2023, Section 3.2.1; used when numPostPeriods == 1.)

.honest_LeeCFN <- function(eta, Sigma) {
  Sigma %*% eta / as.numeric(t(eta) %*% Sigma %*% eta)
}

.honest_VLoVUp <- function(eta, Sigma, A, b, z) {
  cc  <- .honest_LeeCFN(eta, Sigma)
  Ac  <- A %*% cc
  obj <- (b - A %*% z) / Ac
  neg <- which(Ac < 0)
  pos <- which(Ac > 0)
  VLo <- if (length(neg) == 0L) -Inf else max(obj[neg])
  VUp <- if (length(pos) == 0L)  Inf else min(obj[pos])
  c(VLo, VUp)
}

.honest_test_in_idset <- function(y, sigma, A, d, alpha) {
  sigmaTilde <- as.vector(sqrt(diag(A %*% sigma %*% t(A))))
  Atilde <- diag(1 / sigmaTilde) %*% A
  dtilde <- (1 / sigmaTilde) * d
  normMoments <- Atilde %*% y - dtilde
  maxLoc <- which.max(normMoments)
  maxMom <- normMoments[maxLoc]
  T_B   <- .honest_selection_rows(maxLoc, size = nrow(Atilde))
  iota  <- matrix(1, nrow = nrow(Atilde), ncol = 1L)
  gamma <- t(T_B %*% Atilde)
  Abar  <- Atilde - iota %*% T_B %*% Atilde
  dbar  <- (diag(nrow(Atilde)) - iota %*% T_B) %*% dtilde
  sigmabar <- sqrt(t(gamma) %*% sigma %*% gamma)
  cc   <- sigma %*% gamma / as.numeric(t(gamma) %*% sigma %*% gamma)
  z    <- (diag(length(y)) - cc %*% t(gamma)) %*% y
  vv   <- .honest_VLoVUp(eta = gamma, Sigma = sigma, A = Abar, b = dbar, z = z)
  crit <- max(0, .honest_norminvp(1 - alpha, l = vv[1], u = vv[2],
                                  mu = as.numeric(T_B %*% dtilde),
                                  sd = as.numeric(sigmabar)))
  as.numeric(maxMom + as.numeric(T_B %*% dtilde) > crit)
}

# ---- ARP conditional test: nuisance form -----------------------------------
# (Rambachan & Roth 2023, Section 3.2.1 / eqs. 14-15; numPostPeriods > 1.)

# Profiled LP statistic eta_hat (eq. 14): min eta s.t. y_T - X_T tau <= sd*eta.
.honest_lp_eta <- function(y_T, X_T, sigma) {
  dimDelta <- ncol(X_T)
  sdVec    <- sqrt(diag(sigma))
  f <- c(1, rep(0, dimDelta))
  C <- -cbind(sdVec, X_T)          # constraints C %*% (eta,tau) <= -y_T
  b <- -y_T
  lp <- lpSolveAPI::make.lp(nrow = 0L, ncol = length(f))
  lpSolveAPI::set.objfn(lp, f)
  for (r in seq_len(nrow(C)))
    lpSolveAPI::add.constraint(lp, xt = c(C[r, ]), type = "<=", rhs = b[r])
  lpSolveAPI::set.bounds(lp, lower = rep(-Inf, length(f)),
                         columns = seq_along(f))
  lpSolveAPI::lp.control(lp, sense = "min", simplextype = "dual",
                         pivoting = "dantzig", verbose = "neutral")
  err   <- lpSolveAPI::solve.lpExtPtr(lp)
  eta   <- lpSolveAPI::get.objective(lp)
  prim  <- lpSolveAPI::get.primal.solution(lp)
  delta <- prim[(length(prim) - dimDelta + 1L):length(prim)]
  dual  <- -lpSolveAPI::get.sensitivity.rhs(lp)$duals[seq_len(nrow(C))]
  list(eta_star = eta, delta_star = delta, lambda = dual, error_flag = err)
}

.honest_roundeps <- function(x, eps = .Machine$double.eps^(3/4)) {
  if (abs(x) < eps) 0 else x
}

.honest_max_program <- function(s_T, gamma_tilde, sigma, W_T, c) {
  f   <- s_T + as.numeric(t(gamma_tilde) %*% sigma %*% gamma_tilde)^(-1) *
         (sigma %*% gamma_tilde) * c
  Aeq <- t(W_T)
  beq <- c(1, rep(0, nrow(Aeq) - 1L))
  Rglpk::Rglpk_solve_LP(obj = -c(f), mat = Aeq,
                        dir = rep("==", nrow(Aeq)), rhs = beq)
}

.honest_check_solution <- function(c, tol, s_T, gamma_tilde, sigma, W_T) {
  lp <- .honest_max_program(s_T, gamma_tilde, sigma, W_T, c)
  lp$honestsolution <- (abs(c - (-lp$optimum)) <= tol)
  lp
}

.honest_vlo_vup_dual <- function(eta, s_T, gamma_tilde, sigma, W_T) {
  tol_c <- 1e-6; tol_eq <- 1e-6
  sigma_B <- sqrt(t(gamma_tilde) %*% sigma %*% gamma_tilde)
  low_initial  <- min(-100, eta - 20 * sigma_B)
  high_initial <- max( 100, eta + 20 * sigma_B)
  maxiters <- 10000L; switchiters <- 10L
  chk <- .honest_check_solution(eta, tol_eq, s_T, gamma_tilde, sigma, W_T)
  if (is.na(chk$honestsolution) || !chk$honestsolution)
    return(list(vlo = eta, vup = Inf))
  b <- as.numeric(t(gamma_tilde) %*% sigma %*% gamma_tilde)^(-1) *
       (sigma %*% gamma_tilde)
  # ---- vup
  if ((lp <- .honest_check_solution(high_initial, tol_eq, s_T, gamma_tilde,
                                    sigma, W_T))$honestsolution) {
    vup <- Inf
  } else {
    dif <- 0; iters <- 1L
    mid <- (.honest_roundeps(lp$solution %*% s_T) /
              (1 - lp$solution %*% b))[1L]
    while (!(lp <- .honest_check_solution(mid, tol_eq, s_T, gamma_tilde,
                                          sigma, W_T))$honestsolution &&
           iters < maxiters) {
      iters <- iters + 1L
      if (iters >= switchiters) { dif <- tol_c + 1; break }
      mid <- (.honest_roundeps(lp$solution %*% s_T) /
                (1 - lp$solution %*% b))[1L]
    }
    low <- eta; high <- mid
    while (dif > tol_c && iters < maxiters) {
      iters <- iters + 1L
      mid <- (high + low) / 2
      if (.honest_check_solution(mid, tol_eq, s_T, gamma_tilde, sigma,
                                 W_T)$honestsolution) low <- mid else high <- mid
      dif <- high - low
    }
    vup <- mid
  }
  # ---- vlo
  if ((lp <- .honest_check_solution(low_initial, tol_eq, s_T, gamma_tilde,
                                    sigma, W_T))$honestsolution) {
    vlo <- -Inf
  } else {
    dif <- 0; iters <- 1L
    mid <- (.honest_roundeps(lp$solution %*% s_T) /
              (1 - lp$solution %*% b))[1L]
    while (!(lp <- .honest_check_solution(mid, tol_eq, s_T, gamma_tilde,
                                          sigma, W_T))$honestsolution &&
           iters < maxiters) {
      iters <- iters + 1L
      if (iters >= switchiters) { dif <- tol_c + 1; break }
      mid <- (.honest_roundeps(lp$solution %*% s_T) /
                (1 - lp$solution %*% b))[1L]
    }
    low <- mid; high <- eta
    while (dif > tol_c && iters < maxiters) {
      mid <- (low + high) / 2; iters <- iters + 1L
      if (.honest_check_solution(mid, tol_eq, s_T, gamma_tilde, sigma,
                                 W_T)$honestsolution) high <- mid else low <- mid
      dif <- high - low
    }
    vlo <- mid
  }
  list(vlo = vlo, vup = vup)
}

.honest_lp_dual <- function(y_T, X_T, eta, gamma_tilde, sigma) {
  sdVec <- sqrt(diag(sigma))
  W_T   <- cbind(sdVec, X_T)
  s_T <- (diag(length(y_T)) -
            as.numeric(t(gamma_tilde) %*% sigma %*% gamma_tilde)^(-1) *
            (sigma %*% (gamma_tilde %*% t(gamma_tilde)))) %*% y_T
  v <- .honest_vlo_vup_dual(eta, s_T, gamma_tilde, sigma, W_T)
  list(vlo = v$vlo, vup = v$vup, eta = eta, gamma_tilde = gamma_tilde)
}

# Conditional test for H0: theta given nuisance, ARP form (mod_size = alpha).
.honest_lp_cond_test <- function(y_T, X_T, sigma, alpha) {
  if (is.vector(X_T)) X_T <- matrix(X_T, ncol = 1L)
  M <- nrow(sigma); k <- ncol(X_T)
  linSoln <- .honest_lp_eta(y_T, X_T, sigma)
  if (linSoln$error_flag > 0) return(0)         # did not converge -> no reject

  tol_lambda <- 1e-6
  B_index  <- (linSoln$lambda > tol_lambda)
  degenerate <- (sum(B_index) != (k + 1L))
  X_TB <- matrix(X_T[B_index, ], ncol = ncol(X_T))
  Xdim <- min(dim(X_TB))
  fullRank <- if (is.vector(X_TB) || Xdim == 0L) FALSE else
    (as.integer(Matrix::rankMatrix(X_TB)) == Xdim)

  if (!fullRank || degenerate) {
    dual <- .honest_lp_dual(y_T, X_T, linSoln$eta_star, linSoln$lambda, sigma)
    s2 <- as.numeric(t(dual$gamma_tilde) %*% sigma %*% dual$gamma_tilde)
    if (abs(s2) < .Machine$double.eps)
      return(as.numeric(linSoln$eta_star > 0))
    sB <- sqrt(s2)
    maxstat <- linSoln$eta / sB
    zlo <- dual$vlo / sB; zup <- dual$vup / sB
    if (!(zlo <= maxstat && maxstat <= zup)) return(0)
    cval <- max(0, .honest_norminvp(1 - alpha, l = zlo, u = zup))
    return(as.numeric(maxstat > cval))
  }

  # full-rank, non-degenerate primal vertex: closed-form conditioning
  Bc_index <- !B_index
  sdVec <- sqrt(diag(sigma))
  sdVec_B <- sdVec[B_index]; sdVec_Bc <- sdVec[Bc_index]
  X_TBc <- X_T[Bc_index, , drop = FALSE]
  S_B  <- diag(M)[B_index, , drop = FALSE]
  S_Bc <- diag(M)[Bc_index, , drop = FALSE]
  WB_inv <- solve(cbind(sdVec_B, X_TB))
  Gamma_B <- cbind(sdVec_Bc, X_TBc) %*% WB_inv %*% S_B - S_Bc
  e1  <- c(1, rep(0, sum(B_index) - 1L))
  v_B <- t(t(e1) %*% WB_inv %*% S_B)
  sigma2_B <- as.numeric(t(v_B) %*% sigma %*% v_B)
  sigma_B  <- sqrt(sigma2_B)
  rho <- Gamma_B %*% sigma %*% v_B / sigma2_B
  mm  <- (-Gamma_B %*% y_T) / rho + as.numeric(t(v_B) %*% y_T)
  vlo <- if (sum(rho > 0) > 0) max(mm[rho > 0]) else -Inf
  vup <- if (sum(rho < 0) > 0) min(mm[rho < 0]) else  Inf
  zlo <- vlo / sigma_B; zup <- vup / sigma_B
  maxstat <- linSoln$eta_star / sigma_B
  if (!(zlo <= maxstat && maxstat <= zup)) return(0)
  cval <- max(0, .honest_norminvp(1 - alpha, l = zlo, u = zup))
  as.numeric(maxstat > cval)
}

# ---- confidence set over a theta grid for one polyhedron -------------------

# No-nuisance grid inversion (numPostPeriods == 1).
.honest_cs_nonuis <- function(betahat, sigma, A, d, numPrePeriods,
                              alpha, grid) {
  k <- length(betahat)
  accept <- vapply(grid, function(theta) {
    r <- tryCatch({
      y <- betahat - .honest_basis(numPrePeriods + 1L, k) * theta
      .honest_test_in_idset(y, sigma, A, d, alpha)
    }, error = function(e) 1)
    if (!is.finite(r)) r <- 1               # non-finite -> reject (not in CI)
    1 - r
  }, numeric(1L))
  data.frame(grid = grid, accept = accept)
}

# Nuisance grid inversion (numPostPeriods > 1), ARP conditional.
.honest_cs_nuis <- function(betahat, sigma, numPrePeriods, numPostPeriods,
                            A, d, l_vec, alpha, grid, rowsForARP) {
  k <- numPrePeriods + numPostPeriods
  Gamma <- .honest_construct_Gamma(l_vec)
  AGammaInv <- A[, (numPrePeriods + 1L):k, drop = FALSE] %*% solve(Gamma)
  AGI_one   <- AGammaInv[, 1L]
  AGI_minus <- AGammaInv[, -1L, drop = FALSE]
  Y      <- as.numeric(A %*% betahat - d)
  sigmaY <- A %*% sigma %*% t(A)
  yR  <- function(theta) (Y - AGI_one * theta)[rowsForARP]
  XR  <- AGI_minus[rowsForARP, , drop = FALSE]
  sR  <- sigmaY[rowsForARP, rowsForARP, drop = FALSE]
  accept <- vapply(grid, function(theta) {
    r <- tryCatch(.honest_lp_cond_test(yR(theta), XR, sR, alpha),
                  error = function(e) 1)
    if (!is.finite(r)) r <- 1               # non-finite -> reject (not in CI)
    1 - r
  }, numeric(1L))
  data.frame(grid = grid, accept = accept)
}

# ---- restriction-family CS wrappers ----------------------------------------

# sd(theta) = sqrt(l' Sigma_post l), used to set default grid widths.
.honest_sdTheta <- function(sigma, l_vec, numPrePeriods, numPostPeriods) {
  k <- numPrePeriods + numPostPeriods
  sqrt(as.numeric(
    t(l_vec) %*% sigma[(numPrePeriods + 1L):k, (numPrePeriods + 1L):k,
                       drop = FALSE] %*% l_vec))
}

.honest_cs_SD <- function(betahat, sigma, numPrePeriods, numPostPeriods,
                          l_vec, M, alpha, gridPoints) {
  A <- .honest_A_SD(numPrePeriods, numPostPeriods)
  d <- .honest_d_SD(numPrePeriods, numPostPeriods, M)
  # Grid bounds: identified set evaluated at beta = 0, padded by 20*sd(theta)
  # (matches HonestDiD's grid construction).
  id0 <- .honest_idset(A, d, rep(0, numPrePeriods + numPostPeriods), l_vec,
                       numPrePeriods, numPostPeriods)
  sdT  <- .honest_sdTheta(sigma, l_vec, numPrePeriods, numPostPeriods)
  grid <- seq(id0["lb"] - 20 * sdT, id0["ub"] + 20 * sdT,
              length.out = gridPoints)
  if (numPostPeriods == 1L) {
    res <- .honest_cs_nonuis(betahat, sigma, A, d, numPrePeriods, alpha, grid)
  } else {
    # post-period moments only for the ARP rows (matches HonestDiD default)
    postIdx  <- (numPrePeriods + 1L):ncol(A)
    rowsForARP <- which(rowSums(A[, postIdx, drop = FALSE] != 0) > 0)
    res <- .honest_cs_nuis(betahat, sigma, numPrePeriods, numPostPeriods,
                           A, d, l_vec, alpha, grid, rowsForARP)
  }
  .honest_ci_from_grid(res)
}

.honest_cs_RM <- function(betahat, sigma, numPrePeriods, numPostPeriods,
                          l_vec, Mbar, alpha, gridPoints) {
  # Delta^RM(Mbar) = union over s in pre-periods and sign of component polyhedra.
  # Grid is centred at 0 with half-width 20*sd(theta) (matches HonestDiD).
  s_vals  <- (-(numPrePeriods - 1L)):0
  signs   <- c(TRUE, FALSE)
  sdT  <- .honest_sdTheta(sigma, l_vec, numPrePeriods, numPostPeriods)
  grid <- seq(-20 * sdT, 20 * sdT, length.out = gridPoints)
  accept <- rep(0, length(grid))
  for (s in s_vals) for (sg in signs) {
    A <- .honest_A_RM(numPrePeriods, numPostPeriods, Mbar = Mbar, s = s,
                      max_positive = sg)
    d <- rep(0, nrow(A))                    # RM rhs is all zeros
    if (numPostPeriods == 1L) {
      r <- .honest_cs_nonuis(betahat, sigma, A, d, numPrePeriods, alpha, grid)
    } else {
      r <- .honest_cs_nuis(betahat, sigma, numPrePeriods, numPostPeriods,
                           A, d, l_vec, alpha, grid, seq_len(nrow(A)))
    }
    accept <- pmax(accept, r$accept)          # union of confidence sets
  }
  .honest_ci_from_grid(data.frame(grid = grid, accept = accept))
}

# Convex hull (lb, ub) of accepted grid points.
.honest_ci_from_grid <- function(res) {
  acc <- res$grid[res$accept == 1]
  if (length(acc) == 0L) return(c(lb = NA_real_, ub = NA_real_))
  c(lb = min(acc), ub = max(acc))
}

# --------------------------------------------------------------------------- #
#  User-facing API                                                             #
# --------------------------------------------------------------------------- #

#' Honest sensitivity analysis for parallel-trends violations
#'
#' @description
#' Robust inference and sensitivity analysis for event-study / DiD designs
#' following Rambachan and Roth (2023).  Instead of assuming parallel trends
#' holds exactly, it asks how large a violation of parallel trends would have
#' to be before the causal conclusion changes, returning confidence sets for a
#' post-treatment effect under a sequence of restrictions on the possible
#' difference in trends.
#'
#' Two restriction families are supported:
#' \itemize{
#'   \item \code{"relative_magnitude"} (\eqn{\Delta^{RM}(\bar M)}): post-treatment
#'     violations are at most \eqn{\bar M} times the largest pre-treatment
#'     violation (Rambachan & Roth 2023, Section 2.4.1).
#'   \item \code{"smoothness"} (\eqn{\Delta^{SD}(M)}): the difference in trends
#'     deviates from linearity by at most \eqn{M} per period (Section 2.4.3).
#' }
#'
#' Inference uses the Andrews-Roth-Pakes (ARP) conditional moment-inequality
#' test (Section 3.2.1), which is uniformly valid and recommended for general
#' restriction sets.
#'
#' @param object An `es_result` from [run_es()] (with `estimator = "twfe"`,
#'   i.e. classic or `method = "sunab"`, which carry the event-study coefficient
#'   covariance).  Alternatively, supply `betahat` and `sigma` directly (see
#'   below) for any estimator.
#' @param type Restriction family: `"relative_magnitude"` (default) or
#'   `"smoothness"`.
#' @param Mvec Numeric vector of restriction parameters.  For
#'   `"relative_magnitude"` these are \eqn{\bar M} values (default
#'   `seq(0, 2, by = 0.5)`); for `"smoothness"` these are \eqn{M} values
#'   (default a data-driven sequence from 0 to the largest pre-period SD).
#' @param l_vec Numeric weight vector over post-treatment periods defining the
#'   target \eqn{\theta = l'\tau_{post}}.  Defaults to the first post-treatment
#'   period.
#' @param alpha Significance level (default `0.05` for 95% confidence sets).
#' @param gridPoints Number of grid points for test inversion (default `1000`).
#' @param betahat Optional event-study coefficient vector (pre then post,
#'   excluding the reference period), ordered by relative time.  Required when
#'   `object` does not carry an `es_vcov` attribute.
#' @param sigma Optional covariance matrix of `betahat`.
#' @param numPrePeriods,numPostPeriods Optional integer counts; inferred from
#'   `object` or from `betahat`/`l_vec` when omitted.
#'
#' @return A `data.frame` of class `"honest_result"` with one row per
#'   restriction value plus the original (parallel-trends) confidence interval,
#'   with columns `M`, `lb`, `ub`, `method`, and `type`.  The breakdown value
#'   (largest restriction at which the robust CI still excludes 0) is stored in
#'   `attr(., "breakdown")`.
#'
#' @references
#' Rambachan, A. and Roth, J. (2023). A More Credible Approach to Parallel
#' Trends. *Review of Economic Studies*, 90(5), 2555-2591.
#'
#' @seealso [run_es()], [plot_honest()]
#' @export
honest_sensitivity <- function(object = NULL,
                               type = c("relative_magnitude", "smoothness"),
                               Mvec = NULL,
                               l_vec = NULL,
                               alpha = 0.05,
                               gridPoints = 1000L,
                               betahat = NULL,
                               sigma = NULL,
                               numPrePeriods = NULL,
                               numPostPeriods = NULL) {
  type <- match.arg(type)
  .honest_require()

  # ---- resolve betahat / sigma / pre-post counts ---------------------------
  if (is.null(betahat) || is.null(sigma)) {
    if (is.null(object) || !inherits(object, "es_result"))
      stop("Supply either an `es_result` `object`, or both `betahat` and ",
           "`sigma`.")
    V <- attr(object, "es_vcov")
    if (is.null(V))
      stop("This `es_result` does not carry an event-study covariance ",
           "(`es_vcov`). honest_sensitivity() currently supports ",
           "`estimator = \"twfe\"` (classic or `method = \"sunab\"`). ",
           "For other estimators, pass `betahat` and `sigma` directly.")
    rel <- as.integer(rownames(V))
    ord <- order(rel)
    rel <- rel[ord]
    sigma   <- V[ord, ord, drop = FALSE]
    nb      <- object[!object$is_baseline, , drop = FALSE]
    betahat <- nb$estimate[match(rel, nb$relative_time)]
    # run_es() omits the baseline (reference) period; Rambachan-Roth treat that
    # omitted period as their normalized "period 0".  Split pre/post around it
    # so relative_time == 0 (first treated period) counts as a post period.
    baseline <- attr(object, "baseline")
    if (is.null(baseline)) baseline <- -1L
    numPrePeriods  <- sum(rel < baseline)
    numPostPeriods <- sum(rel > baseline)
    if (numPrePeriods + numPostPeriods != length(rel))
      stop("Event-study relative times are not consecutive around the ",
           "baseline; honest_sensitivity() needs a contiguous event window.")
  }
  betahat <- as.numeric(betahat)
  sigma   <- as.matrix(sigma)
  k <- length(betahat)
  if (is.null(numPrePeriods) || is.null(numPostPeriods)) {
    stop("`numPrePeriods` and `numPostPeriods` must be supplied with ",
         "`betahat`/`sigma`.")
  }
  if (numPrePeriods + numPostPeriods != k)
    stop("numPrePeriods + numPostPeriods must equal length(betahat).")
  if (numPostPeriods < 1L) stop("Need at least one post-treatment period.")
  if (numPrePeriods < 1L)
    stop("Honest sensitivity requires at least one pre-treatment period.")

  if (is.null(l_vec)) l_vec <- as.numeric(.honest_basis(1L, numPostPeriods))
  l_vec <- as.numeric(l_vec)
  if (length(l_vec) != numPostPeriods)
    stop("`l_vec` must have length numPostPeriods (", numPostPeriods, ").")

  # ---- default M grid ------------------------------------------------------
  if (is.null(Mvec)) {
    if (type == "relative_magnitude") {
      Mvec <- seq(0, 2, by = 0.5)
    } else {
      # data-driven: 0 .. ~ largest pre-period second-difference SD
      base_sd <- sqrt(max(diag(sigma)[seq_len(numPrePeriods)]))
      Mvec <- seq(0, 2 * base_sd, length.out = 5L)
    }
  }
  Mvec <- sort(unique(as.numeric(Mvec)))

  # ---- original (parallel-trends) CI ---------------------------------------
  post_idx <- (numPrePeriods + 1L):k
  theta_hat <- as.numeric(crossprod(l_vec, betahat[post_idx]))
  se_theta  <- sqrt(as.numeric(
    t(l_vec) %*% sigma[post_idx, post_idx, drop = FALSE] %*% l_vec))
  z <- stats::qnorm(1 - alpha / 2)
  orig <- data.frame(
    M = NA_real_, lb = theta_hat - z * se_theta, ub = theta_hat + z * se_theta,
    method = "Original", type = "Original", stringsAsFactors = FALSE
  )

  # ---- robust CIs over the M grid ------------------------------------------
  rows <- lapply(Mvec, function(M) {
    ci <- if (type == "smoothness") {
      .honest_cs_SD(betahat, sigma, numPrePeriods, numPostPeriods, l_vec,
                    M = M, alpha = alpha, gridPoints = gridPoints)
    } else {
      .honest_cs_RM(betahat, sigma, numPrePeriods, numPostPeriods, l_vec,
                    Mbar = M, alpha = alpha, gridPoints = gridPoints)
    }
    data.frame(M = M, lb = ci["lb"], ub = ci["ub"],
               method = "C-ARP", type = type, stringsAsFactors = FALSE)
  })
  robust <- do.call(rbind, rows)
  rownames(robust) <- NULL

  out <- rbind(orig, robust)
  rownames(out) <- NULL

  # ---- breakdown value: largest M with robust CI excluding 0 ---------------
  excl <- robust$M[!is.na(robust$lb) & !is.na(robust$ub) &
                     (robust$lb > 0 | robust$ub < 0)]
  breakdown <- if (length(excl) > 0L) max(excl) else NA_real_

  attr(out, "type")           <- type
  attr(out, "alpha")          <- alpha
  attr(out, "l_vec")          <- l_vec
  attr(out, "numPrePeriods")  <- numPrePeriods
  attr(out, "numPostPeriods") <- numPostPeriods
  attr(out, "theta_hat")      <- theta_hat
  attr(out, "breakdown")      <- breakdown
  class(out) <- c("honest_result", "data.frame")
  out
}

#' @export
print.honest_result <- function(x, digits = 3L, ...) {
  type <- attr(x, "type")
  bd   <- attr(x, "breakdown")
  lab  <- if (identical(type, "smoothness"))
    "Smoothness  (Delta^SD(M))" else "Relative magnitudes  (Delta^RM(Mbar))"
  cat(sprintf("Honest sensitivity analysis  [%s]\n", lab))
  cat(sprintf("Method: ARP conditional | alpha = %.3g\n", attr(x, "alpha")))
  if (!is.na(bd)) {
    cat(sprintf("Breakdown value: %s = %.3g (largest value with CI excluding 0)\n",
                if (identical(type, "smoothness")) "M" else "Mbar", bd))
  } else {
    cat("Breakdown value: robust CI includes 0 at all reported values\n")
  }
  cat("\n")
  print(as.data.frame(x), digits = digits, ...)
  invisible(x)
}
