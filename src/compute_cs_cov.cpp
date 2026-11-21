// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

using namespace Rcpp;

namespace {

// As in DRDID, fitted propensity scores are capped at 1 - 1e-6 and control
// units with a score at or above 0.995 receive zero weight in the ATT.
constexpr double kPsCap     = 1.0 - 1e-6;
constexpr double kTrimLevel = 0.995;

struct CholFactor {
  std::vector<int> kept;
  arma::mat R; // upper triangular k x k such that R' * R = M(kept, kept)
};

// Sequential keep-first Cholesky factorisation of symmetric M (K x K).
// Column 0 (intercept) is kept whenever M(0, 0) > 0; subsequent columns are
// dropped when their residual diagonal falls below tol * M(j, j).
CholFactor seq_chol_drop(const arma::mat& M, double tol = 1e-10) {
  int K = (int)M.n_cols;
  arma::mat Rbuf(K, K, arma::fill::zeros);
  std::vector<int> kept;
  kept.reserve(K);
  std::vector<double> z(K);

  for (int j = 0; j < K; ++j) {
    double d0j = M(j, j);
    if (!std::isfinite(d0j) || d0j <= 1e-14) continue;

    int a_n = (int)kept.size();
    double d = d0j;
    for (int a = 0; a < a_n; ++a) {
      int i = kept[a];
      double s = M(i, j);
      for (int m = 0; m < a; ++m) s -= Rbuf(m, i) * z[m];
      z[a] = s / Rbuf(a, i);
      d -= z[a] * z[a];
    }
    if (d <= tol * d0j) continue;

    for (int m = 0; m < a_n; ++m) Rbuf(m, j) = z[m];
    Rbuf(a_n, j) = std::sqrt(d);
    kept.push_back(j);
  }

  int k = (int)kept.size();
  arma::mat R_out(k, k, arma::fill::zeros);
  for (int a = 0; a < k; ++a) {
    int col = kept[a];
    for (int m = 0; m <= a; ++m) R_out(m, a) = Rbuf(m, col);
  }
  return {kept, R_out};
}

// Solve (R' * R) * x = b where R is upper triangular (k x k).
arma::vec chol_solve_upper(const arma::mat& R, const arma::vec& b) {
  int k = (int)R.n_cols;
  arma::vec y(k, arma::fill::zeros);
  for (int i = 0; i < k; ++i) {
    double s = b[i];
    for (int m = 0; m < i; ++m) s -= R(m, i) * y[m];
    y[i] = s / R(i, i);
  }
  arma::vec x(k, arma::fill::zeros);
  for (int i = k - 1; i >= 0; --i) {
    double s = y[i];
    for (int m = i + 1; m < k; ++m) s -= R(i, m) * x[m];
    x[i] = s / R(i, i);
  }
  return x;
}

// Extract columns `kept` of `X` into a new matrix.
arma::mat subset_cols(const arma::mat& X, const std::vector<int>& kept) {
  int n = (int)X.n_rows;
  int k = (int)kept.size();
  arma::mat out(n, k);
  for (int a = 0; a < k; ++a) {
    out.col(a) = X.col(kept[a]);
  }
  return out;
}

struct PropensityCache {
  bool valid = false;
  bool not_converged = false;
  std::vector<int> kept;
  arma::vec p;        // length n_sub
  arma::vec w0;       // normalized Hajek control weights (length n_sub)
  arma::mat X_sub_k;  // n_sub x k_ps
  arma::mat R_H;      // Cholesky factor of H = (1/n_sub) X_k' diag(v p(1-p)) X_k
  bool overlap_warn = false;
};

struct OrCache {
  bool valid = false;
  std::vector<int> kept;
  std::vector<int> ctrl_idx; // row indices where C > 0.5
  arma::vec v_c;             // control unit weights
  arma::mat X_sub_k;         // n_sub x k_or
  arma::mat X_c_k;           // n_ct x k_or
  arma::mat R_Xc;            // Cholesky factor of (1/n_sub) X_C_k' diag(v_c) X_C_k
};

struct IptCache {
  bool valid = false;
  bool not_converged = false;
  std::vector<int> kept;
  std::vector<int> ctrl_idx; // row indices where C > 0.5
  arma::vec p;               // length n_sub
  arma::vec w0;              // trimmed control odds weights / mean(v D) (length n_sub)
  arma::vec w_wls_c;         // control WLS weights: v_c * p_c / (1 - p_c)
  arma::mat X_sub_k;         // n_sub x k_wls
  arma::mat X_c_k;           // n_ct x k_wls
  arma::mat R_wls;           // Cholesky factor of (1/n_sub) X_c_k' diag(w_wls_c) X_c_k
  bool overlap_warn = false;
};

// Damped Newton-Raphson weighted logistic regression of D on X_sub (n_sub x K).
PropensityCache fit_logit_sub(const arma::mat& X_sub, const arma::vec& D,
                              const arma::vec& C, const arma::vec& v) {
  PropensityCache res;
  int n = (int)X_sub.n_rows;
  if (n == 0) return res;
  double sum_vD = arma::dot(v, D);
  double sum_vC = arma::dot(v, C);
  if (sum_vD <= 0.0 || sum_vC <= 0.0) return res;

  arma::mat XtX = X_sub.t() * X_sub;
  CholFactor cf = seq_chol_drop(XtX, 1e-10);
  int k = (int)cf.kept.size();
  if (k == 0) return res;

  arma::mat Xk = subset_cols(X_sub, cf.kept);
  double d_bar = std::min(1.0 - 1e-6, std::max(1e-6, sum_vD / arma::sum(v)));
  arma::vec beta(k, arma::fill::zeros);
  beta[0] = std::log(d_bar / (1.0 - d_bar));

  auto loglik_fn = [&](const arma::vec& eta) -> double {
    double ll = 0.0;
    for (int i = 0; i < n; ++i) {
      double e = eta[i];
      double l1p = (e > 0.0) ? (e + std::log1p(std::exp(-e))) : std::log1p(std::exp(e));
      ll += v[i] * (D[i] * e - l1p);
    }
    return ll;
  };

  arma::vec eta = Xk * beta;
  double ll = loglik_fn(eta);
  arma::vec p(n), w(n), grad(k), step(k);
  arma::mat H(k, k);
  // Iterations follow glm / fastglm (used by DRDID with tol = 1e-8,
  // maxit = 100): beyond |eta| = 30 the logit link holds the mean at machine
  // epsilon from 0 or 1 and the working weight at epsilon, and the fit has
  // converged once the deviance changes by less than 1e-8 * (|deviance| + 0.1)
  // in one step.
  bool converged = false;
  const double eps = std::numeric_limits<double>::epsilon();

  for (int iter = 0; iter < 100; ++iter) {
    for (int i = 0; i < n; ++i) {
      double e = eta[i];
      double pi, wi;
      if (e > 30.0) {
        pi = 1.0 - eps;
        wi = eps;
      } else if (e < -30.0) {
        pi = eps;
        wi = eps;
      } else {
        pi = 1.0 / (1.0 + std::exp(-e));
        wi = pi * (1.0 - pi);
      }
      p[i] = pi;
      w[i] = v[i] * wi;
    }
    grad = Xk.t() * (v % (D - p));
    if (arma::max(arma::abs(grad)) / n < 1e-13) {
      converged = true;
      break;
    }

    arma::mat Xw = Xk.each_col() % w;
    H = Xk.t() * Xw;
    bool ok = arma::solve(step, H, grad, arma::solve_opts::fast) && step.is_finite();
    if (!ok) {
      H.diag() += 1e-10 * (double)n;
      ok = arma::solve(step, H, grad, arma::solve_opts::fast) && step.is_finite();
      if (!ok) break;
    }

    double alpha = 1.0;
    arma::vec beta_new, eta_new;
    double ll_new = ll;
    bool accepted = false;
    for (int ls = 0; ls < 25; ++ls) {
      beta_new = beta + alpha * step;
      eta_new = Xk * beta_new;
      ll_new = loglik_fn(eta_new);
      if (std::isfinite(ll_new) && ll_new >= ll - 1e-14 * (1.0 + std::abs(ll))) {
        accepted = true;
        break;
      }
      alpha *= 0.5;
    }
    if (!accepted) break;

    double max_chg = arma::max(arma::abs(alpha * step));
    double dev_change = 2.0 * std::abs(ll_new - ll);
    if (dev_change < 1e-8 * (2.0 * std::abs(ll_new) + 0.1)) converged = true;
    beta = beta_new;
    eta = eta_new;
    ll = ll_new;
    if (max_chg < 1e-13 && arma::max(arma::abs(grad)) / n < 1e-13) break;
  }

  res.not_converged = !converged;

  arma::vec w0_raw(n);
  for (int i = 0; i < n; ++i) {
    double e = std::min(30.0, std::max(-30.0, eta[i]));
    double pi = std::min(1.0 / (1.0 + std::exp(-e)), kPsCap);
    p[i] = pi;
    if (D[i] < 0.5 && pi >= kPsCap) res.overlap_warn = true;
    w[i] = v[i] * pi * (1.0 - pi);
    double trim = (C[i] > 0.5 && pi >= kTrimLevel) ? 0.0 : 1.0;
    w0_raw[i] = trim * v[i] * C[i] * pi / (1.0 - pi);
  }

  double w0_mean = arma::mean(w0_raw);
  if (!std::isfinite(w0_mean) || w0_mean <= 0.0) return res;

  arma::mat Xw = Xk.each_col() % w;
  arma::mat H_avg = (Xk.t() * Xw) / (double)n;
  CholFactor cf_H = seq_chol_drop(H_avg, 1e-12);
  if ((int)cf_H.kept.size() < k) {
    H_avg.diag() += 1e-10;
    cf_H = seq_chol_drop(H_avg, 1e-12);
  }
  if (cf_H.kept.empty()) return res;

  std::vector<int> kept_final;
  kept_final.reserve(cf_H.kept.size());
  for (int idx : cf_H.kept) kept_final.push_back(cf.kept[idx]);

  res.valid = true;
  res.kept = kept_final;
  res.p = p;
  res.w0 = w0_raw / w0_mean;
  res.X_sub_k = subset_cols(X_sub, kept_final);
  res.R_H = cf_H.R;
  return res;
}

OrCache fit_or_cache(const arma::mat& X_sub, const arma::vec& C,
                     const arma::vec& v) {
  OrCache res;
  int n_sub = (int)X_sub.n_rows;
  if (n_sub <= 0) return res;

  std::vector<int> ctrl_idx;
  ctrl_idx.reserve(n_sub);
  for (int i = 0; i < n_sub; ++i) {
    if (C[i] > 0.5) ctrl_idx.push_back(i);
  }
  int n_ct = (int)ctrl_idx.size();
  if (n_ct == 0) return res;

  arma::mat X_c(n_ct, X_sub.n_cols);
  arma::vec v_c(n_ct);
  for (int a = 0; a < n_ct; ++a) {
    int i = ctrl_idx[a];
    X_c.row(a) = X_sub.row(i);
    v_c[a] = v[i];
  }

  arma::mat Xc_v = X_c.each_col() % v_c;
  arma::mat XtX_c = (X_c.t() * Xc_v) / (double)n_sub;
  CholFactor cf = seq_chol_drop(XtX_c, 1e-10);
  if (cf.kept.empty()) return res;

  res.valid = true;
  res.kept = cf.kept;
  res.ctrl_idx = std::move(ctrl_idx);
  res.v_c = std::move(v_c);
  res.X_sub_k = subset_cols(X_sub, cf.kept);
  res.X_c_k = subset_cols(X_c, cf.kept);
  res.R_Xc = cf.R;
  return res;
}

// Inverse probability tilting propensity score (Graham et al. 2012) and
// odds-weighted control WLS factorisation (Sant'Anna & Zhao 2020, Eq. 3.5).
IptCache fit_ipt_sub(const arma::mat& X_sub, const arma::vec& D,
                     const arma::vec& C, const arma::vec& v) {
  IptCache res;
  int n = (int)X_sub.n_rows;
  if (n == 0) return res;
  double sum_vD = arma::dot(v, D);
  double sum_vC = arma::dot(v, C);
  if (sum_vD <= 0.0 || sum_vC <= 0.0) return res;

  arma::mat XtX = X_sub.t() * X_sub;
  CholFactor cf = seq_chol_drop(XtX, 1e-10);
  int k = (int)cf.kept.size();
  if (k == 0) return res;

  arma::mat Xk = subset_cols(X_sub, cf.kept);
  double d_bar = std::min(1.0 - 1e-6, std::max(1e-6, sum_vD / arma::sum(v)));
  arma::vec gamma(k, arma::fill::zeros);
  gamma[0] = std::log(d_bar / (1.0 - d_bar));

  auto ipt_obj_fn = [&](const arma::vec& evec) -> double {
    double obj = 0.0;
    for (int i = 0; i < n; ++i) {
      double e = evec[i];
      if (D[i] > 0.5) {
        obj += v[i] * e;
      } else {
        if (e > 50.0) return -INFINITY;
        obj -= v[i] * std::exp(e);
      }
    }
    return obj;
  };

  arma::vec eta = Xk * gamma;
  double obj = ipt_obj_fn(eta);
  arma::vec exp_e(n), w_h(n), grad(k), step(k);
  arma::mat H(k, k);

  // Accepted iterates have a finite objective, so control indices stay <= 50.
  auto fill_exp = [&](const arma::vec& evec) {
    for (int i = 0; i < n; ++i) {
      exp_e[i] = (C[i] > 0.5) ? std::exp(evec[i]) : 0.0;
    }
  };

  for (int iter = 0; iter < 60; ++iter) {
    fill_exp(eta);
    for (int i = 0; i < n; ++i) {
      w_h[i] = (C[i] > 0.5) ? (v[i] * std::max(exp_e[i], 1e-12)) : 0.0;
    }
    grad = Xk.t() * (v % (D - C % exp_e));
    if (arma::max(arma::abs(grad)) / (double)n < 1e-13) break;

    H = Xk.t() * (Xk.each_col() % w_h);
    bool ok = arma::solve(step, H, grad, arma::solve_opts::fast) && step.is_finite();
    if (!ok) {
      H.diag() += 1e-10 * (double)n;
      ok = arma::solve(step, H, grad, arma::solve_opts::fast) && step.is_finite();
      if (!ok) break;
    }

    double alpha = 1.0;
    arma::vec gamma_new, eta_new;
    double obj_new = obj;
    bool accepted = false;
    for (int ls = 0; ls < 30; ++ls) {
      gamma_new = gamma + alpha * step;
      eta_new = Xk * gamma_new;
      obj_new = ipt_obj_fn(eta_new);
      if (std::isfinite(obj_new) && obj_new >= obj - 1e-13 * (1.0 + std::abs(obj))) {
        accepted = true;
        break;
      }
      alpha *= 0.5;
    }
    if (!accepted) break;

    double max_chg = arma::max(arma::abs(alpha * step));
    gamma = gamma_new;
    eta = eta_new;
    obj = obj_new;
    if (max_chg < 1e-13 && arma::max(arma::abs(grad)) / (double)n < 1e-13) break;
  }

  // Accept the fit only at a verified optimum, with DRDID's tolerances: the
  // scaled Newton decrement and the Newton step in the linear index must both
  // be at most 1e-6. The influence function of the improved DR estimator has
  // no first-stage term only when the IPT first-order conditions hold.
  bool converged = std::isfinite(obj);
  if (converged) {
    fill_exp(eta);
    arma::vec mass_c = v % C % exp_e;
    double mass = arma::dot(v, D) + arma::sum(mass_c);
    arma::vec g_fin = Xk.t() * (v % D - mass_c);
    arma::mat H_fin = Xk.t() * (Xk.each_col() % mass_c);
    arma::vec delta;
    converged = arma::solve(delta, H_fin, g_fin, arma::solve_opts::no_approx) &&
      delta.is_finite();
    if (converged) {
      double decrement = std::sqrt(std::max(arma::dot(g_fin, delta), 0.0) / mass);
      double index_step = arma::max(arma::abs(Xk * delta));
      converged = std::isfinite(decrement) && decrement <= 1e-6 &&
        std::isfinite(index_step) && index_step <= 1e-6;
    }
  }
  if (!converged) {
    res.not_converged = true;
    return res;
  }

  arma::vec p(n), w0(n);
  std::vector<int> ctrl_idx;
  ctrl_idx.reserve(n);
  double mean_vD = sum_vD / (double)n;
  for (int i = 0; i < n; ++i) {
    double e = std::min(30.0, std::max(-30.0, eta[i]));
    double pi = std::min(1.0 / (1.0 + std::exp(-e)), kPsCap);
    p[i] = pi;
    if (C[i] > 0.5) {
      ctrl_idx.push_back(i);
      if (pi >= kPsCap) res.overlap_warn = true;
    }
    double trim = (C[i] > 0.5 && pi >= kTrimLevel) ? 0.0 : 1.0;
    w0[i] = trim * v[i] * C[i] * pi / (1.0 - pi) / mean_vD;
  }

  int n_ct = (int)ctrl_idx.size();
  if (n_ct == 0) return res;

  // The outcome WLS uses every control unit, untrimmed (DRDID wols.br.panel).
  arma::mat X_c_all(n_ct, k);
  arma::vec w_wls_c(n_ct);
  for (int a = 0; a < n_ct; ++a) {
    int i = ctrl_idx[a];
    X_c_all.row(a) = Xk.row(i);
    w_wls_c[a] = v[i] * p[i] / (1.0 - p[i]);
  }

  arma::mat Xc_w = X_c_all.each_col() % w_wls_c;
  arma::mat M_c = (X_c_all.t() * Xc_w) / (double)n;
  CholFactor cf_wls = seq_chol_drop(M_c, 1e-10);
  if (cf_wls.kept.empty()) return res;

  res.valid = true;
  res.kept = cf.kept;
  res.ctrl_idx = std::move(ctrl_idx);
  res.p = p;
  res.w0 = w0;
  res.w_wls_c = std::move(w_wls_c);
  res.X_sub_k = subset_cols(Xk, cf_wls.kept);
  res.X_c_k = subset_cols(X_c_all, cf_wls.kept);
  res.R_wls = cf_wls.R;
  return res;
}

inline arma::vec gather_by_idx(const arma::vec& src, const std::vector<int>& idx) {
  int m = (int)idx.size();
  arma::vec out(m);
  for (int a = 0; a < m; ++a) out[a] = src[idx[a]];
  return out;
}

struct CellResult {
  double att = 0.0;
  arma::vec psi_sub;
};

// Shared 2-period DiD evaluation kernel across compute_cs_cov_cpp and
// compute_dr_did_panel_cpp.
CellResult eval_dr_cell(
    const arma::vec& dy,
    const arma::vec& D,
    const arma::vec& C,
    const arma::vec& v,
    const arma::vec& w1,
    const std::string& est_method,
    const PropensityCache& ps_cache,
    const OrCache& or_cache,
    const IptCache& ipt_cache
) {
  int n_sub = (int)dy.n_elem;
  CellResult out;

  if (est_method == "imp") {
    arma::vec dy_c = gather_by_idx(dy, ipt_cache.ctrl_idx);
    arma::vec rhs = (ipt_cache.X_c_k.t() * (ipt_cache.w_wls_c % dy_c)) / (double)n_sub;
    arma::vec beta_k = chol_solve_upper(ipt_cache.R_wls, rhs);
    arma::vec r = dy - ipt_cache.X_sub_k * beta_k;
    // Weights as in DRDID::drdid_imp_panel; first-stage estimation terms
    // vanish at the IPT optimum (Sant'Anna & Zhao 2020, Eq. 3.6).
    out.att = arma::mean(w1 % r) - arma::mean(ipt_cache.w0 % r);
    out.psi_sub = w1 % (r - out.att) - ipt_cache.w0 % r;
    return out;
  }

  arma::vec r = dy;
  if (est_method == "reg" || est_method == "trad") {
    arma::vec dy_c = gather_by_idx(dy, or_cache.ctrl_idx);
    arma::vec rhs = (or_cache.X_c_k.t() * (or_cache.v_c % dy_c)) / (double)n_sub;
    arma::vec beta_k = chol_solve_upper(or_cache.R_Xc, rhs);
    r = dy - or_cache.X_sub_k * beta_k;
  }

  if (est_method == "reg") {
    out.att = arma::mean(w1 % r);
    arma::vec M1 = (or_cache.X_sub_k.t() * w1) / (double)n_sub;
    arma::vec u1 = chol_solve_upper(or_cache.R_Xc, M1);
    out.psi_sub = w1 % (r - out.att) - v % C % r % (or_cache.X_sub_k * u1);
  } else if (est_method == "ipw") {
    double eta1 = arma::mean(w1 % dy);
    double eta0 = arma::mean(ps_cache.w0 % dy);
    out.att = eta1 - eta0;
    arma::vec M2 = (ps_cache.X_sub_k.t() * (ps_cache.w0 % (dy - eta0))) / (double)n_sub;
    arma::vec u2 = chol_solve_upper(ps_cache.R_H, M2);
    out.psi_sub = w1 % (dy - eta1) - ps_cache.w0 % (dy - eta0)
      - v % (D - ps_cache.p) % (ps_cache.X_sub_k * u2);
  } else {
    double eta1 = arma::mean(w1 % r);
    double eta0 = arma::mean(ps_cache.w0 % r);
    out.att = eta1 - eta0;
    arma::vec M1 = (or_cache.X_sub_k.t() * (w1 - ps_cache.w0)) / (double)n_sub;
    arma::vec u1 = chol_solve_upper(or_cache.R_Xc, M1);
    arma::vec M2 = (ps_cache.X_sub_k.t() * (ps_cache.w0 % (r - eta0))) / (double)n_sub;
    arma::vec u2 = chol_solve_upper(ps_cache.R_H, M2);
    out.psi_sub = w1 % (r - eta1) - ps_cache.w0 % (r - eta0)
      - v % C % r % (or_cache.X_sub_k * u1)
      - v % (D - ps_cache.p) % (ps_cache.X_sub_k * u2);
  }
  return out;
}

} // namespace

//' Callaway-Sant'Anna (2021) ATT(g,t) with covariates in C++
//'
//' Computes ATT(g,t), analytic standard errors, and the n_units x n_gt
//' unit-level influence function matrix for outcome regression ("reg"),
//' inverse probability weighting ("ipw"), traditional doubly robust
//' ("trad"), or improved doubly robust ("imp"; Sant'Anna & Zhao 2020).
//'
//' @noRd
// [[Rcpp::export]]
Rcpp::List compute_cs_cov_cpp(
    Rcpp::NumericMatrix Y_mat,
    Rcpp::NumericVector unit_timing,
    Rcpp::NumericVector cohorts,
    Rcpp::NumericVector all_times,
    Rcpp::List          X_base_list,
    std::string         control_group,
    int                 anticipation,
    std::string         est_method,
    bool                keep_ps = false
) {
  int n_units = Y_mat.nrow();
  int n_times = Y_mat.ncol();
  int n_coh   = cohorts.size();

  const double* pY = Y_mat.begin();
  const double* pU = unit_timing.begin();
  bool never_only  = (control_group == "nevertreated");
  bool use_ipt     = (est_method == "imp");
  bool use_or      = (est_method == "reg" || est_method == "trad");
  bool use_ipw     = (est_method == "ipw" || est_method == "trad");
  bool track_ps    = keep_ps && (use_ipt || use_ipw);
  if (!use_ipt && !use_or && !use_ipw) {
    Rcpp::stop("Unknown `est_method`: " + est_method);
  }

  auto find_time_col = [&](double val) -> int {
    for (int ti = 0; ti < n_times; ++ti) {
      if (all_times[ti] == val) return ti;
    }
    return -1;
  };

  std::vector<double> out_g, out_t, out_att, out_se;
  std::vector<arma::vec> psi_cols;
  std::vector<double> ps_g, ps_t, ps_val;
  std::vector<int> ps_unit, ps_treated;
  std::vector<double> nc_g, nc_t, lnc_g, lnc_t;
  bool any_overlap_warn = false;

  for (int gi = 0; gi < n_coh; ++gi) {
    double g      = cohorts[gi];
    double base_t = g - 1.0 - (double)anticipation;
    int base_ti   = find_time_col(base_t);
    if (base_ti < 0) continue;
    if (Rf_isNull(X_base_list[gi])) continue;

    Rcpp::NumericMatrix Xr(X_base_list[gi]);
    arma::mat X_all(Xr.begin(), Xr.nrow(), Xr.ncol(), false);
    int K = (int)X_all.n_cols;

    std::vector<int> tr_units;
    tr_units.reserve(n_units);
    for (int i = 0; i < n_units; ++i) {
      if (!ISNAN(pU[i]) && pU[i] == g) tr_units.push_back(i);
    }
    if (tr_units.empty()) continue;

    const double* y_base = pY + (size_t)base_ti * n_units;

    // Cache propensity and OR Cholesky across periods sharing the control set
    std::vector<int> prev_ctrl;
    bool has_cache = false;
    bool recorded_never_ps = false;
    std::vector<int> sub_units;
    arma::mat X_sub;
    arma::vec D_sub, C_sub, v_sub, w1;
    PropensityCache ps_cache;
    OrCache or_cache;
    IptCache ipt_cache;

    for (int ti = 0; ti < n_times; ++ti) {
      double t = all_times[ti];
      if (t == base_t) continue;

      std::vector<int> ctrl_units;
      ctrl_units.reserve(n_units);
      double cutoff = std::max(t + (double)anticipation, g - 1.0);
      for (int i = 0; i < n_units; ++i) {
        double ut = pU[i];
        bool ut_na = ISNAN(ut);
        bool is_ctrl = never_only
          ? ut_na
          : ((ut_na || ut > cutoff) && (ut_na || ut != g));
        if (is_ctrl) ctrl_units.push_back(i);
      }
      if (ctrl_units.empty()) continue;

      if (!has_cache || ctrl_units != prev_ctrl) {
        int n_tr = (int)tr_units.size();
        int n_ct = (int)ctrl_units.size();
        int n_sub = n_tr + n_ct;
        sub_units.resize(n_sub);
        D_sub.set_size(n_sub);
        C_sub.set_size(n_sub);
        v_sub.ones(n_sub);
        w1.zeros(n_sub);
        double w1_val = (double)n_sub / (double)n_tr;
        X_sub.set_size(n_sub, K);

        for (int a = 0; a < n_tr; ++a) {
          int i = tr_units[a];
          sub_units[a] = i;
          D_sub[a] = 1.0;
          C_sub[a] = 0.0;
          w1[a] = w1_val;
          X_sub.row(a) = X_all.row(i);
        }
        for (int a = 0; a < n_ct; ++a) {
          int i = ctrl_units[a];
          sub_units[n_tr + a] = i;
          D_sub[n_tr + a] = 0.0;
          C_sub[n_tr + a] = 1.0;
          X_sub.row(n_tr + a) = X_all.row(i);
        }

        if (use_ipt) {
          ipt_cache = fit_ipt_sub(X_sub, D_sub, C_sub, v_sub);
          if (ipt_cache.overlap_warn) any_overlap_warn = true;
        }
        if (use_ipw) {
          ps_cache = fit_logit_sub(X_sub, D_sub, C_sub, v_sub);
          if (ps_cache.overlap_warn) any_overlap_warn = true;
        }
        if (use_or) {
          or_cache = fit_or_cache(X_sub, C_sub, v_sub);
        }
        prev_ctrl = std::move(ctrl_units);
        has_cache = true;
      }

      if (use_ipt && !ipt_cache.valid) {
        if (ipt_cache.not_converged) {
          nc_g.push_back(g);
          nc_t.push_back(t);
        }
        continue;
      }
      if ((use_ipw && !ps_cache.valid) || (use_or && !or_cache.valid)) {
        continue;
      }

      int n_sub = (int)sub_units.size();

      const double* y_t = pY + (size_t)ti * n_units;
      arma::vec dy(n_sub);
      bool any_bad_y = false;
      for (int a = 0; a < n_sub; ++a) {
        int i = sub_units[a];
        double val = y_t[i] - y_base[i];
        if (!std::isfinite(val)) {
          any_bad_y = true;
          break;
        }
        dy[a] = val;
      }
      if (any_bad_y) continue;

      if (track_ps && (!never_only || !recorded_never_ps)) {
        const arma::vec& p_src = use_ipt ? ipt_cache.p : ps_cache.p;
        double t_rec = never_only ? NA_REAL : t;
        ps_g.reserve(ps_g.size() + n_sub);
        ps_t.reserve(ps_t.size() + n_sub);
        ps_unit.reserve(ps_unit.size() + n_sub);
        ps_treated.reserve(ps_treated.size() + n_sub);
        ps_val.reserve(ps_val.size() + n_sub);
        for (int a = 0; a < n_sub; ++a) {
          ps_g.push_back(g);
          ps_t.push_back(t_rec);
          ps_unit.push_back(sub_units[a] + 1);
          ps_treated.push_back((int)D_sub[a]);
          ps_val.push_back(p_src[a]);
        }
        if (never_only) recorded_never_ps = true;
      }

      CellResult cell = eval_dr_cell(
        dy, D_sub, C_sub, v_sub, w1, est_method, ps_cache, or_cache, ipt_cache
      );
      if (use_ipw && ps_cache.not_converged) {
        lnc_g.push_back(g);
        lnc_t.push_back(t);
      }

      arma::vec psi_col(n_units, arma::fill::zeros);
      double scale = (double)n_units / (double)n_sub;
      for (int a = 0; a < n_sub; ++a) {
        psi_col[sub_units[a]] = scale * cell.psi_sub[a];
      }
      double se = std::sqrt(arma::dot(psi_col, psi_col)) / (double)n_units;

      out_g.push_back(g);
      out_t.push_back(t);
      out_att.push_back(cell.att);
      out_se.push_back(se);
      psi_cols.push_back(std::move(psi_col));
    }
  }

  int n_gt = (int)out_g.size();
  Rcpp::NumericMatrix psi_mat(n_units, n_gt);
  for (int j = 0; j < n_gt; ++j) {
    std::copy(psi_cols[j].begin(), psi_cols[j].end(),
              psi_mat.begin() + (size_t)j * n_units);
  }

  return Rcpp::List::create(
    Rcpp::Named("g")            = Rcpp::wrap(out_g),
    Rcpp::Named("t")            = Rcpp::wrap(out_t),
    Rcpp::Named("att")          = Rcpp::wrap(out_att),
    Rcpp::Named("se")           = Rcpp::wrap(out_se),
    Rcpp::Named("psi")          = psi_mat,
    Rcpp::Named("overlap_warn") = any_overlap_warn,
    Rcpp::Named("ps_g")         = Rcpp::wrap(ps_g),
    Rcpp::Named("ps_t")         = Rcpp::wrap(ps_t),
    Rcpp::Named("ps_unit")      = Rcpp::wrap(ps_unit),
    Rcpp::Named("ps_treated")   = Rcpp::wrap(ps_treated),
    Rcpp::Named("ps_val")       = Rcpp::wrap(ps_val),
    Rcpp::Named("ipt_nc_g")     = Rcpp::wrap(nc_g),
    Rcpp::Named("ipt_nc_t")     = Rcpp::wrap(nc_t),
    Rcpp::Named("logit_nc_g")   = Rcpp::wrap(lnc_g),
    Rcpp::Named("logit_nc_t")   = Rcpp::wrap(lnc_t)
  );
}

//' Sant'Anna & Zhao (2020) two-period panel DR-DID estimator in C++
//'
//' Computes the ATT, analytic standard error, influence function, and
//' per-unit propensity score on a balanced two-period panel for improved DR
//' ("imp"), traditional DR ("trad"), outcome regression ("reg"), or
//' standardized IPW ("ipw").
//'
//' @noRd
// [[Rcpp::export]]
Rcpp::List compute_dr_did_panel_cpp(
    Rcpp::NumericVector y0,
    Rcpp::NumericVector y1,
    Rcpp::NumericVector D,
    Rcpp::NumericMatrix X,
    std::string         est_method,
    Rcpp::Nullable<Rcpp::NumericVector> weights = R_NilValue
) {
  int n = y0.size();
  if (n == 0 || y1.size() != n || D.size() != n ||
      X.nrow() != n || X.ncol() < 1) {
    Rcpp::stop("Incompatible input dimensions in compute_dr_did_panel_cpp().");
  }

  arma::vec dy(n), D_vec(n), C_vec(n);
  for (int i = 0; i < n; ++i) {
    double d_i = D[i];
    if (!std::isfinite(y0[i]) || !std::isfinite(y1[i]) ||
        (d_i != 0.0 && d_i != 1.0)) {
      Rcpp::stop("Inputs to compute_dr_did_panel_cpp() must be finite (and D in {0, 1}).");
    }
    dy[i] = y1[i] - y0[i];
    D_vec[i] = d_i;
    C_vec[i] = 1.0 - d_i;
  }

  arma::vec v(n, arma::fill::ones);
  if (weights.isNotNull()) {
    Rcpp::NumericVector w_r(weights.get());
    if (w_r.size() != n) {
      Rcpp::stop("`weights` length must match number of units.");
    }
    for (int i = 0; i < n; ++i) {
      if (!std::isfinite(w_r[i]) || w_r[i] <= 0.0) {
        Rcpp::stop("`weights` must be positive and finite.");
      }
      v[i] = w_r[i];
    }
    double v_mean = arma::mean(v);
    if (!std::isfinite(v_mean) || v_mean <= 0.0) {
      Rcpp::stop("Invalid observation weights.");
    }
    v /= v_mean;
  }

  double mean_vD = arma::mean(v % D_vec);
  double mean_vC = arma::mean(v % C_vec);
  if (mean_vD <= 0.0 || mean_vC <= 0.0) {
    Rcpp::stop("Both treated and control units with positive weight are required.");
  }
  arma::vec w1 = (v % D_vec) / mean_vD;

  arma::mat X_mat(X.begin(), X.nrow(), X.ncol(), false);
  bool use_ipt = (est_method == "imp");
  bool use_or  = (est_method == "reg" || est_method == "trad");
  bool use_ipw = (est_method == "ipw" || est_method == "trad");
  if (!use_ipt && !use_or && !use_ipw) {
    Rcpp::stop("Unknown `est_method`: " + est_method);
  }

  PropensityCache ps_cache;
  OrCache or_cache;
  IptCache ipt_cache;
  bool overlap_warn = false;

  if (use_ipt) {
    ipt_cache = fit_ipt_sub(X_mat, D_vec, C_vec, v);
    if (ipt_cache.not_converged) {
      return Rcpp::List::create(Rcpp::Named("converged") = false);
    }
    if (!ipt_cache.valid) {
      Rcpp::stop("Failed to fit IPT propensity score or outcome WLS model.");
    }
    if (ipt_cache.overlap_warn) overlap_warn = true;
  }
  if (use_ipw) {
    ps_cache = fit_logit_sub(X_mat, D_vec, C_vec, v);
    if (!ps_cache.valid) {
      Rcpp::stop("Failed to fit propensity score logistic model.");
    }
    if (ps_cache.overlap_warn) overlap_warn = true;
  }
  if (use_or) {
    or_cache = fit_or_cache(X_mat, C_vec, v);
    if (!or_cache.valid) {
      Rcpp::stop("Failed to fit control outcome regression model.");
    }
  }

  CellResult cell = eval_dr_cell(
    dy, D_vec, C_vec, v, w1, est_method, ps_cache, or_cache, ipt_cache
  );
  double se = std::sqrt(arma::dot(cell.psi_sub, cell.psi_sub)) / (double)n;

  Rcpp::NumericVector psi_out(cell.psi_sub.begin(), cell.psi_sub.end());

  Rcpp::NumericVector ps_out(n, NA_REAL);
  if (use_ipt) {
    std::copy(ipt_cache.p.begin(), ipt_cache.p.end(), ps_out.begin());
  } else if (use_ipw) {
    std::copy(ps_cache.p.begin(), ps_cache.p.end(), ps_out.begin());
  }

  return Rcpp::List::create(
    Rcpp::Named("converged")    = true,
    Rcpp::Named("ps_converged") = !(use_ipw && ps_cache.not_converged),
    Rcpp::Named("att")          = cell.att,
    Rcpp::Named("se")           = se,
    Rcpp::Named("psi")          = psi_out,
    Rcpp::Named("ps")           = ps_out,
    Rcpp::Named("overlap_warn") = overlap_warn
  );
}
