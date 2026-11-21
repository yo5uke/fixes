// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <algorithm>
#include <cmath>
#include <vector>

using namespace Rcpp;

namespace {

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
  std::vector<int> kept;
  arma::vec p;        // length n_sub
  arma::vec w0;       // normalized Hajek control weights (length n_sub)
  arma::mat X_sub_k;  // n_sub x k_ps
  arma::mat R_H;      // Cholesky factor of H = (1/n_sub) X_k' diag(p(1-p)) X_k
  bool overlap_warn = false;
};

struct OrCache {
  bool valid = false;
  std::vector<int> kept;
  arma::mat X_sub_k;  // n_sub x k_or
  arma::mat X_c_k;    // n_ct x k_or
  arma::mat R_Xc;     // Cholesky factor of (1/n_sub) X_C_k' X_C_k
};

// Damped Newton-Raphson logistic regression of D on X_sub (n_sub x K).
PropensityCache fit_logit_sub(const arma::mat& X_sub, const arma::vec& D,
                              const arma::vec& C) {
  PropensityCache res;
  int n = (int)X_sub.n_rows;
  arma::mat XtX = X_sub.t() * X_sub;
  CholFactor cf = seq_chol_drop(XtX, 1e-10);
  int k = (int)cf.kept.size();
  if (k == 0 || n == 0) return res;

  arma::mat Xk = subset_cols(X_sub, cf.kept);
  double d_bar = std::min(1.0 - 1e-6, std::max(1e-6, arma::mean(D)));
  arma::vec beta(k, arma::fill::zeros);
  beta[0] = std::log(d_bar / (1.0 - d_bar));

  auto loglik_fn = [&](const arma::vec& eta) -> double {
    double ll = 0.0;
    for (int i = 0; i < n; ++i) {
      double e = eta[i];
      double l1p = (e > 0.0) ? (e + std::log1p(std::exp(-e))) : std::log1p(std::exp(e));
      ll += D[i] * e - l1p;
    }
    return ll;
  };

  arma::vec eta = Xk * beta;
  double ll = loglik_fn(eta);
  arma::vec p(n), w(n), grad(k), step(k);
  arma::mat H(k, k);

  for (int iter = 0; iter < 50; ++iter) {
    for (int i = 0; i < n; ++i) {
      double e = std::min(30.0, std::max(-30.0, eta[i]));
      double pi = 1.0 / (1.0 + std::exp(-e));
      p[i] = pi;
      w[i] = std::max(pi * (1.0 - pi), 1e-12);
    }
    grad = Xk.t() * (D - p);
    if (arma::max(arma::abs(grad)) / n < 1e-13) break;

    arma::mat Xw = Xk.each_col() % w;
    H = Xk.t() * Xw;
    bool ok = arma::solve(step, H, grad, arma::solve_opts::fast);
    if (!ok) {
      H.diag() += 1e-10 * (double)n;
      ok = arma::solve(step, H, grad, arma::solve_opts::fast);
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
      if (std::isfinite(ll_new) && ll_new >= ll - 1e-14) {
        accepted = true;
        break;
      }
      alpha *= 0.5;
    }
    if (!accepted) break;

    double max_chg = arma::max(arma::abs(alpha * step));
    beta = beta_new;
    eta = eta_new;
    ll = ll_new;
    if (max_chg < 1e-13 && arma::max(arma::abs(grad)) / n < 1e-13) break;
  }

  arma::vec w0_raw(n);
  for (int i = 0; i < n; ++i) {
    double e = std::min(30.0, std::max(-30.0, eta[i]));
    double pi = 1.0 / (1.0 + std::exp(-e));
    p[i] = pi;
    if (D[i] < 0.5 && pi >= 1.0 - 1e-6) res.overlap_warn = true;
    w[i] = pi * (1.0 - pi);
    double denom = std::max(1.0 - pi, 1e-15);
    w0_raw[i] = C[i] * pi / denom;
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

OrCache fit_or_cache(const arma::mat& X_sub, const arma::mat& X_c, int n_sub) {
  OrCache res;
  if (n_sub <= 0 || X_c.n_rows == 0) return res;
  arma::mat XtX_c = (X_c.t() * X_c) / (double)n_sub;
  CholFactor cf = seq_chol_drop(XtX_c, 1e-10);
  if (cf.kept.empty()) return res;
  res.valid = true;
  res.kept = cf.kept;
  res.X_sub_k = subset_cols(X_sub, cf.kept);
  res.X_c_k = subset_cols(X_c, cf.kept);
  res.R_Xc = cf.R;
  return res;
}

} // namespace

//' Callaway-Sant'Anna (2021) ATT(g,t) with covariates in C++
//'
//' Computes ATT(g,t), analytic standard errors, and the n_units x n_gt
//' unit-level influence function matrix for outcome regression ("reg"),
//' inverse probability weighting ("ipw"), or doubly robust ("dr").
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
    std::string         est_method
) {
  int n_units = Y_mat.nrow();
  int n_times = Y_mat.ncol();
  int n_coh   = cohorts.size();

  const double* pY = Y_mat.begin();
  const double* pU = unit_timing.begin();
  bool never_only  = (control_group == "nevertreated");
  bool use_or      = (est_method == "reg" || est_method == "dr");
  bool use_ipw     = (est_method == "ipw" || est_method == "dr");

  auto find_time_col = [&](double val) -> int {
    for (int ti = 0; ti < n_times; ++ti) {
      if (all_times[ti] == val) return ti;
    }
    return -1;
  };

  std::vector<double> out_g, out_t, out_att, out_se;
  std::vector<arma::vec> psi_cols;
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
    std::vector<int> sub_units;
    arma::mat X_sub, X_c;
    arma::vec D_sub, C_sub, w1;
    PropensityCache ps_cache;
    OrCache or_cache;

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
        w1.zeros(n_sub);
        double w1_val = (double)n_sub / (double)n_tr;
        X_sub.set_size(n_sub, K);
        X_c.set_size(n_ct, K);

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
          X_c.row(a) = X_all.row(i);
        }

        if (use_ipw) {
          ps_cache = fit_logit_sub(X_sub, D_sub, C_sub);
          if (ps_cache.overlap_warn) any_overlap_warn = true;
        }
        if (use_or) {
          or_cache = fit_or_cache(X_sub, X_c, n_sub);
        }
        prev_ctrl = std::move(ctrl_units);
        has_cache = true;
      }

      if ((use_ipw && !ps_cache.valid) || (use_or && !or_cache.valid)) continue;

      int n_tr = (int)tr_units.size();
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

      arma::vec r = dy;
      if (use_or) {
        arma::vec dy_c = dy.subvec(n_tr, n_sub - 1);
        arma::vec rhs = (or_cache.X_c_k.t() * dy_c) / (double)n_sub;
        arma::vec beta_k = chol_solve_upper(or_cache.R_Xc, rhs);
        r = dy - or_cache.X_sub_k * beta_k;
      }

      double att = 0.0;
      arma::vec psi_sub(n_sub, arma::fill::zeros);

      if (est_method == "reg") {
        att = arma::mean(w1 % r);
        arma::vec M1 = (or_cache.X_sub_k.t() * w1) / (double)n_sub;
        arma::vec u1 = chol_solve_upper(or_cache.R_Xc, M1);
        psi_sub = w1 % (r - att) - C_sub % r % (or_cache.X_sub_k * u1);
      } else if (est_method == "ipw") {
        double eta1 = arma::mean(w1 % dy);
        double eta0 = arma::mean(ps_cache.w0 % dy);
        att = eta1 - eta0;
        arma::vec M2 = (ps_cache.X_sub_k.t() * (ps_cache.w0 % (dy - eta0))) / (double)n_sub;
        arma::vec u2 = chol_solve_upper(ps_cache.R_H, M2);
        psi_sub = w1 % (dy - eta1) - ps_cache.w0 % (dy - eta0)
          - (D_sub - ps_cache.p) % (ps_cache.X_sub_k * u2);
      } else {
        double eta1 = arma::mean(w1 % r);
        double eta0 = arma::mean(ps_cache.w0 % r);
        att = eta1 - eta0;
        arma::vec M1 = (or_cache.X_sub_k.t() * (w1 - ps_cache.w0)) / (double)n_sub;
        arma::vec u1 = chol_solve_upper(or_cache.R_Xc, M1);
        arma::vec M2 = (ps_cache.X_sub_k.t() * (ps_cache.w0 % (r - eta0))) / (double)n_sub;
        arma::vec u2 = chol_solve_upper(ps_cache.R_H, M2);
        psi_sub = w1 % (r - eta1) - ps_cache.w0 % (r - eta0)
          - C_sub % r % (or_cache.X_sub_k * u1)
          - (D_sub - ps_cache.p) % (ps_cache.X_sub_k * u2);
      }

      arma::vec psi_col(n_units, arma::fill::zeros);
      double scale = (double)n_units / (double)n_sub;
      for (int a = 0; a < n_sub; ++a) {
        psi_col[sub_units[a]] = scale * psi_sub[a];
      }
      double se = std::sqrt(arma::dot(psi_col, psi_col)) / (double)n_units;

      out_g.push_back(g);
      out_t.push_back(t);
      out_att.push_back(att);
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
    Rcpp::Named("g")   = Rcpp::wrap(out_g),
    Rcpp::Named("t")   = Rcpp::wrap(out_t),
    Rcpp::Named("att") = Rcpp::wrap(out_att),
    Rcpp::Named("se")  = Rcpp::wrap(out_se),
    Rcpp::Named("psi") = psi_mat,
    Rcpp::Named("overlap_warn") = any_overlap_warn
  );
}
