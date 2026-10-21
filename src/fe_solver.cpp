#include <Rcpp.h>
using namespace Rcpp;

//' Alternating-projections solver for a two-way fixed-effects model
//'
//' Solves \eqn{Y = \alpha_{unit} + \beta_{time} + \varepsilon} by
//' Gauss-Seidel alternating means with Irons-Tuck acceleration on the
//' time-FE vector.
//'
//' The estimation sample must already exclude NA outcomes and singleton
//' FE levels, and \code{unit_id}/\code{time_id} must be dense 0-based
//' indices (see \code{.solve_fe_2way()}, which prepares both).
//'
//' Only the sums \eqn{\alpha_i + \beta_t} are identified; no normalization
//' is applied, so callers must always use \code{alpha} and \code{beta}
//' jointly.
//'
//' @param y Numeric outcome vector (length N, no NAs).
//' @param unit_id Integer vector (length N): 0-based unit index per row.
//' @param time_id Integer vector (length N): 0-based time index per row.
//' @param n_unit Number of unit levels.
//' @param n_time Number of time levels.
//' @param tol Relative convergence tolerance on the time-FE vector.
//' @param max_iter Maximum number of Gauss-Seidel sweeps.
//'
//' @return A list with \code{alpha} (length \code{n_unit}), \code{beta}
//'   (length \code{n_time}), \code{iterations}, \code{converged}, and
//'   \code{last_delta} (the last sweep-to-sweep change).
//'
//' @noRd
// [[Rcpp::export]]
List solve_fe_2way_cpp(NumericVector y,
                       IntegerVector unit_id,
                       IntegerVector time_id,
                       int n_unit,
                       int n_time,
                       double tol,
                       int max_iter) {
    int N = y.size();

    std::vector<double> alpha(n_unit, 0.0);
    std::vector<double> cnt_u(n_unit, 0.0), cnt_t(n_time, 0.0);
    for (int k = 0; k < N; k++) {
        cnt_u[unit_id[k]] += 1.0;
        cnt_t[time_id[k]] += 1.0;
    }

    std::vector<double> b0(n_time, 0.0), b1(n_time, 0.0), b2(n_time, 0.0);

    // One sweep: alpha_i = mean_i(y - beta_t), then beta_t = mean_t(y - alpha_i)
    auto sweep = [&](const std::vector<double>& b_in,
                     std::vector<double>& b_out) {
        std::fill(alpha.begin(), alpha.end(), 0.0);
        for (int k = 0; k < N; k++)
            alpha[unit_id[k]] += y[k] - b_in[time_id[k]];
        for (int i = 0; i < n_unit; i++) alpha[i] /= cnt_u[i];

        std::fill(b_out.begin(), b_out.end(), 0.0);
        for (int k = 0; k < N; k++)
            b_out[time_id[k]] += y[k] - alpha[unit_id[k]];
        for (int t = 0; t < n_time; t++) b_out[t] /= cnt_t[t];
    };

    auto max_delta = [&](const std::vector<double>& bnew,
                         const std::vector<double>& bold) {
        double d = 0.0;
        for (int t = 0; t < n_time; t++) {
            double dt = std::abs(bnew[t] - bold[t]) / (1.0 + std::abs(bnew[t]));
            if (dt > d) d = dt;
        }
        return d;
    };

    bool converged = false;
    int iter = 0;
    double delta = R_PosInf;

    while (iter < max_iter) {
        sweep(b0, b1);
        iter++;
        delta = max_delta(b1, b0);
        if (delta < tol) { b0 = b1; converged = true; break; }
        if (iter >= max_iter) { b0 = b1; break; }

        sweep(b1, b2);
        iter++;
        delta = max_delta(b2, b1);
        if (delta < tol) { b0 = b2; converged = true; break; }

        // Irons-Tuck extrapolation: b <- b2 - <d2, dd>/<dd, dd> * dd,
        // where d2 = b2 - b1 and dd = (b2 - b1) - (b1 - b0).
        double num = 0.0, den = 0.0;
        for (int t = 0; t < n_time; t++) {
            double d2t = b2[t] - b1[t];
            double ddt = d2t - (b1[t] - b0[t]);
            num += d2t * ddt;
            den += ddt * ddt;
        }
        double coef = (den > 0.0) ? num / den : 0.0;
        if (den > 0.0 && R_finite(coef)) {
            for (int t = 0; t < n_time; t++) {
                double ddt = (b2[t] - b1[t]) - (b1[t] - b0[t]);
                b0[t] = b2[t] - coef * ddt;
            }
        } else {
            b0 = b2;
        }

        if (iter % 1000 == 0) Rcpp::checkUserInterrupt();
    }

    // Recompute alpha from the returned beta so the pair is consistent
    std::fill(alpha.begin(), alpha.end(), 0.0);
    for (int k = 0; k < N; k++)
        alpha[unit_id[k]] += y[k] - b0[time_id[k]];
    for (int i = 0; i < n_unit; i++) alpha[i] /= cnt_u[i];

    return List::create(
        _["alpha"]      = NumericVector(alpha.begin(), alpha.end()),
        _["beta"]       = NumericVector(b0.begin(), b0.end()),
        _["iterations"] = iter,
        _["converged"]  = converged,
        _["last_delta"] = delta
    );
}

//' Normal-equations solver for a two-way fixed-effects design
//'
//' Solves \eqn{(A'A) z = c} for the unit/time dummy matrix \eqn{A} of the
//' given rows, by the same Gauss-Seidel sweeps with Irons-Tuck acceleration
//' as \code{solve_fe_2way_cpp()}. \code{c_unit}/\code{c_time} are the unit and
//' time blocks of \eqn{c}. \eqn{A z} is unique; \code{z} itself is not.
//'
//' @param unit_id,time_id 0-based unit and time index per row.
//' @param c_unit,c_time Right-hand side blocks (length n_unit, n_time).
//' @param n_unit,n_time Number of unit and time levels.
//' @param tol Relative convergence tolerance on the time block.
//' @param max_iter Maximum number of sweeps.
//'
//' @return A list with \code{z_unit}, \code{z_time}, \code{converged}.
//'
//' @noRd
// [[Rcpp::export]]
List solve_fe_normal_cpp(IntegerVector unit_id,
                         IntegerVector time_id,
                         NumericVector c_unit,
                         NumericVector c_time,
                         int n_unit,
                         int n_time,
                         double tol,
                         int max_iter) {
    int N = unit_id.size();

    double scale = 0.0;
    for (int i = 0; i < n_unit; i++) scale = std::max(scale, std::abs(c_unit[i]));
    for (int t = 0; t < n_time; t++) scale = std::max(scale, std::abs(c_time[t]));
    if (!(scale > 0.0)) scale = 1.0;

    std::vector<double> cu(n_unit), ct(n_time);
    for (int i = 0; i < n_unit; i++) cu[i] = c_unit[i] / scale;
    for (int t = 0; t < n_time; t++) ct[t] = c_time[t] / scale;

    std::vector<double> zu(n_unit, 0.0);
    std::vector<double> cnt_u(n_unit, 0.0), cnt_t(n_time, 0.0);
    for (int k = 0; k < N; k++) {
        cnt_u[unit_id[k]] += 1.0;
        cnt_t[time_id[k]] += 1.0;
    }

    std::vector<double> b0(n_time, 0.0), b1(n_time, 0.0), b2(n_time, 0.0);

    auto sweep = [&](const std::vector<double>& b_in,
                     std::vector<double>& b_out) {
        for (int i = 0; i < n_unit; i++) zu[i] = cu[i];
        for (int k = 0; k < N; k++) zu[unit_id[k]] -= b_in[time_id[k]];
        for (int i = 0; i < n_unit; i++)
            zu[i] = (cnt_u[i] > 0.0) ? zu[i] / cnt_u[i] : 0.0;

        for (int t = 0; t < n_time; t++) b_out[t] = ct[t];
        for (int k = 0; k < N; k++) b_out[time_id[k]] -= zu[unit_id[k]];
        for (int t = 0; t < n_time; t++)
            b_out[t] = (cnt_t[t] > 0.0) ? b_out[t] / cnt_t[t] : 0.0;
    };

    auto max_delta = [&](const std::vector<double>& bnew,
                         const std::vector<double>& bold) {
        double d = 0.0;
        for (int t = 0; t < n_time; t++) {
            double dt = std::abs(bnew[t] - bold[t]) / (1.0 + std::abs(bnew[t]));
            if (dt > d) d = dt;
        }
        return d;
    };

    bool converged = false;
    int iter = 0;

    while (iter < max_iter) {
        sweep(b0, b1);
        iter++;
        if (max_delta(b1, b0) < tol) { b0 = b1; converged = true; break; }
        if (iter >= max_iter) { b0 = b1; break; }

        sweep(b1, b2);
        iter++;
        if (max_delta(b2, b1) < tol) { b0 = b2; converged = true; break; }

        double num = 0.0, den = 0.0;
        for (int t = 0; t < n_time; t++) {
            double d2t = b2[t] - b1[t];
            double ddt = d2t - (b1[t] - b0[t]);
            num += d2t * ddt;
            den += ddt * ddt;
        }
        double coef = (den > 0.0) ? num / den : 0.0;
        if (den > 0.0 && R_finite(coef)) {
            for (int t = 0; t < n_time; t++) {
                double ddt = (b2[t] - b1[t]) - (b1[t] - b0[t]);
                b0[t] = b2[t] - coef * ddt;
            }
        } else {
            b0 = b2;
        }

        if (iter % 1000 == 0) Rcpp::checkUserInterrupt();
    }

    // Unit block consistent with the returned time block
    for (int i = 0; i < n_unit; i++) zu[i] = cu[i];
    for (int k = 0; k < N; k++) zu[unit_id[k]] -= b0[time_id[k]];
    for (int i = 0; i < n_unit; i++)
        zu[i] = (cnt_u[i] > 0.0) ? zu[i] / cnt_u[i] : 0.0;

    NumericVector zu_out(n_unit), zt_out(n_time);
    for (int i = 0; i < n_unit; i++) zu_out[i] = zu[i] * scale;
    for (int t = 0; t < n_time; t++) zt_out[t] = b0[t] * scale;

    return List::create(
        _["z_unit"]    = zu_out,
        _["z_time"]    = zt_out,
        _["converged"] = converged
    );
}

//' Schur complement of a two-way dummy design after eliminating one side
//'
//' For the dummy matrix \eqn{A = [A_e, A_k]} of the given rows, returns
//' \eqn{A_k'A_k - A_k'A_e (A_e'A_e)^{-1} A_e'A_k}, an
//' \code{n_keep} x \code{n_keep} matrix.
//'
//' @param elim_id,keep_id 0-based indices of the eliminated and kept levels.
//' @param n_elim,n_keep Number of levels on each side.
//'
//' @noRd
// [[Rcpp::export]]
NumericMatrix fe_schur_cpp(IntegerVector elim_id,
                           IntegerVector keep_id,
                           int n_elim,
                           int n_keep) {
    int N = elim_id.size();

    std::vector<int> start(n_elim + 1, 0);
    for (int k = 0; k < N; k++) start[elim_id[k] + 1]++;
    for (int e = 0; e < n_elim; e++) start[e + 1] += start[e];

    std::vector<int> pos(start.begin(), start.end() - 1);
    std::vector<int> sorted_keep(N);
    for (int k = 0; k < N; k++) sorted_keep[pos[elim_id[k]]++] = keep_id[k];

    NumericMatrix M(n_keep, n_keep);
    for (int k = 0; k < N; k++) M(keep_id[k], keep_id[k]) += 1.0;

    for (int e = 0; e < n_elim; e++) {
        int n_e = start[e + 1] - start[e];
        if (n_e == 0) continue;
        double inv = 1.0 / n_e;
        for (int a = start[e]; a < start[e + 1]; a++)
            for (int b = start[e]; b < start[e + 1]; b++)
                M(sorted_keep[a], sorted_keep[b]) -= inv;
    }
    return M;
}
