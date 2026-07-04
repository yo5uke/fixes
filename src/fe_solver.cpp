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
