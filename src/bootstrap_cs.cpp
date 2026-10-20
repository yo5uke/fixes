#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
#include <algorithm>
#include <cmath>
using namespace Rcpp;

//' Core of the Callaway-Sant'Anna (2021) multiplier bootstrap (Algorithm 1)
//'
//' Performs two-stage multiplier bootstrap in C++ using R's RNG stream:
//' \enumerate{
//'   \item Pilot stage (B_pilot draws): estimates per-parameter dispersion
//'     \eqn{\hat\sigma_{1/2}} via normalised IQR of
//'     \eqn{\sqrt{n}\,\bar{V}\psi_{g,t}}.
//'   \item Main stage (B draws): computes the maximum studentised t-statistic
//'     per draw and returns the (1-alpha) quantile as the simultaneous
//'     critical value.
//' }
//'
//' Uses \code{R::runif(0,1)} for Mammen weight generation, so the output is
//' identical to the pure-R implementation when the same \code{set.seed()} is
//' applied before calling either version.
//'
//' @param psi     Numeric matrix (n_units × n_gt) of unit-level influence
//'   functions (output of \code{.compute_influence_cs()}).
//' @param B       Number of main bootstrap draws.
//' @param alpha   Significance level for the simultaneous band (e.g. 0.05).
//' @param B_pilot Number of pilot draws for \eqn{\hat\sigma_{1/2}} estimation
//'   (default 199).
//'
//' @return Named list with:
//' \describe{
//'   \item{\code{c_hat}}{Scalar simultaneous critical value.}
//'   \item{\code{margin}}{\code{n_gt}-vector: per-parameter CI half-widths
//'     = \code{c_hat * sigma_half / sqrt(n)}.}
//'   \item{\code{sigma_half}}{\code{n_gt}-vector: estimated
//'     \eqn{\hat\sigma_{1/2}} values (IQR-based).}
//' }
//'
//' @noRd
// [[Rcpp::export]]
List bootstrap_cs_cpp(
    NumericMatrix psi,
    int    B,
    double alpha,
    int    B_pilot
) {
    int n = psi.nrow();
    int m = psi.ncol();
    double sqrt_n     = std::sqrt((double)n);
    double kappa      = (std::sqrt(5.0) + 1.0) / 2.0;
    double p1         = kappa / std::sqrt(5.0);
    double w_lo       = 1.0 - kappa;   // value when runif < p1
    double w_hi       = kappa;         // value when runif >= p1
    double iqr_normal = R::qnorm(0.75, 0.0, 1.0, 1, 0) -
                        R::qnorm(0.25, 0.0, 1.0, 1, 0);  // ≈ 1.3490

    // View psi as arma matrix (no copy)
    arma::mat PSI(psi.begin(), n, m, false);

    // ---- Pilot stage -------------------------------------------------------
    // Fill order: column-major (i fast → b slow, matching
    // R's matrix(values, nrow=B_pilot, ncol=n) fill order) so that the
    // same set.seed() produces identical Mammen weights to the pure-R path.
    arma::mat V_pilot(B_pilot, n);
    for (int i = 0; i < n; i++)
        for (int b = 0; b < B_pilot; b++)
            V_pilot(b, i) = (R::runif(0.0, 1.0) < p1) ? w_lo : w_hi;

    arma::mat R_pilot = (V_pilot * PSI) / sqrt_n;   // B_pilot x m

    // Estimate sigma_half per column via normalised IQR
    arma::vec sigma_half(m);
    {
        std::vector<double> col_buf(B_pilot);
        for (int j = 0; j < m; j++) {
            for (int b = 0; b < B_pilot; b++) col_buf[b] = R_pilot(b, j);
            std::sort(col_buf.begin(), col_buf.end());
            // Type-7 quantile (linear interpolation) at p=0.25 and p=0.75
            auto q7 = [&](double p) -> double {
                double h = (B_pilot - 1) * p;
                int lo   = (int)std::floor(h);
                int hi   = lo + 1;
                if (hi >= B_pilot) hi = B_pilot - 1;
                return col_buf[lo] + (h - lo) * (col_buf[hi] - col_buf[lo]);
            };
            double iqr = q7(0.75) - q7(0.25);
            sigma_half(j) = std::max(iqr / iqr_normal, 1e-10);
        }
    }

    // ---- Main stage --------------------------------------------------------
    arma::mat V_main(B, n);
    for (int i = 0; i < n; i++)                       // column-major fill
        for (int b = 0; b < B; b++)
            V_main(b, i) = (R::runif(0.0, 1.0) < p1) ? w_lo : w_hi;

    arma::mat R_main = (V_main * PSI) / sqrt_n;   // B x m

    // Max studentised t-stat per draw
    std::vector<double> t_stats(B);
    for (int b = 0; b < B; b++) {
        double mx = 0.0;
        for (int j = 0; j < m; j++) {
            double t = std::abs(R_main(b, j)) / sigma_half(j);
            if (t > mx) mx = t;
        }
        t_stats[b] = mx;
    }

    // Critical value: type-7 quantile at (1 - alpha)
    std::sort(t_stats.begin(), t_stats.end());
    auto q7_ts = [&](double p) -> double {
        double h = (B - 1) * p;
        int lo   = (int)std::floor(h);
        int hi   = lo + 1;
        if (hi >= B) hi = B - 1;
        return t_stats[lo] + (h - lo) * (t_stats[hi] - t_stats[lo]);
    };
    double c_hat = q7_ts(1.0 - alpha);

    // Per-parameter CI half-widths
    arma::vec margin = c_hat * sigma_half / sqrt_n;

    return List::create(
        _["c_hat"]      = c_hat,
        _["margin"]     = NumericVector(margin.begin(), margin.end()),
        _["sigma_half"] = NumericVector(sigma_half.begin(), sigma_half.end())
    );
}

//' Compute CS unit-level influence functions in C++
//'
//' Given the unit x time outcome matrix \code{Y_mat} (n_units x n_times),
//' the per-unit timing vector \code{unit_timing} (NA_real_ for never-treated),
//' the 1-based column indices \code{col_t} and \code{col_base} into \code{Y_mat}
//' for each (g, t) pair, and the cohort values \code{gt_g} and calendar times
//' \code{gt_t}, computes the n_units x n_gt influence matrix \code{psi}.
//'
//' @noRd
// [[Rcpp::export]]
NumericMatrix compute_psi_cs_cpp(
    NumericMatrix Y_mat,
    NumericVector unit_timing,
    IntegerVector gt_g,
    IntegerVector gt_t,
    IntegerVector col_t,
    IntegerVector col_base,
    std::string   control_group
) {
    int n_units = Y_mat.nrow();
    int n_gt    = gt_g.size();
    NumericMatrix psi(n_units, n_gt);
    double* pPsi = psi.begin();
    const double* pY = Y_mat.begin();
    const double* pU = unit_timing.begin();
    bool never_only = (control_group == "nevertreated");

    std::vector<int> idx_tr, idx_ct;
    idx_tr.reserve(n_units);
    idx_ct.reserve(n_units);
    std::vector<double> dy(n_units);

    for (int j = 0; j < n_gt; j++) {
        int ct = col_t[j];
        int cb = col_base[j];
        if (ct == NA_INTEGER || cb == NA_INTEGER || ct < 1 || cb < 1) continue;

        const double* y_t    = pY + (size_t)(ct - 1) * n_units;
        const double* y_base = pY + (size_t)(cb - 1) * n_units;
        double g_j = (double)gt_g[j];
        double t_j = (double)gt_t[j];

        idx_tr.clear();
        idx_ct.clear();
        double sum_g = 0.0, sum_c = 0.0;

        for (int i = 0; i < n_units; i++) {
            double yt = y_t[i];
            double yb = y_base[i];
            if (ISNAN(yt) || ISNAN(yb)) continue;
            double d = yt - yb;
            dy[i] = d;

            double ut = pU[i];
            bool ut_na = ISNAN(ut);
            if (!ut_na && ut == g_j) {
                idx_tr.push_back(i);
                sum_g += d;
            }
            bool is_ctrl = never_only
                ? ut_na
                : ((ut_na || ut > t_j) && (ut_na || ut != g_j));
            if (is_ctrl) {
                idx_ct.push_back(i);
                sum_c += d;
            }
        }

        int n_tr = (int)idx_tr.size();
        int n_ct = (int)idx_ct.size();
        if (n_tr == 0 || n_ct == 0) continue;

        double mean_g = sum_g / n_tr;
        double mean_c = sum_c / n_ct;
        double w_tr   = (double)n_units / n_tr;
        double w_ct   = (double)n_units / n_ct;

        double* col_psi = pPsi + (size_t)j * n_units;
        for (int i : idx_tr) {
            col_psi[i] = w_tr * (dy[i] - mean_g);
        }
        for (int i : idx_ct) {
            col_psi[i] -= w_ct * (dy[i] - mean_c);
        }
    }

    return psi;
}
