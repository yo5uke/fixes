#include <Rcpp.h>
#ifdef _OPENMP
#include <omp.h>
#endif
using namespace Rcpp;

//' K-way fixed-effects demeaning by alternating projections
//'
//' Demeans each column of \code{M} with respect to all fixed-effect
//' dimensions simultaneously (Gauss-Seidel sweeps over the FE dimensions
//' until the largest subtracted group mean falls below \code{tol} times the
//' column scale). With a single FE dimension one sweep is exact.
//'
//' Columns are independent, so they are distributed over OpenMP threads;
//' the per-column arithmetic is unchanged by the thread count, making the
//' result bit-identical for any \code{nthreads}.
//'
//' \code{fe_ids} must contain dense 0-based level indices per dimension and
//' the rows must already form the final estimation sample (NA rows and
//' singleton FE levels removed by the caller, see \code{.fit_fe_ols()}).
//'
//' @param M Numeric matrix (N x C): columns to demean (outcome + regressors).
//' @param fe_ids Integer matrix (N x Q): 0-based FE level index per dimension.
//' @param n_levels Integer vector (length Q): number of levels per dimension.
//' @param tol Relative convergence tolerance on subtracted group means.
//' @param max_iter Maximum number of full sweeps per column.
//' @param nthreads Number of OpenMP threads (ignored without OpenMP).
//'
//' @return A list with \code{M} (demeaned copy) and \code{converged}.
//'
//' @noRd
// [[Rcpp::export]]
List demean_kway_cpp(NumericMatrix M,
                     IntegerMatrix fe_ids,
                     IntegerVector n_levels,
                     double tol,
                     int max_iter,
                     int nthreads) {
    int N = M.nrow();
    int C = M.ncol();
    int Q = fe_ids.ncol();
    if (nthreads < 1) nthreads = 1;

    NumericMatrix out(N, C);
    const double* pM  = M.begin();
    double* pOut      = out.begin();
    const int* pIds   = fe_ids.begin();
    std::vector<int> nl(Q);
    for (int q = 0; q < Q; q++) nl[q] = n_levels[q];

    std::vector<std::vector<int>> counts(Q);
    for (int q = 0; q < Q; q++) {
        counts[q].assign(nl[q], 0);
        const int* idq = pIds + (size_t)q * N;
        for (int k = 0; k < N; k++) counts[q][idq[k]]++;
    }

    int n_unconverged = 0;

#ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) \
        reduction(+:n_unconverged)
#endif
    for (int c = 0; c < C; c++) {
        std::vector<double> r(N);
        const double* col = pM + (size_t)c * N;
        double scale = 1.0;
        for (int k = 0; k < N; k++) {
            r[k] = col[k];
            double a = std::abs(r[k]);
            if (a > scale) scale = a;
        }

        std::vector<std::vector<double>> sums(Q);
        for (int q = 0; q < Q; q++) sums[q].resize(nl[q]);

        bool converged = (Q == 0);
        for (int it = 0; it < max_iter && !converged; it++) {
            double max_mean = 0.0;
            for (int q = 0; q < Q; q++) {
                const int* idq = pIds + (size_t)q * N;
                std::fill(sums[q].begin(), sums[q].end(), 0.0);
                for (int k = 0; k < N; k++) sums[q][idq[k]] += r[k];
                for (int g = 0; g < nl[q]; g++) {
                    sums[q][g] /= counts[q][g];
                    double a = std::abs(sums[q][g]);
                    if (a > max_mean) max_mean = a;
                }
                for (int k = 0; k < N; k++) r[k] -= sums[q][idq[k]];
            }
            if (max_mean < tol * scale) converged = true;
        }
        if (!converged) n_unconverged++;

        double* ocol = pOut + (size_t)c * N;
        for (int k = 0; k < N; k++) ocol[k] = r[k];
    }

    return List::create(
        _["M"]         = out,
        _["converged"] = (n_unconverged == 0)
    );
}
