#include <Rcpp.h>
#ifdef _OPENMP
#include <omp.h>
#endif
using namespace Rcpp;

//' Sequential Cholesky with keep-first column dropping
//'
//' In-place C++ counterpart of the previous R implementation in
//' \code{.chol_seq_drop()}: columns of \code{XtX} are processed left to
//' right; a column is dropped when its diagonal is numerically zero
//' relative to \code{d_pre} (absorbed by the FEs) or when its residual
//' diagonal after projecting on the previously kept columns falls below
//' \code{tol} times its own diagonal (collinear with kept columns).
//'
//' @param XtX Symmetric cross-product matrix (K x K).
//' @param d_pre Numeric vector (length K): pre-demeaning column scales.
//' @param tol Relative drop tolerance.
//'
//' @return A list with \code{kept} (1-based indices of kept columns) and
//'   \code{R} (upper-triangular Cholesky factor of the kept block).
//'
//' @noRd
// [[Rcpp::export]]
List chol_seq_drop_cpp(NumericMatrix XtX, NumericVector d_pre, double tol) {
    int K = XtX.ncol();
    const double* px = XtX.begin();

    // Rbuf column j holds the factor entries of variable j over the kept
    // positions (0..a-1) plus its diagonal at position a when kept.
    std::vector<double> Rbuf((size_t)K * K, 0.0);
    std::vector<int> kept;
    kept.reserve(K);
    std::vector<double> z(K);

    for (int j = 0; j < K; j++) {
        double d0j = px[(size_t)j * K + j];
        double ref = d_pre[j] > 0.0 ? d_pre[j] : 0.0;
        if (!std::isfinite(d0j) || d0j <= tol * ref) continue;

        size_t a_n = kept.size();
        double d = d0j;
        for (size_t a = 0; a < a_n; a++) {
            int i = kept[a];
            const double* Ri = &Rbuf[(size_t)i * K];
            double s = px[(size_t)j * K + i];
            for (size_t m = 0; m < a; m++) s -= Ri[m] * z[m];
            z[a] = s / Ri[a];
            d -= z[a] * z[a];
        }
        if (d <= tol * d0j) continue;

        double* Rj = &Rbuf[(size_t)j * K];
        for (size_t m = 0; m < a_n; m++) Rj[m] = z[m];
        Rj[a_n] = std::sqrt(d);
        kept.push_back(j);
    }

    int Kk = (int)kept.size();
    NumericMatrix R_out(Kk, Kk);
    for (int a = 0; a < Kk; a++) {
        const double* Rj = &Rbuf[(size_t)kept[a] * K];
        for (int m = 0; m <= a; m++) R_out(m, a) = Rj[m];
    }
    IntegerVector kept_out(Kk);
    for (int a = 0; a < Kk; a++) kept_out[a] = kept[a] + 1;

    return List::create(_["kept"] = kept_out, _["R"] = R_out);
}

//' Symmetric cross product M'M with OpenMP column parallelism
//'
//' Computes \code{crossprod(M)} with a fixed per-entry accumulation order,
//' so the result is bit-identical for any thread count. Columns of the
//' output are distributed over threads; the inner accumulation processes
//' the left columns in blocks of four to reduce memory traffic.
//'
//' @param M Numeric matrix (N x C).
//' @param nthreads Number of OpenMP threads (ignored without OpenMP).
//'
//' @return C x C numeric matrix M'M (no dimnames).
//'
//' @noRd
// [[Rcpp::export]]
NumericMatrix crossprod_omp_cpp(NumericMatrix M, int nthreads) {
    int N = M.nrow();
    int C = M.ncol();
    NumericMatrix out(C, C);
    const double* p = M.begin();
    double* po = out.begin();
    if (nthreads < 1) nthreads = 1;

#ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
#endif
    for (int j = 0; j < C; j++) {
        const double* cj = p + (size_t)j * N;
        for (int i0 = 0; i0 <= j; i0 += 4) {
            int nb = j + 1 - i0;
            if (nb > 4) nb = 4;
            const double* c0 = p + (size_t)i0 * N;
            if (nb == 4) {
                const double* c1 = c0 + N;
                const double* c2 = c1 + N;
                const double* c3 = c2 + N;
                double a0 = 0.0, a1 = 0.0, a2 = 0.0, a3 = 0.0;
                for (int k = 0; k < N; k++) {
                    double mj = cj[k];
                    a0 += c0[k] * mj;
                    a1 += c1[k] * mj;
                    a2 += c2[k] * mj;
                    a3 += c3[k] * mj;
                }
                double acc[4] = {a0, a1, a2, a3};
                for (int b = 0; b < 4; b++) {
                    po[(size_t)j * C + (i0 + b)] = acc[b];
                    po[(size_t)(i0 + b) * C + j] = acc[b];
                }
            } else {
                for (int b = 0; b < nb; b++) {
                    const double* ci = c0 + (size_t)b * N;
                    double a = 0.0;
                    for (int k = 0; k < N; k++) a += ci[k] * cj[k];
                    po[(size_t)j * C + (i0 + b)] = a;
                    po[(size_t)(i0 + b) * C + j] = a;
                }
            }
        }
    }
    return out;
}
