#include <Rcpp.h>
#include <vector>
using namespace Rcpp;

//' Sums of a vector by 1-based integer group code
//'
//' @param code Integer codes in 1..n.
//' @param x Numeric vector of the same length.
//' @param n Number of groups.
//'
//' @return Numeric vector of length n.
//'
//' @noRd
// [[Rcpp::export]]
NumericVector sum_by_code_cpp(IntegerVector code, NumericVector x, int n) {
    NumericVector out(n);
    int N = code.size();
    for (int k = 0; k < N; k++) out[code[k] - 1] += x[k];
    return out;
}

//' Sums of gathered matrix rows by 1-based integer group code
//'
//' Row \code{to[k]} of the result accumulates \code{wt[k] * X[from[k], ]}
//' for every k, i.e. \eqn{A_{to}' \mathrm{diag}(wt) A_{from} X} for the
//' dummy matrices of the two codes.
//'
//' @param to,from Integer codes (same length); \code{from} indexes the rows
//'   of \code{X}, \code{to} takes values in 1..n.
//' @param X Numeric matrix.
//' @param n Number of rows of the result.
//' @param wt Row weights (same length as \code{to}), or \code{NULL} for 1.
//'
//' @return Numeric matrix, n x ncol(X).
//'
//' @noRd
// [[Rcpp::export]]
NumericMatrix sum_rows_by_code_cpp(IntegerVector to, IntegerVector from,
                                   NumericMatrix X, int n,
                                   Nullable<NumericVector> wt = R_NilValue) {
    int N = to.size();
    int nx = X.nrow();
    int m = X.ncol();
    NumericVector w = wt.isNull() ? NumericVector(N, 1.0) : NumericVector(wt.get());
    const int* pt = to.begin();
    const int* pf = from.begin();
    const double* pw = w.begin();

    // Row-major work copies, so that the m values of a row are contiguous.
    std::vector<double> xt((size_t)nx * m), ot((size_t)n * m, 0.0);
    const double* pX = X.begin();
    for (int j = 0; j < m; j++)
        for (int i = 0; i < nx; i++) xt[(size_t)i * m + j] = pX[(size_t)j * nx + i];

    for (int k = 0; k < N; k++) {
        const double* x = &xt[(size_t)(pf[k] - 1) * m];
        double* o = &ot[(size_t)(pt[k] - 1) * m];
        double wk = pw[k];
        for (int j = 0; j < m; j++) o[j] += wk * x[j];
    }

    NumericMatrix out(n, m);
    double* pO = out.begin();
    for (int j = 0; j < m; j++)
        for (int i = 0; i < n; i++) pO[(size_t)j * n + i] = ot[(size_t)i * m + j];
    return out;
}
