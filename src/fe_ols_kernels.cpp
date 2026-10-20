#include <Rcpp.h>
#include <cstring>
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

//' Check row-wise NA across a numeric matrix
//'
//' Returns a logical vector of length N: TRUE where row \code{i} has no NA/NaN
//' in any column of \code{X}, plus a scalar flag \code{has_na} via attribute or
//' fast early check. Avoids allocating a logical matrix in R via
//' \code{rowSums(is.na(X)) == 0L}.
//'
//' @param X Numeric matrix (N x K).
//'
//' @return LogicalVector of length N (or length 0 if no NA/NaN exists anywhere
//'   in \code{X}).
//'
//' @noRd
// [[Rcpp::export]]
LogicalVector row_no_na_cpp(NumericMatrix X) {
    int N = X.nrow();
    int K = X.ncol();
    const double* pX = X.begin();
    size_t total = (size_t)N * K;

    // Fast scan: check if any NA/NaN exists at all
    size_t first_na = total;
    for (size_t idx = 0; idx < total; idx++) {
        if (ISNAN(pX[idx])) {
            first_na = idx;
            break;
        }
    }
    if (first_na == total) {
        return LogicalVector(0); // sentinel: no NAs in X
    }

    LogicalVector ok(N, TRUE);
    int* pOk = ok.begin();
    int start_j = (int)(first_na / (size_t)N);
    for (int j = start_j; j < K; j++) {
        const double* col = pX + (size_t)j * N;
        for (int i = 0; i < N; i++) {
            if (ISNAN(col[i])) pOk[i] = FALSE;
        }
    }
    return ok;
}

//' Subset and bind outcome vector y and regressor matrix X
//'
//' Constructs the N_ok x (1 + K) double matrix \code{[y | X]} over kept rows
//' in a single pass, and simultaneously computes pre-demeaning column sums of
//' squares \code{d_pre[j] = sum_i w_i * X_{i,j}^2} (with \code{w_i = 1} when
//' unweighted). Avoids R-level \code{X[ok, ]}, \code{cbind(yk, Xk)}, and
//' \code{colSums(Xk^2)} allocations.
//'
//' @param y Numeric vector (length N_full).
//' @param X Numeric matrix (N_full x K).
//' @param keep_idx Integer vector (1-based indices of kept rows), or length 0
//'   when all rows are kept.
//' @param w Numeric vector of kept-row weights (length N_ok), or length 0 when
//'   unweighted.
//' @param nthreads Number of OpenMP threads.
//'
//' @return List with \code{M} (N_ok x (1 + K) numeric matrix) and
//'   \code{d_pre} (length-K numeric vector).
//'
//' @noRd
// [[Rcpp::export]]
List bind_yx_cpp(NumericVector y,
                 NumericMatrix X,
                 IntegerVector keep_idx,
                 NumericVector w,
                 int nthreads) {
    int n_full = y.size();
    int K = X.ncol();
    bool subset = (keep_idx.size() > 0);
    int N = subset ? keep_idx.size() : n_full;
    bool weighted = (w.size() > 0);
    if (nthreads < 1) nthreads = 1;

    NumericMatrix M(N, 1 + K);
    NumericVector d_pre(K);

    const double* py = y.begin();
    const double* pX = X.begin();
    const int* pIdx  = subset ? keep_idx.begin() : nullptr;
    const double* pw = weighted ? w.begin() : nullptr;
    double* pM       = M.begin();
    double* pD       = d_pre.begin();

    if (subset) {
        for (int i = 0; i < N; i++) pM[i] = py[pIdx[i] - 1];
    } else {
        std::memcpy(pM, py, (size_t)N * sizeof(double));
    }

#ifdef _OPENMP
    #pragma omp parallel for schedule(static) num_threads(nthreads)
#endif
    for (int j = 0; j < K; j++) {
        const double* src = pX + (size_t)j * n_full;
        double* dst       = pM + (size_t)(j + 1) * N;
        double ss = 0.0;
        if (subset) {
            if (weighted) {
                for (int i = 0; i < N; i++) {
                    double v = src[pIdx[i] - 1];
                    dst[i] = v;
                    ss += pw[i] * v * v;
                }
            } else {
                for (int i = 0; i < N; i++) {
                    double v = src[pIdx[i] - 1];
                    dst[i] = v;
                    ss += v * v;
                }
            }
        } else {
            std::memcpy(dst, src, (size_t)N * sizeof(double));
            if (weighted) {
                for (int i = 0; i < N; i++) {
                    double v = dst[i];
                    ss += pw[i] * v * v;
                }
            } else {
                for (int i = 0; i < N; i++) {
                    double v = dst[i];
                    ss += v * v;
                }
            }
        }
        pD[j] = ss;
    }

    return List::create(_["M"] = M, _["d_pre"] = d_pre);
}

//' Compute residuals and VCOV meat for .fit_fe_ols()
//'
//' Given the demeaned (and sqrt(w)-scaled) matrix \code{Md = [y | X]} (N x (1+K)),
//' the 1-based \code{kept} column indices of X (length K_kept), and the
//' coefficient vector \code{b} (length K_kept), computes:
//' \itemize{
//'   \item residuals \code{e = yd - Xd_kept * b} and \code{rss = sum(e^2)}
//'   \item for \code{vcov_mode == 1} (hetero / HC1): the K_kept x K_kept meat
//'     \code{crossprod(Xd_kept * e)} without materializing \code{Xd_kept} or
//'     \code{Xd_kept * e}
//'   \item for \code{vcov_mode == 2} (one-way cluster): the G x K_kept cluster
//'     score matrix \code{S[g, a] = sum_{i in g} Xd_{i, kept[a]} * e_i} and its
//'     cross product \code{S'S}
//' }
//' All floating-point accumulations follow fixed index orders so results are
//' bit-identical across \code{nthreads}.
//'
//' @param Md Numeric matrix (N x (1 + K)): demeaned \code{[y | X]}.
//' @param kept Integer vector (length K_kept): 1-based kept column indices in X.
//' @param b Numeric vector (length K_kept): OLS coefficients.
//' @param vcov_mode Integer: 0 = iid (only residuals/rss), 1 = hetero meat,
//'   2 = cluster meat.
//' @param cl_i Integer vector: 1-based cluster index per row (length N) when
//'   \code{vcov_mode == 2}; ignored otherwise.
//' @param G Integer: number of clusters when \code{vcov_mode == 2}.
//' @param nthreads Number of OpenMP threads.
//'
//' @return List with \code{e} (length-N residual vector), \code{rss},
//'   \code{tss_yd} (\code{sum(yd^2)}), and \code{meat} (K_kept x K_kept matrix,
//'   or 0 x 0 when \code{vcov_mode == 0}).
//'
//' @noRd
// [[Rcpp::export]]
List fe_ols_meat_cpp(NumericMatrix Md,
                     IntegerVector kept,
                     NumericVector b,
                     int vcov_mode,
                     IntegerVector cl_i,
                     int G,
                     int nthreads) {
    int N  = Md.nrow();
    int Kk = kept.size();
    if (nthreads < 1) nthreads = 1;

    const double* pMd = Md.begin();
    const double* py  = pMd;
    const double* pb  = b.begin();

    std::vector<const double*> cols(Kk);
    for (int a = 0; a < Kk; a++) {
        cols[a] = pMd + (size_t)kept[a] * N;
    }

    // Step 1: e = yd - Xd_kept * b (matches R's yd - drop(Xd_k %*% b) row order)
    NumericVector e(N);
    double* pe = e.begin();
    for (int i = 0; i < N; i++) pe[i] = py[i];
    for (int a = 0; a < Kk; a++) {
        double ba = pb[a];
        const double* ca = cols[a];
        for (int i = 0; i < N; i++) {
            pe[i] -= ca[i] * ba;
        }
    }

    double rss = 0.0, tss_yd = 0.0;
    for (int i = 0; i < N; i++) {
        rss    += pe[i] * pe[i];
        tss_yd += py[i] * py[i];
    }

    if (vcov_mode == 0) {
        return List::create(
            _["e"]      = e,
            _["rss"]    = rss,
            _["tss_yd"] = tss_yd,
            _["meat"]   = NumericMatrix(0, 0)
        );
    }

    NumericMatrix meat(Kk, Kk);
    double* pm = meat.begin();

    if (vcov_mode == 1) {
        // Hetero meat: (Xd_k * e)' (Xd_k * e), bit-identical to crossprod_omp_cpp(Xd_k * e)
#ifdef _OPENMP
        #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
#endif
        for (int j = 0; j < Kk; j++) {
            const double* cj = cols[j];
            for (int i0 = 0; i0 <= j; i0 += 4) {
                int nb = j + 1 - i0;
                if (nb > 4) nb = 4;
                if (nb == 4) {
                    const double* c0 = cols[i0];
                    const double* c1 = cols[i0 + 1];
                    const double* c2 = cols[i0 + 2];
                    const double* c3 = cols[i0 + 3];
                    double a0 = 0.0, a1 = 0.0, a2 = 0.0, a3 = 0.0;
                    for (int k = 0; k < N; k++) {
                        double ek  = pe[k];
                        double mjk = cj[k] * ek;
                        a0 += (c0[k] * ek) * mjk;
                        a1 += (c1[k] * ek) * mjk;
                        a2 += (c2[k] * ek) * mjk;
                        a3 += (c3[k] * ek) * mjk;
                    }
                    double acc[4] = {a0, a1, a2, a3};
                    for (int b_i = 0; b_i < 4; b_i++) {
                        pm[(size_t)j * Kk + (i0 + b_i)] = acc[b_i];
                        pm[(size_t)(i0 + b_i) * Kk + j] = acc[b_i];
                    }
                } else {
                    for (int b_i = 0; b_i < nb; b_i++) {
                        const double* ci = cols[i0 + b_i];
                        double a = 0.0;
                        for (int k = 0; k < N; k++) {
                            double ek = pe[k];
                            a += (ci[k] * ek) * (cj[k] * ek);
                        }
                        pm[(size_t)j * Kk + (i0 + b_i)] = a;
                        pm[(size_t)(i0 + b_i) * Kk + j] = a;
                    }
                }
            }
        }
    } else if (vcov_mode == 2) {
        // Cluster scores S (G x Kk): column-parallel over Kk, then S'S
        std::vector<double> S((size_t)G * Kk, 0.0);
        const int* pcl = cl_i.begin();

#ifdef _OPENMP
        #pragma omp parallel for schedule(static) num_threads(nthreads)
#endif
        for (int a = 0; a < Kk; a++) {
            const double* ca = cols[a];
            double* sa       = &S[(size_t)a * G];
            for (int i = 0; i < N; i++) {
                sa[pcl[i] - 1] += ca[i] * pe[i];
            }
        }

#ifdef _OPENMP
        #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
#endif
        for (int j = 0; j < Kk; j++) {
            const double* sj = &S[(size_t)j * G];
            for (int i = 0; i <= j; i++) {
                const double* si = &S[(size_t)i * G];
                double acc = 0.0;
                for (int g = 0; g < G; g++) acc += si[g] * sj[g];
                pm[(size_t)j * Kk + i] = acc;
                pm[(size_t)i * Kk + j] = acc;
            }
        }
    }

    return List::create(
        _["e"]      = e,
        _["rss"]    = rss,
        _["tss_yd"] = tss_yd,
        _["meat"]   = meat
    );
}

//' Expand classic TWFE event-study interaction dummies in C++
//'
//' Builds the N x L numeric matrix of interaction dummies
//' \code{X[i, j] = tr_vals[i] * 1(f_vals[i] == lv[j])}, setting rows with NA
//' in \code{f_vals} or \code{tr_vals} to \code{NA_real_} across all columns
//' (matching \code{.build_twfe_design()}).
//'
//' @param f_vals Numeric vector (length N): event-time or calendar-time factor.
//' @param tr_vals Numeric vector (length N): binary treatment indicator.
//' @param lv Numeric vector (length L): sorted non-reference levels.
//'
//' @return NumericMatrix (N x L).
//'
//' @noRd
// [[Rcpp::export]]
NumericMatrix build_twfe_design_cpp(NumericVector f_vals,
                                    NumericVector tr_vals,
                                    NumericVector lv) {
    int N = f_vals.size();
    int L = lv.size();
    NumericMatrix X(N, L);
    double* pX = X.begin();
    const double* pf = f_vals.begin();
    const double* pt = tr_vals.begin();
    const double* pl = lv.begin();

    // Check if there are any NAs and map values to level indices
    bool any_na = false;
    for (int i = 0; i < N; i++) {
        if (ISNAN(pf[i]) || ISNAN(pt[i])) { any_na = true; break; }
    }

    // lv is sorted ascending; use binary search per row to set the single non-zero column
    for (int i = 0; i < N; i++) {
        double fi = pf[i];
        double ti = pt[i];
        if (ISNAN(fi) || ISNAN(ti) || ti == 0.0) continue;
        // binary search for fi in pl[0..L-1]
        int lo = 0, hi = L - 1;
        while (lo <= hi) {
            int mid = (lo + hi) >> 1;
            double vm = pl[mid];
            if (vm < fi) lo = mid + 1;
            else if (vm > fi) hi = mid - 1;
            else {
                pX[(size_t)mid * N + i] = ti;
                break;
            }
        }
    }

    if (any_na) {
        for (int j = 0; j < L; j++) {
            double* col = pX + (size_t)j * N;
            for (int i = 0; i < N; i++) {
                if (ISNAN(pf[i]) || ISNAN(pt[i])) col[i] = NA_REAL;
            }
        }
    }

    return X;
}

