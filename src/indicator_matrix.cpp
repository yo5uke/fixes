#include <Rcpp.h>
#include <unordered_map>
using namespace Rcpp;

//' Build cohort-by-calendar-time indicator matrix
//'
//' For each (g, s) pair, fills column k of the output matrix with 1 for rows
//' where \code{cohort_id[i] == gs_g[k] && time_id[i] == gs_s[k]}, and 0
//' elsewhere.  \code{NA_integer_} values in \code{cohort_id} (never-treated
//' units) are treated as no-cohort and always produce 0.
//'
//' Uses an O(K) hash map from \code{(g, s)} to column index \code{k}, so
//' filling the matrix takes a single O(N) pass over the rows instead of
//' O(N * K).
//'
//' @param cohort_id Integer vector (length N): cohort assignment per row.
//'   Pass \code{NA_integer_} for never-treated observations.
//'   For FLEX, pass the cohort of each observation's group
//'   (pre-computed as \code{timing_by_group[as.character(group_vec)]}).
//' @param time_id Integer vector (length N): calendar time per row.
//' @param gs_g Integer vector (length K): cohort value for each (g,s) pair.
//' @param gs_s Integer vector (length K): calendar time for each (g,s) pair.
//'
//' @return An N × K integer matrix with 0/1 entries.
//'
//' @noRd
// [[Rcpp::export]]
IntegerMatrix build_indicator_matrix_cpp(
    IntegerVector cohort_id,
    IntegerVector time_id,
    IntegerVector gs_g,
    IntegerVector gs_s
) {
    int N = cohort_id.size();
    int K = gs_g.size();
    IntegerMatrix mat(N, K);
    if (K == 0 || N == 0) return mat;

    std::unordered_map<long long, int> pair_to_col;
    pair_to_col.reserve((size_t)K * 2);
    for (int k = 0; k < K; k++) {
        long long key = (((long long)gs_g[k]) << 32) | (unsigned int)gs_s[k];
        pair_to_col[key] = k;
    }

    int* pMat      = mat.begin();
    const int* pC  = cohort_id.begin();
    const int* pT  = time_id.begin();

    for (int i = 0; i < N; i++) {
        int c = pC[i];
        int t = pT[i];
        if (c == NA_INTEGER || t == NA_INTEGER) continue;
        long long key = (((long long)c) << 32) | (unsigned int)t;
        auto it = pair_to_col.find(key);
        if (it != pair_to_col.end()) {
            pMat[(size_t)it->second * N + i] = 1;
        }
    }
    return mat;
}
