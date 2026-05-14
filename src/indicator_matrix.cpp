#include <Rcpp.h>
using namespace Rcpp;

//' Build cohort-by-calendar-time indicator matrix
//'
//' For each (g, s) pair, fills column k of the output matrix with 1 for rows
//' where \code{cohort_id[i] == gs_g[k] && time_id[i] == gs_s[k]}, and 0
//' elsewhere.  \code{NA_integer_} values in \code{cohort_id} (never-treated
//' units) are treated as no-cohort and always produce 0.
//'
//' This is a shared utility used by the SA, TWM, and FLEX estimators to
//' replace the equivalent R nested for-loop, avoiding repeated vector
//' allocation and R overhead per column.
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

    for (int k = 0; k < K; k++) {
        int g = gs_g[k];
        int s = gs_s[k];
        for (int i = 0; i < N; i++) {
            mat(i, k) = (!IntegerVector::is_na(cohort_id[i]) &&
                         cohort_id[i] == g &&
                         time_id[i]   == s) ? 1 : 0;
        }
    }
    return mat;
}
