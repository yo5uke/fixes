#include <Rcpp.h>
#include <unordered_map>
#include <vector>
using namespace Rcpp;

//' Build covariate-interaction matrix with group-level demeaning
//'
//' For each unique non-missing, non-zero value in \code{group_key}, the
//' covariates in that group are centred on their within-group column means.
//' The result is then multiplied element-wise with the 0/1 indicator matrix
//' to produce the N × (K × p) covariate-interaction matrix used by the TWM
//' and FLEX estimators (Wooldridge 2025, Procedure 5.1; Deb et al. 2024,
//' Eq. 2.11 / 3.1).
//'
//' \strong{TWM usage:} pass \code{group_key = as.integer(timing_vec)};
//' NA values (never-treated) are skipped and produce zero rows.
//'
//' \strong{FLEX usage:} pre-compute a cell key in R:
//' \preformatted{
//'   cohort_chr <- ifelse(is.na(cohort_of_obs), "NA",
//'                        as.character(cohort_of_obs))
//'   cell_key   <- as.integer(factor(paste(cohort_chr, time_id, sep="_")))
//'   cell_key[is.na(cohort_of_obs)] <- NA_integer_
//' }
//' This gives one unique integer per (cohort, calendar-time) cell.
//'
//' @param cov_mat    N × p numeric matrix of raw covariate values.
//' @param ind_mat    N × K integer matrix of 0/1 treatment-cell indicators
//'   (output of \code{build_indicator_matrix_cpp()}).
//' @param group_key  Integer vector of length N: group identifier for
//'   centring.  \code{NA_integer_} or \code{0} → row is excluded from all
//'   group means and contributes 0 to the output.
//'
//' @return N × (K × p) numeric matrix.  Column order: for each covariate j
//'   (j = 0 … p-1), the K columns \code{ind_mat[,k] * x_centred[,j]}
//'   for k = 0 … K-1 are contiguous.  Column names are set by the R caller.
//'
//' @noRd
// [[Rcpp::export]]
NumericMatrix build_cov_interactions_cpp(
    NumericMatrix cov_mat,
    IntegerMatrix ind_mat,
    IntegerVector group_key
) {
    int N = cov_mat.nrow();
    int p = cov_mat.ncol();
    int K = ind_mat.ncol();

    // ---- Step 1: group rows by group_key -----------------------------------
    // Skip NA_integer_ and 0.
    std::unordered_map<int, std::vector<int>> groups;
    groups.reserve(64);
    for (int i = 0; i < N; i++) {
        int g = group_key[i];
        if (IntegerVector::is_na(g) || g == 0) continue;
        groups[g].push_back(i);
    }

    // ---- Step 2: compute x_centred (N x p) --------------------------------
    // Allocate once, initialised to 0 (never-treated stays 0).
    NumericMatrix x_centred(N, p);

    for (auto& kv : groups) {
        const std::vector<int>& rows = kv.second;
        int ng = (int)rows.size();
        if (ng == 0) continue;

        for (int j = 0; j < p; j++) {
            // column mean over group rows
            double sum = 0.0;
            for (int r : rows) sum += cov_mat(r, j);
            double mean_val = sum / ng;
            // centre
            for (int r : rows) x_centred(r, j) = cov_mat(r, j) - mean_val;
        }
    }

    // ---- Step 3: build interaction matrix (N x K*p) -----------------------
    // out[i, j*K + k] = ind_mat[i,k] * x_centred[i,j]
    // Exploit sparsity of ind_mat: only fill where ind_mat(i,k) != 0.
    NumericMatrix out(N, K * p);

    for (int k = 0; k < K; k++) {
        for (int i = 0; i < N; i++) {
            if (ind_mat(i, k) == 0) continue;
            for (int j = 0; j < p; j++) {
                out(i, j * K + k) = x_centred(i, j);
                // ind_mat(i,k) is always 1 when non-zero, so no multiplication needed
            }
        }
    }

    return out;
}
