#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

//' Interaction-weighted event-study aggregation with full VCOV
//'
//' Computes the cohort-size-weighted (or cell-weight-weighted) event-study
//' estimate, its standard error, and the full event-study VCOV matrix for
//' each unique event time \eqn{\ell} in \code{unique_l}.
//'
//' @param estimates Numeric vector (length K): \eqn{\hat\tau_{g,s}} estimates.
//' @param l_vals Integer vector (length K): relative time \eqn{\ell = s - g}.
//' @param cohort_vals Integer vector (length K): cohort \eqn{g} for each cell.
//' @param idx_in_V Integer vector (length K): 0-based row/column index into
//'   \code{V_full_r}. Pass \code{-1L} for cells not found in the VCOV.
//' @param V_full_r Numeric matrix: full VCOV from the regression.
//' @param unique_l Sorted integer vector: all unique event times to aggregate.
//' @param cs_keys Integer vector: cohort values (one per cohort).
//' @param cs_vals Integer vector: cohort sizes corresponding to \code{cs_keys}.
//' @param min_t,max_t Integer: range of observed calendar times.
//' @param cell_weights Optional numeric vector (length K) of cell observation
//'   weight sums (used when estimation is weighted).
//'
//' @return A list with \code{es} (data.frame with \code{relative_time},
//'   \code{estimate}, \code{std_error}) and \code{vcov} (event-study VCOV).
//'
//' @noRd
// [[Rcpp::export]]
List aggregate_iw_cpp(
    NumericVector estimates,
    IntegerVector l_vals,
    IntegerVector cohort_vals,
    IntegerVector idx_in_V,
    NumericMatrix V_full_r,
    IntegerVector unique_l,
    IntegerVector cs_keys,
    IntegerVector cs_vals,
    int min_t,
    int max_t,
    Nullable<NumericVector> cell_weights = R_NilValue
) {
    arma::mat V_full(V_full_r.begin(), V_full_r.nrow(), V_full_r.ncol(), false);
    int n_V = (int)V_full.n_rows;

    bool use_cw = cell_weights.isNotNull();
    NumericVector cw_vec = use_cw ? NumericVector(cell_weights.get()) : NumericVector(0);

    int n_l = unique_l.size();
    std::vector<int> out_l;
    std::vector<double> out_est;
    std::vector<arma::uvec> w_idx_list;
    std::vector<arma::vec> w_val_list;
    out_l.reserve(n_l);
    out_est.reserve(n_l);

    for (int li = 0; li < n_l; li++) {
        int l = unique_l[li];

        std::vector<int> valid_k;
        for (int k = 0; k < l_vals.size(); k++)
            if (l_vals[k] == l && idx_in_V[k] >= 0) valid_k.push_back(k);
        if (valid_k.empty()) continue;

        int nv = (int)valid_k.size();
        arma::vec w(nv), tau(nv);
        arma::uvec idx_arma(nv);

        if (use_cw) {
            double cw_denom = 0.0;
            for (int vi = 0; vi < nv; vi++) cw_denom += cw_vec[valid_k[vi]];
            if (!(cw_denom > 0.0)) continue;
            for (int vi = 0; vi < nv; vi++) {
                int k = valid_k[vi];
                w[vi]        = cw_vec[k] / cw_denom;
                tau[vi]      = estimates[k];
                idx_arma[vi] = (arma::uword)idx_in_V[k];
            }
        } else {
            int sz_denom = 0;
            for (int ci = 0; ci < cs_keys.size(); ci++) {
                long long candidate = (long long)cs_keys[ci] + l;
                if (candidate >= min_t && candidate <= max_t)
                    sz_denom += cs_vals[ci];
            }
            if (sz_denom == 0) continue;

            for (int vi = 0; vi < nv; vi++) {
                int k = valid_k[vi];
                int coh = cohort_vals[k];
                int sz  = 0;
                for (int ci = 0; ci < cs_keys.size(); ci++)
                    if (cs_keys[ci] == coh) { sz = cs_vals[ci]; break; }
                w[vi]        = (double)sz / sz_denom;
                tau[vi]      = estimates[k];
                idx_arma[vi] = (arma::uword)idx_in_V[k];
            }
        }

        out_l.push_back(l);
        out_est.push_back(arma::as_scalar(w.t() * tau));
        w_idx_list.push_back(std::move(idx_arma));
        w_val_list.push_back(std::move(w));
    }

    int n_out = (int)out_l.size();
    arma::mat W(n_V, n_out, arma::fill::zeros);
    for (int j = 0; j < n_out; j++) {
        const arma::uvec& idx_arma = w_idx_list[j];
        const arma::vec& w = w_val_list[j];
        for (arma::uword vi = 0; vi < idx_arma.n_elem; vi++) {
            W(idx_arma[vi], j) += w[vi];
        }
    }

    arma::mat V_es = W.t() * V_full * W;
    NumericVector out_se(n_out);
    for (int j = 0; j < n_out; j++) {
        out_se[j] = std::sqrt(std::max(V_es(j, j), 0.0));
    }

    DataFrame es_df = DataFrame::create(
        _["relative_time"] = wrap(out_l),
        _["estimate"]      = wrap(out_est),
        _["std_error"]     = out_se
    );

    return List::create(
        _["es"]   = es_df,
        _["vcov"] = wrap(V_es)
    );
}
