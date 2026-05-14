#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

//' Interaction-weighted event-study aggregation with full VCOV
//'
//' Computes the cohort-size-weighted event-study estimate and its standard
//' error for each unique event time \eqn{\ell} in \code{unique_l}:
//' \deqn{\hat\theta(\ell) = \sum_g w_{g,\ell} \hat\tau_{g,g+\ell}}
//' \deqn{\widehat{SE}^2(\hat\theta(\ell)) =
//'   \mathbf{w}_\ell^\top \Sigma_\ell \mathbf{w}_\ell}
//' where \eqn{\Sigma_\ell} is the VCOV sub-block for all cohorts at \eqn{\ell}
//' and \eqn{w_{g,\ell} = n_g / \sum_{g' : g'+\ell \in [t_{\min}, t_{\max}]} n_{g'}}.
//'
//' This is a shared utility replacing the equivalent R loop in the SA, TWM,
//' and FLEX estimators, using RcppArmadillo for the quadratic form.
//'
//' @param estimates Numeric vector (length K): \eqn{\hat\tau_{g,s}} estimates.
//' @param l_vals Integer vector (length K): relative time \eqn{\ell = s - g}.
//' @param cohort_vals Integer vector (length K): cohort \eqn{g} for each cell.
//' @param idx_in_V Integer vector (length K): 0-based row/column index into
//'   \code{V_full_r}.  Pass \code{-1L} for cells not found in the VCOV.
//' @param V_full_r Numeric matrix: full VCOV from the regression.
//' @param unique_l Sorted integer vector: all unique event times to aggregate.
//' @param cs_keys Integer vector: cohort values (one per cohort).
//' @param cs_vals Integer vector: cohort sizes corresponding to \code{cs_keys}.
//' @param min_t,max_t Integer: range of observed calendar times.
//'
//' @return A data.frame with columns \code{relative_time}, \code{estimate},
//'   \code{std_error}, one row per valid event time.
//'
//' @noRd
// [[Rcpp::export]]
DataFrame aggregate_iw_cpp(
    NumericVector estimates,
    IntegerVector l_vals,
    IntegerVector cohort_vals,
    IntegerVector idx_in_V,
    NumericMatrix V_full_r,
    IntegerVector unique_l,
    IntegerVector cs_keys,
    IntegerVector cs_vals,
    int min_t,
    int max_t
) {
    arma::mat V_full(V_full_r.begin(), V_full_r.nrow(), V_full_r.ncol(), false);

    int n_l = unique_l.size();
    IntegerVector out_l;
    NumericVector out_est, out_se;

    for (int li = 0; li < n_l; li++) {
        int l = unique_l[li];

        // collect valid rows for this l
        std::vector<int> valid_k;
        for (int k = 0; k < l_vals.size(); k++)
            if (l_vals[k] == l && idx_in_V[k] >= 0) valid_k.push_back(k);
        if (valid_k.empty()) continue;

        // in-sample cohort size denominator
        int sz_denom = 0;
        for (int ci = 0; ci < cs_keys.size(); ci++) {
            long long candidate = (long long)cs_keys[ci] + l;
            if (candidate >= min_t && candidate <= max_t)
                sz_denom += cs_vals[ci];
        }
        if (sz_denom == 0) continue;

        int nv = (int)valid_k.size();
        arma::vec w(nv), tau(nv);
        arma::uvec idx_arma(nv);
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
        arma::mat V_sub  = V_full.submat(idx_arma, idx_arma);
        double theta     = arma::as_scalar(w.t() * tau);
        double var_theta = arma::as_scalar(w.t() * V_sub * w);

        out_l.push_back(l);
        out_est.push_back(theta);
        out_se.push_back(std::sqrt(std::max(var_theta, 0.0)));
    }

    return DataFrame::create(
        _["relative_time"] = out_l,
        _["estimate"]      = out_est,
        _["std_error"]     = out_se
    );
}
