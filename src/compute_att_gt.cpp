// compute_att_gt.cpp
//
// Vectorized ATT(g,t) computation for the Callaway-Sant'Anna (2021)
// unconditional DiD estimator.
//
// Reference: Callaway & Sant'Anna (2021), "Difference-in-Differences with
// Multiple Time Periods", Journal of Econometrics 225(2), pp. 200-230.
// Equation (2.8):
//   ATT(g, t) = E[Y_t - Y_{g-1} | G = g] - E[Y_t - Y_{g-1} | C_t]
// where C_t is the clean comparison group and g-1 is the cohort-specific
// base period.
//
// This replaces the R nested for(g){for(t)} loop that called
// merge.data.frame and %in% on every (g,t) pair.  Key data structures:
//   - std::unordered_map<int,double>  : O(1) base-period outcome lookup
//   - std::unordered_set<int>         : O(1) cohort membership test
//
// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <string>

using namespace Rcpp;

// ---------------------------------------------------------------------------
// Helper: compute mean and variance of a std::vector<double>
// Returns {mean, variance} using Welford's online algorithm (numerically
// stable, single-pass).
// ---------------------------------------------------------------------------
static std::pair<double, double> mean_var(const std::vector<double>& v) {
  if (v.empty()) return {0.0, 0.0};
  double m = 0.0, m2 = 0.0;
  int n = 0;
  for (double x : v) {
    ++n;
    double delta = x - m;
    m  += delta / n;
    m2 += delta * (x - m);
  }
  double var = (n > 1) ? m2 / (n - 1) : 0.0;   // matches R var() denominator
  return {m, var};
}

// ---------------------------------------------------------------------------
// compute_att_gt_cpp
//
// Parameters
//   unit_id      : integer vector, unit index (1-based dense 1..n_units, length N)
//   time_id      : integer vector, calendar time for each obs (length N)
//   outcome      : numeric vector, Y_it (length N)
//   cohort       : integer vector, G_i (0 = never-treated) (length N)
//   cohorts      : unique treated cohort values (sorted)
//   all_times    : all distinct calendar periods (sorted)
//   control_group: "nevertreated" or "notyettreated"
//
// Returns a DataFrame with columns g, t, att, se — one row per (g,t) pair
// for which both a treated group and comparison group exist.
// ---------------------------------------------------------------------------
// [[Rcpp::export]]
Rcpp::DataFrame compute_att_gt_cpp(
    Rcpp::IntegerVector unit_id,
    Rcpp::IntegerVector time_id,
    Rcpp::NumericVector outcome,
    Rcpp::IntegerVector cohort,
    Rcpp::IntegerVector cohorts,
    Rcpp::IntegerVector all_times,
    std::string         control_group,
    int                 anticipation = 0
) {
  int N = unit_id.size();
  int n_times = all_times.size();
  if (N == 0 || n_times == 0) {
    return Rcpp::DataFrame::create(
      Rcpp::Named("g") = IntegerVector(0),
      Rcpp::Named("t") = IntegerVector(0),
      Rcpp::Named("att") = NumericVector(0),
      Rcpp::Named("se") = NumericVector(0),
      Rcpp::Named("stringsAsFactors") = false
    );
  }

  int max_u = 0;
  for (int i = 0; i < N; ++i) {
    if (unit_id[i] > max_u) max_u = unit_id[i];
  }
  int n_units = max_u;

  int min_t = all_times[0];
  int max_t = all_times[n_times - 1];
  for (int ti = 0; ti < n_times; ++ti) {
    if (all_times[ti] < min_t) min_t = all_times[ti];
    if (all_times[ti] > max_t) max_t = all_times[ti];
  }
  std::vector<int> time_to_ti((size_t)(max_t - min_t + 1), -1);
  for (int ti = 0; ti < n_times; ++ti) {
    time_to_ti[all_times[ti] - min_t] = ti;
  }

  // Dense unit x time table (stored as 1D vector of size n_units * n_times)
  std::vector<double> Y_grid((size_t)n_units * n_times, NA_REAL);
  std::vector<int>    unit_cohort(n_units + 1, -1);
  std::vector<int>    all_units;
  all_units.reserve(n_units);

  for (int i = 0; i < N; ++i) {
    int u = unit_id[i];
    int t = time_id[i];
    if (t >= min_t && t <= max_t) {
      int ti = time_to_ti[t - min_t];
      if (ti >= 0) {
        Y_grid[(size_t)(u - 1) * n_times + ti] = outcome[i];
      }
    }
    if (unit_cohort[u] == -1) {
      all_units.push_back(u);
    }
    unit_cohort[u] = cohort[i];
  }

  // Group units by cohort (in discovery order)
  std::vector<int> never_units;
  never_units.reserve(n_units);
  int n_coh = cohorts.size();
  std::vector<std::vector<int>> cohort_units(n_coh);
  for (int u : all_units) {
    int g = unit_cohort[u];
    if (g == 0) {
      never_units.push_back(u);
    } else {
      for (int gi = 0; gi < n_coh; ++gi) {
        if (cohorts[gi] == g) {
          cohort_units[gi].push_back(u);
          break;
        }
      }
    }
  }

  // ---- Output accumulators ---------------------------------------------------
  std::vector<int>    out_g, out_t;
  std::vector<double> out_att, out_se;

  std::vector<double> delta_treat, delta_ctrl;

  // ---- Main nested loop: for each cohort g, for each calendar time t --------
  for (int gi = 0; gi < n_coh; ++gi) {
    int g      = cohorts[gi];
    int base_t = g - 1 - anticipation;   // CS 2021, eq. 2.8; shifted by anticipation

    if (base_t < min_t || base_t > max_t) continue;
    int base_ti = time_to_ti[base_t - min_t];
    if (base_ti < 0) continue;

    const std::vector<int>& treat_units = cohort_units[gi];
    if (treat_units.empty()) continue;

    for (int ti = 0; ti < n_times; ++ti) {
      int t = all_times[ti];
      if (t == base_t) continue;   // ATT(g, base_t) = 0 by construction

      // ---- Treated group: ΔY = Y_it - Y_i(base_t) -------------------------
      delta_treat.clear();
      for (int u : treat_units) {
        double y_base = Y_grid[(size_t)(u - 1) * n_times + base_ti];
        double y_t    = Y_grid[(size_t)(u - 1) * n_times + ti];
        if (!ISNAN(y_base) && !ISNAN(y_t)) {
          delta_treat.push_back(y_t - y_base);
        }
      }
      if (delta_treat.empty()) continue;

      // ---- Control group ΔY ------------------------------------------------
      delta_ctrl.clear();
      if (control_group == "nevertreated") {
        for (int u : never_units) {
          double y_base = Y_grid[(size_t)(u - 1) * n_times + base_ti];
          double y_t    = Y_grid[(size_t)(u - 1) * n_times + ti];
          if (!ISNAN(y_base) && !ISNAN(y_t)) {
            delta_ctrl.push_back(y_t - y_base);
          }
        }
      } else {
        // "notyettreated": G_i > t (including never-treated) excluding cohort g
        for (int u : all_units) {
          int uc = unit_cohort[u];
          bool is_nyt = (uc == 0 || uc > t) && (uc != g);
          if (!is_nyt) continue;
          double y_base = Y_grid[(size_t)(u - 1) * n_times + base_ti];
          double y_t    = Y_grid[(size_t)(u - 1) * n_times + ti];
          if (!ISNAN(y_base) && !ISNAN(y_t)) {
            delta_ctrl.push_back(y_t - y_base);
          }
        }
      }
      if (delta_ctrl.empty()) continue;

      // ---- ATT(g,t) and delta-method SE (CS 2021, eq. 2.8) ----------------
      std::pair<double,double> mv_g = mean_var(delta_treat);
      std::pair<double,double> mv_c = mean_var(delta_ctrl);
      double mean_g = mv_g.first,  var_g = mv_g.second;
      double mean_c = mv_c.first,  var_c = mv_c.second;

      int n_g = (int)delta_treat.size();
      int n_c = (int)delta_ctrl.size();

      double att = mean_g - mean_c;
      double se  = std::sqrt(var_g / n_g + var_c / n_c);

      out_g.push_back(g);
      out_t.push_back(t);
      out_att.push_back(att);
      out_se.push_back(se);
    }
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("g")   = out_g,
    Rcpp::Named("t")   = out_t,
    Rcpp::Named("att") = out_att,
    Rcpp::Named("se")  = out_se,
    Rcpp::Named("stringsAsFactors") = false
  );
}
