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
#include <unordered_map>
#include <unordered_set>
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
//   unit_id      : integer vector, unit index (1-based, length N)
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

  // ---- Index observations by (unit, time) for O(1) lookup ------------------
  // Key: (unit << 32) | time  (collision-free for any pair of int32 values)
  // Value: row index in the input vectors
  std::unordered_map<long long, int> obs_index;
  obs_index.reserve(N * 2);
  for (int i = 0; i < N; ++i) {
    long long key = (((long long)unit_id[i]) << 32) | (unsigned int)time_id[i];
    obs_index[key] = i;
  }

  // ---- Sets of units by cohort for O(1) membership test --------------------
  // never_set  : units with cohort == 0
  // cohort_map : cohort_value -> unordered_set of unit ids
  std::unordered_set<int> never_set;
  std::unordered_map<int, std::unordered_set<int>> cohort_unit_map;
  for (int i = 0; i < N; ++i) {
    int u = unit_id[i], g = cohort[i];
    if (g == 0) {
      never_set.insert(u);
    } else {
      cohort_unit_map[g].insert(u);
    }
  }

  // For "notyettreated" we also need quick access to all unique (unit, cohort)
  // pairs so we can filter units with G_i > t.  We store each unit's cohort.
  std::unordered_map<int, int> unit_cohort;
  for (int i = 0; i < N; ++i) {
    unit_cohort[unit_id[i]] = cohort[i];
  }

  // Collect all unique units
  std::vector<int> all_units;
  {
    std::unordered_set<int> seen;
    for (int i = 0; i < N; ++i) {
      if (seen.insert(unit_id[i]).second) all_units.push_back(unit_id[i]);
    }
  }

  // ---- Output accumulators ---------------------------------------------------
  std::vector<int>    out_g, out_t;
  std::vector<double> out_att, out_se;

  // ---- Main nested loop: for each cohort g, for each calendar time t --------
  for (int gi = 0; gi < cohorts.size(); ++gi) {
    int g      = cohorts[gi];
    int base_t = g - 1 - anticipation;   // CS 2021, eq. 2.8; shifted by anticipation

    // base period must be in the sample
    bool base_in_sample = false;
    for (int ti = 0; ti < all_times.size(); ++ti) {
      if (all_times[ti] == base_t) { base_in_sample = true; break; }
    }
    if (!base_in_sample) continue;

    // Units in cohort g
    auto it = cohort_unit_map.find(g);
    if (it == cohort_unit_map.end()) continue;
    const std::unordered_set<int>& treat_units = it->second;

    // Pre-compute base-period outcomes for cohort g: O(|treat_units|)
    std::unordered_map<int, double> base_g;   // unit -> Y_i(base_t)
    base_g.reserve(treat_units.size() * 2);
    for (int u : treat_units) {
      long long key = (((long long)u) << 32) | (unsigned int)base_t;
      auto oit = obs_index.find(key);
      if (oit != obs_index.end()) base_g[u] = outcome[oit->second];
    }

    for (int ti = 0; ti < all_times.size(); ++ti) {
      int t = all_times[ti];
      if (t == base_t) continue;   // ATT(g, base_t) = 0 by construction

      // ---- Treated group: ΔY = Y_it - Y_i(base_t) -------------------------
      std::vector<double> delta_treat;
      delta_treat.reserve(treat_units.size());
      for (int u : treat_units) {
        auto bit = base_g.find(u);
        if (bit == base_g.end()) continue;

        long long key = (((long long)u) << 32) | (unsigned int)t;
        auto oit = obs_index.find(key);
        if (oit != obs_index.end())
          delta_treat.push_back(outcome[oit->second] - bit->second);
      }
      if (delta_treat.empty()) continue;

      // ---- Control group ΔY ------------------------------------------------
      // Determine control unit set for this (g, t)
      std::vector<double> delta_ctrl;

      if (control_group == "nevertreated") {
        // Pre-compute base-period outcomes for never-treated: done once per g
        // (base_t is fixed for this g), but we need it per-t for out_c lookup.
        // We do it inline here; the unordered_set lookup is O(1) per unit.
        delta_ctrl.reserve(never_set.size());
        for (int u : never_set) {
          long long key_t    = (((long long)u) << 32) | (unsigned int)t;
          long long key_base = (((long long)u) << 32) | (unsigned int)base_t;
          auto oit_t    = obs_index.find(key_t);
          auto oit_base = obs_index.find(key_base);
          if (oit_t != obs_index.end() && oit_base != obs_index.end())
            delta_ctrl.push_back(outcome[oit_t->second] -
                                  outcome[oit_base->second]);
        }
      } else {
        // "notyettreated": G_i > t (including never-treated) excluding cohort g
        delta_ctrl.reserve(all_units.size());
        for (int u : all_units) {
          int uc = unit_cohort[u];
          bool is_nyt = (uc == 0 || uc > t) && (uc != g);
          if (!is_nyt) continue;

          long long key_t    = (((long long)u) << 32) | (unsigned int)t;
          long long key_base = (((long long)u) << 32) | (unsigned int)base_t;
          auto oit_t    = obs_index.find(key_t);
          auto oit_base = obs_index.find(key_base);
          if (oit_t != obs_index.end() && oit_base != obs_index.end())
            delta_ctrl.push_back(outcome[oit_t->second] -
                                  outcome[oit_base->second]);
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
