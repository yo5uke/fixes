---
paths:
  - "src/**/*.cpp"
  - "src/**/*.h"
---

# Rcpp conventions

- **Profile first** (`profvis`/`Rprof`) — do not optimize speculatively. The v1.0.0 classic-path speedup came from R-level bookkeeping (integer group codes instead of `paste0`/`table`/`factor`), not new C++.
- Use `RcppArmadillo` for matrix operations; pure `Rcpp` for simple integer/logical fills.
- Tag exports with `// [[Rcpp::export]]`; run `Rcpp::compileAttributes()` after adding any new export.
- Every Rcpp function needs a test verifying numerical agreement with the equivalent R implementation: tolerance `1e-12` for identical float inputs, `expect_identical` for integer outputs.
- `src/Makevars` / `src/Makevars.win` link `$(BLAS_LIBS) $(LAPACK_LIBS) $(FLIBS)` and `$(SHLIB_OPENMP_CXXFLAGS)`.
- Thread count: `options(fixes.nthreads=)` wins; `_R_CHECK_LIMIT_CORES_` set → 2; else `detectCores()/2` (`.fes_nthreads()`). OpenMP kernels must stay bit-identical across thread counts (fixed per-entry accumulation order).

## Kernel inventory

| File | Function | Used by |
|------|----------|---------|
| `fe_ols_kernels.cpp` | `chol_seq_drop_cpp`, `crossprod_omp_cpp` | engine |
| `demean_kway.cpp` | `demean_kway_cpp` (k-way, weighted) | engine |
| `fe_solver.cpp` | `solve_fe_2way_cpp` | BJS |
| `compute_att_gt.cpp` | `compute_att_gt_cpp` | CS |
| `indicator_matrix.cpp` | `build_indicator_matrix_cpp` | SA, TWM, FLEX |
| `iw_aggregation.cpp` | `aggregate_iw_cpp` | SA, TWM, FLEX |
| `cov_demeaning.cpp` | `build_cov_interactions_cpp` | TWM, FLEX |
| `bootstrap_cs.cpp` | `bootstrap_cs_cpp` | CS bootstrap |
