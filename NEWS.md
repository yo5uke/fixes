# fixes 0.2.0 (March 26, 2025)

## Major Features
- **Support for covariates in `run_es()`**:
  - Now accepts covariates specified as either an **additive expression** (e.g., `x1 + x2`) or a **character vector** (e.g., `c("x1", "x2")`).
  - Automatically parses and validates covariate inputs.
- Updated documentation and examples to reflect covariate support.
- Improved error handling for covariates (e.g., missing column names).

---

# fixes 0.1.0 (March 17, 2025)

## Major Changes
- `fe_var` argument now supports additive notation (`firm_id + year`) instead of character vectors.
- Improved `plot_es()` efficiency and documentation.

## Minor Improvements
- Fixed cluster variable handling to correctly reference column names.
- Updated test cases to support new `fe` notation.
- Improved package documentation.

# fixes 0.0.2 (Enhancements & Fixes)

This version introduced several enhancements and refinements to improve usability and maintainability.

## Improvements
- Refactored variable name handling:
  - `outcome_var`, `treated_var`, and `time_var` are now processed using `rlang::ensym()` for better robustness.
  - `fe_var` and `cluster_var` handling improved for more reliable column referencing.
- More informative **error messages** when variables are missing in the dataset.
- Enhanced **baseline term handling** in regression models to prevent incorrect factor levels.
- Improved `plot_es()` function:
  - Added validation checks to ensure required columns (`relative_time`, `estimate`, etc.) are present.
  - Adjusted confidence interval calculations to avoid missing values in error bars.

## Fixes
- Addressed an issue where `baseline` handling could lead to incorrect sorting of lead/lag terms.
- Resolved a minor inconsistency in fixed effects variable name parsing.

# fixes 0.0.1 (Initial Release)

This is the first release of the `fixes` package, providing tools for estimating and visualizing event study models with fixed effects.

## Features
- **`run_es()`**: A function to estimate event study models using `fixest::feols()`, generating lead and lag variables automatically.
  - Supports **fixed effects** (`fe_var` as character vector).
  - Allows **clustered standard errors** via `cluster_var`.
  - Handles **time scaling** through the `interval` argument.
- **`plot_es()`**: A function to visualize event study results with ggplot2.
  - Supports **ribbon-style confidence intervals** (`type = "ribbon"`, default).
  - Allows **error bar visualization** (`type = "errorbar"`).
  - Customizable plot elements including colors, line styles, and reference lines.

## Initial Implementation
- **Fixed effects regression model** using `fixest::feols()`.
- **Automated creation of lead/lag dummy variables** based on treatment timing.
- **Baseline period exclusion** to avoid multicollinearity.
- **Support for custom time intervals (`interval` argument)**.

## Limitations in 0.0.1
- Fixed effects must be specified as a **character vector (`c("firm_id", "year")`)**.
- Clustered standard errors require variable names as **character strings (`"state_id"`)**.
- No direct support for additive notation in `fe_var`.
