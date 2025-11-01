# fixes 0.7.1 (2025-11-02)

## Bug Fixes
- **Lead/lag range filtering now enforced:**
  - Fixed bug where `lead_range` and `lag_range` parameters were ignored, causing all estimated coefficients to be returned regardless of specified ranges
  - Results now correctly filtered to only include coefficients within `[-lead_range, lag_range]`
  - Applies to both `classic` and `sunab` methods

- **sunab() namespace error resolved:**
  - Fixed `'sunab' is not an exported object from 'namespace:fixest'` error when using `method = "sunab"`
  - Formula environment now correctly configured to access `sunab` function from fixest package
  - sunab() calls now work identically to direct fixest::feols() usage

## Enhancements
- **Baseline row now included in sunab results:**
  - The `baseline` parameter (default: -1) now applies to both `classic` and `sunab` methods
  - Baseline period added to results with `estimate = 0`, `std.error = 0`, and `is_baseline = TRUE`
  - Provides consistent behavior across both estimation methods
  - Updated documentation to clarify that `baseline` is used for both methods

- **Improved term column formatting:**
  - Term values now display as clean numeric strings (e.g., `"-9"`, `"0"`, `"3"`)
  - Removed complex fixest-specific notation (e.g., `"year::-9"`) for better readability
  - Consistent formatting across both classic and sunab methods

- **Modernized code with native pipe operator:**
  - Replaced tidyverse pipe (`%>%`) with base R pipe (`|>`) throughout the package
  - Requires R >= 4.1.0 (already enforced in DESCRIPTION)
  - Cleaner code with no functional changes

## Testing
- **Comprehensive test suite overhaul:**
  - Added 25+ test cases covering all bug fixes and enhancements
  - Separate test suites for classic, staggered, and sunab methods
  - Tests for baseline row inclusion, term formatting, and range filtering
  - Integration tests with fixest built-in datasets (base_did, base_stagg)
  - Tests use native pipe operator (`|>`)

## Compatibility
- Fully backward compatible with previous versions
- All changes maintain existing API and functionality
- Enhanced robustness and user experience

---

# fixes 0.7.0 (2025-11-01)

## Bug Fixes
- **Critical fix for relative_time calculation in non-staggered designs:**
  - Fixed bug where `relative_time` was NA for all coefficients except baseline when using non-staggered treatment timing
  - The reference period in `fixest::i()` now correctly corresponds to the `baseline` parameter
  - For example, with `timing = 5` and `baseline = -1`, period 4 (not period 5) is now used as the reference
  - This ensures proper event study estimation with the intended baseline period

## New Features
- **Interactive plotting with plotly:**
  - New function `plot_es_interactive()` for creating interactive event study plots
  - Hover tooltips display: relative time, point estimate, confidence intervals, standard error, and p-value
  - Supports customizable confidence levels, colors, and styling options
  - Optional ribbon or point-only display modes
  - Fully compatible with `es_result` objects from `run_es()`

## Performance Improvements
- **Optimized relative_time extraction:**
  - Vectorized term parsing replaces previous `sapply()` approach for significant performance gains
  - Faster processing of large event study results
- **Enhanced error reporting:**
  - More informative warnings when term parsing fails
  - Better debugging information for malformed coefficient names

## Compatibility
- Fully backward compatible with previous versions
- Requires R >= 4.1.0
- Interactive plotting requires `plotly` package (optional, suggested dependency)

---

# fixes 0.5.0 (2025-07-06)

## New Features
- **Multiple Confidence Intervals:**  
  `run_es()` and `plot_es()` now support returning and visualizing multiple confidence levels (e.g., 90%, 95%, 99%) in a single analysis.
- **Enhanced Plotting Options:**  
  `plot_es()` adds `ci_level` selection, more theme options (`theme_style`), and improved ribbon/error bar display.
- **Expanded Error Handling:**  
  More informative error reporting for all argument validation and model mis-specification.
- **Documentation Overhaul:**  
  Updated README, vignette, and roxygen docs to reflect all new features and usage patterns.

## Improvements
- **Input Validation:**  
  Stricter checks for all arguments and clearer, friendlier error messages.
- **Staggered Timing:**  
  More robust handling of partial or missing treatment timing.
- **Code Refactoring:**  
  Internal codebase cleaned for maintainability and reliability.

## Bug Fixes
- Fixed: Confidence intervals always correctly labeled and included in output.
- Fixed: Dummy variables not created for untreated/never-treated units in staggered settings.
- Fixed: CRAN warnings by declaring all global variables used in tidy evaluation.

## Compatibility
- Fully backward compatible with previous versions.
- Minimum required R version remains 4.1.0.

# fixes 0.4.1 (2025-06-30)

## New Features
- **Support for untreated units (`NA` in timing):**
  - Units with missing `timing` are now retained as never-treated controls (staggered only).
- **Flexible `weights` input:**
  - Accepts formulas (`~ popwt`), bare names (`popwt`), or character strings (`"popwt"`).
- **Improved `cluster` input handling:**
  - Now supports formula, character vector, and bare names consistently.

## Improvements
- Better validation and error messages across all inputs.
- Warning added if `unit` is supplied without `time_transform = TRUE`.
- Documentation switched to Markdown-style lists for better readability.

## Bug Fixes
- Ensured untreated units don’t receive event dummies.
- Improved internal dummy creation and baseline handling.

## Compatibility
- Fully backward compatible with previous versions.

# fixes 0.4.0 (May 25, 2025)

## New Features
- Added support for **staggered treatment timing**:
  - New `staggered = TRUE` option allows `timing` to vary by unit (e.g., treatment year column).
  - Units with `NA` in `timing` are safely retained as untreated.

- Added support for **observation weights**:
  - Use the `weights` argument (e.g., `~ popwt`) to run weighted regressions.

- **Automatic lead/lag range detection**:
  - If `lead_range` or `lag_range` is `NULL`, the function computes the maximum feasible range from the data.

## Improvements
- Safer handling of collinearity and untreated units.
- Warning is issued when `unit` is specified without `time_transform = TRUE`.
- Input validation and internal structure cleaned up for better robustness.

## Compatibility
- Backward compatible with previous versions.


# fixes 0.3.1 (May 18, 2025)

## Improvements
- Improved default x-axis scaling in `plot_es()`:
  - The x-axis now uses `ggplot2::scale_x_continuous()` with integer breaks spaced by 1, aligned to the `relative_time` range.
  - This provides more readable and consistent event time labeling in plots.

# fixes 0.3.0 (May 18, 2025)

## New Features
- Added support for irregular and non-numeric time variables such as `Date`:
  - Introduced `time_transform = TRUE` to automatically convert the `time` variable into a unit-level sequential index (1, 2, 3, ...) for event study estimation.
  - Added `unit` argument to specify the panel unit identifier required when `time_transform = TRUE`.
  - This enables seamless analysis of panel data with monthly, quarterly, or irregular time formats.

## Improvements
- Updated input validation:
  - Now accepts `Date` class in the `time` variable and converts it automatically to numeric if `time_transform = FALSE`.
  - Improved error messages and warnings for better user feedback when `unit` is missing or `time` is of unsupported type.
- Enhanced documentation:
  - Updated `@examples` in the function documentation to include `Date`-based examples.
  - Extended vignette to illustrate `time_transform` usage.
  - Expanded `README.md` to describe irregular time handling and demonstrate new use cases.
- Strengthened test coverage:
  - Added new unit tests for `time_transform`, `unit` handling, and `Date` conversion edge cases.

## Compatibility
- No breaking changes.
- This is a backward-compatible **minor release** introducing significant new functionality for irregular time formats.

---

# fixes 0.2.1 (May 11, 2025)

## Minor Improvements
- Added a warning when lead/lag dummy variable names (e.g., `lead1`, `lag0`) already exist in the dataset to prevent accidental overwriting.
- Added a warning when filtered data (based on `lead_range`, `lag_range`, and `interval`) has fewer than 10 rows, helping users identify overly narrow estimation windows.
- Improved handling of the `treatment` variable: it is now coerced to logical using `as.logical()` to support both binary numeric (`0/1`) and logical (`TRUE/FALSE`) formats.
- Fixed internal bug in model formula construction:
  - Previously, fixed effects specified via the `fe` argument (e.g., `~ id + year`) were combined using `model_formula | fe_text`, which caused evaluation errors during tests.
  - Now, the full model formula is safely constructed as a string and parsed with `as.formula()` to ensure compatibility with `fixest::feols()`.

## Compatibility
- No breaking changes. This is a backward-compatible patch release with internal robustness improvements and enhanced error handling.

# fixes 0.2.0 (March 29, 2025)

## Major Features
- **Support for covariates in `run_es()`**:
  - Covariates must now be specified as a **one-sided formula** (e.g., `~ x1 + x2`).
- **Fixed Effects and Clustering Interface Updated**:
  - `fe` and `cluster` arguments must now be specified using a **one-sided formula** (e.g., `~ id + year`).
  - Character vector input for `cluster` is still accepted.
  - Improved internal handling and validation of fixed effects and clustering variables.
- Improved error messages for invalid or missing variable names.

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
