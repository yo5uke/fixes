---
paths:
  - "R/estimators_*.R"
  - "R/event_study.R"
  - "R/att.R"
---

# Estimator implementation rules

Always run `/read papers/<file>.pdf` before implementing or changing an estimator's math — formulas for weights, aggregation schemes, and identification conditions must follow the paper exactly, not memory.

| Estimator | `estimator =` | Reference paper | Reference implementation |
|-----------|----------------|------------------|---------------------------|
| Classic TWFE | `"twfe"` | — | `fixest::feols` (oracle tests) |
| Sun-Abraham | `"sa"` | papers/Sun and Abraham (2021).pdf | `fixest::sunab()` |
| Callaway-Sant'Anna | `"cs"` | papers/Callaway and Sant'Anna (2021).pdf | `did::att_gt()` |
| Borusyak et al. | `"bjs"` | papers/Borusyak et al. (2024).pdf | `didimputation` |
| Wooldridge TWM | `"twm"` | papers/Woodridge (2025).pdf | — |
| Deb et al. FLEX (RCS) | `"flex"` | papers/Deb et al. (2024).pdf | — |

Roth honest sensitivity (`honest_sensitivity()`) follows papers/Rambachan and Roth (2023).pdf (ARP-conditional only; FLCI/hybrid not implemented), reference implementation `HonestDiD`.

Synthetic DiD (papers/Arkhangelsky et al. (2021).pdf, reference `synthdid`) is not yet implemented.

## TWM `trends = TRUE` collinearity

With unit FE, `d_g * t` is algebraically collinear with treatment-cell indicators + unit FE. Fix (Wooldridge 2025, Section 8): include only post-treatment cells (`s >= g`) and add trend columns — pre-treatment data then identifies the trend. `trends = TRUE` output therefore only shows `relative_time >= 0` (no pre-trend test).
