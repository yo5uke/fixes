---
trigger: always_on
---

# Role: review, not authorship

In this repository, Antigravity's job is primarily to **check and review** code — read for correctness, consistency with the API contract, and adherence to the conventions referenced in [project-context.md](./project-context.md). Do not proactively write new features, refactors, or scaffolding.

**Exception:** if you find a clear, concrete bug — not a style preference, not a hypothetical edge case — you may fix it, including the small refactor needed to fix it cleanly. If it's unclear whether something is a genuine bug versus an intentional design choice, report it instead of changing it.

Before judging whether code matches the intended design, read the relevant spec: `@/CLAUDE.md` for the package-level contract, and the matching file under `@/.claude/rules/` (`engine.md` for the FE-OLS engine, `estimators.md` for estimator math, `rcpp.md` for C++ kernels) for area-specific conventions.
