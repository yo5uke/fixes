# inst/profile/benchmark.R
#
# profvis-based profiling harness for fixes estimators.
# Purpose: identify the dominant bottleneck before any Phase 3 Rcpp work.
#
# Usage (run interactively in RStudio or at the console):
#   devtools::load_all()
#   source("inst/profile/benchmark.R")
#   # — or target a single estimator:
#   profile_estimator("sa")
#
# What to look for in the profvis flamegraph:
#   - fixest::feols() / demean() → HDFE solver is the bottleneck; this is the
#     Phase 3 target (custom alternating-projections solver).
#   - aggregate_iw_cpp / build_indicator_matrix_cpp → Rcpp paths; already
#     accelerated in Phase 2.  If they dominate, revisit the Rcpp code.
#   - R data wrangling (merge, rbind, tapply) → addressable without HDFE work.
#
# Output:
#   profvis HTML files are written to inst/profile/<estimator>_profile.html
#   wall-clock summary is printed to the console.

if (!requireNamespace("profvis", quietly = TRUE))
  stop("Install profvis first:  install.packages('profvis')")

# ---------------------------------------------------------------------------
# Synthetic large panel DGP
# ---------------------------------------------------------------------------
# N units × T periods staggered panel with K treatment cohorts
make_panel <- function(n_units  = 2000L,
                       n_periods = 20L,
                       n_cohorts =  5L,
                       seed      = 42L) {
  set.seed(seed)

  cohort_times <- seq(
    from = n_periods %/% 3,
    to   = n_periods - 2L,
    length.out = n_cohorts
  )
  cohort_times <- as.integer(round(cohort_times))

  group_sizes <- diff(c(0L, round(seq(0, n_units * 0.75, length.out = n_cohorts + 1L)[-1L])))
  n_never     <- n_units - sum(group_sizes)

  timing_unit <- c(
    rep(cohort_times, group_sizes),
    rep(NA_integer_,  n_never)
  )

  panel <- expand.grid(id = seq_len(n_units), year = seq_len(n_periods),
                       stringsAsFactors = FALSE)
  panel <- panel[order(panel$id, panel$year), ]
  panel$g     <- timing_unit[panel$id]
  panel$treat <- as.integer(!is.na(panel$g) & panel$year >= panel$g)

  unit_fe <- stats::rnorm(n_units, sd = 0.5)[panel$id]
  time_fe <- seq(0, 1, length.out = n_periods)[panel$year]
  panel$y <- unit_fe + time_fe + 0.5 * panel$treat + stats::rnorm(nrow(panel), sd = 0.3)

  # Group variable (needed by FLEX)
  panel$group <- timing_unit[panel$id]
  panel$group[is.na(panel$group)] <- 0L  # 0 = never-treated group

  panel
}

# ---------------------------------------------------------------------------
# Single-estimator profile
# ---------------------------------------------------------------------------
profile_estimator <- function(estimator = c("sa", "twm", "flex", "bjs", "cs"),
                               n_units  = 2000L,
                               n_periods = 20L,
                               n_cohorts =  5L,
                               out_dir   = "inst/profile") {
  estimator <- match.arg(estimator)

  message(sprintf("Profiling  estimator = '%s'  (%d units x %d periods) ...",
                  estimator, n_units, n_periods))

  panel <- make_panel(n_units = n_units, n_periods = n_periods,
                      n_cohorts = n_cohorts)

  pv <- profvis::profvis({
    if (estimator == "sa") {
      suppressWarnings(
        run_es(data = panel, outcome = y, time = year, timing = g,
               unit = id, fe = ~ id + year, staggered = TRUE,
               estimator = "sa")
      )
    } else if (estimator == "twm") {
      suppressWarnings(
        run_es(data = panel, outcome = y, time = year, timing = g,
               unit = id, fe = ~ id + year, staggered = TRUE,
               estimator = "twm")
      )
    } else if (estimator == "flex") {
      suppressWarnings(
        run_es(data = panel, outcome = y, time = year, timing = group,
               unit = id, group = group, staggered = TRUE,
               estimator = "flex")
      )
    } else if (estimator == "bjs") {
      suppressWarnings(
        run_es(data = panel, outcome = y, time = year, timing = g,
               unit = id, staggered = TRUE, estimator = "bjs")
      )
    } else if (estimator == "cs") {
      suppressWarnings(
        run_es(data = panel, outcome = y, time = year, timing = g,
               unit = id, staggered = TRUE, estimator = "cs")
      )
    }
  }, interval = 0.005)

  out_file <- file.path(out_dir, sprintf("%s_profile.html", estimator))
  htmlwidgets::saveWidget(pv, out_file, selfcontained = TRUE)
  message("  → wrote ", out_file)

  invisible(pv)
}

# ---------------------------------------------------------------------------
# Wall-clock summary across all estimators
# ---------------------------------------------------------------------------
benchmark_all <- function(n_units  = 2000L,
                           n_periods = 20L,
                           n_cohorts =  5L,
                           reps      = 3L) {
  panel <- make_panel(n_units = n_units, n_periods = n_periods,
                      n_cohorts = n_cohorts)

  estimators <- c("sa", "twm", "bjs", "cs")
  results    <- vector("list", length(estimators))

  for (i in seq_along(estimators)) {
    est <- estimators[[i]]
    message(sprintf("Timing  estimator = '%s'  (x%d reps) ...", est, reps))

    times <- numeric(reps)
    for (r in seq_len(reps)) {
      t0 <- proc.time()[["elapsed"]]
      suppressWarnings(tryCatch(
        switch(est,
          sa  = run_es(data = panel, outcome = y, time = year, timing = g,
                       unit = id, fe = ~ id + year, staggered = TRUE,
                       estimator = "sa"),
          twm = run_es(data = panel, outcome = y, time = year, timing = g,
                       unit = id, fe = ~ id + year, staggered = TRUE,
                       estimator = "twm"),
          bjs = run_es(data = panel, outcome = y, time = year, timing = g,
                       unit = id, staggered = TRUE, estimator = "bjs"),
          cs  = run_es(data = panel, outcome = y, time = year, timing = g,
                       unit = id, staggered = TRUE, estimator = "cs")
        ),
        error = function(e) NULL
      ))
      times[[r]] <- proc.time()[["elapsed"]] - t0
    }

    results[[i]] <- data.frame(
      estimator = est,
      mean_sec  = round(mean(times), 3),
      min_sec   = round(min(times),  3),
      max_sec   = round(max(times),  3)
    )
  }

  summary_df <- do.call(rbind, results)
  message("\n=== Wall-clock summary ===")
  print(summary_df, row.names = FALSE)
  message(sprintf("\nPanel: %d units x %d periods x %d cohorts",
                  n_units, n_periods, n_cohorts))
  message("Slowest estimator is the primary Phase 3 target.")

  invisible(summary_df)
}

# ---------------------------------------------------------------------------
# Entry point — run benchmark_all() if sourced directly
# ---------------------------------------------------------------------------
if (!interactive()) {
  devtools::load_all(quiet = TRUE)
  benchmark_all()
}
