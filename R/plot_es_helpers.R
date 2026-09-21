# Shared plumbing for the static and interactive event-study plots.

# Guard the simultaneous-CI overlay: those columns only exist when the result
# was produced with bootstrap = TRUE.
.es_check_simultaneous <- function(data, show_simultaneous) {
  if (!isTRUE(show_simultaneous)) {
    return(invisible(NULL))
  }
  if (!all(c("conf_low_sim", "conf_high_sim") %in% names(data))) {
    stop(
      "Simultaneous CIs not found. Re-run with bootstrap = TRUE in ",
      "event_study().",
      call. = FALSE
    )
  }
  invisible(NULL)
}

# Pointwise CI columns for `ci_level`. Errors (rather than silently drawing a
# different level) when the result was not estimated at that level.
.es_ci_cols <- function(data, ci_level) {
  if (!is.numeric(ci_level) || length(ci_level) != 1L ||
      is.na(ci_level) || ci_level <= 0 || ci_level >= 1) {
    stop("`ci_level` must be a single number strictly between 0 and 1.",
         call. = FALSE)
  }
  ci_str <- sprintf("%.0f", ci_level * 100)
  low <- paste0("conf_low_", ci_str)
  high <- paste0("conf_high_", ci_str)
  if (!all(c(low, high) %in% names(data))) {
    available <- sub("^conf_low_", "", grep("^conf_low_[0-9]+$", names(data),
                                            value = TRUE))
    stop(
      "No ", ci_str, "% confidence interval in this result. ",
      "Available level(s): ",
      paste0(available, "%", collapse = ", "),
      ". Re-run event_study() with `conf_level = ", ci_level, "`.",
      call. = FALSE
    )
  }
  list(low = low, high = high, pct = ci_str)
}

# Label for the bootstrap CI level used by the simultaneous band.
.es_sim_pct <- function(data) {
  boot_alpha <- attr(data, "boot_alpha")
  sprintf("%.0f%%", (1 - if (is.null(boot_alpha)) 0.05 else boot_alpha) * 100)
}

# Build the x axis. "relative" keeps event time; "calendar" maps it back onto
# the original time values, which requires a single treatment period.
.es_x_axis <- function(data, time_axis = c("relative", "calendar")) {
  time_axis <- match.arg(time_axis)
  rel <- data$relative_time

  if (time_axis == "relative") {
    return(list(
      values = rel,
      zero = 0,
      breaks = sort(unique(rel)),
      label = "Relative Time to Treatment",
      is_date = FALSE
    ))
  }

  ref <- attr(data, "ref_time")
  if (is.null(ref)) {
    reason <- if (isTRUE(attr(data, "staggered"))) {
      paste0("this result comes from a staggered design, where each cohort is ",
             "treated in a different period, so one relative period maps to ",
             "several calendar periods")
    } else {
      paste0("this result carries no calendar reference (event time was ",
             "supplied directly via `rel_time`)")
    }
    stop('`time_axis = "calendar"` is not available: ', reason,
         '. Use `time_axis = "relative"`.', call. = FALSE)
  }

  interval <- attr(data, "interval")
  if (is.null(interval)) interval <- 1
  values <- ref + rel * interval

  # On a numeric time column this arithmetic inverts exactly how the estimator
  # defined event time. On dates it does not: `interval` is a fixed number of
  # days, and a calendar year is not. Snap those back onto the periods the
  # data actually holds, as long as that stays one-to-one.
  obs_levels <- attr(data, "time_levels")
  if (inherits(values, "Date") && inherits(obs_levels, "Date") &&
      length(obs_levels) > 0L) {
    idx <- vapply(
      as.numeric(values),
      function(v) which.min(abs(as.numeric(obs_levels) - v)),
      integer(1L)
    )
    if (!anyDuplicated(idx)) values <- obs_levels[idx]
  }

  label <- attr(data, "time_var")
  if (is.null(label) || !nzchar(label)) label <- "Time"
  if (isTRUE(attr(data, "time_transform"))) {
    label <- paste0(label, " (index)")
  }

  list(
    values = values,
    zero = ref,
    breaks = sort(unique(values)),
    label = label,
    is_date = inherits(values, "Date")
  )
}
