#' Callaway-Sant'Anna (2021) ATT(g,t) Estimator — Unconditional Version
#'
#' @description
#' Implements the unconditional Callaway and Sant'Anna (2021) group-time
#' average treatment effect estimator (equation 2.8) and aggregates the
#' resulting ATT(g,t) matrix to an event-study curve using the dynamic
#' weights from Table 1 (equation 3.4).
#'
#' For each (cohort g, calendar period t) pair, the estimator is a simple
#' two-by-two DiD:
#' \deqn{ATT(g, t) = \bigl[E[Y_t - Y_{g-1}|G=g]\bigr] -
#'                   \bigl[E[Y_t - Y_{g-1}|C_t]\bigr]}
#' where \eqn{G=g} denotes the cohort first treated at time \eqn{g},
#' \eqn{C_t} is the clean comparison group (never-treated or not-yet-treated
#' at \eqn{t}), and \eqn{g-1} is the cohort-specific base period
#' (shifted by \code{anticipation} when relevant).
#'
#' The event-study aggregate at relative time \eqn{\ell} is:
#' \deqn{\theta_{es}(\ell) = \sum_{g:\,g+\ell \in [t_{\min}, t_{\max}]}
#'   ATT(g,\,g+\ell)\;\cdot\;w_{es}(g,\ell)}
#' with cohort-size weights
#' \eqn{w_{es}(g,\ell) = n_g \big/ \sum_{g':\,g'+\ell \in \text{sample}} n_{g'}}.
#'
#' Standard errors use the delta-method variance:
#' \eqn{SE^2(ATT(g,t)) = s^2_g/n_g + s^2_C/n_C} (two-sample DiD),
#' and \eqn{SE^2(\theta_{es}(\ell)) = \sum_g w(g,\ell)^2 \cdot SE^2(ATT(g,g+\ell))}
#' (independence across cohorts).
#'
#' @param data data.frame with one row per unit-period (balanced panel).
#' @param outcome_chr Column name (character) for the outcome variable.
#' @param timing_chr Column name (character) for the first treatment period;
#'   \code{NA} marks never-treated units.
#' @param time_chr Column name (character) for calendar time (numeric).
#' @param unit_chr Column name (character) for the unit identifier.
#' @param anticipation Non-negative integer; anticipated treatment periods.
#'   Shifts the base period to \eqn{g - 1 - \text{anticipation}} (default 0).
#' @param control_group \code{"nevertreated"} (default) uses only
#'   never-treated units as controls; \code{"notyettreated"} also
#'   includes future adopters (units with \eqn{G_i > t}).
#' @param conf.level Numeric confidence level(s) for CIs (default 0.95).
#'
#' @return A list with components:
#' \describe{
#'   \item{\code{es}}{data.frame of event-study (dynamic) aggregates with
#'     columns \code{relative_time}, \code{estimate}, \code{std_error},
#'     \code{conf_low_XX}, \code{conf_high_XX}.}
#'   \item{\code{att_gt}}{data.frame of raw ATT(g,t) estimates with
#'     columns \code{g}, \code{t}, \code{relative_time}, \code{estimate},
#'     \code{std_error}.}
#'   \item{\code{cohorts}}{Sorted numeric vector of cohort values.}
#'   \item{\code{n_never}}{Number of never-treated units.}
#'   \item{\code{cohort_sizes}}{Named integer vector: cohort -> n_units.}
#'   \item{\code{control_group}}{The control group used (character).}
#' }
#' @noRd
.run_cs <- function(data,
                    outcome_chr,
                    timing_chr,
                    time_chr,
                    unit_chr,
                    anticipation  = 0L,
                    control_group = c("nevertreated", "notyettreated"),
                    conf.level    = 0.95) {

  control_group <- match.arg(control_group)
  anticipation  <- as.integer(anticipation)

  # ---- Validate inputs -------------------------------------------------------
  .validate_panel_cols(data, c(outcome_chr, timing_chr, time_chr, unit_chr), time_chr)
  if (anticipation < 0L)
    stop("`anticipation` must be a non-negative integer.")

  # ---- Bookkeeping -----------------------------------------------------------
  data        <- data[order(data[[unit_chr]], data[[time_chr]]), ]
  all_periods <- sort(unique(data[[time_chr]]))
  min_t       <- min(all_periods)
  max_t       <- max(all_periods)

  cohorts <- sort(unique(data[[timing_chr]][!is.na(data[[timing_chr]])]))
  if (length(cohorts) == 0L)
    stop("No treated units found: all values of '", timing_chr, "' are NA.")

  never_units <- unique(data[[unit_chr]][is.na(data[[timing_chr]])])
  n_never     <- length(never_units)

  # Cohort sizes: unique units per cohort
  cohort_sizes <- .compute_cohort_sizes(data, timing_chr, unit_chr, cohorts)

  # ---- ATT(g, t) — CS 2021, eq. 2.8 (unconditional) ------------------------
  # ATT(g, t) = E[Y_t - Y_{base_t} | G=g] - E[Y_t - Y_{base_t} | C_t]
  # base_t(g) = g - 1 - anticipation
  # SE² = s²(ΔY | G=g)/n_g + s²(ΔY | C_t)/n_C   (delta method)

  # ---- ATT(g,t) via Rcpp (compute_att_gt_cpp) --------------------------------
  # Encoding for the C++ kernel:
  #   * unit ids:    dense 1..n via match() — character, factor, and
  #     non-integer numeric identifiers would otherwise be destroyed by
  #     as.integer() (character -> NA collapses every unit into one).
  #   * time/timing: must be integer-valued; both are shifted by a common
  #     offset so every value is >= 1, freeing 0L as the never-treated
  #     sentinel (a real cohort at time 0 would otherwise be silently
  #     absorbed into the control group).
  .assert_integerish(data[[time_chr]], time_chr)
  .assert_integerish(data[[timing_chr]], timing_chr)

  # Base period g - 1 - anticipation assumes a consecutive integer time grid.
  base_periods <- cohorts - 1L - anticipation
  if (!any(base_periods %in% all_periods)) {
    spacing <- if (length(all_periods) > 1L) min(diff(all_periods)) else 1
    stop("No cohort has its base period (g - 1 - anticipation) in the sample. ",
         "The CS estimator assumes the time grid advances in steps of 1 ",
         "(observed minimum spacing: ", spacing, "). Rescale `time` and ",
         "`timing` to consecutive integers, e.g. ",
         "(time - min(time)) / spacing, and re-run.")
  }
  if (!all(base_periods %in% all_periods)) {
    skipped <- cohorts[!base_periods %in% all_periods]
    warning("Cohort(s) ", paste(skipped, collapse = ", "), " skipped: ",
            "base period (g - 1 - anticipation) not observed in the sample.",
            call. = FALSE)
  }

  unit_int <- match(data[[unit_chr]], unique(data[[unit_chr]]))

  min_val <- min(c(all_periods, cohorts))
  offset  <- if (min_val <= 0) 1L - as.integer(min_val) else 0L

  cohort_int <- as.integer(data[[timing_chr]]) + offset
  cohort_int[is.na(cohort_int)] <- 0L

  cpp_result <- compute_att_gt_cpp(
    unit_id       = unit_int,
    time_id       = as.integer(data[[time_chr]]) + offset,
    outcome       = as.numeric(data[[outcome_chr]]),
    cohort        = cohort_int,
    cohorts       = as.integer(cohorts) + offset,
    all_times     = as.integer(all_periods) + offset,
    control_group = control_group,
    anticipation  = as.integer(anticipation)
  )

  if (nrow(cpp_result) == 0L)
    stop("No ATT(g,t) estimates were computed. ",
         "Ensure the timing column, base periods, and comparison group ",
         "are consistent with the panel structure.")

  att_gt <- data.frame(
    g             = cpp_result$g - offset,
    t             = cpp_result$t - offset,
    relative_time = as.integer(cpp_result$t - cpp_result$g),
    estimate      = cpp_result$att,
    std_error     = cpp_result$se,
    stringsAsFactors = FALSE
  )
  att_gt          <- att_gt[order(att_gt$g, att_gt$t), ]
  rownames(att_gt) <- NULL

  # ---- Event-study aggregation — CS 2021, Table 1, eq. 3.4 -----------------
  # theta_es(l) = sum_{g: g+l in [min_t, max_t]} ATT(g, g+l) * w(g, l)
  # w(g, l)     = n_g / sum_{g': g'+l in [min_t, max_t]} n_{g'}
  #
  # Var(theta_es(l)) = sum_g w(g,l)^2 * Var(ATT(g, g+l))
  #   (cohorts are asymptotically independent under CS identification)

  event_times <- sort(unique(att_gt$relative_time))
  es_rows     <- list()
  key_gt      <- paste(att_gt$g, att_gt$t, sep = "\r")  # O(1) (g,t) row lookup

  for (l in event_times) {
    # Cohorts for which g+l falls within the sample
    in_sample <- (cohorts + l) >= min_t & (cohorts + l) <= max_t
    elig      <- cohorts[in_sample]

    # Restrict to cohorts for which ATT(g, g+l) was actually estimated
    idx  <- match(paste(elig, elig + l, sep = "\r"), key_gt)
    elig <- elig[!is.na(idx)]
    idx  <- idx[!is.na(idx)]

    if (length(elig) == 0L) next

    sz    <- cohort_sizes[as.character(elig)]
    total <- sum(sz)
    if (total == 0L) next
    w <- sz / total

    theta  <- sum(w * att_gt$estimate[idx])
    vtheta <- sum(w^2 * att_gt$std_error[idx]^2)

    es_rows[[length(es_rows) + 1L]] <- data.frame(
      relative_time = l,
      estimate      = theta,
      std_error     = sqrt(vtheta),
      stringsAsFactors = FALSE
    )
  }

  if (length(es_rows) == 0L)
    stop("Event-study aggregation produced no estimates.")

  es           <- do.call(rbind, es_rows)
  es           <- es[order(es$relative_time), ]
  rownames(es) <- NULL

  # ---- Confidence intervals --------------------------------------------------
  conf.level <- sort(unique(conf.level))
  es <- .add_ci_columns(es, conf.level, se_col = "std_error")

  list(
    es           = es,
    att_gt       = att_gt,
    cohorts      = cohorts,
    n_never      = n_never,
    cohort_sizes = cohort_sizes,
    control_group = control_group
  )
}
