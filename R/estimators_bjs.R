#' Borusyak, Jaravel, Spiess (2024) Imputation Estimator
#'
#' @description
#' Implements the 3-step imputation estimator from Borusyak, Jaravel, and Spiess
#' (2024) for staggered adoption designs (Theorem 2).
#'
#' \strong{Step 1} estimates unit and time fixed effects on the set of
#' "untreated" observations Ω₀ (never-treated units ∪ not-yet-treated
#' observations, i.e. \eqn{t < G_i} for eventually-treated units):
#' \deqn{Y_{it} = \alpha_i + \beta_t + \varepsilon_{it},\quad (i,t)\in\Omega_0}
#'
#' \strong{Step 2} imputes the counterfactual for each treated observation
#' (i,t) ∈ Ω₁ (where \eqn{G_i} is not \code{NA} and \eqn{t \ge G_i}):
#' \deqn{\hat{Y}_{it}(0) = \hat\alpha_i + \hat\beta_t,\qquad
#'       \hat\tau_{it} = Y_{it} - \hat{Y}_{it}(0)}
#'
#' \strong{Step 3} aggregates to an event-study curve at horizon \eqn{h}:
#' \deqn{\hat\tau_h = \frac{1}{|\Omega_{1,h}|}
#'   \sum_{(i,t):\,K_{it}=h} \hat\tau_{it}}
#'
#' Standard errors follow Theorem 3 / equations (7)--(8): treated observations
#' are demeaned within each cohort \eqn{\times} horizon cell (eq. 8), and the
#' unit-level scores form a cluster sandwich.  The FWL projection term for
#' untreated observations (which accounts for uncertainty in \eqn{\hat\alpha_i}
#' and \eqn{\hat\beta_t} from Step 1) is omitted, making the reported SE a
#' conservative approximation of the exact BJS SE.
#'
#' @param data data.frame with one row per unit-period (balanced panel).
#' @param outcome_chr Column name (character) for the outcome variable.
#' @param timing_chr Column name (character) for the first treatment period;
#'   \code{NA} marks never-treated units.
#' @param time_chr Column name (character) for calendar time (numeric).
#' @param unit_chr Column name (character) for the unit identifier.
#' @param clustervars Character vector of clustering variable name(s), or
#'   \code{NULL} to cluster by unit (default).
#' @param conf.level Numeric confidence level(s) for CIs (default 0.95).
#'
#' @return A list with:
#' \describe{
#'   \item{\code{es}}{data.frame: \code{relative_time}, \code{estimate},
#'     \code{std_error}, \code{conf_low_XX}/\code{conf_high_XX} per level.}
#'   \item{\code{tau_it}}{data.frame of unit-time treatment effects:
#'     \code{unit}, \code{time}, \code{cohort}, \code{relative_time},
#'     \code{tau_hat}.}
#'   \item{\code{n_never}}{Number of never-treated units.}
#'   \item{\code{cohorts}}{Sorted numeric vector of cohort values.}
#'   \item{\code{n_obs_omega0}}{Number of observations used in Step 1.}
#'   \item{\code{n_unimputed}}{Number of treated observations whose
#'     counterfactual could not be imputed (missing FE level) and were
#'     therefore excluded from aggregation.}
#'   \item{\code{n_treated_obs}}{Size of \eqn{\Omega_1} (treated observations)
#'     before the unimputable rows were dropped.}
#' }
#' @noRd
.run_bjs <- function(data,
                     outcome_chr,
                     timing_chr,
                     time_chr,
                     unit_chr,
                     clustervars = NULL,
                     conf.level  = 0.95) {

  # ---- Validate inputs -------------------------------------------------------
  .validate_panel_cols(data, c(outcome_chr, timing_chr, time_chr, unit_chr), time_chr)

  # ---- Bookkeeping -----------------------------------------------------------
  data <- data[order(data[[unit_chr]], data[[time_chr]]), ]

  cohorts <- sort(unique(data[[timing_chr]][!is.na(data[[timing_chr]])]))
  if (length(cohorts) == 0L)
    stop("No treated units found: all values of '", timing_chr, "' are NA.")

  never_units <- unique(data[[unit_chr]][is.na(data[[timing_chr]])])
  n_never     <- length(never_units)

  # ---- Partition into Ω₀ and Ω₁ (BJS 2024, Section 2) ----------------------
  # Ω₀ = never-treated units (all periods) + not-yet-treated observations
  #      (t < G_i for eventually-treated units i)
  # Ω₁ = treated observations: G_i not NA and t >= G_i
  is_treated_obs <- !is.na(data[[timing_chr]]) &
                    data[[time_chr]] >= data[[timing_chr]]
  omega0 <- data[!is_treated_obs, ]
  omega1 <- data[ is_treated_obs, ]

  if (nrow(omega0) == 0L)
    stop("No untreated observations (Omega_0) found. The BJS imputation ",
         "estimator requires never-treated or not-yet-treated units to identify ",
         "the counterfactual. Ensure `timing` is NA for never-treated units and ",
         "that eventually-treated units have at least one pre-treatment period.")

  if (nrow(omega1) == 0L)
    stop("No treated observations (Omega_1) found. Check that `timing` specifies ",
         "the first treatment period and that observations exist at or after it.")

  # ---- Step 1: Estimate FEs on Ω₀ (Theorem 2, Step 1) ----------------------
  # Y_it = alpha_i + beta_t + eps_it,  (i,t) in Omega_0
  step1_formula <- stats::as.formula(
    paste0(outcome_chr, " ~ 0 | ", unit_chr, " + ", time_chr)
  )
  step1_model <- tryCatch(
    fixest::feols(step1_formula, data = omega0, warn = FALSE, notes = FALSE),
    error = function(e)
      stop("BJS Step 1: FE regression on untreated observations failed: ",
           e$message)
  )

  # ---- Step 2: Impute Y_hat_it(0) and compute tau_hat_it (Theorem 2, Step 2)
  # Y_hat_it(0) = alpha_hat_i + beta_hat_t  via fixef() lookup
  # Using fixef() directly avoids predict()-level NA issues for FE models.
  fe_vals   <- fixest::fixef(step1_model)
  alpha_hat <- fe_vals[[unit_chr]]   # named: unit_id  -> unit FE
  beta_hat  <- fe_vals[[time_chr]]   # named: time_val -> time FE

  # Fixest's Gauss-Seidel demeaning cannot recover unit FEs for "singleton"
  # units (those with exactly 1 observation in omega0).  Their FE is
  # closed-form from Theorem 2: alpha_hat_i = Y_{i,t} - beta_hat_t.
  # We patch those entries before the imputation lookup below.
  omega0_u_chr <- as.character(omega0[[unit_chr]])
  omega0_t_chr <- as.character(omega0[[time_chr]])
  u_keys       <- as.character(omega1[[unit_chr]])
  t_keys       <- as.character(omega1[[time_chr]])

  singleton_u <- unique(u_keys[is.na(alpha_hat[u_keys])])
  if (length(singleton_u) > 0L) {
    sel <- omega0_u_chr %in% singleton_u
    if (any(sel)) {
      resid0 <- omega0[[outcome_chr]][sel] - beta_hat[omega0_t_chr[sel]]
      means  <- tapply(resid0, omega0_u_chr[sel], mean, na.rm = TRUE)
      alpha_hat[names(means)] <- means
    }
  }

  y_hat0 <- alpha_hat[u_keys] + beta_hat[t_keys]
  names(y_hat0) <- NULL

  n_na <- sum(is.na(y_hat0))
  if (n_na == nrow(omega1))
    stop("All imputed counterfactuals are NA. This usually means treated units ",
         "have no pre-treatment observations or some time periods are only ",
         "observed in the treated state. Check the panel structure.")
  if (n_na > 0L)
    warning(sprintf("%d treated observation(s) (%.1f%%) could not be imputed ",
                    n_na, 100 * n_na / nrow(omega1)),
            "due to missing FE levels; excluded from aggregation.")

  # Number of treated observations that could not be imputed (missing FE level),
  # retained for the result so callers can audit how much of Omega_1 was dropped.
  n_unimputed <- n_na
  n_omega1    <- nrow(omega1)

  omega1$.y_hat0  <- y_hat0
  omega1$.tau_hat <- omega1[[outcome_chr]] - omega1$.y_hat0
  omega1$.rel_t   <- as.integer(omega1[[time_chr]] - omega1[[timing_chr]])
  omega1          <- omega1[!is.na(omega1$.tau_hat), ]

  if (nrow(omega1) == 0L)
    stop("No valid treated observations remain after counterfactual imputation.")

  # ---- Step 3: Aggregate by event-study horizon (Theorem 2, Step 3) ---------
  # tau_hat_h = (1 / |Omega_{1,h}|) * sum_{(i,t): K_it = h} tau_hat_it
  horizons <- sort(unique(omega1$.rel_t))
  es_rows  <- list()

  for (h in horizons) {
    h_data <- omega1[omega1$.rel_t == h, ]
    n_h    <- nrow(h_data)
    if (n_h == 0L) next

    tau_h <- mean(h_data$.tau_hat)

    # ---- SE: BJS (2024) Theorem 3, eqs. (7)-(8) — conservative ------------
    # Partition treated obs at horizon h into cells by cohort g.
    # tau_tilde_{g,h} (eq. 8): cohort-horizon cell mean of tau_hat_it.
    # eps_tilde_it (treated): tau_hat_it - tau_tilde_{g(i),h}.
    # Unit-level score: s_i(h) = (1/n_h) * sum_{t: K_it=h} eps_tilde_it.
    # SE^2(tau_hat_h) = sum_i s_i(h)^2   [cluster-sandwich, HC0 type].
    #
    # Conservative approximation: the FWL projection term for untreated obs
    # (capturing uncertainty in alpha_hat_i and beta_hat_t from Step 1) is
    # omitted here.
    h_data$.tau_tilde <- stats::ave(h_data$.tau_hat, h_data[[timing_chr]], FUN = mean)
    h_data$.eps_tilde <- h_data$.tau_hat - h_data$.tau_tilde

    if (!is.null(clustervars) && length(clustervars) > 0L) {
      clust_id <- do.call(
        paste,
        c(lapply(clustervars, function(v) as.character(h_data[[v]])),
          list(sep = "__"))
      )
    } else {
      clust_id <- as.character(h_data[[unit_chr]])
    }

    unit_scores <- tapply(h_data$.eps_tilde, clust_id, sum) / n_h
    se_h <- sqrt(sum(unit_scores^2))

    es_rows[[length(es_rows) + 1L]] <- data.frame(
      relative_time = h,
      estimate      = tau_h,
      std_error     = se_h,
      stringsAsFactors = FALSE
    )
  }

  if (length(es_rows) == 0L)
    stop("BJS event-study aggregation produced no estimates.")

  es           <- do.call(rbind, es_rows)
  es           <- es[order(es$relative_time), ]
  rownames(es) <- NULL

  # ---- Confidence intervals -------------------------------------------------
  conf.level <- sort(unique(conf.level))
  es <- .add_ci_columns(es, conf.level, se_col = "std_error")

  # ---- tau_it table for downstream use --------------------------------------
  tau_it <- data.frame(
    unit          = omega1[[unit_chr]],
    time          = omega1[[time_chr]],
    cohort        = omega1[[timing_chr]],
    relative_time = omega1$.rel_t,
    tau_hat       = omega1$.tau_hat,
    stringsAsFactors = FALSE
  )

  list(
    es           = es,
    tau_it       = tau_it,
    n_never      = n_never,
    cohorts      = cohorts,
    n_obs_omega0 = nrow(omega0),
    n_unimputed  = n_unimputed,
    n_treated_obs = n_omega1
  )
}
