#' Tidy an `es_result` object
#'
#' @description
#' Returns the event-study coefficient table of an [event_study()] result as
#' a plain tibble — the format expected by `modelsummary::modelsummary()`
#' and other broom-aware tooling. The baseline row (zero by construction)
#' is included; drop it with `x[!x$is_baseline, ]` beforehand if undesired.
#'
#' @param x An `es_result` object.
#' @param ... Unused; kept for compatibility with the [broom::tidy()] generic.
#'
#' @return A tibble with columns `term`, `estimate`, `std.error`,
#'   `statistic`, `p.value`, `relative_time`, `is_baseline`, and the
#'   `conf_low_XX`/`conf_high_XX` pairs of the originating call.
#'
#' @examples
#' \dontrun{
#' res <- event_study(df, outcome = y, time = year, timing = g,
#'                    unit = id, estimator = "cs")
#' broom::tidy(res)
#' }
#'
#' @exportS3Method broom::tidy es_result
tidy.es_result <- function(x, ...) {
  tibble::as_tibble(as.data.frame(x))
}

#' Glance at an `es_result` object
#'
#' @description
#' Returns a one-row summary of an [event_study()] result: sample sizes,
#' estimator, and VCOV type.
#'
#' @param x An `es_result` object.
#' @param ... Unused; kept for compatibility with the [broom::glance()]
#'   generic.
#'
#' @return A one-row tibble with columns `nobs`, `n_units`, `n_treated`,
#'   `n_nevertreated`, `estimator`, and `vcov_type`.
#'
#' @examples
#' \dontrun{
#' res <- event_study(df, outcome = y, time = year, timing = g,
#'                    unit = id, estimator = "cs")
#' broom::glance(res)
#' }
#'
#' @exportS3Method broom::glance es_result
glance.es_result <- function(x, ...) {
  est <- attr(x, "estimator")
  if (is.null(est) && isTRUE(attr(x, "sunab_used"))) est <- "twfe (sunab)"
  tibble::tibble(
    nobs           = as.integer(attr(x, "N")),
    n_units        = as.integer(attr(x, "N_units")),
    n_treated      = as.integer(attr(x, "N_treated")),
    n_nevertreated = as.integer(attr(x, "N_nevertreated")),
    estimator      = if (is.null(est)) NA_character_ else est,
    vcov_type      = as.character(attr(x, "vcov_type"))
  )
}
