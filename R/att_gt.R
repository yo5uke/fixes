#' Extract the ATT(g,t) table from a Callaway-Sant'Anna result
#'
#' @description
#' Returns the cohort-by-period ATT(g,t) table computed by
#' `event_study(estimator = "cs")` or `att(estimator = "cs")` as a real
#' `att_gt_result` object with its own [plot()][plot.att_gt_result] and
#' `autoplot()` methods. Bootstrap metadata (when the originating call used
#' `bootstrap = TRUE`) is carried along so simultaneous-inference overlays
#' keep working.
#'
#' @param x An `es_result` or `att_result` object estimated with
#'   `estimator = "cs"`.
#'
#' @return A data frame of class `c("att_gt_result", "data.frame")` with
#'   columns `g` (cohort), `t` (calendar time), `estimate`, and `std_error`.
#'
#' @examples
#' \dontrun{
#' res <- event_study(df, outcome = y, time = year, timing = g,
#'                    unit = id, estimator = "cs")
#' gt <- att_gt(res)
#' plot(gt)                  # heatmap
#' plot(gt, type = "facet")  # one panel per cohort
#' }
#'
#' @seealso [plot.att_gt_result()], [event_study()], [att()]
#' @export
att_gt <- function(x) {
  gt <- attr(x, "att_gt")
  if (is.null(gt)) {
    stop(
      "No ATT(g,t) table found on `x`. It is produced by ",
      "`event_study(estimator = \"cs\")` and `att(estimator = \"cs\")`."
    )
  }
  gt <- as.data.frame(gt)
  for (a in c("bootstrap", "boot_alpha")) {
    if (!is.null(attr(x, a))) attr(gt, a) <- attr(x, a)
  }
  class(gt) <- c("att_gt_result", "data.frame")
  gt
}
