# --------------------------------------------------------------------------- #
#  Sensitivity plot for honest_sensitivity() results                            #
# --------------------------------------------------------------------------- #

#' Plot a honest sensitivity analysis
#'
#' @description
#' Visualises the output of [honest_sensitivity()]: robust confidence intervals
#' for the target effect under progressively weaker parallel-trends restrictions
#' (increasing \eqn{M} or \eqn{\bar M}), alongside the original confidence
#' interval that assumes parallel trends holds exactly.  This is the "top-down"
#' sensitivity plot of Rambachan and Roth (2023).
#'
#' @param x A `honest_result` object from [honest_sensitivity()].
#' @param ... Unused.
#'
#' @return A `ggplot` object.
#' @seealso [honest_sensitivity()]
#' @importFrom ggplot2 ggplot aes geom_errorbar geom_point geom_hline labs theme_bw scale_color_manual
#' @importFrom rlang .data
#' @export
plot_honest <- function(x, ...) {
  if (!inherits(x, "honest_result"))
    stop("`x` must be a `honest_result` object from honest_sensitivity().")

  type <- attr(x, "type")
  xlab <- if (identical(type, "smoothness")) "M (smoothness bound)" else
    expression(bar(M)~"(relative magnitude bound)")

  df <- as.data.frame(x)
  orig <- df[df$method == "Original", , drop = FALSE]
  rob  <- df[df$method != "Original", , drop = FALSE]

  # Place the original (parallel-trends) CI one step to the left of the grid.
  step <- if (nrow(rob) > 1L) min(diff(sort(rob$M))) else 1
  orig_x <- min(rob$M) - step
  orig$M <- orig_x

  plot_df <- rbind(
    data.frame(M = rob$M,  lb = rob$lb,  ub = rob$ub,
               Restriction = "Robust (honest)", stringsAsFactors = FALSE),
    data.frame(M = orig$M, lb = orig$lb, ub = orig$ub,
               Restriction = "Original (PT)", stringsAsFactors = FALSE)
  )

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = .data$M, ymin = .data$lb, ymax = .data$ub,
                 color = .data$Restriction)
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        color = "grey50") +
    ggplot2::geom_errorbar(width = step * 0.3, linewidth = 0.7) +
    ggplot2::geom_point(ggplot2::aes(y = (.data$lb + .data$ub) / 2),
                        size = 1.8) +
    ggplot2::scale_color_manual(
      values = c("Original (PT)" = "#999999",
                 "Robust (honest)" = "#2c7fb8")) +
    ggplot2::labs(
      x = xlab, y = "Confidence set for treatment effect",
      color = NULL,
      title = "Honest sensitivity analysis (Rambachan & Roth 2023)") +
    ggplot2::theme_bw()
}

#' @rdname plot_honest
#' @param object A `honest_result` object.
#' @importFrom ggplot2 autoplot
#' @exportS3Method ggplot2::autoplot honest_result
autoplot.honest_result <- function(object, ...) {
  plot_honest(object, ...)
}
