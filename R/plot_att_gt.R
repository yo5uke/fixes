#' Plot the ATT(g,t) matrix from a Callaway-Sant'Anna event study
#'
#' @description
#' Visualises the full cohort-by-period ATT(g,t) matrix stored in the
#' \code{att_gt} attribute of an \code{es_result} object produced by
#' \code{run_es(estimator = "cs")}.  Two display styles are available:
#'
#' \itemize{
#'   \item \code{"heatmap"}: a tile plot with calendar time \eqn{t} on the
#'     x-axis and cohort \eqn{g} on the y-axis, colour-filled by the point
#'     estimate.  Cells whose pointwise confidence interval excludes zero are
#'     marked with a filled dot.
#'   \item \code{"facet"}: one panel per cohort showing ATT(g,t) over
#'     calendar time \eqn{t} with a pointwise confidence ribbon, mirroring
#'     the style of \code{\link{plot_es}}.
#' }
#'
#' Both types draw a vertical dashed line at \eqn{t = g} (treatment onset)
#' for each cohort.
#'
#' @section Note on CIs:
#' Confidence intervals are pointwise (Wald), not simultaneous.  Simultaneous
#' bands (Corollary 1 of Callaway and Sant'Anna 2021) will be added in a
#' later release alongside bootstrap SE support.
#'
#' @param x An \code{es_result} object returned by
#'   \code{run_es(estimator = "cs")}, or an \code{att_gt_result} data frame
#'   produced by extracting \code{attr(x, "att_gt")} and giving it class
#'   \code{"att_gt_result"}.
#' @param type \code{"heatmap"} (default) or \code{"facet"}.
#' @param ci_level Confidence level for pointwise intervals (default 0.95).
#' @param zero_line Logical; draw a horizontal reference line at zero in the
#'   \code{"facet"} display (default \code{TRUE}).
#' @param theme One of \code{"bw"} (default), \code{"minimal"}, or
#'   \code{"classic"}.
#' @param color Line and point colour used in the \code{"facet"} display
#'   (default \code{"#B25D91FF"}, matching \code{plot_es}).
#' @param fill Ribbon fill colour in the \code{"facet"} display (default
#'   \code{"#B25D91FF"}).
#' @param alpha Ribbon transparency in the \code{"facet"} display
#'   (default \code{0.2}).
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @seealso \code{\link{plot_es}}, \code{\link{run_es}}
#'
#' @examples
#' \dontrun{
#' cs_result <- run_es(data = mydata, outcome = y, time = year,
#'                     timing = g, unit = id, fe = ~id + year,
#'                     staggered = TRUE, estimator = "cs")
#' plot_att_gt(cs_result)
#' plot_att_gt(cs_result, type = "facet")
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_tile geom_point geom_vline geom_hline
#'   geom_ribbon geom_line scale_fill_gradient2 scale_x_continuous
#'   facet_wrap labs theme_bw theme_minimal theme_classic theme
#'   element_blank element_text
#' @importFrom rlang .data
#' @export
plot_att_gt <- function(x,
                        type     = c("heatmap", "facet"),
                        ci_level = 0.95,
                        zero_line = TRUE,
                        theme    = c("bw", "minimal", "classic"),
                        color    = "#B25D91FF",
                        fill     = "#B25D91FF",
                        alpha    = 0.2) {

  type  <- match.arg(type)
  theme <- match.arg(theme)

  # ---- Extract att_gt data frame -------------------------------------------
  if (inherits(x, "es_result")) {
    att <- attr(x, "att_gt")
    if (is.null(att))
      stop("`att_gt` attribute not found on this `es_result` object. ",
           "Re-run with `estimator = 'cs'` to produce ATT(g,t) estimates.")
  } else if (inherits(x, "att_gt_result") || is.data.frame(x)) {
    att <- x
  } else {
    stop("`x` must be an `es_result` from `run_es(estimator = 'cs')` or ",
         "an `att_gt_result` data frame.")
  }

  required <- c("g", "t", "estimate", "std_error")
  missing_cols <- setdiff(required, names(att))
  if (length(missing_cols))
    stop("The `att_gt` data frame is missing columns: ",
         paste(missing_cols, collapse = ", "))

  # ---- Pointwise CIs -------------------------------------------------------
  z <- stats::qnorm(1 - (1 - ci_level) / 2)
  att$conf_low   <- att$estimate - z * att$std_error
  att$conf_high  <- att$estimate + z * att$std_error
  att$significant <- att$conf_low > 0 | att$conf_high < 0

  cohorts <- sort(unique(att$g))
  onset   <- data.frame(g = cohorts)  # one row per cohort; t = g is onset

  # ---- Apply theme ---------------------------------------------------------
  apply_theme <- function(p) {
    if (theme == "bw") {
      p + ggplot2::theme_bw() +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
    } else if (theme == "minimal") {
      p + ggplot2::theme_minimal() +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
    } else {
      p + ggplot2::theme_classic()
    }
  }

  # ---- Heatmap -------------------------------------------------------------
  if (type == "heatmap") {
    att$g_label <- factor(att$g, levels = sort(unique(att$g)))

    lim <- max(abs(att$estimate), na.rm = TRUE)

    p <- ggplot2::ggplot(att,
           ggplot2::aes(x = .data$t, y = .data$g_label,
                        fill = .data$estimate)) +
      ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
      ggplot2::scale_fill_gradient2(
        low     = "#2166AC",
        mid     = "white",
        high    = "#D6604D",
        midpoint = 0,
        limits  = c(-lim, lim),
        name    = "ATT(g,t)"
      ) +
      # Treatment-onset lines: one per cohort, spanning all rows
      ggplot2::geom_vline(
        data        = onset,
        ggplot2::aes(xintercept = .data$g),
        linetype    = "dashed",
        colour      = "black",
        linewidth   = 0.5,
        alpha       = 0.7
      ) +
      # Significance dots in the centre of each cell
      ggplot2::geom_point(
        data  = att[att$significant, ],
        ggplot2::aes(x = .data$t, y = .data$g_label),
        size  = 1.5,
        shape = 16,
        colour = "black",
        inherit.aes = FALSE
      ) +
      ggplot2::scale_x_continuous(breaks = sort(unique(att$t))) +
      ggplot2::labs(
        x = "Calendar time (t)",
        y = "Cohort (g)",
        title = sprintf("ATT(g,t) \u2014 %.0f%% pointwise CIs; dots = significant",
                        ci_level * 100)
      ) +
      ggplot2::theme(
        axis.text.x  = ggplot2::element_text(angle = 45, hjust = 1),
        axis.text.y  = ggplot2::element_text(size  = 9)
      )

    return(apply_theme(p))
  }

  # ---- Facet ---------------------------------------------------------------
  # One panel per cohort; x = calendar time t, y = ATT estimate + CI ribbon.
  # Vertical dashed line at t = g (treatment onset) within each panel.
  att$g_label <- paste0("Cohort ", att$g)
  att$g_label <- factor(att$g_label,
                        levels = paste0("Cohort ", sort(unique(att$g))))

  onset$g_label <- paste0("Cohort ", onset$g)
  onset$g_label <- factor(onset$g_label,
                           levels = paste0("Cohort ", sort(unique(onset$g))))

  p <- ggplot2::ggplot(att,
         ggplot2::aes(x = .data$t, y = .data$estimate, group = 1)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$conf_low, ymax = .data$conf_high),
      fill  = fill,
      alpha = alpha
    ) +
    ggplot2::geom_line(linewidth = 0.8, colour = color) +
    ggplot2::geom_point(size = 1.8, colour = color) +
    # Per-panel treatment-onset lines (keyed by g_label to match facets)
    ggplot2::geom_vline(
      data        = onset,
      ggplot2::aes(xintercept = .data$g),
      linetype    = "dashed",
      colour      = "black",
      linewidth   = 0.5,
      alpha       = 0.7,
      inherit.aes = FALSE
    ) +
    ggplot2::facet_wrap(~ g_label, scales = "free_x") +
    ggplot2::scale_x_continuous(breaks = sort(unique(att$t))) +
    ggplot2::labs(
      x = "Calendar time (t)",
      y = sprintf("ATT(g,t) and %.0f%% pointwise CI", ci_level * 100)
    ) +
    ggplot2::theme(
      axis.text.x  = ggplot2::element_text(angle = 45, hjust = 1),
      strip.text   = ggplot2::element_text(face = "bold")
    )

  if (zero_line)
    p <- p + ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                                  colour = "grey40", linewidth = 0.4)

  apply_theme(p)
}


#' @rdname plot_att_gt
#' @param object An \code{att_gt_result} object (extracted from an
#'   \code{es_result} via \code{attr(result, "att_gt")}).
#' @param ... Passed to \code{\link{plot_att_gt}}.
#' @exportS3Method ggplot2::autoplot
autoplot.att_gt_result <- function(object, ...) {
  plot_att_gt(object, ...)
}
