#' Plot the ATT(g,t) matrix from a Callaway-Sant'Anna event study
#'
#' @description
#' Visualises the full cohort-by-period ATT(g,t) matrix stored in the
#' `att_gt` attribute of an `es_result` object produced by
#' `run_es(estimator = "cs")`.  Two display styles are available:
#'
#' - `"heatmap"`: a tile plot with calendar time \eqn{t} on the
#'   x-axis and cohort \eqn{g} on the y-axis, colour-filled by the point
#'   estimate.  Cells whose pointwise confidence interval excludes zero are
#'   marked with a filled dot; cells that are simultaneously significant
#'   (when bootstrap data are available) are additionally marked with an open
#'   diamond.
#' - `"facet"`: one panel per cohort showing ATT(g,t) over
#'   calendar time \eqn{t} with a pointwise confidence ribbon, mirroring
#'   the style of [plot_es()].  A lighter simultaneous CI ribbon
#'   is overlaid when bootstrap data are available.
#'
#' Both types draw a vertical dashed line at \eqn{t = g} (treatment onset)
#' for each cohort.
#'
#' @section Bootstrap annotation:
#' When `attr(x, "bootstrap")` is present (i.e., [run_es()] was
#' called with `bootstrap = TRUE`), both plot types add simultaneous
#' inference overlays sourced from the (g,t)-level bootstrap object.
#'
#' @param x An `es_result` object returned by
#'   `run_es(estimator = "cs")`, or an `att_gt_result` data frame
#'   produced by extracting `attr(x, "att_gt")` and giving it class
#'   `"att_gt_result"`.
#' @param type `"heatmap"` (default) or `"facet"`.
#' @param ci_level Confidence level for pointwise intervals (default 0.95).
#' @param zero_line Logical; draw a horizontal reference line at zero in the
#'   `"facet"` display (default `TRUE`).
#' @param theme One of `"bw"` (default), `"minimal"`, or
#'   `"classic"`.
#' @param color Line and point colour used in the `"facet"` display
#'   (default `"#B25D91FF"`, matching [plot_es()]).
#' @param fill Ribbon fill colour in the `"facet"` display (default
#'   `"#B25D91FF"`).
#' @param alpha Ribbon transparency in the `"facet"` display
#'   (default `0.2`).
#'
#' @return A [ggplot2::ggplot()] object.
#'
#' @seealso [plot_es()], [run_es()]
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
#' @importFrom rlang .data
#' @export
plot_att_gt <- function(
  x,
  type = c("heatmap", "facet"),
  ci_level = 0.95,
  zero_line = TRUE,
  theme = c("bw", "minimal", "classic"),
  color = "#B25D91FF",
  fill = "#B25D91FF",
  alpha = 0.2
) {
  type <- match.arg(type)
  theme <- match.arg(theme)

  # ---- Extract bootstrap data (available only when x is es_result) ----------
  boot_gt <- attr(x, "bootstrap")
  boot_alpha <- attr(x, "boot_alpha")
  has_boot <- !is.null(boot_gt) &&
    all(c("g", "t", "conf_low_sim", "conf_high_sim") %in% names(boot_gt))

  # ---- Extract att_gt data frame --------------------------------------------
  if (inherits(x, "es_result")) {
    att <- attr(x, "att_gt")
    if (is.null(att)) {
      stop(
        "`att_gt` attribute not found on this `es_result` object. ",
        "Re-run with `estimator = 'cs'` to produce ATT(g,t) estimates."
      )
    }
  } else if (inherits(x, "att_gt_result") || is.data.frame(x)) {
    att <- x
  } else {
    stop(
      "`x` must be an `es_result` from `run_es(estimator = 'cs')` or ",
      "an `att_gt_result` data frame."
    )
  }

  required <- c("g", "t", "estimate", "std_error")
  missing_cols <- setdiff(required, names(att))
  if (length(missing_cols)) {
    stop(
      "The `att_gt` data frame is missing columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # ---- Merge simultaneous CI columns if bootstrap is available ---------------
  if (has_boot) {
    boot_sim <- boot_gt[, c("g", "t", "conf_low_sim", "conf_high_sim")]
    att <- merge(att, boot_sim, by = c("g", "t"), all.x = TRUE, sort = FALSE)
  }

  # ---- Pointwise CIs -------------------------------------------------------
  z <- stats::qnorm(1 - (1 - ci_level) / 2)
  att$conf_low <- att$estimate - z * att$std_error
  att$conf_high <- att$estimate + z * att$std_error
  att$significant <- att$conf_low > 0 | att$conf_high < 0

  cohorts <- sort(unique(att$g))
  onset <- data.frame(g = cohorts) # one row per cohort; t = g is onset

  # ---- Apply theme ---------------------------------------------------------
  apply_theme <- function(p) {
    if (theme == "bw") {
      p +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
    } else if (theme == "minimal") {
      p +
        ggplot2::theme_minimal() +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
    } else {
      p + ggplot2::theme_classic()
    }
  }

  # ---- Heatmap -------------------------------------------------------------
  if (type == "heatmap") {
    att$g_label <- factor(att$g, levels = sort(unique(att$g)))

    lim <- max(abs(att$estimate), na.rm = TRUE)

    # Build subtitle and caption when bootstrap data are present
    heatmap_subtitle <- NULL
    heatmap_caption <- NULL

    if (has_boot) {
      c_hat <- round(unique(boot_gt$critical_value)[1L], 2)
      heatmap_subtitle <- sprintf(
        "Dots = pointwise significant at 5%%; simultaneous critical value = %.2f",
        c_hat
      )
      # \u25cf = filled circle, \u25c7 = open diamond
      heatmap_caption <- paste0(
        "\u25cf pointwise sig.   \u25c7 simultaneous sig."
      )
    }

    p <- ggplot2::ggplot(
      att,
      ggplot2::aes(x = .data$t, y = .data$g_label, fill = .data$estimate)
    ) +
      ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
      ggplot2::scale_fill_gradient2(
        low = "#2166AC",
        mid = "white",
        high = "#D6604D",
        midpoint = 0,
        limits = c(-lim, lim),
        name = "ATT(g,t)"
      ) +
      # Treatment-onset lines: one per cohort, spanning all rows
      ggplot2::geom_vline(
        data = onset,
        ggplot2::aes(xintercept = .data$g),
        linetype = "dashed",
        colour = "black",
        linewidth = 0.5,
        alpha = 0.7
      ) +
      # Pointwise significance dots (filled circle)
      ggplot2::geom_point(
        data = att[att$significant, ],
        ggplot2::aes(x = .data$t, y = .data$g_label),
        size = 1.5,
        shape = 16,
        colour = "black",
        inherit.aes = FALSE
      ) +
      ggplot2::scale_x_continuous(breaks = sort(unique(att$t))) +
      ggplot2::labs(
        title = sprintf(
          "ATT(g,t) \u2014 %.0f%% pointwise CIs; dots = significant",
          ci_level * 100
        ),
        subtitle = heatmap_subtitle,
        caption = heatmap_caption,
        x = "Calendar time (t)",
        y = "Cohort (g)"
      ) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        axis.text.y = ggplot2::element_text(size = 9)
      )

    # Add simultaneously significant cells (open diamond) when bootstrap present
    if (has_boot) {
      sim_sig <- att[
        !is.na(att$conf_low_sim) &
          (att$conf_low_sim > 0 | att$conf_high_sim < 0),
      ]
      if (nrow(sim_sig) > 0) {
        p <- p +
          ggplot2::geom_point(
            data = sim_sig,
            ggplot2::aes(x = .data$t, y = .data$g_label),
            size = 2.5,
            shape = 5, # open diamond
            colour = "black",
            stroke = 0.8,
            inherit.aes = FALSE
          )
      }
    }

    return(apply_theme(p))
  }

  # ---- Facet ---------------------------------------------------------------
  # One panel per cohort; x = calendar time t, y = ATT estimate + CI ribbon.
  # Vertical dashed line at t = g (treatment onset) within each panel.
  att$g_label <- paste0("Cohort ", att$g)
  att$g_label <- factor(
    att$g_label,
    levels = paste0("Cohort ", sort(unique(att$g)))
  )

  onset$g_label <- paste0("Cohort ", onset$g)
  onset$g_label <- factor(
    onset$g_label,
    levels = paste0("Cohort ", sort(unique(onset$g)))
  )

  # Add label column for the simultaneous CI legend entry when bootstrap present
  if (has_boot) {
    sim_ci_pct <- if (!is.null(boot_alpha)) {
      sprintf("%.0f%% simultaneous CI", (1 - boot_alpha) * 100)
    } else {
      "Simultaneous CI"
    }
    att$.sim_label <- sim_ci_pct
  }

  p <- ggplot2::ggplot(
    att,
    ggplot2::aes(x = .data$t, y = .data$estimate, group = 1)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$conf_low, ymax = .data$conf_high),
      fill = fill,
      alpha = alpha
    ) +
    ggplot2::geom_line(linewidth = 0.8, colour = color) +
    ggplot2::geom_point(size = 1.8, colour = color) +
    # Per-panel treatment-onset lines (keyed by g_label to match facets)
    ggplot2::geom_vline(
      data = onset,
      ggplot2::aes(xintercept = .data$g),
      linetype = "dashed",
      colour = "black",
      linewidth = 0.5,
      alpha = 0.7,
      inherit.aes = FALSE
    ) +
    ggplot2::facet_wrap(~g_label, scales = "free_x") +
    ggplot2::scale_x_continuous(breaks = sort(unique(att$t))) +
    ggplot2::labs(
      x = "Calendar time (t)",
      y = sprintf("ATT(g,t) and %.0f%% pointwise CI", ci_level * 100)
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      strip.text = ggplot2::element_text(face = "bold")
    )

  # Add simultaneous CI ribbon when bootstrap data are present
  if (has_boot) {
    p <- p +
      ggplot2::geom_ribbon(
        ggplot2::aes(
          ymin = .data[["conf_low_sim"]],
          ymax = .data[["conf_high_sim"]],
          fill = .data[[".sim_label"]]
        ),
        alpha = 0.10,
        na.rm = TRUE
      ) +
      ggplot2::scale_fill_manual(
        name = NULL,
        values = setNames(fill, sim_ci_pct)
      )
  }

  if (zero_line) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = 0,
        linetype = "dashed",
        colour = "grey40",
        linewidth = 0.4
      )
  }

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
