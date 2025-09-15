#' @export
print.es_result <- function(x, ...) {
  cat("Event Study Result (fixes)\n")
  n  <- attr(x, "N"); nu <- attr(x, "N_units")
  nt <- attr(x, "N_treated"); nn <- attr(x, "N_nevertreated")
  fe <- attr(x, "fe"); vc <- attr(x, "vcov_type")
  cl <- attr(x, "cluster_vars"); su <- attr(x, "sunab_used")
  lg <- attr(x, "lead_range"); rg <- attr(x, "lag_range"); bs <- attr(x, "baseline")

  cat("  N:", n, " | Units:", nu,
      " | Treated units:", nt, " | Never-treated:", nn, "\n")
  cat("  FE: ", fe, "\n", sep = "")
  cat("  VCOV:", vc, " | Cluster:", paste(if (is.null(cl)) "-" else cl, collapse = " + "), "\n")
  if (isTRUE(su)) {
    cat("  Method: SUNAB (staggered-safe)\n")
  } else {
    cat("  Method: classic  | lead_range:", lg, " lag_range:", rg, " baseline:", bs, "\n")
  }
  invisible(x)
}

#' @export
autoplot.es_result <- function(object, ci_level = 0.95, type = "ribbon", ...) {
  plot_es(object, ci_level = ci_level, type = type, ...)
}
