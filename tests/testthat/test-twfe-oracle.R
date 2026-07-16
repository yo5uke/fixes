# test-twfe-oracle.R
#
# Numerical agreement of the classic TWFE event-study path (internal FE-OLS
# engine since v1.0.0) against the previous implementation: fixest::feols()
# on an i() formula with broom::tidy() inference. Guards the engine
# migration — estimates, SEs, t statistics, p-values, es_vcov, and N must
# reproduce the pre-migration pipeline.

library(testthat)
library(fixes)

make_stag_panel <- function(seed = 42L) {
  set.seed(seed)
  n_id <- 60L
  df <- expand.grid(id = seq_len(n_id), year = 2001:2010)
  g <- sample(c(2004, 2006, 2008, NA), n_id, replace = TRUE,
              prob = c(0.3, 0.3, 0.2, 0.2))
  df$gvar    <- g[df$id]
  df$treated <- as.integer(!is.na(df$gvar))
  df$x1  <- rnorm(nrow(df))
  df$x2  <- runif(nrow(df), 1, 3)
  df$grp <- factor(sample(letters[1:3], nrow(df), replace = TRUE))
  df$w   <- runif(nrow(df), 0.5, 2)
  te <- ifelse(!is.na(df$gvar) & df$year >= df$gvar,
               0.5 * (df$year - df$gvar + 1), 0)
  df$y <- rnorm(n_id)[df$id] + 0.2 * df$year + te + 0.3 * df$x1 +
    rnorm(nrow(df), sd = 0.5)
  df
}

# Pre-migration pipeline: feols on the i() formula + broom::tidy inference.
oracle_fit <- function(df, i_str, fe_str = "id + year", cov_text = NULL,
                       cluster = NULL, weights = NULL, vcov = "HC1") {
  rhs <- i_str
  if (!is.null(cov_text)) rhs <- paste(rhs, cov_text, sep = " + ")
  fml <- stats::as.formula(paste0("y ~ ", rhs, " | ", fe_str))
  args <- list(fml, data = df)
  if (!is.null(cluster)) args$cluster <- cluster
  if (!is.null(weights)) args$weights <- weights
  m <- do.call(fixest::feols, args)
  td <- if (!is.null(cluster) && identical(vcov, "HC1")) {
    broom::tidy(m)
  } else {
    broom::tidy(m, vcov = stats::vcov(m, vcov = vcov))
  }
  list(model = m, tidy = td)
}

expect_es_matches_oracle <- function(res, orc, value_from_term,
                                     tol = 1e-8) {
  td <- orc$tidy
  td$rt <- value_from_term(as.character(td$term))
  td <- td[!is.na(td$rt), ]
  td <- td[order(td$rt), ]

  est <- res[!res$is_baseline, ]
  est <- est[order(est$relative_time), ]

  expect_equal(est$relative_time, td$rt)
  expect_equal(est$estimate, as.numeric(td$estimate), tolerance = tol)
  expect_equal(est$std.error, as.numeric(td$std.error), tolerance = tol)
  expect_equal(est$statistic, as.numeric(td$statistic), tolerance = tol)
  expect_equal(est$p.value, as.numeric(td$p.value), tolerance = tol)
  expect_equal(attr(res, "N"), stats::nobs(orc$model))
}

# Level value encoded in an i() coefficient name ("fixest::..k::-2:treated"
# or "..k::-2:treated"): last "::" part, before the ":" interaction suffix.
i_level_from_terms <- function(terms) {
  parts <- strsplit(terms, "::", fixed = TRUE)
  last  <- vapply(parts, function(x) x[length(x)], character(1))
  is_ev <- lengths(parts) > 1L
  out <- suppressWarnings(as.numeric(
    vapply(strsplit(last, ":", fixed = TRUE), function(x) x[1], character(1))
  ))
  out[!is_ev] <- NA_real_
  out
}

rt_from_k_terms <- function(terms) {
  as.integer(i_level_from_terms(terms))
}

test_that("classic staggered matches the old feols pipeline (cluster)", {
  skip_if_not_installed("fixest")
  df <- make_stag_panel()

  res <- run_es(
    data = df, outcome = y, treatment = treated, time = year, timing = gvar,
    fe = ~ id + year, cluster = ~id, staggered = TRUE, baseline = -1
  )

  dfo <- df
  dfo$..k <- suppressWarnings(as.integer(round(dfo$year - dfo$gvar)))
  dfo$..k[is.na(dfo$..k)] <- -1L
  orc <- oracle_fit(dfo, "fixest::i(..k, treated, ref = -1)",
                    cluster = ~id)
  expect_es_matches_oracle(res, orc, rt_from_k_terms)

  # es_vcov must equal the clustered VCOV of the event coefficients
  V_ref <- stats::vcov(orc$model)
  ev <- grep("\\.\\.k::", rownames(V_ref), value = TRUE)
  rt <- rt_from_k_terms(ev)
  ev <- ev[order(rt)]
  V_ref <- V_ref[ev, ev]
  es_v <- attr(res, "es_vcov")
  expect_equal(unname(as.matrix(es_v)), unname(as.matrix(V_ref)),
               tolerance = 1e-8)
  expect_equal(rownames(es_v), as.character(sort(rt)))
})

test_that("classic staggered matches old pipeline (weights + covariates)", {
  skip_if_not_installed("fixest")
  df <- make_stag_panel(7L)

  res <- run_es(
    data = df, outcome = y, treatment = treated, time = year, timing = gvar,
    fe = ~ id + year, weights = ~w, covariates = ~ x1 + log(x2) + grp,
    staggered = TRUE, baseline = -1
  )

  dfo <- df
  dfo$..k <- suppressWarnings(as.integer(round(dfo$year - dfo$gvar)))
  dfo$..k[is.na(dfo$..k)] <- -1L
  orc <- oracle_fit(dfo, "fixest::i(..k, treated, ref = -1)",
                    cov_text = "x1 + log(x2) + grp", weights = ~w)
  expect_es_matches_oracle(res, orc, rt_from_k_terms)
})

test_that("classic universal timing matches old pipeline (iid and HC1)", {
  skip_if_not_installed("fixest")
  set.seed(9)
  n_id <- 50L
  df <- expand.grid(id = seq_len(n_id), year = 2001:2010)
  df$treated <- as.integer(df$id <= 25)
  df$y <- rnorm(n_id)[df$id] + 0.1 * df$year +
    ifelse(df$treated == 1 & df$year >= 2006, 0.8, 0) + rnorm(nrow(df))

  rt_from_year_terms <- function(terms) {
    as.integer(round(i_level_from_terms(terms) - 2006))
  }

  for (vc in c("HC1", "iid")) {
    res <- run_es(
      data = df, outcome = y, treatment = treated, time = year,
      timing = 2006, fe = ~ id + year, vcov = vc,
      conf.level = c(0.90, 0.95)
    )
    orc <- oracle_fit(df, "fixest::i(year, treated, ref = 2005)", vcov = vc)
    expect_es_matches_oracle(res, orc, rt_from_year_terms)

    # multi-level CI columns follow the normal-quantile construction
    z <- stats::qnorm(0.95)
    est <- res[!res$is_baseline, ]
    expect_equal(est$conf_low_90, est$estimate - z * est$std.error,
                 tolerance = 1e-12)
  }
})

test_that("classic falls back to fixest for id^year fixed effects", {
  skip_if_not_installed("fixest")
  df <- make_stag_panel(11L)
  df$region <- ((df$id - 1L) %% 5L) + 1L

  res <- run_es(
    data = df, outcome = y, treatment = treated, time = year, timing = gvar,
    fe = ~ region^year + id, staggered = TRUE, baseline = -1
  )

  dfo <- df
  dfo$..k <- suppressWarnings(as.integer(round(dfo$year - dfo$gvar)))
  dfo$..k[is.na(dfo$..k)] <- -1L
  orc <- oracle_fit(dfo, "fixest::i(..k, treated, ref = -1)",
                    fe_str = "region^year + id")
  expect_es_matches_oracle(res, orc, rt_from_k_terms, tol = 1e-6)
})
