# test-formula.R
#
# The formula interface of event_study(), did() and att() must reproduce the
# legacy argument interface exactly (all attributes but `call`), for each of
# the three ways of specifying treatment: a D column, a treatment-timing
# column, and a treated-group dummy with a single treatment date.

library(testthat)
library(fixes)

make_fml_panel <- function(seed = 51L, n_id = 40L) {
  set.seed(seed)
  df <- expand.grid(id = seq_len(n_id), year = 2001:2008)
  g <- sample(c(2004L, 2006L, NA_integer_), n_id, replace = TRUE,
              prob = c(0.4, 0.3, 0.3))
  df$gvar    <- g[df$id]
  df$treated <- as.integer(!is.na(df$gvar))
  df$D       <- as.integer(!is.na(df$gvar) & df$year >= df$gvar)
  df$region  <- (df$id %% 4L) + 1L
  df$x1      <- rnorm(nrow(df))
  df$y <- rnorm(n_id)[df$id] + 0.1 * df$year + 0.7 * df$D + 0.3 * df$x1 +
    rnorm(nrow(df), sd = 0.5)
  df
}

# Single treatment date (2005) for every treated unit.
make_fml_univ <- function(seed = 52L, n_id = 40L) {
  set.seed(seed)
  df <- expand.grid(id = seq_len(n_id), year = 2001:2008)
  tr <- rbinom(n_id, 1L, 0.5)
  df$treated <- tr[df$id]
  df$D  <- as.integer(df$treated == 1L & df$year >= 2005L)
  df$x1 <- rnorm(nrow(df))
  df$y  <- rnorm(n_id)[df$id] + 0.1 * df$year + 0.7 * df$D + 0.3 * df$x1 +
    rnorm(nrow(df), sd = 0.5)
  df
}

make_fml_flex <- function(seed = 53L, M = 10L) {
  set.seed(seed)
  cohort <- c(2L, 2L, 2L, 4L, 4L, 4L, NA, NA, NA)
  group_fe <- rnorm(length(cohort)) * 0.5
  rows <- lapply(seq_along(cohort), function(g) {
    do.call(rbind, lapply(1:8, function(t) {
      on <- !is.na(cohort[g]) && t >= cohort[g]
      data.frame(year = t, group_id = g, g = cohort[g],
                 D = as.integer(on),
                 y = group_fe[g] + 0.1 * t + 1.5 * on + rnorm(M, sd = 0.5))
    }))
  })
  do.call(rbind, rows)
}

strip_attrs <- function(x, drop = "call") {
  for (a in drop) attr(x, a) <- NULL
  x
}

es_core <- function(x) {
  data.frame(relative_time = x$relative_time, estimate = x$estimate,
             std.error = x$std.error)
}

# Evaluate `expr`, return the messages of all warnings it signalled.
warning_messages <- function(expr) {
  msgs <- character()
  withCallingHandlers(
    expr,
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  msgs
}

df   <- make_fml_panel()
df_u <- make_fml_univ()

# ---------------------------------------------------------------------------
# Formula syntax
# ---------------------------------------------------------------------------

test_that("formula syntax errors are explicit", {
  expect_error(event_study(~ D | id + year, df, time = year),
               "two-sided")
  expect_error(event_study(y ~ D | id + year | x1, df, time = year),
               "one `\\|`")
  expect_error(event_study(y ~ i(year, treated, 2004) | id + year, df,
                           time = year),
               "builds the event-time dummies")
  expect_error(event_study(y ~ . | id + year, df, time = year), "`\\.`")
  expect_error(event_study(y ~ D - x1 | id + year, df, time = year),
               "`-`")
  expect_error(event_study(y ~ D + nope | id + year, df, time = year),
               "nope")
  expect_error(event_study(y ~ D | id + nope, df, time = year), "nope")
  expect_error(event_study(y ~ log(D) | id + year, df, time = year, unit = id),
               "column name")
  expect_error(event_study(y ~ 1 | id + year, df, time = year, timing = gvr),
               "`gvr` is neither")
})

test_that("an expression on the left-hand side works as in the legacy API", {
  new <- event_study(log(y + 10) ~ treated | id + year, df_u, time = year,
                     timing = 2005, unit = id)
  old <- event_study(df_u, outcome = log(y + 10), treatment = treated,
                     time = year, timing = 2005, fe = ~ id + year, unit = id)
  expect_identical(strip_attrs(new), strip_attrs(old))
})

# ---------------------------------------------------------------------------
# Equivalence with the legacy interface
# ---------------------------------------------------------------------------

test_that("twfe with a single treatment date matches the legacy call", {
  new <- event_study(y ~ treated + x1 | id + year, df_u, time = year,
                     timing = 2005, unit = id, cluster = ~id)
  old <- event_study(df_u, outcome = y, treatment = treated, time = year,
                     timing = 2005, fe = ~ id + year, covariates = ~ x1,
                     cluster = ~id, unit = id)
  expect_identical(strip_attrs(new), strip_attrs(old))
  expect_false(attr(new, "staggered"))
})

test_that("twfe with a timing column matches the legacy call", {
  new <- event_study(y ~ x1 | id + year, df, time = year, timing = gvar,
                     unit = id, conf_level = c(0.9, 0.95))
  old <- event_study(df, outcome = y, treatment = treated, time = year,
                     timing = gvar, fe = ~ id + year, covariates = ~ x1,
                     unit = id, conf_level = c(0.9, 0.95))
  # The treated-group column is built internally, so only its name in the
  # fixest-style model_formula differs.
  expect_identical(strip_attrs(new, c("call", "model_formula")),
                   strip_attrs(old, c("call", "model_formula")))
  expect_true(attr(new, "staggered"))
})

test_that("twfe with rel_time matches the legacy call", {
  d <- df
  d$rt <- d$year - d$gvar
  new <- event_study(y ~ treated | id + year, d, time = year, rel_time = rt,
                     unit = id)
  old <- event_study(d, outcome = y, treatment = treated, time = year,
                     rel_time = rt, fe = ~ id + year, unit = id)
  expect_identical(strip_attrs(new), strip_attrs(old))
})

test_that("staggered estimators match the legacy call", {
  new <- event_study(y ~ 1 | id + year, df, time = year, timing = gvar,
                     estimator = "cs")
  old <- event_study(df, outcome = y, time = year, timing = gvar, unit = id,
                     estimator = "cs")
  expect_identical(strip_attrs(new), strip_attrs(old))

  new <- event_study(y ~ 1 | id + year, df, time = year, timing = gvar,
                     estimator = "cs", control_group = "notyettreated",
                     bootstrap = TRUE, boot_reps = 49L, boot_seed = 7L)
  old <- event_study(df, outcome = y, time = year, timing = gvar, unit = id,
                     estimator = "cs", control_group = "notyettreated",
                     bootstrap = TRUE, boot_reps = 49L, boot_seed = 7L)
  expect_identical(strip_attrs(new), strip_attrs(old))

  new <- event_study(y ~ 1 | id + year, df, time = year, timing = gvar,
                     estimator = "sa", cluster = ~id)
  old <- event_study(df, outcome = y, time = year, timing = gvar, unit = id,
                     fe = ~ id + year, estimator = "sa", cluster = ~id)
  expect_identical(strip_attrs(new), strip_attrs(old))

  new <- event_study(y ~ 1 | id + year, df, time = year, timing = gvar,
                     estimator = "bjs")
  old <- event_study(df, outcome = y, time = year, timing = gvar, unit = id,
                     estimator = "bjs")
  expect_identical(strip_attrs(new), strip_attrs(old))

  new <- event_study(y ~ x1 | id + year, df, time = year, timing = gvar,
                     estimator = "twm")
  old <- event_study(df, outcome = y, time = year, timing = gvar, unit = id,
                     fe = ~ id + year, covariates = ~ x1, estimator = "twm")
  expect_identical(strip_attrs(new), strip_attrs(old))

  fx <- make_fml_flex()
  new <- event_study(y ~ 1, fx, time = year, timing = g, group = group_id,
                     estimator = "flex", cluster = ~group_id)
  old <- event_study(fx, outcome = y, time = year, timing = g,
                     group = group_id, estimator = "flex", cluster = ~group_id)
  expect_identical(strip_attrs(new), strip_attrs(old))
})

test_that("did() matches the legacy call", {
  new <- did(y ~ D + x1 | id + year, df, cluster = ~id)
  old <- did(df, outcome = y, treatment = D, covariates = ~ x1,
             fe = ~ id + year, cluster = ~id)
  expect_identical(strip_attrs(new), strip_attrs(old))

  new <- did(y ~ treated | id + year, df_u, time = year, timing = 2005)
  old <- did(df_u, outcome = y, treatment = treated, time = year,
             timing = 2005, fe = ~ id + year, unit = id)
  expect_identical(strip_attrs(new), strip_attrs(old))
})

test_that("did() builds D from a timing column", {
  from_timing <- did(y ~ x1 | id + year, df, time = year, timing = gvar)
  from_d      <- did(y ~ D + x1 | id + year, df, time = year)
  expect_identical(from_timing$estimates$estimate, from_d$estimates$estimate)
  expect_identical(from_timing$estimates$std.error,
                   from_d$estimates$std.error)
})

test_that("att() matches the legacy call", {
  for (est in c("cs", "bjs")) {
    for (agg in c("simple", "by_cohort", "by_time")) {
      new <- att(y ~ 1 | id + year, df, time = year, timing = gvar,
                 estimator = est, aggregation = agg)
      old <- att(df, outcome = y, time = year, timing = gvar, unit = id,
                 estimator = est, aggregation = agg)
      expect_identical(new, old)
    }
  }
})

# ---------------------------------------------------------------------------
# D column vs timing
# ---------------------------------------------------------------------------

test_that("a D column reproduces the equivalent timing call", {
  # several adoption dates -> staggered design
  from_d <- event_study(y ~ D + x1 | id + year, df, time = year)
  from_g <- event_study(y ~ x1 | id + year, df, time = year, timing = gvar)
  expect_identical(es_core(from_d), es_core(from_g))
  expect_identical(attr(from_d, "es_vcov"), attr(from_g, "es_vcov"))
  expect_true(attr(from_d, "staggered"))

  # one adoption date -> universal design
  from_d <- event_study(y ~ D + x1 | id + year, df_u, time = year)
  from_v <- event_study(y ~ treated + x1 | id + year, df_u, time = year,
                        timing = 2005)
  expect_identical(es_core(from_d), es_core(from_v))
  expect_identical(attr(from_d, "es_vcov"), attr(from_v, "es_vcov"))
  expect_false(attr(from_d, "staggered"))

  for (est in c("cs", "sa", "bjs", "twm")) {
    from_d <- event_study(y ~ D | id + year, df, time = year, estimator = est)
    from_g <- event_study(y ~ 1 | id + year, df, time = year, timing = gvar,
                          estimator = est)
    expect_identical(es_core(from_d), es_core(from_g))
  }

  fx <- make_fml_flex()
  from_d <- event_study(y ~ D, fx, time = year, group = group_id,
                        estimator = "flex", cluster = ~group_id)
  from_g <- event_study(y ~ 1, fx, time = year, timing = g, group = group_id,
                        estimator = "flex", cluster = ~group_id)
  expect_identical(es_core(from_d), es_core(from_g))

  a_d <- att(y ~ D | id + year, df, time = year, aggregation = "by_cohort")
  a_g <- att(y ~ 1 | id + year, df, time = year, timing = gvar,
             aggregation = "by_cohort")
  expect_identical(as.data.frame(a_d), as.data.frame(a_g))
})

test_that("a D column is read on the time index with time_transform", {
  # irregular calendar: periods 1, 3, 4, 7, 9, 12
  d <- df[df$year %in% c(2001, 2002, 2003, 2005, 2006, 2008), ]
  d$period <- c(1, 3, 4, 7, 9, 12)[match(d$year, sort(unique(d$year)))]
  d$D <- as.integer(!is.na(d$gvar) & d$year >= d$gvar)
  # timing expressed as the index of the first treated period
  idx <- match(d$year, sort(unique(d$year)))
  first_on <- tapply(ifelse(d$D == 1L, idx, NA_integer_), d$id, function(v) {
    if (all(is.na(v))) NA_integer_ else min(v, na.rm = TRUE)
  })
  d$g_idx <- as.integer(first_on[as.character(d$id)])

  from_d <- event_study(y ~ D | id + period, d, time = period,
                        time_transform = TRUE)
  from_g <- event_study(y ~ 1 | id + period, d, time = period,
                        timing = g_idx, time_transform = TRUE)
  expect_identical(es_core(from_d), es_core(from_g))
})

# ---------------------------------------------------------------------------
# Argument order
# ---------------------------------------------------------------------------

test_that("formula and data may come in either order", {
  a <- event_study(y ~ D | id + year, df, time = year, unit = id)
  b <- event_study(df, y ~ D | id + year, time = year, unit = id)
  p <- df |> event_study(y ~ D | id + year, time = year, unit = id)
  f <- y ~ D | id + year
  v <- event_study(df, f, time = year, unit = id)
  expect_identical(strip_attrs(a), strip_attrs(b))
  expect_identical(strip_attrs(a), strip_attrs(p))
  expect_identical(strip_attrs(a), strip_attrs(v))

  expect_identical(
    strip_attrs(did(df, y ~ D | id + year)),
    strip_attrs(did(y ~ D | id + year, df))
  )
  expect_identical(
    att(df, y ~ D | id + year, time = year, unit = id),
    att(y ~ D | id + year, df, time = year, unit = id)
  )
})

test_that("positional legacy calls fail with a pointer to the formula", {
  expect_error(event_study(df, y, treated, year, 2005), "formula")
  expect_error(att(df, y, year, gvar, unit = id), "formula")
  expect_error(did(df, y, D), "formula")
})

test_that("formula and legacy model arguments cannot be mixed", {
  expect_error(event_study(y ~ D | id + year, df, time = year, outcome = y),
               "not both")
  expect_error(did(y ~ D | id + year, df, fe = ~ id + year), "not both")
})

# ---------------------------------------------------------------------------
# Deprecation of the legacy model arguments
# ---------------------------------------------------------------------------

test_that("legacy model arguments are soft-deprecated; formulas are not", {
  lifecycle::expect_deprecated(
    event_study(df, outcome = y, treatment = treated, time = year,
                timing = gvar, fe = ~ id + year)
  )
  lifecycle::expect_deprecated(did(df, outcome = y, treatment = D))
  lifecycle::expect_deprecated(
    att(df, outcome = y, time = year, timing = gvar, unit = id)
  )

  rlang::local_options(lifecycle_verbosity = "warning")
  expect_no_warning(event_study(y ~ D | id + year, df, time = year, unit = id))
  expect_no_warning(did(y ~ D | id + year, df))
  expect_no_warning(att(y ~ D | id + year, df, time = year, unit = id))
})

test_that("the deprecated verb wrappers warn exactly once", {
  rlang::local_options(lifecycle_verbosity = "warning")
  w <- warning_messages(
    run_es(df, outcome = y, treatment = treated, time = year, timing = gvar,
           fe = ~ id + year, staggered = TRUE)
  )
  expect_length(w, 1L)
  w <- warning_messages(run_did(df, outcome = y, treatment = D))
  expect_length(w, 1L)
  w <- warning_messages(
    calc_att(df, outcome = y, time = year, timing = gvar, unit = id)
  )
  expect_length(w, 1L)
})

test_that("the deprecation message announces 2.0.0 and a runnable call", {
  rlang::local_options(lifecycle_verbosity = "warning")
  w <- warning_messages(
    old <- event_study(df_u, outcome = y, treatment = treated, time = year,
                       timing = 2005, fe = ~ id + year, covariates = ~ x1,
                       cluster = ~id, unit = id)
  )
  expect_length(w, 1L)
  expect_match(w, "2.0.0", fixed = TRUE)
  expect_match(w, "planned", fixed = TRUE)
  expect_match(w, as.character(utils::packageVersion("fixes")), fixed = TRUE)

  suggestion <- sub("^.*Equivalent call: ", "", w)
  new <- eval(parse(text = suggestion))
  expect_identical(strip_attrs(new), strip_attrs(old))
})

test_that("suggested calls reproduce cs, did and att legacy calls", {
  rlang::local_options(lifecycle_verbosity = "warning")
  run_suggestion <- function(w) eval(parse(text = sub("^.*Equivalent call: ",
                                                      "", w)))

  w <- warning_messages(
    old <- event_study(df, outcome = y, time = year, timing = gvar,
                       unit = id, estimator = "cs")
  )
  expect_identical(strip_attrs(run_suggestion(w)), strip_attrs(old))

  w <- warning_messages(
    old <- did(df, outcome = y, treatment = D, fe = ~ id + year,
               cluster = ~id)
  )
  expect_identical(strip_attrs(run_suggestion(w)), strip_attrs(old))

  w <- warning_messages(
    old <- att(df, outcome = y, time = year, timing = gvar, unit = id,
               aggregation = "by_time")
  )
  expect_identical(run_suggestion(w), old)
})

# ---------------------------------------------------------------------------
# Panel identifiers
# ---------------------------------------------------------------------------

test_that("unit is taken from the fixed effects and reported", {
  expect_message(
    res <- event_study(y ~ D | id + year, df, time = year),
    "Panel: unit = id, time = year"
  )
  expect_identical(attr(res, "unit_var"), "id")
  expect_identical(attr(res, "time_var"), "year")
  expect_output(print(res), "Panel: unit = id \\| time = year")

  # the order of the fixed effects does not matter once `time` is known
  expect_message(
    rev_fe <- event_study(y ~ D | year + id, df, time = year),
    "unit = id"
  )
  expect_equal(es_core(rev_fe), es_core(res), tolerance = 1e-10)

  expect_no_message(event_study(y ~ D | id + year, df, time = year, unit = id))

  expect_error(event_study(y ~ D | id + year + region, df, time = year),
               "unit = ")
  expect_error(event_study(y ~ D, df, time = year), "unit = ")
  expect_error(event_study(y ~ D | id + year, df), "`time`")

  r_did <- suppressMessages(did(y ~ D | id + year, df, time = year))
  expect_output(print(r_did), "Panel: unit = id \\| time = year")
  r_att <- suppressMessages(att(y ~ D | id + year, df, time = year))
  expect_output(print(r_att), "Panel: unit = id \\| time = year")
})

test_that("non-column fixed effects need an explicit unit", {
  skip_if_not_installed("fixest")
  expect_error(event_study(y ~ D | id + region^year, df, time = year),
               "unit = ")
  new <- event_study(y ~ D | id + region^year, df, time = year, unit = id)
  old <- event_study(df, outcome = y, treatment = treated, time = year,
                     timing = gvar, fe = ~ id + region^year, unit = id)
  expect_identical(es_core(new), es_core(old))
})

# ---------------------------------------------------------------------------
# Reading a D column
# ---------------------------------------------------------------------------

test_that("a D column is validated", {
  d <- df
  d$D2 <- d$D * 2L
  expect_error(event_study(y ~ D2 | id + year, d, time = year), "0/1")

  d <- df
  d$D[d$id == d$id[d$treated == 1L][1] & d$year == 2008L] <- 0L
  expect_error(event_study(y ~ D | id + year, d, time = year, unit = id),
               "switches back")

  expect_error(event_study(y ~ treated | id + year, df, time = year,
                           unit = id),
               "treated-group dummy")

  d <- df
  d$D <- 0L
  expect_error(event_study(y ~ D | id + year, d, time = year, unit = id),
               "never 1")
})

test_that("units treated from their first period and missing D warn", {
  d <- df
  never <- unique(d$id[is.na(d$gvar)])[1]
  d$D[d$id == never] <- 1L
  expect_warning(event_study(y ~ D | id + year, d, time = year, unit = id),
                 "first observed period")

  d <- df
  d$D[c(3L, 50L)] <- NA
  expect_warning(
    res <- event_study(y ~ D | id + year, d, time = year, unit = id),
    "2 row"
  )
  ref <- event_study(y ~ D | id + year, d[-c(3L, 50L), ], time = year,
                     unit = id)
  expect_identical(es_core(res), es_core(ref))
})

# ---------------------------------------------------------------------------
# Guards against mixing up the three specifications
# ---------------------------------------------------------------------------

test_that("treatment terms next to a timing column are rejected", {
  expect_error(event_study(y ~ D | id + year, df, time = year, timing = gvar),
               "defined by `timing`")
  expect_error(event_study(y ~ treated | id + year, df, time = year,
                           timing = gvar),
               "defined by `timing`")
})

test_that("a D column next to a single treatment date is rejected", {
  expect_error(event_study(y ~ D | id + year, df_u, time = year,
                           timing = 2005),
               "varies within units")
})

# ---------------------------------------------------------------------------
# Estimator-specific checks
# ---------------------------------------------------------------------------

test_that("sa accepts covariates via formula in event_study()", {
  e_g <- event_study(y ~ x1 | id + year, df, time = year, timing = gvar,
                     estimator = "sa")
  e_d <- event_study(y ~ D + x1 | id + year, df, time = year,
                     estimator = "sa")
  expect_s3_class(e_g, "es_result")
  expect_identical(es_core(e_g), es_core(e_d))
})

test_that("cs and bjs accept covariates via formula in att() and event_study()", {
  for (est in c("cs", "bjs")) {
    a_g <- att(y ~ x1 | id + year, df, time = year, timing = gvar,
               estimator = est)
    a_d <- att(y ~ D + x1 | id + year, df, time = year, estimator = est)
    expect_s3_class(a_g, "att_result")
    expect_identical(as.data.frame(a_g), as.data.frame(a_d))

    e_g <- event_study(y ~ x1 | id + year, df, time = year, timing = gvar,
                       estimator = est)
    e_d <- event_study(y ~ D + x1 | id + year, df, time = year,
                       estimator = est)
    expect_s3_class(e_g, "es_result")
    expect_identical(es_core(e_g), es_core(e_d))
  }
})

test_that("twm and flex evaluate covariate expressions instead of their variables", {
  d <- df
  d$ax1 <- abs(d$x1)
  d$r2 <- as.integer(d$region == 2L)
  d$r3 <- as.integer(d$region == 3L)
  d$r4 <- as.integer(d$region == 4L)
  expr_fit <- event_study(y ~ abs(x1) + factor(region) | id + year, d,
                          time = year, timing = gvar, estimator = "twm")
  col_fit  <- event_study(y ~ ax1 + r2 + r3 + r4 | id + year, d,
                          time = year, timing = gvar, estimator = "twm")
  expect_identical(es_core(expr_fit), es_core(col_fit))
  # the legacy argument used to read `abs(x1)` as `x1`
  old <- event_study(d, outcome = y, time = year, timing = gvar, unit = id,
                     fe = ~ id + year, covariates = ~ abs(x1),
                     estimator = "twm")
  ref <- event_study(y ~ ax1 | id + year, d, time = year, timing = gvar,
                     estimator = "twm")
  expect_identical(es_core(old), es_core(ref))

  fx <- make_fml_flex()
  set.seed(1L)
  fx$x <- runif(nrow(fx), 1, 5)
  fx$lx <- log(fx$x)
  expr_fit <- event_study(y ~ log(x), fx, time = year, timing = g,
                          group = group_id, estimator = "flex",
                          cluster = ~group_id)
  col_fit  <- event_study(y ~ lx, fx, time = year, timing = g,
                          group = group_id, estimator = "flex",
                          cluster = ~group_id)
  expect_identical(es_core(expr_fit), es_core(col_fit))
})

test_that("unit and time fixed effects are accepted silently by cs and bjs", {
  expect_no_warning(event_study(y ~ 1 | id + year, df, time = year,
                                timing = gvar, estimator = "cs"))
  expect_warning(event_study(y ~ 1 | id + year + region, df, time = year,
                             timing = gvar, unit = id, estimator = "cs"),
                 "region")
  expect_warning(event_study(y ~ 1 | id + year + region, df, time = year,
                             timing = gvar, unit = id, estimator = "bjs"),
                 "region")
})
