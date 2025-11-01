# Test script for run_es() with i() implementation
# This script tests the fixed run_es() function

library(dplyr)
library(fixest)

# Create sample data similar to user's scenario
set.seed(123)
n_units <- 50
n_periods <- 10

base_did <- expand.grid(
  id = 1:n_units,
  period = 1:n_periods
) %>%
  mutate(
    treat = ifelse(id <= n_units/2, 1, 0),  # Half treated, half control
    y = rnorm(n()) + treat * (period >= 5) * 2,  # Treatment effect after period 5
    y = y + rnorm(n(), sd = 0.5)
  )

# Test 1: User's original example (with two-way FE)
# This should work now without collinearity error
es <- run_es(
  data = base_did,
  outcome = y,
  treatment = treat,
  time = period,
  timing = 5,
  fe = ~id + period  # Two-way FE - this was causing collinearity before
)

print("Test 1 passed: Two-way FE works without collinearity")
print(head(es))

# Verify that term contains relative time
stopifnot(all(es$term == as.character(es$relative_time)))
print("Term column contains relative time: PASS")

# Test 2: Without FE (fe = NULL)
es_no_fe <- run_es(
  data = base_did,
  outcome = y,
  treatment = treat,
  time = period,
  timing = 5,
  fe = NULL  # No fixed effects
)

print("Test 2 passed: Works without FE")
print(head(es_no_fe))

# Test 3: Compare with direct feols call
es_direct <- feols(y ~ i(period, treat, ref = 5) | id + period, data = base_did)
es_direct_tidy <- broom::tidy(es_direct)

print("Test 3: Compare with direct feols call")
print("Direct feols result:")
print(head(es_direct_tidy))

# Test 4: plot_es() compatibility
p <- plot_es(es)
print("Test 4 passed: plot_es() works with the result")

print("\nAll tests passed!")
