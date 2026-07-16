# The suite exercises the deprecated verb-style wrappers (run_es, calc_att,
# run_did, plot_es, ...) on purpose; silence lifecycle's deprecation noise
# globally. test-deprecated.R re-enables it locally via
# lifecycle::expect_deprecated().
options(lifecycle_verbosity = "quiet")
