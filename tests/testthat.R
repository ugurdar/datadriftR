# Minimum testthat runner that skips tests when the package is not installed
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("testthat is required to run tests")
}

if (!requireNamespace("datadriftR", quietly = TRUE)) {
  message("datadriftR not installed: tests will be skipped when run outside package build")
  # create a tiny placeholder so `test_check` will not error in some runners
  # but we rely on explicit skip() inside tests
}

testthat::test_check("datadriftR")
