# Test runner. From the repository root:
#   Rscript tests/testthat.R
# Runs the unit suite on synthetic fixtures only, so no data or config.R is
# needed and nothing sensitive is touched.

library(testthat)
testthat::test_dir(
  file.path("tests", "testthat"),
  stop_on_failure = TRUE
)
