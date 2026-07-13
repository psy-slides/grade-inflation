# Entry point for the automated tests. Run from the repository root with:
#   Rscript tests/testthat.R
# or interactively with testthat::test_dir("tests/testthat").
library(testthat)

test_root <- if (dir.exists("tests/testthat")) "tests/testthat" else "testthat"
result <- test_dir(test_root, reporter = "summary", stop_on_failure = TRUE)
