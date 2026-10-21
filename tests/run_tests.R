# Run the tests:  Rscript tests/run_tests.R   (needs the testthat package)
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Install testthat first: install.packages('testthat')", call. = FALSE)
}
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE))
tests_dir <- if (length(file_arg)) {
  dirname(normalizePath(file_arg[1]))
} else {
  file.path(getwd(), "tests")
}
testthat::test_dir(file.path(tests_dir, "testthat"), stop_on_failure = TRUE)
