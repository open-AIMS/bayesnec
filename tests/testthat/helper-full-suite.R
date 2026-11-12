skip_unless_full_suite <- function() {
  if (!identical(tolower(Sys.getenv("BAYESNEC_FULL_TESTS")), "true")) {
    testthat::skip("Set BAYESNEC_FULL_TESTS=true to run the full test suite")
  }
}
