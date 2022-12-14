#' @noRd
capture_family <- function(manec) {
  UseMethod("capture_family")
}

#' @noRd
#' @importFrom utils capture.output
capture_family.bayesmanecfit <- function(manec) {
  x <- manec$mod_fits[[1]]$fit
  out <- capture.output(print(summary(x)))
  list(family = grep("^ Family:", out, value = TRUE),
       links = grep("^  Links:", out, value = TRUE))
}
