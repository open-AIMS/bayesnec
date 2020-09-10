#' validate_priors
#'
#' Checks if priors are appropriate to specified model
#'
#' @inheritParams bnec
#'
#' @return A \code{\link[base]{data.frame}} of class
#' \code{\link[brms]{brmsprior}}.
validate_priors <- function(priors, model) {
  if (inherits(priors, "list")) {
    if (!model %in% names(priors)) {
      stop("Named prior list does not contain ",
           "priors for model ", model)
    }
    priors <- priors[[model]]
  }
  if (!inherits(priors, "brmsprior")) {
    stop("Prior is not of class brmsprior; ",
         "see ?bnec for argument details")
  }
  priors
}
