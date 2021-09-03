#' validate_priors
#'
#' Checks if priors are appropriate to specified model
#'
#' @inheritParams bnec
#'
#' @param priors Either a \code{\link[base]{data.frame}} of class
#' \code{\link[brms]{brmsprior}}, or a \code{\link[base]{list}} containing
#' multiple objects of class \code{\link[brms]{brmsprior}}.
#'
#' @return A \code{\link[base]{data.frame}} of class
#' \code{\link[brms]{brmsprior}}.
#'
#' @noRd
validate_priors <- function(priors, model) {
  if (missing(priors)) {
    stop("No valid prior specified.")
  }
  if (inherits(priors, "list")) {
    if (!model %in% names(priors)) {
      message("Named prior list does not contain ",
              "priors for model ", model, ".\n",
              "Using bayesnec default priors.")
    }
    priors <- priors[[model]]
    if (is.null(priors)) {
      stop("No valid prior specified.")
    }
  }
  if (!inherits(priors, "brmsprior")) {
    stop("Prior for model ", model, " is not of class brmsprior; see ?bnec for",
         " argument details and example.\n Using bayesnec default priors.")
  }
  priors
}
