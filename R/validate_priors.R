#' validate_priors
#'
#' Checks if priors are appropriate to specified model
#'
#' @inheritParams bnec
#'
#' @return A \code{\link[base]{data.frame}} of class
#' \code{\link[brms]{brmsprior}}.
validate_priors <- function(priors, model) {
  if (missing(priors)) {
    return(NULL)
  }
  if (inherits(priors, "list")) {
    if (!model %in% names(priors)) {
      message("Named prior list does not contain ",
              "priors for model ", model, ".\n",
              "Using bayesnec default priors.")
    }
    priors <- priors[[model]]
    if (is.null(priors)) {
      return(NULL)
    }
  }
  if (!inherits(priors, "brmsprior")) {
    message("Prior for model ", model,
            " is not of class brmsprior; ",
            "see ?bnec for argument details ",
            "and example.\n",
            "Using bayesnec default priors.")
    return(NULL)
  }
  priors
}
