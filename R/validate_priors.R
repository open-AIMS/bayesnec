#' Check the class and names of a supplied prior
#' @param priors The supplied prior argument.
#' @param arg The argument name used in an error.
#' @return Invisibly returns \code{NULL} for a valid input.
#' @noRd
check_prior_input <- function(priors, arg = "prior") {
  if (is.null(priors) || inherits(priors, "brmsprior")) {
    return(invisible(NULL))
  }
  named_list <- is.list(priors) && !is.data.frame(priors) &&
    length(priors) > 0 && !is.null(names(priors)) &&
    all(!is.na(names(priors)) & nzchar(names(priors)))
  received <- paste(class(priors), collapse = "/")
  if (named_list) {
    valid <- vapply(priors, inherits, logical(1), "brmsprior")
    if (all(valid)) {
      return(invisible(NULL))
    }
    first_bad <- which(!valid)[1]
    received <- paste0("list with element `", names(priors)[first_bad],
                       "` of class ",
                       paste(class(priors[[first_bad]]), collapse = "/"))
  }
  stop("Argument `", arg, "` must be NULL, a brmsprior, or a named list ",
       "of brmsprior objects; received ", received, ".", call. = FALSE)
}

#' Validate priors for one model
#' @param priors A \code{brmsprior}, a named list of them, or \code{NULL}.
#' @param model The model equation name.
#' @return A \code{brmsprior} for the model, or \code{NULL} when defaults
#' are needed.
#' @noRd
validate_priors <- function(priors, model) {
  check_prior_input(priors)
  if (is.null(priors)) {
    return(NULL)
  }
  if (is.list(priors) && !inherits(priors, "brmsprior")) {
    if (!model %in% names(priors)) {
      message("Named prior list does not contain ",
              "priors for model ", model, ".\n",
              "Using bayesnec default priors.")
      return(NULL)
    }
    priors <- priors[[model]]
  }
  priors
}
