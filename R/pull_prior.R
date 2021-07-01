#' pull_prior
#'
#' Extracts the priors from an object of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @param object An object of class \code{\link{bayesnecfit}}
#' or \code{\link{bayesmanecfit}} returned by \code{\link{bnec}}.
#' @return A list containing the priors
#'
#' @examples
#' library(bayesnec)
#' data(manec_example)
#' pull_prior(manec_example)
#'
#' @export
pull_prior <- function(object) {
  if (inherits(object, "bayesmanecfit")) {
    mods <- object$mod_fits
    out <- list()
    for (i in seq_along(mods)) {
      out[[i]] <- mods[[i]]$fit$prior
      names(out)[i] <- mods[[i]]$model
    }
    out
  } else if (inherits(object, "bayesnecfit")) {
    object$fit$prior
  } else {
    stop("Object is not of class bayesnecfit or bayesmanecfit")
  }
}
