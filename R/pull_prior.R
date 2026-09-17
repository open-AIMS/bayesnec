#' pull_prior
#'
#' Extracts the priors from an object of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @param object An object of class \code{\link{bayesnecfit}}
#' or \code{\link{bayesmanecfit}} returned by \code{\link{bnec}}.
#'
#' @details Returns the whole \code{\link[brms]{brmsprior}} each fit carries,
#' \pkg{brms} defaults and duplicated vectorized rows included, which is what to
#' use for looking at a fit. It is not accepted by \code{bnec(prior = )} --
#' \code{\link{get_priors}} is the function for feeding priors back in.
#'
#' Before reusing these rows in a \pkg{brms} formula that adds population-level
#' coefficients to a non-linear parameter, check whether the coefficients have
#' the same meaning and support. A prior that names the parameter but no
#' coefficient applies to every added coefficient, including any contrasts.
#' Rebuild the priors where necessary as described in
#' \code{\link{get_priors}}.
#'
#' @return A \code{\link[base]{list}} containing the priors.
#'
#' @seealso \code{\link{get_priors}}, \code{\link{check_priors}}
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
    list(object$fit$prior)
  } else {
    stop("Object is not of class bayesnecfit or bayesmanecfit")
  }
}
