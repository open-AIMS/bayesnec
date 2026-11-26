#' pull_prior
#'
#' Extracts the priors from an object of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @param object A fitted \code{bayesnec} object.
#'
#' @details Returns the whole \code{\link[brms]{brmsprior}} each fit carries,
#' \pkg{brms} defaults and duplicated vectorised rows included, which is what to
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
#' @return A one-element list of full \code{brmsprior} objects for a
#' \code{bayesnecfit} or \code{bayesnecjointfit}; a named list by equation
#' for a \code{bayesmanecfit}; or a named list by component or level for
#' a \code{bayesnechurdlefit} or \code{bayesnecgroupfit}.
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
  UseMethod("pull_prior")
}

#' @rdname pull_prior
#' @return A one-element list containing this fit's full prior object.
#' @export
pull_prior.bayesnecfit <- function(object) list(object$fit$prior)

#' @rdname pull_prior
#' @return A named list of full prior objects by equation.
#' @export
pull_prior.bayesmanecfit <- function(object) {
  out <- lapply(object$mod_fits, function(fit) fit$fit$prior)
  names(out) <- vapply(object$mod_fits, `[[`, character(1), "model")
  out
}

#' @rdname pull_prior
#' @return A named list with the growth and survival components' full
#' prior lists.
#' @export
pull_prior.bayesnechurdlefit <- function(object) {
  list(growth = pull_prior(object$growth),
       survival = pull_prior(object$survival))
}

#' @rdname pull_prior
#' @return A named list of full prior lists by level.
#' @export
pull_prior.bayesnecgroupfit <- function(object) {
  group_method(object, NULL, function(fit, lev) pull_prior(fit))
}

#' @rdname pull_prior
#' @return A one-element list containing the joint fit's full prior object.
#' @export
pull_prior.bayesnecjointfit <- function(object) list(object$fit$prior)

#' @rdname pull_prior
#' @return An error naming the supplied unsupported class.
#' @export
pull_prior.default <- function(object) {
  stop("pull_prior() has no method for class ", class(object)[1], ".",
       call. = FALSE)
}
