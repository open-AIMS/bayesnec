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
#' \donttest{
#' library(brms)
#' library(bayesnec)
#' options(mc.cores = parallel::detectCores())
#' data(nec_data)
#'
#' exmp_a <- bnec(data = nec_data, x_var = "x", y_var = "y",
#'                model = "nec3param",
#'                family = Beta(link = "identity"),
#'                iter = 1e4, control = list(adapt_delta = 0.99))
#' pull_prior(exmp_a)
#' }
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
