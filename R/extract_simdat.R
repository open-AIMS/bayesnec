#'extract_simdat
#'
#' Extracts a range of statistics from a \code{\link{prebayesnecfit}} object.
#'
#' @param modfit An object of class \code{\link{prebayesnecfit}}, as returned
#' by \code{\link{fit_bayesnec}}.
#'
#' @return A \code{\link[base]{list}} of model simulation statistics: iter,
#' thin, warmup, chains, init and sample_prior. Count the draws a fit holds
#' with \code{set_draw_count()}, not from these settings.
#'
#' @noRd
extract_simdat <- function(modfit) {
  x <- modfit$fit$fit@sim
  sample_prior <- attr(modfit$fit$prior, "sample_prior")
  list(iter = x$iter, thin = x$thin, warmup = x$warmup, chains = x$chains,
       init = modfit$fit$fit@inits, sample_prior = sample_prior)
}

#' The number of posterior draws available from every model of a set
#'
#' Counts the draws the fits hold, as \code{posterior_epred()} and
#' \code{posterior_predict()} do, so that a stored \code{sample_size} matches
#' the count those methods find.
#'
#' @param mod_fits A \code{\link[base]{list}} of objects that each hold a
#' \code{\link[brms]{brmsfit}} as \code{fit}.
#'
#' @return A \code{\link[base]{numeric}} of length 1, the smallest number of
#' draws held by any model.
#'
#' @importFrom brms ndraws
#'
#' @noRd
set_draw_count <- function(mod_fits) {
  min(vapply(mod_fits, function(x) ndraws(x$fit), numeric(1)))
}
