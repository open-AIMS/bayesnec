#'extract_simdat
#'
#' Extracts a range of statistics from a \code{\link{prebayesnecfit}} object.
#'
#' @param modfit An object of class \code{\link{prebayesnecfit}}, as returned
#' by \code{\link{fit_bayesnec}}.
#'
#' @return A \code{\link[base]{list}} of model simulation statistics including
#' iter, thin, warmup, chains, inits and nsamples.
extract_simdat <- function(modfit) {
  x <- modfit$fit$fit@sim
  list(iter = x$iter,
       thin = x$thin,
       warmup = x$warmup,
       chains = x$chains,
       inits = modfit$fit$fit@inits,
       n_samples = ceiling((x$iter - x$warmup) /
                             x$thin * x$chains))
}
