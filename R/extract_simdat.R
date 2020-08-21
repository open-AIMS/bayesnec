#'extract_simdat
#'
#' Extracts a range of statistics from a list of bayesnecfit model fits.
#'
#' @param modfit a bayesnecfit, as returned by fit_bayesnec
#'
#' @return A list of model simulation statistics including
#' iter, thin, warmup and chains
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
