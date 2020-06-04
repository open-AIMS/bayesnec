#'extract_simdat
#'
#' Extracts a range of statistics from a list of bayesnecfit model fits.
#'
#' @param modfit a bayesnecfit, as returned by fit_bayesnec
#'
#' @return A list of model simulation statistics including
#' iter, thin, warmup and chains
extract_simdat <- function(modfit) {
  list(iter = modfit$fit$fit@sim$iter,
       thin = modfit$fit$fit@sim$thin,
       warmup = modfit$fit$fit@sim$warmup,
       chains = modfit$fit$fit@sim$chains)
}
