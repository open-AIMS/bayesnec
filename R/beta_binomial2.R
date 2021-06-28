#' beta_binomial2_lpmf
#'
#' Beta-binomial wrapper LPMF
#'
#' @param y vector of observation successes.
#' @param trials vector of observation trials.
#' @param mu posterior mu.
#' @param phi posterior phi.
#'
#' @importFrom extraDistr dbbinom
#' 
#' @return A \code{\link[base]{numeric}} value or vector containing the probability density of the beta binomial distribution
#'
#' @export
beta_binomial2_lpmf <- function(y, mu, phi, trials) {
  a <- mu * phi
  b <- (1 - mu) * phi
  dbbinom(y, trials, alpha = a, beta = b, log = TRUE)
}

#' beta_binomial2_rng
#'
#' Beta-binomial wrapper RNG
#'
#' @param trials vector of observation trials..
#' @param mu posterior mu.
#' @param phi posterior phi.
#'
#' @importFrom extraDistr rbbinom
#'
#' @return A \code{\link[base]{numeric}} value or vector containing random values of the beta binomial distribution
#'
#' @export
beta_binomial2_rng <- function(mu, phi, trials) {
  a <- mu * phi
  b <- (1 - mu) * phi
  out <- numeric(length = length(a))
  for (i in seq_along(out)) {
    out[i] <- rbbinom(1, trials, alpha = a[i], beta = b[i])
  }
  out
}

#' log_lik_beta_binomial2
#'
#' Beta-binomial wrapper LL
#'
#' @param i observation i.
#' @param prep data with posterior.
#' 
#'
#' @return Log likelihood of the beta binomial distribution
#'
#' @export
log_lik_beta_binomial2 <- function(i, prep) {
  mu <- prep$dpars$mu[, i]
  phi <- prep$dpars$phi
  trials <- prep$data$trials[i]
  y <- prep$data$Y[i]
  beta_binomial2_lpmf(y, mu, phi, trials)
}

#' posterior_predict_beta_binomial2
#'
#' Beta-binomial wrapper posterior_predict method
#'
#' @param i observation i.
#' @param prep data with posterior.
#' @param ... unused.
#' 
#' @return A \code{\link[base]{numeric}} value or vector containing predicted probability values of the beta binomial distribution
#'
#' @export
posterior_predict_beta_binomial2 <- function(i, prep, ...) {
  mu <- prep$dpars$mu[, i]
  phi <- prep$dpars$phi
  trials <- prep$data$trials[i]
  beta_binomial2_rng(mu, phi, trials)
}

#' posterior_epred_beta_binomial2
#'
#' Beta-binomial wrapper posterior_epred method
#'
#' @param prep data with posterior.
#' 
#' @return A \code{\link[base]{numeric}} value or vector containing predicted random values of the beta binomial distribution
#'
#' @export
posterior_epred_beta_binomial2 <- function(prep) {
  mu <- prep$dpars$mu
  trials <- prep$data$trials
  trials <- matrix(trials, nrow = nrow(mu), ncol = ncol(mu), byrow = TRUE)
  mu * trials
}
