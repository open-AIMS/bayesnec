#' define_model
#'
#' Generates model formula and prior model objects to pass to brms
#'
#' @inheritParams bnec
#'
#' @param response The response variable for the NEC model fit.
#'
#' @return A \code{\link[base]{list}} containing the \code{\link[brms]{prior}}
#' and the string defining response distribution family to use.
#' @importFrom brms bf prior_string negbinomial Beta
#' @importFrom stats qlogis binomial quantile Gamma poisson gaussian
define_prior <- function(model, x_type, family, response) {
  link_fct <- family$linkfun
  prior_top <- quantile(link_fct(response), probs = 0.8)
  prior_bot <- quantile(link_fct(response), probs = 0.2)
  prior_ec50 <- quantile(link_fct(response), probs = 0.5)

  priors <- prior_string("normal(0, 10)", nlpar = "beta", lb = 0) +
            paste_normal_prior(prior_top, "top", 5)

  x_based_priors <- c(beta = "uniform(0.0001, 0.9999)",
                      Gamma = "normal(0, 100)",
                      gaussian = "normal(3, 100)")
  prior_nec <- prior_string(x_based_priors[x_type], nlpar = "nec")
  prior_d <- paste_normal_prior(0, "d")

  if (model == "nec3param") {
    priors <- priors + prior_nec
  }
  if (model == "ecxsigm") {
    priors <- priors + prior_d
  }
  if (model %in% c("ecx4param", "ecxwb1", "ecxwb2")) {
    priors <- priors +
              paste_normal_prior(prior_bot, "bot") +
              paste_normal_prior(prior_ec50, "ec50")
  }
  if (model == "nec4param") {
    priors <- priors +
              paste_normal_prior(prior_bot, "bot") +
              prior_nec
  }
  if (model == "nechorme") {
    priors <- priors +
              prior_nec +
              paste_normal_prior(0, "slope", lb = 0)
  }
  if (model == "necsigm") {
    priors <- priors +
              prior_nec +
              prior_d
  }
  priors
}
