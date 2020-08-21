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
  mean_top <- quantile(link_fct(response), probs = 0.8)
  mean_bot <- quantile(link_fct(response), probs = 0.2)

  priors <- prior_string("normal(0, 1)", nlpar = "beta") +
            paste_normal_prior(mean_top, "top", 5)

  x_based_priors <- c(beta = "uniform(0.0001, 0.9999)",
                      Gamma = "normal(0, 1)",
                      gaussian = "normal(3, 1)")
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
              paste_normal_prior(mean_bot, "bot") +
              prior_string(x_based_priors[x_type], nlpar = "ec50")
  }
  if (model == "nec4param") {
    priors <- priors +
              paste_normal_prior(mean_bot, "bot") +
              prior_nec
  }
  if (model == "nechorme") {
    priors <- priors +
              prior_nec +
              paste_normal_prior(0, "slope")
  }
  if (model == "necsigm") {
    priors <- priors +
              prior_nec +
              prior_d
  }
  priors
}
