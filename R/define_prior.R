#' define_model
#'
#' Generates model formula and prior model objects to pass to brms
#'
#' @inheritParams bnec
#'
#' @param mod_dat The model data to use for the NEC model fit.
#'
#' @return A \code{\link[base]{list}} containing the \code{\link[brms]{prior}}
#' and the string defining response distribution family to use.
#' @importFrom brms bf prior_string negbinomial Beta
#' @importFrom stats qlogis binomial quantile Gamma poisson gaussian
define_prior <- function(model, x_type, family, mod_dat) {
  priors <- prior_string("normal(0, 10)", nlpar = "beta", lb = 0)
  prior_slope <- prior_string("normal(0, 100)", nlpar = "slope", lb = 0)

  if (family == "binomial") {
    mod_family <- binomial()
    prior_top <- quantile(qlogis(mod_dat$y / mod_dat$trials), probs = 0.8)
    prior_bot <- quantile(qlogis(mod_dat$y / mod_dat$trials), probs = 0.2)
    prior_ec50 <- quantile(qlogis(mod_dat$y / mod_dat$trials), probs = 0.5)
  }
  if (family == "Gamma") {
    mod_family <- Gamma()
    prior_top <- quantile(log(mod_dat$y), probs = 0.8)
    prior_bot <- quantile(log(mod_dat$y), probs = 0.2)
    prior_ec50 <- quantile(log(mod_dat$y), probs = 0.5)
  }
  if (family == "poisson") {
    mod_family <- poisson()
    prior_top <- quantile(log(mod_dat$y), probs = 0.8)
    prior_bot <- quantile(log(mod_dat$y), probs = 0.2)
    prior_ec50 <- quantile(log(mod_dat$y), probs = 0.5)
  }
  if (family == "gaussian") {
    mod_family <- gaussian()
    prior_top <- quantile(mod_dat$y, probs = 0.8)
    prior_bot <- quantile(mod_dat$y, probs = 0.2)
    prior_ec50 <- quantile(mod_dat$y, probs = 0.5)
  }
  if (family == "Beta") {
    mod_family <- Beta()
    prior_top <- quantile(qlogis(mod_dat$y), probs = 0.8)
    prior_bot <- quantile(qlogis(mod_dat$y), probs = 0.2)
    prior_ec50 <- quantile(qlogis(mod_dat$y), probs = 0.5)
  }
  if (family == "negbinomial") {
    mod_family <- negbinomial()
    prior_top <- quantile(log(mod_dat$y), probs = 0.8)
    prior_bot <- quantile(log(mod_dat$y), probs = 0.2)
    prior_ec50 <- quantile(log(mod_dat$y), probs = 0.5)
  }

  priors <- c(priors,
              prior_string(paste0("normal(", prior_top, ", 5)"),
                           nlpar = "top"))

  if (x_type == "Beta") {
    prior_nec <- prior_string("uniform(0.0001, 0.9999)", nlpar = "nec")
  }
  if (x_type == "Gamma") {
    prior_nec <- prior_string("normal(0, 100)", nlpar = "nec", lb = 0)
  }
  if (x_type == "gaussian") {
    prior_nec <- prior_string("normal(3, 100)", nlpar = "nec")
  }

  if (model == "nec3param") {
    priors <- priors + prior_nec
  }
  if (model == "ecxsigm") {
    priors <- priors + prior_string("normal(0, 100)", nlpar = "d")
  }
  if (model == "ecx4param") {
    priors <- c(priors,
                prior_string(paste0("normal(",
                                    prior_bot,
                                    ", 100)"),
                             nlpar = "bot"),
                prior_string(paste0("normal(",
                                    prior_ec50,
                                    ", 100)"),
                             nlpar = "ec50"))
  }
  if (model == "nec4param") {
    priors <- c(priors,
                prior_string(paste0("normal(",
                                    prior_bot,
                                    ", 100)"),
                              nlpar = "bot"),
                prior_nec)
  }
  if (model == "ecxwb1") {
    priors <- c(priors,
                prior_string(paste0("normal(",
                                    prior_bot,
                                    ", 100)"),
                             nlpar = "bot"),
                prior_string(paste0("normal(",
                                    prior_ec50,
                                    ", 100)"),
                             nlpar = "ec50"))
  }
  if (model == "ecxwb2") {
    priors <- c(priors,
                prior_string(paste0("normal(",
                                    prior_bot,
                                    ", 100)"),
                             nlpar = "bot"),
                prior_string(paste0("normal(",
                                    prior_ec50,
                                    ", 100)"),
                             nlpar = "ec50"))
  }
  if (model == "nechorme") {
    priors <- c(priors, prior_nec, prior_slope)
  }
  if (model == "necsigm") {
    priors <- priors + prior_nec + prior_string("normal(0, 100)", nlpar = "d")
  }

  list(priors = priors, mod_family = mod_family)
}
