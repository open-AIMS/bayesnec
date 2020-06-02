#' define_model
#'
#' Generates model formula and prior model objects to pass to brms
#' 
#' @param x_type the statistical distribution to use for the x (concentration) data. 
#' This may currently be one of  'beta', 'gaussian', or 'gamma'. 
#' Others can be added as required, please contact the package maintainer.
#' 
#' @param y_type the statistical distribution to use for the y (response) data.
#'
#' @param model a character string indicating the model to fit
#'
#' @param mod_dat the model data to use for the NEC model fit
#'
#' @export
#' @return a model formula, priors and the family to use
#' @importFrom brms bf prior_string negbinomial Beta
#' @importFrom stats qlogis binomial quantile Gamma poisson gaussian
define_model <- function(model, x_type, y_type, mod_dat) {
  priors <- prior_string("normal(0, 10)", nlpar = "beta", lb = 0)
  prior_slope <- prior_string("normal(0, 100)", nlpar = "slope", lb = 0)

  if (y_type == "binomial") {
    mod_family <- binomial()
    prior_top <- quantile(qlogis(mod_dat$y / mod_dat$trials), probs = 0.8)
    prior_bot <- quantile(qlogis(mod_dat$y / mod_dat$trials), probs = 0.2)
    prior_ec50 <- quantile(qlogis(mod_dat$y / mod_dat$trials), probs = 0.5)
  }
  if (y_type == "gamma") {
    mod_family <- Gamma()
    prior_top <- quantile(log(mod_dat$y), probs = 0.8)
    prior_bot <- quantile(log(mod_dat$y), probs = 0.2)
    prior_ec50 <- quantile(log(mod_dat$y), probs = 0.5)
  }
  if (y_type == "poisson") {
    mod_family <- poisson()
    prior_top <- quantile(log(mod_dat$y), probs = 0.8)
    prior_bot <- quantile(log(mod_dat$y), probs = 0.2)
    prior_ec50 <- quantile(log(mod_dat$y), probs = 0.5)
  }
  if (y_type == "gaussian") {
    mod_family <- gaussian()
    prior_top <- quantile(mod_dat$y, probs = 0.8)
    prior_bot <- quantile(mod_dat$y, probs = 0.2)
    prior_ec50 <- quantile(mod_dat$y, probs = 0.5)
  }
  if (y_type == "beta") {
    mod_family <- Beta()
    prior_top <- quantile(qlogis(mod_dat$y), probs = 0.8)
    prior_bot <- quantile(qlogis(mod_dat$y), probs = 0.2)
    prior_ec50 <- quantile(qlogis(mod_dat$y), probs = 0.5)
  }
  if (y_type == "negbin") {
    mod_family <- negbinomial()
    prior_top <- quantile(log(mod_dat$y), probs = 0.8)
    prior_bot <- quantile(log(mod_dat$y), probs = 0.2)
    prior_ec50 <- quantile(log(mod_dat$y), probs = 0.5)
  }

  priors <- c(priors,
              prior_string(paste0("normal(", prior_top, ", 5)"),
                           nlpar = "top"))

  if (x_type == "beta") {
    prior_nec <- prior_string("uniform(0.0001, 0.9999)", nlpar = "nec")
  }
  if (x_type == "gamma") {
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
