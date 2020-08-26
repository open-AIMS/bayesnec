#' define_model
#'
#' Generates model formula and prior model objects to pass to brms
#'
#' @inheritParams bnec
#'
#' @param predictor The predictor variable for the NEC model fit.
#' @param response The response variable for the NEC model fit.
#'
#' @return A \code{\link[base]{list}} containing the \code{\link[brms]{prior}}
#' and the string defining response distribution family to use.
#' @importFrom brms bf prior_string negbinomial Beta
#' @importFrom stats qlogis binomial quantile Gamma poisson gaussian
define_prior <- function(model, family, predictor, response) {
  family <- family$family
  x_type <- set_distribution(predictor)
  u_t_g <- paste0("gamma(2, ",
                  1 / (quantile(response, probs = 0.75) / 2),
                  ")")
  u_b_g <- paste0("gamma(2, ",
                  1 / ((quantile(response, probs = 0.25)+min(response[response > 0])/100) / 2),
                  ")")
  y_t_prs <- c(Gamma = u_t_g,
               poisson = u_t_g,
               negbinomial = u_t_g,
               gaussian = paste0("normal(",
                                 quantile(response, probs = 0.75),
                               ", ", sd(response), ")"),
               binomial = "beta(2, 1)",
               beta = "beta(2, 1)")
  y_b_prs <- c(Gamma = u_b_g,
               poisson = u_b_g,
               negbinomial = u_b_g,
               gaussian = paste0("normal(",
                                 quantile(response, probs = 0.25),
                               ", ", sd(response),")"),
               binomial = "beta(1, 2)",
               beta = "beta(1, 2)")

  x_prs <- c(beta = "beta(2, 2)",
             Gamma = paste0("gamma(2, ",
                            1 / (quantile(predictor,
                                          probs = 0.5) / 2),
                            ")"),
             gaussian = paste0("normal(",
                               quantile(predictor,
                                        probs = 0.5),
                               ", ", sd(predictor), ")"))
  # y-dependent priors
  pr_top <- prior_string(y_t_prs[family], nlpar = "top")
  pr_bot <- prior_string(y_b_prs[family], nlpar = "bot")
  # x-dependent priors
  x_type <- set_distribution(predictor)
  pr_nec <- prior_string(x_prs[x_type], nlpar = "nec")
  pr_ec50 <- prior_string(x_prs[x_type], nlpar = "ec50")
  # x- and y-independent priors
  pr_d <- prior_string("normal(0, 1)", nlpar = "d")
  
  pr_beta <- prior_string("gamma(0.5, 2)", nlpar = "beta")
  
  # scaling dependent priors
  pr_slope <- prior_string(paste0("gamma(2, ",
                                  1 / ((diff(range(response))/diff(range(predictor))) / 2),
                                  ")"), nlpar = "slope")

  # assemble
  if (model == "ecxsigm") {
    priors <- pr_beta + pr_top + pr_d
  }
  if (model %in% c("ecx4param", "ecxwb1", "ecxwb2")) {
    priors <- pr_beta + pr_top + pr_bot + pr_ec50
  }
  if (model == "nec3param") {
    priors <- pr_beta + pr_top + pr_nec
  }
  if (model == "nec4param") {
    priors <- pr_beta + pr_top + pr_bot + pr_nec
  }
  if (model == "nechorme") {
    priors <- pr_beta + pr_top + pr_nec + pr_slope
  }
  if (model == "necsigm") {
    priors <- pr_beta + pr_top + pr_nec + pr_d
  }
  if (model == "ecxlin") {
    priors <- pr_slope + pr_top
  }  
  if (model == "ecxexp") {
    priors <- pr_beta + pr_top
  } 
  priors
}
