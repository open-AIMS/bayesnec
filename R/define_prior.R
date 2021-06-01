#' define_prior
#'
#' Generates prior model objects to pass to brms
#'
#' @inheritParams bnec
#'
#' @param predictor The predictor variable for the NEC model fit.
#' @param response The response variable for the NEC model fit.
#'
#' @return A \code{\link[base]{list}} containing the \code{\link[brms]{prior}}
#' and the string defining response distribution family to use.
#' @importFrom brms bf prior_string negbinomial Beta
#' @importFrom stats qlogis binomial quantile Gamma poisson gaussian sd
define_prior <- function(model, family, predictor, response) {
  link_tag <- family$link
  custom_name <- check_custom_name(family)
  if (link_tag %in% c("logit", "log")) {
    fam_tag <- "gaussian"
    if (custom_name == "beta_binomial2") {
      response <- binomial(link = link_tag)$linkfun(response)
    } else {
      if (family$family == "binomial") {
        response <- linear_rescale(response, r_out = c(0.001, 0.999))
      }
      response <- family$linkfun(response)
    }
    response <- response[is.finite(response)]
  } else {
    if (custom_name == "beta_binomial2") {
      fam_tag <- custom_name
    } else {
      fam_tag <- family$family
    }
  }
  if (length(response) == 0) {
    stop("response vector is empty, most likely caused by Inf values",
         " on the inverse link scale (i.e. 0 or 1 data) or for containing NAs",
         " only")
  }
  x_type <- set_distribution(predictor)
  u_t_g <- paste0("gamma(2, ",
                  1 / (quantile(response, probs = 0.75) / 2),
                  ")")
  u_b_g <- paste0("gamma(2, ",
                  1 / ((quantile(response, probs = 0.25) +
                    min(response[response > 0]) / 100) / 2),
                  ")")
  y_t_prs <- c(Gamma = u_t_g,
               poisson = u_t_g,
               negbinomial = u_t_g,
               gaussian = paste0("normal(",
                                 quantile(response, probs = 0.9),
                                 ", ", sd(response) * 2.5, ")"),
               binomial = "beta(5, 1)",
               beta_binomial2 = "beta(5, 1)",
               beta = "beta(5, 1)")
  y_b_prs <- c(Gamma = u_b_g,
               poisson = u_b_g,
               negbinomial = u_b_g,
               gaussian = paste0("normal(",
                                 quantile(response, probs = 0.1),
                                 ", ", sd(response) * 2.5, ")"),
               binomial = "beta(1, 5)",
               beta_binomial2 = "beta(1, 5)",
               beta = "beta(1, 5)")
  x_prs <- c(beta = "beta(2, 2)",
             Gamma = paste0("gamma(5, ",
                            1 / (quantile(predictor,
                                          probs = 0.5) / 2),
                            ")"),
             gaussian = paste0("normal(",
                               quantile(predictor,
                                        probs = 0.5),
                               ", ", sd(predictor) * 2.5, ")"))
  lbs <- c(Gamma = 0, poisson = 0, negbinomial = 0, gaussian = NA,
           binomial = 0, beta_binomial2 = 0, beta = 0)
  ubs <- c(Gamma = NA, poisson = NA, negbinomial = NA, gaussian = NA,
           binomial = 1, beta_binomial2 = 1, beta = 1)
  # y-dependent priors
  pr_top <- prior_string(y_t_prs[fam_tag], nlpar = "top",
                         lb = lbs[fam_tag], ub = ubs[fam_tag])
  pr_bot <- prior_string(y_b_prs[fam_tag], nlpar = "bot",
                         lb = lbs[fam_tag], ub = ubs[fam_tag])
  # x-dependent priors
  x_type <- set_distribution(predictor)
  pr_nec <- prior_string(x_prs[x_type], nlpar = "nec",
                         lb = min(predictor), ub = max(predictor))
  pr_ec50 <- prior_string(x_prs[x_type], nlpar = "ec50",
                          lb = min(predictor), ub = max(predictor))
  # x- and y-independent priors

  pr_d <- prior_string("normal(0, 2)", nlpar = "d")
  pr_beta <- prior_string("normal(0, 2)", nlpar = "beta")
  pr_f <- prior_string("normal(0, 2)", nlpar = "f")
  pr_slope <- prior_string("normal(0, 2)", nlpar = "slope")

  # assemble
  if (model == "ecxsigm") {
    priors <- pr_beta + pr_top + pr_d
  }
  if (model %in% c("ecx4param", "ecxwb1", "ecxwb2", "ecxll4")) {
    priors <- pr_beta + pr_top + pr_bot + pr_ec50
  }
  if (model == "ecxll5") {
    priors <- pr_beta + pr_top + pr_bot + pr_ec50 + pr_f
  }
  if (model %in% c("ecxwb1p3", "ecxwb2p3", "ecxll3")) {
    priors <- pr_beta + pr_top + pr_ec50
  }
  if (model == "neclin") {
    priors <- pr_top + pr_slope + pr_nec
  }
  if (model == "nec3param") {
    priors <- pr_beta + pr_top + pr_nec
  }
  if (model == "nec4param") {
    priors <- pr_beta + pr_top + pr_bot + pr_nec
  }
  if (model %in% c("nechorme", "nechormepwr", "nechormepwr01")) {
    priors <- pr_beta + pr_top + pr_nec + pr_slope
  }
  if (model == "neclinhorme") {
    priors <- pr_beta + pr_top + pr_nec + pr_slope
  }
  if (model %in% c("nechorme4",  "nechorme4pwr")) {
    priors <- pr_beta + pr_top + pr_nec + pr_slope + pr_bot
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
  if (model == "ecxhormebc4") {
    priors <- pr_top + pr_beta + pr_ec50 + pr_slope
  }
  if (model == "ecxhormebc5") {
    priors <- pr_bot + pr_top + pr_beta + pr_ec50 + pr_slope
  }
  priors
}
