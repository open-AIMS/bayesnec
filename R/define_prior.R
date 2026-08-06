#' define_prior
#'
#' Generates prior model objects to pass to \pkg{brms}
#'
#' @inheritParams bnec
#'
#' @param family A \code{\link[stats]{family}} function.
#' @param predictor The predictor variable for the NEC model fit.
#' @param response The response variable for the NEC model fit.
#'
#' @return An object of class \code{\link[brms]{brmsprior}}.
#' @importFrom brms prior_string
#' @importFrom stats sd
#'
#' @noRd
define_prior <- function(model, family, predictor, response,
                         prior_type = "uninformative",
                         ymax = NULL, u_loc = NULL, u_scale = NULL,
                         model_survival = NULL) {
  prior_type <- match.arg(prior_type, c("uninformative", "regularizing"))
  if (is_hurdle_family(family)) {
    return(define_hurdle_prior(model, family, predictor, response,
                               prior_type = prior_type,
                               model_survival = model_survival))
  }
  link_tag <- family$link
  custom_name <- check_custom_name(family)
  if (link_tag %in% c("logit", "log")) {
    fam_tag <- "gaussian"
  } else {
    fam_tag <- family_tag(family)
   }
  # The mu block of a beta_ub fit is an ordinary positive, natural-scale
  # response model, so it takes the existing Gamma/identity defaults. Only the
  # ceiling needs a prior of its own, appended at the end.
  beta_ub_fit <- is_beta_ub_family(family)
  if (beta_ub_fit) {
    fam_tag <- "Gamma"
  }
  if (family$family == "beta_binomial" || family$family == "binomial") {
    if (is.integer(response) || max(response) > 1) {
      stop("Response vector must be passed as a proportion to define_prior",
           " (not as integers) for the binomial and beta_binomial families.")
    }
  }
  response <- response_link_scale(response, family)
  x_type <- set_distribution(predictor, silence_y_msgs = TRUE,
                             silence_x_msgs = FALSE)
  # Two prior sets for the response-scaled parameters (top, bot):
  #  - "uninformative": the weakly-informative defaults described in the JSS
  #    article (Fisher et al. 2024); wider, closer to truly uninformative.
  #  - "regularizing": narrower priors, with the no-effect (top) parameter
  #    centred on the control mean -- which, for these monotonically decreasing
  #    models, sits at the upper end of the response range.
  # Only the response-scaled (top/bot) priors differ between the two sets; the
  # predictor-scaled and fixed priors below are shared.
  if (prior_type == "uninformative") {
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
                 bernoulli = "beta(5, 2)",
                 binomial = "beta(5, 2)",
                 "beta_binomial" = "beta(5, 2)",
                 beta = "beta(5, 2)")
    y_b_prs <- c(Gamma = u_b_g,
                 poisson = u_b_g,
                 negbinomial = u_b_g,
                 gaussian = paste0("normal(",
                                   quantile(response, probs = 0.1),
                                   ", ", sd(response) * 2.5, ")"),
                 bernoulli = "beta(2, 5)",
                 binomial = "beta(2, 5)",
                 "beta_binomial" = "beta(2, 5)",
                 beta = "beta(2, 5)")
  } else {
    u_t_g <- paste0("gamma(5, ",
                    5 / (quantile(response, probs = 1)),
                    ")")
    u_b_g <- paste0("gamma(5, ",
                    5 / ((quantile(response, probs = 0) +
                      min(response[response > 0]) / 10)),
                    ")")
    y_t_prs <- c(Gamma = u_t_g,
                 poisson = u_t_g,
                 negbinomial = u_t_g,
                 gaussian = paste0("normal(",
                                   quantile(response, probs = 1),
                                   ", ", sd(response), ")"),
                 bernoulli = "beta(5, 1)",
                 binomial = "beta(5, 1)",
                 "beta_binomial" = "beta(5, 1)",
                 beta = "beta(5, 1)")
    y_b_prs <- c(Gamma = u_b_g,
                 poisson = u_b_g,
                 negbinomial = u_b_g,
                 gaussian = paste0("normal(",
                                   quantile(response, probs = 0),
                                   ", ", sd(response), ")"),
                 bernoulli = "beta(1, 5)",
                 binomial = "beta(1, 5)",
                 "beta_binomial" = "beta(1, 5)",
                 beta = "beta(1, 5)")
  }
  x_prs <- c(Beta = "beta(2, 2)",
             Gamma = paste0("gamma(5, ",
                            1 / (quantile(predictor,
                                          probs = 0.5) / 2),
                            ")"),
             gaussian = paste0("normal(",
                               quantile(predictor,
                                        probs = 0.5),
                               ", ", sd(predictor) * 10, ")"))
  lbs <- c(Gamma = 0, poisson = 0, negbinomial = 0, gaussian = NA,
           bernoulli = 0, binomial = 0, "beta_binomial" = 0, beta = 0)
  ubs <- c(Gamma = NA, poisson = NA, negbinomial = NA, gaussian = NA,
           bernoulli = 1, binomial = 1, "beta_binomial" = 1, beta = 1)
  # y-dependent priors
  pr_top <- prior_string(y_t_prs[fam_tag], nlpar = "top",
                         lb = lbs[fam_tag], ub = ubs[fam_tag])
  pr_bot <- prior_string(y_b_prs[fam_tag], nlpar = "bot",
                         lb = lbs[fam_tag], ub = ubs[fam_tag])
  # x-dependent priors
  pr_nec <- prior_string(x_prs[x_type], nlpar = "nec",
                         lb = min(predictor), ub = max(predictor))
  pr_ec50 <- prior_string(x_prs[x_type], nlpar = "ec50",
                          lb = min(predictor), ub = max(predictor))
  # x- and y-independent priors
  pr_d <- prior_string("normal(0, 5)", nlpar = "d")
  pr_beta <- prior_string("normal(0, 5)", nlpar = "beta")
  pr_f <- prior_string("normal(0, 5)", nlpar = "f")
  pr_slope <- prior_string("normal(0, 5)", nlpar = "slope")
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
  if (beta_ub_fit) {
    priors <- priors + define_delta_prior(response, ymax, u_loc, u_scale)
  }
  priors
}

#' define_delta_prior
#'
#' The prior on the beta_ub ceiling, expressed as a prior on \code{delta}.
#'
#' @param response The response variable for the model fit.
#' @param ymax The largest observed response, as recorded by
#' \code{\link{check_data}}.
#' @param u_loc,u_scale Location and scale of the prior on the ceiling
#' \code{U}, elicited from the biology or from historical controls.
#'
#' @details \code{U = ymax + delta} with \code{delta > 0}, and the map is a pure
#' location shift with Jacobian 1, so a prior \code{normal(U_loc, U_scale)} on
#' \code{U} truncated to \code{U > ymax} is exactly
#' \code{normal(U_loc - ymax, U_scale)} on \code{delta} with \code{lb = 0}.
#' Stating it on \code{U} is the point: a prior placed directly on \code{delta}
#' centres the ceiling just above the sample maximum, so it moves with \code{n}
#' and with the noise in a single extreme order statistic.
#'
#' When neither \code{u_loc} nor \code{u_scale} is given there is nothing to
#' shift, and the fallback is exactly the half-normal-on-\code{delta} that the
#' paragraph above argues against. That is why it messages rather than passing
#' silently: the Phase 0 study found the posterior for \code{U} equal to its
#' prior to three decimal places whenever the curve did not approach the
#' ceiling, so on this path every statement about \code{U} is one the prior
#' made.
#'
#' @return An object of class \code{\link[brms]{brmsprior}}.
#'
#' @importFrom brms prior_string
#'
#' @noRd
define_delta_prior <- function(response, ymax = NULL, u_loc = NULL,
                               u_scale = NULL) {
  if (is.null(ymax)) {
    ymax <- max(response, na.rm = TRUE)
  }
  if (is.null(u_loc) && is.null(u_scale)) {
    # Scale chosen so that U sits mostly within (ymax, 1.5 * ymax): wide enough
    # to be weakly informative, tight enough that top/U stays in the region
    # where Phase 0 found the family behaves.
    scale <- ymax / 4
    message("No U_loc or U_scale supplied, so the beta_ub ceiling is",
            " prior-driven: it falls back to a half-normal on delta with",
            " scale ", signif(scale, 3), ", which centres U just above the",
            " largest observed response. That is the very thing an elicited",
            " ceiling avoids -- it makes U move with the sample size and with",
            " the noise in a single extreme value. Read intervals at the top",
            " of the curve accordingly, and prefer supplying U_loc and U_scale",
            " from the biology or from historical controls. See ?beta_ub.")
    return(prior_string(paste0("normal(0, ", scale, ")"), class = "delta",
                        lb = 0))
  }
  if (is.null(u_loc) || is.null(u_scale)) {
    stop("Supply both U_loc and U_scale, or neither. You gave ",
         if (is.null(u_loc)) "U_scale" else "U_loc", " only.", call. = FALSE)
  }
  if (!is.numeric(u_loc) || length(u_loc) != 1 || !is.finite(u_loc) ||
        !is.numeric(u_scale) || length(u_scale) != 1 || !is.finite(u_scale) ||
        u_scale <= 0) {
    stop("U_loc must be a single finite number and U_scale a single positive",
         " number.", call. = FALSE)
  }
  if (u_loc <= ymax) {
    # Not fatal -- the truncation still leaves a proper posterior -- but the
    # whole prior sits in the rejected region, so U is pinned just above ymax
    # and its posterior means nothing. Phase 0 reproduced this deliberately:
    # coverage of U fell to 0.35 while NEC and ECx moved less than 2%.
    warning("U_loc (", signif(u_loc, 4), ") is not greater than the largest",
            " observed response (", signif(ymax, 4), "), so the prior on the",
            " ceiling lies entirely in the region the likelihood rejects.",
            " U will be pinned just above the data and its posterior should",
            " not be interpreted. Either the elicited ceiling is wrong or the",
            " data contain something unexpected.", call. = FALSE)
  }
  prior_string(paste0("normal(", u_loc - ymax, ", ", u_scale, ")"),
               class = "delta", lb = 0)
}

#' define_hurdle_prior
#'
#' Builds priors for both parameter blocks of a joint hurdle fit.
#'
#' @inheritParams define_prior
#'
#' @details The two blocks are primed from different views of the same data.
#' The mu block is a zero-bounded response model and is given the existing
#' Gamma/identity defaults, but computed from the **survivors only** --
#' including the zeros would drag the \code{top} and \code{bot} quantiles well
#' below the real control mean. The hu block is a probability and is given the
#' existing bernoulli/identity defaults, which are already 0-1 bounded.
#'
#' The hu sub-model is written as \code{1 - survival}, so its \code{hutop} is
#' control *survival* and takes the same \code{beta(5, 2)} style prior that a
#' bernoulli fit would use.
#'
#' @return An object of class \code{\link[brms]{brmsprior}}.
#'
#' @importFrom stats Gamma
#' @importFrom brms prior_string
#'
#' @noRd
define_hurdle_prior <- function(model, family, predictor, response,
                                prior_type = "uninformative",
                                model_survival = NULL) {
  dpar <- hurdle_dpar(family)
  # The second block may carry a different equation from the response block,
  # in which case its priors must be built for that equation's parameters.
  if (is.null(model_survival)) {
    model_survival <- model
  }
  parts <- split_hurdle_response(predictor, response)
  # mu block: reuse the defaults of whatever the non-zero response looks like
  # (Gamma for hurdle_gamma, Beta for zero_inflated_beta), built from the
  # non-zeros only -- including the zeros would drag the top and bot quantiles
  # well below the real control level.
  mu_priors <- define_prior(model, hurdle_mu_family(family),
                            parts$mu$x, parts$mu$y, prior_type = prior_type)
  # second block: reuse the bernoulli/identity defaults on the proportion
  # non-zero, then rename every non-linear parameter into its namespace.
  hu_priors <- define_prior(model_survival, bernoulli(link = "identity"),
                            parts$hu$x, parts$hu$y, prior_type = prior_type)
  hu_priors$nlpar <- ifelse(nzchar(hu_priors$nlpar),
                            paste0(dpar, hu_priors$nlpar), hu_priors$nlpar)
  # Both blocks are evaluated over the *whole* predictor range inside the joint
  # fit, but each was primed from a subset of it: mu from non-zeros only (which
  # stop short of the concentrations where everything is zero) and the second
  # block from the deduplicated unique-x vector. Rebuild the predictor-scaled
  # bounds from the full predictor so neither threshold is boxed out of the
  # range it must cover.
  rebound <- function(prs, pars) {
    is_pred <- prs$nlpar %in% pars
    if (any(is_pred)) {
      prs$lb[is_pred] <- as.character(min(predictor))
      prs$ub[is_pred] <- as.character(max(predictor))
    }
    prs
  }
  mu_priors <- rebound(mu_priors, c("nec", "ec50"))
  hu_priors <- rebound(hu_priors, paste0(dpar, c("nec", "ec50")))
  mu_priors + hu_priors
}
