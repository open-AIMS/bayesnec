#' Quantile of a response on the scale of its positive part
#'
#' The gamma priors for "top" and "bot" set their rate from a quantile of the
#' response. Where a share of the response is exactly zero those quantiles are
#' pulled down towards zero, and the prior collapses onto a scale that has
#' nothing to do with the asymptote it is meant to locate. That is not a rare
#' corner: the zero-inflated count families added under #104 exist precisely for
#' responses where a quarter or more of the values are zero.
#'
#' \strong{Why the probability is rescaled rather than the vector.} A quantile
#' of a zero-inflated response is a quantile of the \emph{mixture}, and it is
#' biased downward by the zero fraction throughout its range -- reaching exactly
#' zero only at the extreme. So testing for an exactly-zero quantile and
#' substituting the positive part, as the first version of this guard did, is a
#' step function applied to a continuous problem: it left the worst case, just
#' below the threshold, entirely unguarded. On a `nec4param` response with a true
#' `top` of 40 the `top` prior mean ran 33.5 at no zeros, 6.8 at 72\% zeros, and
#' then jumped back to 29.0 at 76\% once the raw quantile finally hit zero.
#'
#' Level \code{p} of the distribution conditional on being positive sits at level
#' \code{z + (1 - z)p} of the mixture, where \code{z} is the zero fraction: a
#' share \code{z} of the mass has to be passed before any positive value is
#' reached. Equivalently \code{1 - (1 - p)(1 - z)}. That is the quantile taken
#' here. It reduces to the raw quantile exactly when \code{z} is zero, so a
#' response with no zeros is untouched, and it degrades smoothly rather than in a
#' jump.
#'
#' Note the multiplication. The dividing form \code{1 - (1 - p) / (1 - z)} moves
#' the level the wrong way -- it returns 0.5 where the answer is 0.875 at
#' \code{p = 0.75, z = 0.5} -- and goes negative past 75\% zeros, which is the
#' regime this exists for.
#'
#' Deliberately not the same trick as \code{define_hurdle_prior()}, which
#' computes the whole mu-block prior from the non-zero subset. That is exact for
#' \code{hurdle_gamma}, because a Gamma has no mass at zero, so the non-zero
#' subset \emph{is} the mu process. Under zero-inflation it is not: the base
#' distribution emits zeros of its own, so conditioning on the positives draws
#' from a truncated count distribution and biases the location upward. Here the
#' positive part informs only a \emph{scale}, never the estimate itself.
#'
#' See #210 for the original three failure modes and #232 for why the guard
#' became a rescaling.
#'
#' @param response A \code{\link[base]{numeric}} vector.
#' @param probs A \code{\link[base]{numeric}} vector of length 1.
#'
#' @return A \code{\link[base]{numeric}} vector of length 1, strictly positive.
#'
#' @importFrom stats quantile
#'
#' @noRd
positive_scale <- function(response, probs) {
  finite <- response[is.finite(response)]
  pos <- finite[finite > 0]
  if (!length(pos)) {
    stop("Cannot build priors for \"top\" and \"bot\": the response contains no",
         " positive values, so there is no scale to place them on. Check the",
         " response variable, and see ?bnec for the families bayesnec supports.",
         call. = FALSE)
  }
  zero_frac <- sum(finite <= 0) / length(finite)
  # z == 0 leaves probs untouched, so a response with no zeros gets exactly the
  # quantile it got before this guard existed.
  probs_adj <- 1 - (1 - probs) * (1 - zero_frac)
  q <- unname(quantile(finite, probs = probs_adj))
  # The rescaled level can still land on a zero: at probs = 0 it stays at 0, and
  # ties across the boundary can put it there too. min(pos) is the smallest
  # scale the data actually supports, and is what the caller's fudge terms are
  # already built around.
  if (!is.finite(q) || q <= 0) {
    return(min(pos))
  }
  q
}

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
                         model_survival = NULL, disp_spec = NULL,
                         group_spec = NULL) {
  prior_type <- match.arg(prior_type, c("uninformative", "regularizing"))
  if (is_hurdle_family(family)) {
    hurdle_priors <- define_hurdle_prior(model, family, predictor, response,
                                         prior_type = prior_type,
                                         model_survival = model_survival)
    # A group-level term reaches the mu block only. add_formula_glef() runs
    # before the hu sub-formulas are attached, so `ogl` and `pgl` never see
    # them -- checked against the formula wrangle_model_formula() actually
    # builds, not assumed. So the same priors apply here, on the same nlpar
    # names, and the hu block needs nothing. Without this the hurdle families
    # kept the whole of #245: they take the early return above, so group_spec
    # was ignored for exactly the fits vignette("example8") part 3 needs.
    # Scaled from the survivors only, which is the response the mu block is
    # actually fitted to -- including the structural zeros would drag the scale
    # down for the same reason define_hurdle_prior() excludes them from top and
    # bot. Put on the link scale of the mu family before it is measured, so
    # that this branch and the one below both take the scale from the quantity
    # the offsets are actually added to rather than differing over a step that
    # is a no-op only for as long as bnec() forces the identity link.
    mu_family <- hurdle_mu_family(family)
    mu_response <- response_link_scale(
      split_hurdle_response(predictor, response)$mu$y, mu_family
    )
    group_priors <- define_group_prior(group_spec, predictor, mu_response,
                                       prior_type = prior_type)
    if (!is.null(group_priors)) {
      hurdle_priors <- hurdle_priors + group_priors
    }
    return(hurdle_priors)
  }
  link_tag <- family$link
  custom_name <- check_custom_name(family)
  if (link_tag %in% c("logit", "log")) {
    fam_tag <- "gaussian"
  } else { 
    fam_tag <- family$family
   }
  # The mu block of a zero-inflated count family is an ordinary poisson or
  # negbinomial mean -- the mixture changes how many zeros are observed, not the
  # scale of mu -- so the base family's priors are the right ones rather than a
  # duplicated set of entries in every table below.
  #
  # The quantiles below are taken over the whole response, structural zeros
  # included. That used to collapse the `top` and `bot` priors once a large
  # share of the response was zero -- the regime these families exist for --
  # and is now guarded by positive_scale(), which falls back to the same
  # quantile of the positive part. See its documentation for why that is not
  # the same trick define_hurdle_prior() uses, and #210 for what the three
  # failure modes were.
  if (fam_tag %in% c("zero_inflated_poisson", "zero_inflated_negbinomial")) {
    fam_tag <- sub("^zero_inflated_", "", fam_tag)
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
  # Only these three families read u_t_g / u_b_g out of the tables below; every
  # other entry is a literal or is built from quantile()/sd() directly, and is
  # well defined on a response that is entirely negative or entirely zero. So
  # the gamma-scaled strings are built only when they will be used. Building
  # them unconditionally made positive_scale()'s "no positive values" error --
  # and the unguarded min(response[response > 0]) beside it -- reachable for
  # gaussian, where an all-negative response (log ratios, growth increments,
  # anything expressed as a change) is ordinary input. See #229.
  gamma_scaled <- fam_tag %in% c("Gamma", "poisson", "negbinomial")
  if (prior_type == "uninformative") {
    u_t_g <- u_b_g <- NA_character_
    if (gamma_scaled) {
      u_t_g <- paste0("gamma(2, ",
                      1 / (positive_scale(response, probs = 0.75) / 2),
                      ")")
      u_b_g <- paste0("gamma(2, ",
                      1 / ((positive_scale(response, probs = 0.25) +
                        min(response[response > 0]) / 100) / 2),
                      ")")
    }
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
    u_t_g <- u_b_g <- NA_character_
    if (gamma_scaled) {
      u_t_g <- paste0("gamma(5, ",
                      5 / (positive_scale(response, probs = 1)),
                      ")")
      # probs = 0 is the minimum, which is zero for a response containing a
      # single zero -- not merely for a mostly-zero one. So under
      # "regularizing" the collapse was unconditional on the zero fraction,
      # where under "uninformative" it needed a quarter of the response to be
      # zero.
      u_b_g <- paste0("gamma(5, ",
                      5 / ((positive_scale(response, probs = 0) +
                        min(response[response > 0]) / 10)),
                      ")")
    }
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
  disp_priors <- define_disp_prior(disp_spec, family, response)
  if (!is.null(disp_priors)) {
    priors <- priors + disp_priors
  }
  # response is on the link scale by this point, which is what the group-level
  # offsets are added on, so it is the right scale to take the prior from.
  group_priors <- define_group_prior(group_spec, predictor, response,
                                     prior_type = prior_type)
  if (!is.null(group_priors)) {
    priors <- priors + group_priors
  }
  priors
}

#' define_disp_prior
#'
#' Builds priors for the non-linear parameters a variance function introduces.
#'
#' @param disp_spec The output of \code{\link{parse_disp_term}}.
#' @param family A \code{\link[stats]{family}} function.
#' @param response The response variable, already on the link scale.
#'
#' @details Only route (B) is given priors here. Route (A) is an ordinary
#' distributional formula and is left to the \pkg{brms} defaults, which already
#' suit a linear predictor on a log link.
#'
#' \code{c1} and \code{c2} are centred on zero, which is the constant-dispersion
#' case, so the prior asserts no mean-variance relationship and lets the data
#' supply one. \code{c0} is the dispersion parameter on the log scale at the
#' variance function's reference value (see \code{\link{disp_centre}}) -- that
#' is, at a typical response rather than at \code{mu = 1}. That is what makes
#' these priors mean anything at all: uncentred, \code{c0} and the slope are
#' near-perfectly confounded and the induced prior on the dispersion parameter
#' at the data spans many orders of magnitude whenever the response is far from
#' one. The scale is still deliberately loose, because the reference locates the
#' intercept but says nothing about how large the dispersion there should be.
#'
#' @return An object of class \code{\link[brms]{brmsprior}}, or \code{NULL}.
#'
#' @importFrom brms prior_string
#' @importFrom stats sd
#'
#' @noRd
define_disp_prior <- function(disp_spec, family, response) {
  if (is.null(disp_spec) || disp_spec$route != "B") {
    return(NULL)
  }
  fam_tag <- family$family
  c0_prs <- c(
    gaussian = paste0("normal(", round(log(sd(response)), 3), ", 2)"),
    # shape is an inverse dispersion for both of these: a CV of 0.1 to 0.5 puts
    # a Gamma shape between about 4 and 100, i.e. 1.4 to 4.6 on the log scale.
    Gamma = "normal(2, 2)",
    negbinomial = "normal(2, 2)",
    # phi likewise, on the wider scale the PAM fits needed.
    beta = "normal(4, 3)",
    beta_binomial = "normal(4, 3)"
  )
  vf <- disp_functions[[disp_spec$value]]
  # A slope on log(mu) is dimensionless, so a fixed scale means the same thing
  # whatever the response is measured in. A slope on mu itself is not: it
  # carries units of 1/response, and normal(0, 2) would be near-flat for a
  # response spanning thousands and highly informative for one spanning a
  # fraction. Scaling by the observed spread restores the intended meaning --
  # that a one-standard-deviation change in the mean moves the dispersion
  # parameter by about two units on the log scale at the edge of the prior.
  slope_prior <- if (isTRUE(vf$scale_free)) {
    "normal(0, 2)"
  } else {
    paste0("normal(0, ", signif(2 / sd(response), 4), ")")
  }
  out <- prior_string(unname(c0_prs[fam_tag]), nlpar = "c0")
  for (p in setdiff(vf$pars, "c0")) {
    out <- out + prior_string(slope_prior, nlpar = p)
  }
  out
}

#' define_group_prior
#'
#' Builds priors for the parameters a group-level term introduces.
#'
#' @param group_spec The output of \code{\link{parse_group_terms}}.
#' @param predictor A \code{\link[base]{numeric}} vector of the predictor.
#' @param response A \code{\link[base]{numeric}} vector of the response, on
#' the link scale.
#' @param prior_type One of \code{"uninformative"} or \code{"regularizing"}.
#'
#' @details Without this, no prior is generated for a group-level standard
#' deviation and it falls through to the \pkg{brms} default,
#' \code{student_t(3, 0, 2.5)}. On a bounded response under the identity link
#' \code{\link{bnec}} forces, an offset drawn at that scale puts the mean
#' outside its support, where the likelihood is undefined. There is no inverse
#' link to rescue it -- that is the trade \code{\link{bnec}} makes so that
#' \code{top}, \code{bot} and \code{nec} stay directly interpretable -- so
#' keeping the mean in range falls entirely to the prior. See #245.
#'
#' \strong{The rule.} A group-level standard deviation is given one tenth of
#' the observed range of \emph{the scale its parameter lives on}, following the
#' same three-way split \code{\link{define_prior}} already makes for the
#' curve's own parameters:
#'
#' \itemize{
#'   \item \code{top}, \code{bot} and \code{ogl} are on the response scale:
#'     \code{diff(range(response)) / 10}.
#'   \item \code{nec} and \code{ec50} are on the predictor scale:
#'     \code{diff(range(predictor)) / 10}.
#'   \item \code{beta}, \code{slope}, \code{d} and \code{f} are
#'     dimensionless and take \code{normal(0, 5)} of their own, so 0.5.
#' }
#'
#' \strong{It is a rule about the data, not about the parameter's own prior},
#' and the distinction is worth keeping straight because the two coincide only
#' for the dimensionless parameters, where \code{normal(0, 5)} gives exactly
#' 0.5. Elsewhere the realised group-level standard deviation runs between
#' roughly a twentieth and a third of the spread of the parameter's own prior,
#' depending on family and data -- 0.06 of it for a \code{poisson} \code{top},
#' 0.34 for a Beta one. Tying it to the parameter's prior instead would inherit
#' scales chosen to be deliberately diffuse: \code{top} on a gaussian response
#' takes \code{2.5 * sd(response)}, which is a reasonable statement of
#' ignorance about a level and a poor one about deviation around it. The
#' observed range is the more defensible anchor, and it is the same quantity
#' the predictor-scaled priors are already bounded by.
#'
#' For \code{nec} and \code{ec50} the two readings do agree, because those
#' priors are truncated to \code{[min(predictor), max(predictor)]}, so the
#' range \emph{is} the scale the prior spans.
#'
#' \code{student_t(3, 0, s)} keeps the shape and heavy tail of the \pkg{brms}
#' default and changes only its scale, so this narrows a default that was never
#' scale-aware rather than substituting a differently-shaped one.
#'
#' \strong{What the prior cannot do.} Every prior
#' \code{\link{define_prior}} generates constrains its parameter to the region
#' where the model is defined: \code{beta(5, 2)} on (0, 1), \code{lb = 0} for
#' the count and Gamma families, \code{nec} truncated to the predictor range.
#' A group-level deviation cannot be constrained that way -- \pkg{brms} declares
#' \code{r_} unconstrained -- so a grouped fit does not inherit the property
#' that \code{top}, \code{bot} and \code{nec} remain in range, and no choice
#' of scale here restores it. Scaling the prior is what allows such a fit to
#' initialise; it does not stop the sampler reaching the boundary afterwards,
#' because the posterior for the standard deviation is an order of magnitude
#' smaller than the distance from the fitted mean to that boundary and it is the
#' length of the leapfrog trajectory that crosses it. Divergent transitions on a
#' grouped fit are therefore expected wherever the response distribution
#' restricts the range of the mean; see \code{\link{add_brm_defaults}} for the
#' \code{adapt_delta} that mitigates them, \code{vignette("example3")} for what
#' a user should check, and #257 for the parameterisation change that removes
#' the cause.
#'
#' \strong{prior_type.} \code{"regularizing"} halves every generated scale.
#' The two default sets differ only in the response-scaled parameters for the
#' curve itself, but a user reaching for the narrower set on a grouped fit is
#' usually reaching for it \emph{because} of the grouping, and leaving the one
#' parameter that provoked the choice untouched would make the argument inert
#' where it is most wanted. A factor of two is the same order as the narrowing
#' the response-scaled priors get (2.5 for gaussian, 1.6 for the gamma-scaled
#' families, 1.3 for the beta-scaled ones).
#'
#' The \code{ogl} \emph{intercept} gets a prior too, and needs one for a
#' different reason. \code{ogl} enters as an offset added to the whole curve,
#' so its population intercept is \strong{not identified}: a constant added to
#' \code{ogl} can be taken back out of \code{top} and \code{bot} with no
#' change to the likelihood, and \pkg{brms} leaves a non-linear
#' population-level parameter flat by default. Centring it at zero is what makes
#' the decomposition identified -- \code{top} and \code{bot} carry the level,
#' and the grouping term carries deviation about it.
#'
#' @return An object of class \code{\link[brms]{brmsprior}}, or \code{NULL}.
#'
#' @importFrom brms prior_string
#'
#' @noRd
define_group_prior <- function(group_spec, predictor, response,
                               prior_type = "uninformative") {
  if (is.null(group_spec) || length(group_spec$nlpars) == 0) {
    return(NULL)
  }
  narrow <- if (prior_type == "regularizing") 2 else 1
  # A response or predictor with no spread gives a scale of zero, which is not a
  # usable prior. It is degenerate input rather than something to model around,
  # so fall back to the dimensionless scale and let the fit fail on its own
  # terms if it is going to. The fallback is not narrowed: it is a stand-in for
  # a scale that could not be measured, not a measurement to be regularized.
  safe_scale <- function(x) {
    s <- diff(range(x)) / (10 * narrow)
    if (!is.finite(s) || s <= 0) 0.5 else s
  }
  s_y <- safe_scale(response)
  s_x <- safe_scale(predictor)
  scale_for <- function(par) {
    if (par %in% c("top", "bot", "ogl")) {
      s_y
    } else if (par %in% c("nec", "ec50")) {
      s_x
    } else {
      0.5 / narrow
    }
  }
  out <- NULL
  for (p in group_spec$nlpars) {
    pr <- prior_string(paste0("student_t(3, 0, ", signif(scale_for(p), 4), ")"),
                       class = "sd", nlpar = p)
    out <- if (is.null(out)) pr else out + pr
  }
  if (isTRUE(group_spec$ogl)) {
    out <- out + prior_string(paste0("normal(0, ", signif(s_y, 4), ")"),
                              nlpar = "ogl")
  }
  out
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
