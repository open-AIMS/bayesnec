# Internals for the joint two-block families, where the response and the
# probability of a zero are two parameter blocks inside one brms fit.
#
# brms names the second block "hu" for the hurdle families and "zi" for the
# zero-inflated ones. bayesnec treats the two alike and passes the name through
# rather than branching on it. That is safe because Gamma and Beta emit no zeros
# of their own, and a count hurdle's positive block is zero-truncated. In a fit
# the name prefixes every non-linear parameter of the second block: `top`
# belongs to mu, `hutop` or `zitop` to the second block.

#' Whether a family is a joint two-block (hurdle or zero-inflated) family
#'
#' @param family Either a \code{\link[stats]{family}} object or a family tag.
#'
#' @return A \code{\link[base]{logical}}.
#'
#' @noRd
is_hurdle_family <- function(family) {
  fam_tag <- if (inherits(family, "family")) family$family else family
  isTRUE(fam_tag %in% names(hurdle_fams))
}

#' Is this family a joint count hurdle?
#'
#' @param family Either a \code{\link[stats]{family}} object or a family tag.
#'
#' @details \code{hurdle_poisson} and \code{hurdle_negbinomial}, whose
#' positive block is a zero-truncated count in the likelihood \pkg{brms}
#' writes for them.
#'
#' @return A \code{\link[base]{logical}}.
#'
#' @noRd
is_count_hurdle <- function(family) {
  is_hurdle_family(family) &&
    isTRUE(unname(hurdle_mu_fams[[canonical_fam_tag(family)]]) %in%
             c("poisson", "negbinomial"))
}

#' Name brms gives the second parameter block for this family
#'
#' @param family Either a \code{\link[stats]{family}} object or a family tag.
#'
#' @return A \code{\link[base]{character}} string, "hu" or "zi".
#'
#' @noRd
hurdle_dpar <- function(family) {
  fam_tag <- if (inherits(family, "family")) family$family else family
  unname(hurdle_fams[[fam_tag]])
}

#' Family whose defaults the mu block reuses
#'
#' The mu block describes the response with the zeros set aside. Its priors
#' and initial values reuse its base family: Gamma for hurdle_gamma, Beta for
#' zero_inflated_beta, and the count family for hurdle_poisson and
#' hurdle_negbinomial. The positive count likelihood is zero-truncated.
#'
#' The caller's mean link is kept. brms applies the inverse mean link to the
#' whole non-linear expression, so under a log or logit link \code{top} and
#' \code{bot} are on the link scale, and \code{\link{define_prior}} must build
#' their priors on that scale. An identity-link family here gives priors on the
#' wrong scale, such as a prior bounded to \code{[0, 1]} for a logit-scale
#' \code{bot} that must be negative when the response floor is below 0.5.
#'
#' The second block stays on the identity link, which
#' \code{\link{validate_family}} requires because the block is written as
#' \code{1 - <non-zero probability>}.
#'
#' @param family Either a \code{\link[stats]{family}} object or a family tag.
#' A tag takes the identity link, the one \code{\link{bnec}} assigns where it
#' chooses one.
#'
#' @return An object of class \code{\link[stats]{family}}.
#'
#' @importFrom stats Gamma poisson
#' @importFrom brms Beta brmsfamily negbinomial
#'
#' @noRd
hurdle_mu_family <- function(family) {
  is_fam <- inherits(family, "family")
  fam_tag <- if (is_fam) family$family else family
  link <- if (is_fam && !is.null(family$link)) family$link else "identity"
  switch(unname(hurdle_mu_fams[[fam_tag]]),
         Gamma = Gamma(link = link),
         beta = Beta(link = link),
         poisson = brmsfamily("poisson", link = link),
         negbinomial = negbinomial(link = link),
         stop("No mu family defined for ", fam_tag, ".", call. = FALSE))
}

#' Conditional mean of the positive part of a count distribution
#'
#' @param mu A numeric matrix of mean-parameter draws.
#' @param family Either \code{"hurdle_poisson"} or
#' \code{"hurdle_negbinomial"}.
#' @param shape A numeric matrix of negative-binomial shape draws, required for
#' \code{"hurdle_negbinomial"}.
#'
#' @return A numeric matrix with the same dimensions as \code{mu}.
#'
#' @noRd
hurdle_positive_mean <- function(mu, family, shape = NULL) {
  if (family %in% c("poisson", "hurdle_poisson")) {
    nonzero <- -expm1(-mu)
  } else if (family %in% c("negbinomial", "hurdle_negbinomial")) {
    if (is.null(shape)) {
      stop("Negative-binomial positive means require shape draws.",
           call. = FALSE)
    }
    nonzero <- -expm1(-shape * log1p(mu / shape))
  } else {
    return(mu)
  }
  out <- mu / nonzero
  out[nonzero == 0] <- 1
  out
}

#' Conditional variance of the positive part of a count distribution
#'
#' @inheritParams hurdle_positive_mean
#'
#' @return A numeric matrix with the same dimensions as \code{mu}.
#'
#' @noRd
hurdle_positive_variance <- function(mu, family, shape = NULL) {
  if (family %in% c("poisson", "hurdle_poisson")) {
    nonzero <- -expm1(-mu)
    second_moment <- mu + mu^2
  } else if (family %in% c("negbinomial", "hurdle_negbinomial")) {
    if (is.null(shape)) {
      stop("Negative-binomial positive variances require shape draws.",
           call. = FALSE)
    }
    nonzero <- -expm1(-shape * log1p(mu / shape))
    second_moment <- mu + mu^2 / shape + mu^2
  } else {
    stop("Positive-count variances require a count family.", call. = FALSE)
  }
  positive_mean <- hurdle_positive_mean(mu, family, shape)
  out <- second_moment / nonzero - positive_mean^2
  out[nonzero == 0] <- 0
  out
}

#' Whether a formula is the truncated positive block of a count hurdle
#'
#' @param formula A bayesnec formula.
#' @param family A family object.
#'
#' @return A logical value.
#'
#' @noRd
is_factorised_count_formula <- function(formula, family) {
  isTRUE(attr(formula, "bayesnec_internal_truncation")) &&
    family$family %in% c("poisson", "negbinomial")
}

#' Exact expected response for a factorised count hurdle
#'
#' @param fit A fitted \code{brmsfit}.
#' @param formula The bayesnec formula used for the fit.
#' @param dpar,nlpar Optional parameter predictions passed to
#' \code{posterior_epred}.
#' @param ndraws,draw_ids Optional posterior draw selection passed to
#' \code{posterior_epred}.
#' @param ... Further arguments passed to \code{posterior_epred}.
#'
#' @return A numeric matrix of posterior draws.
#'
#' @details brms obtains expectations for truncated discrete families by
#' summing a finite grid whose upper end is three times the largest fitted
#' mean. That approximation can lose most of an overdispersed negative-
#' binomial tail. For the one-sided truncation used here the conditional mean
#' is available in closed form, so bayesnec computes it from the underlying
#' \code{mu} and \code{shape} draws instead.
#'
#' @importFrom brms ndraws posterior_epred
#'
#' @noRd
factorised_count_epred <- function(fit, formula, dpar = NULL, nlpar = NULL,
                                   ndraws = NULL, draw_ids = NULL, ...) {
  if (!is_factorised_count_formula(formula, fit$family) ||
      !is.null(dpar) || !is.null(nlpar)) {
    return(posterior_epred(
      fit, dpar = dpar, nlpar = nlpar, ndraws = ndraws,
      draw_ids = draw_ids, ...
    ))
  }
  # brms samples draw_ids inside each posterior prediction call when only
  # ndraws is supplied. The mu and shape calls must use the same draws or the
  # negative-binomial conditional mean combines unrelated parameters.
  if (!is.null(ndraws) && is.null(draw_ids)) {
    draw_ids <- sample.int(ndraws(fit), ndraws)
    ndraws <- NULL
  }
  mu <- posterior_epred(
    fit, dpar = "mu", ndraws = ndraws, draw_ids = draw_ids, ...
  )
  shape <- NULL
  if (fit$family$family == "negbinomial") {
    shape <- posterior_epred(
      fit, dpar = "shape", ndraws = ndraws, draw_ids = draw_ids, ...
    )
  }
  hurdle_positive_mean(mu, fit$family$family, shape)
}

#' Posterior draws for one block of a joint hurdle fit
#'
#' @param object A \code{bayesnecfit} containing a joint hurdle family.
#' @param newdata A prediction data frame.
#' @param dpar The requested parameter block, or \code{NULL} for the combined
#' endpoint.
#'
#' @return A numeric matrix of posterior draws.
#'
#' @importFrom brms posterior_epred
#'
#' @noRd
joint_hurdle_epred <- function(object, newdata, dpar = NULL) {
  if (is.null(dpar)) {
    return(posterior_epred(object, newdata = newdata, re_formula = NA))
  }
  family <- object$fit$family
  if (!is_hurdle_family(family)) {
    stop("The \"dpar\" argument is only valid for hurdle families.",
         call. = FALSE)
  }
  dpar <- match.arg(dpar, c("mu", hurdle_dpar(family)))
  out <- posterior_epred(
    object, newdata = newdata, re_formula = NA, dpar = dpar
  )
  if (dpar != "mu") {
    return(1 - out)
  }
  if (family$family == "hurdle_negbinomial") {
    shape <- posterior_epred(
      object, newdata = newdata, re_formula = NA, dpar = "shape"
    )
    return(hurdle_positive_mean(out, family$family, shape))
  }
  hurdle_positive_mean(out, family$family)
}

#' Non-linear parameter names of the second block for a given model
#'
#' @param model A \code{\link[base]{character}} string naming a bayesnec model.
#' @param dpar The second block's name, "hu" or "zi".
#'
#' @return A \code{\link[base]{character}} vector.
#'
#' @noRd
hu_pars <- function(model, dpar) {
  paste0(dpar, names(get(paste0("bf_", model))$pforms))
}

#' Build the zero-probability sub-model for a bayesnec equation
#'
#' Derives the second block from an existing bayesnec equation. Every
#' non-linear parameter takes the block's prefix and the right-hand side is
#' wrapped as \code{1 - (...)}. The curve then models the probability of a
#' *non*-zero, which declines with the predictor like every bayesnec equation,
#' while the block itself, the probability of a zero, increases.
#'
#' @param model A \code{\link[base]{character}} string naming a bayesnec model.
#' @param dpar The second block's name, "hu" or "zi".
#'
#' @return A \code{\link[base]{list}} with elements \code{nlf} (the block's
#' formula), \code{lf} (the parameter formula) and \code{pars}.
#'
#' @importFrom stats as.formula
#'
#' @noRd
make_hu_block <- function(model, dpar) {
  bf_obj <- get(paste0("bf_", model))
  pars <- names(bf_obj$pforms)
  rhs <- deparse1(bf_obj$formula[[3]])
  # Longest name first, so that "top" inside an already-substituted "hutop" is
  # never rewritten a second time. Word boundaries alone are not enough,
  # because bayesnec parameter names are substrings of one another (bot/top).
  for (p in pars[order(nchar(pars), decreasing = TRUE)]) {
    rhs <- gsub(paste0("(?<![[:alnum:]_])", p, "(?![[:alnum:]_])"),
                paste0(dpar, p), rhs, perl = TRUE)
  }
  list(nlf = as.formula(paste0(dpar, " ~ 1 - (", rhs, ")")),
       lf = as.formula(paste0(paste0(dpar, pars, collapse = " + "), " ~ 1")),
       pars = paste0(dpar, pars))
}

#' Append the zero-probability block to a brms formula
#'
#' @param brms_bf An object of class \code{\link[brms]{brmsformula}}.
#' @param model A \code{\link[base]{character}} string naming a bayesnec model.
#' @param x_var The predictor column name, substituted for the generic "x".
#' @param dpar The second block's name, "hu" or "zi".
#'
#' @return An object of class \code{\link[brms]{brmsformula}}.
#'
#' @importFrom brms nlf lf
#' @importFrom stats as.formula
#'
#' @noRd
add_hu_block <- function(brms_bf, model, x_var, dpar) {
  hb <- make_hu_block(model, dpar)
  rhs <- substitute_x_in_formula(x_var, deparse1(hb$nlf[[3]]))
  brms_bf + nlf(as.formula(paste0(dpar, " ~ ", rhs))) + lf(hb$lf)
}

#' Proportion of non-zero responses at each unique predictor value
#'
#' Used to prime the second block's initial-value search: the sub-model is
#' written as \code{1 - <non-zero probability>}, so the curve being initialised
#' is that probability.
#'
#' @param predictor A \code{\link[base]{numeric}} vector.
#' @param response A \code{\link[base]{numeric}} vector; zero denotes the
#' hurdle, e.g. an individual that did not survive.
#'
#' @return A \code{\link[base]{list}} with elements \code{x} and \code{y}.
#'
#' @noRd
survival_by_x <- function(predictor, response) {
  ux <- sort(unique(predictor))
  p <- vapply(ux, function(z) mean(response[predictor == z] > 0), numeric(1))
  # Keep proportions inside (0, 1) for priors and initial values, as
  # response_link_scale() does. boundary_inset() derives a separate inset
  # for the initial-curve band from these clamped values.
  eps <- 1 / (2 * length(response))
  list(x = ux, y = pmin(pmax(p, eps), 1 - eps))
}

#' Split a two-block response into its two component views
#'
#' @param predictor A \code{\link[base]{numeric}} vector.
#' @param response A \code{\link[base]{numeric}} vector.
#'
#' @return A \code{\link[base]{list}} with a \code{mu} element (predictor and
#' response restricted to non-zeros) and a \code{hu} element (unique predictor
#' values and the proportion non-zero at each).
#'
#' @noRd
split_hurdle_response <- function(predictor, response) {
  keep <- response > 0
  list(mu = list(x = predictor[keep], y = response[keep]),
       hu = survival_by_x(predictor, response))
}
