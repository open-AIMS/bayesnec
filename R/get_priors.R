#' Priors for a \pkg{bayesnec} model
#'
#' Returns priors in the form \code{\link{bnec}} accepts them, either from a fit
#' that has already been run or from a formula and data before anything is
#' fitted.
#'
#' @param object An object of class \code{\link{bayesnecfit}},
#' \code{\link{bayesmanecfit}} or \code{\link{bayesnechurdlefit}} as returned by
#' \code{\link{bnec}}; or a \code{\link{bayesnecformula}}, a
#' \code{\link[stats]{formula}} or a \code{\link[base]{character}} string that
#' can be coerced to one.
#' @param ... Unused.
#'
#' @details Two entry points, answering two different questions.
#'
#' \bold{Given a fit,} \code{get_priors} returns the priors that fit actually
#' used, including any the user overrode. This is the record of what was run:
#' \code{bnec(formula, data = data, prior = get_priors(fit))} reproduces the same
#' model.
#'
#' \bold{Given a formula and data,} it returns the priors \code{\link{bnec}}
#' would generate, without fitting anything, so they can be inspected and edited
#' before the first run. The family is chosen from the data exactly as
#' \code{\link{bnec}} would unless one is supplied, and models invalid for it are
#' dropped with the same message.
#'
#' The two can disagree, and that is the point of having both: once a prior has
#' been overridden, the fit's priors are no longer the ones the defaults would
#' produce. Given the same formula, data and family, and no user prior, they
#' agree.
#'
#' What is returned is the population-level priors on the parameters of the
#' model's own non-linear formula, which are the ones \code{\link{bnec}} accepts
#' through \code{prior}. Where a \code{disp} term adds a variance function on the
#' fitted mean -- \code{disp("power")} and the other named forms -- its
#' parameters (\code{c0}, \code{c1}, ...) belong to that formula and come back
#' with the curve parameters. They are needed: \code{\link{bnec}} takes a
#' supplied prior whole, so a set handed back without them leaves \pkg{brms} to
#' put a flat prior on each, which is a different model. See
#' \code{\link{bayesnecformula}}.
#'
#' What is left to \pkg{brms} and its defaults, and so is not returned, is the
#' family's own dispersion parameter (\code{sigma}, \code{shape}, \code{phi})
#' where dispersion is held constant, and the linear predictor of a
#' \code{disp(~x)} sub-model. Neither can be supplied through
#' \code{bnec(prior = )} in any case, because the initial-value search matches
#' prior names against the model's own parameters and rejects any prior naming
#' one outside that set.
#'
#' This is why \code{get_priors} is not \code{\link{pull_prior}}, which returns
#' the whole \code{brmsprior} a fit carries -- \pkg{brms} defaults, duplicated
#' vectorized rows and all -- and is for looking at rather than for feeding back
#' in.
#'
#' @return For a single model, an object of class
#' \code{\link[brms]{brmsprior}}. For a model set, a named
#' \code{\link[base]{list}} of them, one per model, named as
#' \code{\link{bnec}} expects a named prior list to be. For a
#' \code{\link{bayesnechurdlefit}}, a \code{\link[base]{list}} of two such
#' objects, one per component.
#'
#' @seealso \code{\link{bnec}}, \code{\link{pull_prior}},
#' \code{\link{check_priors}}, \code{\link{show_params}}
#'
#' @examples
#' library(bayesnec)
#' data(manec_example)
#' # the priors a fit used, ready to pass back to bnec(prior = )
#' get_priors(manec_example)
#' # the priors bayesnec would generate, without fitting anything
#' get_priors(y ~ crf(x, "nec3param"), data = nec_data)
#'
#' @export
get_priors <- function(object, ...) {
  UseMethod("get_priors")
}

#' @noRd
#' @export
get_priors.default <- function(object, ...) {
  stop("get_priors() takes an object fitted by bnec, or a formula and data.",
       " You supplied an object of class \"", class(object)[1], "\".",
       call. = FALSE)
}

#' @noRd
#' @export
get_priors.bayesnecfit <- function(object, ...) {
  usable_prior(object$fit$prior)
}

#' @noRd
#' @export
get_priors.bayesmanecfit <- function(object, ...) {
  lapply(object$mod_fits, function(x) usable_prior(x$fit$prior))
}

#' @noRd
#' @export
get_priors.bayesnechurdlefit <- function(object, ...) {
  # Two separate fits with two separate model sets, so two sets of priors; there
  # is no single prior object that would reproduce the pair.
  list(growth = get_priors(object$growth),
       survival = get_priors(object$survival))
}

#' @rdname get_priors
#'
#' @param data A \code{\link[base]{data.frame}} containing the data referred to
#' by the formula.
#' @param family A \code{\link[stats]{family}} function, as passed to
#' \code{\link{bnec}}. If \code{NULL} (the default) it is chosen from the data
#' the way \code{\link{bnec}} would.
#' @param prior_type A \code{\link[base]{character}} string, either
#' \code{"uninformative"} (the default) or \code{"regularizing"}. See
#' \code{\link{bnec}}.
#' @param model_survival An optional \code{\link[base]{character}} string naming
#' the equation for the second block of a joint hurdle or zero-inflated fit. See
#' \code{\link{bnec}}.
#'
#' @inherit get_priors return details examples
#'
#' @method get_priors formula
#'
#' @importFrom stats model.frame
#'
#' @export
get_priors.formula <- function(object, data, family = NULL,
                               prior_type = "uninformative",
                               model_survival = NULL, ...) {
  # bayesnecformula() returns an object whose class is c("formula",
  # "bayesnecformula"), formula first, so this is the method dispatch reaches
  # for both a bare formula and a bnf() one. The bayesnecformula method below
  # forwards here rather than the other way round, which would recurse.
  object <- bayesnecformula(object)
  if (missing(data)) {
    stop("Argument `data` is required when get_priors() is given a formula:",
         " every default prior is derived from the data. See ?get_priors.",
         call. = FALSE)
  }
  prior_type <- match.arg(prior_type, c("uninformative", "regularizing"))
  bdat <- model.frame(object, data = data, run_par_checks = TRUE)
  # Deliberately the same sequence bnec() runs, and through the same functions:
  # a prior generated by a parallel implementation would be the wrong answer the
  # moment either changed. See #141.
  link_source <- family_link_source(substitute(family), env = parent.frame())
  fam_args <- if (is.null(family)) list() else list(family = family)
  family <- retrieve_valid_family(fam_args, bdat, link_source = link_source)
  model <- check_models(get_model_from_formula(object), family, bdat)
  model_survival <- check_model_survival(model_survival, family, bdat)
  if (length(model) == 0) {
    stop("No valid models have been supplied for this data type.",
         call. = FALSE)
  }
  disp_spec <- parse_disp_term(object)
  out <- lapply(model, function(m) {
    single_form <- single_model_formula(object, m)
    m_dat <- model.frame(single_form, data = data, run_par_checks = TRUE)
    # check_data() runs before the prior is built inside fit_bayesnec(), and it
    # can move the data the prior is derived from -- nudging zeros off a
    # boundary, for instance. Skipping it here would give priors that differ
    # from the fitted ones on exactly the datasets where it matters.
    checked <- check_data(data = m_dat, family = family, model = m)
    y <- checked$mod_dat$y
    if (checked$family$family %in% c("binomial", "beta_binomial")) {
      y <- y / checked$mod_dat$trials
    }
    # Same reason as the binomial branch above: the priors for top and bot are
    # built from quantiles of the response, and for a rate model the parameters
    # they describe live on the rate scale, not the count scale. Deriving them
    # from raw counts gave a prior mean of ~61 against a true top of 20 on the
    # #136 reprex -- silently, because nothing downstream checks a prior for
    # plausibility.
    if (!is.null(checked$mod_dat$denom)) {
      y <- y / checked$mod_dat$denom
    }
    define_prior(m, checked$family, checked$mod_dat$x, y,
                 prior_type = prior_type, model_survival = model_survival,
                 disp_spec = disp_spec,
                 group_spec = parse_group_terms(single_form, m))
  })
  names(out) <- model
  if (length(out) == 1) {
    out[[1]]
  } else {
    out
  }
}

#' @noRd
#' @export
get_priors.bayesnecformula <- function(object, data, ...) {
  get_priors.formula(object, data = data, ...)
}

#' @noRd
#' @export
get_priors.character <- function(object, data, ...) {
  get_priors.formula(bayesnecformula(object), data = data, ...)
}

#' Reduce a fitted brmsprior to the rows bnec() will accept back
#'
#' @param prior The \code{\link[brms]{brmsprior}} carried by a
#' \code{\link[brms]{brmsfit}}.
#'
#' @details Two changes, both needed for the round trip.
#'
#' \pkg{brms} stores more than was supplied: a duplicated \code{coef =
#' "Intercept"} row for each population-level prior, and its own defaults for
#' the dispersion parameter. Handing all of that back is what makes
#' \code{\link{pull_prior}} output unusable as a \code{prior} argument --
#' \code{make_inits()} matches prior names against the model's own parameters
#' and rejects the set over the extra \code{sigma} row. The \code{source} column
#' distinguishes them, so the supplied rows are the ones kept.
#'
#' The \code{class == "b"} test is what separates the two, and it deliberately
#' keeps the parameters a route B \code{disp} term adds: \code{c0} and its
#' slopes are non-linear parameters of the dispersion sub-model, are supplied by
#' \code{define_disp_prior()} rather than by \pkg{brms}, and have to come back
#' for the fit to be reproducible. Only a prior \pkg{brms} itself chose is
#' dropped.
#'
#' \pkg{brms} also records an absent bound as \code{""} in a fit's own
#' \code{prior} slot, where \code{define_prior()} uses \code{NA}. Both mean
#' "unbounded", and \code{make_inits()} now treats them alike, but the returned
#' object is normalised to \code{NA} so that it compares equal to a generated
#' prior.
#'
#' @return An object of class \code{\link[brms]{brmsprior}}.
#'
#' @noRd
usable_prior <- function(prior) {
  if (is.null(prior) || nrow(prior) == 0) {
    return(prior)
  }
  # class "b" is the curve's own coefficients and the parameters a route B
  # disp() term adds. The parameter classes are kept too, so that a prior the
  # user supplied on sigma/shape/phi/zi/hu round trips and get_priors() is a
  # complete record of the fit rather than a record of everything except those.
  # `source == "user"` is what makes that safe: a prior brms chose for itself is
  # still dropped, because reporting it would suggest bayesnec had made a choice
  # it did not make. See #207 and #231.
  keep <- prior$source == "user" & prior$coef == "" &
    (prior$class == "b" | prior$class %in% auxiliary_classes())
  out <- prior[keep, , drop = FALSE]
  for (bound in c("lb", "ub")) {
    if (bound %in% names(out)) {
      out[[bound]][!nzchar(out[[bound]])] <- NA_character_
    }
  }
  rownames(out) <- NULL
  out
}

#' Classes brms uses for a family parameter that is not part of the mean curve
#'
#' Two kinds, kept together because \code{usable_prior()} treats them the same
#' way -- a user may set a prior on either, and neither takes an initial value
#' from the curve's own search.
#'
#' \emph{Dispersion}: the spread around the mean. \code{sigma} for gaussian,
#' \code{shape} for Gamma, negbinomial and their hurdle/zero-inflated forms,
#' \code{phi} for Beta and beta_binomial. Poisson, binomial and bernoulli have
#' none.
#'
#' \emph{Mixing}: \code{zi} and \code{hu}, the probability of the second
#' component. These are here only for the families bayesnec fits as a
#' \emph{single} block -- \code{zero_inflated_poisson} and
#' \code{zero_inflated_negbinomial}, added under #104 -- where brms carries the
#' parameter with a class of its own and a default \code{beta(1, 1)}. For
#' \code{hurdle_gamma} and \code{zero_inflated_beta}, which bayesnec fits as
#' two parameter blocks, the second block's priors carry class "b" with a
#' prefixed \code{nlpar} and no class-\code{zi} row is ever generated, so
#' including these names is a no-op there rather than a conflict.
#'
#' Reporting \code{zi} matters more than it might look: for the two count
#' families it is the zero-inflation probability, which is the quantity those
#' families exist to estimate. Leaving it out of \code{\link{get_priors}} made
#' the returned set silently incomplete in exactly the case the user cares
#' about. See #231.
#'
#' @return A \code{\link[base]{character}} vector.
#'
#' @noRd
auxiliary_classes <- function() {
  # "sd" is a group-level standard deviation. It is here for the same reason
  # the dispersion classes are: bayesnec now generates one, so leaving it out
  # would make get_priors() a record of everything except the parameter a
  # grouped model is hardest to get right. See #245.
  c("sigma", "shape", "phi", "zi", "hu", "sd")
}
