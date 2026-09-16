#' models
#'
#' Lists the fitted or available models.
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} as returned by \code{\link{bnec}},
#' a \code{\link[base]{character}} vector indicating the type of model set for
#' which to list the available models, or a \code{\link[base]{numeric}} vector
#' indicating the natural range of values which the models should be able to
#' handle (see Details). If missing, all available models and their groups are
#' listed when \code{max_pars} is also \code{NULL}. When \code{object} is
#' missing and \code{max_pars} is supplied, all available equations are
#' considered before applying the limit.
#' @param max_pars An optional positive whole number giving the maximum number
#' of curve parameters an equation may contain.
#'
#' @details The available models are "nec3param", "nec4param", "nechorme",
#' "nechorme4", "necsigm", "neclin", "neclinhorme", "nechormepwr",
#' "nechorme4pwr", "nechormepwr01", "ecxlin", "ecxexp", "ecxsigm", "ecx4param",
#' "ecxwb1", "ecxwb2", "ecxwb1p3", "ecxwb2p3", "ecxll5", "ecxll4", "ecxll3",
#' "ecxhormebc4", and "ecxhormebc5".
#'
#' To see the model formula and parameters for a specific model use the
#' function \code{\link{show_params}}.
#'
#' To see all the models in an available set (e.g. "all", "nec" or ecx") use
#' the function \code{\link{models}} specifying the group name.
#'
#' To see the model names, model formula and parameters fitted in an existing
#' \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}} model object use
#' the function \code{\link{models}} specifying the fitted object.
#'
#' To see what models are available for a given type of data use the function
#' \code{\link{models}} passing a \code{\link[base]{numeric}} vector indicating
#' the range of possible data types: \code{c(-Inf, Inf)} for a response that can
#' take any value, \code{c(0, 1)} for one bounded on the unit interval, and
#' \code{c(0, Inf)} for one bounded below at zero only. The list returned is
#' produced by the same internal check \code{\link{bnec}} applies at fit time, so
#' the two cannot disagree.
#'
#' Four restrictions decide which equations a given response admits. Models with
#' an exponential decay (most models with parameter "beta") and no "bot"
#' parameter are zero-bounded, and are not suitable for the Gaussian family or
#' for any family modelled on a logit or log link. Models with a linear decay
#' ("neclin", "neclinhorme", "ecxlin") are unbounded below, and so are suitable
#' for neither the zero-bounded families (Gamma, Poisson, negative binomial) nor
#' the 0, 1 bounded ones (bernoulli, binomial, beta, beta_binomial) on an
#' identity link. Models whose hormesis term raises the predictor to a power
#' with no coefficient ("nechormepwr", "nechorme4pwr") can put the mean above 1
#' at any predictor range, and are excluded for the 0, 1 bounded families on an
#' identity link; "nechormepwr01" is the bounded form of the same equation and
#' is retained there, and is excluded from the zero-bounded identity families for
#' the converse reason. Models that raise the predictor to a fractional power
#' ("ecxsigm", "necsigm", "nechormepwr", "nechorme4pwr") are not suitable where
#' the predictor takes negative values, and \code{"ecxhormebc5"} joins them
#' where an identity-linked family also requires a positive mean.
#'
#' None of this has to be controlled by the user: a \code{\link{bnec}} call
#' with \code{model = "all"} excludes the inadmissible equations.
#' \code{vignette("example2b")} sets out each equation and the reasoning behind
#' its restrictions.
#'
#' A model group names a shape, not a set of equations admissible for a given
#' response. "decline" is the set that excludes the hormesis models, and it
#' therefore includes "neclin" and "ecxlin", whose mean decays by subtraction
#' and is unbounded below. Neither is admissible for a response bounded at
#' zero. A \code{\link{bnec}} call is unaffected, because the same internal
#' check described above drops them where the family requires it; what the
#' group name does not do is state which equations that check will keep. Code
#' that needs the admissible set should ask for it directly, by passing the
#' numeric range --- \code{models(c(0, 1))} --- rather than reading a group.
#'
#' Set \code{max_pars} to restrict the resolved set to equations with no more
#' than that number of curve parameters. The limit can be used by itself or
#' combined with a model group, response range, or fitted object. The returned
#' list can be passed directly as the \code{model} argument in a
#' \code{\link{bayesnecformula}}, for example
#' \code{crf(x, models("decline", max_pars = 3))}.
#'
#' \bold{Equivalents in the \code{drc} package}
#'
#' \code{drc}'s \code{NEC.4()} and \code{NEC.3()} correspond to
#' \code{"nec4param"} and \code{"nec3param"}, which are the same models under
#' the reparameterisation \code{b = exp(}"beta"\code{)}. \code{NEC.2()} fixes
#' the upper asymptote at a constant and has no equivalent here by choice; a
#' \code{\link[brms]{constant}} or tight informative prior on "top" states the
#' same constraint where it is genuinely structural.
#' \code{vignette("example2b")} gives the parameter mapping, the agreement
#' between the two implementations and the reason \code{NEC.2()} is not
#' provided.
#'
#' @return A \code{\link[base]{list}} of the available or fitted models.
#' @examples
#' library(bayesnec)
#' # default to all models and model groups
#' models()
#' # single model
#' show_params("nec3param")
#' # group of models
#' models("all")
#' # models that are suitable for 0,1 bounded data
#' models(c(0,1))
#' # models with no more than three curve parameters
#' models(max_pars = 3)
#'
#' @export
models <- function(object, max_pars = NULL) {
  if (!is.null(max_pars) &&
      (!is.numeric(max_pars) || length(max_pars) != 1 || is.na(max_pars) ||
       !is.finite(max_pars) || max_pars < 1 || max_pars != floor(max_pars))) {
    stop("Argument `max_pars` must be a single positive whole number.",
         call. = FALSE)
  }
  if (missing(object) && is.null(max_pars)) {
    return(mod_groups)
  }
  if (missing(object)) {
    use_mods <- mod_groups$all
  } else if (is_bayesnecfit(object)) {
    use_mods <- object$model
  } else if (is_bayesmanecfit(object)) {
    use_mods <- names(object$mod_fits)
  } else if (object[1] %in% names(mod_groups)) {
    # names(mod_groups) rather than a hard-coded subset: "decline" and
    # "hormesis" are accepted by bnec(model = ) via handle_set() but were not
    # listed here, the same kind of drift as #170 itself.
    use_mods <- mod_groups[[object[1]]]
  } else if (is.numeric(object)) {
    # Derived from check_models() rather than restated here. The two lists had
    # drifted apart -- this branch dropped nechorme and nechorme4 for a 0-1
    # bounded response, which bnec() fits happily, and kept nechormepwr01 for a
    # zero-bounded one, which bnec() drops. Asking the same function the fitting
    # path asks is what stops that happening again. See #170.
    use_mods <- suppressMessages(
      check_models(mod_groups$all, range_to_family(object))
    )
  } else {
    stop("Argument `object` must be a bayesnecfit or bayesmanecfit, one of the",
         " model group names (", paste0("\"", names(mod_groups), "\"",
                                        collapse = ", "),
         "), or a numeric range. See ?models.", call. = FALSE)
  }
  mod_params <- show_params(use_mods)
  names(mod_params) <- use_mods
  if (!is.null(max_pars)) {
    n_pars <- vapply(
      mod_params, function(x) length(names(x$pforms)), integer(1)
    )
    mod_params <- mod_params[n_pars <= max_pars]
    if (!length(mod_params)) {
      stop("No selected model equations have ", max_pars,
           " or fewer curve parameters.", call. = FALSE)
    }
  }
  mod_params
}

#' Representative family for a response range
#'
#' Maps the numeric range accepted by \code{\link{models}} onto a family, so
#' that the available models can be looked up with the same
#' \code{check_models()} call \code{\link{bnec}} makes. Any family sharing a
#' range gives the same answer, so one representative per range is enough:
#' \code{bernoulli}, \code{binomial}, \code{beta} and \code{beta_binomial} all
#' behave as \code{Beta} does, and \code{poisson} and \code{negbinomial} as
#' \code{Gamma} does.
#'
#' @param object A \code{\link[base]{numeric}} vector giving the range of the
#' response.
#'
#' @return An object of class \code{\link[stats]{family}}.
#'
#' @importFrom brms Beta
#' @importFrom stats gaussian Gamma
#'
#' @noRd
range_to_family <- function(object) {
  lo <- min(object)
  hi <- max(object)
  if (lo < 0) {
    gaussian(link = "identity")
  } else if (hi <= 1) {
    Beta(link = "identity")
  } else if (is.infinite(hi)) {
    Gamma(link = "identity")
  } else {
    # Previously fell through leaving `use_mods` undefined, so models(c(0, 100))
    # failed with "object 'use_mods' not found".
    stop("A numeric `object` must be one of the response ranges bayesnec",
         " distinguishes: c(-Inf, Inf) for an unbounded response, c(0, 1) for a",
         " 0-1 bounded one, or c(0, Inf) for a zero-bounded one. You supplied a",
         " range of ", lo, " to ", hi, ". See ?models.", call. = FALSE)
  }
}
