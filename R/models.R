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
#' listed.
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
#' Models that have an exponential decay
#' (most models with parameter "beta") with no "bot" parameter are zero-bounded
#' and are not suitable for the Gaussian family, or any family modelled using a
#' logit or log link function. Models with a linear decay
#' (containing the string "lin" in their name) are not suitable for modelling
#' families that are zero bounded (Gamma, Poisson, Negative binomial) using an
#' identity link, nor for families that are 0, 1 bounded (bernoulli, binomial,
#' beta, beta_binomial) using an identity link. Note that the linear-decay
#' restriction applies to the models containing "lin" only
#' ("neclin", "neclinhorme", "ecxlin"); the hormesis models that also carry a
#' "slope" parameter are retained for 0, 1 bounded families, with two
#' exceptions. Models whose hormesis term raises the predictor to a power with
#' no coefficient ("nechormepwr", "nechorme4pwr") are excluded for 0, 1 bounded
#' families under an identity link. The term
#' \code{x^(1 / (1 + exp(slope)))} contributes exactly 1 at \code{x = 1}
#' whatever "slope" is, and below the threshold the decay factor is 1, so the
#' fitted mean is at least \code{top + 1} wherever the predictor reaches 1.
#' There is no parameter value that keeps it inside (0, 1), which is why this is
#' an exclusion rather than a harder search for initial values.
#' "nechormepwr01" is the bounded hormesis form and is retained there;
#' conversely it is excluded for the zero-bounded identity families, being
#' bounded on (0, 1) by construction and so unable to represent a response with
#' no upper bound. Additionally,
#' models that raise the predictor to a fractional power ("ecxsigm",
#' "necsigm", "nechormepwr", "nechorme4pwr") are not suitable where the
#' predictor contains negative values. These restrictions do
#' not need to be controlled by the user and a call to \code{\link{bnec}} with
#' \code{models = "all"} will simply exclude inappropriate models.
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
#'
#' @export
models <- function(object) {
  if (missing(object)) {
    return(mod_groups)
  }
  if (is_bayesnecfit(object)) {
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
