#' The interval the response distribution allows the mean to occupy
#'
#' @param family A \code{\link[stats]{family}} object, already through
#' \code{\link{validate_family}}.
#'
#' @details Answers one question: given the family \emph{and the link as
#' actually fitted}, what values may \code{mu} take before the likelihood
#' becomes undefined? A proposal reaching a value outside the returned interval
#' is rejected by Stan rather than merely improbable.
#'
#' Under a \code{log} or \code{logit} link \code{mu} is the linear predictor and
#' spans the whole real line, whatever the family. Under any other link:
#'
#' \itemize{
#'   \item \code{gaussian} is unconstrained. The data enter only through the
#'     residual, and a negative fitted mean is an ordinary prediction rather
#'     than an invalid one. See #206.
#'   \item \code{bernoulli}, \code{beta}, \code{binomial}, \code{beta_binomial}
#'     and the \code{mu} block of \code{zero_inflated_beta} are on (0, 1).
#'   \item \code{Gamma}, \code{poisson}, \code{negbinomial}, the zero-inflated
#'     counts and the \code{mu} block of \code{hurdle_gamma} are on (0, Inf).
#' }
#'
#' The \code{hu} block of a hurdle family is a probability and is always on
#' (0, 1); this function describes the \code{mu} block, which is the one the
#' curve's own parameters live on.
#'
#' \strong{The family tag alone is not a sufficient key}, which is why this
#' takes the whole family object. \code{\link{validate_family}} assigns the
#' identity link only where the user did not choose one; an explicitly supplied
#' \code{log} or \code{logit} link is honoured and changes the answer.
#'
#' @return A \code{\link[base]{numeric}} vector of length two, the open interval
#' \code{mu} may occupy.
#'
#' @seealso \code{\link{model_mu_ranges}}, which answers the complementary
#' question of whether a given model's mean can leave that interval.
#'
#' @noRd
mu_support <- function(family) {
  if (is.null(family) || is.null(family$link) || is.null(family$family)) {
    return(c(-Inf, Inf))
  }
  if (family$link %in% c("log", "logit")) {
    return(c(-Inf, Inf))
  }
  if (identical(family$family, "gaussian")) {
    return(c(-Inf, Inf))
  }
  if (family$family %in% unit_interval_families()) {
    return(c(0, 1))
  }
  c(0, Inf)
}

#' Families whose mean is a probability or a proportion
#'
#' @return A \code{\link[base]{character}} vector.
#'
#' @noRd
unit_interval_families <- function() {
  c("bernoulli", "beta", "binomial", "beta_binomial", "zero_inflated_beta")
}

#' Does the response distribution restrict the range of the mean?
#'
#' @param family A \code{\link[stats]{family}} object.
#'
#' @details The coarsest question \code{\link{mu_support}} answers: is the
#' interval bounded at either end? Where it is, an unconstrained group-level
#' deviation added to the mean can carry it out of the interval and the
#' proposal is rejected. See \code{\link{add_brm_defaults}} and #245.
#'
#' @return A \code{\link[base]{logical}} of length one.
#'
#' @noRd
mu_is_constrained <- function(family) {
  any(is.finite(mu_support(family)))
}

#' What each model's mean can do, and which parameters can take it there
#'
#' @details The counterpart of \code{\link{mu_support}}: that function says what
#' the likelihood permits, this one says what the mean function can produce.
#' Together they decide whether a model is admissible for a response.
#'
#' \strong{Two kinds of flag, and they are not interchangeable.} Conflating them
#' is what produced the \code{nechormepwr01} entry in \code{\link{check_models}}
#' that looks like a discrepancy and is not. See #256.
#'
#' \emph{Support} --- can the mean leave the interval the likelihood defines?
#'
#' \itemize{
#'   \item \code{below_zero}: the mean is unbounded below, because a linear term
#'     is subtracted from it with nothing to stop it. \code{neclin},
#'     \code{neclinhorme} and \code{ecxlin}.
#'   \item \code{unscaled_excess}: the mean can exceed its own level through a
#'     term carrying \strong{no coefficient}, so the fit cannot shrink it.
#'     \code{nechormepwr} and \code{nechorme4pwr}, whose hormesis term is
#'     \code{x^(1 / (1 + exp(slope)))}: the exponent lies in (0, 1), so at
#'     \code{x = 1} the term contributes exactly 1 whatever \code{slope} is.
#'     The other hormesis models can also exceed their level, through
#'     \code{exp(slope) * x}, but that term has a coefficient the fit can drive
#'     towards zero, which is why they are allowed on a bounded response and
#'     these two are not.
#' }
#'
#' \emph{Reachability} --- can the mean function cover the range a link scale
#' requires?
#'
#' \itemize{
#'   \item \code{zero_asymptote}: the mean decays onto zero and has no free
#'     lower asymptote, so it cannot produce the negative values a \code{log} or
#'     \code{logit} linear predictor needs. This is the property
#'     \code{mod_groups$zero_bounded} records, derived here rather than listed.
#' }
#'
#' \emph{Appropriateness} --- is the shape meaningful for the response, whether
#' or not it is valid?
#'
#' \itemize{
#'   \item \code{ceiling_at_one}: the mean saturates at exactly 1 by
#'     construction. \code{nechormepwr01}, whose hormesis term is
#'     \code{1 / (1 + ((1 / top) - 1) * exp(-exp(slope) * x))}. For \code{top}
#'     below 1 that term rises towards 1, which is the intended hormetic
#'     increase on a 0-1 response. For \code{top} above 1 --- an ordinary count
#'     or Gamma mean --- it \emph{falls} towards 1, expressing a decline where
#'     hormesis is intended, and cannot represent a mean above \code{top} at
#'     all. \code{mu} stays positive throughout, so the model is valid and
#'     unsuitable at the same time, and only an appropriateness flag excludes
#'     it.
#' }
#'
#' \code{unsafe} lists the parameters a group-level deviation on which can take
#' the mean outside the model's own range. Two entries contradict the
#' abbreviated rules in circulation. \code{slope} is passed through \code{exp()}
#' in all ten models that carry it, so a deviation on it can never make
#' \code{exp(slope)} invalid --- but it sets a \emph{level}, the height of the
#' hormetic peak or the rate of linear decline, and that is what leaves the
#' interval. And \code{beta} is safe in every model except \code{neclinhorme},
#' where it enters as a subtractive linear term rather than inside a decay
#' factor bounded in (0, 1].
#'
#' @return A \code{\link[base]{data.frame}}, one row per model in
#' \code{\link{models}}, with a list column \code{unsafe}.
#'
#' @noRd
model_mu_ranges <- function() {
  spec <- list(
    nec3param     = list(unsafe = "top"),
    nec4param     = list(unsafe = c("top", "bot")),
    nechorme      = list(unsafe = c("top", "slope")),
    nechorme4     = list(unsafe = c("top", "bot", "slope")),
    necsigm       = list(unsafe = "top"),
    neclin        = list(unsafe = c("top", "slope", "nec"), below_zero = TRUE),
    neclinhorme   = list(unsafe = c("top", "slope", "beta", "nec"),
                         below_zero = TRUE),
    nechormepwr   = list(unsafe = c("top", "slope"), unscaled_excess = TRUE),
    nechorme4pwr  = list(unsafe = c("top", "bot", "slope"),
                         unscaled_excess = TRUE),
    nechormepwr01 = list(unsafe = "top", ceiling_at_one = TRUE),
    ecxlin        = list(unsafe = c("top", "slope"), below_zero = TRUE),
    ecxexp        = list(unsafe = "top"),
    ecxsigm       = list(unsafe = "top"),
    ecx4param     = list(unsafe = c("top", "bot")),
    ecxwb1        = list(unsafe = c("top", "bot")),
    ecxwb2        = list(unsafe = c("top", "bot")),
    ecxwb1p3      = list(unsafe = "top"),
    ecxwb2p3      = list(unsafe = "top"),
    ecxll5        = list(unsafe = c("top", "bot")),
    ecxll4        = list(unsafe = c("top", "bot")),
    ecxll3        = list(unsafe = "top"),
    ecxhormebc4   = list(unsafe = c("top", "slope")),
    ecxhormebc5   = list(unsafe = c("top", "bot", "slope"))
  )
  flag <- function(x, nm) isTRUE(x[[nm]])
  out <- data.frame(
    model = names(spec),
    below_zero = vapply(spec, flag, logical(1), "below_zero"),
    unscaled_excess = vapply(spec, flag, logical(1), "unscaled_excess"),
    ceiling_at_one = vapply(spec, flag, logical(1), "ceiling_at_one"),
    stringsAsFactors = FALSE
  )
  # zero_asymptote is derived rather than listed: a model has it when it carries
  # no bot parameter and its mean is not unbounded below, which leaves decay
  # onto zero as the only possibility. That is exactly the membership of
  # mod_groups$zero_bounded, and test-mu_support.R asserts the two agree, so a
  # model added to one and not the other is a test failure rather than the
  # silent disagreement #170 recorded.
  has_bot <- vapply(out$model, function(m) {
    "bot" %in% names(get(paste0("bf_", m))[[2]])
  }, logical(1))
  out$zero_asymptote <- !has_bot & !out$below_zero
  out$unsafe <- lapply(spec, function(x) x$unsafe)
  rownames(out) <- NULL
  out
}
