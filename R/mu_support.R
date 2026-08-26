#' The interval the response distribution allows the mean to occupy
#'
#' @param family A \code{\link[stats]{family}} object, already through
#' \code{\link{validate_family}}.
#' @param dpar Which block of a two-block family to describe. \code{"mu"}, the
#' default, is the block the curve's own parameters live on; \code{"hu"} and
#' \code{"zi"} are the hurdle and zero-inflation probabilities.
#'
#' @details This is a property of the \emph{response distribution} and does not
#' depend on the link. It says where the mean must lie for the likelihood to be
#' defined:
#'
#' \itemize{
#'   \item \code{gaussian} is unconstrained. The data enter only through the
#'     residual, and a negative fitted mean is an ordinary prediction rather
#'     than an invalid one. See #206.
#'   \item \code{bernoulli}, \code{beta}, \code{binomial},
#'     \code{beta_binomial} and the \code{mu} block of
#'     \code{zero_inflated_beta} are on (0, 1).
#'   \item \code{Gamma}, \code{poisson}, \code{negbinomial}, the
#'     zero-inflated counts and the \code{mu} block of \code{hurdle_gamma} are
#'     on (0, Inf).
#'   \item the \code{hu} and \code{zi} blocks are probabilities, so (0, 1)
#'     whatever the \code{mu} block is. \code{hurdle_gamma} is the case where
#'     the two blocks differ, and it is why this takes \code{dpar} at all:
#'     \code{\link{check_models}} applies both blocks' restrictions at once.
#' }
#'
#' Whether a \emph{proposal} can reach outside this interval is a different
#' question, answered by \code{\link{mu_is_constrained}}, because the link
#' decides it.
#'
#' @return A \code{\link[base]{numeric}} vector of length two.
#'
#' @seealso \code{\link{model_mu_ranges}}, which answers the complementary
#' question of whether a given model's mean can leave that interval.
#'
#' @noRd
mu_support <- function(family, dpar = "mu") {
  dpar <- match.arg(dpar, c("mu", "hu", "zi"))
  if (dpar %in% c("hu", "zi")) {
    return(c(0, 1))
  }
  if (is.null(family) || is.null(family$family)) {
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

#' Links whose inverse cannot map into the response distribution's support
#'
#' @details \pkg{brms} applies the inverse link to the linear predictor before
#' the likelihood is evaluated, so under most links the mean is valid by
#' construction whatever the sampler proposes: \code{log} gives
#' \code{exp(eta)}, \code{probit} gives \code{Phi(eta)}, \code{cloglog}
#' gives \code{inv_cloglog(eta)}, and \code{logit} is usually absorbed into a
#' \code{_logit} likelihood variant. Two links are not like that:
#'
#' \itemize{
#'   \item \code{identity} passes the linear predictor through untouched, so
#'     the mean is whatever the curve produces;
#'   \item \code{inverse} gives \code{inv(eta)}, which is negative wherever
#'     \code{eta} is. Verified against the generated Stan code for
#'     \code{Gamma(link = "inverse")}, which emits \code{mu = inv(mu)} and
#'     then \code{gamma_lpdf(Y | shape, shape ./ mu)}.
#' }
#'
#' An unrecognised link is treated as not guaranteeing, which is the
#' conservative direction: the only consequence is that
#' \code{\link{add_brm_defaults}} raises \code{adapt_delta} where it may not
#' be needed.
#'
#' @return A \code{\link[base]{character}} vector.
#'
#' @noRd
unguarded_links <- function() {
  c("identity", "inverse")
}

#' Can a proposal make the likelihood undefined?
#'
#' @param family A \code{\link[stats]{family}} object.
#' @param dpar Passed to \code{\link{mu_support}}.
#'
#' @details True when the support is bounded at either end \emph{and} the link
#' does not guarantee the mean lands inside it. Where both hold, an
#' unconstrained group-level deviation added to the mean can carry it out of the
#' interval and Stan rejects the proposal. See \code{\link{add_brm_defaults}}
#' and #245.
#'
#' @return A \code{\link[base]{logical}} of length one.
#'
#' @noRd
mu_is_constrained <- function(family, dpar = "mu") {
  if (is.null(family) || is.null(family$link)) {
    return(FALSE)
  }
  if (!family$link %in% unguarded_links()) {
    return(FALSE)
  }
  any(is.finite(mu_support(family, dpar = dpar)))
}

#' What each model's mean can produce
#'
#' @details The counterpart of \code{\link{mu_support}}: that function says
#' what the likelihood permits, this one says what the mean function can
#' produce. Together they decide whether a model is admissible for a response.
#'
#' \strong{Two kinds of flag, and they are not interchangeable.} Conflating
#' them is what produces the \code{nechormepwr01} entry in
#' \code{\link{check_models}} that looks like a discrepancy and is not. See
#' #256.
#'
#' \emph{Support} --- can the mean leave the interval the likelihood defines?
#' Both flags below are evaluated for a non-negative predictor and for
#' population-level parameters inside a (0, 1) response's constraints; see
#' \strong{Scope} for why that matters.
#'
#' \itemize{
#'   \item \code{below_zero}: the mean is unbounded below, because a linear
#'     term is subtracted from it with nothing to stop it. \code{neclin},
#'     \code{neclinhorme} and \code{ecxlin}.
#'   \item \code{unscaled_excess}: the mean can exceed 1 through a term
#'     carrying \strong{no coefficient}, so the fit cannot shrink it.
#'     \code{nechormepwr} and \code{nechorme4pwr}, whose hormesis term is
#'     \code{x^(1 / (1 + exp(slope)))}: the exponent lies in (0, 1), so at
#'     \code{x = 1} the term contributes exactly 1 whatever \code{slope} is.
#'     \code{nechorme}, \code{nechorme4}, \code{ecxhormebc4} and
#'     \code{ecxhormebc5} can also exceed 1, through \code{exp(slope) * x},
#'     but that term has a coefficient the fit can drive towards zero, which is
#'     why they are admitted on a 0-1 bounded response and these two are not.
#' }
#'
#' \emph{Reachability} --- can the mean function cover the range a link scale
#' requires?
#'
#' \itemize{
#'   \item \code{zero_asymptote}: the mean decays onto zero and has no free
#'     lower asymptote, so it cannot produce the negative values a \code{log}
#'     or \code{logit} linear predictor needs. Derived as "carries no
#'     \code{bot} and is not unbounded below", which reproduces
#'     \code{mod_groups$zero_bounded} exactly.
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
#' \strong{Scope.} Three restrictions, each of which would otherwise be an
#' unstated assumption:
#'
#' \itemize{
#'   \item \code{below_zero} is evaluated for \code{x >= 0}. Four hormesis
#'     equations with \code{below_zero = FALSE} do produce a negative mean at a
#'     negative predictor; \code{\link{check_models}} has a separate gate for a
#'     predictor containing negative values, which this table does not yet
#'     describe.
#'   \item \code{unscaled_excess} is stated against 1 rather than against the
#'     model's own level, so it is specific to a (0, 1) bounded response. On a
#'     count response nothing caps the mean and the flag has no work to do.
#'   \item Which \emph{individual parameters} can carry the mean out of range
#'     is deliberately not recorded. It is not a per-parameter property: whether
#'     a deviation on \code{nec} takes \code{nechorme} above 1 depends on where
#'     \code{slope} and \code{top} are, so any such column needs an
#'     existential definition over the remaining parameters, and would need a
#'     derivation test to match. Left to #257, which is where it would be
#'     consumed.
#' }
#'
#' Two observations from reading the 23 equations, recorded because both
#' contradict abbreviated rules in circulation. \code{slope} is passed through
#' \code{exp()} in all ten models that carry it, so a deviation on it can never
#' make \code{exp(slope)} invalid; what it does is set a \emph{level}, and that
#' is what leaves the interval. It is harmless in exactly one of the ten,
#' \code{nechormepwr01}, whose factor is bounded by \code{max(top, 1)}. And of
#' the 21 equations carrying \code{beta}, it is harmless in 20 and harmful in
#' \code{neclinhorme} alone, where it enters as a subtractive linear term
#' rather than inside a decay factor bounded in (0, 1].
#'
#' @return A \code{\link[base]{data.frame}}, one row per model in
#' \code{\link{models}}.
#'
#' @noRd
model_mu_ranges <- function() {
  spec <- list(
    nec3param     = list(),
    nec4param     = list(),
    nechorme      = list(),
    nechorme4     = list(),
    necsigm       = list(),
    neclin        = list(below_zero = TRUE),
    neclinhorme   = list(below_zero = TRUE),
    nechormepwr   = list(unscaled_excess = TRUE),
    nechorme4pwr  = list(unscaled_excess = TRUE),
    nechormepwr01 = list(ceiling_at_one = TRUE),
    ecxlin        = list(below_zero = TRUE),
    ecxexp        = list(),
    ecxsigm       = list(),
    ecx4param     = list(),
    ecxwb1        = list(),
    ecxwb2        = list(),
    ecxwb1p3      = list(),
    ecxwb2p3      = list(),
    ecxll5        = list(),
    ecxll4        = list(),
    ecxll3        = list(),
    ecxhormebc4   = list(),
    ecxhormebc5   = list()
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
  rownames(out) <- NULL
  out
}
