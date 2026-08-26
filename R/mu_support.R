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
  if (is.null(family) || is.null(family$family)) {
    return(c(-Inf, Inf))
  }
  if (dpar %in% c("hu", "zi")) {
    return(c(0, 1))
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

#' The interval an inverse link maps the linear predictor into
#'
#' @param link A \code{\link[base]{character}} string naming a link.
#'
#' @details \pkg{brms} applies the inverse link to the linear predictor before
#' the likelihood is evaluated, so what reaches the likelihood is the image of
#' the real line under that inverse. \code{log} gives \code{exp(eta)} on
#' (0, Inf); \code{logit}, \code{probit}, \code{cloglog} and \code{cauchit}
#' give (0, 1); \code{sqrt} and \code{softplus} give (0, Inf);
#' \code{identity} passes the predictor through untouched; and \code{inverse}
#' gives \code{inv(eta)}, which is negative wherever \code{eta} is.
#'
#' An unrecognised link returns the whole real line, which is the conservative
#' answer: it never lies inside a bounded support, so
#' \code{\link{mu_is_constrained}} reports the mean as reachable and the only
#' consequence is that \code{\link{add_brm_defaults}} raises
#' \code{adapt_delta} where it may not be needed.
#'
#' @return A \code{\link[base]{numeric}} vector of length two.
#'
#' @noRd
link_range <- function(link) {
  switch(link,
    log = c(0, Inf),
    softplus = c(0, Inf),
    sqrt = c(0, Inf),
    logit = c(0, 1),
    probit = c(0, 1),
    probit_approx = c(0, 1),
    cloglog = c(0, 1),
    cauchit = c(0, 1),
    c(-Inf, Inf)
  )
}

#' Can a proposal make the likelihood undefined?
#'
#' @param family A \code{\link[stats]{family}} object.
#' @param dpar Which block to ask about; passed to \code{\link{mu_support}}.
#'
#' @details True when the mean the likelihood receives is not guaranteed to lie
#' in the interval that likelihood requires. That is a property of the family
#' and the link \strong{together}, and neither alone decides it: the test is
#' whether \code{\link{link_range}} lies inside \code{\link{mu_support}}.
#'
#' \code{Beta(link = "log")} is the case that makes the point.
#' \code{exp(eta)} is positive but unbounded above, so on a response requiring
#' (0, 1) it can hand \code{beta_lpdf} a negative second shape parameter --- the
#' same failure an identity link produces, under a link that guarantees a valid
#' mean for every count family. Confirmed against the generated Stan code, which
#' emits \code{mu = exp(mu)} then
#' \code{beta_lpdf(Y | mu .* phi, (1 - mu) .* phi)}.
#'
#' Where both hold --- a bounded support, and a link that does not map into it
#' --- an unconstrained group-level deviation added to the mean can carry it out
#' of the interval and Stan rejects the proposal. See
#' \code{\link{add_brm_defaults}} and #245.
#'
#' For a two-block family the link asked is the one belonging to the block:
#' \code{link_hu} for \code{dpar = "hu"}, \code{link_zi} for \code{"zi"}.
#'
#' @return A \code{\link[base]{logical}} of length one.
#'
#' @noRd
mu_is_constrained <- function(family, dpar = "mu") {
  if (is.null(family) || is.null(family$family)) {
    return(FALSE)
  }
  support <- mu_support(family, dpar = dpar)
  if (all(is.infinite(support))) {
    return(FALSE)
  }
  link <- if (identical(dpar, "mu")) {
    family$link
  } else {
    family[[paste0("link_", dpar)]]
  }
  if (is.null(link)) {
    return(TRUE)
  }
  reachable <- link_range(link)
  !(reachable[1] >= support[1] && reachable[2] <= support[2])
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
#'   \item \code{below_zero} is evaluated for \code{x >= 0}. At a negative
#'     predictor \code{nechorme}, \code{nechorme4}, \code{ecxhormebc4} and
#'     \code{ecxhormebc5} do return a negative mean despite carrying
#'     \code{below_zero = FALSE}, and \strong{no gate excludes them}.
#'     \code{\link{check_models}} does have a separate gate for a predictor
#'     containing negative values, but it drops a different and disjoint set ---
#'     \code{necsigm}, \code{ecxsigm}, \code{nechormepwr} and
#'     \code{nechorme4pwr} --- and for a different reason, a fractional power
#'     of a negative base being undefined rather than the mean being negative.
#'     That sixth gate is not described by this table.
#'   \item \code{unscaled_excess} is stated against 1 rather than against the
#'     model's own level, so it is specific to a (0, 1) bounded response. On a
#'     count response nothing caps the mean and the flag has no work to do.
#'   \item Which \emph{individual parameters} can carry the mean out of range
#'     is deliberately not recorded as a column. A well-formed definition exists
#'     --- a deviation on the parameter takes the mean outside the interval for
#'     \emph{some} setting of the others inside their priors --- but nothing
#'     consumes it yet, so it cannot be tied to a requirement, and an untested
#'     column is what this file exists to avoid. Left to #257, where the
#'     transform decides which parameters it applies to.
#' }
#'
#' Two observations from reading the 23 equations are recorded here rather than
#' as a column, because both contradict abbreviated rules in circulation and
#' both are asserted in \code{test-mu_support.R} over the equations that are
#' actually admissible for a (0, 1) response.
#'
#' \code{slope} is passed through \code{exp()} in all ten equations that carry
#' it, so a deviation on it can never make \code{exp(slope)} invalid. What it
#' does is set a \emph{level} --- the height of the hormetic peak, or the rate
#' of linear decline --- and that is what leaves the interval. Of the five
#' slope-bearing equations admissible on a (0, 1) response, a deviation on
#' \code{slope} alone takes the mean above 1 in four, and cannot in
#' \code{nechormepwr01}, whose factor is bounded by \code{max(top, 1)}.
#'
#' \code{beta} is carried by 21 of the 23 equations, \code{neclin} and
#' \code{ecxlin} being the two without. In every equation admissible on a
#' (0, 1) response a deviation on \code{beta} alone leaves the mean in range,
#' because it enters through a factor bounded in (0, 1]. \code{neclinhorme} is
#' the equation where it does not --- it enters there as a subtractive linear
#' term --- and that equation is excluded from bounded responses on
#' \code{below_zero} in any case.
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
