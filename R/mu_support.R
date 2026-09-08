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
#' the real line under that inverse. \code{log}, \code{softplus} and
#' \code{squareplus} give (0, Inf); \code{logit}, \code{probit},
#' \code{probit_approx}, \code{cloglog}, \code{cauchit} and \code{softit}
#' give (0, 1); \code{sqrt} gives \code{eta^2}, so \strong{[0, Inf)} rather
#' than (0, Inf) --- it reaches zero at \code{eta = 0}, which is outside the
#' open support the count and Gamma families require, though on a set of
#' measure zero under continuous sampling; \code{identity} passes the predictor
#' through untouched; and \code{inverse} gives \code{inv(eta)}, which is
#' negative wherever \code{eta} is.
#'
#' \code{1/mu^2} also falls to the default, and correctly: its inverse is
#' \code{eta^(-1/2)}, which is undefined for a non-positive linear predictor.
#'
#' \code{test-mu_support.R} constructs each of the twelve families in
#' \code{mod_fams} against a fixed list of candidate links, keeps those the
#' family accepts, and asserts that none falls to the default except
#' \code{identity}, \code{inverse} and \code{1/mu^2}. That candidate list is
#' complete for brms 2.23.0; a link added to an existing family in a later
#' version would be missed rather than caught, since brms does not expose the
#' accepted set programmatically.
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
    squareplus = c(0, Inf),
    sqrt = c(0, Inf),
    logit = c(0, 1),
    probit = c(0, 1),
    probit_approx = c(0, 1),
    cloglog = c(0, 1),
    cauchit = c(0, 1),
    softit = c(0, 1),
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
    # A dpar the family does not carry. No caller reaches this, and reporting
    # the mean as constrained is the conservative answer if one ever does.
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
#'   \item \code{can_exceed_one}: the mean can exceed 1 for \emph{some}
#'     setting of the parameters, whether or not the fit can shrink the term
#'     responsible. Strictly weaker than admissibility and strictly stronger
#'     than \code{unscaled_excess}: it holds for all six hormesis equations
#'     with an additive or multiplicative excess term, not only the two whose
#'     excess carries no coefficient. Nothing consumes it for
#'     \code{\link{check_models}}, which is right --- an equation whose excess
#'     the fit can shrink is still admissible. It is consumed by #257, which
#'     needs the mean to be provably \emph{strictly} inside (0, 1) before it
#'     may take a logit of it, and for which "the fit can shrink it" is not
#'     good enough.
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
#' \strong{Scope.} Four restrictions, each of which would otherwise be an
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
#'   \item \code{zero_asymptote} states a property of the mean function, but
#'     the gate consuming it tests \code{link} against \code{"logit"} and
#'     \code{"log"} only. The same reachability argument applies to
#'     \code{probit}, \code{cloglog}, \code{cauchit} and \code{softit},
#'     under which no model is currently dropped. The table describes the
#'     property and \code{\link{check_models}} implements a narrower rule;
#'     closing that gap changes which models a fit uses, and is left to the
#'     change that makes the gates derive from this table.
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
    nechorme      = list(can_exceed_one = TRUE),
    nechorme4     = list(can_exceed_one = TRUE),
    necsigm       = list(),
    neclin        = list(below_zero = TRUE),
    neclinhorme   = list(below_zero = TRUE),
    nechormepwr   = list(unscaled_excess = TRUE, can_exceed_one = TRUE),
    nechorme4pwr  = list(unscaled_excess = TRUE, can_exceed_one = TRUE),
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
    ecxhormebc4   = list(can_exceed_one = TRUE),
    ecxhormebc5   = list(can_exceed_one = TRUE)
  )
  flag <- function(x, nm) isTRUE(x[[nm]])
  out <- data.frame(
    model = names(spec),
    below_zero = vapply(spec, flag, logical(1), "below_zero"),
    unscaled_excess = vapply(spec, flag, logical(1), "unscaled_excess"),
    can_exceed_one = vapply(spec, flag, logical(1), "can_exceed_one"),
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

#' Which scale a group-level deviation on the whole curve should be applied on
#'
#' @details A group-level term adds an offset that \pkg{brms} declares
#' unconstrained. Under the identity link \code{\link{bnec}} uses, the mean it
#' is added to is often not unconstrained, and every leapfrog step that carries
#' \code{mu} outside the likelihood's support is rejected by Stan and counted as
#' a divergence. Applying the deviation on a transformed scale makes those
#' excursions impossible by construction, without changing what \code{top},
#' \code{bot}, \code{nec} and \code{beta} mean: the deviation is zero-centred,
#' and \code{m * exp(0)} is \code{m}. See #257.
#'
#' Two gates, and both must pass.
#'
#' \strong{Is a transform needed?} \code{\link{mu_is_constrained}} says the
#' likelihood constrains \code{mu} and the link cannot keep it inside. Under a
#' \code{log} or \code{logit} link the offset is already on the linear predictor
#' and there is nothing to do.
#'
#' \strong{Is a transform applicable?} \code{\link{model_mu_ranges}} says the
#' mean is provably strictly inside the interval, so \code{log} or
#' \code{logit} of it is defined. This is a blocker rather than a caveat for
#' three groups of equations: \code{neclin}, \code{neclinhorme} and
#' \code{ecxlin} are unbounded below, so \code{log} of the mean is \code{NaN};
#' \code{nechormepwr} and \code{nechorme4pwr} can exceed 1, so \code{logit} is
#' \code{NaN}; and \code{nechormepwr01} saturates at exactly 1, where
#' \code{logit} is \code{Inf}. Those keep the additive offset and the raised
#' \code{adapt_delta} permanently.
#'
#' @param model A \code{\link[base]{character}} string naming one equation.
#' @param family A \code{\link[stats]{family}} object.
#'
#' @return \code{"logit"}, \code{"log"}, or \code{"none"}.
#'
#' @seealso \code{\link{mu_is_constrained}}, \code{\link{model_mu_ranges}}
#'
#' @noRd
ogl_transform_kind <- function(model, family) {
  if (is.null(family) || is.null(model) || length(model) != 1) {
    return("none")
  }
  if (!mu_is_constrained(family)) {
    return("none")
  }
  ranges <- model_mu_ranges()
  row <- ranges[ranges$model == model, , drop = FALSE]
  if (nrow(row) != 1) {
    return("none")
  }
  # Unbounded below rules out both transforms: neither log nor logit of a
  # negative mean is defined.
  if (isTRUE(row$below_zero)) {
    return("none")
  }
  support <- mu_support(family)
  if (identical(support, c(0, 1))) {
    # Anything that can reach or pass 1 rules out logit, and "can" means for
    # any setting of the parameters -- not merely for settings the fit cannot
    # shrink. unscaled_excess is the narrower property, and gating on it
    # admitted nechorme, nechorme4, ecxhormebc4 and ecxhormebc5, whose mean can
    # exceed 1 through exp(slope) * x. The collapsed form has a pole at
    # o = log((m - 1) / m) once m > 1 and changes sign across it -- at m = 1.5
    # it returns 8.1e4 at o = -1.0986 and -720 at o = -1.1 -- and those fits
    # would also have lost the adapt_delta raise on the false premise that the
    # mean cannot leave its support.
    if (isTRUE(row$can_exceed_one) || isTRUE(row$ceiling_at_one)) {
      return("none")
    }
    return("logit")
  }
  if (identical(support, c(0, Inf))) {
    return("log")
  }
  "none"
}

#' The collapsed transform for a group-level deviation on the whole curve
#'
#' @details Written in collapsed form, never as the literal
#' \code{inv_logit(logit(m) + o)} sandwich. \code{logit(m)} underflows to
#' \code{-Inf} once the decay term exceeds about 709, and
#' \code{inv_logit(-Inf + o)} is exactly 0, which fails the likelihood's
#' positivity check just as surely as \code{mu > 1} does. The collapsed forms
#' are stable as \code{m -> 0}, which is the region occupied by the tail of a
#' declining curve such as \code{nec3param}. See #257.
#'
#' \code{logit}: \code{mu = m e^o / (1 - m + m e^o)}, which is
#' \code{odds(mu) = odds(m) e^o}.
#'
#' \code{log}: \code{mu = m e^o}.
#'
#' @param kind \code{"logit"} or \code{"log"}.
#' @param m A \code{\link[base]{character}} string naming the curve term.
#' @param o A \code{\link[base]{character}} string naming the deviation term.
#'
#' @return A \code{\link[base]{character}} string.
#' @noRd
ogl_transform_expr <- function(kind, m = "bnecmu", o = "ogl") {
  switch(
    kind,
    logit = paste0(m, " * exp(", o, ") / (1 - ", m, " + ", m,
                   " * exp(", o, "))"),
    log = paste0(m, " * exp(", o, ")"),
    stop("kind must be \"logit\" or \"log\".", call. = FALSE)
  )
}

#' The parameters a group-level deviation may be applied multiplicatively on
#'
#' @details \code{top} and \code{bot} are the two parameters on the response
#' scale, and \code{\link{define_prior}} bounds both of them to the support of
#' the mean --- \code{lb = 0} and \code{ub = 1} for the unit-interval families,
#' \code{lb = 0} for the count and Gamma families. Every other parameter is on
#' the predictor scale (\code{nec}, \code{ec50}) or is dimensionless and enters
#' through an exponential (\code{beta}, \code{slope}, \code{d}, \code{f}), so
#' an unconstrained deviation on it cannot put the parameter itself outside a
#' range the likelihood requires, and \code{log} or \code{logit} of it is not
#' generally defined --- \code{nec} on a log predictor is routinely negative.
#' See #294.
#'
#' @return A \code{\link[base]{character}} vector.
#'
#' @noRd
par_transform_pars <- function() {
  c("top", "bot")
}

#' Which scale a group-level deviation on a single parameter should be applied on
#'
#' @details The parameter-level counterpart of \code{\link{ogl_transform_kind}}.
#' \code{pgl()} and an explicit \code{(par | group)} term add the deviation to
#' the parameter on the parameter's own scale, and \pkg{brms} declares it
#' unconstrained, so where that parameter is against a boundary of the mean's
#' support the same excursions #257 removed from the mean occur on the
#' parameter. \code{bot} is the case that matters: it is the lower asymptote, it
#' is routinely estimated close to zero, and a deviation large enough to take it
#' below zero makes the likelihood undefined. Measured on \code{herbicide} with
#' \code{Beta(link = "identity")} and \code{nec4param}, a \code{(bot | herbicide)}
#' term gives 51 divergent transitions of 2000 at \code{adapt_delta = 0.95} where
#' the same term on a \code{gaussian} response gives none. See #294.
#'
#' \strong{The gate is the family, not the equation}, and this is where the
#' judgement differs from \code{\link{ogl_transform_kind}}. That function asks
#' whether the \emph{mean} is provably strictly inside the interval, which
#' depends on the equation: \code{nechorme} can exceed 1 through
#' \code{exp(slope) * x}, so \code{logit} of its mean is undefined. A parameter
#' is not the mean. \code{top} and \code{bot} are bounded to the family's support
#' by their own priors whatever equation they appear in, so
#' \code{\link{model_mu_ranges}} is not consulted and the transform is defined
#' for \code{nechorme}, \code{nechorme4}, \code{nechormepwr01},
#' \code{ecxhormebc4} and \code{ecxhormebc5} even though the mean transform is
#' not. Those equations keep the raised \code{adapt_delta} for the separate
#' reason that their mean can leave the support with every parameter inside it;
#' see \code{\link{add_brm_defaults}}.
#'
#' @param family A \code{\link[stats]{family}} object.
#'
#' @return \code{"logit"}, \code{"log"}, or \code{"none"}.
#'
#' @seealso \code{\link{ogl_transform_kind}}, \code{\link{ogl_transform_expr}}
#'
#' @noRd
par_transform_kind <- function(family) {
  if (is.null(family) || !mu_is_constrained(family)) {
    return("none")
  }
  support <- mu_support(family)
  if (identical(support, c(0, 1))) {
    return("logit")
  }
  if (identical(support, c(0, Inf))) {
    return("log")
  }
  "none"
}

#' Whether a group-level term on one parameter takes the multiplicative form
#'
#' @param par A \code{\link[base]{character}} string naming one parameter.
#' @param kind The output of \code{\link{par_transform_kind}}.
#'
#' @return A \code{\link[base]{logical}}.
#' @noRd
par_is_transformed <- function(par, kind) {
  !identical(kind, "none") && par %in% par_transform_pars()
}

#' The names of the two terms a transformed parameter introduces
#'
#' @details \code{bot} keeps its name and stays a population-level non-linear
#' parameter, so \code{b_bot_Intercept} and the prior
#' \code{\link{define_prior}} builds for it are unchanged and every estimate
#' function that reads them is unaffected. What is new is \code{botgl}, the
#' deviation, and \code{bnecbot}, the intermediate the curve reads in place of
#' \code{bot}. The naming follows \code{bnecmu} from #257 for the same reason:
#' the generated term is the one that is renamed, never the parameter the user
#' asked about.
#'
#' @param par A \code{\link[base]{character}} string naming one parameter.
#'
#' @return A named \code{\link[base]{character}} vector of length two.
#' @noRd
par_gl_names <- function(par) {
  c(dev = paste0(par, "gl"), inter = paste0("bnec", par))
}

#' Whether an equation's mean is confined to the support by its own parameters
#'
#' @details The question the \code{adapt_delta} raise turns on. With every
#' deviation applied on a scale it cannot leave, a group-level term can still
#' put \code{mu} outside the support if the equation's mean is not bounded by
#' \code{top} and \code{bot}. Three properties break that, and any one of them
#' is enough:
#'
#' \itemize{
#'   \item \code{below_zero} --- \code{neclin}, \code{neclinhorme} and
#'     \code{ecxlin}, whose mean is unbounded below.
#'   \item \code{can_exceed_one} --- the six hormesis equations with an excess
#'     term in \code{exp(slope) * x}.
#'   \item \code{ceiling_at_one} --- \code{nechormepwr01}.
#' }
#'
#' \strong{All three are tested whatever the family's support is}, and that is
#' where this differs from \code{\link{ogl_transform_kind}}, which tests
#' \code{can_exceed_one} and \code{ceiling_at_one} on the \code{(0, 1)} branch
#' only because they are about a \code{logit} being defined. They are not only
#' about that here. An excess term in \code{exp(slope) * x} makes the mean
#' negative for a sufficiently negative predictor --- \code{nechorme}'s mean is
#' negative for \code{x < -top / exp(slope)} --- and \code{crf(log(x), ...)}
#' supplies a negative predictor as a matter of course. So the hormesis
#' equations can leave a \code{(0, Inf)} support as well, and a deviation on
#' \code{slope} increases \code{exp(slope)} directly.
#'
#' Delegating this to \code{\link{ogl_transform_kind}} dropped the raise for
#' those equations under \code{Gamma}, \code{poisson} and \code{negbinomial},
#' where 2.1.4 applied it. See #294.
#'
#' @param model A \code{\link[base]{character}} string naming one equation.
#'
#' @return A \code{\link[base]{logical}}.
#' @noRd
mu_confined_by_pars <- function(model) {
  if (is.null(model) || length(model) != 1) {
    return(FALSE)
  }
  ranges <- model_mu_ranges()
  row <- ranges[ranges$model == model, , drop = FALSE]
  if (nrow(row) != 1) {
    return(FALSE)
  }
  !isTRUE(row$below_zero) && !isTRUE(row$can_exceed_one) &&
    !isTRUE(row$ceiling_at_one)
}

#' Every generated term name a group-level structure can introduce
#'
#' @details Used by \code{\link{check_reserved_names}} to refuse a data column
#' that would be resolved in place of one of them, and by
#' \code{\link{add_brm_defaults}} to drop their rows from the prior set the
#' initial-value search reads. Enumerated rather than derived from the formula
#' so that both callers refuse the same set whatever the formula turns out to
#' be.
#'
#' @return A \code{\link[base]{character}} vector.
#' @noRd
generated_term_names <- function() {
  c("bnecmu", "ogl", unlist(lapply(par_transform_pars(), par_gl_names),
                            use.names = FALSE))
}

#' The deviation intercepts a group-level structure leaves unidentified
#'
#' @details \code{ogl} enters as an offset on the whole curve, so a constant
#' added to it can be taken back out of \code{top} and \code{bot} with no
#' change to the likelihood. Its population intercept is therefore not
#' identified by the data, is given a zero-centred prior by
#' \code{\link{define_group_prior}}, and is started at zero rather than at
#' Stan's own draw. See #245.
#'
#' The parameter-level transform has the same non-identifiability and resolves
#' it differently: the deviation is written with no population intercept at all,
#' so there is nothing here to initialise. See \code{\link{add_par_gl_term}}
#' and #294.
#'
#' @param group_spec The output of \code{\link{parse_group_terms}}.
#' @param family A \code{\link[stats]{family}} object.
#'
#' @return A \code{\link[base]{character}} vector, possibly empty.
#' @noRd
group_zero_intercepts <- function(group_spec, family) {
  if (is.null(group_spec)) {
    return(character(0))
  }
  # Only ogl. A transformed parameter deviation is written botgl ~ 0 + (1 |
  # group) and has no population intercept to initialise; see
  # add_par_gl_term(). family is kept in the signature because which terms a
  # formula generates is a property of it, and a caller should not have to know
  # that the answer happens not to depend on it today.
  if (isTRUE(group_spec$ogl)) "ogl" else character(0)
}
