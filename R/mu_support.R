#' The interval the response distribution allows the mean to occupy
#'
#' @param family A \code{\link[stats]{family}} object, already through
#' \code{\link{validate_family}}.
#' @param dpar Which block of a two-block family to describe: \code{"mu"}, the
#' default, or the probability blocks \code{"hu"} and \code{"zi"}.
#'
#' @details A property of the response distribution, not of the link.
#' \code{gaussian} is unconstrained: a negative fitted mean is an ordinary
#' prediction there. The \code{hu} and \code{zi} blocks are probabilities
#' whatever the \code{mu} block is. Whether a proposal can leave this interval
#' depends on the link as well; see \code{\link{mu_is_constrained}}.
#'
#' @return A \code{\link[base]{numeric}} vector of length two.
#'
#' @seealso \code{\link{model_mu_ranges}}, which says whether an equation's
#' mean can leave that interval.
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
#' @details \code{sqrt} maps onto [0, Inf) and reaches zero only at
#' \code{eta = 0}, a set of measure zero under continuous sampling. The support
#' check therefore treats its range as (0, Inf). \code{identity},
#' \code{inverse}, \code{1/mu^2} and any
#' unrecognised link fall to the default, the whole real line. That is the
#' conservative answer: \code{\link{mu_is_constrained}} then reports the mean
#' as reachable, and \code{\link{add_brm_defaults}} raises \code{adapt_delta}
#' where it may not be needed.
#'
#' \code{test-mu_support.R} checks this against a fixed list of links for each
#' family. \pkg{brms} does not expose the links it accepts, so a link it adds
#' later falls to the default without a test failing.
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

#' Whether a proposal can make the likelihood undefined
#'
#' @param family A \code{\link[stats]{family}} object.
#' @param dpar Which block to ask about; passed to \code{\link{mu_support}}.
#'
#' @details \code{TRUE} where \code{\link{link_range}} does not lie inside
#' \code{\link{mu_support}}. The family and the link decide this together:
#' \code{Beta(link = "log")} is constrained, because \code{exp(eta)} can exceed
#' 1, although a log link keeps the mean of every count family valid. Where it
#' is \code{TRUE}, a group-level deviation added to the mean can take it
#' outside the interval, and Stan rejects the proposal; see
#' \code{\link{add_brm_defaults}}.
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
    # A dpar the family does not have. No caller reaches this, and reporting
    # the mean as constrained is the conservative answer if one ever does.
    return(TRUE)
  }
  reachable <- link_range(link)
  !(reachable[1] >= support[1] && reachable[2] <= support[2])
}

#' What each equation's mean can produce
#'
#' @details The counterpart of \code{\link{mu_support}}: that function says
#' what the likelihood permits, and this one what the mean function can
#' produce. The flags answer different questions and are not interchangeable.
#'
#' \itemize{
#'   \item \code{below_zero}: the mean is unbounded below, through a
#'     subtracted linear term (\code{neclin}, \code{neclinhorme},
#'     \code{ecxlin}).
#'   \item \code{can_exceed_one}: the mean can exceed 1 for some setting of
#'     the parameters, including where the fit can shrink the term
#'     responsible. Read where the mean must be strictly inside (0, 1) before
#'     a logit of it is taken.
#'   \item \code{unscaled_excess}: the mean can exceed 1 through a term with
#'     no coefficient, which the fit cannot shrink (\code{nechormepwr},
#'     \code{nechorme4pwr}). \code{\link{check_models}} drops these two from a
#'     (0, 1) response and admits the other \code{can_exceed_one} equations.
#'   \item \code{ceiling_at_one}: the mean saturates at exactly 1
#'     (\code{nechormepwr01}). Where \code{top} is above 1, as on a count or
#'     Gamma response, its hormesis term falls towards 1 instead of rising, so
#'     the equation is valid there but does not describe hormesis.
#'   \item \code{zero_asymptote}: the mean decays onto zero with no free lower
#'     asymptote, so it cannot produce the negative values a \code{log} or
#'     \code{logit} linear predictor needs.
#' }
#'
#' The flags assume \code{x >= 0}. At a negative predictor \code{nechorme},
#' \code{nechorme4}, \code{ecxhormebc4} and \code{ecxhormebc5} return a
#' negative mean although \code{below_zero} is \code{FALSE}, and no gate in
#' \code{\link{check_models}} excludes them. \code{unscaled_excess} is stated
#' against 1, so it applies to a (0, 1) response only. \code{zero_asymptote}
#' holds under every bounded link, but \code{\link{check_models}} applies it
#' under \code{log} and \code{logit} only.
#'
#' @return A \code{\link[base]{data.frame}}, one row per equation, the
#' members of \code{models()$all} and \code{ecxflat}.
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
    ecxhormebc5   = list(can_exceed_one = TRUE),
    ecxflat       = list()
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
  # Derive zero_asymptote: a mean with no bot that is not unbounded below can
  # only decay onto zero. test-mu_support.R asserts that it matches
  # mod_groups$zero_bounded.
  has_bot <- vapply(out$model, function(m) {
    "bot" %in% names(get(paste0("bf_", m))[[2]])
  }, logical(1))
  # Exclude ecxflat by name. It has no bot and is not unbounded below, but its
  # mean is constant rather than decaying, and check_models() keeps it under a
  # log or logit link.
  out$zero_asymptote <- !has_bot & !out$below_zero &
    !out$model %in% constant_equations()
  rownames(out) <- NULL
  out
}

#' Which scale a group-level deviation on the whole curve should be applied on
#'
#' @details \pkg{brms} declares a group-level offset unconstrained. Added to a
#' bounded mean under the identity link, it can put \code{mu} outside the
#' likelihood's support, and Stan rejects the step as a divergence. Applied on
#' the log or logit scale it cannot, and \code{top}, \code{bot}, \code{nec} and
#' \code{beta} keep their meaning because the deviation is zero-centred.
#'
#' Returns \code{"none"} where no transform is needed, because
#' \code{\link{mu_is_constrained}} is \code{FALSE}, and where none is defined,
#' because the mean is not provably strictly inside the interval: an equation
#' flagged \code{below_zero}, or on (0, 1) also \code{can_exceed_one} or
#' \code{ceiling_at_one}. Those keep the additive offset and the raised
#' \code{adapt_delta}.
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
    # Gate on can_exceed_one, not the narrower unscaled_excess. Once the mean
    # passes 1, the collapsed logit form has a pole and changes sign across it.
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
#' @details Write the collapsed form, never \code{inv_logit(logit(m) + o)}.
#' \code{logit(m)} underflows to \code{-Inf} once the decay term exceeds about
#' 709, and the result is then exactly 0, which the likelihood rejects. The
#' collapsed forms are stable as \code{m} approaches 0, in the tail of a
#' declining curve such as \code{nec3param}.
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
#' @details \code{top} and \code{bot} are on the response scale, and
#' \code{\link{define_prior}} bounds both to the support of the mean. Every
#' other parameter is on the predictor scale or enters through an exponential,
#' so a log or logit of it is not generally defined: \code{nec} on a log
#' predictor is often negative.
#'
#' @return A \code{\link[base]{character}} vector.
#'
#' @noRd
par_transform_pars <- function() {
  c("top", "bot")
}

#' Which scale a group-level deviation on a single parameter should be applied on
#'
#' @details The parameter-level counterpart of \code{\link{ogl_transform_kind}},
#' for \code{pgl()} and an explicit \code{(par | group)} term. A deviation on
#' \code{bot}, which is often estimated near zero, can take it below zero and
#' make the likelihood undefined.
#'
#' The answer depends on the family alone, not on the equation. \code{top} and
#' \code{bot} are bounded to the family's support by their own priors in every
#' equation, so the transform is defined even where the mean transform is not,
#' as for \code{nechorme}. Such equations keep the raised \code{adapt_delta};
#' see \code{\link{mu_confined_by_pars}}.
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
#' @details \code{bot} keeps its name, so \code{b_bot_Intercept} and its prior
#' are unchanged. The new terms are \code{botgl}, the deviation, and
#' \code{bnecbot}, which the curve reads in place of \code{bot}. Rename the
#' generated term, never the parameter the user asked about.
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
#' @details Decides the \code{adapt_delta} raise. Even with every deviation
#' transformed, a group-level term can put \code{mu} outside the support where
#' the mean is not bounded by \code{top} and \code{bot}: where
#' \code{\link{model_mu_ranges}} flags the equation \code{below_zero},
#' \code{can_exceed_one} or \code{ceiling_at_one}.
#'
#' All three flags are tested whatever the family's support. Do not delegate
#' this to \code{\link{ogl_transform_kind}}, which tests the last two on (0, 1)
#' only: a hormesis term in \code{exp(slope) * x} makes the mean negative for
#' a negative enough predictor, which \code{crf(log(x), ...)} often supplies.
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
#' @details A data column with one of these names would be resolved in place
#' of the generated term, so \code{\link{check_reserved_names}} refuses it.
#' Initialisation code also removes these names from prior sets. The fixed
#' list keeps both uses independent of any one formula.
#'
#' @return A \code{\link[base]{character}} vector.
#' @noRd
generated_term_names <- function() {
  c("bnecmu", "ogl", unlist(lapply(par_transform_pars(), par_gl_names),
                            use.names = FALSE))
}

#' The deviation intercepts a group-level structure leaves unidentified
#'
#' @details A constant added to \code{ogl} can be offset by \code{top} and
#' \code{bot} with no change to the likelihood, so its population intercept is
#' not identified. \code{\link{define_group_prior}} gives it a zero-centred
#' prior, and it is started at zero. A transformed parameter deviation has no
#' population intercept, so there is nothing to start; see
#' \code{\link{add_par_gl_term}}.
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
  # Only ogl: a transformed parameter deviation has no population intercept.
  # `family` is unused, but which terms a formula generates is a property of
  # the family, so callers pass it.
  if (isTRUE(group_spec$ogl)) "ogl" else character(0)
}
