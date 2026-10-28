#' Quantile of a response on the scale of its positive part
#'
#' The gamma priors for \code{top} and \code{bot} set their rate from a
#' quantile of the response. Zeros pull that quantile towards zero, and the
#' prior then collapses onto a scale unrelated to the asymptote. This returns
#' level \code{p} of the positive part instead, which is level
#' \code{1 - (1 - p)(1 - z)} of the whole response, where \code{z} is the share
#' of values at or below zero. A response with no zeros gets the raw quantile.
#'
#' Multiply by \code{1 - z}; do not divide. The dividing form,
#' \code{1 - (1 - p) / (1 - z)}, shifts the level the wrong way and goes
#' negative where most values are zero.
#'
#' The positive part sets a scale only. \code{define_hurdle_prior()} instead
#' builds the whole prior from the non-zero subset. That is exact for
#' \code{hurdle_gamma}, because a Gamma has no mass at zero. Under
#' zero-inflation the count process emits zeros of its own, and the non-zero
#' subset would bias the location upward.
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
  probs_adj <- 1 - (1 - probs) * (1 - zero_frac)
  q <- unname(quantile(finite, probs = probs_adj))
  # The rescaled quantile can still be zero, at probs = 0 or through ties.
  # Fall back to the smallest positive value.
  if (!is.finite(q) || q <= 0) {
    return(min(pos))
  }
  q
}

#' The factor the "regularizing" prior set narrows by
#'
#' \code{prior_type = "uninformative"} is the set Fisher et al. (2024)
#' describe, stated per family. It is not identical to the CRAN release: the
#' \code{nec} and \code{ec50} entry differs, and so does the zero guard of
#' \code{positive_scale()}. Every \code{"regularizing"} entry for \code{top}
#' and \code{bot} is derived from two rules:
#'
#' \itemize{
#'   \item Location: the mean response at the end of the predictor where the
#'     parameter is the level of the curve, the lowest concentrations for
#'     \code{top} and the highest for \code{bot}, on the link scale. See
#'     \code{regularizing_location()}.
#'   \item Spread: \code{regularizing_factor} times the standard deviation of
#'     the \code{"uninformative"} entry for the same parameter and family,
#'     floored at the standard error of the location and capped at the
#'     uninformative width. See \code{regularizing_entry()}.
#' }
#'
#' Each entry is the distribution matching the parameter's support (normal,
#' gamma or beta) with its \strong{mode}, not its mean, at the location. A
#' gamma set by its mean peaks elsewhere, so matching the mode makes the rule
#' mean the same thing on all three branches. Derive the entry for a new family
#' from these two rules rather than writing a separate one.
#'
#' The spread is inherited from the uninformative entry, which reads the pooled
#' response, while the location reads one end of the predictor. Where the
#' uninformative entry is poorly scaled, so is this one, and the cap can then
#' hold the spread below the standard error of the location.
#'
#' \code{define_group_prior()} narrows every group-level scale by the same
#' factor. \code{nec} and \code{ec50} do not use it; see
#' \code{predictor_prior()}.
#'
#' @noRd
regularizing_factor <- 0.4

#' Standard deviation of a beta distribution
#'
#' @param shape1,shape2 The two shape parameters.
#'
#' @return A \code{\link[base]{numeric}} vector of length 1.
#'
#' @noRd
beta_sd <- function(shape1, shape2) {
  n <- shape1 + shape2
  sqrt(shape1 * shape2 / (n^2 * (n + 1)))
}

#' Shape and rate of a gamma with a stated mode and standard deviation
#'
#' @details A gamma with shape \emph{s} > 1 and rate \emph{r} has its mode at
#' \code{(s - 1) / r} and its standard deviation at \code{sqrt(s) / r}. Setting
#' the mode to \emph{L} and the standard deviation to \emph{S} therefore
#' requires \code{sqrt(s) / (s - 1) = S / L}, which is a quadratic in
#' \code{sqrt(s)} with the one positive root
#' \code{sqrt(s) = (L/S + sqrt(L^2/S^2 + 4)) / 2}. The rate follows as
#' \code{(s - 1) / L}. The root is always above 1, so the shape is always above
#' 1 and the distribution always has an interior mode.
#'
#' @param mode The intended maximum density, strictly positive.
#' @param spread The intended standard deviation, strictly positive.
#'
#' @return A named \code{\link[base]{numeric}} vector of length 2.
#'
#' @noRd
gamma_from_mode_sd <- function(mode, spread) {
  ratio <- mode / spread
  root <- (ratio + sqrt(ratio^2 + 4)) / 2
  shape <- root^2
  c(shape = shape, rate = (shape - 1) / mode)
}

#' Shape parameters of a beta with a stated mode and standard deviation
#'
#' @details A beta with both shapes above 1 has its mode at
#' \code{(a - 1) / (a + b - 2)}, so fixing the mode at \emph{L} leaves one free
#' parameter, the concentration \code{n = a + b}. The standard deviation falls
#' monotonically in \emph{n} from \code{1 / sqrt(12)} at \code{n = 2}, where
#' the beta is uniform, so \emph{n} is found by root-finding and is unique.
#'
#' No beta with an interior mode is wider than the uniform, so a spread at or
#' above \code{1 / sqrt(12)} returns the uniform.
#'
#' @param mode The intended maximum density, in [0, 1].
#' @param spread The intended standard deviation, strictly positive.
#'
#' @return A named \code{\link[base]{numeric}} vector of length 2.
#'
#' @importFrom stats uniroot
#'
#' @noRd
beta_from_mode_sd <- function(mode, spread) {
  shapes <- function(n) c(1 + mode * (n - 2), 1 + (1 - mode) * (n - 2))
  sd_at <- function(n) {
    ab <- shapes(n)
    beta_sd(ab[1], ab[2])
  }
  lo <- 2
  if (spread >= sd_at(lo)) {
    ab <- shapes(lo)
    return(c(shape1 = ab[1], shape2 = ab[2]))
  }
  # Double the upper bracket rather than fix it: a fixed ceiling would return
  # the ceiling, silently, for a narrow spread.
  hi <- 4
  while (sd_at(hi) > spread && hi < 1e12) {
    hi <- hi * 2
  }
  # The doubling stops at a ceiling so that a spread of zero terminates. The
  # bracket then holds no root, so return the narrowest beta reached rather
  # than let uniroot() stop. define_prior() never requests so narrow a spread.
  if (sd_at(hi) > spread) {
    ab <- shapes(hi)
    return(c(shape1 = ab[1], shape2 = ab[2]))
  }
  n <- uniroot(function(n) sd_at(n) - spread, c(lo, hi),
               tol = .Machine$double.eps^0.5)$root
  ab <- shapes(n)
  c(shape1 = ab[1], shape2 = ab[2])
}

#' The location of a regularizing prior for one response-scaled parameter
#'
#' \code{top} is the level of the response before the curve responds, and
#' \code{bot} the level where it no longer changes. This estimates either from
#' the observations at the matching end of the predictor: the lowest
#' concentrations for \code{top} and the highest for \code{bot}. A quantile of
#' the pooled response is not used. How well it tracks a plateau depends on the
#' share of the design on that plateau, and for an over-dispersed count it sits
#' far above the plateau.
#'
#' @details The subset starts at the extreme predictor value and extends to the
#' next distinct value until it holds a twentieth of the observations, and no
#' fewer than three, but never past a fifth of the distinct values. The
#' location is its mean. The second limit keeps the subset at the end of the
#' series for the second block of a hurdle or zero-inflated fit, which
#' \code{split_hurdle_response()} primes from one proportion per
#' concentration. A design with fewer than ten concentrations and no
#' replication presents the same input, so its subset is one observation.
#'
#' The rule assumes the lowest concentration is at \code{top} and the highest
#' at \code{bot}. The first holds exactly at a predictor of zero for the ten
#' equations with a \code{nec} parameter and for \code{ecxlin}, \code{ecxexp}
#' and \code{ecxsigm}, and approximately for the other \code{ecx} equations.
#' The second fails on a design that stops before the lower asymptote. The
#' \code{bot} location then sits above the true value, a bias that the
#' standard-error floor does not cover. \code{asymptote_observed = FALSE}
#' replaces the entry for such a design; see \code{unobserved_bot_entry()}.
#'
#' On the zero-bounded branch the two ends treat zeros differently. At the
#' control a zero is structural, so the \code{top} mean is taken over the
#' positive observations, as in \code{positive_scale()}. At the highest
#' concentration a zero is the endpoint responding, so the \code{bot} mean
#' keeps it. Under zero-inflation some of those zeros are structural, which
#' biases the \code{bot} location low. On the other branches
#' \code{response_link_scale()} has already shifted any zero, and every
#' observation is used.
#'
#' The standard error is returned so that \code{regularizing_entry()} can floor
#' the spread at it. A group whose values are all equal, such as a binary
#' control in which every individual survived, has a standard deviation of
#' zero without being exact, so the spread of the whole response stands in.
#' This errs towards a wider prior.
#'
#' @param predictor A \code{\link[base]{numeric}} vector of the predictor.
#' @param response A \code{\link[base]{numeric}} vector of the response,
#' already on the link scale.
#' @param side One of \code{"top"} or \code{"bot"}.
#' @param zero_bounded Whether the parameter is bounded below at zero, which is
#' the gamma branch. It selects the treatment of zeros described above.
#'
#' @return A named \code{\link[base]{numeric}} vector of length 2, the
#' location and its standard error.
#'
#' @importFrom stats quantile sd
#'
#' @noRd
regularizing_location <- function(predictor, response, side,
                                  zero_bounded = FALSE) {
  n <- length(response)
  ux <- sort(unique(predictor), decreasing = side != "top")
  # max_levels keeps the subset at the end of the series where each
  # concentration has one value, as in a hurdle second block. A limit that
  # suits small unreplicated designs better places that block worse.
  min_n <- max(3, ceiling(0.05 * n))
  max_levels <- max(1, floor(0.2 * length(ux)))
  idx <- integer(0)
  for (i in seq_along(ux)) {
    idx <- c(idx, which(predictor == ux[i]))
    if (length(idx) >= min_n || i >= max_levels) {
      break
    }
  }
  y <- response[idx]
  # A zero at the control is structural and is dropped from the top mean. A
  # zero at the highest concentration is the response, and is kept.
  if (zero_bounded && side == "top") {
    pos <- y[y > 0]
    if (length(pos)) {
      y <- pos
    }
  }
  location <- mean(y)
  if (zero_bounded && (!is.finite(location) || location <= 0)) {
    pos <- response[is.finite(response) & response > 0]
    if (side == "bot" && length(pos)) {
      # The whole group at the highest concentration is zero. A tenth of the
      # smallest positive observation places bot below anything resolved.
      location <- min(pos) / 10
    } else {
      # The whole control group is zero, which under zero-inflation is a run of
      # structural zeros. Use the scale of the positive part, not a detection
      # floor, or the top prior collapses.
      location <- positive_scale(response, probs = 0.95)
    }
  }
  if (!is.finite(location)) {
    probs <- if (side == "top") 0.95 else 0.05
    location <- unname(quantile(response, probs = probs))
  }
  se <- if (length(y) > 1) sd(y) / sqrt(length(y)) else NA_real_
  # A group of equal values has sd() of zero without being exact, as in a
  # binary control where every individual survived. Use the spread of the
  # whole response instead.
  if (!is.finite(se) || se <= 0) {
    se <- sd(response) / sqrt(max(length(y), 1))
  }
  if (!is.finite(se) || se < 0) {
    se <- 0
  }
  c(location = location, se = se)
}

#' The tag \code{define_prior} keys its response-scaled entries on
#'
#' @details This is not the family name. Two rewrites change which \code{top}
#' and \code{bot} entries apply:
#'
#' \itemize{
#'   \item A \code{log} or \code{logit} link puts the response on an unbounded
#'     scale, so the gaussian entries apply whatever the family. Without this,
#'     a \code{Gamma(link = "log")} response below 1 is negative throughout
#'     and \code{positive_scale()} refuses it.
#'   \item A zero-inflated count family takes its base family's entries,
#'     because the mixture changes the number of zeros and not the scale of
#'     mu. No table holds an entry under the zero-inflated name.
#' }
#'
#' The tag is read through \code{canonical_fam_tag()}, because a family taken
#' from a fit records Gamma as \code{gamma}.
#'
#' @param family A \code{\link[stats]{family}} object.
#'
#' @return A \code{\link[base]{character}} string.
#'
#' @noRd
prior_family_tag <- function(family) {
  if (is.null(family) || is.null(family$family)) {
    return(NA_character_)
  }
  if (isTRUE(family$link %in% c("logit", "log"))) {
    return("gaussian")
  }
  sub("^zero_inflated_(poisson|negbinomial)$", "\\1",
      canonical_fam_tag(family))
}

#' Whether a family's response-scaled parameters are bounded below at zero
#'
#' @details \code{define_prior()} and \code{init_limits()} both read this, and
#' must agree, because it selects how \code{regularizing_location()} treats
#' zeros. Keep the list here rather than at each caller. It is keyed on
#' \code{prior_family_tag()} for the reasons given there.
#' @param family A \code{\link[stats]{family}} object.
#'
#' @return A \code{\link[base]{logical}} of length 1.
#'
#' @noRd
zero_bounded_family <- function(family) {
  isTRUE(prior_family_tag(family) %in% c("Gamma", "poisson", "negbinomial"))
}

#' The floor of the lower asymptote, on the scale the curve is fitted on
#'
#' @details Under \code{asymptote_observed = FALSE} the response bounds
#' \code{bot} from above, and the prior and the initial-value band need the
#' lower end, the smallest value the mean can take. \code{define_prior()} and
#' \code{make_good_inits()} both read it here so that they agree.
#'
#' Every tag but \code{gaussian} has a mean bounded below at zero, so the floor
#' is zero. A gaussian tag under the identity link has a floor of zero where
#' the response is non-negative throughout, and none otherwise. Under a
#' \code{log} or \code{logit} link the floor on the link scale is \code{-Inf}
#' however the response is signed, so none is returned.
#'
#' @param family A \code{\link[stats]{family}} object.
#' @param response The response, already on the link scale.
#'
#' @return A \code{\link[base]{numeric}} vector of length 1: the floor, or
#' \code{NA_real_} where none can be derived.
#'
#' @noRd
asymptote_floor <- function(family, response) {
  tag <- prior_family_tag(family)
  if (is.na(tag)) {
    return(NA_real_)
  }
  if (!identical(tag, "gaussian")) {
    return(0)
  }
  if (!isTRUE(identical(family$link, "identity"))) {
    return(NA_real_)
  }
  finite <- response[is.finite(response)]
  if (length(finite) > 0 && min(finite) >= 0) 0 else NA_real_
}

#' Why no floor could be derived, as a clause for the refusal
#'
#' @details The two causes need different remedies, so each is named: a
#' gaussian response with negative values, and a non-identity link. The
#' negative case counts the values below zero and reports the smallest, so that
#' a user can tell one stray reading from a response negative throughout.
#'
#' @param family A \code{\link[stats]{family}} object.
#' @param response The response, already on the link scale. Read on the
#' negative branch alone.
#'
#' @return A \code{\link[base]{character}} string.
#'
#' @noRd
no_floor_reason <- function(family, response = NULL) {
  if (!isTRUE(identical(family$link, "identity"))) {
    return(paste0("the \"", family$link, "\" link puts the curve on a scale",
                  " whose floor is -Inf, so none is available."))
  }
  below <- if (is.null(response)) {
    numeric(0)
  } else {
    response[is.finite(response) & response < 0]
  }
  detail <- if (length(below) == 0) {
    ""
  } else {
    paste0(" ", length(below), " of ",
           sum(is.finite(response)), " observations are below zero, the",
           " smallest being ", signif(min(below), 4), ".")
  }
  paste0("a ", family$family, " response spanning negative values has no",
         " floor that can be derived from the data.", detail)
}

#' The message refusing a declaration that cannot be acted on
#'
#' @param reason The clause \code{\link{no_floor_reason}} supplies, naming
#' what could not be derived.
#'
#' @return A \code{\link[base]{character}} string.
#'
#' @noRd
asymptote_floor_error <- function(reason) {
  paste0("`asymptote_observed = FALSE` places the prior for \"bot\" between",
         " the floor of the response and the mean at the highest predictor",
         " level, and ", reason, " Supply a prior for \"bot\" through the",
         " `prior` argument and leave `asymptote_observed` at its default.")
}

#' The bot prior for a design that did not reach the lower asymptote
#'
#' @details The central 95\% of the prior spans from the floor
#' \code{\link{asymptote_floor}} returns to the mean response at the highest
#' predictor level, which on such a design bounds \code{bot} from above. The
#' endpoint mean is the \code{"bot"} location of
#' \code{\link{regularizing_location}}, so its treatment of zeros applies here.
#'
#' The three branches, for an endpoint mean \emph{e} and a floor of zero:
#'
#' \itemize{
#'   \item gaussian: \code{normal(e/2, e/(2 qnorm(0.975)))}, whose central 95\%
#'   is exactly \code{[0, e]}.
#'   \item beta: \code{beta(1, log(0.025) / log(1 - e))}, whose 97.5th
#'   percentile is exactly \emph{e}.
#'   \item gamma: \code{gamma(2, qgamma(0.975, 2) / e)}, whose 97.5th
#'   percentile is exactly \emph{e}. With the shape fixed at 2, the same shape
#'   as the \code{"uninformative"} entry, the interval cannot also start at
#'   zero: its 2.5th percentile is 0.0435\emph{e}.
#' }
#'
#' The beta branch fixes \code{shape1} at 1 so that the density is positive at
#' the floor. \code{\link{beta_from_mode_sd}} requires both shapes above 1,
#' which puts the 2.5th percentile well above the floor and excludes a true
#' \code{bot} near it.
#'
#' The cap in \code{\link{regularizing_entry}} is not applied. The rule fixes
#' both ends of the interval, so the width follows from the endpoint mean, and
#' the entry can be wider than the \code{"uninformative"} one.
#'
#' @param branch One of \code{"normal"}, \code{"gamma"} or \code{"beta"}.
#' @param endpoint The mean response at the highest predictor level, strictly
#' positive and on the link scale.
#'
#' @return A \code{\link[base]{character}} string of length 1, a \pkg{brms}
#' prior string.
#'
#' @importFrom stats qgamma qnorm
#'
#' @noRd
unobserved_bot_entry <- function(branch, endpoint) {
  half <- endpoint / 2
  switch(
    branch,
    normal = paste0("normal(", signif(half, 6), ", ",
                    signif(endpoint / (2 * qnorm(0.975)), 6), ")"),
    # The rate places the 97.5th percentile at the endpoint mean.
    gamma = paste0("gamma(2, ", signif(qgamma(0.975, 2) / endpoint, 6), ")"),
    beta = {
      # Test the endpoint before the logarithm, so that an endpoint at or above
      # 1 reaches the fallback without a "NaNs produced" warning.
      # define_prior() never passes one; the fallback serves other callers.
      shape2 <- if (endpoint >= 1) {
        NA_real_
      } else {
        # qbeta(p, 1, b) is 1 - (1 - p)^(1/b), so the shape whose 97.5th
        # percentile is the endpoint mean is log(0.025) / log(1 - e).
        log(0.025) / log(1 - endpoint)
      }
      if (!is.finite(shape2) || shape2 <= 0) {
        pars <- beta_from_mode_sd(min(half, 0.5),
                                  endpoint / (2 * qnorm(0.975)))
        paste0("beta(", signif(pars[["shape1"]], 6), ", ",
               signif(pars[["shape2"]], 6), ")")
      } else {
        paste0("beta(1, ", signif(shape2, 6), ")")
      }
    },
    stop("Unknown prior branch \"", branch, "\".", call. = FALSE)
  )
}

#' The endpoint mean the unobserved-asymptote bot prior is built from
#'
#' @details The \code{"bot"} location of \code{\link{regularizing_location}}.
#' Where the endpoint group is entirely at the floor, a tenth of the smallest
#' positive observation stands in, as \code{\link{regularizing_location}} does
#' on its zero-bounded branch. A response with no positive value has no
#' interval between the floor and the endpoint, so the declaration is refused.
#'
#' @inheritParams regularizing_location
#' @param zero_bounded Passed through to \code{\link{regularizing_location}}.
#'
#' @return A \code{\link[base]{numeric}} vector of length 1, strictly positive.
#'
#' @noRd
unobserved_endpoint_mean <- function(predictor, response, zero_bounded) {
  no_interval <- asymptote_floor_error(
    paste0("the response holds no value above that floor, so there is no",
           " interval to place it on.")
  )
  # Test for a positive value before reading the location. Otherwise, on a
  # zero-bounded family, positive_scale() refuses first, with a message that
  # does not name the declaration. Do not wrap the read in try() instead: that
  # would turn later errors into this one as well.
  pos <- response[is.finite(response) & response > 0]
  if (length(pos) == 0) {
    stop(no_interval, call. = FALSE)
  }
  e <- regularizing_location(predictor, response, "bot",
                             zero_bounded = zero_bounded)[["location"]]
  if (!is.finite(e) || e <= 0) {
    e <- min(pos) / 10
  }
  if (!is.finite(e) || e <= 0) {
    stop(no_interval, call. = FALSE)
  }
  e
}

#' The regularizing prior for one response-scaled parameter
#'
#' Applies the rules documented at \code{regularizing_factor} to one branch.
#'
#' @param branch One of \code{"normal"}, \code{"gamma"} or \code{"beta"}, the
#' distribution family matching the parameter's support.
#' @param location The intended maximum density, on the link scale.
#' @param uninformative_sd The standard deviation of the
#' \code{"uninformative"} prior for the same parameter on the same family.
#' @param location_se The standard error of \code{location}, which the spread
#' is floored at. Zero where the location was not estimated from a subset.
#'
#' @return A \code{\link[base]{character}} string of length 1, a \pkg{brms}
#' prior string.
#'
#' @noRd
regularizing_entry <- function(branch, location, uninformative_sd,
                               location_se = 0) {
  # Floor the spread at the standard error of the location, so that the prior
  # is never narrower than the noise in its own anchor. Cap it at the
  # uninformative width, so that this set is never the wider of the two. The
  # cap takes precedence where they conflict.
  spread <- min(max(regularizing_factor * uninformative_sd, location_se),
                uninformative_sd)
  # A response with no spread gives no scale to narrow, and every branch below
  # divides by it. Keep the prior well formed and leave the failure to the
  # sampler.
  if (!is.finite(spread) || spread <= 0) {
    spread <- if (branch == "beta") beta_sd(5, 2) else abs(location) / 10
    if (!is.finite(spread) || spread <= 0) {
      spread <- 1
    }
  }
  switch(
    branch,
    normal = paste0("normal(", location, ", ", spread, ")"),
    gamma = {
      pars <- gamma_from_mode_sd(location, spread)
      paste0("gamma(", signif(pars[["shape"]], 6), ", ",
             signif(pars[["rate"]], 6), ")")
    },
    beta = {
      pars <- beta_from_mode_sd(location, spread)
      paste0("beta(", signif(pars[["shape1"]], 6), ", ",
             signif(pars[["shape2"]], 6), ")")
    },
    stop("Unknown prior branch \"", branch, "\".", call. = FALSE)
  )
}


#' Check the \code{predictor_scale} argument
#'
#' @param predictor_scale One of \code{"auto"}, \code{"concentration"} or
#' \code{"log"}. See \code{\link{bnec}}.
#' @param predictor The predictor, or \code{NULL} to check the value alone.
#' \code{"concentration"} refuses a negative predictor.
#'
#' @return \code{predictor_scale}, matched.
#'
#' @noRd
validate_predictor_scale <- function(predictor_scale, predictor = NULL) {
  predictor_scale <- match.arg(predictor_scale,
                               c("auto", "concentration", "log"))
  if (!is.null(predictor) && predictor_scale == "concentration" &&
      any(predictor < 0)) {
    stop("`predictor_scale = \"concentration\"` requires a non-negative",
         " predictor. Use \"log\" if the supplied values are already logged,",
         " or \"auto\" to retain the existing sign-based choice.",
         call. = FALSE)
  }
  predictor_scale
}

#' The default prior for the nec and ec50 parameters
#'
#' \code{nec} and \code{ec50} are in units of the predictor, so their prior
#' describes the concentration series tested. It is a normal prior on the log
#' of the predictor: \code{lognormal(mu, sigma)} where the predictor is a
#' concentration, and \code{normal(mu, sigma)} where it was supplied already
#' logged. The caller sets the bound from the distribution's support,
#' \code{lb = 0} for the lognormal and none for the normal, and does not
#' truncate the prior to the tested range.
#'
#' @details \code{mu} is the median of the distinct logged values, so that
#' replication does not change the prior. The maximum density on the log scale,
#' and the median on the concentration scale, are then at the median dose.
#' Fisher et al. (2024) place the maximum density at the median predictor
#' without naming the scale; this reads it on the log scale, on which a
#' dilution series is designed.
#'
#' On the lognormal branch \code{sigma} is set so that the central 95\%
#' interval reaches every dose tested. It therefore depends on the two extreme
#' doses, and a control recorded as a small positive value widens it. A control
#' recorded as 0 is dropped before the log is taken, which is what
#' \code{\link{bnec}} expects.
#'
#' On the branch for a predictor supplied already logged, \code{sigma} is the
#' published \code{10 sd(x)}, read from the distinct values. That entry is much
#' wider, so \code{crf(x)} and \code{crf(log(x))} give different priors on
#' \code{nec}.
#'
#' \code{"regularizing"} changes the spread only, to
#' \code{half_width / qnorm(0.99)} on both branches, so that the central 98\%
#' interval reaches the farthest dose. It does not use
#' \code{regularizing_factor}: a narrower width would exclude doses that were
#' applied.
#'
#' Under \code{predictor_scale = "auto"} the branch is chosen by
#' \code{min(u) < 0}. That cannot recognise a logged series whose lowest
#' concentration is at or above 1, and such a series is logged a second time.
#' \code{"log"} and \code{"concentration"} declare the scale instead. On the
#' log scale a zero is \code{log(1)} and is kept; on the concentration scale it
#' is the control and is dropped.
#'
#' A design with fewer than two distinct positive values gives no range, and
#' \code{sigma} falls back to 1. A predictor with no positive value is refused,
#' though \code{check_data()} normally refuses it first. The share of the prior
#' above the highest dose, by branch and \code{prior_type}, is tabulated under
#' \code{prior_type} in \code{?\link{bnec}}.
#'
#' @param predictor A \code{\link[base]{numeric}} vector, the predictor as it
#' was supplied.
#' @param prior_type One of \code{"uninformative"} or \code{"regularizing"}.
#' @param predictor_scale One of \code{"auto"}, \code{"concentration"} or
#' \code{"log"}. See \code{\link{bnec}}.
#'
#' @return A \code{\link[base]{character}} string of length 1, a \pkg{brms}
#' prior string.
#'
#' @importFrom stats median sd qnorm
#'
#' @noRd
predictor_prior <- function(predictor, prior_type = "uninformative",
                            predictor_scale = "auto") {
  u <- unique(predictor)
  predictor_scale <- validate_predictor_scale(predictor_scale, u)
  # The values cannot show whether a non-negative column is already logged.
  # Keep the sign rule under "auto", and let a declaration replace it.
  is_logged <- switch(
    predictor_scale,
    auto = min(u) < 0,
    concentration = FALSE,
    log = TRUE
  )
  z <- if (is_logged) u else log(u[u > 0])
  if (!length(z)) {
    stop("Cannot build a prior for \"nec\" or \"ec50\": the predictor",
         " contains no positive values, so there is no concentration scale",
         " to place them on. Check the predictor variable, and see ?bnec.",
         call. = FALSE)
  }
  mu <- median(z)
  # The larger of the two half-widths, not half the range, which stops short
  # of one end on a series asymmetric about its median.
  half_width <- max(mu - min(z), max(z) - mu)
  if (is_logged) {
    dist <- "normal"
    sigma <- sd(z) * 10
  } else {
    dist <- "lognormal"
    sigma <- half_width / qnorm(0.975)
  }
  # Both branches degenerate on a single distinct (positive) value: sd() is NA
  # and both half-widths are zero.
  if (!is.finite(sigma) || sigma <= 0) {
    sigma <- 1
  } else if (prior_type == "regularizing") {
    # A coverage rule rather than a multiple of the uninformative spread, so
    # that it means the same thing on both branches; the logged branch's
    # uninformative spread is a constant, not a coverage width. Do not narrow
    # the degenerate fallback above, which stands in for a scale that could not
    # be measured.
    sigma <- half_width / qnorm(0.99)
  }
  paste0(dist, "(", mu, ", ", sigma, ")")
}

#' define_prior
#'
#' Generates prior model objects to pass to \pkg{brms}
#'
#' @inheritParams bnec
#'
#' @param family A \code{\link[stats]{family}} function.
#' @param predictor The predictor variable.
#' @param response The response variable.
#' @param asymptote_observed A \code{\link[base]{logical}}. \code{FALSE}
#' declares that the highest predictor level did not reach the lower asymptote,
#' which replaces the \code{bot} entry of whichever set \code{prior_type}
#' selected with the construction \code{\link{unobserved_bot_entry}} describes.
#' @param prior_predictor The predictor the \code{nec} and \code{ec50} prior is
#' built from. Defaults to \code{predictor}, and differs from
#' it only for the two blocks of a hurdle or zero-inflated fit, each of which is
#' primed from a subset of the predictor but evaluated over the whole of it.
#' See \code{define_hurdle_prior()}.
#'
#' @return An object of class \code{\link[brms]{brmsprior}}.
#' @importFrom brms prior_string
#' @importFrom stats sd
#' @importFrom stats median
#'
#' @noRd
define_prior <- function(model, family, predictor, response,
                         prior_type = "uninformative",
                         asymptote_observed = TRUE,
                         model_survival = NULL, disp_spec = NULL,
                         group_spec = NULL, prior_predictor = NULL,
                         predictor_scale = "auto", rate_response = FALSE,
                         truncated_response = FALSE) {
  if (is.null(prior_predictor)) {
    prior_predictor <- predictor
  }
  # The scale an ogl deviation is applied on sets the width of its prior. It
  # depends on the model and the family, which define_group_prior() lacks.
  ogl_kind <- if (isTRUE(group_spec$ogl)) {
    ogl_transform_kind(model, family)
  } else {
    "none"
  }
  # The same for a deviation on top or bot, decided by the family alone.
  par_kind <- if (is.null(group_spec)) "none" else par_transform_kind(family)
  prior_type <- match.arg(prior_type, c("uninformative", "regularizing"))
  if (is_hurdle_family(family)) {
    hurdle_priors <- define_hurdle_prior(model, family, predictor, response,
                                         prior_type = prior_type,
                                         asymptote_observed =
                                           asymptote_observed,
                                         model_survival = model_survival,
                                         prior_predictor = prior_predictor,
                                         predictor_scale = predictor_scale)
    # A group-level term reaches the mu block only, because add_formula_glef()
    # runs before the hu sub-formulas are attached. Build its priors from the
    # survivors, on the link scale of the mu family, which is the quantity the
    # offsets are added to.
    mu_family <- hurdle_mu_family(family)
    mu_split <- split_hurdle_response(predictor, response)$mu
    mu_response <- response_link_scale(mu_split$y, mu_family)
    group_priors <- define_group_prior(group_spec, predictor, mu_response,
                                       prior_type = prior_type,
                                       ogl_transform = ogl_kind,
                                       par_transform = par_kind)
    if (!is.null(group_priors)) {
      hurdle_priors <- hurdle_priors + group_priors
    }
    # A disp() term models the positive block's dispersion, so take its priors
    # from the positive block's family and the survivors. This branch returns
    # before the point below where a single-block family's disp() priors are
    # added.
    #
    # Backstop for amend() and direct calls: bnec() and get_priors() check the
    # term before their model loops.
    if (!is.null(disp_spec)) {
      check_disp_spec(disp_spec, family, response = mu_response)
    }
    # Pass the survivors' own predictor values, so that the replicate centre
    # of the regularizing set groups each response by its concentration.
    #
    # brms truncates a count hurdle's positive block at zero, and no formula
    # attribute records it, so read the truncation from the family.
    disp_priors <- define_disp_prior(disp_spec, mu_family, mu_response,
                                     predictor = mu_split$x,
                                     prior_type = prior_type,
                                     rate_response = rate_response,
                                     truncated_response = truncated_response ||
                                       is_count_hurdle(family))
    if (!is.null(disp_priors)) {
      hurdle_priors <- hurdle_priors + disp_priors
    }
    return(hurdle_priors)
  }
  # Backstop for amend() and direct calls: bnec() and get_priors() check the
  # term before their model loops.
  if (!is.null(disp_spec)) {
    check_disp_spec(disp_spec, family, response = response)
  }
  link_tag <- family$link
  custom_name <- check_custom_name(family)
  # prior_family_tag() applies the link and zero-inflation rewrites, and the
  # initial-value search reads it too. positive_scale() guards the quantiles
  # below against a response that is mostly zero.
  fam_tag <- prior_family_tag(family)
  if (family$family == "beta_binomial" || family$family == "binomial") {
    if (is.integer(response) || max(response) > 1) {
      stop("Response vector must be passed as a proportion to define_prior",
           " (not as integers) for the binomial and beta_binomial families.")
    }
  }
  response <- response_link_scale(response, family)
  # Called for its error alone: it refuses an integer predictor on the routes
  # that do not pass through check_data(). Its value is not used.
  set_distribution(predictor, silence_y_msgs = TRUE, silence_x_msgs = FALSE)
  # The two prior sets for top and bot are described at regularizing_factor.
  # Build the gamma entries only for the zero-bounded families. On gaussian an
  # all-negative response is ordinary input, and positive_scale() would refuse
  # it.
  gamma_scaled <- zero_bounded_family(family)
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
    # Each entry applies the rules documented at regularizing_factor. The
    # uninformative standard deviations are restated in closed form rather
    # than parsed back from the prior strings:
    #
    #   normal(m, 2.5 sd(y))  sd is 2.5 sd(y)
    #   gamma(2, 2/q)         mean is q at shape 2, so sd is sqrt(2) q / 2
    #   beta(5, 2)            sd is beta_sd(5, 2), and beta(2, 5) is the same
    u_t_g <- u_b_g <- NA_character_
    if (gamma_scaled) {
      loc_t <- regularizing_location(predictor, response, "top",
                                     zero_bounded = TRUE)
      u_t_g <- regularizing_entry(
        "gamma",
        location = loc_t[["location"]], location_se = loc_t[["se"]],
        uninformative_sd = sqrt(2) * positive_scale(response, probs = 0.75) / 2
      )
      # Keep the fudge term on the spread, which must match the quantile of
      # the uninformative entry exactly.
      loc_b <- regularizing_location(predictor, response, "bot",
                                     zero_bounded = TRUE)
      u_b_g <- regularizing_entry(
        "gamma",
        location = loc_b[["location"]], location_se = loc_b[["se"]],
        uninformative_sd = sqrt(2) * (positive_scale(response, probs = 0.25) +
          min(response[response > 0]) / 100) / 2
      )
    }
    # Built only where used, as for the gamma entries: on a gaussian response
    # with negative values, beta_from_mode_sd() would be asked for a mode
    # outside the unit interval.
    u_t_b <- u_b_b <- NA_character_
    if (fam_tag %in% c("bernoulli", "binomial", "beta_binomial", "beta")) {
      loc_t <- regularizing_location(predictor, response, "top")
      loc_b <- regularizing_location(predictor, response, "bot")
      u_t_b <- regularizing_entry(
        "beta",
        location = loc_t[["location"]], location_se = loc_t[["se"]],
        uninformative_sd = beta_sd(5, 2)
      )
      u_b_b <- regularizing_entry(
        "beta",
        location = loc_b[["location"]], location_se = loc_b[["se"]],
        uninformative_sd = beta_sd(2, 5)
      )
    }
    y_t_prs <- c(Gamma = u_t_g,
                 poisson = u_t_g,
                 negbinomial = u_t_g,
                 gaussian = local({
                   l <- regularizing_location(predictor, response, "top")
                   regularizing_entry("normal", location = l[["location"]],
                                      location_se = l[["se"]],
                                      uninformative_sd = sd(response) * 2.5)
                 }),
                 bernoulli = u_t_b,
                 binomial = u_t_b,
                 "beta_binomial" = u_t_b,
                 beta = u_t_b)
    y_b_prs <- c(Gamma = u_b_g,
                 poisson = u_b_g,
                 negbinomial = u_b_g,
                 gaussian = local({
                   l <- regularizing_location(predictor, response, "bot")
                   regularizing_entry("normal", location = l[["location"]],
                                      location_se = l[["se"]],
                                      uninformative_sd = sd(response) * 2.5)
                 }),
                 bernoulli = u_b_b,
                 binomial = u_b_b,
                 "beta_binomial" = u_b_b,
                 beta = u_b_b)
  }
  # The declaration replaces the bot entry of whichever set was built, so a
  # user with an incomplete design keeps the rest of the regularizing set.
  # bnec() and bnec_group() refuse a response with no floor before their
  # loops; this refusal serves get_priors() and amend(). Skip an equation with
  # no bot, such as nec3param, so that it is not refused.
  if (!asymptote_observed && "bot" %in% equation_par_names(model)) {
    floor_val <- asymptote_floor(family, response)
    if (!is.finite(floor_val)) {
      stop(asymptote_floor_error(no_floor_reason(family, response)),
           call. = FALSE)
    }
    branch <- if (gamma_scaled) {
      "gamma"
    } else if (identical(fam_tag, "gaussian")) {
      "normal"
    } else {
      "beta"
    }
    y_b_prs[fam_tag] <- unobserved_bot_entry(
      branch, unobserved_endpoint_mean(predictor, response, gamma_scaled)
    )
  }
  # One construction for nec and ec50; see predictor_prior(). prior_type
  # changes its spread only.
  x_pr <- predictor_prior(prior_predictor, prior_type = prior_type,
                          predictor_scale = predictor_scale)
  lbs <- c(Gamma = 0, poisson = 0, negbinomial = 0, gaussian = NA,
           bernoulli = 0, binomial = 0, "beta_binomial" = 0, beta = 0)
  ubs <- c(Gamma = NA, poisson = NA, negbinomial = NA, gaussian = NA,
           bernoulli = 1, binomial = 1, "beta_binomial" = 1, beta = 1)
  # y-dependent priors
  pr_top <- prior_string(y_t_prs[fam_tag], nlpar = "top",
                         lb = lbs[fam_tag], ub = ubs[fam_tag])
  pr_bot <- prior_string(y_b_prs[fam_tag], nlpar = "bot",
                         lb = lbs[fam_tag], ub = ubs[fam_tag])
  # x-dependent priors. Take the bound from the support of the distribution,
  # not from the tested range: truncating to the range piles the posterior
  # against the highest concentration. A lognormal needs lb = 0, or Stan
  # rejects every negative proposal. The normal branch takes none, because a
  # predictor supplied already logged can be negative.
  x_lb <- if (grepl("^lognormal", x_pr)) 0 else NA
  pr_nec <- prior_string(x_pr, nlpar = "nec", lb = x_lb)
  pr_ec50 <- prior_string(x_pr, nlpar = "ec50", lb = x_lb)
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
  # ecxflat takes the ordinary top entry. A response at a bound still has one,
  # because response_link_scale() shifts it inside the support first.
  if (model == "ecxflat") {
    priors <- pr_top
  }
  if (model == "ecxhormebc4") {
    priors <- pr_top + pr_beta + pr_ec50 + pr_slope
  }
  if (model == "ecxhormebc5") {
    priors <- pr_bot + pr_top + pr_beta + pr_ec50 + pr_slope
  }
  disp_priors <- define_disp_prior(disp_spec, family, response,
                                   predictor = predictor,
                                   prior_type = prior_type,
                                   rate_response = rate_response,
                                   truncated_response = truncated_response)
  if (!is.null(disp_priors)) {
    priors <- priors + disp_priors
  }
  # response is on the link scale by this point, which is what the group-level
  # offsets are added on, so it is the right scale to take the prior from.
  group_priors <- define_group_prior(group_spec, predictor, response,
                                     prior_type = prior_type,
                                     ogl_transform = ogl_kind,
                                     par_transform = par_kind)
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
#' @param predictor The predictor, aligned with \code{response}, or
#' \code{NULL}. Read only for the replicate centre of the
#' \code{"regularizing"} set.
#' @param prior_type One of \code{"uninformative"} or \code{"regularizing"}.
#' @param rate_response Whether the response was divided by a \code{rate()}
#' denominator before the prior was built.
#' @param truncated_response Whether the count likelihood is zero-truncated.
#'
#' @details Only a variance function, such as \code{disp("power")} (route B
#' of \code{\link{parse_disp_term}}), is given priors here. A formula such as
#' \code{disp(~x)} (route A) is an ordinary distributional formula and keeps
#' the \pkg{brms} defaults, which suit a linear predictor on a log link.
#'
#' \code{c0} is the log dispersion at the variance function's reference value
#' (see \code{\link{disp_centre}}), a typical response rather than
#' \code{mu = 1}. Without that centring, \code{c0} and the slope are nearly
#' confounded. Its prior stays loose, because the reference fixes where
#' \code{c0} is read and not how large the dispersion is there.
#'
#' \code{disp_slope_priors()} builds the entry and bounds of each slope,
#' \code{c1} and \code{c2}, for this function and for \code{disp_inits()}.
#'
#' @return An object of class \code{\link[brms]{brmsprior}}, or \code{NULL}.
#'
#' @importFrom brms prior_string
#'
#' @noRd
define_disp_prior <- function(disp_spec, family, response, predictor = NULL,
                              prior_type = "uninformative",
                              rate_response = FALSE,
                              truncated_response = FALSE) {
  if (is.null(disp_spec) || disp_spec$route != "B") {
    return(NULL)
  }
  # A family taken from a fit records Gamma as gamma.
  fam_tag <- canonical_fam_tag(family)
  c0_prs <- disp_intercept_priors(response)
  out <- prior_string(unname(c0_prs[fam_tag]), nlpar = "c0")
  slopes <- disp_slope_priors(disp_spec, family, response, predictor,
                              prior_type = prior_type,
                              rate_response = rate_response,
                              truncated_response = truncated_response)
  for (p in names(slopes)) {
    s <- slopes[[p]]
    out <- out + prior_string(s$prior, nlpar = p, lb = s$lb, ub = s$ub)
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
#' @param ogl_transform The output of \code{\link{ogl_transform_kind}} for this
#' model and family, or \code{"none"}.
#' @param par_transform The output of \code{\link{par_transform_kind}} for this
#' family, or \code{"none"}.
#'
#' @details Without this, a group-level standard deviation takes the
#' \pkg{brms} default \code{student_t(3, 0, 2.5)}. Under the identity link
#' \code{\link{bnec}} assigns, an offset at that scale puts a bounded mean
#' outside its support, and no inverse link brings it back, so the prior must
#' keep the mean in range.
#'
#' The scale is one tenth of the observed range of the scale the parameter is
#' on:
#'
#' \itemize{
#'   \item \code{top}, \code{bot} and \code{ogl}: the response,
#'     \code{diff(range(response)) / 10}, converted to the log or log-odds
#'     scale where the deviation is applied multiplicatively.
#'   \item \code{nec} and \code{ec50}: the predictor,
#'     \code{diff(range(predictor)) / 10}.
#'   \item \code{beta}, \code{slope}, \code{d} and \code{f}: 0.5, one tenth of
#'     their own \code{normal(0, 5)}.
#' }
#'
#' The scale is taken from the data, not from the parameter's own prior, whose
#' scale is chosen to be diffuse. \code{student_t(3, 0, s)} keeps the shape of
#' the \pkg{brms} default and changes only its scale.
#'
#' \pkg{brms} declares the deviations \code{r_} unconstrained, so no prior here
#' keeps \code{top}, \code{bot} or \code{nec} inside their support. A grouped
#' fit on a bounded response can therefore diverge; see
#' \code{\link{add_brm_defaults}} for the raised \code{adapt_delta}.
#'
#' Where the deviation is applied multiplicatively, as
#' \code{p = m e^o / (1 - m + m e^o)} on (0, 1) or \code{p = m e^o} on
#' (0, Inf), no value of \code{o} leaves the support. The standard deviation is
#' then declared on the deviation term, \code{botgl} rather than \code{bot},
#' and the term has no population intercept to give a prior to; see
#' \code{\link{add_par_gl_term}}.
#'
#' \code{"regularizing"} narrows every scale by \code{regularizing_factor},
#' the factor the response-scaled priors use, except the fallback for a
#' response or predictor with no spread.
#'
#' The \code{ogl} population intercept also takes a prior, centred at zero.
#' \code{ogl} is an offset on the whole curve, so a constant added to it can be
#' taken back out of \code{top} and \code{bot}; the prior identifies the
#' split.
#'
#' @return An object of class \code{\link[brms]{brmsprior}}, or \code{NULL}.
#'
#' @importFrom brms prior_string
#'
#' @noRd
define_group_prior <- function(group_spec, predictor, response,
                               prior_type = "uninformative",
                               ogl_transform = "none",
                               par_transform = "none") {
  if (is.null(group_spec) || length(group_spec$nlpars) == 0) {
    return(NULL)
  }
  narrow <- if (prior_type == "regularizing") 1 / regularizing_factor else 1
  # A response or predictor with no spread gives a scale of zero. Fall back to
  # the dimensionless scale, and do not narrow the fallback: it stands in for a
  # scale that could not be measured.
  safe_scale <- function(x) {
    s <- diff(range(x)) / (10 * narrow)
    if (!is.finite(s) || s <= 0) 0.5 else s
  }
  s_y <- safe_scale(response)
  s_x <- safe_scale(predictor)
  # A multiplicative ogl deviation is on the log or log-odds scale, so convert
  # s_y by the delta method at the mean response:
  #
  #   s_log   = s_y / mean(y)             a group-level coefficient of variation
  #   s_logit = s_y / (m * (1 - m))
  #
  # The Jacobian varies along the curve, so the conversion is approximate.
  m_y <- mean(response, na.rm = TRUE)
  # The kinds are arguments, computed by define_prior(), so that bnec(),
  # get_priors() and amend() give the same width.
  #
  # `cap` caps the logit branch as well, for a deviation on top or bot. The
  # conversion is made at the response mean, and bot sits near zero, so the
  # width would otherwise be unbounded on a response whose mean is near a
  # bound. The ogl logit branch is left uncapped.
  converted_scale <- function(kind, cap = FALSE) {
    out <- switch(
      kind,
      # Capped: s_y does not shrink as the mean approaches zero, so on a count
      # response with many zeros s_y / m_y is unbounded. Cap at 1 / narrow,
      # not at 1, so that "regularizing" still narrows the width where the cap
      # binds.
      log = if (is.finite(m_y) && m_y > 0) min(s_y / m_y, 1 / narrow) else s_y,
      logit = if (is.finite(m_y) && m_y > 0 && m_y < 1) {
        s_y / (m_y * (1 - m_y))
      } else {
        s_y
      },
      s_y
    )
    # Cap at 1 / narrow for the same reason: narrow enters s_y before the
    # conversion, so a constant cap would give both prior types one width.
    if (cap && !identical(kind, "none")) min(out, 1 / narrow) else out
  }
  s_ogl <- converted_scale(ogl_transform)
  s_par <- converted_scale(par_transform, cap = TRUE)
  scale_for <- function(par) {
    if (par == "ogl") {
      s_ogl
    } else if (par %in% c("top", "bot")) {
      # A transformed deviation takes the converted width, as ogl does.
      if (par_is_transformed(par, par_transform)) s_par else s_y
    } else if (par %in% c("nec", "ec50")) {
      s_x
    } else {
      0.5 / narrow
    }
  }
  # Declare the standard deviation under the deviation term's name: a
  # transformed term puts (1 | group) on botgl. A class "sd" prior on nlpar
  # "bot" would match nothing, and brms would drop it silently.
  nlpar_for <- function(par) {
    if (par_is_transformed(par, par_transform)) {
      unname(par_gl_names(par)[["dev"]])
    } else {
      par
    }
  }
  out <- NULL
  for (p in group_spec$nlpars) {
    pr <- prior_string(paste0("student_t(3, 0, ", signif(scale_for(p), 4), ")"),
                       class = "sd", nlpar = nlpar_for(p))
    out <- if (is.null(out)) pr else out + pr
  }
  if (isTRUE(group_spec$ogl)) {
    out <- out + prior_string(paste0("normal(0, ", signif(s_ogl, 4), ")"),
                              nlpar = "ogl")
  }
  # No prior on a deviation intercept: add_par_gl_term() writes
  # botgl ~ 0 + (1 | group), so brms declares no b_botgl. A free intercept
  # would be unidentified against bot. ogl does take one.
  out
}

#' define_hurdle_prior
#'
#' Builds priors for both parameter blocks of a joint hurdle fit.
#'
#' @inheritParams define_prior
#'
#' @details The mu block takes the defaults of its own family, from
#' \code{hurdle_mu_family()}, built from the non-zero responses only, because
#' the zeros would pull \code{top} and \code{bot} below the control level. The
#' second block is a probability and takes the bernoulli defaults. It is
#' written as \code{1 - survival}, so its \code{top} (\code{hutop}, for
#' example) is control survival.
#'
#' @return An object of class \code{\link[brms]{brmsprior}}.
#'
#' @importFrom stats Gamma
#' @importFrom brms prior_string
#'
#' @noRd
define_hurdle_prior <- function(model, family, predictor, response,
                                prior_type = "uninformative",
                                asymptote_observed = TRUE,
                                model_survival = NULL,
                                prior_predictor = NULL,
                                predictor_scale = "auto") {
  if (is.null(prior_predictor)) {
    prior_predictor <- predictor
  }
  dpar <- hurdle_dpar(family)
  # The second block may carry a different equation from the response block,
  # in which case its priors must be built for that equation's parameters.
  if (is.null(model_survival)) {
    model_survival <- model
  }
  parts <- split_hurdle_response(predictor, response)
  # mu block: the defaults of the non-zero response's family, built from the
  # non-zeros only. The declaration reaches both blocks, because a design that
  # stopped before one block's lower asymptote stopped before the other's.
  mu_priors <- define_prior(model, hurdle_mu_family(family),
                            parts$mu$x, parts$mu$y, prior_type = prior_type,
                            asymptote_observed = asymptote_observed,
                            prior_predictor = prior_predictor,
                            predictor_scale = predictor_scale)
  # second block: reuse the bernoulli/identity defaults on the proportion
  # non-zero, then rename every non-linear parameter into its namespace.
  hu_priors <- define_prior(model_survival, bernoulli(link = "identity"),
                            parts$hu$x, parts$hu$y, prior_type = prior_type,
                            asymptote_observed = asymptote_observed,
                            prior_predictor = prior_predictor,
                            predictor_scale = predictor_scale)
  hu_priors$nlpar <- ifelse(nzchar(hu_priors$nlpar),
                            paste0(dpar, hu_priors$nlpar), hu_priors$nlpar)
  # Each block is primed from a subset but evaluated over the whole predictor,
  # so prior_predictor builds the nec and ec50 prior from the whole predictor.
  # A prior from the survivors alone would place the threshold below the
  # highest concentration at which anything survived, and has no scale where
  # the only survivors are at the zero control.
  mu_priors + hu_priors
}
