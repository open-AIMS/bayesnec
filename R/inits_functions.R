#' Treat an empty-string prior bound as absent
#'
#' \pkg{brms} records an absent bound as \code{""} in the \code{prior} slot a
#' fitted object carries, while \code{define_prior()} and \code{brms::prior()}
#' use \code{NA}. All three mean unbounded, but the bound-respecting redraw in
#' \code{make_inits()} tests with \code{is.na()}, so \code{""} was read as a
#' bound, then coerced to \code{NA} by \code{as.numeric()}, leaving a
#' \code{while (NA)} and the error "missing value where TRUE/FALSE needed".
#' That is what stopped a fit's own priors from being usable as a \code{prior}
#' argument. See #141.
#'
#' @param priors A \code{\link[base]{data.frame}} of priors.
#'
#' @return \code{priors}, with blank bounds set to \code{NA}.
#'
#' @noRd
blank_bounds_to_na <- function(priors) {
  for (bound in c("lb", "ub")) {
    if (bound %in% names(priors)) {
      priors[[bound]][!nzchar(as.character(priors[[bound]]))] <- NA
    }
  }
  priors
}

#' Recognise a brms constant() prior, and read the value it fixes
#'
#' A \code{constant()} prior is a point mass rather than a distribution, so it
#' has no entry in the sampling tables \code{make_inits()},
#' \code{refine_inits()} and \code{sample_priors()} use, and looking it up
#' there raised "attempt to apply non-function". These two helpers are the
#' single place the form is parsed. See #244.
#'
#' @param prior A \code{\link[base]{character}} vector of prior strings.
#'
#' @return \code{is_constant_prior()} a \code{\link[base]{logical}} vector;
#' \code{constant_prior_value()} a \code{\link[base]{numeric}} vector of the
#' fixed values.
#'
#' @noRd
is_constant_prior <- function(prior) {
  grepl("^\\s*constant\\s*\\(", as.character(prior))
}

#' @noRd
constant_prior_value <- function(prior) {
  out <- vapply(as.character(prior), constant_one_value, numeric(1),
                USE.NAMES = FALSE)
  if (any(is.na(out))) {
    stop("A constant() prior must fix a single numeric value; could not read ",
         paste0(prior[is.na(out)], collapse = ", "), ".")
  }
  out
}

#' The fixed value of a single constant() prior string
#'
#' Two things a plain \code{as.numeric()} on the bracket contents gets wrong,
#' both of them legal \code{brms} priors: \code{constant()} takes a second
#' \code{broadcast} argument, so \code{constant(0.5, broadcast = FALSE)} is
#' not a number; and the value is an R expression rather than a literal, so
#' \code{constant(1/2)} is as valid as \code{constant(0.5)}. Both used to
#' reach the "must fix a single numeric value" error.
#'
#' Evaluated in \code{\link[base]{baseenv}}, so the expression sees base R and
#' nothing of the caller's workspace --- a prior is a specification, not a hook
#' for arbitrary code from elsewhere in the session.
#'
#' @param x A \code{\link[base]{character}} string.
#'
#' @return A \code{\link[base]{numeric}} vector of length 1, \code{NA} if the
#' value could not be read.
#'
#' @noRd
constant_one_value <- function(x) {
  inner <- sub("^\\s*constant\\s*\\(\\s*", "", x)
  inner <- sub("\\s*\\)\\s*$", "", inner)
  arg <- strsplit(inner, ",", fixed = TRUE)[[1]][1]
  # The literal case first, so the overwhelmingly common form never goes near
  # parse(). suppressWarnings: a non-literal is handled below, and the coercion
  # warning would say the same thing less clearly.
  out <- suppressWarnings(as.numeric(arg))
  if (!is.na(out)) {
    return(out)
  }
  out <- tryCatch(eval(parse(text = arg), envir = baseenv()),
                  error = function(e) NA_real_)
  if (!is.numeric(out) || length(out) != 1) {
    return(NA_real_)
  }
  as.numeric(out)
}

#' Random generators for the prior distributions bayesnec can draw from
#'
#' @details Initial values and \code{\link{sample_priors}} both need to draw
#' from a prior written as a \pkg{brms} prior string, which requires a
#' generator per distribution name. The list was previously written out at each
#' of the three places that needs it, so a distribution added at one was absent
#' at the others, and an unrecognised name reached \code{fcts[[dist]](...)} as
#' \code{NULL} and failed with "attempt to apply non-function" -- naming
#' neither the prior nor the distribution. See #302.
#'
#' \code{lognormal} is included because the predictor-scaled parameters are
#' bounded below at zero and spread over orders of magnitude on a dilution
#' series, which is what a lognormal describes and a gamma does not.
#'
#' @return A named \code{\link[base]{list}} of functions.
#'
#' @importFrom stats rgamma rnorm rbeta runif rlnorm
#'
#' @noRd
prior_samplers <- function() {
  list(gamma = rgamma, normal = rnorm, beta = rbeta, uniform = runif,
       lognormal = rlnorm)
}

#' Look up the generator for one prior distribution
#'
#' @param dist A \code{\link[base]{character}} string naming the distribution.
#'
#' @return A \code{\link[base]{function}}.
#'
#' @noRd
prior_sampler <- function(dist) {
  fcts <- prior_samplers()
  # The name is parsed out of a prior string, and a string a user wrote by hand
  # can carry surrounding whitespace that prior_string() would not produce.
  dist <- trimws(dist)
  if (!dist %in% names(fcts)) {
    stop("bayesnec cannot draw initial values or prior samples from a \"",
         dist, "\" prior. It draws from: ",
         paste(names(fcts), collapse = ", "),
         ". Supply the prior on one of those distributions, or fit with",
         " brms directly.", call. = FALSE)
  }
  fcts[[dist]]
}

#' make_inits
#'
#' Creates list of initialisation values
#'
#' @inheritParams bnec
#'
#' @param fct_args A \code{\link[base]{character}} string containing
#' the expected argument names to be used.
#' @param priors an object of class \code{\link[brms]{brmsprior}} from package
#' \pkg{brms}.
#' @param chains Number of chains to be passed to \pkg{brms} model.
#'
#' @seealso \code{\link{bnec}}
#' @return A \code{\link[base]{list}} containing the initialisation values.
#'
#' @noRd
make_inits <- function(model, fct_args, priors, chains) {
  priors <- blank_bounds_to_na(as.data.frame(priors))
  priors <- priors[priors$prior != "", ]
  # Only the curve's own coefficients are the business of the initial-value
  # search. Any prior carrying a class other than "b" describes no part of the
  # mean curve -- the family's dispersion parameter (sigma, shape, phi), the
  # mixing probability of a single-block zero-inflated family (zi, hu), a
  # group-level standard deviation (sd) -- and previously made the name check
  # below fail outright, so a user simply could not supply one. Note the filter
  # is general, not a list of dispersion classes: anything that is not "b" is
  # out, which is the correct rule and needs no maintenance as families are
  # added. Dropping them here is the same move add_brm_defaults() already makes
  # for the parameters a disp() variance function introduces, and it has the
  # second effect the fix needs: no initial value is generated for them, which
  # is correct. Stan random-initialises any parameter absent from an init list,
  # and bayesnec has never given sigma an init, so nothing downstream needs
  # teaching. The priors themselves still reach brm(); only the init search
  # ignores them. See #207 and #231.
  priors <- priors[priors$class == "b", ]
  par_names <- character(length = nrow(priors))
  for (j in seq_along(par_names)) {
    sep <- ifelse(priors$class[j] == "b", "_", "")
    par_names[j] <- paste(priors$class[j],
                          priors$nlpar[j],
                          sep = sep)
  }
  check_args <- identical(sort(par_names), sort(fct_args))
  if (!check_args) {
    out_args <- gsub("^b_", "", fct_args)
    out_pars <- gsub("^b_", "", par_names)
    stop("In model ", model, ", user-specific parameter ",
         "prior names (",
         paste0(out_pars, collapse = ", "), ") do not ",
         "match expectation (",
         paste0(out_args, collapse = ", "),
         "). Consider ",
         "reconstructing your priors; check necessary ",
         "parameters with show_params(\"", model, "\")")
  }
  out <- vector(mode = "list", length = chains)
  for (i in seq_along(out)) {
    out[[i]] <- vector(mode = "list", length = nrow(priors))
    names(out[[i]]) <- par_names
    for (j in seq_len(nrow(priors))) {
      # A constant() prior fixes the parameter, so it is assigned rather than
      # sampled, and the bound-respecting redraw below is skipped -- a constant
      # sitting outside its own lb/ub would spin that while loop forever. The
      # value is kept in the list here on purpose: make_good_inits() evaluates
      # the candidate curve, and a parameter fixed at, say, bot = 0 is
      # genuinely part of that curve. It is removed only where the list is
      # handed to brm(), in add_brm_defaults(), because Stan does not declare a
      # parameter whose prior is constant. See #244.
      if (is_constant_prior(priors$prior[j])) {
        out[[i]][[j]] <- constant_prior_value(priors$prior[j])
      } else {
        bits <- gsub("\\(|\\)", ",", priors$prior[j])
        bits <- strsplit(bits, ",", fixed = TRUE)[[1]]
        fct_i <- prior_sampler(bits[1])
        v1 <- as.numeric(bits[2])
        v2 <- as.numeric(bits[3])
        out[[i]][[j]] <- fct_i(1, v1, v2)
        if (any(!is.na(priors[j, c("lb", "ub")]))) {
          n_bounds <- sum(!is.na(priors[j, c("lb", "ub")]))
          if (n_bounds == 2) {
            bounds <- as.numeric(priors[j, c("lb", "ub")])
            while (out[[i]][[j]] <= min(bounds) |
                     out[[i]][[j]] >= max(bounds)) {
              out[[i]][[j]] <- fct_i(1, v1, v2)
            }
          } else if (n_bounds == 1) {
            direction <- c("lb", "ub")[!is.na(priors[j, c("lb", "ub")])]
            bound_fct <- ifelse(direction == "lb", `<=`, `>=`)
            bounds <- as.numeric(priors[j, direction])
            while (bound_fct(out[[i]][[j]], bounds)) {
              out[[i]][[j]] <- fct_i(1, v1, v2)
            }
          }
        }
      }
      if (priors$class[j] == "b") {
        dim(out[[i]][[j]]) <- 1
      }
    }
  }
  out
}

#' refine_inits
#'
#' For a single chain's init list that fails prediction checks, attempt to
#' fix it by re-drawing one parameter at a time while holding the others
#' fixed.  Targets \code{slope}, \code{d} and \code{beta} -- the parameters
#' most likely to push hormesis / sigmoidal predictions out of range.
#'
#' @param init A named \code{\link[base]{list}} of init values for one chain.
#' @param x Sorted predictor values.
#' @param pred_fct The prediction function for the model.
#' @param fct_args Parameter names expected by the prediction function.
#' @param limits A length-2 \code{\link[base]{numeric}} vector, the band from
#'   \code{\link{init_limits}}.
#' @param priors A \code{\link[base]{data.frame}} of priors (already filtered
#'   to non-empty rows).
#' @param n_sub Maximum number of single-parameter re-draws per parameter.
#'
#' @return The (possibly improved) init list.
#' @noRd
refine_inits <- function(init, x, pred_fct, fct_args, limits,
                         priors, n_sub = 500) {
  preds <- get_init_predictions(init, x, pred_fct, fct_args)
  if (check_init_predictions(preds, limits)) {
    return(init)
  }
  # refine_inits nudges a finite-but-out-of-range curve back into range by
  # re-drawing slope/d/beta. NaN predictions signal a structural problem
  # instead -- e.g. models that raise the predictor to a fractional power
  # (nechormepwr, nechorme4pwr, ecxsigm) return NaN wherever x < 0, for every
  # parameter draw. No re-draw fixes that, so bail early rather than burn the
  # full n_sub search on a hopeless case. We test NaN specifically rather than
  # all non-finite values: Inf from numerical overflow can sometimes be cured
  # by re-drawing a parameter smaller, so those are left for the search below.
  if (any(is.nan(preds))) {
    return(init)
  }
  # Identify tunable parameters (slope, d, beta -- those most likely
  # to push predictions out of range in hormesis / sigmoidal models).
  tunable <- intersect(names(init), c("b_slope", "b_d", "b_beta"))
  for (par in tunable) {
    pr_row <- which(priors$nlpar == gsub("^b_", "", par))
    if (length(pr_row) != 1) next
    # Nothing to tune on a parameter the user has fixed, and the sampling
    # table below has no constant() entry. See #244.
    if (is_constant_prior(priors$prior[pr_row])) next
    bits <- gsub("\\(|\\)", ",", priors$prior[pr_row])
    bits <- strsplit(bits, ",", fixed = TRUE)[[1]]
    fct_i <- prior_sampler(bits[1])
    v1 <- as.numeric(bits[2])
    v2 <- as.numeric(bits[3])
    for (k in seq_len(n_sub)) {
      candidate <- init
      new_val <- fct_i(1, v1, v2)
      if (priors$class[pr_row] == "b") {
        dim(new_val) <- 1
      }
      candidate[[par]] <- new_val
      preds <- get_init_predictions(candidate, x, pred_fct, fct_args)
      if (check_init_predictions(preds, limits)) {
        return(candidate)
      }
    }
  }
  init
}

#' The interval the initial curve is permitted to occupy, on its own scale
#'
#' @param family A \code{\link[stats]{family}} object, or \code{NULL}.
#'
#' @details \code{\link{mu_support}} states what the likelihood permits the
#' \strong{mean} to be. The initial-value search does not evaluate the mean: a
#' \pkg{brms} non-linear formula defines the linear predictor, and
#' \code{pred_<model>()} returns that, so the curve and the response the band is
#' built from are both on the link scale --- \code{fit_bayesnec()} passes
#' \code{response_link_scale(response, family)}.
#'
#' Under the identity link the two scales are the same and the support is the
#' bound. Under any other link it is not, and clamping the linear predictor to
#' the support of the mean excludes the region the curve occupies: measured on a
#' \code{beta_binomial(link = "logit")} response of proportions from 0.083 to
#' 0.932, whose logit-scale response runs -2.40 to 2.62, an unmapped clamp gives
#' a band of exactly (0, 1) and so admits only means between 0.5 and 0.73.
#'
#' The support is therefore mapped through the link. \code{log} takes (0, 1) to
#' (-Inf, 0) and (0, Inf) to the whole line; \code{logit} takes (0, 1) to the
#' whole line. Those are the three links \code{\link{supported_links}} admits;
#' anything else leaves the band unbounded, which is the released behaviour and
#' cannot reject a curve that the likelihood would accept.
#'
#' @return A \code{\link[base]{numeric}} vector of length 2.
#'
#' @importFrom stats qlogis
#'
#' @noRd
init_support <- function(family) {
  support <- mu_support(family)
  if (all(is.infinite(support)) || is.null(family$link)) {
    return(c(-Inf, Inf))
  }
  if (identical(family$link, "identity")) {
    return(support)
  }
  mapped <- switch(family$link,
                   log = suppressWarnings(log(support)),
                   logit = suppressWarnings(qlogis(support)),
                   c(-Inf, Inf))
  if (any(is.na(mapped))) {
    return(c(-Inf, Inf))
  }
  sort(mapped)
}

#' The mean response at each replicated predictor value
#'
#' @param x A \code{\link[base]{numeric}} vector, the predictor.
#' @param y A \code{\link[base]{numeric}} vector, the response on the link
#' scale.
#'
#' @details A group mean is an estimate of the mean response at that
#' concentration, which is the same quantity an initial curve returns there, so
#' the two are on the same footing and a level the design measured cannot be
#' out of bounds. Only replicated values are returned: where a concentration
#' holds one observation its "mean" is that observation, and admitting those
#' would put \code{\link{init_limits}} back on the extrema it exists to leave.
#'
#' @return A \code{\link[base]{numeric}} vector, possibly empty.
#'
#' @noRd
replicated_group_means <- function(x, y) {
  groups <- split(y, factor(x))
  groups <- groups[lengths(groups) > 1]
  if (length(groups) == 0) {
    return(numeric(0))
  }
  vapply(groups, mean, numeric(1), USE.NAMES = FALSE)
}

#' The spread of a response with nothing replicated, read from its successive
#' differences
#'
#' @param x A \code{\link[base]{numeric}} vector, the predictor.
#' @param y A \code{\link[base]{numeric}} vector, the response on the link
#' scale.
#'
#' @details For a curve that is smooth between neighbouring concentrations the
#' variance of the successive differences is twice the noise variance, so
#' \code{sd(diff(y)) / sqrt(2)} estimates it. It is biased upward by whatever
#' the curve does between the two points, which is the safe direction, and
#' unlike the spread of the whole response it does not grow with the size of the
#' effect.
#'
#' \strong{It is read over each half of the series and the larger taken.} A
#' single number is the wrong summary where the dispersion changes across the
#' predictor, which is the ordinary case for a growth or a count endpoint and is
#' what the \code{disp()} variance function exists for. On the one simulated
#' cell that decided the width of \code{\link{init_limits}} --- an unreplicated
#' design with a fivefold rise in dispersion --- the whole-series estimate is
#' 2.35 against a true standard deviation running 1.2 to 4.0, and every failure
#' to cover the asymptotes was at the lower end of the response, where the noise
#' is largest. Taking the larger of the two half-series estimates covers that
#' cell completely at a width of four, where the whole-series estimate needed
#' five, so the halves remove a width the band would otherwise have paid on
#' every design to compensate for an estimator that is wrong on this one.
#'
#' Each half needs at least three differences before it is read on its own;
#' below that the whole series is used, because a standard deviation of two
#' differences is not an estimate of anything and taking the larger of two such
#' would be a maximum over noise.
#'
#' @return A \code{\link[base]{numeric}} of length 1, \code{NA} where the series
#' is too short to difference.
#'
#' @importFrom stats sd
#'
#' @noRd
successive_difference_spread <- function(x, y) {
  ordered <- y[order(x)]
  n <- length(ordered)
  if (n < 3) {
    return(NA_real_)
  }
  whole <- sd(diff(ordered)) / sqrt(2)
  half <- floor(n / 2)
  if (half < 4) {
    return(whole)
  }
  lower <- sd(diff(ordered[seq_len(half)])) / sqrt(2)
  upper <- sd(diff(ordered[(half + 1):n])) / sqrt(2)
  halves <- c(lower, upper)
  halves <- halves[is.finite(halves) & halves > 0]
  if (length(halves) == 0) {
    return(whole)
  }
  max(halves)
}

#' The pooled within-group standard deviation of the response
#'
#' @param x A \code{\link[base]{numeric}} vector, the predictor.
#' @param y A \code{\link[base]{numeric}} vector, the response on the link
#' scale.
#'
#' @details The spread \code{\link{init_limits}} widens its band by. It is the
#' variation of the response about the level of its own concentration, which is
#' what the band has to allow for once every level's mean is inside it.
#'
#' Two alternatives were measured and are not used. The largest single group's
#' standard deviation is the least robust of the three: on a replicated design
#' a single aberrant observation widened the band by a factor of 6.7, against
#' 4.1 for the pooled spread and 3.0 for \code{range(y)}, because a maximum over
#' groups follows whichever group that observation lands in. The standard
#' deviation of the whole response is the most robust, at 1.6, and is not a
#' spread at all -- it grows with the size of the effect, giving a band 2.6 to
#' 3.8 times the response range on the replicated designs measured against 1.1
#' to 1.4 for the pooled one, so it widens the band for a reason that has
#' nothing to do with the noise.
#'
#' \strong{Where the replicates say too little} the spread is read from the
#' response's successive differences instead; see
#' \code{\link{successive_difference_spread}}. On the eight-concentration
#' unreplicated design measured that gives 0.068 against 0.395 for
#' \code{sd(y)}, and a band 1.7 times the response range against 4.4. Below
#' three observations there are too few differences and the spread of the whole
#' response stands in.
#'
#' A pooled variance is not robust either, and a single aberrant observation
#' widens the band: on a four-concentration design of six replicates, replacing
#' one control observation by a value three times the control mean widened the
#' band by a factor of 4.1, against 3.0 for \code{range(y)}. That is the
#' permissive direction -- the wider band contains the narrower one, so every
#' starting value accepted before is still accepted -- and what the aberrant
#' observation does not do is move the band's ends, which shift by a sixth of
#' it rather than by the whole of it as \code{max(y)} does.
#'
#' @return A \code{\link[base]{numeric}} of length 1.
#'
#' @importFrom stats var sd
#'
#' @noRd
group_spread <- function(x, y) {
  groups <- split(y, factor(x))
  n <- lengths(groups)
  replicated <- n > 1
  # The branch is chosen on how much the replicates actually say, not on
  # whether any exist. One repeated predictor value contributes a variance on a
  # single degree of freedom, and taking it in preference to the twenty-odd
  # differences the rest of the design offers made the spread a function of that
  # one pair: on a 24-point series, duplicating one value with an equal response
  # sent the pooled estimate to zero and the band from 1.35 to 4.83 times the
  # response range, and five draws of a non-equal pair gave estimates spanning a
  # factor of seven. Three degrees of freedom is the least that reads as an
  # estimate rather than as one pair.
  df <- sum(n[replicated] - 1)
  differences <- successive_difference_spread(x, y)
  out <- if (any(replicated) && df >= 3) {
    v <- vapply(groups[replicated], var, numeric(1))
    sqrt(sum((n[replicated] - 1) * v) / df)
  } else {
    differences
  }
  # Falling back to the differences and not to sd(y), which is the statistic
  # this function exists to avoid.
  if (!is.finite(out) || out <= 0) {
    out <- differences
  }
  if (!is.finite(out) || out <= 0) sd(y) else out
}

#' The band an initial curve is required to lie within
#'
#' @param x A \code{\link[base]{numeric}} vector, the predictor.
#' @param y A \code{\link[base]{numeric}} vector, the response on the link
#' scale.
#' @param width The number of \code{\link{group_spread}} units the band extends
#' beyond the outermost level mean.
#' @param zero_bounded Passed to \code{\link{regularizing_location}}, which
#' treats a zero differently at the two ends of the predictor series.
#' @param support A \code{\link[base]{numeric}} vector of length 2, the
#' interval the curve is permitted to occupy on its own scale, from
#' \code{\link{init_support}}. The band is intersected with it.
#' @param spread_fn The estimator of the spread. Exposed so that
#' \code{notes/scripts/init_search_audit.R} can compare alternatives with
#' everything else held at what the package does; no caller in the package
#' passes it.
#'
#' @details \code{\link{check_init_predictions}} requires the initial curve to
#' lie between these two values. They used to be \code{range(y)}, the smallest
#' and largest single observations, and three properties make that the wrong
#' reference rather than merely a strict one. See #309.
#'
#' It compares quantities that are not on the same footing. The initial curve's
#' upper asymptote is an estimate of the mean response at the control, and
#' \code{max(y)} is the largest single observation in the dataset.
#'
#' It is tied to the prior it filters. Both \code{max(y)} and the location of
#' the \code{top} prior are read from the same response, and on a declining
#' curve both are read from the control, so the threshold lands close to the
#' centre of the prior it is testing and the outcome is near a coin toss
#' whatever the data show. Measured on the packaged \code{alga}
#' \code{c_proliferum} contaminant A series under the default
#' \code{"uninformative"} priors, the \code{top} prior is
#' \code{normal(0.1284, 0.4044)} and the threshold \code{max(y)} is 0.1367 --
#' 0.021 prior standard deviations from the centre -- and over 2,000 draws per
#' equation the clause passed 46.2 to 61.4 per cent of them for every one of the
#' fourteen equations of the declining set.
#'
#' And it gets looser as replicates are added, because an extremum drifts
#' outward with sample size. A starting-value check that is more permissive on
#' a larger design is the wrong way round.
#'
#' \strong{What the band is.} Every mean response the design estimates, widened
#' by the variation about those means. The means are those of the replicated
#' predictor values, plus the two \code{\link{regularizing_location}} anchors --
#' the mean of the observations at the end of the predictor series where the
#' parameter is the level of the curve, the control end for \code{top} and the
#' highest concentrations for \code{bot}. That is the anchor #307 adopted for
#' the regularizing \code{top} and \code{bot} priors, so the search and that
#' prior set read the ends of the curve from the same statistic. The
#' \code{"uninformative"} set, which is the default, locates \code{top} and
#' \code{bot} at quantiles of the pooled response instead and is unchanged by
#' this, so the shared definition covers one of the two prior sets.
#'
#' The two do different work. On a replicated design the anchors lie inside the
#' range of the group means, which is where the band's ends come from; on an
#' unreplicated one there are no group means at all and the anchors are the
#' whole band, because \code{\link{regularizing_location}} averages
#' neighbouring concentrations there rather than reducing to a single
#' observation.
#'
#' Every quantity the band is built from is a mean, which is what the asymptote
#' it is compared against estimates, and none of them drifts with sample size.
#' The one exception is where the band reaches the boundary of the support and
#' is set back from it, which is read from an extremum and does drift; see
#' \code{\link{boundary_inset}} for what that is for and what it gives up.
#'
#' \strong{The width.} Four standard deviations. The rule is the smallest width
#' that covers the asymptotes of the curve that generated the data in every cell
#' measured, because a band that excludes them rejects a correct starting value
#' and a wider one admits a starting point further into a tail. Two kinds of
#' cell are out of scope, both because no width covers them and the rule would
#' otherwise never be satisfied: a design whose predictor stops short of the
#' crossing, and the count design described at \code{\link{boundary_inset}}
#' whose generating \code{bot} lies below the floor. Both were confirmed
#' width-invariant before being excluded.
#'
#' Measured over 6,480 simulated responses: three generating processes ---
#' gaussian, Beta and poisson, so that the clamp and the boundary inset are
#' exercised and not only the band --- by three predictor grids, three
#' replication levels, three equations, a steep and a shallow curve, constant
#' and fivefold-rising dispersion, twenty seeds. Of the 269 in-scope cells, four
#' covers all of them; three covers 267, two 263 and one 240. By process at a
#' width of four: 90 of 90 gaussian, 90 of 90 Beta, and 89 of 89 poisson.
#'
#' \strong{Four rather than five, and why the earlier answer was five.} Under a
#' whole-series spread on an unreplicated design the gaussian process needed
#' five, and the cell that decided it was an unreplicated design with rising
#' dispersion, missed at the lower end of the response where the noise is
#' largest. That is a property of the estimator and not of the band, and it was
#' paid on every design including the replicated ones. Reading the spread over
#' each half of the series and taking the larger ---
#' \code{\link{successive_difference_spread}} --- covers that cell at four, so
#' the width no longer compensates for it.
#'
#' The reduction from five to four is not felt equally. On a replicated design
#' the pooled spread is unchanged and the band narrows by the full fifth. On an
#' unreplicated one the half-series estimator is 7 to 21 per cent wider than the
#' whole-series estimator it replaces, so the band narrows by 5 to 11 per cent:
#' most of the width the rule gives back is spent on the better spread.
#'
#' \strong{The support is a hard bound on it.} Under the identity link
#' \code{\link{bnec}} assigns, an initial curve outside the interval the
#' likelihood permits is not a poor starting point but an invalid one: Stan
#' rejects it and the fit ends on "Initialization failed". \code{range(y)} kept
#' the curve inside the support by accident, because a response is inside its
#' own support and the released criterion never looked beyond it. A band built
#' from a location and a spread has no such guarantee -- on the
#' \code{beta_binomial} series of #162 it reaches above 1 -- so it is
#' intersected with \code{\link{init_support}}, which is the support of the
#' mean mapped onto the scale the curve is on. The clauses that read it are
#' strict inequalities, so the curve is required strictly inside.
#'
#' \strong{What the band does not fix.} Where the highest concentration has not
#' reached the lower asymptote, every level mean and both anchors sit above the
#' true \code{bot} and widening does not reach it: over the simulated designs
#' whose predictor stops short of the crossing, coverage is 0.160 at a width
#' of four and 0.172 at five, against 0.003 at one.
#' limitation \code{\link{regularizing_location}} records for the regularizing
#' prior. The released criterion is affected identically, and worse, because
#' \code{min(y)} is above the true asymptote on such a design as well.
#'
#' @return A \code{\link[base]{numeric}} vector of length 2, the lower and
#' upper bound in that order.
#'
#' @noRd
init_limits <- function(x, y, width = 4, zero_bounded = FALSE,
                        support = c(-Inf, Inf),
                        spread_fn = group_spread) {
  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]
  y <- y[keep]
  if (length(y) == 0) {
    # No observation to anchor on, so every draw is rejected and the fit goes to
    # Stan's own initialisation. Returned as NA rather than as an inverted pair:
    # check_init_predictions() reads the band through min() and max(), which
    # reorder it, so c(Inf, -Inf) is read as the whole line and accepts
    # everything -- which is what range(y) did on this input, and is the reverse
    # of what it was taken to do. check_data() refuses an all-missing response
    # before bnec() reaches here, so this is a guard rather than a fix for an
    # observed failure.
    return(c(NA_real_, NA_real_))
  }
  # regularizing_location()'s zero-bounded branch falls back to
  # positive_scale(), which refuses a response with no positive value at all.
  # That refusal names prior construction and would arrive from inside an
  # initial-value search, so the branch is taken only where there is something
  # for it to do. define_prior() refuses such a response first on the bnec()
  # path, so this guards the direct callers.
  zero_bounded <- zero_bounded && any(y > 0)
  centres <- c(regularizing_location(x, y, "top", zero_bounded)[["location"]],
               regularizing_location(x, y, "bot", zero_bounded)[["location"]],
               replicated_group_means(x, y))
  centres <- centres[is.finite(centres)]
  spread <- width * spread_fn(x, y)
  # min() and max() over every level mean rather than the two anchors in their
  # nominal roles. The anchors are read from the ends of the predictor, and a
  # response that is not monotone at its ends -- which the alga series is not,
  # its lowest group mean falling at 15 units rather than at 20 -- puts its
  # extreme level mean somewhere in the interior.
  # A degenerate response leaves nothing to anchor on, and the fallback restores
  # the released reference for that case rather than inventing one. Tested
  # before the band is computed, because min() and max() of an empty vector
  # return infinities with a warning the user can do nothing about. Note what
  # the fallback does and does not do: on a constant response it returns a band
  # of zero width, which rejects every draw and sends the fit to Stan's
  # initialisation -- which is what range(y) did on the same response, and is
  # the right outcome, because a constant response identifies no curve.
  out <- if (length(centres) == 0) {
    range(y, na.rm = TRUE)
  } else {
    c(min(centres) - spread, max(centres) + spread)
  }
  if (!all(is.finite(out)) || out[1] > out[2]) {
    out <- range(y, na.rm = TRUE)
  }
  if (!all(is.finite(out))) {
    # Unreachable on a non-empty finite response, since range(y) is finite
    # there. c(NA, NA) and not c(-Inf, Inf) for the reason given above: an
    # unbounded pair would accept every draw, which is the reverse of what a
    # band with nothing to anchor on should do.
    return(c(NA_real_, NA_real_))
  }
  out <- c(max(out[1], support[1]), min(out[2], support[2]))
  # Where the band reaches a boundary of the support it stops at the nearest
  # value the response actually takes instead. A mean at the boundary is not
  # merely an unlikely starting point: the likelihood cannot evaluate it, and
  # under a bounded family it is arbitrarily close to one that it can. On the
  # second block of a hurdle fit the band spans the whole of (0, 1) -- one
  # survival proportion per concentration is an unreplicated series of a few
  # points, so the spread read from its successive differences is a third of the
  # response range -- and a curve accepted at a survival of 1e-66 made the joint
  # log likelihood -Inf and the fit end on "Initialization failed".
  #
  # The extremum is used here to say what the measurement can distinguish from
  # the boundary, which is what an extremum does state, and not to estimate a
  # plateau, which is what it cannot. It never crosses a level mean, so the band
  # still contains every level the design measured. Where the band does not
  # reach the boundary nothing here applies.
  out[1] <- boundary_inset(out[1], support[1], centres, spread / width, y,
                           "lower")
  out[2] <- boundary_inset(out[2], support[2], centres, spread / width, y,
                           "upper")
  out
}

#' Stop a band at the nearest observed value rather than at the support boundary
#'
#' @param edge The band's end after clamping to the support.
#' @param bound The support boundary on that side.
#' @param centres The level means the band is built from.
#' @param spread One standard deviation from \code{\link{group_spread}}.
#' @param y The response on the link scale, read only to cap the inset.
#' @param side One of \code{"lower"} or \code{"upper"}.
#' @param fraction How far from the boundary towards the nearest observed value
#' the band stops.
#'
#' @details A mean at the boundary of the support is one the likelihood cannot
#' evaluate, and the clauses of \code{\link{check_init_predictions}} are strict
#' inequalities, so a band whose end is the boundary admits a curve arbitrarily
#' close to it. On the second block of a hurdle fit that is not academic: the
#' block is primed from one survival proportion per concentration, so its band
#' spans the whole of (0, 1), and a curve accepted at a survival of 1e-66 made
#' the joint log likelihood \code{-Inf} and the fit end on "Initialization
#' failed".
#'
#' \strong{The gap is a tenth of the smaller of two quantities}: the distance
#' from the boundary to the nearest level mean inside it, and one
#' \code{\link{group_spread}}. Both are needed, because each alone fails in the
#' opposite direction.
#'
#' The distance alone ties the gap to the level of the response rather than to
#' anything near the boundary, so a design whose observations crowd the boundary
#' --- which is where a curve is most likely to be pushed onto it --- gets the
#' smallest gap, and one whose observations are far away gets the largest and
#' needs it least. On a Gamma design that stops short of its asymptote, whose
#' lowest level mean is 76.8, it gives a floor of 7.68 on a response that says
#' nothing whatever about zero.
#'
#' The spread alone fails the other way, and hard: on the second block of a
#' hurdle fit it gives 0.024 against the 6e-4 the design resolves, which
#' excludes the level means the block was primed from.
#'
#' The smaller of the two changes nothing wherever the first is already the more
#' permissive, and relaxes exactly the cases where it is strictest.
#'
#' \strong{And the gap never exceeds the distance to the closest value the
#' response takes strictly inside the boundary.} That is the one place an
#' extremum enters the band, and it enters as a cap on a tightening rather than
#' as a reference the band is built from. Where the response does not reach the
#' boundary the cap makes the inset no stricter than \code{range(y)} was: on the
#' second block of a hurdle fit both are 6e-3. The cap therefore inherits
#' \code{range(y)}'s sensitivity to one observation at that boundary: a single
#' value just inside it sets the whole gap, and can reduce the guard to almost
#' nothing. That is the price of the guarantee, and it is bounded --- the result
#' is never worse than the criterion this replaces. Where the response does
#' reach it
#' --- a count with zeros, a survival of exactly none --- \code{range(y)} put
#' its bound \emph{at} the boundary, and the inset is deliberately stricter,
#' which is the case it exists for. On a \code{zero_inflated_poisson} design of
#' six concentrations by eight the floor is 0.0375 where \code{range(y)} gave 0,
#' and 0.0275 at eighty replicates: it falls as the design resolves more.
#'
#' \strong{The nearest \emph{level mean}, not the nearest observation.} An
#' extremum drifts with sample size, which is the property the band exists to
#' remove, and on a count response it carries no information about the design at
#' all: \code{min(y[y > 0])} is 1 for any integer response, so a floor read from
#' it is 0.1 for eight replicates or eight hundred. Read from the level means it
#' is a tenth of the smallest group mean.
#'
#' One case qualifies that. Where the extreme concentration of an unreplicated
#' count design returns zero, \code{\link{regularizing_location}}'s
#' zero-bounded branch substitutes \code{min(y[y > 0]) / 10} for the
#' \code{bot} location, so the "level mean" is an extremum one layer down and
#' the floor is \code{min(y[y > 0]) / 100}. That is the residue of the
#' constant-floor defect rather than a return to it --- it tracks the data
#' where 0.1 did not --- and it is the mechanism behind the one simulated cell
#' no width covers.
#'
#' On a
#' \code{zero_inflated_poisson} design of six concentrations by eight it is
#' 0.0375 against a generating \code{bot} of 0.3, where the observation-based
#' floor was 0.1 and the observed-value floor before that was 0.375 --- above
#' the asymptote, which is the error the width is chosen to avoid.
#'
#' What replication changes is the spread of that floor and not its level. Over
#' 200 seeds of that design the median floor is 0.025 at eight replicates, at
#' eighty and at four hundred; the interquartile range falls from
#' [0.0125, 0.0375] to [0.0223, 0.0255], and the share of seeds whose top group
#' is entirely zero --- which puts a level mean on the boundary and disables the
#' inset altogether --- falls from 15 per cent to none. So the floor
#' concentrates on a tenth of the true smallest level mean rather than
#' falling.
#'
#' \strong{What this guards against, and what it does not.} A curve collapsing
#' onto the boundary, not a merely small value. On the hurdle block the floor is
#' 6e-4 and the draw that failed was 1e-66, sixty-two orders of magnitude below
#' it; a draw at 1e-5 would be accepted and gives a finite log likelihood. The
#' floor still falls as the design grows, because a group mean at the boundary
#' end does, and on that block it is \code{survival_by_x()}'s \code{eps} of
#' \code{1/(2n)} wherever a proportion is exactly 0 or 1. That is the right
#' direction: more data should mean a smaller mean is distinguishable from the
#' boundary.
#'
#' \strong{The \code{centres} term} disables the inset wherever a level mean
#' sits at the boundary itself, which is the complete-effect design --- a count
#' response whose highest concentration is entirely zero. The band must contain
#' every level the design measured, so the protection is absent exactly there.
#' Nothing else is lost by it: \code{min(y[y > bound])} is at or below every
#' centre whenever no centre is at the boundary.
#'
#' @return A \code{\link[base]{numeric}} of length 1.
#'
#' @noRd
boundary_inset <- function(edge, bound, centres, spread, y, side,
                           fraction = 0.1) {
  if (!is.finite(bound) || length(centres) == 0) {
    return(edge)
  }
  if (side == "lower" && edge > bound) {
    return(edge)
  }
  if (side == "upper" && edge < bound) {
    return(edge)
  }
  inside <- if (side == "lower") centres[centres > bound] else
    centres[centres < bound]
  if (length(inside) == 0) {
    return(edge)
  }
  nearest <- if (side == "lower") min(inside) - bound else bound - max(inside)
  gap <- fraction * min(nearest, spread)
  # And never further from the boundary than the closest value the response
  # actually takes, so that the inset cannot make the criterion stricter at this
  # boundary than range(y) was. It is a cap on a tightening and never creates
  # one, which is the only role an extremum has in the band.
  observed <- if (side == "lower") y[y > bound] else y[y < bound]
  if (length(observed)) {
    reach <- if (side == "lower") {
      min(observed) - bound
    } else {
      bound - max(observed)
    }
    gap <- min(gap, reach)
  }
  if (!is.finite(gap) || gap <= 0) {
    return(edge)
  }
  if (side == "lower") {
    max(bound, min(c(centres, bound + gap)))
  } else {
    min(bound, max(c(centres, bound - gap)))
  }
}

#' make_good_inits
#'
#' Creates list of initialisation values that generate data within the band
#' \code{\link{init_limits}} defines.
#'
#' @inheritParams bnec
#'
#' @param x A \code{\link[base]{numeric}} vector containing the x predictor.
#' @param y A \code{\link[base]{numeric}} vector containing the y response,
#' on the link scale.
#' @param report_after A \code{\link[base]{numeric}} value, the number of
#' seconds after which the search says it is still running. It does not end the
#' search: the only bound is \code{n_trials}, so that the initial values a
#' given seed produces do not depend on how busy the machine is.
#' @param n_trials A \code{\link[base]{numeric}} vector indicating
#' how many attempts the function should run before giving up.
#' @param seed seed number for reproducible random number generation. Defaults
#' to \code{NULL}.
#' @param family A \code{\link[stats]{family}} object. Two things are read from
#' it and nothing else: the interval the curve is permitted to occupy, and
#' whether the response-scaled parameters are bounded below at zero. Both are
#' properties \code{\link{init_limits}} needs, and passing the family rather
#' than the two answers is what stops them being derived from different families
#' at different call sites.
#'
#' It has no default on purpose. A default of \code{NULL} leaves the band
#' unbounded, and the constraint it drops is one \code{range(y)} used to supply
#' for free: a response is inside its own support, so the released criterion
#' could not admit a curve outside it and a band read from a location and a
#' spread can. A caller that forgot the argument would get a search that
#' silently accepts invalid starting values, which is how
#' \code{test-nechormepwr-bounded.R} began passing when it should not.
#' @param ... Additional arguments to \code{\link{make_inits}}.
#'
#' @details \strong{A chain is accepted on its own.} The four chains of a fit
#' are drawn independently, so a chain whose curve lies in the band is a draw
#' from the same distribution whether the other three passed or not. Requiring
#' all of them to pass at the same time, and re-drawing the complete set when
#' any one failed, therefore left the accepted values unchanged and raised the
#' number of proposals to the fourth power of the per-chain rate. Measured on
#' the packaged \code{alga} \code{c_proliferum} contaminant A series, where the
#' per-chain rate runs 9 to 31 per cent, the released rule drew 148,397
#' proposals over the fourteen equations of the declining set at five seeds
#' against 237 for the change. Accepted chains are kept and only the empty slots
#' are re-drawn. See #309.
#'
#' The set of accepted values is unchanged by this, exactly for a fresh
#' proposal: the four draws are independent, so conditioning on the other three
#' having passed does not change the law of the first. Once
#' \code{\link{refine_inits}} is included it is unchanged up to the weight of
#' the fourth power of the per-chain rate, because the released loop did not
#' refine the set it drew before entering the loop and this one refines from the
#' first round. That weight is between 4e-5 and 9e-3 on the designs measured.
#'
#' \strong{The cap stays at 1e4.} It is now exactly \code{n_trials} rounds
#' where the released loop allowed one more --- it drew the first set before the
#' loop and then tested \code{n_t <= n_trials} from \code{n_t = 1} --- so the
#' released bound was \code{n_trials + 1} sets.
#'
#' #266 objects that this ran 561 seconds for a
#' single model with no output, and proposes a smaller cap on the grounds that
#' the outcome after exhausting it -- Stan's own random initialisation -- is
#' available at the first attempt. Measured before changing it, and that
#' reasoning does not hold: a search that succeeds is not equivalent to one that
#' stops early. On a twenty-row, four-dose dataset \code{nec4param} needed 250
#' attempts to succeed at one seed and more than 1000 at two others, while
#' \code{nec3param} on the same data never succeeded at all. A cap of 1e3 would
#' therefore have turned working fits into random initialisation, silently, on
#' exactly the small designs where good initial values matter most. Accepting
#' chains individually removes the need to approach the cap rather than
#' lowering it.
#'
#' \strong{A wall-clock bound was tried and removed.} It made the number of
#' attempts, and so the initial values, and so the fit, a function of machine
#' load: the search needs about 3.6 s idle on one packaged case and exceeded a
#' 10 s budget under a parallel test run on the same machine. A fit whose
#' starting values depend on what else is running is not reproducible, and #266
#' asked for time not to be wasted, not for results to change. What is fixed
#' instead is the actual complaint: a user watching a long search could not tell
#' it from a hang. It now says so while it runs.
#'
#' \strong{The fallback is all or nothing.} Where the cap is reached with any
#' slot still empty the whole fit is handed to Stan rather than half-primed.
#' There are no values for the empty slots to supply, and starting some chains
#' from the search and the rest from Stan's uniform draw would make the chains
#' of one fit incomparable during warmup.
#'
#' @seealso \code{\link{make_inits}}, \code{\link{init_limits}}
#' @return A \code{\link[base]{list}} containing the initialisation values.
#'
#' @noRd
make_good_inits <- function(model, x, y, family, n_trials = 1e4, seed = NULL,
                            report_after = 20, ...) {
  limits <- init_limits(x, y, zero_bounded = zero_bounded_family(family),
                        support = init_support(family))
  pred_fct <- get(paste0("pred_", model))
  fct_args <- names(unlist(as.list(args(pred_fct))))
  fct_args <- setdiff(fct_args, "x")
  dots <- list(...)
  priors_df <- blank_bounds_to_na(as.data.frame(dots$priors))
  priors_df <- priors_df[priors_df$prior != "", ]
  chains <- dots$chains
  x_sorted <- sort(x)
  # make_inits() takes the number of chains through the same `...` the caller
  # supplies, so it is overridden here rather than passed separately. Each
  # round draws only the slots still empty.
  draw <- function(n) {
    do.call(make_inits,
            c(list(model, fct_args), modifyList(dots, list(chains = n))))
  }
  passes <- function(inits) {
    vapply(inits, function(init) {
      check_init_predictions(
        get_init_predictions(init, x_sorted, pred_fct, fct_args), limits)
    }, logical(1))
  }
  set.seed(seed)
  accepted <- vector("list", chains)
  filled <- rep(FALSE, chains)
  started <- Sys.time()
  reported <- FALSE
  n_t <- 0
  while (any(!filled) && n_t < n_trials) {
    empty <- which(!filled)
    inits <- draw(length(empty))
    ok <- passes(inits)
    # refine_inits() re-draws one parameter at a time for a curve that is
    # finite but out of range. Applied only to the chains that failed: a chain
    # already accepted is finished, and re-running it would replace one good
    # draw with another for nothing.
    if (any(!ok)) {
      inits[!ok] <- lapply(inits[!ok], refine_inits, x_sorted, pred_fct,
                           fct_args, limits, priors_df)
      ok[!ok] <- passes(inits[!ok])
    }
    if (any(ok)) {
      accepted[empty[ok]] <- inits[ok]
      filled[empty[ok]] <- TRUE
    }
    n_t <- n_t + 1
    # Said once, the first time the search passes report_after seconds, so a
    # long search is distinguishable from a hang without a message per attempt.
    if (!reported &&
          as.numeric(Sys.time() - started, units = "secs") > report_after) {
      message("Still searching for initial values for the ", model,
              " model (", n_t, " of ", n_trials, " attempts so far, ",
              sum(filled), " of ", chains, " chains found). This can",
              " take a few minutes for a small or awkward design; the fit will",
              " proceed on Stan's default initialisation if it does not",
              " succeed.")
      reported <- TRUE
    }
  }
  if (any(!filled)) {
    elapsed <- as.numeric(Sys.time() - started, units = "secs")
    message("bayesnec failed to find initial values for all ", chains,
            " chains of the ", model, " model after ", n_t, " attempts and ",
            signif(elapsed, 2), " seconds; ", sum(filled), " were found.",
            " Using Stan's default initialisation process for the whole fit.",
            " This usually means the priors and the response do not overlap;",
            " get_priors() reports the priors in use.")
    list(random = "random")
  } else {
    accepted
  }
}

#' make_good_hurdle_inits
#'
#' Initial values for both parameter blocks of a joint hurdle fit.
#'
#' @inheritParams make_good_inits
#'
#' @param predictor A \code{\link[base]{numeric}} vector containing the full
#' predictor, including the rows where the response is zero.
#' @param response A \code{\link[base]{numeric}} vector; zero denotes a
#' non-survivor.
#' @param priors An object of class \code{\link[brms]{brmsprior}} covering both
#' blocks, i.e. containing both \code{top} and \code{hutop} and so on.
#' @param model_survival The equation used for the second block, which need not
#' be the one used for the response block. Defaults to \code{NULL}, i.e. the
#' same as \code{model}.
#'
#' @details Each block is primed from the view of the data it actually models,
#' then the two are merged chain-wise. The mu block sees survivors only; the hu
#' block sees the proportion surviving at each unique predictor value, because
#' the sub-model is written as \code{1 - survival} and so the curve being
#' initialised is survival.
#'
#' Both passes reuse the same \code{pred_<model>()} prediction function, which
#' expects unprefixed parameter names -- the \code{hu} prefix is stripped before
#' the search and restored afterwards.
#'
#' @seealso \code{\link{make_good_inits}}
#' @return A \code{\link[base]{list}} of initial values, or
#' \code{list(random = "random")} if either block could not be initialised.
#'
#' @importFrom brms bernoulli
#'
#' @noRd
make_good_hurdle_inits <- function(model, predictor, response, priors, chains,
                                   family, dpar = "hu", seed = NULL,
                                   model_survival = NULL, ...) {
  if (is.null(model_survival)) {
    model_survival <- model
  }
  parts <- split_hurdle_response(predictor, response)
  pr <- as.data.frame(priors)
  is_hu <- nzchar(pr$nlpar) & grepl(paste0("^", dpar), pr$nlpar)
  mu_pr <- pr[!is_hu, , drop = FALSE]
  hu_pr <- pr[is_hu, , drop = FALSE]
  hu_pr$nlpar <- sub(paste0("^", dpar), "", hu_pr$nlpar)
  # Each block is primed under the family its own priors are built from, which
  # is what define_hurdle_prior() uses: the non-zero part's family for the mu
  # block, and bernoulli with an identity link for the second, whose response is
  # the proportion surviving. Passing the joint family to either would give the
  # wrong support -- (0, Inf) for a block whose mean is a proportion.
  mu_inits <- make_good_inits(model, parts$mu$x, parts$mu$y,
                              family = hurdle_mu_family(family), priors = mu_pr,
                              chains = chains, seed = seed, ...)
  hu_inits <- make_good_inits(model_survival, parts$hu$x, parts$hu$y,
                              family = bernoulli(link = "identity"),
                              priors = hu_pr, chains = chains, seed = seed, ...)
  # If either block fell back to Stan's random initialisation there is nothing
  # coherent to merge -- hand the whole fit to Stan rather than half-priming it.
  fell_back <- function(x) length(x) == 1 && "random" %in% names(x)
  if (fell_back(mu_inits) || fell_back(hu_inits)) {
    return(list(random = "random"))
  }
  lapply(seq_len(chains), function(i) {
    hu_i <- hu_inits[[i]]
    names(hu_i) <- sub("^b_", paste0("b_", dpar), names(hu_i))
    c(mu_inits[[i]], hu_i)
  })
}

#' Initial values for the parameters a group-level term introduces
#'
#' @param brms_bf The \code{\link[brms]{brmsformula}} the fit will use.
#' @param data The \code{\link[base]{data.frame}} the fit will use.
#' @param family A \code{\link[stats]{family}} object.
#' @param priors The \code{\link[brms]{brmsprior}} the fit will use, read for
#' the scale of the generated group-level standard deviations.
#' @param group_spec The output of \code{\link{parse_group_terms}}, used only
#' to decide which deviation intercepts to start at zero. See
#' \code{\link{group_zero_intercepts}}.
#'
#' @details A prior is not enough here, and this is the part of #245 that is
#' easy to get wrong. Stan draws its default initial values as
#' \code{uniform(-2, 2)} on the \emph{unconstrained} scale, which it does
#' \strong{regardless of the prior declared}. A group-level standard deviation
#' is lower-bounded at zero, so the realised initial value is
#' \code{exp(uniform(-2, 2))}, between 0.135 and 7.39; the offset is
#' \code{sd * z} under the non-centred parameterisation \pkg{brms} uses, with
#' \code{z} initialised in the same range. On a response bounded in (0, 1)
#' under the identity link \code{\link{bnec}} assigns, that puts the mean
#' outside its support before sampling begins, and no prior can prevent it.
#'
#' The \code{ogl} intercept is the same problem in a simpler form: it is a
#' population-level parameter with no bounds, so Stan starts it anywhere in
#' (-2, 2), which is already outside a unit-interval response.
#'
#' All group-level effects are therefore started at \strong{exactly zero}
#' deviation -- \code{z = 0}, and the \code{ogl} offset at 0 -- which is a
#' valid point for any family and any link, and is the model the fit reduces to
#' if the grouping turns out to explain nothing.
#'
#' Because \code{z} is zero, the value given to \code{sd} itself does not
#' affect whether the starting point is valid; it sets only where the sampler
#' begins exploring. The median of the generated \code{sd} prior scales is
#' used, so the starting scale tracks the data rather than being a constant that
#' is tiny for one response and large for another.
#'
#' The indices are read from \code{\link[brms]{make_standata}} rather than
#' reconstructed from the formula. \pkg{brms} numbers group-level terms by its
#' own internal ordering -- a single \code{pgl} term over four parameters
#' becomes four separately indexed terms, not one -- and guessing that ordering
#' would be a silent source of mismatched initial values.
#'
#' The query is made twice where it has to be, because no single family can
#' answer it for every fit. See the comment at the call.
#'
#' @return A named \code{\link[base]{list}} of initial values.
#'
#' @importFrom brms make_standata
#' @importFrom stats median gaussian
#'
#' @noRd
group_inits <- function(brms_bf, data, family, priors, group_spec = NULL) {
  # The group-level dimensions M_k and N_k come from the random-effects
  # structure alone, so the family is irrelevant to the answer -- but it is not
  # irrelevant to whether the call succeeds, and each of the two candidates
  # fails on a case the other handles.
  #
  # The fit's own family fails where the response has not yet been through
  # check_data(): a Beta response still carrying exact zeros and ones, which is
  # what reaches here when the formula transforms a variable, or on the amend()
  # path where check_data() never runs at all. gaussian() fails where the
  # formula carries a trials() aterm, because trials is not a valid aterm for
  # gaussian -- that is every binomial and beta_binomial fit, which is the
  # standard workflow for those families and one of the three bounded families
  # this function exists for.
  #
  # So: the fit's own family first, gaussian() as the fallback. Ordered that
  # way round because the real family is the one that describes the model, and
  # a query answered by it needs no justification; gaussian() is the escape
  # hatch for a response the family will not accept yet, and can only be wrong
  # about something this function does not ask.
  ask <- function(fam) {
    try(suppressMessages(make_standata(brms_bf, data = data, family = fam)),
        silent = TRUE)
  }
  sdata <- ask(family)
  if (inherits(sdata, "try-error")) {
    sdata <- ask(gaussian())
  }
  if (inherits(sdata, "try-error")) {
    # Genuinely unexpected now. Warn rather than return quietly: an empty init
    # list here is the difference between a fit that starts and one that does
    # not, and a silent one is very hard to trace back to this line.
    warning("Could not determine the group-level dimensions, so no initial ",
            "values were generated for them. The fit may fail to initialise. ",
            "See #245.", call. = FALSE)
    return(list())
  }
  m_names <- grep("^M_[0-9]+$", names(sdata), value = TRUE)
  if (length(m_names) == 0) {
    return(list())
  }
  sd_scales <- sd_prior_scales(priors)
  start_sd <- if (length(sd_scales) > 0) median(sd_scales) else 0.1
  out <- list()
  for (k in sort(as.integer(sub("^M_", "", m_names)))) {
    n_terms <- sdata[[paste0("M_", k)]]
    n_levels <- sdata[[paste0("N_", k)]]
    out[[paste0("sd_", k)]] <- as.array(rep(start_sd, n_terms))
    out[[paste0("z_", k)]] <- matrix(0, nrow = n_terms, ncol = n_levels)
  }
  for (nm in group_zero_intercepts(group_spec, family)) {
    out[[paste0("b_", nm)]] <- as.array(0)
  }
  out
}

#' The scales of the group-level standard deviation priors in a prior set
#'
#' @param priors An object of class \code{\link[brms]{brmsprior}}.
#'
#' @return A \code{\link[base]{numeric}} vector, possibly empty.
#'
#' @noRd
sd_prior_scales <- function(priors) {
  if (is.null(priors) || nrow(priors) == 0 || !"class" %in% names(priors)) {
    return(numeric(0))
  }
  strs <- priors$prior[priors$class == "sd"]
  strs <- strs[nzchar(strs)]
  if (length(strs) == 0) {
    return(numeric(0))
  }
  # Only read a scale from the distributions whose last argument is one. The
  # last argument of gamma() and inv_gamma() is a rate, exponential() has a rate
  # and no comma at all, and constant() carries a value rather than a scale --
  # taking the last number from any of those returns a number that is not a
  # scale, silently. A user-supplied gamma(2, 100) previously gave a starting
  # value of 100 on a unit-interval response.
  scale_families <- "^\\s*(student_t|normal|cauchy|lognormal|logistic)\\s*\\("
  strs <- strs[grepl(scale_families, strs)]
  if (length(strs) == 0) {
    return(numeric(0))
  }
  vals <- suppressWarnings(
    as.numeric(sub("^.*,\\s*([0-9.eE+-]+)\\)\\s*$", "\\1", strs))
  )
  vals[is.finite(vals) & vals > 0]
}
