#' Treat an empty-string prior bound as absent
#'
#' \pkg{brms} records an absent bound as \code{""} in a fitted object's
#' \code{prior} slot, where \code{define_prior()} and \code{brms::prior()} use
#' \code{NA}. \code{make_inits()} tests bounds with \code{is.na()}, so a
#' \code{""} left in place becomes \code{NA} and stops the bound-respecting
#' redraw with "missing value where TRUE/FALSE needed".
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
#' A \code{constant()} prior is a point mass, so it has no generator in
#' \code{prior_samplers()}, and code that draws from a prior must test for it
#' first. These two helpers are the one place the form is parsed.
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
#' The value is the first argument, so \code{constant(0.5, broadcast = FALSE)}
#' reads as 0.5, and it may be an R expression such as \code{constant(1/2)}.
#' The expression is evaluated in \code{\link[base]{baseenv}}, so it sees base
#' R and nothing of the caller's workspace.
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
  # Try a literal first. A non-literal is handled below, so the coercion
  # warning is suppressed.
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
#' @details The one list of generators, used by the initial-value search and
#' by \code{\link{sample_priors}}. \code{prior_sampler()} refuses a
#' distribution missing from it. \code{lognormal} is included because the
#' predictor-scaled parameters are positive and span orders of magnitude on a
#' dilution series.
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
  # Search only the curve's own coefficients, class "b". A parameter of any
  # other class (a dispersion, zi or hu, a group-level sd) would fail the name
  # check below; Stan initialises it, and its prior still reaches brm().
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
      # Assign a constant() prior's value and skip the redraw below, which
      # would never end for a constant outside its own bounds. Keep the value
      # in the list: make_good_inits() evaluates the curve with it.
      # add_brm_defaults() removes it, because Stan declares no such parameter.
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
  # NaN marks a failure no redraw fixes, such as an equation that raises a
  # negative predictor to a fractional power. Inf is left to the search below,
  # since a smaller parameter can cure an overflow.
  if (any(is.nan(preds))) {
    return(init)
  }
  tunable <- intersect(names(init), c("b_slope", "b_d", "b_beta"))
  for (par in tunable) {
    pr_row <- which(priors$nlpar == gsub("^b_", "", par))
    if (length(pr_row) != 1) next
    # A constant() prior is fixed, and prior_sampler() has no entry for it.
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
#' @details \code{\link{mu_support}} bounds the mean, but the search evaluates
#' \code{pred_<model>()}, which returns the linear predictor, against a
#' response on the link scale. Under a non-identity link the support is
#' therefore mapped through the link: \code{log} takes (0, 1) to (-Inf, 0) and
#' (0, Inf) to the whole line, and \code{logit} takes (0, 1) to the whole line.
#' Any other link leaves the band unbounded.
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
#' @details A group mean estimates the mean response at that concentration,
#' the quantity an initial curve returns there. Only replicated values are
#' returned: an unreplicated "mean" is a single observation, the kind of
#' extremum \code{\link{init_limits}} is built to avoid.
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
#' @details For a curve that is smooth between neighbouring concentrations,
#' \code{sd(diff(y)) / sqrt(2)} estimates the noise standard deviation. The
#' curve between points biases it upward, the safe direction, and unlike
#' \code{sd(y)} it does not grow with the size of the effect.
#'
#' It is read over each half of the series and the larger taken, because the
#' dispersion often changes along the predictor. A half needs at least three
#' differences; otherwise the whole series is used.
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
#' @details The spread \code{\link{init_limits}} widens its band by: the
#' pooled within-group standard deviation, the variation of the response about
#' the mean at its own concentration. \code{sd(y)} is not used, because it
#' grows with the size of the effect. Where the replicates give fewer than
#' three degrees of freedom the spread is read from successive differences
#' instead; see \code{\link{successive_difference_spread}}.
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
  # Choose on degrees of freedom, not on whether any value is replicated: one
  # duplicated pair would otherwise set the spread for the whole series.
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
#' lie strictly between these two values. The band spans every mean response
#' the design estimates, widened by \code{width} spreads: the means of the
#' replicated predictor values, and the two \code{\link{regularizing_location}}
#' anchors, which are the means at each end of the series. It is not
#' \code{range(y)}: an extremum is not on the footing of a mean, sits near the
#' centre of the \code{top} prior it would filter, and drifts outward as
#' replicates are added.
#'
#' The width of four is the smallest that covered the asymptotes of the
#' generating curve in every in-scope cell of a simulation over three
#' families. \code{notes/scripts/init_search_audit.R} reproduces it.
#'
#' The band is clamped to \code{support}, because under the identity link a
#' curve outside the support of the mean is invalid and Stan fails with
#' "Initialization failed". Where the band reaches the boundary it stops short;
#' see \code{\link{boundary_inset}}.
#'
#' Where the series stops short of the lower asymptote, every level mean lies
#' above the true \code{bot} and no width reaches it. \code{floor_value}
#' extends the band down to the value \code{\link{define_prior}} spans the
#' \code{bot} prior from under \code{asymptote_observed = FALSE}.
#'
#' @param floor_value The lowest value the mean can take, or \code{NA_real_}
#' where the band is not to be extended. Applied through \code{min()}, so it
#' widens the band and never narrows it, and is then clamped to \code{support}
#' with everything else.
#'
#' @return A \code{\link[base]{numeric}} vector of length 2, the lower and
#' upper bound in that order.
#'
#' @noRd
init_limits <- function(x, y, width = 4, zero_bounded = FALSE,
                        support = c(-Inf, Inf),
                        spread_fn = group_spread,
                        floor_value = NA_real_) {
  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]
  y <- y[keep]
  if (length(y) == 0) {
    # Return NA, which rejects every draw. Not c(Inf, -Inf):
    # check_init_predictions() reorders the band with min() and max(), and
    # would read that as the whole line.
    return(c(NA_real_, NA_real_))
  }
  # The zero-bounded branch of regularizing_location() refuses a response with
  # no positive value, with a message about priors. Take it only where some
  # value is positive.
  zero_bounded <- zero_bounded && any(y > 0)
  centres <- c(regularizing_location(x, y, "top", zero_bounded)[["location"]],
               regularizing_location(x, y, "bot", zero_bounded)[["location"]],
               replicated_group_means(x, y))
  centres <- centres[is.finite(centres)]
  spread <- width * spread_fn(x, y)
  # Use the extreme level means, not the two anchors: a response that is not
  # monotone at its ends has its extreme mean in the interior. With no centre,
  # fall back to range(y), since min() and max() warn on an empty vector. A
  # constant response then gives a band of zero width, which rejects every
  # draw and leaves the fit to Stan's initialisation.
  out <- if (length(centres) == 0) {
    range(y, na.rm = TRUE)
  } else {
    c(min(centres) - spread, max(centres) + spread)
  }
  if (!all(is.finite(out)) || out[1] > out[2]) {
    out <- range(y, na.rm = TRUE)
  }
  # Under asymptote_observed = FALSE the band must admit draws down to the
  # floor. Use min(), not assignment: the floor only ever widens the band, and
  # on a gaussian response the band already reaches below it.
  if (is.finite(floor_value)) {
    out[1] <- min(out[1], floor_value)
  }
  if (!all(is.finite(out))) {
    # Unreachable on a finite response. NA, not c(-Inf, Inf), as above.
    return(c(NA_real_, NA_real_))
  }
  out <- c(max(out[1], support[1]), min(out[2], support[2]))
  # Stop short of a support boundary the band reaches; see boundary_inset().
  out[1] <- boundary_inset(out[1], support[1], centres, spread / width, y,
                           "lower")
  out[2] <- boundary_inset(out[2], support[2], centres, spread / width, y,
                           "upper")
  out
}

#' Stop a band short of the boundary of the support
#'
#' @param edge The band's end after clamping to the support.
#' @param bound The support boundary on that side.
#' @param centres The level means the band is built from.
#' @param spread One standard deviation from \code{\link{group_spread}}.
#' @param y The response on the link scale, read only to cap the inset.
#' @param side One of \code{"lower"} or \code{"upper"}.
#' @param fraction The share of the smaller of the two distances below at which
#' the band stops short of the boundary.
#'
#' @details A mean at the boundary of the support is one the likelihood cannot
#' evaluate, and the clauses of \code{\link{check_init_predictions}} are
#' strict inequalities, so a band that ends at the boundary admits a curve
#' arbitrarily close to it. On the survival block of a hurdle fit, primed from
#' proportions spanning (0, 1), such a curve made the log likelihood
#' \code{-Inf} and the fit fail to initialise.
#'
#' The gap is \code{fraction} of the smaller of two quantities: the distance
#' from the boundary to the nearest level mean inside it, and one
#' \code{\link{group_spread}}. The distance alone gives the largest gap to the
#' design furthest from the boundary; the spread alone can exclude the level
#' means of a block primed from proportions. Level means are used rather than
#' observations, because an extremum drifts with sample size and is 1 for any
#' count response.
#'
#' The gap never exceeds the distance to the closest observation strictly
#' inside the boundary, so the band is never stricter there than
#' \code{range(y)} would be. That cap lets one observation just inside the
#' boundary shrink the gap to almost nothing.
#'
#' The inset guards against a curve collapsing onto the boundary, not against
#' a small value. It is disabled where a level mean sits at the boundary, as
#' in a count design whose highest concentration is entirely zero, because the
#' band must contain every level the design measured.
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
  # Cap the gap at the closest observation inside the boundary.
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
#' seconds after which the search says it is still running. It does not end
#' the search; see Details.
#' @param n_trials A \code{\link[base]{numeric}} vector indicating
#' how many attempts the function should run before giving up.
#' @param asymptote_observed A \code{\link[base]{logical}}. \code{FALSE}
#' extends the band down to \code{\link{asymptote_floor}}, which is where
#' \code{\link{define_prior}} places the bottom of the \code{bot} prior under
#' the same declaration.
#' @param seed A \code{\link[base]{numeric}} vector of length 1, \code{NULL},
#' or \code{NA}, which is how \pkg{brms} writes "no seed". Without a seed the
#' search draws from the caller's stream, so a \code{\link[base]{set.seed}}
#' before the call fixes the initial values; with one it seeds itself.
#' @param family A \code{\link[stats]{family}} object, read for the support of
#' the curve and for whether the response-scaled parameters are bounded below
#' at zero. It has no default on purpose: \code{NULL} would leave the band
#' unbounded and admit invalid starting values without any error.
#' @param ... Additional arguments to \code{\link{make_inits}}.
#'
#' @details Each chain is accepted on its own, and each round redraws only the
#' chains still empty. Failed proposals are refined and tested again,
#' including in the first round.
#'
#' The search seeds itself only where \code{seed} is given:
#' \code{set.seed(NULL)} re-initialises the stream from the clock, which would
#' discard a \code{set.seed()} the user ran. The stream is not restored
#' afterwards. Fitting is meant to advance it, since the Stan seed is drawn
#' from it next, but by an amount a seed makes repeatable.
#'
#' The only bound is \code{n_trials} rounds. Small designs can need over a
#' thousand, so a lower cap would turn working fits into random
#' initialisation. A time limit would make the initial values depend on
#' machine load, so the search instead reports when it passes
#' \code{report_after} seconds.
#'
#' The fallback is all or nothing: if any chain is still empty at the cap, the
#' whole fit is handed to Stan, so that no fit starts some chains from the
#' search and others from Stan's draw.
#'
#' @seealso \code{\link{make_inits}}, \code{\link{init_limits}}
#' @return A \code{\link[base]{list}} containing the initialisation values.
#'
#' @noRd
make_good_inits <- function(model, x, y, family, n_trials = 1e4, seed = NULL,
                            report_after = 20, asymptote_observed = TRUE,
                            ...) {
  # NA where asymptote_observed is TRUE, or where no floor can be derived. The
  # second case is refused earlier, by define_prior() and bnec(); a direct
  # caller then gets the band without a floor rather than an error.
  init_floor <- if (asymptote_observed) {
    NA_real_
  } else {
    asymptote_floor(family, y)
  }
  limits <- init_limits(x, y, zero_bounded = zero_bounded_family(family),
                        support = init_support(family),
                        floor_value = init_floor)
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
  # Hold a constant equation to the support, not the band:
  # check_init_predictions() requires a declining curve with more than three
  # distinct values, which a constant never is. A draw strictly inside the
  # support is a level the likelihood can evaluate.
  constant <- model %in% constant_equations()
  support <- init_support(family)
  passes <- function(inits) {
    vapply(inits, function(init) {
      preds <- get_init_predictions(init, x_sorted, pred_fct, fct_args)
      if (constant) {
        all(is.finite(preds)) && min(preds) > support[1] &&
          max(preds) < support[2]
      } else {
        check_init_predictions(preds, limits)
      }
    }, logical(1))
  }
  # Seed only where a seed was supplied; see Details. NA is brms's "no seed",
  # and set.seed(NA) is an error. Test on length 1, because
  # all(is.na(integer(0))) is TRUE, and refuse a longer seed, which set.seed()
  # would silently truncate to its first value.
  if (!is.null(seed) && !(length(seed) == 1 && is.na(seed))) {
    if (!is.numeric(seed) || length(seed) != 1) {
      stop("`seed` must be a numeric vector of length 1, NULL or NA.")
    }
    set.seed(seed)
  }
  accepted <- vector("list", chains)
  filled <- rep(FALSE, chains)
  started <- Sys.time()
  reported <- FALSE
  n_t <- 0
  while (any(!filled) && n_t < n_trials) {
    empty <- which(!filled)
    inits <- draw(length(empty))
    ok <- passes(inits)
    # Refine only the chains that failed.
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
    # Stan draws uniform(-2, 2) on the unconstrained scale. A nec bounded on
    # both sides then starts inside the tested series, but one bounded below
    # at zero starts in (0.135, 7.39) and an unbounded one in (-2, 2), whatever
    # the units of the predictor. The message names get_priors(); supplying
    # `init` is the remedy.
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
                                   model_survival = NULL,
                                   asymptote_observed = TRUE, ...) {
  if (is.null(model_survival)) {
    model_survival <- model
  }
  parts <- split_hurdle_response(predictor, response)
  pr <- as.data.frame(priors)
  is_hu <- nzchar(pr$nlpar) & grepl(paste0("^", dpar), pr$nlpar)
  mu_pr <- pr[!is_hu, , drop = FALSE]
  hu_pr <- pr[is_hu, , drop = FALSE]
  hu_pr$nlpar <- sub(paste0("^", dpar), "", hu_pr$nlpar)
  # Prime each block under the family define_hurdle_prior() builds its priors
  # from: the non-zero part's family for mu, and bernoulli(link = "identity")
  # for survival. The joint family would give the wrong support. Both blocks
  # take asymptote_observed, as both get a bot prior from the floor.
  mu_inits <- make_good_inits(model, parts$mu$x, parts$mu$y,
                              family = hurdle_mu_family(family), priors = mu_pr,
                              chains = chains, seed = seed,
                              asymptote_observed = asymptote_observed, ...)
  hu_inits <- make_good_inits(model_survival, parts$hu$x, parts$hu$y,
                              family = bernoulli(link = "identity"),
                              priors = hu_pr, chains = chains, seed = seed,
                              asymptote_observed = asymptote_observed, ...)
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
#' @details Stan's default initial values are \code{uniform(-2, 2)} on the
#' unconstrained scale whatever the prior, so a group-level standard deviation
#' starts between 0.135 and 7.39 and the unbounded \code{ogl} intercept
#' anywhere in (-2, 2). On a response bounded in (0, 1) under the identity
#' link, that puts the mean outside its support before sampling begins. Every
#' group-level effect therefore starts at zero deviation (\code{z = 0}, and
#' the \code{ogl} offset at 0), which is valid for any family.
#'
#' With \code{z} at zero, \code{sd} only sets where exploration begins; it
#' takes the median of the \code{sd} prior scales.
#'
#' The indices are read from \code{\link[brms]{make_standata}}, because
#' \pkg{brms} numbers group-level terms in its own order: one \code{pgl} term
#' over four parameters becomes four terms.
#'
#' @return A named \code{\link[base]{list}} of initial values.
#'
#' @importFrom brms make_standata
#' @importFrom stats median gaussian
#'
#' @noRd
group_inits <- function(brms_bf, data, family, priors, group_spec = NULL) {
  # The family does not affect M_k and N_k, but it decides whether the call
  # succeeds. The fit's family fails on a Beta response not yet through
  # check_data(), with exact zeros or ones; gaussian() fails on a trials()
  # term. Try the fit's family first and gaussian() second.
  ask <- function(fam) {
    try(suppressMessages(make_standata(brms_bf, data = data, family = fam)),
        silent = TRUE)
  }
  sdata <- ask(family)
  if (inherits(sdata, "try-error")) {
    sdata <- ask(gaussian())
  }
  if (inherits(sdata, "try-error")) {
    # Warn rather than return quietly: without these values the fit may not
    # start, and the cause would be hard to trace.
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
  # Read a scale only from distributions whose last argument is one. In
  # gamma() and inv_gamma() it is a rate, exponential() has one argument, and
  # constant() holds a value.
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

#' Priors and initial values for the coefficients a level term introduces
#'
#' @param brm_args The list of \code{\link[brms]{brm}} arguments built by
#' \code{add_brm_defaults()}, with its priors and initial values already
#' derived for the model with no level term.
#' @param level_spec The level-term specification built by
#' \code{\link{bnec_joint}}.
#' @param family An object of class \code{\link[stats]{family}}.
#' @param response The response variable, on the response scale.
#'
#' @details The level-term counterpart of \code{\link{group_inits}}. Each level
#' coefficient starts at the value \code{make_good_inits()} found for that
#' parameter without a level term, replicated across the levels, so that the
#' levels start on the same footing whatever their sizes.
#'
#' The curve priors need no new rows: \pkg{brms} applies a class \code{"b"}
#' row with an \code{nlpar} and no \code{coef} to every coefficient of that
#' parameter, bounds included. The dispersion is the exception, because
#' \pkg{brms} models a predicted dispersion on the log scale; without a row its
#' level coefficients would take the improper flat default. See
#' \code{add_level_disp_defaults()}.
#'
#' Call this after the initial-value search. The dispersion row has a
#' \code{dpar} and no \code{nlpar}, and \code{make_inits()} would read it as a
#' curve coefficient named \code{b_} and refuse it.
#'
#' @return The modified \code{brm_args}.
#'
#' @importFrom brms prior_string
#'
#' @noRd
add_level_defaults <- function(brm_args, level_spec, family, response,
                               predictor = NULL,
                               prior_type = "uninformative",
                               predictor_scale = "auto",
                               init_supplied = FALSE,
                               prior_supplied = FALSE) {
  n_lev <- length(level_spec$levels)
  if (isTRUE(level_spec$composed)) {
    brm_args <- compose_level_defaults(brm_args, level_spec, family, predictor,
                                       response, prior_type, predictor_scale,
                                       init_supplied, prior_supplied)
    return(add_level_disp_defaults(brm_args, level_spec, family, response,
                                   n_lev))
  }
  # A character init is the "random" fallback: Stan initialises everything.
  if (is.list(brm_args$init)) {
    brm_args$init <- lapply(brm_args$init, function(chain) {
      for (p in level_spec$nlpars) {
        nm <- paste0("b_", p)
        # Length-tested rather than replicated unconditionally: a caller who
        # supplied `init` themselves bypasses the search entirely, and one who
        # already wrote a value per level meant them.
        if (nm %in% names(chain) && length(chain[[nm]]) != n_lev) {
          chain[[nm]] <- as.array(rep(chain[[nm]][[1]], n_lev))
        }
      }
      chain
    })
  }
  add_level_disp_defaults(brm_args, level_spec, family, response, n_lev)
}

#' The prior and initial value for a dispersion given a value per level
#'
#' Used by both branches of \code{\link{add_level_defaults}}: the dispersion
#' sub-model is \code{phi ~ 0 + <group_var>} whichever equation each level
#' takes.
#'
#' @inheritParams add_level_defaults
#' @param n_lev The number of levels.
#'
#' @return The modified \code{brm_args}.
#'
#' @importFrom brms prior_string
#'
#' @noRd
add_level_disp_defaults <- function(brm_args, level_spec, family, response,
                                    n_lev) {
  if (!isTRUE(level_spec$disp) || !has_disp_par(family)) {
    return(brm_args)
  }
  dpar <- disp_dpar(family)
  response_link <- response_link_scale(response, family)
  prior_df <- as.data.frame(brm_args$prior)
  supplied <- nrow(prior_df) > 0 &&
    any(prior_df$class == "b" & prior_df$dpar == dpar)
  if (!supplied) {
    # Keyed through disp_family_tag(), as disp_intercept_centre() is below,
    # so that the prior and the initial value name the same family.
    entry <- disp_intercept_priors(response_link)[[disp_family_tag(family)]]
    brm_args$prior <- brm_args$prior +
      prior_string(entry, class = "b", dpar = dpar)
  }
  if (is.list(brm_args$init)) {
    c0 <- disp_intercept_centre(family, response_link)
    brm_args$init <- lapply(brm_args$init, function(chain) {
      chain[[paste0("b_", dpar)]] <- as.array(rep(c0, n_lev))
      chain
    })
  }
  brm_args
}

#' Priors and initial values where each level has its own equation
#'
#' @inheritParams add_level_defaults
#' @param predictor The predictor, on the scale the curve is fitted on.
#' @param prior_type Passed to \code{\link{define_prior}}.
#' @param predictor_scale Passed to \code{\link{define_prior}}.
#' @param init_supplied Whether the caller supplied \code{init}, in which case
#' the search never ran and what is there is theirs.
#' @param prior_supplied Whether the caller supplied \code{prior}.
#'
#' @details The composed branch of a joint refit, where each level's
#' parameters are separate \pkg{brms} non-linear parameters and need their own
#' prior rows. Each level takes the priors of its own equation, derived once
#' per equation from the whole predictor and response rather than from the
#' level's subset, so that the contrast between levels is not partly a
#' contrast between their priors. Only the \code{nlpar} column is rewritten.
#'
#' Initial values follow the same rule, because Stan's default initialisation
#' ignores the prior; see \code{\link{group_inits}}. If any equation's search
#' falls back to \code{"random"}, Stan initialises everything, since
#' \pkg{brms} cannot use a per-chain list that names only some levels.
#'
#' @return The modified \code{brm_args}.
#'
#' @noRd
compose_level_defaults <- function(brm_args, level_spec, family, predictor,
                                   response, prior_type, predictor_scale,
                                   init_supplied, prior_supplied) {
  eqs <- unique(unlist(level_spec$models))
  per_eq <- lapply(eqs, function(m) {
    define_prior(m, family, predictor, response, prior_type = prior_type,
                 predictor_scale = predictor_scale)
  })
  names(per_eq) <- eqs
  if (!prior_supplied) {
    # A row with no nlpar is a statement about the response rather than about a
    # curve parameter -- the family's own dispersion -- so it is taken once
    # from the first equation rather than repeated per level.
    ref <- per_eq[[1]]
    out <- ref[!nzchar(ref$nlpar), ]
    for (l in level_spec$levels) {
      p <- per_eq[[level_spec$models[[l]]]]
      p <- p[nzchar(p$nlpar), ]
      p$nlpar <- paste0(p$nlpar, level_spec$tags[[l]])
      out <- out + p
    }
    brm_args$prior <- out
  }
  if (init_supplied) {
    return(brm_args)
  }
  response_link <- response_link_scale(response, family)
  chains <- if (is.null(brm_args$chains)) 4 else brm_args$chains
  seed <- if ("seed" %in% names(brm_args)) brm_args$seed else NULL
  per_eq_init <- lapply(eqs, function(m) {
    pr <- per_eq[[m]]
    pr <- pr[pr$class != "sd", ]
    pr <- pr[!pr$nlpar %in% generated_term_names(), ]
    suppressMessages(
      make_good_inits(m, predictor, response_link, family = family,
                      priors = pr, chains = chains, seed = seed)
    )
  })
  names(per_eq_init) <- eqs
  gave_up <- vapply(per_eq_init, function(x) {
    length(x) == 1 && "random" %in% names(x)
  }, logical(1))
  if (any(gave_up)) {
    message("The initial-value search gave up for ",
            paste0(eqs[gave_up], collapse = ", "),
            ", so Stan's own initialisation is used for every level. On a",
            " bounded response the fit may fail to initialise; see ?bnec for",
            " supplying `init` directly.")
    brm_args$init <- "random"
    return(brm_args)
  }
  brm_args$init <- lapply(seq_len(chains), function(i) {
    chain <- list()
    for (l in level_spec$levels) {
      src <- per_eq_init[[level_spec$models[[l]]]][[i]]
      names(src) <- paste0(names(src), level_spec$tags[[l]])
      chain <- c(chain, src)
    }
    chain
  })
  brm_args
}
