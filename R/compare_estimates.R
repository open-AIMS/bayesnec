#' compare_estimates
#'
#' Extracts posterior predicted values from a list of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}} model fits and
#' compares these via bootstrap re sampling.
#'
#' @inheritParams compare_posterior
#' 
#' @importFrom chk chk_numeric chk_number
#'
#' @seealso \code{\link{bnec}}
#'
#' @return A named \code{\link[base]{list}} containing bootstrapped differences
#' in posterior predictions of the \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} model fits contained in \code{x}, with elements:
#' \describe{
#'   \item{\code{posterior_list}}{The posterior of the estimate for each fit,
#'     as the estimator returns it with \code{posterior = TRUE}, including the
#'     \code{"censored"} attribute that records which draws lie beyond the
#'     prediction range.}
#'   \item{\code{posterior_data}}{The paired draws of each posterior, in long
#'     format, with columns \code{model} and \code{value}. It carries no
#'     censoring mark; see the \emph{Censored estimates} section.}
#'   \item{\code{diff_list}}{The paired differences for each pair of fits,
#'     named \code{"a-b"} for the first fit minus the second. A difference
#'     involving a censored draw has no value and is \code{NA}.}
#'   \item{\code{diff_data}}{The same differences in long format, with columns
#'     \code{comparison} and \code{diff}, and two columns that mark each
#'     difference: \code{positive}, \code{TRUE} where the difference is known
#'     to be above zero, \code{FALSE} where it is known to be at or below zero,
#'     and \code{NA} where it is not known; and \code{indeterminate},
#'     \code{TRUE} where censoring leaves the sign unknown.}
#'   \item{\code{prob_diff}}{One row per pair of fits, with columns
#'     \code{comparison}; \code{prob}, the probability that the difference is
#'     above zero; \code{prob_lower} and \code{prob_upper}, the lowest and
#'     highest values of that probability consistent with the censoring
#'     record; and \code{censored_first} and \code{censored_second}, the
#'     fraction of each fit's paired draws that lie beyond the prediction
#'     range. See the \emph{Censored estimates} section.}
#' }
#'
#' @section Censored estimates:
#' In the differences and the probabilities, a draw that lies beyond the
#' prediction range is treated as the estimator reports it: it is given no
#' value, and it is known only to lie at or beyond the end of the range. This
#' applies to every comparison type. An ECx or NSEC draw whose curve does not
#' reach the target is such a draw, and so is a threshold equation's
#' \code{nec} draw above the prediction range, which is sampled and has a
#' number but is reported by \code{\link{nec}} as a bound. Under "nec" and
#' "n(s)ec", \code{posterior_list} and \code{posterior_data} still hold that
#' sampled value, as \code{\link{nec}} returns it with \code{posterior = TRUE}.
#' The \code{"censored"} attribute of \code{posterior_list} marks the draw and
#' \code{posterior_data} does not, so a value there beyond the prediction range
#' is not an identified estimate.
#'
#' The comparison does not delete these draws. For many pairs the censoring
#' record fixes the sign of the difference whatever the censored value is: a
#' draw at or above the upper end of the range, paired with a draw identified
#' below it, gives a positive difference. The sign is unknown in three cases:
#' where both draws are censored at the same end; where an identified draw
#' lies at or above the upper bound of a draw censored above it, or below the
#' lower bound of a draw censored below it; and where a draw censored above
#' one bound is paired with a draw censored below another and the two
#' prediction ranges do not overlap. The second case arises from an identified
#' draw exactly at the bound, since the record states "at or above", or from
#' components predicted over different ranges, as does the third. Such a pair
#' is indeterminate: it is counted as not positive in \code{prob_lower} and as
#' positive in \code{prob_upper}, and it is marked in the \code{indeterminate}
#' column of \code{diff_data}. Where no pair is indeterminate the two
#' probabilities are equal, and \code{prob} is that value. Where any pair is
#' indeterminate \code{prob} is \code{NA}, because the comparison does not
#' identify a single probability. \code{censored_first} and
#' \code{censored_second} state how much of each posterior was censored.
#'
#' A draw that is \code{NA} without the censoring record accounting for it
#' could not be computed at all, and a pair containing one is left out of both
#' probabilities. So is a pair of identified draws whose difference is not a
#' number, such as two infinite draws of the same sign.
#'
#' @importFrom dplyr bind_rows arrange
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect everything
#' @importFrom utils combn
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' data(manec_example)
#' nec4param <- pull_out(manec_example, model = "nec4param")
#' ecx4param <- pull_out(manec_example, model = "ecx4param")
#' compare_estimates(list("nec" = ecx4param, "ecx" = nec4param), ecx_val = 50,
#' comparison="ecx")
#' }
#'
#' @section Reproducibility:
#' The draws of each posterior are paired by a random permutation, so
#' \code{prob_diff} and the difference intervals are a Monte Carlo
#' approximation to the difference of two \bold{independent} posteriors, using
#' \emph{n} of the \emph{n}^2 available pairs. The permutation is drawn under
#' \code{seed}, so two calls on the same fits return the same comparison and a
#' \code{\link[base]{set.seed}} in the session has no effect on it. A different
#' \code{seed} gives another realisation of the same approximation;
#' where the approximation matters, compare a few and report the spread. The
#' caller's random number state is restored afterwards.
#'
#' @section The independence assumption:
#' The pairing is valid only where the posteriors being compared come from
#' \bold{separate fits}. Two levels of one fit share draws --- draw \emph{i}
#' of each comes from the same sweep of the sampler --- and permuting them
#' destroys that pairing, which discards the correlation between the levels and
#' widens the difference posterior. \code{prob_diff} is then pulled toward 0.5
#' and a real difference is under-detected, which is the wrong direction to err
#' in. A within-fit contrast needs draw-wise differencing and must not be routed
#' through this function. See #218 and #33.
#'
#' @export
compare_estimates <- function(x, comparison = "n(s)ec", ecx_val = 10,
                              type = "absolute",
                              sig_val = 0.01, resolution = 100, x_range = NA,
                              seed = 10) {
  if ((comparison %in% c("nec", "n(s)ec", "ecx", "nsec")) == FALSE) {
    stop("comparison must be one of nec, n(s)ec, ecx or nsec.")
  }
  chk_numeric(ecx_val)
  # The same validator ecx() uses, rather than a second copy of the vocabulary.
  # The copy that stood here still listed the 2.1.3 three-value set, so it
  # refused type = "range" -- which is the name the rename warning gives users
  # for the behaviour they had, making the migration it names impossible from
  # here and from compare_posterior(), which forwards to this function.
  type <- validate_ecx_type(type, match.call())
  # Warned once for the call rather than once per fit in x: the ecx() calls
  # below name type explicitly, so each would otherwise repeat the message.
  # Same reasoning as ecx.bayesmanecfit. See D15 ruling 8.
  warned <- options(bayesnec.relative_warned = TRUE)
  on.exit(options(warned), add = TRUE)
  chk_numeric(sig_val)
  chk_numeric(resolution)
  chk_number(seed)
  if (is.na(x_range[1])) {
    x_range <- return_x_range(x)
  } else {
    chk_numeric(x_range)    
  }
  if (comparison == "nec") {
    posterior_list <- lapply(x, nec, posterior = TRUE, xform = identity)
  }
  if (comparison == "n(s)ec") {
    posterior_list <- lapply(x, return_nec_post, xform = identity)
  }
  if (comparison == "ecx") {
    posterior_list <- lapply(x, ecx, ecx_val = ecx_val, resolution = resolution,
                             posterior = TRUE, type = type,
                             x_range = x_range)
  }
  if (comparison == "nsec") {
    posterior_list <- lapply(x, nsec, sig_val = sig_val, resolution = resolution,
                             posterior = TRUE,
                             x_range = x_range)
  }
  names(posterior_list) <- names(x)
  n_samples <- min(sapply(posterior_list, length))
  # Random pairing affects the reported estimates. Seed it locally so calls
  # repeat without changing the caller's RNG state (#343).
  with_preserved_rng_state({
    set.seed(seed, sample.kind = "Rejection")
    r_posterior_list <- lapply(posterior_list, function(m, n_samples) {
      # A random subset of a longer posterior, not its first n_samples draws.
      # sample(seq_len(n_samples)) permuted only the head of the vector, so
      # where components had unequal draw counts the tail of the longer one was
      # never used -- systematic rather than random thinning. Harmless when the
      # counts are equal, which is the normal case. See #218.
      # subset_draws() rather than `[`, which drops the censoring record, after
      # which a draw beyond the prediction range cannot be told from one that
      # is merely missing (#404).
      subset_draws(m, sample(seq_along(m), n_samples, replace = FALSE))
    }, n_samples = n_samples)
  })
  posterior_data <- do.call("cbind", r_posterior_list) |>
    data.frame() |>
    pivot_longer(cols = everything(), names_to = "model") |>
    arrange(.data$model) |>
    data.frame()
  all_combn <- combn(names(x), 2, simplify = FALSE)
  pair_signs <- lapply(all_combn, function(a, r_list) {
    difference_sign(r_list[[a[1]]], r_list[[a[2]]])
  }, r_list = r_posterior_list)
  diff_list <- lapply(pair_signs, `[[`, "diff")
  names(diff_list) <- sapply(all_combn, function(m) paste0(m[1], "-", m[2]))
  diff_data_out <- bind_rows(diff_list, .id = "comparison") |>
    pivot_longer(everything(), names_to = "comparison", values_to = "diff") |>
    data.frame()
  # pivot_longer() above lays the draws out draw by draw, every comparison for
  # the first draw and then every comparison for the second, so the marks are
  # laid out the same way: one column per comparison, read across rows.
  diff_data_out$positive <- as.vector(t(
    do.call("cbind", lapply(pair_signs, `[[`, "positive"))
  ))
  diff_data_out$indeterminate <- as.vector(t(
    do.call("cbind", lapply(pair_signs, `[[`, "indeterminate"))
  ))
  # Indexed by position rather than by the "a-b" label, which two different
  # pairs of names can share.
  prob_diff <- lapply(seq_along(all_combn), function(i, r_list) {
    a <- all_combn[[i]]
    s <- pair_signs[[i]]
    # The sign is read from the censoring record, per D22, rather than from the
    # differences that have a value. A draw beyond the prediction range has no
    # value, but it is known to lie beyond the bound, and against a draw on the
    # near side of that bound the sign of the difference is fixed whatever the
    # censored value is. Only a pair whose sign the record leaves open is
    # indeterminate, and it is counted as not positive for the lower
    # probability and as positive for the upper one, rather than deleted. The
    # earlier na.rm deleted every pair with a censored draw and reported the
    # probability over the rest as though it were the whole comparison.
    #
    # A pair that is missing without being censored -- an NA the record does
    # not account for -- could not be computed at all, and is left out of both
    # probabilities, as summarise_censored() leaves it out of a quantile.
    sign01 <- as.numeric(s$positive)
    prob_lower <- mean(replace(sign01, s$indeterminate, 0), na.rm = TRUE)
    prob_upper <- mean(replace(sign01, s$indeterminate, 1), na.rm = TRUE)
    # prob is kept, and computed by the expression that computed it before, so
    # that a comparison with no indeterminate pair returns the number it
    # returned before to the last bit. Where the two probabilities differ the
    # comparison does not identify a single one, and NA says so; any number
    # here would be one end of the interval presented as the whole of it.
    prob <- if (any(s$indeterminate)) {
      NA_real_
    } else {
      mean(sign01, na.rm = TRUE)
    }
    data.frame(prob = prob, prob_lower = prob_lower, prob_upper = prob_upper,
               censored_first = censored_fraction(r_list[[a[1]]]),
               censored_second = censored_fraction(r_list[[a[2]]]))
  }, r_list = r_posterior_list)
  names(prob_diff) <- names(diff_list)
  prob_diff_out <- bind_rows(prob_diff, .id = "comparison") |>
    data.frame()
  list(posterior_list = posterior_list, posterior_data = posterior_data,
       diff_list = diff_list, diff_data = diff_data_out,
       prob_diff = prob_diff_out)
}

#' The values a posterior draw is known to lie between
#'
#' A draw the censoring record marks beyond an end has no value, or a value
#' that the estimator itself reports only as a bound: a threshold equation's
#' \code{nec} draw above the prediction range is sampled and has a number, and
#' \code{\link{nec}} still reports it as lying at or above the bound. Both are
#' read here as the interval the record places them in, so that every
#' comparison type treats a draw beyond the range as the estimator that
#' produced it reports it (#404).
#'
#' @param m A posterior vector, optionally carrying attribute
#' \code{"censored"}.
#'
#' @return A \code{\link[base]{list}} with elements \code{lo} and \code{hi},
#' the limits of each draw, \code{censored}, and \code{missing}, which marks a
#' draw that is \code{NA} without the record accounting for it.
#' @noRd
draw_limits <- function(m) {
  value <- as.numeric(m)
  lo <- value
  hi <- value
  censored <- logical(length(value))
  cens <- attr(m, "censored")
  if (!is.null(cens)) {
    # "At or above" and "at or below", which is how warn_censored_draws()
    # states the record, so the limits are closed at the bound.
    lo[cens$above] <- cens$upper
    hi[cens$above] <- Inf
    lo[cens$below] <- -Inf
    hi[cens$below] <- cens$lower
    censored <- cens$above | cens$below
  }
  list(lo = lo, hi = hi, censored = censored,
       missing = is.na(value) & !censored)
}

#' The paired differences of two posteriors, with the sign each is known to
#' have
#'
#' @param first,second Posterior vectors of equal length, paired by position,
#' each optionally carrying attribute \code{"censored"}.
#'
#' @return A \code{\link[base]{list}}: \code{diff}, the difference where both
#' draws have a value and neither is censored, and \code{NA} otherwise;
#' \code{positive}, \code{TRUE} where the difference is known to be above zero,
#' \code{FALSE} where it is known to be at or below zero, and \code{NA} where
#' neither is known; and \code{indeterminate}, \code{TRUE} where the sign is
#' unknown because of censoring rather than because a draw is missing or the
#' difference of two identified draws is not a number.
#' @noRd
difference_sign <- function(first, second) {
  a <- draw_limits(first)
  b <- draw_limits(second)
  # The subtraction the function has always made, so that where nothing is
  # censored the differences are the ones it has always returned. Arithmetic
  # copies the record from the operands, and a difference is not on the
  # scale that record describes, so it is removed.
  diff <- first - second
  attr(diff, "censored") <- NULL
  diff[a$censored | b$censored] <- NA
  smallest <- a$lo - b$hi
  largest <- a$hi - b$lo
  positive <- rep(NA, length(smallest))
  # Above zero is the event prob has always counted, and at or below zero its
  # complement, so a difference known to be exactly zero is not positive.
  positive[which(smallest > 0)] <- TRUE
  positive[which(largest <= 0)] <- FALSE
  # Two identified draws whose difference is not a number -- Inf against Inf
  # -- have no sign. The earlier na.rm dropped such a pair, and it is dropped
  # here too rather than counted as indeterminate, so that a comparison with
  # nothing censored returns what it returned before. A NaN limit on a pair
  # with a censored draw is a sign the record leaves open, and stays
  # indeterminate.
  undefined <- !a$censored & !b$censored & is.nan(smallest)
  missing <- a$missing | b$missing | undefined
  positive[missing] <- NA
  list(diff = diff, positive = positive,
       indeterminate = is.na(positive) & !missing)
}

#' The share of a posterior's draws that lie beyond the prediction range
#'
#' Over the draws the record accounts for, which is the denominator
#' \code{summarise_censored()} states a censored fraction over.
#'
#' @param m A posterior vector, optionally carrying attribute
#' \code{"censored"}.
#'
#' @return A \code{\link[base]{numeric}} value.
#' @noRd
censored_fraction <- function(m) {
  lims <- draw_limits(m)
  sum(lims$censored) / sum(!lims$missing)
}
