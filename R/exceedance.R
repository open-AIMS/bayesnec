#' Probability that a toxicity estimate exceeds a threshold
#'
#' Computes the posterior probability that a no-effect or effect-concentration
#' estimate lies above a threshold concentration, such as a guideline or
#' trigger value. Posterior draws that lie beyond the prediction range are
#' counted from the censoring record rather than deleted.
#'
#' @param object An object of class \code{\link{bayesnecfit}},
#' \code{\link{bayesmanecfit}}, \code{\link{bayesnechurdlefit}} or
#' \code{\link{bayesnecgroupfit}}.
#' @param threshold A single finite number, the concentration the estimate is
#' compared with. It is read on the scale the estimate is returned on; see
#' Details.
#' @param estimate The estimate to compare: \code{"nec"} (the default),
#' \code{"nsec"} or \code{"ecx"}, read by \code{\link{nec}}, \code{\link{nsec}}
#' or \code{\link{ecx}} respectively.
#' @param ecx_val The effect level of the ECx, as in \code{\link{ecx}}. Used
#' only where \code{estimate = "ecx"}, and refused for the other two.
#' @param xform A function applied to the estimate before it is compared with
#' \code{threshold}, as in \code{\link{nec}}.
#' @param ... Further arguments passed to \code{\link{nec}}, \code{\link{nsec}}
#' or \code{\link{ecx}}, such as \code{x_range}, \code{resolution},
#' \code{type}, \code{sig_val} or \code{extrapolate}, and \code{which} for a
#' \code{\link{bayesnechurdlefit}}. \code{posterior} is not accepted: the
#' full posterior is always read.
#'
#' @details The probability is the share of the posterior draws of the
#' estimate that lie above \code{threshold}. The draws are those the estimator
#' returns with \code{posterior = TRUE}, so the estimate compared is the one
#' \code{\link{nec}}, \code{\link{nsec}} or \code{\link{ecx}} reports for the
#' same object and arguments. For a \code{\link{bayesmanecfit}} that is the
#' model-averaged posterior, which under \code{"nec"} mixes NEC and NSEC draws
#' where the model set contains smooth equations. For a
#' \code{\link{bayesnechurdlefit}} it is the combined endpoint unless
#' \code{which} names a component; under \code{"nec"} that is the combined
#' no-effect concentration described in \code{\link{nec.bayesnechurdlefit}}.
#' For a \code{\link{bayesnecgroupfit}} each level is compared in turn.
#'
#' \code{threshold} and the estimate are compared on one scale, the scale the
#' estimator returns the estimate on. Without \code{xform} that is the
#' predictor scale the model was fitted on, so under
#' \code{crf(log(concentration))} the threshold is a log concentration; with
#' \code{xform = exp} it is a concentration. A decreasing \code{xform}
#' reverses the order of the scale. A draw then exceeds the threshold on the
#' transformed scale where it lies below the corresponding value on the fitted
#' scale, and the censoring record is remapped with the draws, so that a draw
#' beyond the top of the fitted range is counted at the foot of the
#' transformed one.
#'
#' @section Censored draws:
#' A draw beyond an end of the prediction range is not given a value. That
#' includes a threshold equation's sampled \code{nec} draw beyond the range,
#' which \code{\link{nec}} reports only as a bound. The censoring record states
#' such a draw as lying at or above the upper bound of the range, or at or
#' below its lower bound, and the draw is counted from that statement.
#'
#' Inside the prediction range the record decides every draw. A draw censored
#' above lies above any threshold below the upper bound, and a draw censored
#' below does not lie above any threshold at or above the lower bound. The
#' probability is then exact: \code{prob_lower} and \code{prob_upper} are equal
#' and \code{prob} is that value.
#'
#' Beyond an end the record does not decide the draws censored at that end,
#' because it does not say on which side of the threshold they lie. For a
#' threshold at or above the upper bound, \code{prob_lower} counts every draw
#' censored above as not exceeding it and \code{prob_upper} counts each as
#' exceeding it, so the two are the share of identified draws above the
#' threshold and that share plus the share censored above. A threshold below
#' the lower bound is treated in the same way with the draws censored below.
#' \code{prob} is \code{NA} in both cases, because the posterior does not
#' identify a single probability. Widening the prediction range decides more of
#' those draws, at the price of reading the curve where there are no data:
#' \code{x_range} widens it for \code{\link{ecx}} and \code{\link{nsec}}, and
#' \code{extrapolate} for \code{\link{nec}} and \code{\link{nsec}}.
#'
#' A threshold equal to the upper bound is also reported as an interval. A
#' sampled \code{nec} draw is marked censored where it is at or above the
#' bound, so a censored draw may lie on the bound itself, and a draw equal to
#' the threshold does not exceed it. A threshold equal to the lower bound is
#' decided, because a draw at or below it does not exceed it. Where the bound
#' is itself back-transformed, as with \code{xform = exp} on a fit to
#' \code{crf(log(concentration))}, it can differ in the last digits from the
#' concentration it was computed from: \code{exp(log(100))} is slightly above
#' 100. A threshold given as that concentration then falls just inside or just
#' outside the range, and the rounding decides whether its probability is
#' exact or reported as an interval.
#'
#' A draw that is \code{NA} without the record accounting for it could not be
#' computed at all, and is left out of both probabilities and of
#' \code{n_draws}. An object whose no-effect posterior carries no censoring
#' record, as one built by an earlier version of the package does, has every
#' \code{nec} draw compared by its value.
#'
#' The comparison is strict: a draw equal to \code{threshold} does not exceed
#' it. The result is a probability only, and is not converted to an evidence
#' ratio.
#'
#' @return A \code{\link[base]{data.frame}} with one row, and for a
#' \code{\link{bayesnecgroupfit}} one row per level preceded by a column
#' \code{level}. The columns are:
#' \describe{
#'   \item{\code{threshold}}{The threshold supplied.}
#'   \item{\code{prob}}{The posterior probability that the estimate exceeds
#'     \code{threshold}, or \code{NA} where the censoring record leaves it
#'     between \code{prob_lower} and \code{prob_upper}.}
#'   \item{\code{prob_lower}, \code{prob_upper}}{The lowest and highest values
#'     of that probability consistent with the censoring record. They are equal
#'     wherever the record decides every draw.}
#'   \item{\code{n_above}, \code{n_below}}{The number of draws censored above
#'     the upper bound and below the lower bound of the prediction range, on
#'     the scale the threshold is read on.}
#'   \item{\code{n_draws}}{The number of draws the probabilities are taken
#'     over.}
#' }
#'
#' @seealso \code{\link{nec}}, \code{\link{nsec}}, \code{\link{ecx}},
#' \code{\link{compare_estimates}}
#'
#' @examples
#' library(bayesnec)
#' data(manec_example)
#' exceedance(manec_example, threshold = 1.5)
#' \donttest{
#' exceedance(manec_example, threshold = 1.5, estimate = "ecx", ecx_val = 50)
#' }
#'
#' @export
exceedance <- function(object, threshold, estimate = c("nec", "nsec", "ecx"),
                       ecx_val = 10, xform = identity, ...) {
  UseMethod("exceedance")
}

#' @inheritParams exceedance
#'
#' @noRd
#'
#' @export
exceedance.bnecfit <- function(object, threshold,
                               estimate = c("nec", "nsec", "ecx"),
                               ecx_val = 10, xform = identity, ...) {
  # One method for bayesnecfit, bayesmanecfit and bayesnechurdlefit rather
  # than one each: the estimator generics already dispatch on those classes,
  # and reading the posterior through them is what guarantees the draws and
  # the record compared here are the ones nec(), nsec() and ecx() report for
  # the same call.
  estimate <- match.arg(estimate)
  check_exceedance_args(threshold, estimate, ecx_val, !missing(ecx_val),
                        list(...))
  exceedance_fit(object, threshold, estimate, ecx_val,
                 if (missing(xform)) NULL else xform, ...)
}

#' @inheritParams exceedance
#'
#' @noRd
#'
#' @export
exceedance.bayesnecgroupfit <- function(object, threshold,
                                        estimate = c("nec", "nsec", "ecx"),
                                        ecx_val = 10, xform = identity, ...) {
  estimate <- match.arg(estimate)
  # Validated once, before the first level, so that an argument error is
  # raised before any level's curve is evaluated.
  check_exceedance_args(threshold, estimate, ecx_val, !missing(ecx_val),
                        list(...))
  xform <- if (missing(xform)) NULL else xform
  # Each level is an ordinary fit, so the per-level comparison is a map over
  # them, as group_lapply() makes it for the estimators themselves.
  rows <- group_lapply(object, exceedance_fit, threshold = threshold,
                       estimate = estimate, ecx_val = ecx_val, xform = xform,
                       ...)
  out <- do.call(rbind, lapply(seq_along(rows), function(i) {
    cbind(data.frame(level = object$levels[i], stringsAsFactors = FALSE),
          rows[[i]])
  }))
  rownames(out) <- NULL
  out
}

#' Validate the arguments of exceedance()
#'
#' @param threshold,estimate,ecx_val As in \code{exceedance()}.
#' @param ecx_val_given A \code{\link[base]{logical}} value, whether the
#' caller supplied \code{ecx_val}.
#' @param dots The \code{...} of the calling method, as a list.
#'
#' @return Invisibly \code{TRUE}, or an error.
#'
#' @importFrom chk chk_number
#'
#' @noRd
check_exceedance_args <- function(threshold, estimate, ecx_val, ecx_val_given,
                                  dots) {
  chk_number(threshold)
  if (!is.finite(threshold)) {
    stop("threshold must be a finite number.", call. = FALSE)
  }
  if (estimate == "ecx") {
    chk_number(ecx_val)
  } else if (ecx_val_given) {
    # Refused rather than ignored: a caller who names an effect level and
    # leaves estimate at its default would otherwise receive the probability
    # for a no-effect estimate under a call that reads as an ECx one.
    stop("ecx_val applies only to estimate = \"ecx\"; this call compares ",
         "estimate = \"", estimate, "\".", call. = FALSE)
  }
  if ("posterior" %in% names(dots)) {
    stop("exceedance() always reads the full posterior of the estimate, so ",
         "posterior is not an argument to it.", call. = FALSE)
  }
  invisible(TRUE)
}

#' The exceedance probability for one fit
#'
#' @param object A \code{\link{bayesnecfit}}, \code{\link{bayesmanecfit}} or
#' \code{\link{bayesnechurdlefit}}.
#' @param threshold,estimate,ecx_val As in \code{exceedance()}.
#' @param xform A function, or \code{NULL} where the caller supplied none.
#' @param ... Passed to the estimator.
#'
#' @return A one-row \code{\link[base]{data.frame}}; see \code{exceedance()}.
#'
#' @noRd
exceedance_fit <- function(object, threshold, estimate, ecx_val, xform, ...) {
  read_posterior <- switch(
    estimate,
    nec = function(...) nec(object, posterior = TRUE, ...),
    nsec = function(...) nsec(object, posterior = TRUE, ...),
    ecx = function(...) ecx(object, ecx_val = ecx_val, posterior = TRUE, ...)
  )
  # xform is forwarded only where the caller supplied one. Otherwise the
  # estimator's own default applies, and the estimator receives the call a
  # user would have made to it directly. No estimator currently distinguishes
  # a supplied identity from its default. #299 proposes a message for a
  # transformed predictor with no xform, and if that tests whether xform was
  # supplied, forwarding identity by name would suppress it for every call
  # made from here.
  post <- if (is.null(xform)) {
    read_posterior(...)
  } else {
    read_posterior(xform = xform, ...)
  }
  exceedance_from_draws(post, threshold)
}

#' The probability that posterior draws exceed a threshold, with the bounds the
#' censoring record places on it
#'
#' @param post A posterior vector, optionally carrying attribute
#' \code{"censored"}.
#' @param threshold A \code{\link[base]{numeric}} value on the scale of
#' \code{post}.
#'
#' @return A one-row \code{\link[base]{data.frame}}; see \code{exceedance()}.
#'
#' @noRd
exceedance_from_draws <- function(post, threshold) {
  # The limits compare_estimates() reads a draw between, so that a draw beyond
  # the prediction range is treated here exactly as it is in a comparison of
  # two fits and as the estimator reports it: an identified draw is its own
  # value, a draw censored above lies in [upper, Inf), and one censored below
  # in (-Inf, lower]. Nothing is subset, because every draw enters and none is
  # paired, so the record is read whole and subset_draws() is not needed.
  lims <- draw_limits(post)
  kept <- !lims$missing
  # A draw is known to exceed the threshold where even its lowest possible
  # value does, and known not to where even its highest possible value does
  # not. Every other draw is one whose side of the threshold the record leaves
  # open, and it is counted as not exceeding for the lower probability and as
  # exceeding for the upper one. Inside the prediction range no draw is left
  # open; beyond an end the draws censored at that end are, which gives the
  # interval of D25. The limits are closed at the bound, as the record states
  # them ("at or above"), so a threshold equal to the upper bound leaves the
  # draws censored above open rather than counting them as exceeding it.
  exceeds <- lims$lo[kept] > threshold
  may_exceed <- lims$hi[kept] > threshold
  n_draws <- sum(kept)
  if (n_draws == 0) {
    prob_lower <- NA_real_
    prob_upper <- NA_real_
  } else {
    prob_lower <- mean(exceeds)
    prob_upper <- mean(may_exceed)
  }
  # NA wherever the two differ, as prob is in compare_estimates(): any single
  # number would be one end of the interval presented as the whole of it. The
  # two means are over the same draws and differ only where some draw is left
  # open, so an exact comparison is the right test.
  prob <- if (isTRUE(prob_lower == prob_upper)) prob_lower else NA_real_
  cens <- attr(post, "censored")
  n_above <- if (is.null(cens)) 0L else sum(cens$above)
  n_below <- if (is.null(cens)) 0L else sum(cens$below)
  data.frame(threshold = threshold, prob = prob, prob_lower = prob_lower,
             prob_upper = prob_upper, n_above = n_above, n_below = n_below,
             n_draws = n_draws)
}
