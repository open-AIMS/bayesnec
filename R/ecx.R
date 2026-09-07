#' Extracts the predicted ECx value
#' 
#' Extracts the predicted ECx value as desired from an object of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesnecfit}}.
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} returned by \code{\link{bnec}}.
#' @param ecx_val The desired percentage effect value, defaults to 10. Any
#' value above 0 is accepted for every \code{type} except "direct", where it
#' is a response value rather than a percentage. Values above 100 are
#' meaningful under "absolute" for a response that can go negative --- see
#' Details.
#' @param type A \code{\link[base]{character}} vector, taking values of
#' "absolute" (the default), "relative", "range" or "direct". See Details.
#' @param resolution The number of unique x values over which to find ECx --
#' large values will make the ECx estimate more precise.
#' @param posterior A \code{\link[base]{logical}} value indicating if the full
#' posterior sample of calculated ECx values should be returned instead of
#' just the median and 95 credible intervals.
#' @param xform A function to apply to the returned estimated concentration
#' values.
#' @param x_range A range of x values over which to consider extracting ECx.
#' @param prob_vals A vector indicating the probability values over which to
#' return the estimated ECx value. Defaults to 0.5 (median) and 0.025 and
#' 0.975 (95 percent credible intervals).
#' @param dpar For a joint two-block fit only (\code{family = "hurdle_gamma"}
#' or \code{"zero_inflated_beta"}), the parameter block to report:
#' \code{"mu"} for the response block, or \code{"hu"} (\code{"zi"} for the
#' zero-inflated families) for survival. Defaults to \code{NULL}, which gives
#' the combined endpoint \code{mu * (1 - hu)}. The zero-probability block is
#' inverted to survival before computing, so ECx keeps its usual meaning of a
#' percentage decline from the fitted control value. See Details.
#' @param ... Additional arguments passed to methods.
#'
#' @details \bold{Every ECx is measured from the control.} The control is the
#' predicted mean at the lowest concentration in the supplied predictor, taken
#' per posterior draw. \code{type} names what the percentage is measured
#' \emph{towards}:
#'
#' \itemize{
#'   \item \code{"absolute"} (the default) --- control to 0.
#'   \item \code{"relative"} --- control to the equation's theoretical
#'     asymptote, the \code{bot} parameter where the equation has one and 0
#'     otherwise. Refused where the equation has no \code{bot} and the family
#'     is unbounded below, because there is then no finite denominator.
#'   \item \code{"range"} --- control to the lowest response the curve
#'     predicts over the predictor range.
#'   \item \code{"direct"} --- \code{ecx_val} is a response value rather than
#'     a percentage, and the x value at which the curve reaches it is returned.
#' }
#'
#' \code{"range"} is what \code{"relative"} computed in versions up to
#' 2.1.3, except that the span was taken from the maximum of the predicted
#' curve rather than from the control. The two differ only for a curve whose
#' maximum is not at the lowest concentration, which in practice means a
#' hormetic one.
#'
#' Measuring from the control rather than from the maximum of the predicted
#' curve is what makes an ECx well defined for a hormesis equation: the target
#' response lies below the control, the rising limb lies above it, so the
#' curve crosses the target exactly once. The \code{hormesis_def} argument
#' selected between those two references and has been removed; the control is
#' now always the reference.
#'
#' \code{"absolute"} measures towards 0 even for a family whose support is
#' unbounded below. This follows OECD TG 201, whose percent inhibition
#' rearranges to the same reference and which permits a value above 100 per
#' cent rather than truncating it, so \code{ecx_val} is not capped at 100.
#'
#' Where the curve does not reach the target anywhere in the predictor range
#' the ECx is not identified and \code{NA} is returned, with a warning
#' reporting how many draws were affected.
#' 
#' Calls to functions \code{\link{ecx}} and \code{\link{nsec}} and
#' \code{\link{compare_fitted}} do not require the same level of flexibility
#' in the context of allowing argument \code{newdata}
#' (from a \code{\link[brms]{posterior_predict}} perspective) to
#' be supplied manually, as this is and should be handled within the function
#' itself. The argument \code{resolution} controls how precisely the
#' \code{\link{ecx}} or \code{\link{nsec}} value is estimated, with 
#' argument \code{x_range} allowing estimation beyond the existing range of
#' the observed data (otherwise the default range) which can be useful in a
#' small number of cases. There is also no reasonable case where estimating
#' these from the raw data would be of value, because both functions would
#' simply return one of the treatment concentrations, making NOEC a better
#' metric in that case.
#'
#' \bold{Selecting a component of a hurdle model}
#'
#' The two implementations of a hurdle model name the component differently,
#' and the two arguments are not interchangeable. A
#' \code{\link{bayesnechurdlefit}} from \code{\link{bnec_hurdle}} holds two
#' separate fits, so it takes \code{which = "growth"}, \code{"survival"} or
#' \code{"combined"}. A joint fit from \code{bnec(family = "hurdle_gamma")}
#' holds two parameter blocks inside one model, so it takes \code{dpar} naming
#' the \pkg{brms} distributional parameter. Supplying one where the other is
#' expected is an error rather than silently ignored.
#'
#' @seealso \code{\link{bnec}}, \code{\link{bnec_hurdle}}, \code{\link{nsec}}
#'
#' @return A vector containing the estimated ECx value, including upper and
#' lower 95% credible interval bounds.
#'
#' @examples
#' \donttest{
#' library(brms)
#' library(bayesnec)
#' data(manec_example)
#' ecx(manec_example, ecx_val = 50)
#' ecx(manec_example)
#' }
#'
#' @export
# dpar sits after `...` to match the methods, which all declare their
# class-specific arguments there. Naming it on the generic is what puts it in
# the \usage section; documented-but-absent arguments are an R CMD check
# WARNING, and methods are @noRd so the generic is the only place it can appear.
ecx <- function(object, ecx_val = 10, resolution = 1000,
                posterior = FALSE, type = "absolute", x_range = NA,
                xform = identity, prob_vals = c(0.5, 0.025, 0.975), ...,
                dpar = NULL) {
  UseMethod("ecx")
}

#' @inheritParams ecx
#'
#' @inherit ecx details return seealso examples
#'
#' @param object An object of class \code{\link{bayesnecfit}} returned by
#' \code{\link{bnec}}.
#' 
#' @importFrom stats quantile
#' @importFrom brms posterior_epred
#' @importFrom chk chk_logical chk_numeric
#'
#' @noRd
#'
#' @export
ecx.bayesnecfit <- function(object, ecx_val = 10, resolution = 1000,
                            posterior = FALSE, type = "absolute",
                            x_range = NA, xform = identity,
                            prob_vals = c(0.5, 0.025, 0.975), ...,
                            dpar = NULL) {
  check_component_arg(list(...), object)
  check_removed_args(list(...))
  chk_numeric(ecx_val)
  if (length(ecx_val)>1) {
    stop("You may only pass one ecx_val")  
  }
  chk_numeric(resolution)  
  chk_logical(posterior)
  type <- validate_ecx_type(type, match.call())
  if (!inherits(xform, "function")) {
    stop("xform must be a function.")
  }
  if (length(prob_vals) < 3 || prob_vals[1] < prob_vals[2] ||
      prob_vals[1] > prob_vals[3] || prob_vals[2] > prob_vals[3]) {
    stop("prob_vals must include central, lower and upper quantiles,",
         " in that order")
  }
  # No upper cap. Under "absolute" the reference is 0, so an ecx_val above 100
  # names a target below zero -- 120% inhibition of a growth rate is a rate of
  # -0.2 times the control, which is a real measurement on an unbounded
  # response and is what OECD TG 201 reports rather than truncating at 100.
  # Unreachable targets are not an error either: the curve simply never crosses
  # them and the draw returns NA with the warning below. See D15 ruling 7.
  if (type != "direct" && ecx_val <= 0) {
    stop("Supplied ecx_val is not in the required range. ",
         "Please supply a percentage value greater than 0.", call. = FALSE)
  }
  # The refusal that stood here rejected an absolute ECx for a gaussian
  # response fitted without a bot parameter, on the grounds that the curve
  # cannot reach 0. It is removed with #206: absolute measures towards 0
  # deliberately, following OECD TG 201, and a gaussian mean function
  # asymptoting to zero is internally consistent -- the likelihood evaluates
  # y - mu and never tests the sign of y. See D15 ruling 7.
  newdata_list <- newdata_eval(
    object, resolution = resolution, x_range = x_range
  )
  # dpar lets a two-block fit report its components separately. The default
  # (NULL) gives what posterior_epred always gave: mu * (1 - hu) for such a
  # family, the single mean curve otherwise. The zero-probability block is
  # inverted so that "decline from control" means the same thing as it does
  # everywhere else. Valid names are "mu" and whichever brms uses for the
  # second block: "hu" for hurdle families, "zi" for zero-inflated ones.
  # Written as a closure so the two-block handling reaches the control
  # prediction as well as the curve. Reading the control off a matrix that had
  # had the dpar inversion applied, or not applied, inconsistently with the
  # curve would put the reference and the curve on different quantities.
  epred_fun <- function(nd) {
    if (is.null(dpar)) {
      return(posterior_epred(object, newdata = nd, re_formula = NA))
    }
    if (!is_hurdle_family(object$fit$family)) {
      stop("The \"dpar\" argument is only valid for hurdle families.",
           call. = FALSE)
    }
    dpar <- match.arg(dpar, c("mu", hurdle_dpar(object$fit$family)))
    out <- posterior_epred(object, newdata = nd, re_formula = NA, dpar = dpar)
    if (dpar != "mu") {
      out <- 1 - out
    }
    out
  }
  p_samples <- epred_fun(newdata_list$newdata)
  x_vec <- newdata_list$x_vec
  control <- control_posterior(object, newdata_list$newdata, epred_fun)
  asymptote <- ecx_asymptote(object, type)
  ecx_out <- ecx_from_posterior(p_samples, x_vec, ecx_val, type, control,
                                asymptote)
  n_missing <- sum(is.na(ecx_out))
  ecx_out <- sub_x_transformation(ecx_out, object$bayesnecformula)
  bound <- sub_x_transformation(max(x_vec), object$bayesnecformula)
  # xform is applied to the censoring bound as well as to the estimates, and
  # the warning is raised after both, so that the bound and the numbers the
  # caller is about to read are on one scale. Reporting the bound before xform
  # named the fitted scale beside estimates in concentrations -- on
  # vignette("example1")'s xform = function(x) exp(x) - 1, a bound of 4.6 beside
  # an estimate of 99.
  if (inherits(xform, "function")) {
    ecx_out <- xform(ecx_out)
    bound <- xform(bound)
  }
  if (n_missing > 0) {
    warning("The ", object$model, " curve does not reach the ", type,
            " ECx", ecx_val, " target anywhere in the predictor range for ",
            n_missing, " of ", length(ecx_out), " draws, which return NA. ",
            "The estimate is censored above ", signif(bound, 3), ".",
            call. = FALSE)
  }

  ecx_estimate <- quantile(unlist(ecx_out), probs = prob_vals, na.rm = TRUE)
  names(ecx_estimate) <- clean_names(ecx_estimate)
  attr(ecx_estimate, "resolution") <- resolution
  attr(ecx_out, "resolution") <- resolution
  attr(ecx_estimate, "ecx_val") <- ecx_val
  attr(ecx_out, "ecx_val") <- ecx_val
  attr(ecx_estimate, "toxicity_estimate") <- "ecx"
  attr(ecx_out, "toxicity_estimate") <-  "ecx"
  # Both advisory messages compare estimates, so neither can be evaluated when
  # every draw returned NA -- signif(NA) == signif(NA) is NA, not FALSE, and
  # if() on it is an error. An all-NA estimate has already been reported by the
  # warning above, which says the same thing more precisely.
  if (all(is.na(ecx_estimate))) {
    NULL
  } else if (signif(ecx_estimate[1], 3) == signif(ecx_estimate[3], 3)) {
    message("The estimated mean is identical or nearly identical to your",
            " upper credible interval for the ", object$model, " model.",
            " This suggests the ecx estimate lies beyond the upper bound of",
            " your x_range and should be reported as greater than, and used",
            " as a censored value. You could try increasing x_range, although",
            " extrapolation beyond the data range should be done with",
            " caution.")
  } else if (signif(ecx_estimate[3], 3) == signif(max(x_vec), 3)) {
    message("The estimated upper credible interval is identical or nearly",
            " identical to the upper bound of your x_range value for the ",
            object$model, " model. This suggests the estimated uncertainty",
            "may be constrained. You could try increasing x_range to ensure",
            " this is not the case.")
  }
  if (!posterior) {
    ecx_estimate
  } else {
    ecx_out
  }
}

#' @inheritParams ecx
#'
#' @param object An object of class \code{\link{bayesmanecfit}} returned by
#' \code{\link{bnec}}.
#'
#' @inherit ecx details return seealso examples
#'
#' @importFrom stats quantile
#' @importFrom chk chk_logical chk_numeric
#'
#' @noRd
#'
#' @export
ecx.bayesmanecfit <- function(object, ecx_val = 10, resolution = 1000,
                              posterior = FALSE, type = "absolute",
                              x_range = NA, xform = identity,
                              prob_vals = c(0.5, 0.025, 0.975), ...,
                              dpar = NULL) {
  check_component_arg(list(...), object)
  check_removed_args(list(...))
  chk_numeric(ecx_val)
  chk_numeric(resolution)  
  chk_logical(posterior)
  if (length(ecx_val)>1) {
    stop("You may only pass one ecx_val")  
  }
  type <- validate_ecx_type(type, match.call())
  if (!inherits(xform, "function")) { 
    stop("xform must be a function.")}   
  if (length(prob_vals) < 3 || prob_vals[1] < prob_vals[2] ||
      prob_vals[1] > prob_vals[3] || prob_vals[2] > prob_vals[3]) {
    stop("prob_vals must include central, lower and upper quantiles,",
         " in that order")
  }
  # The rename warning belongs to the call, not to each member of the set.
  # sample_ecx() below calls ecx() once per equation with type passed
  # explicitly, so without this a model-averaged ecx(type = "relative") warned
  # once for the set and once more for every equation in it. See D15 ruling 8.
  warned <- options(bayesnec.relative_warned = TRUE)
  on.exit(options(warned), add = TRUE)
  sample_size <- object$sample_size
  # The same weighted index every other quantity on this object uses, rather
  # than a fresh unseeded sample() here. Without it a model-averaged ECx was a
  # different number on every call -- and it is the lower bound, the end a
  # protective concentration is read off, that moved most. Which draws are kept
  # is unchanged in kind; only the redraw is gone. See #216.
  draw_index <- pull_draw_index(object, names(object$mod_fits), sample_size)
  # Written as a closure over the arguments rather than a function taking them
  # all positionally: the previous form dispatched through
  # sapply(to_iter, sample_ecx, object, ecx_val, ...), which matched by
  # position, and any argument not named in that list -- dpar among them -- was
  # dropped before reaching the per-model call. That returned the combined
  # endpoint for a two-block fit with no error, which is a wrong answer rather
  # than a missing feature.
  sample_ecx <- function(x) {
    mod <- names(object$mod_fits)[x]
    target <- suppressMessages(pull_out(object, model = mod))
    out <- ecx(target, ecx_val = ecx_val, resolution = resolution,
               posterior = TRUE, type = type,
               x_range = x_range, xform = xform, prob_vals = prob_vals,
               dpar = dpar)
    out[draw_index[[mod]]]
  }
  to_iter <- seq_len(length(object$success_models))
  ecx_out <- unlist(lapply(to_iter, sample_ecx))
  ecx_estimate <- quantile(ecx_out, probs = prob_vals, na.rm = TRUE)
  names(ecx_estimate) <- clean_names(ecx_estimate)
  attr(ecx_estimate, "resolution") <- resolution
  attr(ecx_out, "resolution") <- resolution
  attr(ecx_estimate, "ecx_val") <- ecx_val
  attr(ecx_out, "ecx_val") <- ecx_val
  attr(ecx_estimate, "toxicity_estimate") <- "ecx"
  attr(ecx_out, "toxicity_estimate") <-  "ecx"
  if (!posterior) {
    ecx_estimate
  } else {
    ecx_out
  }
}

#' Validate the type argument, and warn on the relative rename
#'
#' \code{"relative"} named the control-to-minimum span up to 2.1.3 and names
#' the control-to-asymptote span from 2.2.0. The two are different quantities,
#' so a caller who wrote it explicitly is told once, by name, what to write for
#' the old behaviour. Warned rather than errored: the new meaning is a valid
#' request and the majority of callers will want it. See D15 ruling 8.
#'
#' A plain warning rather than \code{lifecycle::deprecate_warn()}, to avoid
#' adding a dependency for one message.
#'
#' @noRd
validate_ecx_type <- function(type, mc) {
  valid <- c("absolute", "relative", "range", "direct")
  if (length(type) != 1 || !is.character(type) || !(type %in% valid)) {
    stop("type must be one of 'absolute' (the default), 'relative', 'range' ",
         "or 'direct'. Please see ?ecx for more details.", call. = FALSE)
  }
  if (identical(type, "relative") && "type" %in% names(mc) &&
      !isTRUE(getOption("bayesnec.relative_warned"))) {
    warning("type = \"relative\" now measures from the control to the ",
            "equation's theoretical asymptote (the bot parameter, or 0 where ",
            "the equation has none). Up to bayesnec 2.1.3 it measured from ",
            "the maximum of the predicted curve to its minimum, which is now ",
            "type = \"range\". The two differ for a hormetic curve and ",
            "wherever the equation has a bot parameter.", call. = FALSE)
  }
  type
}

#' The theoretical asymptote each draw's curve declines towards
#'
#' Used by \code{type = "relative"} only, and \code{NA} for every other type.
#' An equation with a \code{bot} parameter declines towards it; the fourteen
#' equations without one decline towards 0, which is their theoretical minimum
#' and is a finite bound under any family bounded below. Under a family that is
#' not, there is no bound and no denominator, so the request is refused rather
#' than answered with a number that means nothing. See D15 ruling 6.
#'
#' A \code{\link{bayesmanecfit}} has no single \code{fit} to read \code{bot}
#' from, and the quantity it needs is the model-averaged asymptote, because the
#' curve it is the denominator for is the model-averaged curve. Assembled by
#' \code{manec_asymptote()}. Without that branch, \code{ecnsec()} on a model set
#' with \code{type = "relative"} failed inside \pkg{posterior} on a \code{NULL}
#' \code{fit}, with a message naming neither the argument nor the class.
#'
#' @importFrom brms as_draws_df
#' @noRd
ecx_asymptote <- function(object, type) {
  if (!identical(type, "relative")) {
    return(NA_real_)
  }
  if (inherits(object, "bayesmanecfit")) {
    return(manec_asymptote(object))
  }
  bot_draws <- as_draws_df(object$fit)[["b_bot_Intercept"]]
  if (!is.null(bot_draws)) {
    return(as.numeric(bot_draws))
  }
  if (!family_has_lower_bound(object$fit$family)) {
    stop("type = \"relative\" needs a finite asymptote to measure towards. ",
         "The ", object$model, " equation has no bot parameter, so it ",
         "declines towards 0, and the ", object$fit$family$family,
         " family is not bounded below, so 0 is not a bound. Use ",
         "type = \"absolute\" to measure towards 0 regardless, or ",
         "type = \"range\" to measure towards the lowest predicted response.",
         call. = FALSE)
  }
  0
}

#' The model-averaged theoretical asymptote
#'
#' Each equation's own asymptote --- its \code{bot} draws, or 0 where it has no
#' \code{bot} --- thinned to the draws the weighting keeps and stacked in the
#' order the model-averaged posterior stacks them.
#'
#' The stacking must match \code{posterior_epred.bayesmanecfit()} exactly, or
#' element \emph{i} of the asymptote belongs to a different (equation, draw)
#' pair than row \emph{i} of the curve it is the denominator for, and the
#' resulting ECNSEC is a ratio of two unrelated quantities. Both use
#' \code{names(mod_fits)} for the order and \code{pull_draw_index()} for the
#' draws, which is what makes the pairing correct rather than coincidental.
#' See #216.
#'
#' An equation that refuses \code{"relative"} --- no \code{bot} under a family
#' unbounded below --- refuses here too, so the model set fails exactly where
#' \code{ecx(type = "relative")} on the same set fails.
#'
#' @param object An object of class \code{\link{bayesmanecfit}}.
#'
#' @return A \code{\link[base]{numeric}} vector, one asymptote per draw of the
#' model-averaged posterior.
#'
#' @importFrom brms as_draws_df
#' @noRd
manec_asymptote <- function(object) {
  model_set <- names(object$mod_fits)
  sample_size <- min(vapply(object$mod_fits,
                            function(x) nrow(as_draws_df(x$fit)), numeric(1)))
  draw_index <- pull_draw_index(object, model_set, sample_size)
  unlist(lapply(model_set, function(m) {
    part <- suppressMessages(pull_out(object, model = m))
    a <- ecx_asymptote(part, "relative")
    idx <- draw_index[[m]]
    if (length(a) == 1) rep_len(a, length(idx)) else a[idx]
  }))
}

#' The x value at which each draw reaches its ECx target
#'
#' The reference is the control -- this draw's predicted mean at the lowest
#' observed concentration -- for every type. See D15.
#'
#' @noRd
ecx_from_posterior <- function(p_samples, x_vec, ecx_val, type, control,
                               asymptote) {
  n_draws <- nrow(p_samples)
  control <- rep_len(control, n_draws)
  asymptote <- rep_len(asymptote, n_draws)
  vapply(seq_len(n_draws), function(i) {
    y <- p_samples[i, ]
    target <- switch(
      type,
      absolute = control[i] * (1 - ecx_val / 100),
      relative = control[i] - (control[i] - asymptote[i]) * (ecx_val / 100),
      range = control[i] -
        (control[i] - min(y, na.rm = TRUE)) * (ecx_val / 100),
      direct = ecx_val
    )
    crossing_x(y, target, x_vec)
  }, numeric(1))
}
