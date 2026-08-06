#' Extracts the predicted ECx value
#' 
#' Extracts the predicted ECx value as desired from an object of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesnecfit}}.
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} returned by \code{\link{bnec}}.
#' @param ecx_val The desired percentage effect value. This must be a value
#' between 1 and 99 (for type = "relative" and "absolute"), defaults to 10.
#' @param type A \code{\link[base]{character}} vector, taking values of
#' "relative", "absolute" (the default) or "direct". See Details.
#' @param resolution The number of unique x values over which to find ECx --
#' large values will make the ECx estimate more precise.
#' @param posterior A \code{\link[base]{logical}} value indicating if the full
#' posterior sample of calculated ECx values should be returned instead of
#' just the median and 95 credible intervals.
#' @param hormesis_def A \code{\link[base]{character}} vector, taking values
#' of "max" or "control". See Details.
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
#' @details \code{type} "relative" is calculated as the percentage decrease
#' from the maximum predicted value of the response (top) to the minimum
#' predicted value of the response. Type "absolute" (the default) is
#' calculated as the percentage decrease from the maximum value of the
#' response (top) to 0. Type "direct"
#' provides a direct estimate of the x value for a given y.
#' Note that for the current version, ECx for an "nechorme" (NEC Hormesis)
#' model is estimated at a percent decline from the control.
#' 
#' For \code{hormesis_def}, if "max", then ECx values are calculated as a
#' decline from the maximum estimates (i.e. the peak at NEC);
#' if "control", then ECx values are calculated relative to the control, which
#' is assumed to be the lowest observed concentration.
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
#' \bold{Do not normalise the response to the control first}
#'
#' Fit the raw, unnormalised response and take \code{type = "absolute"}. Do not
#' convert the response to percent inhibition, percent of control, or
#' percent of the observed maximum before calling \code{\link{bnec}}.
#'
#' The conventional pre-processing step \code{y0 = 1 - y / mean(y_control)}
#' divides every observation by the same random quantity, so the normalised
#' values are correlated and the uncertainty in the divisor is discarded.
#' By Jensen's inequality applied to \code{y -> 1/y} the inhibition trend is
#' biased downwards, and the resulting effective doses are biased upwards.
#' Ritz et al. (2026) report, for an ED10 with six control replicates, a bias
#' of 6.8% and a coefficient of variation of 26.4% under normalisation
#' against 2.1% and 12.7% for the same quantity estimated from the raw
#' response, with nominal 95% intervals covering at 90%. Roughly half the
#' reported variability is an artefact of the normalisation itself.
#'
#' Nothing is lost by not normalising. The concentration at which inhibition
#' increases by \code{x} percent is the same concentration at which the
#' response declines by \code{x} percent, and the latter is what
#' \code{type = "absolute"} returns: it is the decline relative to the
#' \emph{fitted} control value, Ritz et al.'s recommended estimand
#' \code{f(EDx) = (1 - x/100) * f(0)}. Because it is evaluated separately
#' within each posterior draw, uncertainty in the control level propagates
#' into the credible interval rather than being thrown away. The reference
#' within a draw is the maximum of the fitted curve, which is the fitted
#' control value for the monotonically declining models; for hormesis models
#' the curve peaks above the control, and \code{hormesis_def} selects which of
#' the two is meant.
#'
#' Dividing instead by the maximum observed response is worse on three counts:
#' an extreme order statistic is more variable than a mean of three to six
#' control values; the divisor then depends on every treatment rather than on
#' the controls alone; and it forces one observation to exactly 1, outside the
#' open support of the Beta family, so a boundary nudge is applied on top of
#' the other two distortions.
#'
#' Where a divisor is unavoidable -- the "Beta" and "zero_inflated_beta"
#' families need a response on (0, 1) -- it must be a constant fixed in
#' advance, such as a design ceiling or a value from accumulated historical
#' controls, and not a quantity computed from the dataset under analysis. The
#' problem is dividing by something random, not dividing as such. Note that
#' ECx is invariant to the choice of constant divisor, because it is a
#' relative decline from the fitted \code{top}: the divisor changes what
#' \code{top} means, not the toxicity estimate.
#'
#' @seealso \code{\link{bnec}}, \code{\link{bnec_hurdle}}, \code{\link{nsec}}
#'
#' @return A vector containing the estimated ECx value, including upper and
#' lower 95% credible interval bounds.
#'
#' @references
#' Ritz C, Gerhard D, Streibig JC (2026). Better alternatives than normalizing
#' to control: case studies with algae toxicity and dose-response analysis.
#' Environmental and Ecological Statistics, 33, 35-55.
#' doi:10.1007/s10651-025-00698-y.
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
                posterior = FALSE, type = "absolute",
                hormesis_def = "control", x_range = NA,
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
                            hormesis_def = "control", x_range = NA,
                            xform = identity,
                            prob_vals = c(0.5, 0.025, 0.975), ...,
                            dpar = NULL) {
  check_component_arg(list(...), object)
  chk_numeric(ecx_val)
  if (length(ecx_val)>1) {
    stop("You may only pass one ecx_val")  
  }
  chk_numeric(resolution)  
  chk_logical(posterior)
  if ((type %in% c("relative", "absolute", "direct")) == FALSE) {
    stop("type must be one of 'relative', 'absolute' (the default) or 'direct'. 
         Please see ?ecx for more details.")
  }
  if ((hormesis_def %in% c("max", "control")) == FALSE) {
    stop("type must be one of 'max' or 'control' (the default). 
         Please see ?ecx for more details.")
  }
  if (!inherits(xform, "function")) {
    stop("xform must be a function.")
  }
  if (length(prob_vals) < 3 || prob_vals[1] < prob_vals[2] ||
      prob_vals[1] > prob_vals[3] || prob_vals[2] > prob_vals[3]) {
    stop("prob_vals must include central, lower and upper quantiles,",
         " in that order")
  }
  if (type != "direct") {
    if (ecx_val < 1 || ecx_val > 99) {
      stop("Supplied ecx_val is not in the required range. ",
           "Please supply a percentage value between 1 and 99.")
    }
  }
  if (length(grep("ecx", object$model)) > 0) {
    mod_class <- "ecx"
  } else {
    mod_class <- "nec"
  }
  if (!is.null(object$bot)) {
    m4param <- 1
  } else {
    m4param <- 0
  }
  if (object$fit$family$family == "gaussian" && type == "absolute" &&
      m4param == 0) {
    stop("Absolute ECx values are not valid for a gaussian ",
         "response variable unless a model with a bot parameter is fit")
  }
  newdata_list <- newdata_eval(
    object, resolution = resolution, x_range = x_range
  )
  # dpar lets a two-block fit report its components separately. The default
  # (NULL) gives what posterior_epred always gave: mu * (1 - hu) for such a
  # family, the single mean curve otherwise. The zero-probability block is
  # inverted so that "decline from control" means the same thing as it does
  # everywhere else. Valid names are "mu" and whichever brms uses for the
  # second block: "hu" for hurdle families, "zi" for zero-inflated ones.
  if (is.null(dpar)) {
    p_samples <- posterior_epred(object, newdata = newdata_list$newdata,
                                 re_formula = NA)
  } else {
    if (!is_hurdle_family(object$fit$family)) {
      stop("The \"dpar\" argument is only valid for hurdle families.",
           call. = FALSE)
    }
    dpar <- match.arg(dpar, c("mu", hurdle_dpar(object$fit$family)))
    p_samples <- posterior_epred(object, newdata = newdata_list$newdata,
                                 re_formula = NA, dpar = dpar)
    if (dpar != "mu") {
      p_samples <- 1 - p_samples
    }
  }
  x_vec <- newdata_list$x_vec
  # if (grepl("horme", object$model)) {
  #   n <- seq_len(nrow(p_samples))
  #   p_samples <- do_wrapper(n, modify_posterior, object, x_vec,
  #                           p_samples, hormesis_def, fct = "rbind")
  # }
  ecx_fct <- get(paste0("ecx_x_", type))
  ecx_out <- apply(p_samples, 1, ecx_fct, ecx_val, x_vec)
  formula <- object$bayesnecformula
  x_str <- grep("crf(", labels(terms(formula)), fixed = TRUE, value = TRUE)
  x_call <- str2lang(eval(parse(text = x_str)))
  if (inherits(x_call, "call")) {
    x_call[[2]] <- str2lang("ecx_out")
    ecx_out <- eval(x_call)
  }
  if (inherits(xform, "function")) {
    ecx_out <- xform(ecx_out)
  }

  ecx_estimate <- quantile(unlist(ecx_out), probs = prob_vals)
  names(ecx_estimate) <- clean_names(ecx_estimate)
  attr(ecx_estimate, "resolution") <- resolution
  attr(ecx_out, "resolution") <- resolution
  attr(ecx_estimate, "ecx_val") <- ecx_val
  attr(ecx_out, "ecx_val") <- ecx_val
  attr(ecx_estimate, "toxicity_estimate") <- "ecx"
  attr(ecx_out, "toxicity_estimate") <-  "ecx"
  if (signif(ecx_estimate[1], 3) == signif(ecx_estimate[3], 3)) {
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
                              hormesis_def = "control", x_range = NA,
                              xform = identity,
                              prob_vals = c(0.5, 0.025, 0.975), ...,
                              dpar = NULL) {
  check_component_arg(list(...), object)
  chk_numeric(ecx_val)
  chk_numeric(resolution)  
  chk_logical(posterior)
  if (length(ecx_val)>1) {
    stop("You may only pass one ecx_val")  
  }
  if ((type %in% c("relative", "absolute", "direct")) == FALSE) {
    stop("type must be one of 'relative', 'absolute' (the default) or 'direct'. 
         Please see ?ecx for more details.")
  }
  if ((hormesis_def %in% c("max", "control")) == FALSE) {
    stop("type must be one of 'max' or 'control' (the default). 
         Please see ?ecx for more details.")
  }
  if (!inherits(xform, "function")) { 
    stop("xform must be a function.")}   
  if (length(prob_vals) < 3 || prob_vals[1] < prob_vals[2] ||
      prob_vals[1] > prob_vals[3] || prob_vals[2] > prob_vals[3]) {
    stop("prob_vals must include central, lower and upper quantiles,",
         " in that order")
  }
  sample_size <- object$sample_size
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
               posterior = TRUE, type = type, hormesis_def = hormesis_def,
               x_range = x_range, xform = xform, prob_vals = prob_vals,
               dpar = dpar)
    n_s <- as.integer(round(sample_size * object$mod_stats[x, "wi"]))
    sample(out, n_s)
  }
  to_iter <- seq_len(length(object$success_models))
  ecx_out <- unlist(lapply(to_iter, sample_ecx))
  ecx_estimate <- quantile(ecx_out, probs = prob_vals)
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

#' @noRd
ecx_x_relative <- function(y, ecx_val, x_vec) {
  if (length(which(!is.na(y))) == 0) {
    outval <- max(x_vec)
  } else {
    range_y <- range(y, na.rm = TRUE)
    ecx_y <- max(range_y) - diff(range_y) * (ecx_val / 100)
    outval <- x_vec[min_abs(y - ecx_y)]
  }
  outval
}

#' @noRd
ecx_x_absolute <- function(y, ecx_val, x_vec) {
  if (length(which(!is.na(y))) == 0) {
    outval <- max(x_vec)
  } else {
    range_y <- c(0, max(y, na.rm = TRUE))
    ecx_y <- max(range_y) - diff(range_y) * (ecx_val / 100)
    outval <- x_vec[min_abs(y - ecx_y)]
  }
  outval
}

#' @noRd
ecx_x_direct <- function(y, ecx_val, x_vec) {
  if (length(which(!is.na(y))) == 0) {
    outval <- max(x_vec)
  } else {
    ecx_y <- ecx_val
    outval <- x_vec[min_abs(y - ecx_y)]
  }
  outval
}
