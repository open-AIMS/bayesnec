#' Extracts the predicted NSEC value as desired from an 
#' object of class \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} returned by \code{\link{bnec}}.
#' @param sig_val Probability value to use as the lower quantile to test
#' significance of the predicted posterior values.
#' @param resolution The number of unique x values over which to find NSEC.
#' The crossing is located by linear interpolation between the two grid values
#' that bracket it, so precision saturates well below the grid spacing.
#' Increasing the resolution beyond the default of 200 changed the estimate by
#' less than 0.01 percent on the fits tested, and increases the run time
#' roughly in proportion.
#' @param xform A function to apply to the returned estimated concentration
#' values.
#' @param x_range A range of x values over which to consider extracting NSEC.
#' @param prob_vals A vector indicating the probability values over which to
#' return the estimated NSEC value. Defaults to 0.5 (median) and 0.025 and
#' 0.975 (95 percent credible intervals).
#' @param extrapolate The bound beyond which the estimate is reported as
#' censored. \code{FALSE}, the default, censors at the ends of the prediction
#' grid. A single number is an upper limit, and a pair of numbers is a lower
#' and an upper limit, in that order; the grid is extended to reach them.
#' \code{TRUE} is refused here, because every NSEC is read off a fitted curve.
#' See the \emph{Extrapolation} section of \code{\link{nsec}}.
#' @param dpar For a joint two-block fit only (\code{family = "hurdle_gamma"},
#' \code{"zero_inflated_beta"}, \code{"hurdle_poisson"} or
#' \code{"hurdle_negbinomial"}), the parameter block to report:
#' \code{"mu"} for the response block, or \code{"hu"} (\code{"zi"} for the
#' zero-inflated families) for survival. Defaults to \code{NULL}, which gives
#' the expected positive response multiplied by \code{1 - hu}. For continuous
#' hurdles the positive response is \code{mu}; for count hurdles it is
#' \code{E[Y | Y > 0]}. The zero-probability block is
#' inverted to survival before computing, so the NSEC is read off a declining
#' curve. See Details.
#' For the count hurdles, \code{"mu"} is converted to the positive-count mean
#' \code{E[Y | Y > 0]} so it matches the growth component returned by
#' \code{\link{bnec_hurdle}}.
#' @param ... Further arguments to pass to class specific methods.
#'
#' @details NSEC is no-effect toxicity metric that estimates the concentration 
#' at which the modeled mean response is statistically indistinguishable from 
#' the mean control response. See the detailed derivation in
#' Fisher and Fox (2023).
#' 
#' The reference is the \code{sig_val} quantile of the control posterior,
#' the control being the predicted mean at the lowest concentration in the
#' supplied predictor. That holds for every equation, hormetic ones included:
#' the \code{hormesis_def} argument selected between the control and the
#' maximum of the predicted curve and has been removed, because a target
#' below the control is crossed exactly once whatever the curve does above it.
#'
#' The attached \code{ecnsec} attribute is the percent effect at the NSEC,
#' defined as \code{\link{ecx}} defines it under \code{type = "absolute"}:
#' the decline from the control towards zero. Up to 2.1.3 it was measured
#' against the fitted range and computed by three different formulas that
#' agreed only for a monotonic curve.
#'
#' Two consequences follow from the reference being a quantile of the control
#' posterior. A \code{sig_val} share of the draws have a control at or below the
#' reference, and each of those reaches it at the control itself, so the control
#' concentration is that draw's NSEC. Fisher and Fox (2023) report the same
#' behaviour: the lower credible bound of the NSEC is the lowest concentration
#' whenever \code{sig_val} is above the quantile the bound is read at, 0.025
#' under the default \code{prob_vals}. And the crossing is sought at or above the
#' control, so extending \code{x_range} below the data does not place an estimate
#' at a concentration lower than any tested.
#'
#' Two limits on that. The value is the lowest \emph{observed} concentration,
#' where Fisher and Fox (2023) report zero concentration, which they reach by
#' extrapolating the fitted curve below the data. The two agree where the control
#' of the design is a true zero and the predictor is untransformed; otherwise the
#' bound reported here is the control rather than the 0 of their Table 3, and a
#' transformed predictor returns it on the transformed scale like any other
#' estimate. And it applies where the prediction grid begins at the control. Where
#' \code{x_range} begins at a higher concentration, a draw already at or below
#' the reference at the first grid point reached it somewhere below the range
#' asked for, which is not identified within that range: such a draw returns
#' \code{NA} and is reported with those that never reach the reference.
#'
#' Where a draw's curve does not reach the reference at any tested concentration
#' its NSEC is above the highest concentration in the prediction grid. Such a
#' draw returns \code{NA} and is excluded from the summary, which is therefore
#' censored above that concentration, and a warning reports how many draws were
#' affected. Extending \code{x_range} will estimate it, at the price of reading
#' the curve where there are no data.
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
#' @seealso \code{\link{bnec}}, \code{\link{bnec_hurdle}}, \code{\link{ecx}}
#'
#' @return A vector containing the estimated NSEC value, including upper and
#' lower 95% credible interval bounds.
#'
#' @section Capping and dropping:
#' A draw whose curve does not reach the target anywhere in the prediction
#' range has no such concentration in that range, and may have none at any
#' concentration. It is not capped at the top of the range, which would assert
#' that its estimate equals that value; it is recorded as lying beyond it. The
#' summary is then censored: the draw keeps its rank and is given no value, so
#' a reported quantile that falls among such draws is the end of the prediction
#' range rather than a quantile of the draws that did reach the target.
#' Attribute \code{"censored_summary"} marks which entries those are, and
#' states how many draws lie beyond each end and where the ends are. A draw
#' whose curve had already passed the target where the range begins is recorded
#' at the other end and reported the same way.
#'
#' Which quantile estimator is used depends on whether anything is censored.
#' A posterior with no beyond-range draw is summarised exactly as it was up to
#' version 2.1.3, with \code{\link[stats]{quantile}}'s default type 7, which
#' interpolates between two adjacent order statistics. Once any draw is
#' censored every reported entry becomes an order statistic instead, because a
#' value interpolated across a draw that has no value would be one the
#' posterior does not support.
#' Every entry therefore changes a little when the first draw is censored,
#' including entries at the other end of the interval. Keeping the uncensored
#' summary bit-identical to the release was preferred to making the two agree
#' at the boundary, because every archived analysis is compared against it.
#'
#' Up to version 2.1.3 such a draw was deleted and the remaining draws were
#' summarised as though nothing had been removed, which reported an estimate
#' lower than the quantity it was labelled as, with an interval narrower than
#' the posterior supports. See \code{\link{nec}} for the same treatment of a
#' threshold parameter, where the draw does have a value and the operation is
#' right-censoring in the ordinary sense.
#'
#' @section Extrapolation:
#' \code{extrapolate} chooses the bound the estimate is censored at, on the
#' same predictor scale as \code{x_range} and the data, not the scale
#' \code{crf()} fits on and not the scale \code{xform} displays.
#'
#' \code{TRUE} asks for no bound at either end, and is an error here whatever
#' the fit is. An NSEC is the concentration at which a fitted curve reaches a
#' reference, so reading one means evaluating that curve on a grid, and no grid
#' reaches infinity. Name a finite limit instead.
#'
#' A finite limit extends the grid the curve is searched on and then censors
#' there, so \code{extrapolate} does through a checked argument what
#' \code{x_range} does through an unchecked one: a limit inside the current
#' range is an error rather than a silent tightening, because it would report
#' an estimate as censored at a value the fit had no trouble identifying. Use
#' \code{x_range} to narrow the range.
#'
#' A limit is measured against the range this call would otherwise use.
#' Where \code{x_range} is given, that is the range, at both ends: it is a
#' deliberate narrowing, and \code{extrapolate} extends from it rather than
#' discarding it. Where it is not, the range is the wider of the observed range
#' of the predictor and the prediction range the fit itself stores, which
#' differ for a fit built over a grid above the data; a limit between them
#' would report the estimate as censored inside the grid the fit was built on,
#' so both have to be cleared. \code{\link{nec}} measures against that stored
#' grid alone, having no \code{x_range} of its own, so the two agree on a fit
#' whose grid reaches beyond the data and can differ on one whose grid stops
#' short of it.
#'
#' A lower limit extends the grid but not the search. The reference is a
#' quantile of the control posterior, so the control is where the search
#' begins, and a draw already past the reference there has no identifiable
#' crossing at any lower concentration. Such a limit is accepted, and a message
#' states that the estimate stays censored at the control.
#'
#' @references
#' Fisher R, Fox DR (2023). Introducing the no significant effect concentration
#' (NSEC). Environmental Toxicology and Chemistry, 42(9), 2019–2028.
#' doi: 10.1002/etc.5610.
#'
#' @examples
#' \donttest{
#' library(bayesnec)
#'
#' data(manec_example)
#' nsec(manec_example)
#' }
#'
#' @export
# dpar sits after `...` for the same reason as in ecx(): it matches the methods,
# and naming it here is what puts it in \usage. Methods that have no use for it
# (nsec.drc, nsec.brmsfit) absorb it through their own `...`.
nsec <- function(object, sig_val = 0.01, resolution = 200,
                 x_range = NA,
                 xform = identity, prob_vals = c(0.5, 0.025, 0.975),
                 extrapolate = FALSE, ...,
                 dpar = NULL) {
  UseMethod("nsec")
}

#' @inheritParams nsec
#'
#' @param object An object of class \code{\link{bayesnecfit}} returned by
#' \code{\link{bnec}}.
#' @param posterior A \code{\link[base]{logical}} value indicating if the full
#' posterior sample of calculated NSEC values should be returned instead of
#' just the median and 95 credible intervals.
#'
#' @inherit nsec details seealso return examples
#' 
#' @importFrom stats quantile
#' @importFrom brms as_draws_df posterior_epred
#' @importFrom chk chk_logical chk_numeric
#' 
#' @noRd
#'
#' @export
nsec.bayesnecfit <- function(object, sig_val = 0.01, resolution = 200,
                             x_range = NA,
                             xform = identity, prob_vals = c(0.5, 0.025, 0.975),
                             extrapolate = FALSE, ...,
                             posterior = FALSE, dpar = NULL) {
  check_component_arg(list(...), object)
  check_removed_args(list(...))
  chk_numeric(sig_val)
  chk_numeric(resolution)
  chk_logical(posterior)
  if (length(sig_val)>1) {
    stop("You may only pass one sig_val")  
  }
  if(!inherits(xform, "function")) { 
    stop("xform must be a function.")}  
  if (length(prob_vals) < 3 | prob_vals[1] < prob_vals[2] |
      prob_vals[1] > prob_vals[3] | prob_vals[2] > prob_vals[3]) {
    stop("prob_vals must include central, lower and upper quantiles,",
         " in that order.")
  }
  # extrapolate resolves into x_range, because the grid is the only thing an
  # NSEC can be extended over: the estimate is read off a curve and the curve
  # is read off the grid. What extrapolate adds over x_range is the refusal to
  # narrow and the refusal of an infinite limit, both of which a bare x_range
  # accepts silently.
  lims <- extrapolate_limits(extrapolate,
                             searched_or_stored_bounds(object, x_range),
                             "NSEC")
  if (!is.null(lims)) {
    report_curve_read_lower_limit(object, lims)
    x_range <- c(lims$lower, lims$upper)
  }
  newdata_list <- newdata_eval(
    object, resolution = resolution, x_range = x_range
  )
  # dpar selects one block of a two-block (hurdle / zero-inflated) fit, exactly
  # as in ecx(). The default (NULL) leaves the behaviour posterior_epred always
  # gave: the positive-part mean times (1 - hu) for such a family, the single
  # mean curve otherwise. The zero-probability block is inverted to survival
  # first, so that the NSEC is
  # read off a declining curve and "decline from control" keeps its usual
  # meaning.
  epred_fun <- function(nd) joint_hurdle_epred(object, nd, dpar)
  p_samples <- epred_fun(newdata_list$newdata)
  x_vec <- newdata_list$x_vec
  # The control posterior is read at the lowest observed concentration rather
  # than at the first column of the grid, so that supplying x_range does not
  # change the reference and therefore the estimate. See D15 ruling 2.
  control <- control_posterior(object, newdata_list$newdata, epred_fun)
  reference <- quantile(control, sig_val)
  # ecnsec is the percent effect at the NSEC, defined exactly as ecx defines
  # it and defaulting to the same reference: the decline from the control to
  # zero. The three formulas this replaces measured the decline against the
  # fitted range instead, and disagreed with each other. See toxval#49 and
  # D15 ruling 5.
  ecnsecP <- as.numeric((control - reference) / control * 100)
  ecnsec <- quantile(ecnsecP, probs = prob_vals, na.rm = TRUE)
  # The hormesis branch that stood here switched the reference to the maximum
  # of the predicted curve when hormesis_def was "max". The control is now
  # always the reference, so the branch selects nothing and hormesis_def has
  # been removed. See D15 rulings 1 and 4.
  x_control <- control_x(object)
  nsec_out <- nsec_from_posterior(p_samples, reference, x_vec, x_control,
                                  control)
  # The two classes of draw that return NA are opposite statements about where
  # the estimate is, and one message for both would say of each the thing that
  # is true of the other. Counted before sub_x_transformation(), which returns a
  # bare value.
  below <- attr(nsec_out, "below_range")
  n_below <- sum(below)
  searched_from <- attr(nsec_out, "x_searched_from")
  above <- is.na(nsec_out) & !below
  n_above <- sum(above)
  nsec_out <- sub_x_transformation(nsec_out, object$bayesnecformula)
  # The record is built here rather than read off the fit, because nsec() works
  # on a grid the caller may have changed through x_range. Its lower bound is
  # the point the search started from, which is what a below-range draw is
  # known to lie beneath, and not min(x_vec): the two differ wherever the grid
  # reaches below the control. It is built on the recorded scale, the scale the
  # search ran on, and remapped onto the fitted scale, which swaps the two ends
  # under a decreasing crf(). See the same construction in ecx.bayesnecfit.
  cens <- xform_censoring(
    censoring_record(max(x_vec), searched_from, above, below),
    function(value) sub_x_transformation(value, object$bayesnecformula)
  )
  # xform reaches the censoring bounds as well as the estimates, and the
  # warnings follow both, so that a bound is on the scale the caller reads the
  # estimate on. See the same reordering in ecx.bayesnecfit.
  if (inherits(xform, "function")) {
    nsec_out <- xform(nsec_out)
    cens <- xform_censoring(cens, xform)
  }
  # The end each class is named at comes from the record's own flag rather than
  # from a local variable, so that a decreasing crf() or xform reports each set
  # of draws at the end of the reported scale they actually lie beyond, and does
  # so where draws sit at both ends at once.
  if (n_above > 0) {
    warning("The ", object$model, " curve does not fall below the control's ",
            sig_val, " quantile anywhere in the predictor range for ",
            n_above, " of ", length(nsec_out), " draws. The NSEC is censored ",
            "at ", signif(censored_end(cens, "above"), 3),
            ": those draws keep their rank in the summary and are given no ",
            "value.", call. = FALSE)
  }
  if (n_below > 0) {
    below_at <- signif(censored_end(cens, "below"), 3)
    warning("The ", object$model, " curve falls below the control's ", sig_val,
            " quantile before ", below_at, ", the lowest concentration in the ",
            "prediction range, for ", n_below, " of ", length(nsec_out),
            " draws. The NSEC is censored at ", below_at,
            ", which this x_range does not cover.", call. = FALSE)
  }
  # sub_x_transformation() returns the vector with its attributes, so the three
  # nsec_from_posterior() left for the warnings would otherwise reach the caller,
  # and only on this class: the bayesmanecfit path subsets by draw index and
  # drops them. The censoring record replaces them and is meant to travel.
  attr(nsec_out, "n_below_range") <- NULL
  attr(nsec_out, "below_range") <- NULL
  attr(nsec_out, "x_searched_from") <- NULL
  attr(nsec_out, "censored") <- cens
  nsec_estimate <- summarise_censored(unlist(nsec_out), prob_vals, cens)
  names(nsec_estimate) <- clean_names(nsec_estimate)
  attr(nsec_estimate, "resolution") <- resolution
  attr(nsec_out, "resolution") <- resolution
  attr(nsec_estimate, "sig_val") <- sig_val
  attr(nsec_out, "sig_val") <- sig_val
  attr(nsec_estimate, "toxicity_estimate") <- "nsec"
  attr(nsec_out, "toxicity_estimate") <-  "nsec"
  attr(nsec_estimate, "ecnsec_relativeP") <- ecnsec
  attr(nsec_out, "ecnsec_relativeP") <-  ecnsecP
  if (!posterior) {
    nsec_estimate
  } else {
    nsec_out
  }
}

#' @inheritParams nsec
#'
#' @param object An object of class \code{\link{bayesmanecfit}} returned by
#' \code{\link{bnec}}.
#' @param posterior A \code{\link[base]{logical}} value indicating if the full
#' posterior sample of calculated NSEC values should be returned instead of
#' just the median and 95 credible intervals.
#'
#' @inherit nsec details seealso return examples
#' 
#' @importFrom stats quantile
#'
#' @noRd
#'
#' @export
nsec.bayesmanecfit <- function(object, sig_val = 0.01, resolution = 200,
                               x_range = NA,
                               xform = identity, prob_vals = c(0.5, 0.025, 0.975),
                               extrapolate = FALSE, ...,
                               posterior = FALSE, dpar = NULL) {
  check_component_arg(list(...), object)
  check_removed_args(list(...))
  if (length(sig_val)>1) {
    stop("You may only pass one sig_val")
  }
  # Resolved once for the set and passed down as x_range, so that every
  # component is searched over one grid. Resolving it again inside each
  # component call would measure each limit against that component's own
  # range.
  lims <- extrapolate_limits(extrapolate,
                             searched_or_stored_bounds(object, x_range),
                             "NSEC")
  if (!is.null(lims)) {
    report_curve_read_lower_limit(object, lims)
    x_range <- c(lims$lower, lims$upper)
  }
  sample_size <- object$sample_size
  # The same weighted index every other quantity on this object uses, rather
  # than a fresh unseeded sample() here. Two things follow. The model-averaged
  # NSEC stops moving between calls -- and it was the lower bound, the end a
  # protective concentration is read off, that moved most. And the NSEC and its
  # ecnsec are drawn with one index instead of two independent ones, so a pair
  # is now the same posterior draw of the same model rather than two unrelated
  # ones. Which draws are kept is unchanged in kind, and each NSEC is still read
  # off its own model's curve exactly as before, so this settles nothing about
  # what those curves are anchored to (#19). See #216.
  draw_index <- pull_draw_index(object, names(object$mod_fits), sample_size)
  # A closure rather than positional dispatch through sapply(), for the reason
  # given in ecx.bayesmanecfit: the previous form silently dropped any argument
  # not named in the positional list, dpar included.
  sample_nsec <- function(x) {
    mod <- names(object$mod_fits)[x]
    target <- suppressMessages(pull_out(object, model = mod))
    out <- nsec(target, sig_val = sig_val, resolution = resolution,
                x_range = x_range, xform = xform, prob_vals = prob_vals,
                posterior = TRUE, dpar = dpar)
    idx <- draw_index[[mod]]
    sample_out <- out[idx]
    attr(sample_out, "ecnsec_relativeP") <-
      attributes(out)$ecnsec_relativeP[idx]
    # Subsetting drops the record, so the share of it that belongs to these
    # draws is rebuilt here. Without this a single fit and the
    # one-model average of it would report the same quantity two different
    # ways, and pull_out() would change a number without changing a model.
    attr(sample_out, "censored") <-
      subset_censoring(attr(out, "censored"), idx)
    sample_out
  }
  to_iter <- seq_len(length(object$success_models))
  nsec_out <- lapply(to_iter, sample_nsec)
  ecnsecP <- unlist(lapply(nsec_out, 
                    FUN = function(p){attributes(p)$ecnsec_relativeP}))
  ecnsec <- quantile(ecnsecP, probs = prob_vals, na.rm = TRUE)
  cens <- concat_censoring(lapply(nsec_out, attr, "censored"),
                           vapply(nsec_out, length, integer(1)))
  nsec_out <- unlist(lapply(nsec_out, as.numeric))
  attr(nsec_out, "censored") <- cens
  nsec_estimate <- summarise_censored(nsec_out, prob_vals, cens)
  names(nsec_estimate) <- clean_names(nsec_estimate)
  attr(nsec_estimate, "resolution") <- resolution
  attr(nsec_out, "resolution") <- resolution
  attr(nsec_estimate, "sig_val") <- sig_val
  attr(nsec_out, "sig_val") <- sig_val
  attr(nsec_estimate, "toxicity_estimate") <- "nsec"
  attr(nsec_out, "toxicity_estimate") <-  "nsec"
  attr(nsec_estimate, "ecnsec_relativeP") <- ecnsec
  attr(nsec_out, "ecnsec_relativeP") <-  ecnsecP
  if (!posterior) {
    nsec_estimate
  } else {
    nsec_out
  }
}

#' @inheritParams nsec
#'
#' @param object An object of class \code{\link{brmsfit}} returned by
#' \code{\link{brms}}.
#' @param posterior A \code{\link[base]{logical}} value indicating if the full
#' posterior sample of calculated NSEC values should be returned instead of
#' just the median and 95 credible intervals.
#' @param x_var A character indicating the name of the predictor (x) data in object
#' @param group_var A character indicating the name of the grouping variable in object
#' @param by_group A logical indicating if nsec values should be returned for 
#' each level in group_var, or marginalised across all groups.
#' @param horme Logical indicating if hormesis is evident.
#' 
#' @importFrom stats quantile
#' @importFrom dplyr bind_cols
#' @importFrom brms as_draws_df posterior_epred
#' @importFrom chk chk_logical chk_numeric
#' 
#' @noRd
#'
#' @export
nsec.brmsfit <- function(object, sig_val = 0.01, resolution = 200,    
                         x_range = NA,
                         xform = identity, prob_vals = c(0.5, 0.025, 0.975),
                         extrapolate = FALSE, ..., 
                         posterior = FALSE,
                         x_var, 
                         group_var = NA, 
                         by_group = FALSE,
                         horme = FALSE){
  chk_numeric(sig_val)
  chk_numeric(resolution)
  chk_logical(posterior)
  if (length(sig_val)>1) {
    stop("You may only pass one sig_val")  
  }
  if(!inherits(xform, "function")) { 
    stop("xform must be a function.")}  
  if (length(prob_vals) < 3 | prob_vals[1] < prob_vals[2] |
      prob_vals[1] > prob_vals[3] | prob_vals[2] > prob_vals[3]) {
    stop("prob_vals must include central, lower and upper quantiles,",
         " in that order.")
  }
  if (missing(x_var)) {
    stop("x_var must be supplied for a brmsfit object.")    
  }  
  check_no_extrapolate(extrapolate, "brmsfit")
  if (by_group & is.na(group_var)){
    stop("You must specify a group_by variable if you want values returned by groups.")
  }
  
  col_names <- colnames(object$data)
  if(max(grepl(x_var, col_names))==0) {
    stop("Your suplied x_var is not contained in the object data.frame")
  }
  if(!is.na(group_var)){
    if(max(grepl(group_var, col_names))==0) {
      stop("Your suplied group_var is not contained in the object data.frame")
    }     
  }
 
  if(is.na(x_range)){
    x_range = range(object$data[x_var])
  }
  x_vec <- seq(min(x_range), max(x_range), length=resolution)

  if(is.na(group_var)){
    pred_dat <- data.frame(x_vec)
    names(pred_dat) <- x_var
    
    p_samples <- try(posterior_epred(object, newdata = pred_dat, re_formula = NA),
                     silent = TRUE)
    if (class(p_samples)[1] == "try-error"){
      stop(paste(attributes(p_samples)$condition, "Do you need to specify a group_var variable?", sep=""))
    }
    control <- p_samples[, 1]
    reference <- quantile(control, sig_val)
    ecnsecP <- as.numeric((control - reference) / control * 100)
    ecnsec <- quantile(ecnsecP, probs = prob_vals, na.rm = TRUE)
    nsec_out <- nsec_from_posterior(p_samples, reference, x_vec, min(x_vec),
                                    control)

  } else {
    groups <-  unlist(unique(object$data[group_var]))
    out_vals <- lapply(groups, FUN = function(g){
      dat_list <- list(x_vec, g) 
      names(dat_list) <- c(x_var, group_var)
      pred_dat <- expand.grid(dat_list)
      
      p_samples <- posterior_epred(object, newdata = pred_dat, re_formula = NA)
      control <- p_samples[, 1]
      reference <- quantile(control, sig_val)
      ecnsecP <- as.numeric((control - reference) / control * 100)
      ecnsec <- quantile(ecnsecP, probs = prob_vals, na.rm = TRUE)
      nsec_out <- nsec_from_posterior(p_samples, reference, x_vec, min(x_vec),
                                      control)
      nsec_out <- unlist(nsec_out)
      attr(nsec_out, "ecnsec_relativeP") <- ecnsec
      nsec_out
    })
    ecnsec <- lapply(out_vals, 
                     FUN = function(p){attributes(p)$ecnsec_relativeP})
    names(ecnsec) <- groups
  }
  
  if(by_group & posterior & !is.na(group_var)){
    names(out_vals) <- groups
    out_vals <- out_vals |> bind_cols() |> 
      pivot_longer(everything(), names_to = group_var, values_to = "NSEC")
    attr(out_vals, "ecnsec_relativeP") <- ecnsec
  }
  
  if(by_group & !posterior & !is.na(group_var)){   
    names(out_vals) <- groups
    out_vals <- lapply(out_vals, quantile, probs = prob_vals) |> 
      bind_rows(.id = group_var)
    names(out_vals) <- clean_names(out_vals)
    attr(out_vals, "ecnsec_relativeP") <- ecnsec
  }
  
  if(!by_group & posterior & !is.na(group_var)){
    out_vals <- as.numeric((unlist(out_vals)))
    attr(out_vals, "ecnsec_relativeP") <- ecnsec
  }
  
  if(!by_group & !posterior & !is.na(group_var)){   
    out_vals <- quantile(unlist(out_vals), probs = prob_vals)
    names(out_vals) <- clean_names(out_vals)
    attr(out_vals, "ecnsec_relativeP") <- ecnsec
  }
  
  if(posterior & is.na(group_var)){ 
    out_vals <- unlist(nsec_out)
    # below_range is the per-draw vector nsec_from_posterior() adds for the
    # censoring record. This method builds no record -- it has no
    # bayesnecformula to take a bound from, and it summarises with quantile()
    # and no na.rm, so a beyond-range draw raises an error here rather than
    # being deleted in silence -- so the vector it would otherwise return is
    # dropped. The n_below_range and x_searched_from this method has always
    # returned are left alone: they are not this branch's to change.
    attr(out_vals, "below_range") <- NULL
    attr(out_vals, "ecnsec_relativeP") <- ecnsecP
  }
  
  if(!posterior & is.na(group_var)){   
    
    out_vals <- quantile(unlist(nsec_out), probs = prob_vals)
    attr(out_vals, "ecnsec_relativeP") <- ecnsec
    names(out_vals) <- clean_names(out_vals)
  }
  
  attr(out_vals, "resolution") <- resolution
  attr(out_vals, "sig_val") <- sig_val
  attr(out_vals, "toxicity_estimate") <- "nsec"

  return(out_vals)
}

#' @inheritParams nsec
#'
#' @param object An object of class \code{\link{drc}} returned by
#' \code{\link{drc}}.
#' @param x_var A character indicating the name of the predictor (x) data in object
#' each level in group_var, or marginalised across all groups.
#' @param horme Logical indicating if hormesis is evident. Not currently implemented.
#' @param curveid A character indicating the name of the grouping variable in object
#' 
#' @importFrom chk chk_logical chk_numeric
#' 
#' @noRd
#'
#' @export
nsec.drc <- function(object, sig_val = 0.01, resolution = 200,
                     x_range = NA,
                     xform = identity, prob_vals = c(0.5, 0.025, 0.975),
                     extrapolate = FALSE, ...,
                     x_var,
                     horme = FALSE,
                     curveid = NA) {
  check_no_extrapolate(extrapolate, "drc")
  chk_numeric(sig_val)
  chk_numeric(resolution)
  
  if (length(sig_val)>1) {
    stop("You may only pass one sig_val")  
  }
  if(!inherits(xform, "function")) { 
    stop("xform must be a function.")}  
  if (length(prob_vals) < 3 | prob_vals[1] < prob_vals[2] |
      prob_vals[1] > prob_vals[3] | prob_vals[2] > prob_vals[3]) {
    stop("prob_vals must include central, lower and upper quantiles,",
         " in that order.")
  }

  if(is.na(x_range)){
    x_range = range(object$data[x_var])
  }
  x_vec <- seq(min(x_range), max(x_range), length=resolution)
  
  if(is.na(curveid)){
    pred_dat <- data.frame(x_vec)
    names(pred_dat) <- x_var
  
    p_samples <- suppressWarnings(predict(object, newdata = pred_dat,
                      interval = "confidence", level = prob_vals[3]-prob_vals[2]))
    # check curve goes down
    if (p_samples[1, "Prediction"]<p_samples[2, "Prediction"]){
      stop("nsec can currently only be estimated for curves that represent an overall decreasing function")
    }
      
    # calculate the reference level
    ref_dat <- data.frame(min(x_vec))
    colnames(ref_dat) <- colnames(x_vec)
    reference <- suppressWarnings(predict(object, newdata = ref_dat,
                         interval = "confidence" , 
                         level = 1-(sig_val*2))["Lower"])
    control <- p_samples[1, "Prediction"]
    ecnsec <- as.numeric((control - reference) / control * 100)
    # p_samples holds the fitted curve and its two confidence limits rather than
    # posterior draws, and the reference is the control's lower limit at
    # level = 1 - 2 * sig_val while the curves are drawn at
    # prob_vals[3] - prob_vals[2]. The lower curve therefore begins at or below
    # the reference for any sig_val at or above half the excluded probability --
    # 0.025 under the default prob_vals -- and returned NA for those. The
    # crossing of a curve that begins on the reference is the control, which is
    # what x_start supplies. drc is not a dependency of this package, so no test
    # covers this method; the interval this construction builds is not from
    # Fisher and Fox (2023), whose frequentist NSEC is the fitted-mean crossing
    # alone. See #325.
    nsec_out <- apply(p_samples, 2, function(col) {
      crossing_x(col, reference, x_vec, x_start = min(x_vec))
    })
    if (inherits(xform, "function")) {
      xform(nsec_out)
    } 
    out_vals <- as.numeric(unlist(nsec_out))
    attr(out_vals, "ecnsec_relativeP") <- ecnsec
  } else {
    groups <-  unlist(unique(object$data[, 4]))
    out_vals <- lapply(groups, FUN = function(g){
      dat_list <- list(x_vec, g) 
      names(dat_list) <- c(x_var, curveid)
      pred_dat <- expand.grid(dat_list)

      p_samples <- suppressWarnings(predict(object, newdata = pred_dat,
                                            interval = "confidence", level = prob_vals[3]-prob_vals[2]))
      # check curve goes down
      if (p_samples[1, "Prediction"]<p_samples[2, "Prediction"]){
        stop("nsec can currently only be estimated for curves that represent an overall decreasing function")
      }
      
      # calculate the reference level
      ref_dat <- data.frame(min(x_vec))
      colnames(ref_dat) <- colnames(x_vec)
      reference <- suppressWarnings(predict(object, newdata = ref_dat,
                                            interval = "confidence" , 
                                            level = 1-(sig_val*2))["Lower"])
      control <- p_samples[1, "Prediction"]
      ecnsec <- as.numeric((control - reference) / control * 100)
      # x_start as in the branch above, for the same reason.
      nsec_out <- apply(p_samples, 2, function(col) {
        crossing_x(col, reference, x_vec, x_start = min(x_vec))
      })

      if (inherits(xform, "function")) {
        nsec_out <- xform(nsec_out)
      }
      attr(nsec_out, "ecnsec_relativeP") <- ecnsec
      nsec_out
    })  
    names(out_vals) <- groups
    ecnsec <- do.call("rbind", lapply(out_vals, FUN = function(x){attributes(x)$ecnsec_relativeP}))
    out_vals <- do.call("rbind", out_vals) 
    attr(out_vals, "ecnsec_relativeP") <- ecnsec
  }

  nsec_estimate <- out_vals
  attr(nsec_estimate, "resolution") <- resolution
  attr(nsec_estimate, "sig_val") <- sig_val
  attr(nsec_estimate, "toxicity_estimate") <- "nsec"
  nsec_estimate
  
}
