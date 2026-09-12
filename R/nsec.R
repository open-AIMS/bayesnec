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
#' @param dpar For a joint two-block fit only (\code{family = "hurdle_gamma"}
#' or \code{"zero_inflated_beta"}), the parameter block to report:
#' \code{"mu"} for the response block, or \code{"hu"} (\code{"zi"} for the
#' zero-inflated families) for survival. Defaults to \code{NULL}, which gives
#' the combined endpoint \code{mu * (1 - hu)}. The zero-probability block is
#' inverted to survival before computing, so the NSEC is read off a declining
#' curve. See Details.
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
#' under the default \code{prob_vals}. And the crossing is sought from the
#' control upward, so extending \code{x_range} below the data does not place an
#' estimate at a concentration lower than any tested.
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
#' @references
#' Fisher R, Fox DR (2023). Introducing the no significant effect concentration
#' (NSEC).Environmental Toxicology and Chemistry, 42(9), 2019–2028.
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
                 xform = identity, prob_vals = c(0.5, 0.025, 0.975), ...,
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
                             xform = identity, prob_vals = c(0.5, 0.025, 0.975), ...,
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
  newdata_list <- newdata_eval(
    object, resolution = resolution, x_range = x_range
  )
  # dpar selects one block of a two-block (hurdle / zero-inflated) fit, exactly
  # as in ecx(). The default (NULL) leaves the behaviour posterior_epred always
  # gave: mu * (1 - hu) for such a family, the single mean curve otherwise. The
  # zero-probability block is inverted to survival first, so that the NSEC is
  # read off a declining curve and "decline from control" keeps its usual
  # meaning.
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
  nsec_out <- nsec_from_posterior(p_samples, reference, x_vec, x_control)
  n_missing <- sum(is.na(nsec_out))
  nsec_out <- sub_x_transformation(nsec_out, object$bayesnecformula)
  bound <- sub_x_transformation(max(x_vec), object$bayesnecformula)
  # xform reaches the censoring bound as well as the estimates, and the warning
  # follows both, so that the bound is on the scale the caller reads the
  # estimate on. See the same reordering in ecx.bayesnecfit.
  if (inherits(xform, "function")) {
    nsec_out <- xform(nsec_out)
    bound <- xform(bound)
  }
  if (n_missing > 0) {
    warning("The ", object$model, " curve does not fall below the control's ",
            sig_val, " quantile anywhere in the predictor range for ",
            n_missing, " of ", length(nsec_out), " draws, which return NA. ",
            "The NSEC is censored above ", signif(bound, 3), ".",
            call. = FALSE)
  }
  nsec_estimate <- quantile(unlist(nsec_out), probs = prob_vals, na.rm = TRUE)
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
                               xform = identity, prob_vals = c(0.5, 0.025, 0.975), ...,
                               posterior = FALSE, dpar = NULL) {
  check_component_arg(list(...), object)
  check_removed_args(list(...))
  if (length(sig_val)>1) {
    stop("You may only pass one sig_val")
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
    sample_out
  }
  to_iter <- seq_len(length(object$success_models))
  nsec_out <- lapply(to_iter, sample_nsec)
  ecnsecP <- unlist(lapply(nsec_out, 
                    FUN = function(p){attributes(p)$ecnsec_relativeP}))
  ecnsec <- quantile(ecnsecP, probs = prob_vals, na.rm = TRUE)
  nsec_out <- unlist(nsec_out)
  nsec_estimate <- quantile(nsec_out, probs = prob_vals, na.rm = TRUE)
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
                         xform = identity, prob_vals = c(0.5, 0.025, 0.975), ..., 
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
    nsec_out <- nsec_from_posterior(p_samples, reference, x_vec, min(x_vec))

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
      nsec_out <- nsec_from_posterior(p_samples, reference, x_vec, min(x_vec))
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
                     xform = identity, prob_vals = c(0.5, 0.025, 0.975), ...,
                     x_var,
                     horme = FALSE,
                     curveid = NA) {
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
