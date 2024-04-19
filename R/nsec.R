#' Extracts the predicted NSEC value as desired from an 
#' object of class \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} returned by \code{\link{bnec}}.
#' @param sig_val Probability value to use as the lower quantile to test
#' significance of the predicted posterior values.
#' @param resolution The number of unique x values over which to find NSEC -
#' large values will make the NSEC estimate more precise.
#' @param hormesis_def A \code{\link[base]{character}} vector, taking values
#' of "max" or "control". See Details.
#' @param xform A function to apply to the returned estimated concentration
#' values.
#' @param x_range A range of x values over which to consider extracting NSEC.
#' @param prob_vals A vector indicating the probability values over which to
#' return the estimated NSEC value. Defaults to 0.5 (median) and 0.025 and
#' 0.975 (95 percent credible intervals).
#' @param ... Further arguments to pass to class specific methods.
#'
#' @details NSEC is no-effect toxicity metric that estimates the concentration 
#' at which the modeled mean response is statistically indistinguishable from 
#' the mean control response. See the detailed derivation in
#' Fisher and Fox (2023).
#' 
#' For \code{hormesis_def}, if "max", then NSEC values are calculated
#' as a decline from the maximum estimates (i.e. the peak at NEC);
#' if "control", then NSEC values are calculated relative to the control, which
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
#' @seealso \code{\link{bnec}}
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
nsec <- function(object, sig_val = 0.01, resolution = 1000,
                 x_range = NA, hormesis_def = "control",
                 xform = identity, prob_vals = c(0.5, 0.025, 0.975), ...) {
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
nsec.bayesnecfit <- function(object, sig_val = 0.01, resolution = 1000,
                             x_range = NA, hormesis_def = "control", 
                             xform = identity, prob_vals = c(0.5, 0.025, 0.975), ..., 
                             posterior = FALSE) {
  chk_numeric(sig_val)
  chk_numeric(resolution)
  chk_logical(posterior)
  if (length(sig_val)>1) {
    stop("You may only pass one sig_val")  
  }
  if ((hormesis_def %in% c("max", "control")) == FALSE) {
    stop("type must be one of \"max\" or \"control\" (the default). ",
         "Please see ?ecx for more details.")
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
  p_samples <- posterior_epred(object, newdata = newdata_list$newdata,
                               re_formula = NA)
  x_vec <- newdata_list$x_vec
  reference <- quantile(p_samples[, 1], sig_val)
  ecnsecP <- apply(p_samples, MARGIN = 1, FUN = function(r){
    #(max(r) - diff(range(r)))/reference * 100
    (1-diff(c(min(r), reference))/(diff(range(r)))) * 100
  })
  ecnsec <- quantile(ecnsecP, probs = prob_vals)
  if (grepl("horme", object$model)) {
    n <- seq_len(nrow(p_samples))
    p_samples <- do_wrapper(n, modify_posterior, object, x_vec,
                            p_samples, hormesis_def, fct = "rbind")
    nec_posterior <- as_draws_df(object$fit)[["b_nec_Intercept"]]
    if (hormesis_def == "max") {
      reference <- quantile(apply(p_samples, 2, max), probs = sig_val)
    }
  }
  nsec_out <- apply(p_samples, 1, nsec_fct, reference, x_vec)
  formula <- object$bayesnecformula
  x_str <- grep("crf(", labels(terms(formula)), fixed = TRUE, value = TRUE)
  x_call <- str2lang(eval(parse(text = x_str)))
  if (inherits(x_call, "call")) {
    x_call[[2]] <- str2lang("nsec_out")
    nsec_out <- eval(x_call)
  }
  if (inherits(xform, "function")) {
    nsec_out <- xform(nsec_out)
  }
  nsec_estimate <- quantile(unlist(nsec_out), probs = prob_vals)
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
nsec.bayesmanecfit <- function(object, sig_val = 0.01, resolution = 1000,
                               x_range = NA, hormesis_def = "control", 
                               xform = identity, prob_vals = c(0.5, 0.025, 0.975), ..., 
                               posterior = FALSE) {
  if (length(sig_val)>1) {
    stop("You may only pass one sig_val")  
  }
  sample_nsec <- function(x, object, sig_val, resolution,
                          hormesis_def,
                          x_range, xform, prob_vals, 
                          posterior,
                          sample_size) {
    mod <- names(object$mod_fits)[x]
    target <- suppressMessages(pull_out(object, model = mod))
    out <- nsec(target, sig_val = sig_val, resolution = resolution,
                hormesis_def = hormesis_def,
                x_range = x_range, xform = xform, prob_vals = prob_vals,
                posterior = posterior)
    n_s <- as.integer(round(sample_size * object$mod_stats[x, "wi"]))
    sample_out <- sample(out, n_s)
    attr(sample_out, "ecnsec_relativeP") <- sample(attributes(out)$ecnsec_relativeP, n_s)
    sample_out
  }
  sample_size <- object$sample_size
  to_iter <- seq_len(length(object$success_models))
  nsec_out <- sapply(to_iter, sample_nsec, object, sig_val, resolution,
                     hormesis_def, x_range,
                     xform, prob_vals, posterior = TRUE, sample_size)
  ecnsecP <- unlist(lapply(nsec_out, 
                    FUN = function(p){attributes(p)$ecnsec_relativeP}))
  ecnsec <- quantile(ecnsecP, probs = prob_vals)
  nsec_out <- unlist(nsec_out)
  nsec_estimate <- quantile(nsec_out, probs = prob_vals)
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

#' @importFrom modelbased zero_crossings
#' 
#' @noRd
nsec_fct <- function(y, reference, x_vec) {
  val <- min(zero_crossings(y - reference))
  if(is.na(val)) {
    return(max(x_vec))} else {
      floor_x <-  x_vec[floor(val)] 
      ceiling_x <- x_vec[ceiling(val)]
      prop_x <- (val-floor(val))*(ceiling_x-floor_x)
      return(floor_x + prop_x)
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
nsec.brmsfit <- function(object, sig_val = 0.01, resolution = 1000,    
                         x_range = NA, hormesis_def = "control",  
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
  if ((hormesis_def %in% c("max", "control")) == FALSE) {
    stop("type must be one of \"max\" or \"control\" (the default). ",
         "Please see ?ecx for more details.")
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
    reference <- quantile(p_samples[, 1], sig_val)
    ecnsecP <- apply(p_samples, MARGIN = 1, FUN = function(r){
      #(max(r) - diff(range(r)))/reference * 100
      (1-diff(c(min(r), reference))/(diff(range(r)))) * 100
    })
    ecnsec <- quantile(ecnsecP, probs = prob_vals)
    
    if (horme) {
      n <- seq_len(nrow(p_samples))
      p_samples <- do_wrapper(n, modify_posterior, object, x_vec,
                                         p_samples, hormesis_def, fct = "rbind")
      nec_posterior <- as_draws_df(object$fit)[["b_nec_Intercept"]]
      if (hormesis_def == "max") {
        reference <- quantile(apply(p_samples, 2, max), probs = sig_val)
      }
    }    
    
    nsec_out <- apply(p_samples, 1, nsec_fct, reference, x_vec)
    
  } else {
    groups <-  unlist(unique(object$data[group_var]))
    out_vals <- lapply(groups, FUN = function(g){
      dat_list <- list(x_vec, g) 
      names(dat_list) <- c(x_var, group_var)
      pred_dat <- expand.grid(dat_list)
      
      p_samples <- posterior_epred(object, newdata = pred_dat, re_formula = NA)
      reference <- quantile(p_samples[, 1], sig_val)
      ecnsecP <- apply(p_samples, MARGIN = 1, FUN = function(r){
        #(max(r) - diff(range(r)))/reference * 100
        (1-diff(c(min(r), reference))/(diff(range(r)))) * 100
      })
      ecnsec <- quantile(ecnsecP, probs = prob_vals)      
      if (horme) {
        n <- seq_len(nrow(p_samples))
        p_samples <- do_wrapper(n, modify_posterior, object, x_vec,
                                           p_samples, hormesis_def, fct = "rbind")
        nec_posterior <- as_draws_df(object$fit)[["b_nec_Intercept"]]
        if (hormesis_def == "max") {
          reference <- quantile(apply(p_samples, 2, max), probs = sig_val)
        }
      }    
      
      nsec_out <- apply(p_samples, 1, nsec_fct, reference, x_vec)
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
nsec.drc <- function(object, sig_val = 0.01, resolution = 1000,
                     x_range = NA, hormesis_def = "control", 
                     xform = identity, prob_vals = c(0.5, 0.025, 0.975), ...,
                     x_var,
                     horme = FALSE,
                     curveid = NA) {
  chk_numeric(sig_val)
  chk_numeric(resolution)
  
  if (length(sig_val)>1) {
    stop("You may only pass one sig_val")  
  }
  if ((hormesis_def %in% c("max", "control")) == FALSE) {
    stop("type must be one of \"max\" or \"control\" (the default). ",
         "Please see ?ecx for more details.")
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
    ecnsec <- apply(p_samples, MARGIN = 2, FUN = function(r){
      (1-diff(c(min(r), reference))/(diff(range(r)))) * 100
    })
    
    nsec_out <- apply(p_samples, 2, nsec_fct, reference, x_vec)
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
      ecnsec <- apply(p_samples, MARGIN = 2, FUN = function(r){
        (1-diff(c(min(r), reference))/(diff(range(r)))) * 100
      })
      
      nsec_out <- apply(p_samples, 2, nsec_fct, reference, x_vec)

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
