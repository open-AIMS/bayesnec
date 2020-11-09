#' check_models
#'
#' Check model input for a Bayesian model fit
#'
#' @inheritParams bnec
#'
#' @details
#'
#' This is a wrapper function to test input model criteria and find the
#' correct models for use in \code{\link{fit_bayesnec}}.
#'
#' @importFrom stats na.omit
#' @return A \code{\link[base]{list}} of modified elements
#' necessary for \code{\link{fit_bayesnec}}.
check_models <- function(model, family){
  fam_tag <- family$family
  link_tag <- family$link
  
  if (link_tag=="logit" | link_tag=="log") {
    use_model <-  model[(model %in% mod_groups$bot_free)==FALSE]
    drop_model <- setdiff(model, use_model)
   if (length(drop_model)>0) {
      message(paste( "Dropping the model(s)", paste0(drop_model, collapse=", "), 
                     "as they are not valid in the case of a", link_tag, "link."))     
   }
   if (length(use_model)==0) {
      stop(paste("None of the model(s) specified are valid for a",  link_tag, "link."))
    }else{
      model <- use_model
    }
  }
  
  if (link_tag=="identity" & (fam_tag=="Beta" | fam_tag=="binomial")) {
    use_model <-  model[(model %in% c("nechorme", "nechorme4", "ecxlin"))==FALSE]
    drop_model <- setdiff(model, use_model)
    if (length(drop_model)>0) {
      message(paste( "Dropping the model(s)", paste0(drop_model, collapse=", "), 
                     "as they are not valid in the case of a", fam_tag, "with identity link."))     
    }
    if (length(use_model)==0) {
      stop(paste("None of the model(s) specified are valid for a",  fam_tag, "with identity link."))
    }else{
      model <- use_model
    }
  }
  
  if (link_tag=="identity" & (fam_tag=="Gamma" | fam_tag=="poisson" | fam_tag=="negbinomial")) {
    use_model <-  model[(model %in% "ecxlin")==FALSE]
    drop_model <- setdiff(model, use_model)
    if (length(drop_model)>0) {
      message(paste( "Dropping the model", paste0(drop_model, collapse=", "), 
                     "as they are not valid in the case of a", fam_tag, "with identity link."))     
    }
    if (length(use_model)==0) {
      stop(paste("The ecxlin model is not valid for a",  fam_tag, "with identity link."))
    }else{
      model <- use_model
    }
  }
  
  if (fam_tag=="gaussian") {
    use_model <-  model[(model %in% setdiff(mod_groups$bot_free, "ecxlin"))==FALSE]
    drop_model <- setdiff(model, use_model)
    if (length(drop_model)>0) {
      message(paste( "Dropping the model(s)", paste0(drop_model, collapse=", "), 
                     "as they are not valid in the case of gaussian y data."))     
    }
    if (length(use_model)==0) {
      stop("None of the model(s) specified are valid for gaussian y data.")
    }else{
      model <- use_model
    }
  }
return(model)
}
