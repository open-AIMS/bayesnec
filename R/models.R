#' models
#'
#' Lists the fitted or available models
#'
#' @param object An object of class \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}} as returned by \code{\link{bnec}}, 
#' a character vector indicating the type of model set for which to list the available models,
#' or a numeric vector indicating the natural range of values which the models should be able to handle (see details). 
#' If missing all available models and their groups are listed.
#' 
#' @details The available models are "ecx4param", "ecxexp", "ecxlin", "ecxsigm", "ecxwb1", "ecxwb2",     
#'  "nec3param", "nec4param", "nechorme", "nechorme4", "neclin", "neclinhorme" and "necsigm". 
#'  
#'  To see the model formula and parameters for a specific model use the function \code{\link{show_params}}. 
#'  
#'  To see all the models in an available set (e.g. "all", "nec" or ecx") use the function \code{\link{models}} specifying the group name.
#'  
#'  To see the model names, model formula and parameters fitted in an existing bayesnec or bayesmanec model object use the function \code{\link{models}} specifying the fitted object.
#'  
#'  To see what models are available for a given type of data use the function \code{\link{models}} passing a numeric vector indicating the range of possible data types. Models that have an exponential decay (most models with parameter "beta") with no "bot" parameter are zero_bounded and are not suitable for the gaussian family, or any family modelled using a logit or log link function. Models with a linear decay (containing the string "lin" in their name) are not suitable for modelling families that are zero bounded (gamma, poisson, negative binomial) using an identity link. Models with a linear decay or hormesis linear increase (all models with parameter "slope") are not suitable for modelling families that are 0, 1 bounded (binomial, beta, betabinomial2). These restrictions do not need to be controlled by the user and a call to \code{\link{bnec}} with models="all" will simply exclude inappropriate models.
#'
#' @return A list of the available or fitted models
#' @examples
#' library(bayesnec)
#' # default to all models and model groups
#' models()
#' # single model
#' show_params("nec3param")
#' # group of models
#' models("all)
#' # models that are suitable for 0,1 bounded data
#' models(c(0,1))
#' 
#' @export
models <- function(object) {
  if (missing(object)) {return(mod_groups)}
  if (class(object) == "bayesnecfit") {use_mods <- object$model}
  else if (class(object) == "bayesmanecfit") {
    use_mods <- names(object$mod_fits)
    }
  else if (object[1]=="all") {use_mods <- mod_groups$all}
  else if (object[1]=="nec") {use_mods <- mod_groups$nec}
  else if (object[1]=="ecx") {use_mods <- mod_groups$ecx}
  else if (object[1]=="bot_free") {use_mods <- mod_groups$bot_free}
  else if (object[1]=="zero_bounded") {use_mods <- mod_groups$zero_bounded}
  else if (class(object)=="numeric"){
    all_mod <- mod_groups$all
    if(min(object)<0){ #gaussian family, logit and log link
      use_mods <- all_mod[!all_mod %in% mod_groups$zero_bounded]}
    if(min(object)>=0 & max(object)<=1){ #Identity link binomial, beta
      use_mods <- all_mod[!all_mod %in% c("neclin", "nechorme", "neclinhorme", "nechorme4", "ecxlin")]}
    if(min(object)>=0 & max(object)==Inf){ #Identity link gamma, poisson, negative binomial
      use_mods <-  all_mod[!all_mod %in% c("neclin", "neclinhorme", "ecxlin")]}
  }
  mod_params <- show_params(use_mods)
  names(mod_params) <- use_mods
  return(mod_params)       
  }