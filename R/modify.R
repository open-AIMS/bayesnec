#' modify
#'
#' Modifies an existing bayesmanecfit object, for example, by adding or removing fitted models.
#' 
#' @param object An object of class "bayesmanecfit" output list, as returned by \code{\link{bnec}}.
#' @param drop A \code{\link[base]{character}} vector containing the names of model types you which to drop for the modified fit.
#' @param add A \code{\link[base]{character}} vector containing the names of model types to add to the modified fit.
#' 
#' @return All successfully fitted "bayesmanecfit" model fits.
#' @export
modify <- function(object, drop, add) {
  if (missing(drop) && missing(add)) {
    message("Nothing to modify, please specify a model to ",
            "either add or drop;\n",
            "Returning original model set")
    return(object)
  }
  model_set <- names(object$mod_fits)
  if (!missing(drop)) {
    model_set <- handle_set(model_set, drop = drop)
  }
  if (!missing(add)) {
    model_set <- handle_set(model_set, add = add)
  }
  if (is.logical(model_set)) {
    message("Returning original model set")
    return(object)
  }
  simdat <- extract_simdat(object$mod_fits[[1]])
  data <- object$mod_fits[[1]]$mod_dat
  x_type <- object$mod_fits[[1]]$x_type
  family <- object$mod_fits[[1]]$family
  mod_fits <- vector(mode = "list", length = length(model_set))
  names(mod_fits) <- model_set

  for (m in seq_along(model_set)) {
    model <- model_set[m]
    mod_m <- try(object$mod_fits[[model]], silent = TRUE)
    if (!inherits(mod_m, "bayesnecfit")) {
      fit_m <- try(
        fit_bayesnec(data = data,
                     x_var = "x",
                     y_var = "y",
                     trials_var = "trials",
                     x_type = x_type,
                     family = family,
                     model = model,
                     iter = simdat$iter,
                     thin = simdat$thin,
                     warmup = simdat$warmup,
                     chains = simdat$chains),
        silent = TRUE)
      if (!inherits(fit_m, "try-error")) {
        mod_fits[[model]] <- fit_m
      } else {
        mod_fits[[model]] <- NA
      }
    } else {
      mod_fits[[m]] <- mod_m
    }
  }
  mod_fits <- extract_modstats(mod_fits)
  export_list <- mod_fits
  if (!inherits(mod_fits, "bayesnecfit")) {
    class(export_list) <- "bayesmanecfit"
  }
  export_list
}
