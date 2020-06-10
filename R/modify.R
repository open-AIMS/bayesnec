#' modify
#'
#' Modifies an existing bayesmanecfit object, for example, but adding or removing fitted models.
#'
#' @aliases modify
#' 
#' @param object An object of class "bayesmanecfit" output list, as returned by \code{\link{bnec}}.
#' @param model_set A \code{\link[base]{character}} vector containing the of names of model types to be included in the modified fit.
#' @param drop_models A \code{\link[base]{character}} vector containing the names of model types you which to drop for the modified fit.
#' @param add_models A \code{\link[base]{character}} vector containing the names of model types to add to the modified fit.
#' 
#' @return All successfully fitted "bayesmanecfit" model fits.
modify <- function(object, model_set = NA, drop_models = NA,
                   add_models = NA) {
  if (is.na(model_set[1])) {
    model_set <- names(object$mod_fits)
  }
  if (model_set[1] == "nec") {
    model_set <- c("nec3param", "nec4param", "nechorme", "necsigm")
  }
  if (model_set[1] == "ecx") {
    model_set <- c("ecx4param", "ecxwb1", "ecxwb2")
  }
  if (model_set[1] == "all") {
    model_set <- c("nec3param", "nec4param", "nechorme", "necsigm",
                   "ecxlin", "ecxexp", "ecxsigm",
                   "ecx4param", "ecxwb1", "ecxwb2")
  }
  if (model_set[1] == "bot_free") {
    model_set <- c("nec3param", "nechorme", "necsigm",
                   "ecxlin", "ecxexp", "ecxsigm")
  }

  if (!is.na(drop_models[1])) {
    model_set <- model_set[is.na(match(model_set, drop_models))]
  }
  if (!is.na(add_models[1])) {
    model_set <- unique(c(model_set, add_models))
  }
  simdat <- extract_simdat(object$mod_fits[[1]])
  mod_fits <- vector(mode = "list", length = length(model_set))
  names(mod_fits) <- model_set

  for (m in seq_len(length(model_set))) {
    model <- model_set[m]
    mod_m <- NULL
    mod_m <- try(object$mod_fits[[model]], silent = TRUE)
    if (!inherits(mod_m, "bayesnecfit")) {
      fit_m <- try(
        fit_bayesnec(data = object,
                     x_var = object$x_var,
                     y_var = object$y_var,
                     trials_var = object$trials_var,
                     x_type = object$x_type,
                     family = object$family,
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
  export_list <- c(extract_modstats(mod_fits),
                   list(data = object$data,
                        x_var = object$x_var,
                        y_var = object$y_var,
                        trials_var = object$trials_var))
  class(export_list) <- "bayesmanecfit"
  export_list
}
