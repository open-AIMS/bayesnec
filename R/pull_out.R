#' pull_out
#'
#' Pulls a single model from an existing bayesmanecfit object,
#' and converts into a bayesnecfit object.
#'
#' @inheritParams bnec
#'
#' @param manec An object of class bayesmanecfit output list
#' as returned by \code{\link{bnec}}.
#' @param model A \code{\link[base]{character}} string indicating
#' which model or class of models to pull out.
#' @param ... Additional arguments to \code{\link{expand_nec}} or
#' \code{\link{expand_manec}}.
#'
#' @return An object of class bayesnecfit.
#'
#' @examples
#' library(bayesnec)
#' data(manec_example)
#' ecx4param <- pull_out(manec_example, model = "ecx4param")
#'
#' @export
pull_out <- function(manec, model, loo_controls, ...) {
  old_method <- attributes(manec$mod_stats$wi)$method
  if (missing(loo_controls)) {
    loo_controls <- list(fitting = list(), weights = list(method = old_method))
  } else {
    fam_tag <- manec$mod_fits[[1]]$fit$family$family
    loo_controls <- validate_loo_controls(loo_controls, fam_tag)
    if (!"method" %in% names(loo_controls$weights)) {
      loo_controls$weights$method <- old_method
    } else {
      message("You have specified a list of arguments in loo_control$weights; ",
              "this will be ignored in pull_out as it is only relevant for",
              "model averaging. See function ?amend instead of that was your",
              "intention.")
      loo_controls$weights$method <- old_method
    }
  }
  existing <- names(manec$mod_fits)
  msets <- names(mod_groups)
  if (any(model %in% msets)) {
    group_mods <- intersect(model, msets)
    model <- union(model, unname(unlist(mod_groups[group_mods])))
    model <- setdiff(model, msets)
  }
  to_go <- intersect(model, existing)
  if (length(to_go) == 0) {
    message("Model(s) ", paste0(model, collapse = ", "),
            " non-existent in current set of models: ",
            paste0(existing, collapse = ", "), ".\n",
            "They may have been removed due to incompatibility with the",
            " modelled response distribution. If needed, add desired model(s)",
            " via function amend (see ?amend).\n Returning original object.")
    return(manec)
  } else if (!all(model %in% existing)) {
    non_existing <- setdiff(model, existing)
    message("Model(s) ", paste0(non_existing, collapse = ", "),
            " non-existent in current set of models: ",
            paste0(existing, collapse = ", "), ".\n",
            "They may have been removed due to incompatibility with the",
            " modelled response distribution. If needed, add desired model(s)",
            " via function amend (see ?amend).")
  }
  if (all(existing %in% to_go)) {
    message("Current model(s) are 100% contained ",
            "within target model(s) to pull out.\n",
            "Returning original object.")
    return(manec)
  } else if (all(!(to_go %in% existing))) {
    message("Target model(s) are 100% contained ",
            "within target model(s) to pull out.\n",
            "Returning original object.")
  }
  mod_fits <- suppressMessages(expand_manec(manec$mod_fits[to_go], ...))
  message("Pulling out model(s): ", paste0(to_go, collapse = ", "))
  if (length(mod_fits) > 1) {
    allot_class(mod_fits, "bayesmanecfit")
  } else {
    mod_fits <- expand_nec(mod_fits[[1]], model = model, ...)
    allot_class(mod_fits, "bayesnecfit")
  }
}
