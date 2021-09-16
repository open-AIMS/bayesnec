#' pull_out
#'
#' Subsets model(s) from an existing object of class \code{\link{bayesmanecfit}}
#'
#' @inheritParams bnec
#'
#' @param manec An object of class \code{\link{bayesmanecfit}} as returned by
#' \code{\link{bnec}}.
#' @param model A \code{\link[base]{character}} string indicating
#' which model or suite of models to pull out.
#' @param ... Additional arguments to \code{\link{expand_nec}} or
#' \code{\link{expand_manec}}.
#'
#' @importFrom dplyr %>%
#'
#' @return If \code{model} is a string representing a single model, an object
#' of class \code{\link{bayesnecfit}}; If \code{model} is instead a string
#' depicting a suite of models, and object of class \code{\link{bayesmanecfit}}.
#'
#' @seealso \code{\link{bnec}}, \code{\link{models}}.
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' data(manec_example)
#' nec4param <- pull_out(manec_example, model = "nec4param")
#' # use "ecx" to get all ECx-containing models
#' # (only one ["ecx4param"] in this minimal example)
#' ecx_models <- pull_out(manec_example, model = "ecx")
#' }
#'
#' @export
pull_out <- function(manec, model, loo_controls, ...) {
  if (length(model) > 1) {
    stop("Argument model can only take one value. See ?pull_out and ?models.")
  }
  old_method <- attributes(manec$mod_stats$wi)$method
  if (missing(loo_controls)) {
    loo_controls <- list(fitting = list(), weights = list(method = old_method))
  } else {
    fam_tag <- manec$mod_fits[[1]]$fit$family$family
    loo_controls <- validate_loo_controls(loo_controls, fam_tag)
    if (length(loo_controls$weights) > 0) {
      message("You have specified a list of arguments in loo_control$weights; ",
              "this is ignored in pull_out. Use function ?amend first if your",
              "intention is to modify the model averaging specs.")
    }
    loo_controls$weights <- list(method = old_method)
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
    return(manec)
  }
  formulas <- lapply(manec$mod_fits[to_go], extract_formula)
  mod_fits <- expand_manec(manec$mod_fits[to_go], formula = formulas,
                           loo_controls = loo_controls, ...) %>%
    suppressMessages %>%
    suppressWarnings
  message("Pulling out model(s): ", paste0(to_go, collapse = ", "))
  if (length(mod_fits) > 1) {
    allot_class(mod_fits, c("bayesmanecfit", "bnecfit"))
  } else {
    mod_fits <- expand_nec(mod_fits[[1]], model = to_go,
                           formula = mod_fits[[1]]$bayesnecformula,
                           loo_controls = loo_controls, ...) %>%
    suppressMessages %>%
    suppressWarnings
    allot_class(mod_fits, c("bayesnecfit", "bnecfit"))
  }
}
