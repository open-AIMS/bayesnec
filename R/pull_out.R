#' pull_out
#'
#' Pulls a single model from an existing bayesmanecfit object,
#' and converts into a bayesnecfit object.
#'
#' @param manec An object of class bayesmanecfit output list
#' as returned by \code{\link{bnec}}.
#' @param model A \code{\link[base]{character}} string indicating
#' which model or class of models to pull out.
#' @param ... Additional arguments to \code{\link{expand_nec}}.
#'
#' @return An object of class bayesnecfit.
#'
#' @examples
#' library(bayesnec)
#' data(manec_example)
#' ecx4param <- pull_out(manec_example, model = "ecx4param")
#'
#' @export
pull_out <- function(manec, model, ...) {
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
  if (!inherits(mod_fits, "prebayesnecfit")) {
    allot_class(mod_fits, "bayesmanecfit")
  } else {
    mod_fits <- expand_nec(mod_fits, ...)
    allot_class(mod_fits, "bayesnecfit")
  }
}
