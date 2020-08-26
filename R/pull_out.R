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
#' @export
pull_out <- function(manec, model, ...) {
  if (model == "all") {
    stop("\"all\" is not an allowed option for ",
         "argument model in function pull_out")
  }
  existing <- names(manec$mod_fits)
  msets <- names(mod_groups)
  if (any(model %in% msets)) {
    group_mods <- intersect(model, msets)
    model <- union(model, unname(unlist(mod_groups[group_mods])))
    model <- intersect(model, existing)
  }
  pass <- suppressMessages(handle_set(existing, drop = model))
  if (is.logical(pass)) {
    stop("Model \"", model, "\" non-existent in current set of models: ",
         paste0(existing, collapse = ", "))
  }
  mod_fits <- expand_manec(manec$mod_fits[model], ...)
  if (!inherits(mod_fits, "prebayesnecfit")) {
    allot_class(mod_fits, "bayesmanecfit")
  } else {
    mod_fits <- expand_nec(mod_fits, ...)
    allot_class(mod_fits, "bayesnecfit")
  }
}
