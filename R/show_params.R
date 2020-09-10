#' show_params
#'
#' Displays non-linear equation and parameter names
#'
#' @inheritParams bnec
#'
#' @return An object of class
#' \code{\link[brms]{brmsformula}}.
#'
#' @examples
#' library(bayesnec)
#' # default to all models (i.e. model = "all")
#' show_params()
#' # single model
#' show_params(model = "nec3param")
#' # group of models
#' show_params(model = c("nec3param", "ecx"))
#' 
#' @export
show_params <- function(model = "all") {
  display <- function(x) {
    get(paste("bf", x, "deflt", sep = "_"))
  }
  msets <- names(mod_groups)
  if (any(model %in% msets)) {
      group_mods <- intersect(model, msets)
      model <- union(model, unname(unlist(mod_groups[group_mods])))
      model <- setdiff(model, msets)
      names(model) <- model
  }
  if (length(model) > 1) {
    lapply(model, display)
  } else {
    display(model)
  }
}
