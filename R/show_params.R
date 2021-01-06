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
  if (class(model) != "character") {
    stop("Not a valid model name")
  }
  display <- function(x) {
    get(paste0("bf_", x))
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
  } else if (length(model) == 1) {
    display(model)
  }
}
