#' Concatenate multiple \code{\link{bnecfit}} objects into one single
#' \code{\link{bayesmanecfit}} object containing Bayesian model averaging
#' statistics.
#'
#' @param x An object of class \code{\link{bnecfit}}.
#' @param ... Additional objects of class \code{\link{bnecfit}}.
#'
#' @return An object of class \code{\link{bayesmanecfit}}.
#'
#' @export
c.bnecfit <- function(x, ...) {
  dots <- list(...)
  if (!all(c(is_bnecfit(x), sapply(dots, is_bnecfit)))) {
    stop("All objects must be an object fitted by bnec.")
  } else {
    mod_fits <- recover_prebayesnecfit(x)
    for (i in seq_along(dots)) {
      mod_fits <- c(mod_fits, recover_prebayesnecfit(dots[[i]]))
    }
  }
  mod_fits <- mod_fits[!duplicated(names(mod_fits))]
  formulas <- lapply(mod_fits, extract_formula)
  out <- expand_manec(mod_fits, formulas)
  allot_class(out, c("bayesmanecfit", "bnecfit"))
}

#' "Add" multiple \code{\link{bnecfit}} objects into one single
#' \code{\link{bayesmanecfit}} object containing Bayesian model averaging
#' statistics.
#'
#' @param e1 An object of class \code{\link{bnecfit}}.
#' @param e2 An object of class \code{\link{bnecfit}}.
#'
#' @return An object of class \code{\link{bayesmanecfit}}.
#'
#' @export
`+.bnecfit` <- function(e1, e2) {
  if (is.null(e2)) {
    return(e1)
  }
  if (!all(sapply(list(e1, e2), is_bnecfit))) {
    stop("Cannot add \"", class(e2)[1], "\" objects.")
  }
  c(e1, e2)
}
