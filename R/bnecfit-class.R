#' Class \code{bnecfit} of models fitted with function \code{\link{bnec}}
#'
#' This is a wrapper class which will be attached to both
#' \code{\link{bayesnecfit}} and \code{\link{bayesmanecfit}} classes. Useful
#' for methods which might need to take either class as an input
#' simultaneously.
#'
#' @name bnecfit-class
#' @aliases bnecfit
#' @docType class
#'
#' @details See \code{methods(class = "bnecfit")} for an overview of
#' available methods.
#'
#' @seealso
#'   \code{\link{bayesnec}},
#'   \code{\link{bnec}},
#'   \code{\link{bayesnecfit}},
#'   \code{\link{bayesmanecfit}}
#'
NULL

#' Checks if argument is a \code{\link{bnecfit}} object
#'
#' @param x An \R object
#'
#' @noRd
is_bnecfit <- function(x) {
  inherits(x, "bnecfit")
}
