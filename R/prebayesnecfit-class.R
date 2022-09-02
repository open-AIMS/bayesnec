#' Class \code{prebayesnecfit} of models fitted with the \pkg{brms} package
#'
#' This is an intermediate class that was created to make both
#' \code{\link{bayesnecfit}} and \code{\link{bayesmanecfit}} objects lighter
#' to handle. It contains the original \code{\link[brms]{brmsfit}}
#' fitted object, name of non-linear model that was fitted, the list of
#' initialisation values applied, and the validated
#' \code{\link{bayesnecformula}}.
#'
#' @name prebayesnecfit-class
#' @aliases prebayesnecfit
#' @docType class
#'
#' @details See \code{methods(class = "prebayesnecfit")} for an overview of
#' available methods.
#'
#' @slot fit The fitted Bayesian model of class \code{\link[brms]{brmsfit}}.
#' @slot model A \code{\link[base]{character}} string indicating the name of
#' the fitted model.
#' @slot init A \code{\link[base]{list}} containing the initialisation values
#' for to fit the model.
#' @slot bayesnecformula An object of class \code{\link{bayesnecformula}} and
#' \code{\link[stats]{formula}}.
#'
#' @seealso
#'   \code{\link{bayesnec}},
#'   \code{\link{bnec}},
#'   \code{\link{bayesnecfit}},
#'   \code{\link{bayesmanecfit}},
#'   \code{\link{bayesnecformula}}
#'
NULL

#' Checks if argument is a \code{\link{prebayesnecfit}} object
#'
#' @param x An \R object
#' 
#' @return A \code{\link[base]{logical}} vector.
#'
#' @noRd
is_prebayesnecfit <- function(x) {
  inherits(x, "prebayesnecfit")
}


#' @param x An \R object
#' 
#' @return A \code{\link[base]{logical}} vector.
#'
#' @noRd
recover_prebayesnecfit <- function(x) {
  if (is_bayesnecfit(x)) {
    out <- allot_class(x[c("fit", "model", "init", "bayesnecformula")],
                       "prebayesnecfit")
    out <- list(out = out)
    names(out) <- out$out$model
    out
  } else if (is_bayesmanecfit(x)) {
    for (i in seq_along(x$mod_fits)) {
      x$mod_fits[[i]] <- allot_class(x$mod_fits[[i]], "prebayesnecfit")
    }
    x$mod_fits
  } else {
    stop("Objects must be either of class bayesnecfit or bayesmanecfit.")
  }
}
