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
#' \code{\link[stats]{formula}}: the formula the equation was fitted with. A
#' \code{(par | group)} term naming a parameter the equation does not have is
#' left out of it, as it is left out of the fit.
#' @slot requested_formula The formula as written, with the equation named in
#' \code{crf()}. Present only where a \code{(par | group)} term was left out of
#' \code{bayesnecformula}; \code{\link{amend}} fits a new equation from it.
#' @slot retained_data An optional \code{\link[base]{data.frame}} containing
#' columns retained by \code{\link{bnec}} for plotting. This element is present
#' on a recovered object when the fitted object contained retained columns.
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
    keep <- c("fit", "model", "init", "bayesnecformula")
    # Kept where present, as retained_data is, so that a fit recovered for c()
    # or amend() still holds the formula as written. Absent on a fit whose terms
    # were all kept, which therefore recovers to the elements it always had.
    if (!is.null(x[["requested_formula"]])) {
      keep <- c(keep, "requested_formula")
    }
    if (!is.null(x[["retained_data"]])) {
      keep <- c(keep, "retained_data")
    }
    out <- allot_class(x[keep],
                       "prebayesnecfit")
    out <- list(out = out)
    names(out) <- out$out$model
    out
  } else if (is_bayesmanecfit(x)) {
    retained_data <- x[["retained_data"]]
    for (i in seq_along(x$mod_fits)) {
      x$mod_fits[[i]] <- allot_class(x$mod_fits[[i]], "prebayesnecfit")
      if (!is.null(retained_data)) {
        x$mod_fits[[i]]$retained_data <- retained_data
      }
    }
    x$mod_fits
  } else {
    stop("Objects must be either of class bayesnecfit or bayesmanecfit.")
  }
}
