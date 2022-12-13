#' print.necsummary
#'
#' @param x An object of class \code{\link{necsummary}} as
#' returned by \code{\link{summary.bayesnecfit}}.
#' @param ... Unused.
#'
#' @return A \code{\link[base]{list}} containing a summary of model features
#' and statistics.
#'
#' @export
print.necsummary <- function(x, ...) {
  cat("Object of class bayesnecfit containing the following",
      " non-linear model: ", x$model, "\n\n", sep = "")
  print(x$brmssummary)
  if (x$is_ecx) {
    cat("\nNB: Model ", x$model, " is an ECX model and so ",
        "the NEC estimate is an NSEC surrogate.\n", sep = "")
  }
  if (!is.null(x$ecs)) {
    cat("\n\n")
    for (i in seq_along(x$ecs)) {
      nice_ecx_out(x$ecs[[i]], names(x$ecs)[i])
      if (i < length(x$ecs)) {
        cat("\n")
      }
    }
  }
  cat("\n\nBayesian R2 estimates:\n")
  print_mat(x$bayesr2)
  cat("\n\n")
  invisible(x)
}

#' print.bayesnecfit
#'
#' @param x An object of class \code{\link{bayesnecfit}} as
#' returned by \code{\link{bnec}}.
#' @param ... Further arguments to function summary.
#'
#' @return A \code{\link[base]{list}} containing a summary of the model fit as
#' returned for a \code{\link[brms]{brmsfit}} object.
#'
#' @export
print.bayesnecfit <- function(x, ...) {
  print(summary(x, ...))
}

#' formula.bayesnecfit
#'
#' @param x An object of class \code{\link{bayesnecfit}} as
#' returned by \code{\link{bnec}}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return An object of class \code{\link[stats]{formula}}.
#'
#' @importFrom stats formula
#' @export
formula.bayesnecfit <- function(x, ...) {
  formula(x$fit, ...)
}

#' model.frame.bayesnecfit
#'
#' @param formula An object of class \code{\link{bayesnecfit}} as
#' returned by \code{\link{bnec}}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A \code{\link[base]{data.frame}} containing the data used to fit
#' the model.
#'
#' @importFrom stats model.frame
#' @export
model.frame.bayesnecfit <- function(formula, ...) {
  model.frame(formula$fit, ...)
}
