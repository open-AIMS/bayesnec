#' @describeIn predict Generates predictions for \code{\link{bayesnecfit}}
#' objects fitted by \code{\link{bnec}}.
#' @order 2
#'
#' @inheritParams predict.bnecfit
#'
#' @method predict bayesnecfit
#' @inherit predict.bnecfit description return examples
#' @importFrom stats predict
#' @export
predict.bayesnecfit <- function(object, ...) {
  predict(pull_brmsfit(object), ...)
}

#' @describeIn posterior_predict Generates posterior predictions for
#' \code{\link{bayesnecfit}} objects fitted by \code{\link{bnec}}.
#' @order 2
#'
#' @inheritParams posterior_predict.bnecfit
#'
#' @method posterior_predict bayesnecfit
#' @inherit posterior_predict.bnecfit description return examples
#' @importFrom brms posterior_predict
#' @export
posterior_predict.bayesnecfit <- function(object, ...) {
  posterior_predict(pull_brmsfit(object), ...)
}

#' @describeIn fitted Generates mean posterior predictions for
#' \code{\link{bayesnecfit}} objects fitted by \code{\link{bnec}}.
#' @order 2
#'
#' @inheritParams fitted.bnecfit
#' @param summary Should summary statistics be returned
#'  instead of the raw values? Default is \code{TRUE}.
#' @param robust If \code{FALSE} (the default) the mean is used as
#'  the measure of central tendency and the standard deviation as
#'  the measure of variability. If \code{TRUE}, the median and the
#'  median absolute deviation (MAD) are applied instead.
#'  Only used if \code{summary} is \code{TRUE}.
#' @param probs  The percentiles to be computed by the \code{quantile}
#'  function. Only used if \code{summary} is \code{TRUE}.
#'
#' @method fitted bayesnecfit
#' @inherit fitted.bnecfit description return examples
#' @importFrom stats fitted
#' @importFrom brms posterior_summary
#' @export
fitted.bayesnecfit <- function(object, summary = TRUE, robust = FALSE,
                               probs = c(0.025, 0.975), ...) {
  fitted(pull_brmsfit(object), ...)
}

#' @describeIn posterior_epred Generates mean posterior predictions for
#' \code{\link{bayesnecfit}} objects fitted by \code{\link{bnec}}.
#' @order 2
#'
#' @inheritParams posterior_epred.bnecfit
#'
#' @method posterior_epred bayesnecfit
#' @inherit posterior_epred.bnecfit description return examples
#' @importFrom brms posterior_epred
#' @export
posterior_epred.bayesnecfit <- function(object, ...) {
  posterior_epred(pull_brmsfit(object), ...)
}

#' rhat.bayesnecfit
#'
#' @param object An object of class \code{\link{bayesnecfit}} as returned
#' by \code{\link{bnec}}.
#' @param ... Unused.
#'
#' @return A named \code{\link[base]{numeric}} vector containing Rhat values as
#' returned for a \code{\link[brms]{brmsfit}} object for each of the estimated
#' parameters.
#'
#' @importFrom brms rhat
#'
#' @export
rhat.bayesnecfit <- function(object, ...) {
  rhat(object$fit)
}

#' summary.bayesnecfit
#'
#' @param object An object of class \code{\link{bayesnecfit}} as returned
#' by \code{\link{bnec}}.
#' @param ecx Should summary ECx values be calculated? Defaults to FALSE.
#' @param ecx_vals ECx targets (between 1 and 99). Only relevant if ecx = TRUE.
#' If no value is specified by the user, returns calculations for EC10, EC50,
#' and EC90.
#' @param ... Unused.
#'
#' @return A summary of the fitted model as returned for a
#' \code{\link[brms]{brmsfit}} object.
#'
#' @importFrom brms bayes_R2
#' @importFrom chk chk_numeric chk_lgl
#'
#' @export
summary.bayesnecfit <- function(object, ..., ecx = FALSE,
                                ecx_vals = c(10, 50, 90)) {
  chk_lgl(ecx)
  chk_numeric(ecx_vals)    
  x <- object
  ecs <- NULL
  if (ecx) {
    message("ECX calculation takes a few seconds per model, calculating...\n")
    ecs <- list()
    for (i in seq_along(ecx_vals)) {
      ecs[[i]] <- ecx(x, ecx_val = ecx_vals[i])
    }
    names(ecs) <- paste0("ECx (", ecx_vals, "%) estimate:")
  }
  out <- list(
    brmssummary = cleaned_brms_summary(x$fit),
    model = x$model,
    is_ecx = x$model %in% mod_groups$ecx,
    ecs = ecs,
    bayesr2 = bayes_R2(x$fit)
  )
  allot_class(out, "necsummary")
}

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
