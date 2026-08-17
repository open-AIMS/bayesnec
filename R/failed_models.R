#' Models that failed to fit
#'
#' Returns the models that \code{\link{bnec}} or \code{\link{amend}} attempted
#' and could not fit, together with the priors and initial values that were
#' used in the attempt.
#'
#' @param object An object of class \code{\link{bayesnecfit}},
#' \code{\link{bayesmanecfit}} or \code{\link{bayesnechurdlefit}} as returned by
#' \code{\link{bnec}}.
#' @param ... Unused.
#'
#' @details A model set fitted with \code{model = "all"} regularly loses a
#' model or two, and the error scrolls past in the middle of a long run. What is
#' needed to diagnose one is the prior and the starting values it was given, and
#' both are constructed inside \code{\link{bnec}} rather than supplied by the
#' user, so re-running the set was previously the only way to see them. They are
#' now kept on the returned object.
#'
#' Each element is named for the model and holds \code{model}, the error
#' \code{message}, the \code{prior} (an object of class
#' \code{\link[brms]{brmsprior}}) and the \code{init} used. \code{init} is a
#' \code{\link[base]{list}} with one element per chain, or the string
#' \code{"random"} where \code{bayesnec}'s search could not find starting values
#' inside the range of the response and left initialisation to Stan.
#'
#' \code{prior} and \code{init} are \code{NULL} where the model failed before
#' they were built -- a formula or data problem rather than a sampling one.
#'
#' \code{\link{pull_out}} carries the record across unchanged, since it subsets
#' a set that has already been fitted rather than refitting it.
#' \code{\link{amend}} instead records only the models that call attempted:
#' a model that failed before may have been dropped now, or is being retried
#' there with different priors.
#'
#' A returned prior is directly usable, so the usual next step is to fit the
#' failed model on its own with an adjusted version of it:
#'
#' \preformatted{
#' f <- failed_models(fit)$nechormepwr
#' f$message
#' f$prior
#' bnec(formula, data = data, model = "nechormepwr", prior = f$prior)
#' }
#'
#' @return An object of class \code{bnecfailures}: a named
#' \code{\link[base]{list}}, empty if every model fitted. For a
#' \code{\link{bayesnechurdlefit}}, a \code{\link[base]{list}} of two such
#' objects, one per component.
#'
#' @seealso \code{\link{bnec}}, \code{\link{amend}}, \code{\link{pull_out}}
#'
#' @examples
#' library(bayesnec)
#' data(manec_example)
#' failed_models(manec_example)
#'
#' @export
failed_models <- function(object, ...) {
  UseMethod("failed_models")
}

#' @noRd
#' @export
failed_models.default <- function(object, ...) {
  stop("failed_models() applies to objects fitted by bnec.", call. = FALSE)
}

#' @noRd
#' @export
failed_models.bnecfit <- function(object, ...) {
  out <- object[["failed_models"]]
  if (is.null(out)) {
    out <- list()
  }
  allot_class(out, "bnecfailures")
}

#' @noRd
#' @export
failed_models.bayesnechurdlefit <- function(object, ...) {
  # Two independent model sets, so two independent sets of failures; a model can
  # fail on one component and fit on the other.
  list(growth = failed_models(object$growth),
       survival = failed_models(object$survival))
}

#' print.bnecfailures
#'
#' @param x An object of class \code{bnecfailures} as returned by
#' \code{\link{failed_models}}.
#' @param ... Unused.
#'
#' @return \code{x}, invisibly.
#'
#' @export
#' @noRd
print.bnecfailures <- function(x, ...) {
  if (length(x) == 0) {
    cat("No models failed to fit.\n")
    return(invisible(x))
  }
  cat(length(x), " model(s) failed to fit:\n\n", sep = "")
  for (i in seq_along(x)) {
    cat("  ", names(x)[i], "\n", sep = "")
    cat("    ", gsub("\n", "\n    ", trimws(x[[i]]$message)), "\n", sep = "")
  }
  cat("\nThe priors and initial values used are retained, e.g.\n")
  cat("  failed_models(fit)$", names(x)[1], "$prior\n", sep = "")
  cat("  failed_models(fit)$", names(x)[1], "$init\n", sep = "")
  invisible(x)
}

#' A condition carrying the priors and initial values of a failed fit
#'
#' Raised by \code{fit_bayesnec} in place of a bare \code{stop()} so that the
#' caller, which catches the error and carries on with the remaining models, can
#' recover what the attempt was given. There is nowhere else to get it from: the
#' prior and the initial values are constructed inside the failed call.
#'
#' @param model A \code{\link[base]{character}} string naming the model.
#' @param message The error message.
#' @param prior The prior used, or \code{NULL}.
#' @param init The initial values used, or \code{NULL}.
#'
#' @return An object of class \code{bnec_fit_failure}, inheriting from
#' \code{error}.
#'
#' @noRd
fit_failure_condition <- function(model, message, prior = NULL, init = NULL) {
  structure(
    class = c("bnec_fit_failure", "error", "condition"),
    list(message = message, call = NULL, model = model,
         prior = prior, init = init)
  )
}

#' Build one entry of a \code{bnecfailures} list
#'
#' @param model A \code{\link[base]{character}} string naming the model.
#' @param cond The condition attached to the \code{try-error}, possibly
#' \code{NULL}.
#'
#' @return A named \code{\link[base]{list}}.
#'
#' @noRd
failure_record <- function(model, cond) {
  if (inherits(cond, "bnec_fit_failure")) {
    list(model = model, message = conditionMessage(cond),
         prior = cond$prior, init = cond$init)
  } else {
    # A failure raised before the priors and inits were built -- a formula or
    # data problem rather than a sampling one. Recorded anyway, because "which
    # models are missing and why" is the first question either way.
    list(model = model,
         message = if (is.null(cond)) {
           "Model failed for an unrecorded reason."
         } else {
           conditionMessage(cond)
         },
         prior = NULL, init = NULL)
  }
}

#' Attach a set of failures to a fitted object
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}.
#' @param failed A named \code{\link[base]{list}} of failure records.
#'
#' @return \code{object}, with a \code{failed_models} element where there were
#' failures.
#'
#' @noRd
attach_failed_models <- function(object, failed) {
  # Added only when non-empty. Appending unconditionally would change names() on
  # every fitted object, which is a gratuitous break for anything indexing one
  # positionally or checking its structure -- the same reasoning as the hurdle
  # element in expand_nec().
  if (length(failed) == 0) {
    return(object)
  }
  cls <- class(object)
  object <- c(unclass(object), list(failed_models = failed))
  allot_class(object, cls)
}
