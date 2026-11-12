#' Models that failed to fit
#'
#' Returns the models that \code{\link{bnec}} or \code{\link{amend}} attempted
#' and could not fit, together with the priors and initial values that were
#' used in the attempt.
#'
#' @param object An object of class \code{\link{bayesnecfit}},
#' \code{\link{bayesmanecfit}}, \code{\link{bayesnechurdlefit}} or
#' \code{\link{bayesnecgroupfit}}, as returned by \code{\link{bnec}},
#' \code{\link{bnec_hurdle}} or \code{\link{bnec_group}}.
#' @param ... Unused.
#'
#' @details A model set fitted with many equations can lose one or two, and the
#' error scrolls past in the middle of a long run. Diagnosing a failure needs
#' the prior and the starting values the model was given, which
#' \code{\link{bnec}} constructs rather than the user, so both are kept on the
#' returned object.
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
#' \code{\link[base]{c}} and \code{+} keep the failures of every object they
#' combine, except for an equation that another of those objects fitted and
#' the combined set therefore holds.
#' \code{\link{amend}} instead records only the models that call attempted:
#' a model that failed before may have been dropped now, or is being retried
#' there with different priors.
#'
#' A returned prior is directly usable, so the usual next step is to fit the
#' failed model on its own with an adjusted version of it, naming the equation
#' in \code{crf()}:
#'
#' \preformatted{
#' f <- failed_models(fit)$nechormepwr
#' f$message
#' f$prior
#' bnec(y ~ crf(x, "nechormepwr"), data = data, prior = f$prior)
#' }
#'
#' @return An object of class \code{bnecfailures}: a named
#' \code{\link[base]{list}}, empty if every model fitted. For a
#' \code{\link{bayesnechurdlefit}}, a \code{\link[base]{list}} of two such
#' objects, one per component. For a \code{\link{bayesnecgroupfit}}, a
#' \code{\link[base]{list}} of such objects named by level, since each level
#' is fitted by its own call to \code{\link{bnec}}.
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
  list(growth = failure_path(failed_models(object$growth), "$growth"),
       survival = failure_path(failed_models(object$survival), "$survival"))
}

#' @noRd
#' @export
failed_models.bayesnecgroupfit <- function(object, ...) {
  # Read each level's fit: the group object holds no record of its own.
  out <- group_lapply(object, failed_models)
  for (level in names(out)) {
    out[[level]] <- failure_path(out[[level]],
                                 paste0("[[\"", level, "\"]]"))
  }
  out
}

#' Record where a set of failures sits inside the result
#'
#' @param x An object of class \code{bnecfailures}.
#' @param path The extraction that reaches \code{x} from the value of
#' \code{failed_models(fit)}, such as \code{"$growth"}.
#'
#' @return \code{x}, with attribute \code{"path"}, which the print method uses
#' in its example call.
#'
#' @noRd
failure_path <- function(x, path) {
  attr(x, "path") <- path
  x
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
  path <- attr(x, "path")
  cat("\nThe priors and initial values used are retained, e.g.\n")
  cat("  failed_models(fit)", path, "$", names(x)[1], "$prior\n", sep = "")
  cat("  failed_models(fit)", path, "$", names(x)[1], "$init\n", sep = "")
  invisible(x)
}

#' A condition carrying the priors and initial values of a failed fit
#'
#' Raised by \code{fit_bayesnec} in place of \code{stop()}, so that a caller
#' that catches the error and moves on can recover the prior and initial
#' values, which were built inside the failed call.
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
    # A failure before the priors and inits were built, from a formula or data
    # problem. It is still recorded.
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
#' failures, and with every attribute it had.
#'
#' @noRd
attach_failed_models <- function(object, failed) {
  # Add the element only when non-empty, so that names() of a fit with no
  # failures is unchanged.
  if (length(failed) == 0) {
    return(object)
  }
  # Assign the element in place: c(unclass(object), ...) drops every attribute.
  object[["failed_models"]] <- failed
  object
}

#' The failures of the objects \code{c()} combines
#'
#' @param inputs A \code{\link[base]{list}} of fitted objects.
#' @param held The equations the combined set holds.
#'
#' @return A named \code{\link[base]{list}} of failure records, the first for
#' each equation, leaving out any equation the combined set holds.
#'
#' @noRd
joined_failures <- function(inputs, held) {
  failed <- do.call(c, lapply(inputs, function(x) x[["failed_models"]]))
  failed[!duplicated(names(failed)) & !names(failed) %in% held]
}
