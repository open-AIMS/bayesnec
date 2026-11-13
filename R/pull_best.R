#' The highest-weighted model of a fitted set
#'
#' Returns the candidate holding the most model weight in an object of class
#' \code{\link{bayesmanecfit}}, as an object of class \code{\link{bayesnecfit}}.
#' An object that is already a \code{\link{bayesnecfit}} is returned unchanged,
#' so the caller does not have to test the class first.
#'
#' @param object An object of class \code{\link{bayesnecfit}},
#' \code{\link{bayesmanecfit}} or \code{\link{bayesnechurdlefit}} as returned by
#' \code{\link{bnec}}, or of class \code{\link{bayesnecjointfit}} as returned
#' by \code{\link{bnec_joint}}.
#' @param ... Not used. Any argument supplied here is refused; see Details.
#'
#' @details The highest-weighted candidate contributes most to the
#' model-averaged estimate, so its fit is the one normally inspected with
#' \code{\link{pp_check}} and \code{\link{check_fit}}.
#'
#' The weight selected on is reported with the number of candidates it was
#' selected from: the highest weight of a flat set of ten candidates is little
#' more than the equal share of 0.1. No threshold is applied;
#' \code{\link{summary}} prints the whole weights table. Where two or more
#' candidates hold exactly the same highest weight, the first in the order of
#' the set is returned and the tie is reported.
#'
#' The object is the only argument, and any other is refused. To rebuild a fit
#' at a different \code{x_range}, \code{resolution}, \code{sig_val} or
#' \code{loo_controls}, use \code{\link{pull_out}} with a model name, or
#' \code{\link{amend}}. The selected equation is reported for a
#' \code{\link{bayesnecfit}} as well; \code{suppressMessages} silences it.
#'
#' A \code{\link{bayesnechurdlefit}} is handled one component at a time and
#' rewrapped, as \code{\link{screen_models}} and \code{\link{amend}} already do
#' for that class. The two components may select different equations: the
#' survival component is 0-1 bounded and the growth component need not be, so
#' they do not hold the same candidates, let alone weight them alike.
#'
#' @return For a \code{\link{bayesmanecfit}}, an object of class
#' \code{\link{bayesnecfit}}. For a \code{\link{bayesnecfit}}, \code{object}
#' unchanged. For a \code{\link{bayesnechurdlefit}}, an object of the same class
#' with each component reduced to its own highest-weighted model. For a
#' \code{\link{bayesnecjointfit}}, \code{object} unchanged, because each level
#' already has a single equation, the one its own model weights favoured
#' when \code{\link{bnec_joint}} chose it.
#'
#' @seealso \code{\link{pull_out}}, \code{\link{screen_models}},
#' \code{\link{check_fit}}, \code{\link{summary}}
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' data(manec_example)
#' best <- pull_best(manec_example)
#' check_fit(best)
#' # a bayesnecfit is returned unchanged, so this is safe on either class
#' pull_best(best)
#' }
#'
#' @export
pull_best <- function(object, ...) {
  # Refuse `...` in the generic, before dispatch, so that the refusal cannot
  # depend on the class or follow the selection report. A `model` argument
  # would otherwise fail in pull_out() as "matched by multiple actual
  # arguments".
  if (...length() > 0) {
    supplied <- ...names()
    supplied <- supplied[nzchar(supplied)]
    stop("pull_best() takes the object and nothing else",
         if (length(supplied) > 0) {
           paste0(", so ", paste0("`", supplied, "`", collapse = ", "),
                  " cannot be passed here")
         },
         ". The model is selected by weight, so name one with ?pull_out",
         " instead; and rebuild a fit at a different x_range, resolution,",
         " sig_val or loo_controls with ?pull_out or ?amend.", call. = FALSE)
  }
  UseMethod("pull_best")
}

#' @noRd
#' @export
pull_best.default <- function(object, ...) {
  stop("pull_best() applies to an object of class bayesnecfit, bayesmanecfit",
       " or bayesnechurdlefit, as returned by bnec(). A bayesnecgroupfit holds",
       " one such fit per level in `fits`; select on each of those.",
       call. = FALSE)
}

#' @noRd
#' @export
pull_best.bayesnecfit <- function(object, ...) {
  # Not a usage error: screen_models() returns a bayesnecfit when one equation
  # is left, and this saves the caller testing the class. Report it anyway, so
  # that the record of the selection does not depend on the branch.
  message("This fit holds the single model ", object$model,
          "; returning it unchanged.")
  object
}

#' @noRd
#' @export
pull_best.bayesnecjointfit <- function(object, ...) {
  models <- vapply(object$levels, joint_level_model, character(1),
                   object = object, USE.NAMES = FALSE)
  message("Each level of this joint refit has one equation (",
          paste0(object$levels, " = ", models, collapse = ", "),
          "); returning it unchanged.")
  object
}

#' @noRd
#' @export
pull_best.bayesmanecfit <- function(object, ...) {
  model <- best_weighted_model(object$mod_stats)
  # Delegate to pull_out(), which rebuilds the weighted quantities and copies
  # the record of failed models. It returns a bayesnecfit for every set the
  # public API can produce. `...` is empty here: the generic refuses it.
  pull_out(object, model = model)
}

#' @noRd
#' @export
pull_best.bayesnechurdlefit <- function(object, ...) {
  # Label each component's report. The components hold different candidate
  # sets, so they often select different equations.
  message("Growth component:")
  growth <- pull_best(object$growth)
  message("Survival component:")
  survival <- pull_best(object$survival)
  hurdle_rewrap(object, growth, survival)
}

#' Name the highest-weighted model of a weights table
#'
#' @param mod_stats The \code{mod_stats} element of a
#' \code{\link{bayesmanecfit}}.
#'
#' @details Reads the \code{model} column, not \code{rownames()}: only the
#' column is set by name in \code{expand_manec()}. The row names come from the
#' dispersion matrix at \code{cbind()}, and would be lost without warning if
#' that step changed.
#'
#' @return A \code{\link[base]{character}} string naming one model.
#'
#' @noRd
best_weighted_model <- function(mod_stats) {
  # Unreachable through the public API. These refusals stop which.max() on an
  # all-NA vector, or a missing `model` column, from failing several frames
  # away in pull_out().
  if (is.null(mod_stats) || is.null(mod_stats$model) ||
        is.null(mod_stats$wi) || nrow(mod_stats) == 0) {
    stop("This object holds no table of model weights -- a `model` column and",
         " a `wi` column, one row per model -- so there is no",
         " highest-weighted model to return.", call. = FALSE)
  }
  wi <- as.numeric(mod_stats$wi)
  usable <- is.finite(wi)
  if (!any(usable)) {
    stop("None of the ", length(wi), " model weights is a finite number, so",
         " there is no highest-weighted model to return. Refit the weights",
         " with ?amend, or read summary(x)$mod_weights.", call. = FALSE)
  }
  models <- as.character(mod_stats$model)
  top <- which(usable & wi == max(wi[usable]))
  if (length(top) > 1) {
    message("Models ", paste0(models[top], collapse = ", "), " hold the same",
            " weight of ", signif(wi[top[1]], 3), ". Returning the first of",
            " them, ", models[top[1]], "; the choice between them is",
            " arbitrary.")
  }
  message("Highest-weighted model is ", models[top[1]], ", holding ",
          signif(wi[top[1]], 3), " of the weight of ", length(wi),
          " candidate models.")
  models[top[1]]
}
