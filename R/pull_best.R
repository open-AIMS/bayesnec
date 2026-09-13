#' The highest-weighted model of a fitted set
#'
#' Returns the candidate holding the most model weight in an object of class
#' \code{\link{bayesmanecfit}}, as an object of class \code{\link{bayesnecfit}}.
#' An object that is already a \code{\link{bayesnecfit}} is returned unchanged,
#' so the caller does not have to test the class first.
#'
#' @param object An object of class \code{\link{bayesnecfit}},
#' \code{\link{bayesmanecfit}} or \code{\link{bayesnechurdlefit}} as returned by
#' \code{\link{bnec}}.
#' @param ... Additional arguments passed to \code{\link{pull_out}}, and through
#' it to \code{\link{expand_nec}}.
#'
#' @details The highest-weighted candidate contributes most to the
#' model-averaged estimate, so it is the candidate whose fit is worth inspecting
#' with \code{\link{pp_check}} and \code{\link{check_fit}}. Selecting it by hand
#' requires two things that are not obvious. The weights are held in
#' \code{mod_stats} and \code{\link{pull_out}} takes a model name, so the name
#' has to be found before it can be passed; and \code{mod_stats} exists on a
#' \code{\link{bayesmanecfit}} alone, so the same code applied to a set that
#' \code{\link{screen_models}} has reduced to one equation --- which is returned
#' as a \code{\link{bayesnecfit}} --- reads a \code{NULL}. Both cases reach the
#' same result here.
#'
#' The weight selected on is reported along with the number of candidates it was
#' selected from, since the two together are what says whether the selection
#' means anything. The highest weight of a flat set of ten candidates is little
#' more than the equal share of 0.1, and one such candidate describes the
#' model-averaged estimate hardly at all. No warning is raised against a
#' threshold, because what counts as a small weight depends on the size of the
#' set, and a cut placed anywhere would be either noise on a set of two or false
#' comfort on a set of twenty. \code{\link{summary}} reports the whole weights
#' table, which is what a judgement of a marginal case needs.
#'
#' Where two or more candidates hold exactly the same highest weight, the first
#' in the order of the set is returned and the tie is reported. Only exact ties
#' are detected; two weights differing in the last decimal place are not a tie
#' and the larger is taken.
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
#' with each component reduced to its own highest-weighted model.
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
  # Silent, and not a usage error. The pass-through is the reason the function
  # exists: screen_models() returns a bayesnecfit whenever the screen leaves one
  # equation, so a caller that had to test the class would still be writing the
  # block this replaces.
  object
}

#' @noRd
#' @export
pull_best.bayesmanecfit <- function(object, ...) {
  model <- best_weighted_model(object$mod_stats)
  # Delegated rather than reimplemented: pull_out() rebuilds the object's
  # weighted quantities and carries the record of failed models across, and a
  # direct expand_nec() call here would have to repeat both. It returns a
  # bayesnecfit for every set reachable through the public API, since bnec(),
  # amend(), update() and screen_models() each collapse a set reduced to one
  # model to that class rather than leaving a one-model bayesmanecfit.
  pull_out(object, model = model, ...)
}

#' @noRd
#' @export
pull_best.bayesnechurdlefit <- function(object, ...) {
  hurdle_rewrap(object, pull_best(object$growth, ...),
                pull_best(object$survival, ...))
}

#' Name the highest-weighted model of a weights table
#'
#' @param mod_stats The \code{mod_stats} element of a
#' \code{\link{bayesmanecfit}}.
#'
#' @details Read from the \code{model} column rather than from
#' \code{rownames()}. Both name the models of a set built by
#' \code{expand_manec()}, but the column is the documented one, and row names
#' are lost by any subsetting that resets them.
#'
#' @return A \code{\link[base]{character}} string naming one model.
#'
#' @noRd
best_weighted_model <- function(mod_stats) {
  if (is.null(mod_stats) || nrow(mod_stats) == 0) {
    stop("This object holds no model weights, so there is no highest-weighted",
         " model to return.", call. = FALSE)
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
