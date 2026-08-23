#' Prints a summary for objects fitted by \code{\link{bnec}}
#'
#' Prints a summary for objects fitted by \code{\link{bnec}}.
#' \code{x} should be of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}.
#'
#' @name print
#' @order 1
#'
#' @param x An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}.
#' @param ... Unused.
#'
#' @return A summary print of the fitted model as returned for a
#' \code{\link[brms]{brmsfit}} object.
#'
#' @examples
#' \donttest{
#' library(bayesnec)
#' print(manec_example)
#' nec4param <- pull_out(manec_example, "nec4param")
#' print(nec4param)
#' }
NULL

#' @rdname print
#' @order 2
#'
#' @method print bayesnecfit
#'
#' @inherit print description return examples
#'
#' @export
print.bayesnecfit <- function(x, ...) {
  print(summary(x))
}

#' @rdname print
#' @order 3
#'
#' @method print bayesmanecfit
#'
#' @inherit print description return examples
#'
#' @export
print.bayesmanecfit <- function(x, ...) {
  print(summary(x))
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
#' @noRd
print.necsummary <- function(x, ...) {
  cat("Object of class bayesnecfit containing the", x$model,
      "model\n\n", sep = " ")
  print(x$brmssummary)
  cat("\n\n")
  if (x$is_ecx) {
    cat("NB: Model", x$model, "is an ECx model, thus",
        "the NEC estimate is an\n", "   NSEC surrogate.\n", sep = " ")
  } else if (identical(x$ne_type, "N(S)EC")) {
    # Reachable only for a two-block fit whose blocks carry different equation
    # types, e.g. a threshold response block with a smooth survival block.
    cat("NB: The two blocks of this fit use a threshold model on one and a\n",
        "   smooth model on the other, so the combined estimate is a\n",
        "   combination of a NEC and a NSEC.\n", sep = "")
  }
  print_mat(x$nec_vals)
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
  print_failed_models(x$failed_models)
  invisible(x)
}

#' print.manecsummary
#'
#' @param x An object of class \code{\link{manecsummary}} as
#' returned by \code{\link{summary.bayesmanecfit}}.
#' @param ... Unused.
#'
#' @return A list containing a summary of model features and statistics.
#'
#' @export
#' @noRd
print.manecsummary <- function(x, ...) {
  cat("Object of class bayesmanecfit\n")
  cat("\n")
  cat(x$family$family, "\n")
  cat(x$family$links, "\n")
  cat("\n")
  cat("Number of posterior draws per model: ", x$sample_size)
  cat("\n\n")
  cat("Model weights (Method: ", x$mod_weights_method, "):\n", sep = "")
  print_mat(x$mod_weights)
  cat("\n\n")
  neclab <- rownames(x$nec_vals)
  cat("Summary of weighted", neclab, "posterior estimates:\n", sep = " ")
  if (neclab == "N(S)EC") {
    cat("NB: Model set contains a combination of ECx and NEC\n",
        "    models, and is therefore a model averaged\n",
        "    combination of NEC and NSEC estimates.\n", sep = "")
  }
  print_mat(x$nec_vals)
  cat("\n\n")
  if (!is.null(x$ecs)) {
    for (i in seq_along(x$ecs)) {
      nice_ecx_out(x$ecs[[i]], names(x$ecs)[i])
      cat("\n")
    }
    cat("\n")
  }
  cat("Bayesian R2 estimates:\n")
  print_mat(x$bayesr2)
  cat("\n\n")
  print_failed_models(x$failed_models)
  with_issues <- names(x$rhat_issues[unlist(x$rhat_issues)])
  if (length(with_issues) > 0) {
      warning("The following model had Rhats > ",
              if (is.null(x$rhat_cutoff)) 1.01 else x$rhat_cutoff,
              " (no convergence):\n",
              paste0("  -  ", with_issues, collapse = "\n"), "\n",
              "Consider dropping them (see ?amend)\n", sep = "")
  }
  invisible(x)
}

#' One line in a summary naming the models that did not fit
#'
#' Kept short deliberately: the summary says how many failed and which, and
#' \code{\link{failed_models}} holds the priors and initial values for anyone who
#' wants to diagnose one.
#'
#' @param failed An object of class \code{bnecfailures}, or a named
#' \code{\link[base]{list}} of them as \code{\link{failed_models}} returns for a
#' \code{\link{bayesnechurdlefit}}. Possibly \code{NULL} for a summary of an
#' object fitted before this was recorded.
#'
#' @return \code{invisible(NULL)}, called for its side effect.
#'
#' @noRd
print_failed_models <- function(failed) {
  if (is.null(failed) || length(failed) == 0) {
    return(invisible(NULL))
  }
  # A hurdle fit is two independent model sets, so failed_models() returns one
  # bnecfailures per component and a model can fail on one and not the other.
  # Normalising to a list of sets here keeps all three summary print methods
  # identical at the call site.
  sets <- if (inherits(failed, "bnecfailures")) list(failed) else failed
  sets <- sets[lengths(sets) > 0]
  if (length(sets) == 0) {
    return(invisible(NULL))
  }
  for (i in seq_along(sets)) {
    label <- if (is.null(names(sets))) "" else paste0(names(sets)[i], ": ")
    cat(label, length(sets[[i]]), " model(s) failed to fit: ",
        paste0(names(sets[[i]]), collapse = ", "), "\n", sep = "")
  }
  cat("See ?failed_models for the priors and initial values used.\n\n")
  invisible(NULL)
}
