#' @noRd
capture_family <- function(manec) {
  UseMethod("capture_family")
}

#' @noRd
#' @importFrom utils capture.output
capture_family.bayesmanecfit <- function(manec) {
  x <- manec$mod_fits[[1]]$fit
  out <- capture.output(print(summary(x)))
  list(family = grep("^ Family:", out, value = TRUE),
       links = grep("^  Links:", out, value = TRUE))
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
  cat("Summary of weighted NEC posterior estimates:\n")
  if (!is.null(x$ecx_mods)) {
    cat("NB: Model set contains the ECX models: ",
        paste0(x$ecx_mods, collapse = ";"),
        "; weighted NEC estimates include NSEC surrogates for NEC\n", sep = "")
  }
  print_mat(x$nec_vals)
  cat("\n\n")
  if (!is.null(x$ecs)) {
    for (i in seq_along(x$ecs)) {
      nice_ecx_out(x$ecs[[i]], names(x$ecs)[i])
      "\n\n"
    }
  }
  cat("Bayesian R2 estimates:\n")
  print_mat(x$bayesr2)
  cat("\n\n")
  with_issues <- names(x$rhat_issues[unlist(x$rhat_issues)])
  if (length(with_issues) > 0) {
      warning("The following model had Rhats > 1.05 (no convergence):\n",
              paste0("  -  ", with_issues, collapse = "\n"), "\n",
              "Consider dropping them (see ?amend)\n", sep = "")
  }
  invisible(x)
}

#' print.bayesmanecfit
#'
#' @param x An object of class \code{\link{bayesmanecfit}} as
#' returned by \code{\link{bnec}}.
#' @param ... Further arguments to function summary.
#'
#' @return A \code{\link[base]{list}} containing a summary of the model fit as
#' returned by a \code{\link[brms]{brmsfit}} object for each model.
#'
#' @export
print.bayesmanecfit <- function(x, ...) {
  print(summary(x, ...))
}

#' formula.bayesmanecfit
#'
#' @param x An object of class \code{\link{bayesmanecfit}} as
#' returned by \code{\link{bnec}}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @inheritParams pull_out
#'
#' @return An object of class \code{\link[stats]{formula}}.
#'
#' @importFrom stats formula
#' @export
formula.bayesmanecfit <- function(x, ..., model) {
  x <- suppressMessages(suppressWarnings(pull_out(x, model)))
  formula(x, ...)
}

#' model.frame.bayesmanecfit
#'
#' @param formula An object of class \code{\link{bayesmanecfit}} as
#' returned by \code{\link{bnec}}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @inheritParams pull_out
#'
#' @return A \code{\link[base]{data.frame}} containing the data used to fit
#' the model chosen from the existing \code{\link{bayesmanecfit}} set.
#'
#' @importFrom stats model.frame
#' @export
model.frame.bayesmanecfit <- function(formula, ..., model) {
  x <- suppressMessages(suppressWarnings(pull_out(formula, model)))
  model.frame(x, ...)
}
