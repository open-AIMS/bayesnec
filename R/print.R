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
  with_issues <- names(x$rhat_issues[unlist(x$rhat_issues)])
  if (length(with_issues) > 0) {
      warning("The following model had Rhats > 1.05 (no convergence):\n",
              paste0("  -  ", with_issues, collapse = "\n"), "\n",
              "Consider dropping them (see ?amend)\n", sep = "")
  }
  invisible(x)
}
