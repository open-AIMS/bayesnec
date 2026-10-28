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
  # Absent from a necsummary stored before the count was recorded.
  if (!is.null(x$divergences)) {
    cat("Divergent transitions after warmup: ", x$divergences, "\n", sep = "")
  }
  cat("\n\n")
  if (x$is_ecx) {
    cat("NB: Model", x$model, "is an ECx model, thus",
        "the NEC estimate is an\n", "   NSEC surrogate.\n", sep = " ")
  } else if (identical(x$ne_type, "N(S)EC")) {
    # Reachable only for a two-block fit whose blocks use different equation
    # types, such as a threshold response block with a smooth survival block.
    cat("NB: The two blocks of this fit use a threshold model on one and a\n",
        "   smooth model on the other, so the combined estimate is a\n",
        "   combination of a NEC and a NSEC.\n", sep = "")
  }
  print_mat(x$nec_vals)
  print_censoring_note(attr(x$nec_vals, "censored_summary"),
                       rownames(x$nec_vals)[1])
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
  print_version(x$bnec_version)
  div_bad <- divergent_models(x)
  if (length(div_bad) > 0) {
    warning(divergence_message(div_bad, x$divergence_cutoff),
            "\nConsider refitting with a larger adapt_delta, passed to bnec()",
            " as control = list(adapt_delta = ...).\n")
  }
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
  print_censoring_note(attr(x$nec_vals, "censored_summary"), neclab)
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
  # Absent from a manecsummary stored before the count was recorded.
  if (!is.null(x$divergences)) {
    cat("Divergent transitions after warmup:\n")
    print(cbind(n_divergent = x$divergences))
    cat("\n\n")
  }
  print_failed_models(x$failed_models)
  print_version(x$bnec_version)
  # Report both axes in one block: convergence and the control fit inform one
  # decision about which candidates belong in the averaged set.
  # Use which() rather than logical indexing. A manecsummary stored by an older
  # version can hold an NA, which would print "- NA" as a failed model.
  rhat_bad <- names(x$rhat_issues)[which(unlist(x$rhat_issues))]
  fit_bad <- if (is.null(x$fit_issues)) {
    character(0)
  } else {
    names(x$fit_issues)[which(unlist(x$fit_issues))]
  }
  div_bad <- divergent_models(x)
  sampler_bad <- length(rhat_bad) > 0 || length(div_bad) > 0
  if (sampler_bad || length(fit_bad) > 0) {
    msg <- character(0)
    if (length(rhat_bad) > 0) {
      msg <- c(msg,
               paste0("Rhat > ",
                      # An object stored without a cutoff was assessed
                      # against the earlier threshold of 1.05, so report that.
                      if (is.null(x$rhat_cutoff)) 1.05 else x$rhat_cutoff,
                      " (no convergence):
",
                      paste0("  -  ", rhat_bad, collapse = "
")))
    }
    if (length(div_bad) > 0) {
      msg <- c(msg, divergence_message(div_bad, x$divergence_cutoff))
    }
    if (length(fit_bad) > 0) {
      msg <- c(msg,
               paste0("control observed/simulated ratio beyond ",
                      # As for rhat_cutoff above, an object stored without
                      # the field was assessed against the default.
                      if (is.null(x$fit_ratio_cutoff)) {
                        1.15
                      } else {
                        x$fit_ratio_cutoff
                      },
                      " (see ?check_fit):
",
                      paste0("  -  ", fit_bad, collapse = "
")))
    }
    # Give different advice per axis. A model that did not converge should be
    # dropped; a converged model that reproduces the control badly is a result,
    # and dropping it silently would hide it.
    tail_txt <- if (sampler_bad && length(fit_bad) > 0) {
      "
Sampler failures should be dropped (see ?amend or ?screen_models).
A control mis-fit is a result, not a fault: inspect it with check_fit().
"
    } else if (sampler_bad) {
      "
Consider dropping them (see ?amend or ?screen_models)
"
    } else {
      "
This is a modelling result rather than a fault; inspect with check_fit().
"
    }
    warning(paste0(msg, collapse = "

"), tail_txt, sep = "")
  }
  invisible(x)
}

#' The models of a summary with more divergent transitions than its cutoff
#'
#' @param x A \code{necsummary} or \code{manecsummary}.
#'
#' @return A named \code{\link[base]{numeric}} vector of counts, empty for a
#' summary stored before the count was recorded.
#'
#' @noRd
divergent_models <- function(x) {
  n <- x$divergences
  if (is.null(n)) {
    return(numeric(0))
  }
  # Strictly greater, as check_sampling() fails a model. which() drops the NA
  # of a model not sampled by NUTS.
  n[which(n > x$divergence_cutoff)]
}

#' The lines of a summary warning that name the divergent models
#'
#' @param div_bad A named \code{\link[base]{numeric}} vector of counts, from
#' \code{divergent_models()}.
#' @param cutoff The cutoff the counts exceed.
#'
#' @return A \code{\link[base]{character}} string.
#'
#' @noRd
divergence_message <- function(div_bad, cutoff) {
  paste0("Divergent transitions > ", cutoff, " (see ?check_sampling):\n",
         paste0("  -  ", names(div_bad), " (", div_bad, ")", collapse = "\n"))
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
