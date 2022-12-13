#' @describeIn predict Generates predictions for \code{\link{bayesmanecfit}}
#' objects fitted by \code{\link{bnec}}.
#' @order 3
#'
#' @inheritParams predict.bnecfit
#' @param ... Additional arguments to \code{\link[brms]{posterior_predict}}.
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
#' @method predict bayesmanecfit
#' @inherit predict.bnecfit description return examples
#' @importFrom stats predict
#' @importFrom brms posterior_summary
#' @export
predict.bayesmanecfit <- function(object, summary = TRUE,
                                  robust = FALSE,
                                  probs = c(0.025, 0.975), ...) {
  av_post_preds <- posterior_predict(object, ...)
  if (!summary) {
    av_post_preds
  } else {
    out <- apply(av_post_preds, 2, posterior_summary,
                 robust = robust, probs = probs) |>
      t()
    colnames(out) <- c("Estimate", "Est.Error",
                       paste0("Q", probs * 100))
    out
  }
}

#' @describeIn posterior_predict Generates posterior predictions for
#' \code{\link{bayesmanecfit}} objects fitted by \code{\link{bnec}}.
#' @order 3
#'
#' @inheritParams posterior_predict.bnecfit
#'
#' @method posterior_predict bayesmanecfit
#' @inherit posterior_predict.bnecfit description return examples
#' @importFrom brms posterior_predict
#' @export
posterior_predict.bayesmanecfit <- function(object, ...) {
  mod_fits <- object$mod_fits
  model_set <- names(mod_fits)
  mod_stats <- object$mod_stats
  pred_list <- lapply(mod_fits, function(x, ...) {
    posterior_predict(x$fit, ...)
  }, ...)
  sample_size <- min(sapply(pred_list, nrow))
  do_wrapper(model_set, w_pred_list_calc, pred_list, sample_size,
             mod_stats, fct = "rbind")
}

#' @describeIn fitted Generates mean posterior predictions for
#' \code{\link{bayesmanecfit}} objects fitted by \code{\link{bnec}}.
#' @order 3
#'
#' @inheritParams fitted.bnecfit
#' @param ... Additional arguments to \code{\link[brms]{posterior_epred}}.
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
#' @method fitted bayesmanecfit
#' @inherit fitted.bnecfit description return examples
#' @importFrom stats fitted
#' @importFrom brms posterior_summary
#' @export
fitted.bayesmanecfit <- function(object, summary = TRUE,
                                  robust = FALSE,
                                  probs = c(0.025, 0.975), ...) {
  av_post_preds <- posterior_epred(object, ...)
  if (!summary) {
    av_post_preds
  } else {
    out <- apply(av_post_preds, 2, posterior_summary,
                 robust = robust, probs = probs) |>
      t()
    colnames(out) <- c("Estimate", "Est.Error",
                       paste0("Q", probs * 100))
    out
  }
}

#' @describeIn posterior_epred Generates mean posterior predictions for
#' \code{\link{bayesmanecfit}} objects fitted by \code{\link{bnec}}.
#' @order 3
#'
#' @inheritParams posterior_epred.bnecfit
#'
#' @method posterior_epred bayesmanecfit
#' @inherit posterior_epred.bnecfit description return examples
#' @importFrom brms posterior_epred
#' @export
posterior_epred.bayesmanecfit <- function(object, ...) {
  mod_fits <- object$mod_fits
  model_set <- names(mod_fits)
  mod_stats <- object$mod_stats
  pred_list <- lapply(mod_fits, function(x, ...) {
    posterior_epred(x$fit, ...)
  }, ...)
  sample_size <- min(sapply(pred_list, nrow))
  do_wrapper(model_set, w_pred_list_calc, pred_list, sample_size,
             mod_stats, fct = "rbind")
}

#' rhat.bayesmanecfit
#'
#' @param object An object of class \code{\link{bayesmanecfit}} as
#' returned by \code{\link{bnec}}.
#' @param ... Unused.
#' @param rhat_cutoff A \code{\link[base]{numeric}} vector indicating the Rhat
#' cut-off used to test for model convergence.
#'
#' @return A \code{\link[base]{list}} containing a vector or Rhat values
#' returned for each parameter for a \code{\link[brms]{brmsfit}} object,
#' for each of the fitted models.
#'
#' @importFrom brms rhat
#' @importFrom chk chk_numeric
#'
#' @export
rhat.bayesmanecfit <- function(object, rhat_cutoff = 1.05, ... ) {
  chk_numeric(rhat_cutoff)
  rhat_vals <- lapply(object$mod_fits, function(x) rhat(x$fit))
  check <- lapply(rhat_vals, function(x, rhat_cutoff) max(x > rhat_cutoff),
                  rhat_cutoff)
  failed <- names(rhat_vals)[check == 1]
  if (length(failed) == length(rhat_vals)) {
    message(paste("All models failed the rhat_cutoff of", rhat_cutoff))
  }
  list(rhat_vals = rhat_vals, failed = failed)
}

#' summary.bayesmanecfit
#'
#' @param object An object of class \code{\link{bayesmanecfit}} as
#' returned by \code{\link{bnec}}.
#' @param ecx Should summary ECx values be calculated? Defaults to FALSE.
#' @param ecx_vals ECx targets (between 1 and 99). Only relevant if ecx = TRUE.
#' If no value is specified by the user, returns calculations for EC10, EC50,
#' and EC90.
#' @param ... Unused.
#'
#' @return A \code{\link[base]{list}} containing a summary of the model fit as
#' returned by a \code{\link[brms]{brmsfit}} object for each model.
#'
#' @importFrom purrr map
#' @importFrom brms bayes_R2
#' @importFrom chk chk_lgl chk_numeric
#' @export
summary.bayesmanecfit <- function(object, ..., ecx = FALSE,
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
  ecx_mods <- NULL
  if (any(x$success_models %in% mod_groups$ecx)) {
    ecx_mods <- x$success_models[x$success_models %in% mod_groups$ecx]
  }
  out <- list(
    models = x$success_models,
    family = capture_family(x),
    sample_size = x$sample_size,
    mod_weights = clean_mod_weights(x),
    mod_weights_method = class(x$mod_stats$wi),
    ecx_mods = ecx_mods,
    nec_vals = clean_nec_vals(x),
    ecs = ecs,
    bayesr2 = x$mod_fits |>
      lapply(function(y)bayes_R2(y$fit)) |>
      do.call(what = "rbind.data.frame"),
    rhat_issues = map(x$mod_fits, "fit") |>
      map(has_r_hat_warnings)
  )
  allot_class(out, "manecsummary")
}

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
