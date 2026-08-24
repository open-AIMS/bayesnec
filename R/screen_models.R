#' Drop candidate models that failed the sampler diagnostics
#'
#' Runs \code{\link{check_sampling}}, drops every candidate that failed, and
#' reports what went and why. The message is the point of the function, not
#' decoration: it is what a methods section has to cite, and an exclusion step
#' that leaves no record is not reproducible.
#'
#' @section What it screens on, and what it does not:
#'
#' Sampler diagnostics only. A poor \code{check_fit()} is a \emph{modelling
#' result} --- the model reproduces the data badly, which is information --- and
#' dropping on it silently would hide exactly what the user needs to see. A
#' sampler failure is different in kind: the fit is unusable, whatever it says.
#' So report the two side by side and screen on this one.
#'
#' @section Three cases:
#'
#' \emph{Nothing failed} --- returns the object unchanged, and says so.
#' \code{\link{amend}} is not called at all.
#'
#' \emph{Some failed} --- the failures are intersected against the models the
#' object actually holds before \code{\link{amend}} is called. That guard lives
#' here rather than in \code{\link{amend}}, which is deliberately not modified:
#' \code{amend(drop = )} already handles a mix of present and absent names
#' correctly, and silently returns the object unchanged only when \emph{every}
#' named model is absent.
#'
#' \emph{Everything failed} --- \code{\link{amend}} errors with "All models
#' removed, nothing to return". That is caught and replaced with something that
#' says what happened and what to do about it, because a fit where no candidate
#' sampled properly is a result, not a usage error.
#'
#' @param x An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}.
#' @inheritParams check_sampling
#' @param quiet A \code{\link[base]{logical}}. Suppress the message reporting
#' what was dropped. Defaults to \code{FALSE}; there is rarely a good reason to
#' set it.
#'
#' @return The object with failing candidates removed. A
#' \code{\link{bayesmanecfit}} reduced to one model becomes a
#' \code{\link{bayesnecfit}}, as \code{\link{amend}} already does.
#'
#' @seealso \code{\link{check_sampling}}, \code{check_fit()},
#' \code{\link{amend}}
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' screen_models(manec_example)
#' }
#'
#' @export
screen_models <- function(x, rhat_cutoff = 1.01, ess_cutoff = 400,
                          divergence_cutoff = 10, quiet = FALSE) {
  chk_lgl(quiet)
  tab <- check_sampling(x, rhat_cutoff = rhat_cutoff,
                        ess_cutoff = ess_cutoff,
                        divergence_cutoff = divergence_cutoff)
  failed <- tab$model[tab$failed]
  if (length(failed) == 0) {
    if (!quiet) {
      message("All ", nrow(tab), " candidate models passed the sampler",
              " screen (Rhat <= ", rhat_cutoff, ", ESS >= ", ess_cutoff,
              ", divergences <= ", divergence_cutoff, ").")
    }
    return(x)
  }
  held <- if (is_bayesmanecfit(x)) names(x$mod_fits) else x$model
  to_drop <- intersect(failed, held)
  if (!quiet) {
    message("Dropping ", length(to_drop), " of ", nrow(tab),
            " candidate models that failed the sampler screen:\n",
            paste0(screen_reasons(tab[tab$model %in% to_drop, , drop = FALSE],
                                  rhat_cutoff, ess_cutoff,
                                  divergence_cutoff),
                   collapse = "\n"))
  }
  out <- try(amend(x, drop = to_drop), silent = TRUE)
  if (inherits(out, "try-error")) {
    stop("Every candidate model failed the sampler screen, so there is",
         " nothing left to average. This is a result rather than a usage",
         " error: none of the models sampled well enough to be reported.",
         " Refit with more iterations, or inspect check_sampling(x) and",
         " relax a threshold deliberately if you judge the failures benign.",
         call. = FALSE)
  }
  out
}

#' One line per dropped model, naming which threshold it failed
#'
#' Every failing criterion is listed, not just the first: a model that failed
#' on two counts is a different situation from one that scraped past on one.
#'
#' @param tab Rows of a \code{\link{check_sampling}} table.
#' @param rhat_cutoff,ess_cutoff,divergence_cutoff Thresholds.
#'
#' @return A \code{\link[base]{character}} vector.
#'
#' @noRd
screen_reasons <- function(tab, rhat_cutoff, ess_cutoff, divergence_cutoff) {
  vapply(seq_len(nrow(tab)), function(i) {
    r <- tab[i, ]
    why <- character(0)
    if (r$max_rhat > rhat_cutoff) {
      why <- c(why, paste0("Rhat ", round(r$max_rhat, 3)))
    }
    if (r$min_ess < ess_cutoff) {
      why <- c(why, paste0("ESS ", round(r$min_ess, 1)))
    }
    if (r$n_divergent > divergence_cutoff) {
      why <- c(why, paste0(r$n_divergent, " divergent transitions"))
    }
    paste0("  -  ", r$model, ": ", paste0(why, collapse = ", "))
  }, character(1))
}
