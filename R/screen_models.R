#' Drop candidate models that failed the sampler diagnostics
#'
#' Runs \code{\link{check_sampling}}, drops every candidate that failed, and
#' reports what went and why. The message is the point of the function, not
#' decoration: it is what a methods section has to cite, and an exclusion step
#' that leaves no record is not reproducible.
#'
#' @section What it screens on, and what it does not:
#'
#' Sampler diagnostics only. A poor \code{\link{check_fit}} is a \emph{modelling
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
#' \emph{Everything failed} --- decided here, from the table, before
#' \code{\link{amend}} is called at all, and reported as a result rather than
#' a usage error, with the same per-model reasons the drop message carries: a
#' fit where no candidate sampled properly is an outcome the analysis has to
#' state, and stating it without saying what failed is not enough to write up. Deciding it up front rather than catching an error out
#' of \code{\link{amend}} matters, because \code{\link{amend}} raises
#' unrelated errors that would otherwise be reported as convergence results ---
#' a \code{\link{bayesnecfit}} raises "Cannot drop models from a bayesnecfit",
#' and refitting the model weights can fail for reasons of its own. Everything
#' except the all-failed case now propagates as itself.
#'
#' @section Hurdle fits:
#'
#' A \code{\link{bayesnechurdlefit}} is screened one component at a time and
#' rewrapped, as \code{\link{amend}} already does for that class. The two
#' components may legitimately end up with different model sets --- a model
#' valid for the 0-1 bounded survival component need not be valid for growth.
#'
#' @param x An object of class \code{\link{bayesnecfit}},
#' \code{\link{bayesmanecfit}} or \code{\link{bayesnechurdlefit}}.
#' @inheritParams check_sampling
#' @param quiet A \code{\link[base]{logical}}. Suppress the message reporting
#' what was dropped. Defaults to \code{FALSE}; there is rarely a good reason to
#' set it.
#'
#' @return The object with failing candidates removed. A
#' \code{\link{bayesmanecfit}} reduced to one model becomes a
#' \code{\link{bayesnecfit}}, as \code{\link{amend}} already does. A
#' \code{\link{bayesnechurdlefit}} comes back as one, with each component
#' screened.
#'
#' @seealso \code{\link{check_sampling}}, \code{\link{check_fit}},
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
  if (is_bayesnechurdlefit(x)) {
    screen_part <- function(part) {
      screen_models(part, rhat_cutoff = rhat_cutoff, ess_cutoff = ess_cutoff,
                    divergence_cutoff = divergence_cutoff, quiet = quiet)
    }
    return(hurdle_rewrap(x, screen_part(x$growth), screen_part(x$survival)))
  }
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
  # Decided from the table rather than from an error out of amend(), so that
  # amend()'s own errors are not all reported as "everything failed". Both
  # numbers are already in hand. See #148 Part D.
  if (length(to_drop) == nrow(tab)) {
    # The reasons go into the error, not only into the drop message: the
    # all-failed case is the one a methods section most has to account for, and
    # an error that says only "everything failed" makes the user re-run
    # check_sampling() to find out what failed and by how much.
    stop("Every candidate model failed the sampler screen, so there is",
         " nothing left to return. This is a result rather than a usage",
         " error: none of the models sampled well enough to be reported.",
         " Refit with more iterations, or inspect check_sampling(x) and",
         " relax a threshold deliberately if you judge the failures benign.",
         "\n",
         paste0(screen_reasons(tab, rhat_cutoff, ess_cutoff,
                               divergence_cutoff),
                collapse = "\n"),
         call. = FALSE)
  }
  if (!quiet) {
    message("Dropping ", length(to_drop), " of ", nrow(tab),
            " candidate models that failed the sampler screen:\n",
            paste0(screen_reasons(tab[tab$model %in% to_drop, , drop = FALSE],
                                  rhat_cutoff, ess_cutoff,
                                  divergence_cutoff),
                   collapse = "\n"))
  }
  amend(x, drop = to_drop)
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
    # isTRUE to match the construction of `failed` in check_sampling(): a row
    # whose diagnostics were not assessable carries NA, and `if (NA)` is a hard
    # error rather than a skipped reason.
    if (isTRUE(r$max_rhat > rhat_cutoff)) {
      why <- c(why, paste0("Rhat ", round(r$max_rhat, 3)))
    }
    if (isTRUE(r$min_ess < ess_cutoff)) {
      why <- c(why, paste0("ESS ", round(r$min_ess, 1)))
    }
    if (isTRUE(r$n_divergent > divergence_cutoff)) {
      why <- c(why, paste0(r$n_divergent, " divergent transitions"))
    }
    paste0("  -  ", r$model, ": ", paste0(why, collapse = ", "))
  }, character(1))
}
