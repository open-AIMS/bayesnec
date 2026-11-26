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
#' \emph{Some failed} --- the failed models the object holds are dropped with
#' \code{\link{amend}}.
#'
#' \emph{Everything failed} --- an error, raised before \code{\link{amend}}
#' is called, that gives the reason each model failed. A fit where no candidate
#' sampled properly is an outcome the analysis has to state, with what failed.
#' An error raised by \code{\link{amend}} in the other cases is passed on
#' unchanged.
#'
#' @section Hurdle fits:
#'
#' A \code{\link{bayesnechurdlefit}} is screened one component at a time and
#' rewrapped, as \code{\link{amend}} already does for that class. The two
#' components may legitimately end up with different model sets --- a model
#' valid for the 0-1 bounded survival component need not be valid for growth.
#'
#' @section Grouped fits:
#'
#' A \code{\link{bayesnecgroupfit}} is screened with
#' \code{group_action = "per_level"} by default. An equation which fails at one
#' level is then removed only from that level, because \code{\link{bnec_group}}
#' fitted the levels independently. The returned levels may consequently have
#' different candidate sets.
#'
#' Set \code{group_action = "common"} to remove an equation from every level
#' where it is present when it fails at any level. This keeps an initially
#' common candidate set aligned, but discards fits which passed their own
#' sampler diagnostics. Existing differences caused by equations that failed
#' to fit cannot be restored by screening.
#'
#' @section Joint refits:
#'
#' A \code{\link{bayesnecjointfit}} is screened one level at a time, on the
#' table \code{\link{check_sampling}} returns. Each level has one equation in
#' one posterior, so no level can be dropped. The refit is returned unchanged
#' where every level passes, and the call stops, naming each level that
#' failed, where any does, as it stops for a \code{\link{bayesnecfit}} that
#' fails.
#'
#' @param x An object of class \code{\link{bayesnecfit}},
#' \code{\link{bayesmanecfit}}, \code{\link{bayesnechurdlefit}},
#' \code{\link{bayesnecgroupfit}} or \code{\link{bayesnecjointfit}}.
#' @inheritParams check_sampling
#' @param group_action For a \code{\link{bayesnecgroupfit}}, either
#' \code{"per_level"} to screen each independently fitted level separately, or
#' \code{"common"} to remove from every level each equation which fails at any
#' level. Ignored for other classes.
#' @param quiet A \code{\link[base]{logical}}. Suppress the message reporting
#' what was dropped. Defaults to \code{FALSE}; there is rarely a good reason to
#' set it.
#'
#' @return The object with failing candidates removed. A
#' \code{\link{bayesmanecfit}} reduced to one model becomes a
#' \code{\link{bayesnecfit}}, as \code{\link{amend}} already does. A
#' \code{\link{bayesnechurdlefit}} comes back as one, with each component
#' screened. A \code{\link{bayesnecgroupfit}} comes back as one, with each
#' element of \code{fits} replaced by its screened fit. A
#' \code{\link{bayesnecjointfit}} that passes comes back unchanged.
#'
#' @seealso \code{\link{check_sampling}}, \code{\link{check_fit}},
#' \code{\link{amend}}, \code{\link{pull_best}}
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' screen_models(manec_example)
#' }
#'
#' @export
screen_models <- function(x, rhat_cutoff = 1.01, ess_cutoff = 400,
                          divergence_cutoff = 0.00125, quiet = FALSE,
                          group_action = c("per_level", "common")) {
  chk_lgl(quiet)
  check_divergence_cutoff(divergence_cutoff)
  if (is_bayesnechurdlefit(x)) {
    screen_part <- function(part) {
      screen_models(part, rhat_cutoff = rhat_cutoff, ess_cutoff = ess_cutoff,
                    divergence_cutoff = divergence_cutoff, quiet = quiet)
    }
    return(hurdle_rewrap(x, screen_part(x$growth), screen_part(x$survival)))
  }
  if (is_bayesnecgroupfit(x)) {
    group_action <- match.arg(group_action)
    return(screen_group_models(
      x, rhat_cutoff = rhat_cutoff, ess_cutoff = ess_cutoff,
      divergence_cutoff = divergence_cutoff, quiet = quiet,
      group_action = group_action
    ))
  }
  if (is_bayesnecjointfit(x)) {
    return(screen_joint_levels(x, rhat_cutoff, ess_cutoff, divergence_cutoff,
                               quiet))
  }
  tab <- check_sampling(x, rhat_cutoff = rhat_cutoff,
                        ess_cutoff = ess_cutoff,
                        divergence_cutoff = divergence_cutoff)
  failed <- tab$model[tab$failed]
  if (length(failed) == 0) {
    if (!quiet) {
      message("All ", nrow(tab), " candidate models passed the sampler",
              " screen (Rhat <= ", rhat_cutoff, ", ESS >= ", ess_cutoff,
              ", ", divergence_rule_label(divergence_cutoff), ").")
    }
    return(x)
  }
  held <- if (is_bayesmanecfit(x)) names(x$mod_fits) else x$model
  to_drop <- intersect(failed, held)
  # Decide the all-failed case from the table, not from an error out of
  # amend(), so that amend()'s own errors are not reported as all failed.
  if (length(to_drop) == nrow(tab)) {
    # Put the reasons in the error, so that the user need not re-run
    # check_sampling() to see what failed.
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

#' Screen the levels of a joint refit, which pass or fail together
#'
#' @param x A \code{\link{bayesnecjointfit}}.
#' @inheritParams screen_models
#'
#' @return \code{x} unchanged, or an error naming each level that failed.
#'
#' @noRd
screen_joint_levels <- function(x, rhat_cutoff, ess_cutoff, divergence_cutoff,
                                quiet) {
  tab <- check_sampling(x, rhat_cutoff = rhat_cutoff, ess_cutoff = ess_cutoff,
                        divergence_cutoff = divergence_cutoff)
  if (!any(tab$failed)) {
    if (!quiet) {
      message("All ", nrow(tab), " levels of the joint refit passed the",
              " sampler screen (Rhat <= ", rhat_cutoff, ", ESS >= ",
              ess_cutoff, ", ", divergence_rule_label(divergence_cutoff), ").")
    }
    return(x)
  }
  failed <- tab[tab$failed, , drop = FALSE]
  failed$model <- paste0(failed$level, " (", failed$model, ")")
  stop("A level of the joint refit failed the sampler screen. Each level has",
       " one equation in one posterior, so no level can be dropped. Refit",
       " with more iterations through update(), or inspect",
       " check_sampling(x).\n",
       paste0(screen_reasons(failed, rhat_cutoff, ess_cutoff,
                             divergence_cutoff), collapse = "\n"),
       call. = FALSE)
}

#' Screen the independently fitted levels of a bayesnecgroupfit
#'
#' @param x A \code{\link{bayesnecgroupfit}}.
#' @inheritParams screen_models
#'
#' @return A \code{\link{bayesnecgroupfit}}.
#'
#' @noRd
screen_group_models <- function(x, rhat_cutoff, ess_cutoff,
                                divergence_cutoff, quiet, group_action) {
  if (identical(group_action, "per_level")) {
    fits <- lapply(seq_along(x$fits), function(i) {
      level <- x$levels[i]
      if (!quiet) {
        message("Screening level \"", level, "\".")
      }
      tryCatch(
        screen_models(
          x$fits[[i]], rhat_cutoff = rhat_cutoff, ess_cutoff = ess_cutoff,
          divergence_cutoff = divergence_cutoff, quiet = quiet
        ),
        error = function(e) {
          stop("Level \"", level, "\": ", conditionMessage(e), call. = FALSE)
        }
      )
    })
    names(fits) <- x$levels
    x$fits <- fits
    return(x)
  }

  tab <- check_sampling(
    x, rhat_cutoff = rhat_cutoff, ess_cutoff = ess_cutoff,
    divergence_cutoff = divergence_cutoff
  )
  failed <- unique(tab$model[tab$failed])
  if (length(failed) == 0) {
    if (!quiet) {
      message("All ", nrow(tab), " candidate fits across ", length(x$levels),
              " levels passed the sampler screen (Rhat <= ", rhat_cutoff,
              ", ESS >= ", ess_cutoff, ", ",
              divergence_rule_label(divergence_cutoff), ").")
    }
    return(x)
  }

  failed_rows <- tab[tab$failed, , drop = FALSE]
  reasons <- paste0(
    "  -  ", failed_rows$level, " / ",
    sub("^  -  ", "", screen_reasons(
      failed_rows, rhat_cutoff, ess_cutoff, divergence_cutoff
    ))
  )
  held <- lapply(x$fits, screen_models_held)
  emptied <- vapply(held, function(models) {
    length(intersect(models, failed)) == length(models)
  }, logical(1))
  if (any(emptied)) {
    levels <- paste0("\"", x$levels[emptied], "\"", collapse = ", ")
    stop("Common screening would remove every candidate model from level",
         if (sum(emptied) == 1) " " else "s ", levels,
         ". Refit the failing equations, or use group_action = \"per_level\"",
         " to screen each independently fitted level separately.",
         "\n", paste0(reasons, collapse = "\n"),
         call. = FALSE)
  }

  if (!quiet) {
    message("Removing ", length(failed), " equation",
            if (length(failed) == 1) "" else "s",
            " from every level where present because they failed at least",
            " one level:\n", paste0(reasons, collapse = "\n"))
  }

  x$fits <- Map(function(fit, models) {
    to_drop <- intersect(models, failed)
    if (length(to_drop) == 0) fit else amend(fit, drop = to_drop)
  }, x$fits, held)
  names(x$fits) <- x$levels
  x
}

#' Candidate model names held by one fit
#'
#' @param x A \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @return A \code{\link[base]{character}} vector.
#'
#' @noRd
screen_models_held <- function(x) {
  if (is_bayesmanecfit(x)) names(x$mod_fits) else x$model
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
    if (divergence_failed(r$divergent_ratio, divergence_cutoff)) {
      # A failing row has a positive count and ratio, so their quotient
      # recovers the number of draws.
      n_draws <- round(r$n_divergent / r$divergent_ratio)
      why <- c(why, paste0(r$n_divergent, " divergent transitions (",
                           format_percent(r$divergent_ratio), " of ",
                           n_draws, " draws)"))
    }
    paste0("  -  ", r$model, ": ", paste0(why, collapse = ", "))
  }, character(1))
}
