#' Generates a summary for objects fitted by \code{\link{bnec}}
#'
#' Generates a summary for objects fitted by \code{\link{bnec}}.
#' \code{object} should be of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}.
#'
#' @name summary
#' @order 1
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}.
#' @param ... With \code{ecx = TRUE}, passed to \code{\link{ecx}} for each
#' ECx row, so that \code{x_range} and \code{resolution} in particular apply
#' to the ECx rows only. An \code{xform} is also applied to the no-effect row,
#' with or without \code{ecx = TRUE}. See Details.
#'
#' @return A summary of the fitted model. In the case of a
#' \code{\link{bayesnecfit}} object, the summary contains most of the original
#' contents of a \code{\link[brms]{brmsfit}} object with the addition of
#' an R2. In the case of a \code{\link{bayesmanecfit}} object, summary
#' displays the family distribution information, model weights and averaging
#' method, and Bayesian R2 estimates for each individual model.
#' Warning messages are also printed to screen in case
#' model fits are not satisfactory with regards to their Rhats.
#' 
#' @details The summary method for both \code{\link{bayesnecfit}} and 
#' \code{\link{bayesmanecfit}} also returns a no-effect toxicity
#' estimate. Where the fitted model(s) are NEC models (threshold models,
#' containing a step function) the no-effect estimate is a true 
#' no-effect-concentration (NEC, see Fox 2010). Where the fitted model(s) are 
#' smooth ECx models with no step function, the no-effect estimate is a 
#' no-significant-effect-concentration (NSEC, see Fisher and Fox 2023). In the 
#' case of a \code{\link{bayesmanecfit}} that contains a mixture of both NEC and
#' ECx models, the no-effect estimate is a model averaged combination of the NEC
#' and NSEC estimates, and is reported as the N(S)EC (see Fisher et al. 2023).
#'
#' With \code{ecx = TRUE}, each ECx row is computed by \code{\link{ecx}} over
#' the prediction grid stored with the fit, which is the grid the no-effect
#' estimate is censored at. An \code{x_range} supplied in \code{...} replaces
#' that grid. It and the other arguments in \code{...}, such as
#' \code{resolution} and \code{xform}, reach \code{\link{ecx}} as given, so
#' an ECx row equals a bare \code{\link{ecx}} call given the same arguments.
#' \code{x_range = NULL} is the exception: it is not passed on, and
#' \code{\link{ecx}} builds its grid from the data, as it does when given no
#' \code{x_range}. \code{x_range} must be given by its full name, because an
#' abbreviation such as \code{x_ran} is ignored without a message.
#'
#' The no-effect estimate is read from the posterior stored when the model was
#' fitted, and is censored at the bound of the prediction grid stored then, so
#' a range supplied now cannot change it. An \code{xform} supplied in
#' \code{...} is applied to those stored draws and to the bound they are
#' censored at, as \code{\link{nec}} applies it, so the no-effect row and the
#' ECx rows are reported on one scale.
#'
#' \code{posterior = TRUE} is refused. A summary reports each estimate as
#' quantiles; the draws themselves are returned by \code{\link{nec}},
#' \code{\link{nsec}} and \code{\link{ecx}} with \code{posterior = TRUE}.
#' 
#' @references
#' Fisher R, Fox DR (2023). Introducing the no significant effect concentration 
#' (NSEC).Environmental Toxicology and Chemistry, 42(9), 2019–2028. 
#' doi: 10.1002/etc.5610.
#'
#' Fisher R, Fox DR, Negri AP, van Dam J, Flores F, Koppel D (2023). Methods for
#' estimating no-effect toxicity concentrations in ecotoxicology. Integrated 
#' Environmental Assessment and Management. doi:10.1002/ieam.4809.
#' 
#' Fox DR (2010). A Bayesian Approach for Determining the No Effect
#' Concentration and Hazardous Concentration in Ecotoxicology. Ecotoxicology
#' and Environmental Safety, 73(2), 123–131. doi: 10.1016/j.ecoenv.2009.09.012.
#'
#' @examples
#' \donttest{
#' library(bayesnec)
#' summary(manec_example)
#' nec4param <- pull_out(manec_example, "nec4param")
#' summary(nec4param)
#' }
NULL

#' @rdname summary
#' @order 2
#'
#' @param ecx Should summary ECx values be calculated? Defaults to FALSE.
#' @param ecx_vals ECx targets (between 1 and 99). Only relevant if ecx = TRUE.
#' If no value is specified by the user, returns calculations for EC10, EC50,
#' and EC90.
#'
#' @method summary bayesnecfit
#'
#' @inherit summary description return details examples
#'
#' @importFrom brms bayes_R2
#' @importFrom chk chk_numeric chk_lgl
#'
#' @export
summary.bayesnecfit <- function(object, ..., ecx = FALSE,
                                ecx_vals = c(10, 50, 90)) {
  chk_lgl(ecx)
  chk_numeric(ecx_vals)
  x <- object
  check_summary_ecx_posterior(list(...))
  # Resolved from the dots as ecx() matches them, so that the no-effect row is
  # given the xform the ECx rows are given (#439, option A). See
  # summary_ne_vals().
  ne_xform <- summary_ecx_xform(list(...))
  ecs <- NULL
  if (ecx) {
    message("ECx calculation takes a few seconds per model, calculating...\n")
    # By default on the grid the fit was built over, not the range of the
    # data. ecx() rebuilds its own grid from the data when x_range is absent,
    # so a fit given an x_range reported its ECx over a different range from
    # the no-effect estimate printed three lines above it. Since #395 marks a
    # censored no-effect estimate with the bound it is censored at, the two
    # claims sit on one screen: a note reading "the upper bound of the
    # prediction range is 0.9" stood directly above an unmarked ECx of 1.67.
    # A caller's x_range replaces this default (#439); see summary_ecx_rows().
    ecs <- summary_ecx_rows(..., .fit = x, .ecx_vals = ecx_vals,
                            .stored_range = range(x$pred_vals$data$x))
  }
  # Read off the equation's parameters rather than group membership, so that
  # ecxflat, which belongs to no group, is reported as the NSEC it gives. See
  # #419 and has_nec_parameter().
  is_ecx <- !has_nec_parameter(x$model, x$fit)
  ecx_mod <- NULL
  if (is_ecx) {
    ecx_mod <- x$model
  }
  out <- list(
    brmssummary = cleaned_brms_summary(x$fit),
    model = x$model,
    is_ecx = is_ecx,
    ne_type = x$ne_type,
    nec_vals = clean_nec_vals(x, x$model, ecx_mod,
                              summary_ne_vals(x, ne_xform)),
    ecs = ecs,
    bayesr2 = bayes_R2(x$fit),
    failed_models = failed_models(x)
  )
  allot_class(out, "necsummary")
}

#' @rdname summary
#' @order 3
#'
#' @method summary bayesmanecfit
#'
#' @inherit summary description return details examples
#'
#' @importFrom purrr map
#' @param rhat_cutoff A \code{\link[base]{numeric}} vector of length 1. The
#' convergence threshold the summary reports against. Defaults to 1.01,
#' following Vehtari et al. (2021) and matching \code{\link{rhat}}.
#' @param fit_ratio_cutoff A \code{\link[base]{numeric}} vector of length 1.
#' The threshold for flagging a candidate model that mis-states the control:
#' the summary reports a model whose observed control statistic differs from
#' the simulated one by more than this ratio, either way. Defaults to 1.15.
#' Thresholded on the ratio rather than the posterior predictive p-value ---
#' see \code{\link{check_fit}}.
#' @param check_fit A \code{\link[base]{logical}} vector of length 1. Whether
#' to run the control lack-of-fit check and report it in the summary block.
#' Defaults to \code{TRUE}. Set \code{FALSE} to skip the posterior simulation
#' it requires.
#'
#' @importFrom brms bayes_R2
#' @importFrom chk chk_lgl chk_numeric chk_number
#'
#' @export
summary.bayesmanecfit <- function(object, ..., ecx = FALSE,
                                  ecx_vals = c(10, 50, 90),
                                  rhat_cutoff = 1.01,
                                  fit_ratio_cutoff = 1.15,
                                  check_fit = TRUE) {
  chk_lgl(ecx)
  chk_numeric(ecx_vals)
  # chk_number, not chk_numeric: the documented type of both cutoffs is a
  # vector of length 1, and chk_numeric admits any length.
  chk_number(rhat_cutoff)
  chk_number(fit_ratio_cutoff)
  chk_lgl(check_fit)
  x <- object
  # As in summary.bayesnecfit().
  check_summary_ecx_posterior(list(...))
  ne_xform <- summary_ecx_xform(list(...))
  ecs <- NULL
  if (ecx) {
    message("ECx calculation takes a few seconds per model, calculating...\n")
    # By default the grid the set was built over, for the reason given in
    # summary.bayesnecfit.
    ecs <- summary_ecx_rows(..., .fit = x, .ecx_vals = ecx_vals,
                            .stored_range = range(x$w_pred_vals$data$x))
  }
  # As in summary.bayesnecfit(): the parameters, not group membership.
  no_nec <- !vapply(x$success_models, function(m) {
    has_nec_parameter(m, x$mod_fits[[m]]$fit)
  }, logical(1))
  ecx_mods <- NULL
  if (any(no_nec)) {
    ecx_mods <- x$success_models[no_nec]
  }
  out <- list(
    models = x$success_models,
    family = capture_family(x),
    sample_size = x$sample_size,
    mod_weights = clean_mod_weights(x),
    mod_weights_method = class(x$mod_stats$wi),
    ecx_mods = ecx_mods,
    nec_vals = clean_nec_vals(x, x$success_models, ecx_mods,
                              summary_ne_vals(x, ne_xform)),
    ecs = ecs,
    bayesr2 = x$mod_fits |>
      lapply(function(y)bayes_R2(y$fit)) |>
      do.call(what = "rbind.data.frame"),
    # Computed, not grepped. This used to be has_r_hat_warnings(), which
    # searched brms's captured warning text for the literal string
    # "some Rhats are > 1.05". That made the summary's threshold brms's to set
    # rather than bayesnec's, and it fails silently: brms (>= 2.23.0) is a
    # floor, not a ceiling, so if that warning is ever reworded every model
    # reports FALSE and the summary quietly stops warning. Silence reads as a
    # pass. See #148 Part D.
    rhat_issues = lapply(rhat(x, rhat_cutoff = rhat_cutoff), "[[", "failed"),
    rhat_cutoff = rhat_cutoff,
    # The fit axis of the same block. Recomputed rather than cached: #180 (PR
    # #205) removed the stored prediction matrices deliberately, and stashing
    # one back on the object here would undo that. Thinned to 200 draws because
    # this runs on every summary() call across every candidate model, and the
    # ratio it reports is stable well below the 1000 check_fit() defaults to.
    fit_issues = if (check_fit) {
      control_fit_issues(x, fit_ratio_cutoff)
    } else {
      NULL
    },
    fit_ratio_cutoff = fit_ratio_cutoff,
    failed_models = failed_models(x)
  )
  allot_class(out, "manecsummary")
}

#' The ECx rows of a summary of a single fit or a model set
#'
#' Each row is one \code{\link{ecx}} call, given the dots of the summary
#' method, so that a summary's ECx row equals a bare \code{\link{ecx}} call
#' given the same arguments (#439). Before, the dots were absorbed by the
#' summary method, and an \code{x_range} or \code{resolution} named there was
#' ignored without a message.
#'
#' @param ... The dots of the summary method, passed to \code{\link{ecx}}.
#' @param .fit A \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#' @param .ecx_vals The ECx targets.
#' @param .stored_range The range of the prediction grid stored with
#' \code{.fit}, which is the default \code{x_range}. It is evaluated only where
#' the caller supplies no \code{x_range}.
#'
#' @return A named \code{\link[base]{list}}, one element per entry of
#' \code{.ecx_vals}.
#'
#' @noRd
summary_ecx_rows <- function(..., .fit, .ecx_vals, .stored_range) {
  # The helper's own arguments follow ... and are dotted, because a formal
  # ahead of ... is matched by partial name. With ecx_vals there, an ecx_val
  # meant for ecx() was taken as ecx_vals and every positional argument
  # moved along one place: summary(fit, ecx = TRUE, ecx_val = 50) returned
  # an EC50 of 10, read over a grid of c(10, 50, 90) at a resolution equal to
  # the stored range. After ..., a name is matched only in full, so ecx_val
  # reaches ecx() beside the one ecx_row() names and stops the call with R's
  # "matched by multiple actual arguments", as on the hurdle summary.
  #
  # Once for the table rather than once per ecx_vals entry, from the dots as
  # ecx() will match them. See summary_ecx_xform(). This is done here rather
  # than in the summary methods because there the logical argument ecx masks
  # the generic: dots_xform() would be handed TRUE in place of ecx(), match
  # nothing, and report the fitted scale beside rows the caller's xform had
  # already inverted.
  quiet <- report_fitted_scale(.fit, summary_ecx_xform(list(...)), "ecx")
  on.exit(options(quiet), add = TRUE)
  # A caller's x_range is matched by ecx_row()'s own formal and replaces the
  # default, so ecx() receives it once. Forwarded in ... beside the default,
  # it would stop the call with "formal argument "x_range" matched by
  # multiple actual arguments", which is what #416 found on the hurdle
  # summary. This is the rule summary.bayesnechurdlefit() applies, so the
  # three summary methods treat x_range alike. The formal follows ... so that
  # it cannot take a positional argument meant for ecx(), and it matches only
  # the full name: an abbreviation such as x_ran reaches ecx() through ...,
  # where the default has already matched x_range exactly, and is ignored.
  # ?summary asks for the full name. A NULL leaves ecx() to build its own
  # grid from the data, as it does on the hurdle summary.
  ecx_row <- function(..., .v, x_range = .stored_range) {
    if (is.null(x_range)) {
      ecx(.fit, ecx_val = .v, ...)
    } else {
      ecx(.fit, ecx_val = .v, x_range = x_range, ...)
    }
  }
  ecs <- lapply(.ecx_vals, function(v) ecx_row(..., .v = v))
  names(ecs) <- paste0("ECx (", .ecx_vals, "%) estimate:")
  ecs
}

#' The xform the ECx rows of a summary are given
#'
#' The dots are matched against \code{\link{ecx}} as \code{ecx_row()} in
#' \code{summary_ecx_rows()} will pass them, so that an \code{xform} given by
#' position is found at the position it takes there. That call names
#' \code{ecx_val}, and names \code{x_range} unless the caller supplied
#' \code{x_range = NULL}, so the rebuilt call names them too. Without
#' \code{x_range} in it, a fourth positional argument was read as
#' \code{xform} by \code{ecx()} and as \code{x_range} here, and the scale
#' message was raised beside rows already on the recorded scale.
#'
#' @param dots The dots of a summary method, as a \code{\link[base]{list}},
#' less \code{ecx} and \code{ecx_vals}.
#'
#' @return A function, or whatever was supplied. See \code{dots_xform()}.
#'
#' @noRd
summary_ecx_xform <- function(dots) {
  dots_xform(ecx, summary_ecx_dots(dots))
}

#' The dots of a summary method as ecx_row() passes them to ecx()
#'
#' @param dots The dots of a summary method, as a \code{\link[base]{list}},
#' less \code{ecx} and \code{ecx_vals}.
#'
#' @return \code{dots}, led by the arguments \code{ecx_row()} names, so that
#' matching the list against \code{\link{ecx}} finds each argument where
#' \code{ecx()} will. See \code{summary_ecx_xform()}.
#'
#' @noRd
summary_ecx_dots <- function(dots) {
  nm <- names(dots)
  given <- !is.null(nm) && "x_range" %in% nm
  # The values stand in for what ecx_row() passes; only the names and their
  # positions are read.
  lead <- list(ecx_val = 10)
  if (!given || !is.null(dots[["x_range"]])) {
    lead$x_range <- NA
  }
  if (given) {
    dots <- dots[nm != "x_range"]
  }
  c(lead, dots)
}

#' Refuse posterior = TRUE in a summary
#'
#' A summary prints each estimate as a row of quantiles. With
#' \code{posterior = TRUE} the estimators return the draws instead, and the
#' printed table showed columns of \code{NA} with no censoring note. The group
#' tables refuse it for the same reason (\code{group_estimate_table()}).
#'
#' @param generic The estimator the dots are passed to, \code{\link{ecx}} or
#' \code{\link{nec}}, against whose formals they are matched, so that a
#' \code{posterior} given by position is found where that estimator takes it.
#' @param dots The dots as they are passed, as a \code{\link[base]{list}}.
#'
#' @return \code{NULL}, invisibly, or an error.
#'
#' @noRd
check_summary_posterior <- function(generic, dots) {
  # Matched as dots_xform() matches, with the object slot filled. A call that
  # does not match is left to the estimator, which raises the error with the
  # caller's own call in it.
  matched <- tryCatch(
    match.call(generic, as.call(c(list(quote(f), quote(object)), dots))),
    error = function(e) NULL
  )
  if (!is.null(matched) && isTRUE(matched[["posterior"]])) {
    stop("summary() reports each estimate as quantiles, which a posterior",
         " sample is not. Use ecx(), nec() or nsec() with posterior = TRUE",
         " for the draws.", call. = FALSE)
  }
  invisible(NULL)
}

#' Refuse posterior = TRUE among the dots a summary passes to ecx()
#'
#' Defined here rather than inlined, because in the summary methods the name
#' \code{ecx} is the logical argument and masks the generic.
#'
#' @param dots The dots of a summary method, as a \code{\link[base]{list}}.
#'
#' @noRd
check_summary_ecx_posterior <- function(dots) {
  check_summary_posterior(ecx, summary_ecx_dots(dots))
}

#' The no-effect row of a summary of a single fit or a model set
#'
#' The row summary() has always printed is the one stored when the model was
#' fitted, \code{object$ne} or \code{object$w_ne}. Given an \code{xform}, the
#' stored posterior is transformed, with its censoring record, and summarised
#' by the function that built the stored row, so that the row is on the scale
#' of the ECx rows (#439, option A). The steps are those of
#' \code{nec.bayesnecfit()}; nec() itself is not called, because it refuses a
#' single fit of an ECx equation, whose no-effect row is an NSEC.
#'
#' @param x A \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#' @param xform The \code{xform} resolved from the summary's dots, as
#' \code{summary_ecx_xform()} returns it: \code{identity} where none was
#' supplied.
#'
#' @return The stored summary where \code{xform} is \code{identity}, and
#' otherwise a summary of the same form, from \code{estimates_summary()}.
#'
#' @noRd
summary_ne_vals <- function(x, xform) {
  stored <- if (is_bayesnecfit(x)) x$ne else x$w_ne
  # Returned as stored rather than recomputed, so that a summary called
  # without an xform prints exactly what it printed before.
  if (identical(xform, identity)) {
    return(stored)
  }
  if (!inherits(xform, "function")) {
    stop("xform must be a function.", call. = FALSE)
  }
  post <- if (is_bayesnecfit(x)) x$ne_posterior else x$w_ne_posterior
  cens <- attr(post, "censored")
  post <- xform(post)
  # Set after xform rather than relied on, because a general function need not
  # keep an attribute. A decreasing xform swaps the end the draws are censored
  # at; xform_censoring() handles that, as it does for nec().
  attr(post, "censored") <- xform_censoring(cens, xform)
  estimates_summary(post)
}

#' Which candidate models mis-state the control, by ratio
#'
#' The fit half of the summary block. Flags a model where the observed control
#' statistic differs from the simulated one by more than
#' \code{fit_ratio_cutoff} either way.
#'
#' \bold{Thresholded on the ratio, not the posterior predictive p-value.} This
#' is settled and the evidence is specific: measured twice on independently
#' fitted parameterisations of the same simulated data, the simulated control
#' mean came out at 5.5--5.6 against an observed 4.50 and a true 4.77 --- a
#' ~19\% overshoot that reproduces across fits and is a property of the curve
#' shape rather than noise. \bold{Both p-values were about 0.82 and neither came
#' near flagging.} A \code{ppp} threshold would stay silent on exactly the case
#' this exists to catch, and silence reads as a pass.
#'
#' The control matters more than the other groups because \code{\link{nsec}}
#' reads its reference from the control posterior, so mis-stating control
#' variability moves a reported no-effect concentration.
#'
#' @param x A \code{\link{bayesmanecfit}}.
#' @param cutoff A \code{\link[base]{numeric}} vector of length 1.
#'
#' @return A named \code{\link[base]{logical}}, one element per candidate
#' model, or \code{NULL} where the check could not be run.
#'
#' @noRd
control_fit_issues <- function(x, cutoff) {
  tab <- try(suppressWarnings(suppressMessages(
    check_fit(x, ndraws = 200)
  )), silent = TRUE)
  if (inherits(tab, "try-error")) {
    return(NULL)
  }
  d <- as.data.frame(tab)
  d <- d[d$control, , drop = FALSE]
  if (nrow(d) == 0) {
    return(NULL)
  }
  off <- function(r) !is.finite(r) | r > cutoff | r < 1 / cutoff
  flagged <- off(d$mean_ratio) | off(d$sd_ratio)
  out <- as.list(flagged)
  names(out) <- if (is.null(d$model)) x$success_models[1] else d$model
  out
}
