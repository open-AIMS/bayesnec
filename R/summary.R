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
#' Both report the number of divergent transitions after warmup for each model,
#' and the installation of \pkg{bayesnec} that fitted it (see
#' \code{\link{bnec_version}}).
#' Warning messages are also printed to screen in case
#' model fits are not satisfactory with regards to their Rhats or their
#' divergent transitions.
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
#' a range supplied now cannot change it. Where \code{crf()} transforms the
#' predictor inline, that posterior is on the transformed scale, and the row is
#' returned on the recorded scale as \code{\link{nec}} returns it. An
#' \code{xform} supplied in \code{...} is applied to the stored draws and to
#' the bound they are censored at instead, as \code{\link{nec}} applies it,
#' so the no-effect row and the ECx rows are reported on one scale.
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
#' @param divergence_cutoff A \code{\link[base]{numeric}} vector of length 1.
#' The summary warns of a model with more divergent transitions after warmup
#' than this. Defaults to 10, the cutoff \code{\link{check_sampling}} and
#' \code{\link{screen_models}} use. On a model set the warning gives each
#' flagged equation's model weight beside its count.
#'
#' @method summary bayesnecfit
#'
#' @inherit summary description return details examples
#'
#' @importFrom brms bayes_R2
#' @importFrom chk chk_numeric chk_lgl chk_number
#'
#' @export
summary.bayesnecfit <- function(object, ..., ecx = FALSE,
                                ecx_vals = c(10, 50, 90),
                                divergence_cutoff = 10) {
  chk_lgl(ecx)
  chk_numeric(ecx_vals)
  chk_number(divergence_cutoff)
  x <- object
  check_summary_ecx_posterior(list(...))
  # Resolve xform from the dots as ecx() matches them, so that the no-effect
  # row is on the scale of the ECx rows. See summary_ne_vals().
  ne_xform <- summary_ecx_xform(list(...))
  ecs <- NULL
  if (ecx) {
    message("ECx calculation takes a few seconds per model, calculating...\n")
    # Default to the grid the fit was built over, so that the ECx rows and the
    # no-effect row describe one range. ecx() given no x_range builds its own
    # grid from the data. A caller's x_range replaces the default; see
    # summary_ecx_rows().
    ecs <- summary_ecx_rows(..., .fit = x, .ecx_vals = ecx_vals,
                            .stored_range = range(x$pred_vals$data$x))
  }
  # Classify by the equation's parameters, as nec() does, not by its group or
  # name.
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
    divergences = summary_divergences(x),
    divergence_cutoff = divergence_cutoff,
    failed_models = failed_models(x),
    bnec_version = bnec_version(x)
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
                                  divergence_cutoff = 10,
                                  fit_ratio_cutoff = 1.15,
                                  check_fit = TRUE) {
  chk_lgl(ecx)
  chk_numeric(ecx_vals)
  # chk_number, not chk_numeric: the documented type of each cutoff is a
  # vector of length 1, and chk_numeric admits any length.
  chk_number(rhat_cutoff)
  chk_number(divergence_cutoff)
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
    # Compute the Rhat verdict with rhat(), not by searching brms's warning
    # text, so that bayesnec sets the threshold and a reworded brms warning
    # cannot silence the summary.
    rhat_issues = lapply(rhat(x, rhat_cutoff = rhat_cutoff), "[[", "failed"),
    rhat_cutoff = rhat_cutoff,
    divergences = summary_divergences(x),
    divergence_cutoff = divergence_cutoff,
    # Recompute the control check rather than store a prediction matrix on the
    # object, which bnec() leaves out to keep fits small. It uses 200 draws,
    # because it runs on every summary() call for every candidate model.
    fit_issues = if (check_fit) {
      control_fit_issues(x, fit_ratio_cutoff)
    } else {
      NULL
    },
    fit_ratio_cutoff = fit_ratio_cutoff,
    failed_models = failed_models(x),
    bnec_version = bnec_version(x)
  )
  allot_class(out, "manecsummary")
}

#' The ECx rows of a summary of a single fit or a model set
#'
#' Each row is one \code{\link{ecx}} call, given the dots of the summary
#' method, so that a summary's ECx row equals a bare \code{\link{ecx}} call
#' given the same arguments.
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
  # before ... is matched by partial name: an ecx_val meant for ecx() would be
  # taken as ecx_vals and shift every positional argument. After ..., a
  # duplicate ecx_val stops with "matched by multiple actual arguments".
  #
  # ecx_row()'s own x_range formal takes a caller's x_range in place of the
  # default, so ecx() receives it once; forwarded in ... beside the default it
  # would stop with "matched by multiple actual arguments". The hurdle summary
  # applies the same rule. The formal follows ..., so it takes no positional
  # argument and matches only the full name: an abbreviation such as x_ran
  # reaches ecx() through ... and is ignored. NULL leaves ecx() to build its
  # own grid from the data.
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
#' \code{x_range = NULL}, so the rebuilt call names them too.
#'
#' @param dots The dots of a summary method, as a \code{\link[base]{list}},
#' less \code{ecx} and \code{ecx_vals}.
#'
#' @return A function, or whatever was supplied, or \code{NULL} where nothing
#' was. See \code{dots_xform()}.
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
#' table would print columns of \code{NA}. The group tables refuse it for the
#' same reason (\code{group_estimate_table()}).
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
#' Where the map from \code{recorded_xform()} is \code{identity}, the row is
#' the one stored at fit time, \code{object$ne} or \code{object$w_ne}.
#' Otherwise the stored posterior and its censoring record are mapped and
#' summarised by the function that built the stored row, so that the row is on
#' the scale of the ECx rows. The steps are those of
#' \code{nec.bayesnecfit()}; \code{nec()} is not called, because it refuses a
#' single fit of an ECx equation, whose no-effect row is an NSEC.
#'
#' @param x A \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#' @param xform The \code{xform} resolved from the summary's dots, as
#' \code{summary_ecx_xform()} returns it: \code{NULL} where none was
#' supplied.
#'
#' @return The stored summary where the map is \code{identity}, and
#' otherwise a summary of the same form, from \code{estimates_summary()}.
#'
#' @noRd
summary_ne_vals <- function(x, xform) {
  check_xform(xform)
  stored <- if (is_bayesnecfit(x)) x$ne else x$w_ne
  map <- recorded_xform(x, xform)
  # Return the stored row itself, not a recomputation, so that a summary
  # of an untransformed fit prints exactly the row stored at fit time.
  if (identical(map, identity)) {
    return(stored)
  }
  post <- if (is_bayesnecfit(x)) x$ne_posterior else x$w_ne_posterior
  cens <- attr(post, "censored")
  post <- map_draws(post, map, cens, "no-effect estimate")
  # Set after the map rather than relied on, because a general function need
  # not keep an attribute. A decreasing map swaps the end the draws are
  # censored at; xform_censoring() handles that, as it does for nec().
  attr(post, "censored") <- xform_censoring(cens, map)
  estimates_summary(post)
}

#' Which candidate models mis-state the control, by ratio
#'
#' The fit half of the summary block. Flags a model where the observed control
#' statistic differs from the simulated one by more than
#' \code{fit_ratio_cutoff} either way.
#'
#' It thresholds the ratio, not the posterior predictive p-value: a control
#' overshoot that reproduces across fits can leave the p-value far from any
#' threshold. The control matters most because \code{\link{nsec}} reads its
#' reference from the control posterior.
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

#' The number of divergent transitions after warmup in each model of a fit
#'
#' @param x A \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @return A named \code{\link[base]{numeric}} vector, one element per model,
#' \code{NA} for a model not sampled by NUTS.
#'
#' @noRd
summary_divergences <- function(x) {
  vapply(sampling_fits(x), function(fit) {
    # nuts_params() stops on a variational fit, which has no sampler output,
    # and the summary has to print for it. brms reads a NULL as "sampling".
    if (!is.null(fit$algorithm) && !identical(fit$algorithm, "sampling")) {
      return(NA_real_)
    }
    count_divergent(fit)
  }, numeric(1))
}
