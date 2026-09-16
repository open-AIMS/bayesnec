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
#' @param ... Unused.
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
  ecs <- NULL
  if (ecx) {
    message("ECx calculation takes a few seconds per model, calculating...\n")
    ecs <- list()
    for (i in seq_along(ecx_vals)) {
      ecs[[i]] <- ecx(x, ecx_val = ecx_vals[i])
    }
    names(ecs) <- paste0("ECx (", ecx_vals, "%) estimate:")
  }
  is_ecx <- x$model %in% mod_groups$ecx
  ecx_mod <- NULL
  if (is_ecx) {
    ecx_mod <- x$model
  }
  out <- list(
    brmssummary = cleaned_brms_summary(x$fit),
    model = x$model,
    is_ecx = is_ecx,
    ne_type = x$ne_type,
    nec_vals = clean_nec_vals(x, x$model, ecx_mod),
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
  ecs <- NULL
  if (ecx) {
    message("ECx calculation takes a few seconds per model, calculating...\n")
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
    nec_vals = clean_nec_vals(x, x$success_models, ecx_mods),
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
