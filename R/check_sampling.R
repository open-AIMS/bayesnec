#' Sampler diagnostics for every candidate model
#'
#' Reports, per candidate model, whether the sampler explored the posterior:
#' the largest Rhat, the smallest effective sample size, and the number of
#' divergent transitions. It sits alongside \code{\link{check_chains}} (chain
#' mixing, visually), \code{\link{check_priors}} (the priors) and
#' \code{\link{check_fit}} (the fit against the data).
#'
#' \code{check_sampling()} asks \emph{did this model sample properly}, where
#' \code{\link{check_fit}} asks \emph{does this model reproduce the data}. Both
#' feed the same downstream question --- which candidates belong in the averaged
#' set --- which is why \code{\link{screen_models}} screens on this one and
#' reports the other beside it rather than acting on it.
#'
#' @section Thresholds:
#'
#' All three are arguments; the defaults are recommendations, not rules, and
#' whichever ships will be quoted in methods sections.
#'
#' \bold{Rhat 1.01} follows Vehtari et al. (2021), which is also the reference
#' \code{vignette("example2")} cites. \code{\link{rhat}} and
#' \code{\link{summary}} default to the same value.
#'
#' \bold{ESS 400} is Vehtari's recommendation that both bulk and tail ESS exceed
#' 100 per chain, at the four chains \code{\link{bnec}} fits by default. The
#' reported \code{min_ess} is the minimum over parameters of
#' \code{min(bulk-ESS, tail-ESS)}, obtained as
#' \code{min(neff_ratio(x)) * ndraws(x)}.
#'
#' \bold{Divergences 10} \emph{has no literature behind it}. Stan's own guidance
#' is that \emph{any} divergence means the sampler failed to explore the
#' posterior and estimates may be biased. Ten is pragmatic, from practice with
#' these non-linear models, which routinely produce a handful near a boundary.
#' It is a working default and should be described that way rather than cited.
#'
#' @section A heavily thinned fit can fail ESS that a ratio would pass:
#'
#' At three chains, \code{thin = 3} and roughly a thousand retained draws, an
#' \code{min_ess} of 400 needs a ratio of 0.40, which is demanding for these
#' correlated non-linear parameterisations. Under \code{\link{bnec}} defaults
#' --- 8000 draws --- the same cutoff is a ratio of 0.05 and is trivially met.
#' So this screen fails some heavily thinned fits that a ratio-based screen
#' passed. \bold{The correct response is to retain more draws, not to lower the
#' cutoff}: thinning lowers ESS by construction, and the tempting fix is the
#' wrong one. \code{min_ess_ratio} is reported alongside so that "passed because
#' we drew 8000" can be told apart from "passed efficiently".
#'
#' @section Which parameters are reduced over:
#'
#' The same set \code{\link{rhat}} reports: the \code{prior_*} draws that
#' \code{sample_prior = "yes"} adds are excluded, because their Rhat and ESS
#' describe a distribution the sampler never had to explore, and a parameter
#' with no variance is excluded because both quantities are undefined for it ---
#' which is what a \code{constant()} prior produces. \code{failed} is a
#' logical by construction and is never \code{NA}: a model that cannot be
#' assessed is not the same thing as a model that failed, and folding the two
#' together silently turns \code{\link{screen_models}} into a no-op.
#'
#' @param x An object of class \code{\link{bayesnecfit}},
#' \code{\link{bayesmanecfit}} or \code{\link{bayesnechurdlefit}}.
#' @param rhat_cutoff A \code{\link[base]{numeric}} vector of length 1.
#' @param ess_cutoff A \code{\link[base]{numeric}} vector of length 1.
#' @param divergence_cutoff A \code{\link[base]{numeric}} vector of length 1.
#'
#' @return A \code{\link[base]{data.frame}} with one row per candidate model and
#' columns \code{model}, \code{max_rhat}, \code{min_ess}, \code{min_ess_ratio},
#' \code{n_divergent} and \code{failed}. For a
#' \code{\link{bayesnechurdlefit}}, a named \code{\link[base]{list}} of two
#' such tables --- \code{growth} and \code{survival} --- as
#' \code{\link{rhat}} also returns for that class.
#'
#' @references
#' Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Bürkner, P.-C. (2021).
#' Rank-normalization, folding, and localization: An improved Rhat for assessing
#' convergence of MCMC (with discussion). \emph{Bayesian Analysis}, 16(2),
#' 667--718. doi:10.1214/20-BA1221
#'
#' @seealso \code{\link{screen_models}}, \code{\link{check_fit}},
#' \code{\link{check_chains}}, \code{\link{rhat}}
#'
#' @importFrom brms nuts_params neff_ratio ndraws rhat
#' @importFrom chk chk_number
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' check_sampling(manec_example)
#' }
#'
#' @export
check_sampling <- function(x, rhat_cutoff = 1.01, ess_cutoff = 400,
                           divergence_cutoff = 10) {
  # chk_number rather than chk_numeric: chk_numeric admits a vector of any
  # length, and every use here is scalar. amend() already uses chk_number for
  # its scalars.
  chk_number(rhat_cutoff)
  chk_number(ess_cutoff)
  chk_number(divergence_cutoff)
  if (is_bayesnechurdlefit(x)) {
    return(hurdle_delegate(x, check_sampling, rhat_cutoff = rhat_cutoff,
                           ess_cutoff = ess_cutoff,
                           divergence_cutoff = divergence_cutoff))
  }
  fits <- sampling_fits(x)
  rows <- lapply(names(fits), function(m) {
    sampling_row(fits[[m]], m, rhat_cutoff, ess_cutoff, divergence_cutoff)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' The candidate brmsfits of a fit, named by model
#'
#' @param x A \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @return A named \code{\link[base]{list}} of \code{\link[brms]{brmsfit}}.
#'
#' @noRd
sampling_fits <- function(x) {
  if (is_bayesmanecfit(x)) {
    # x$mod_fits[[m]]$fit is the same brmsfit pull_out() would hand back, at a
    # fraction of the cost -- pull_out() rebuilds predictions and posteriors to
    # reach it. names(mod_fits) == success_models (expand_classes.R:253).
    out <- lapply(x$success_models, function(m) x$mod_fits[[m]]$fit)
    names(out) <- x$success_models
    out
  } else if (is_bayesnecfit(x)) {
    out <- list(pull_brmsfit(x))
    names(out) <- x$model
    out
  } else {
    stop("check_sampling requires a bayesnecfit, a bayesmanecfit or a",
         " bayesnechurdlefit.", call. = FALSE)
  }
}

#' One diagnostic row for one brmsfit
#'
#' @param fit A \code{\link[brms]{brmsfit}}.
#' @param model A \code{\link[base]{character}} string.
#' @param rhat_cutoff,ess_cutoff,divergence_cutoff Thresholds.
#'
#' @return A one-row \code{\link[base]{data.frame}}.
#'
#' @noRd
sampling_row <- function(fit, model, rhat_cutoff, ess_cutoff,
                         divergence_cutoff) {
  # nuts_params() and neff_ratio() are brmsfit generics and work under either
  # backend. rstan::get_num_divergent(x$fit) is the obvious thing to reach for
  # and works often enough to look correct, but it reaches past brms into the
  # stanfit slot and rstan is Suggests-only here -- so it would fail under
  # cmdstanr and on any install without rstan. See #148.
  np <- nuts_params(fit, pars = "divergent__")
  n_div <- sum(np$Value)
  # Both reductions go through screenable_pars() (see R/rhat.R), so the screen
  # and rhat() agree on what counts as a parameter: no prior_* draws, and no
  # zero-variance parameter for which the diagnostic is undefined. Reducing
  # over the raw vectors fails a model on a prior draw, and returns NA for a
  # constant() parameter -- which then propagates into `failed` and makes
  # screen_models() report a drop it did not perform.
  # neff_ratio() is min(ess_bulk, ess_tail) / ndraws, so multiplying back gives
  # the absolute minimum over parameters with no dependency on `posterior`.
  ratio <- min_or_na(screenable_pars(neff_ratio(fit)))
  n_draws <- ndraws(fit)
  min_ess <- ratio * n_draws
  max_rhat <- max_or_na(screenable_pars(rhat(fit)))
  data.frame(
    model = model,
    max_rhat = max_rhat,
    min_ess = min_ess,
    min_ess_ratio = ratio,
    n_divergent = n_div,
    # isTRUE, so that a model with nothing assessable reports FALSE rather than
    # NA. "Not assessable" is visible in the NA of max_rhat and min_ess, which
    # is where it belongs; `failed` is read as a logical downstream.
    failed = isTRUE(max_rhat > rhat_cutoff) || isTRUE(min_ess < ess_cutoff) ||
      isTRUE(n_div > divergence_cutoff),
    stringsAsFactors = FALSE
  )
}

#' max() and min() that return NA rather than -Inf/Inf on an empty vector
#'
#' Reachable only if every parameter of a fit is fixed by a \code{constant()}
#' prior, which Stan would not sample, but \code{max(numeric(0))} warns and
#' returns \code{-Inf}, and an \code{-Inf} max_rhat reads as a pass.
#'
#' @param x A \code{\link[base]{numeric}} vector.
#'
#' @return A \code{\link[base]{numeric}} vector of length 1.
#'
#' @noRd
max_or_na <- function(x) {
  if (length(x) == 0) NA_real_ else max(x)
}

#' @noRd
min_or_na <- function(x) {
  if (length(x) == 0) NA_real_ else min(x)
}
