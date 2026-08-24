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
#' \code{vignette("example2")} cites. Note \code{\link{rhat}} defaults to the
#' older 1.05.
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
#' @param x An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}.
#' @param rhat_cutoff A \code{\link[base]{numeric}} vector of length 1.
#' @param ess_cutoff A \code{\link[base]{numeric}} vector of length 1.
#' @param divergence_cutoff A \code{\link[base]{numeric}} vector of length 1.
#'
#' @return A \code{\link[base]{data.frame}} with one row per candidate model and
#' columns \code{model}, \code{max_rhat}, \code{min_ess}, \code{min_ess_ratio},
#' \code{n_divergent} and \code{failed}.
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
  chk_numeric(rhat_cutoff)
  chk_numeric(ess_cutoff)
  chk_numeric(divergence_cutoff)
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
    out <- lapply(x$success_models, function(m) {
      pull_brmsfit(suppressMessages(pull_out(x, model = m)))
    })
    names(out) <- x$success_models
    out
  } else if (is_bayesnecfit(x)) {
    out <- list(pull_brmsfit(x))
    names(out) <- x$model
    out
  } else {
    stop("check_sampling requires a bayesnecfit or a bayesmanecfit.",
         call. = FALSE)
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
  # neff_ratio() is min(ess_bulk, ess_tail) / ndraws, so multiplying back gives
  # the absolute minimum over parameters with no dependency on `posterior`.
  ratio <- min(neff_ratio(fit))
  n_draws <- ndraws(fit)
  min_ess <- ratio * n_draws
  max_rhat <- max(rhat(fit), na.rm = TRUE)
  data.frame(
    model = model,
    max_rhat = max_rhat,
    min_ess = min_ess,
    min_ess_ratio = ratio,
    n_divergent = n_div,
    failed = max_rhat > rhat_cutoff | min_ess < ess_cutoff |
      n_div > divergence_cutoff,
    stringsAsFactors = FALSE
  )
}
