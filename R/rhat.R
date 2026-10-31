#' Extract Diagnostic Quantities of 'brms' Models
#'
#' Extract Rhat statistic that can be used to diagnose sampling behaviour
#' of the algorithms applied by 'Stan' at the back-end of 'brms'.
#' \code{x} should be of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @name rhat
#' @order 1
#'
#' @param x An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}.
#' @param rhat_cutoff A \code{\link[base]{numeric}} vector of length 1
#' indicating the Rhat cut-off used to test for model convergence. Defaults to
#' 1.01 following Vehtari et al. (2021).
#' @param ... Unused.
#'
#' @return A \code{\link[base]{list}} containing a vector of Rhat values
#' returned for each parameter for a \code{\link[brms]{brmsfit}} object,
#' for each of the fitted models, and the logical \code{failed} verdict
#' against \code{rhat_cutoff}.
#'
#' @section Which parameters are reported:
#'
#' The \code{prior_*} variables are excluded. \code{\link{bnec}} forces
#' \code{sample_prior = "yes"}, so every fit carries an independent draw from
#' the prior for every parameter; the Rhat of those draws is Monte Carlo noise
#' about a distribution the sampler never had to explore, and including them can
#' fail a model on a variable that is not part of the model.
#'
#' Parameters with no variance are excluded too, because their Rhat is
#' undefined. That is what a \code{constant()} prior produces: a parameter
#' fixed at a known value has nothing to converge to.
#'
#' \code{lp__} and \code{lprior} are kept. They are deterministic functions of
#' the posterior draws rather than of the prior, so they do carry a convergence
#' signal.
#'
#' @references
#' Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Bürkner, P.-C. (2021).
#' Rank-normalization, folding, and localization: An improved Rhat for assessing
#' convergence of MCMC (with discussion). \emph{Bayesian Analysis}, 16(2),
#' 667--718. doi:10.1214/20-BA1221
#'
#' @seealso \code{\link{check_sampling}}, \code{\link{screen_models}}
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' rhat(manec_example)
#' nec4param <- pull_out(manec_example, model = "nec4param")
#' rhat(nec4param)
#' }
NULL

#' @rdname rhat
#' @order 2
#'
#' @method rhat bayesnecfit
#'
#' @inherit rhat description return examples
#'
#' @importFrom brms rhat
#' @importFrom chk chk_number
#'
#' @export
rhat.bayesnecfit <- function(x, rhat_cutoff = 1.01, ...) {
  chk_number(rhat_cutoff)
  out <- list(rhat_one(pull_brmsfit(x), rhat_cutoff))
  names(out) <- x$model
  out
}

#' @rdname rhat
#' @order 3
#'
#' @method rhat bayesmanecfit
#'
#' @inherit rhat description return examples
#'
#' @importFrom brms rhat
#' @importFrom chk chk_number
#'
#' @export
rhat.bayesmanecfit <- function(x, rhat_cutoff = 1.01, ...) {
  chk_number(rhat_cutoff)
  # Read $fit directly: pull_out() would rebuild a whole bayesnecfit to reach
  # it, and summary() calls this for every equation.
  out <- lapply(x$success_models, function(m) {
    rhat_one(x$mod_fits[[m]]$fit, rhat_cutoff)
  })
  names(out) <- x$success_models
  failed <- vapply(out, "[[", logical(1), "failed")
  if (length(failed) > 0 && all(failed)) {
    message("All models failed the rhat_cutoff of ", rhat_cutoff)
  }
  out
}

#' Rhat values and the convergence verdict for one brmsfit
#'
#' @param fit A \code{\link[brms]{brmsfit}}.
#' @param rhat_cutoff A \code{\link[base]{numeric}} vector of length 1.
#'
#' @return A \code{\link[base]{list}} of \code{rhat_vals} and \code{failed}.
#'
#' @noRd
rhat_one <- function(fit, rhat_cutoff) {
  rhat_vals <- rhat(fit) |>
    screenable_pars() |>
    clean_rhat_names()
  # Keep na.rm although screenable_pars() removes NA: an NA `failed` would
  # make the convergence check in summary() and screen_models() a silent
  # no-op.
  list(rhat_vals = rhat_vals,
       failed = any(rhat_vals > rhat_cutoff, na.rm = TRUE))
}

#' The parameters a convergence screen should reduce over
#'
#' Drops two kinds of entry from a named vector of per-parameter diagnostics.
#' \code{prior_*} entries are draws from the prior, which \code{\link{bnec}}
#' adds to every fit with \code{sample_prior = "yes"}; their Rhat is noise and
#' can fail a model on a variable that is not part of it. \code{NA} entries
#' come from zero-variance columns, such as a parameter fixed by a
#' \code{constant()} prior, which has nothing to converge to.
#'
#' \code{lp__} and \code{lprior} are kept on purpose: they are functions of the
#' posterior draws and carry a convergence signal.
#'
#' @param x A named \code{\link[base]{numeric}} vector.
#'
#' @return \code{x}, with the non-posterior and non-assessable entries removed.
#'
#' @noRd
screenable_pars <- function(x) {
  x[!grepl("^prior_", names(x)) & !is.na(x)]
}

#' @noRd
clean_rhat_names <- function(x) {
  y <- names(x)
  names(x) <- gsub("^b\\_", "", y) |>
    (\(.)gsub("\\_b\\_", "_", .))() |>
    (\(.)gsub("\\_Intercept$", "", .))()
  x
}
