#' Best crossed model combination from a factorised hurdle fit
#'
#' Returns the growth and survival model pair carrying the highest crossed
#' weight, i.e. the single combination \code{\link{crossed_weights}} favours.
#'
#' @param object An object of class \code{\link{bayesnechurdlefit}}.
#'
#' @details The crossed weights are the outer product of the two components'
#' model weights (see \code{\link{crossed_weights}}), so the best pair is the
#' best growth model paired with the best survival model. It is returned as a
#' pair rather than assembled by eye because it is the natural starting point
#' for a joint refit: \code{\link{bnec_joint}} takes it directly.
#'
#' Selecting a single combination discards the rest of the crossed table, which
#' is a real loss where the weights are spread. Prefer model-averaged estimates
#' from the factorised fit itself unless a single fit is needed for a reason
#' the factorisation cannot serve -- coupling the two blocks through a shared
#' group-level effect being the main one.
#'
#' @return A \code{\link[base]{list}} with elements \code{growth},
#' \code{survival} (model names) and \code{weight} (the crossed weight of that
#' pair).
#'
#' @seealso \code{\link{crossed_weights}}, \code{\link{bnec_joint}},
#' \code{\link{bnec_hurdle}}
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' data(nec_data)
#' nec_data$y[nec_data$x > 2.5] <- 0
#' fit <- bnec_hurdle(y ~ crf(x, c("nec3param", "nec4param", "ecx4param")),
#'                    data = nec_data)
#' crossed_weights(fit)
#' best_crossed(fit)
#' }
#'
#' @export
best_crossed <- function(object) {
  w <- crossed_weights(object)
  # Indexed positionally: crossed_weights() names the dimensions "growth" and
  # "survival", and arr.ind takes its column names from those rather than the
  # usual "row"/"col".
  best <- which(w == max(w), arr.ind = TRUE)[1, ]
  i <- best[[1]]
  j <- best[[2]]
  list(growth = rownames(w)[i], survival = colnames(w)[j],
       weight = unname(w[i, j]))
}

#' Refit a factorised hurdle model as a single joint fit
#'
#' Takes the model combination favoured by \code{\link{crossed_weights}} and
#' refits it as one \code{\link{bnec}} model with a two-block
#' (\code{hurdle_gamma} or \code{zero_inflated_beta}) family.
#'
#' @param object An object of class \code{\link{bayesnechurdlefit}} returned by
#' \code{\link{bnec_hurdle}}.
#' @param model An optional \code{\link[base]{character}} string naming the
#' equation for the response block. Defaults to the highest-weighted growth
#' model in \code{object}.
#' @param model_survival An optional \code{\link[base]{character}} string naming
#' the equation for the survival block. Defaults to the highest-weighted
#' survival model in \code{object}.
#' @param formula An optional \code{\link{bayesnecformula}} to fit instead of
#' the one held in \code{object}. Supply this to add structure the factorised
#' fit cannot carry, for example a group-level term. The \code{crf} model
#' argument is overwritten by \code{model} either way.
#' @param ... Further arguments passed to \code{\link{bnec}}.
#'
#' @details
#'
#' \bold{Why refit at all}
#'
#' The factorised and joint routes give equivalent inference where the two
#' components are independent, so a joint refit of the same pair of equations
#' adds nothing on its own -- the estimates differ only by Monte Carlo noise.
#' What it adds is the ability to write structure that spans the two blocks,
#' which the factorisation cannot express by construction: a group-level effect
#' shared between response and survival makes the two posteriors dependent, and
#' a dependent posterior cannot be assembled from two fits run separately.
#'
#' The division of labour is therefore: fit the model sets with
#' \code{\link{bnec_hurdle}}, where all \code{n_growth * n_survival}
#' combinations can be compared from two fits; choose a combination from
#' \code{\link{crossed_weights}}; then refit that one jointly here, adding the
#' shared structure. Model averaging happens in the first step, because the
#' joint route can only average over the response block.
#'
#' \bold{What is carried over}
#'
#' The response variable, predictor and data are taken from \code{object}, and
#' the family follows the growth component: \code{hurdle_gamma} where that was
#' fitted as a Gamma, \code{zero_inflated_beta} where it was a Beta. Nothing
#' else transfers -- priors, control arguments and the like are defaults again
#' unless passed through \code{...}.
#'
#' @return An object of class \code{\link{bayesnecfit}}.
#'
#' @seealso \code{\link{bnec_hurdle}}, \code{\link{crossed_weights}},
#' \code{\link{best_crossed}}, \code{\link{bnec}}
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' data(nec_data)
#' nec_data$y[nec_data$x > 2.5] <- 0
#' nec_data$tank <- factor(rep(1:10, length.out = nrow(nec_data)))
#' fit <- bnec_hurdle(y ~ crf(x, c("nec3param", "nec4param", "ecx4param")),
#'                    data = nec_data)
#' # the pair crossed_weights() favours, refitted as one model
#' fit_j <- bnec_joint(fit)
#' # the same pair with a group-level effect the factorisation cannot carry
#' fit_r <- bnec_joint(fit, formula = y ~ crf(x, "nec3param") + (nec | tank))
#' }
#'
#' @export
bnec_joint <- function(object, model = NULL, model_survival = NULL,
                       formula = NULL, ...) {
  if (!is_bayesnechurdlefit(object)) {
    stop("bnec_joint requires an object of class bayesnechurdlefit, as",
         " returned by bnec_hurdle().", call. = FALSE)
  }
  best <- best_crossed(object)
  if (is.null(model)) {
    model <- best$growth
  }
  if (is.null(model_survival)) {
    model_survival <- best$survival
  }
  growth_fit <- if (inherits(object$growth, "bayesmanecfit")) {
    object$growth$mod_fits[[1]]$fit
  } else {
    pull_brmsfit(object$growth)
  }
  mu_fam <- growth_fit$family$family
  # Matched case-insensitively: bayesnec's identity-link families report
  # lower-case tags ("gamma", "beta") while the map is keyed on the stats/brms
  # constructor names ("Gamma", "beta").
  joint_fam <- names(hurdle_mu_fams)[match(tolower(mu_fam),
                                           tolower(unname(hurdle_mu_fams)))]
  if (length(joint_fam) == 0 || is.na(joint_fam)) {
    stop("There is no two-block family corresponding to a \"", mu_fam,
         "\" growth component, so this fit cannot be refitted jointly.",
         call. = FALSE)
  }
  if (is.null(formula)) {
    formula <- object$formula
  }
  formula <- swap_crf_model(bayesnecformula(formula), model)
  message("Refitting jointly as a ", joint_fam, " with a ", model,
          " response block and a ", model_survival, " survival block",
          " (crossed weight ", signif(best$weight, 3), ").")
  bnec(formula, data = object$data, family = joint_fam,
       model_survival = model_survival, ...)
}
