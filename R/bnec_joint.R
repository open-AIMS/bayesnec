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

#' Refit a factorised model as a single joint fit
#'
#' Takes what a factorised fit favoured and refits it as one model expressing
#' structure the factorisation could not. For a
#' \code{\link{bayesnechurdlefit}} that is the model combination
#' \code{\link{crossed_weights}} favours, refitted with a two-block
#' (\code{hurdle_gamma}, \code{zero_inflated_beta}, \code{hurdle_poisson} or
#' \code{hurdle_negbinomial}) family. For a \code{\link{bayesnecgroupfit}} it
#' is one equation in which every curve parameter takes a separate value per
#' level of the grouping factor, estimated in a single posterior.
#'
#' @param object An object of class \code{\link{bayesnechurdlefit}} returned by
#' \code{\link{bnec_hurdle}}, or of class \code{\link{bayesnecgroupfit}}
#' returned by \code{\link{bnec_group}}.
#' @param model An optional \code{\link[base]{character}} string naming the
#' equation to fit. For a hurdle fit it is the response block's equation and
#' defaults to the highest-weighted growth model in \code{object}; for a
#' grouped fit it is the one equation fitted at every level and defaults to
#' the equation holding the highest summed weight across levels.
#' @param model_survival An optional \code{\link[base]{character}} string naming
#' the equation for the survival block. Defaults to the highest-weighted
#' survival model in \code{object}. Hurdle fits only.
#' @param formula An optional \code{\link{bayesnecformula}} to fit instead of
#' the one held in \code{object}. Supply this to add structure the factorised
#' fit cannot carry, for example a group-level term. The \code{crf} model
#' argument is overwritten by \code{model} either way.
#' @param disp_by_level A \code{\link[base]{logical}}. Should the family's
#' dispersion parameter also take a separate value per level? Defaults to
#' \code{TRUE}. Grouped fits only.
#' @param ... Further arguments passed to \code{\link[brms]{brm}}.
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
#' fitted as a Gamma, \code{zero_inflated_beta} where it was a Beta, and the
#' corresponding count hurdle where it was Poisson or negative binomial.
#' Nothing else transfers -- priors, control arguments and the like are
#' defaults again unless passed through \code{...}.
#'
#' \bold{The joint refit of a grouped fit}
#'
#' \code{\link{bnec_group}} fits each level of a factor separately, and a
#' group-level term such as \code{ogl()} or \code{(nec | site)} pools the
#' levels towards a common curve. Neither fits one model in which every level
#' has its own curve parameters estimated together, which is what this route
#' adds. Each parameter's \code{~ 1} is replaced by \code{~ 0 + <group_var>},
#' so \code{top}, \code{nec} and the rest take a value per level within one
#' posterior. That posterior supports a contrast between two levels
#' conditional on everything they share, which differencing the independent
#' posteriors of a \code{\link{bnec_group}} fit cannot.
#'
#' One equation is fitted for all levels, because one model has one functional
#' form. The default is the equation holding the highest summed weight across
#' levels, and where that equation holds less than half of the summed weight
#' the levels favouring other equations are reported.
#'
#' \code{disp_by_level} decides whether the family's dispersion parameter is
#' also estimated per level. \code{FALSE} shares one dispersion across the
#' levels, which is the model for a measurement error that is a property of the
#' assay rather than of the level.
#'
#' \code{\link{ecx}}, \code{\link{nsec}}, \code{\link{nec}} and
#' \code{\link{ecnsec}} on the returned object give one row per level, in the
#' \code{\link[base]{data.frame}} a \code{\link{bayesnecgroupfit}} returns, and
#' \code{\link{autoplot}} draws one panel per level. Each level is estimated on
#' its own prediction grid, so the estimate for a level is read off that
#' level's curve alone.
#'
#' @return For a \code{\link{bayesnechurdlefit}}, an object of class
#' \code{\link{bayesnecfit}}; for a \code{\link{bayesnecgroupfit}}, an object
#' of class \code{\link{bayesnecjointfit}}.
#'
#' @seealso \code{\link{bnec_hurdle}}, \code{\link{bnec_group}},
#' \code{\link{crossed_weights}}, \code{\link{best_crossed}}, \code{\link{bnec}}
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
#'
#' # a grouped fit, refitted as one model with a value per level
#' data(nec_data)
#' nec_data$site <- factor(rep(c("a", "b"), length.out = nrow(nec_data)))
#' fits <- bnec_group(y ~ crf(x, c("nec3param", "nec4param")),
#'                    data = nec_data, group_var = "site")
#' joint <- bnec_joint(fits)
#' }
#'
#' @export
bnec_joint <- function(object, ...) {
  UseMethod("bnec_joint")
}

#' @rdname bnec_joint
#' @export
bnec_joint.default <- function(object, ...) {
  stop("bnec_joint requires an object of class bayesnechurdlefit, as",
       " returned by bnec_hurdle(), or of class bayesnecgroupfit, as",
       " returned by bnec_group().", call. = FALSE)
}

#' @rdname bnec_joint
#' @export
bnec_joint.bayesnechurdlefit <- function(object, model = NULL,
                                         model_survival = NULL,
                                         formula = NULL, ...) {
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
  formula <- swap_crf_model(bayesnecformula(formula, env = parent.frame()),
                            model)
  message("Refitting jointly as a ", joint_fam, " with a ", model,
          " response block and a ", model_survival, " survival block",
          " (crossed weight ", signif(best$weight, 3), ").")
  bnec(formula, data = object$data, family = joint_fam,
       model_survival = model_survival, ...)
}

#' @rdname bnec_joint
#'
#' @importFrom chk chk_logical
#'
#' @export
bnec_joint.bayesnecgroupfit <- function(object, model = NULL, formula = NULL,
                                        disp_by_level = TRUE, ...) {
  chk_logical(disp_by_level)
  eq_weights <- joint_equation_weights(object)
  n_lev <- length(object$levels)
  if (is.null(model)) {
    model <- names(eq_weights)[1]
    share <- unname(eq_weights[[1]]) / n_lev
    # Reported, not refused. A spread of weight across equations is a result
    # about the data, and the user asked for a joint fit; what they need is to
    # know that one equation is being imposed on levels that did not choose it.
    if (share < 0.5) {
      favoured <- vapply(object$fits, function(x) {
        w <- fit_model_weights(x)
        names(w)[which.max(w)]
      }, character(1))
      message("The levels do not agree on an equation (",
              paste0("\"", object$levels, "\" favours ", favoured,
                     collapse = "; "),
              "). A joint refit has one functional form, and ", model,
              " holds the highest summed weight, ", signif(share, 3),
              " of the ", n_lev, " available. Pass `model` to choose",
              " another.")
    }
  } else {
    if (!is.character(model) || length(model) != 1) {
      stop("`model` must name a single equation. A joint refit fits one",
           " equation at every level, so a set cannot be averaged over here;",
           " the averaging belongs in the bnec_group() call that preceded it.",
           call. = FALSE)
    }
    share <- if (model %in% names(eq_weights)) {
      unname(eq_weights[[model]]) / n_lev
    } else {
      NA_real_
    }
  }
  if (is.null(formula)) {
    formula <- object$formula
  }
  formula <- swap_crf_model(bayesnecformula(formula, env = parent.frame()),
                            model)
  family <- unmark_family(validate_family(object$family))
  if (isTRUE(disp_by_level) && !is.null(parse_disp_term(formula))) {
    stop("The formula already has a disp() term, which models the",
         " dispersion parameter, so it cannot also be given a value per",
         " level. Set disp_by_level = FALSE, or drop the disp() term.",
         call. = FALSE)
  }
  group_var <- object$group_var
  data <- object$data
  # Re-levelled rather than taken as found. The coefficient order brms builds
  # is the order of levels(), and a character column would be levelled
  # alphabetically by the design matrix, so pinning it here is what makes
  # `levels` on the returned object name the coefficients it has.
  data[[group_var]] <- factor(data[[group_var]], levels = object$levels)
  # Read from the equation's own template, which is what parse_group_terms()
  # does for the same purpose. The formula-building side reads the parameters
  # off the formula it has built, for the reason add_formula_glef() records;
  # here there is no built formula yet and the two agree because both come
  # from bf_<model>.
  level_spec <- list(group_var = group_var, levels = object$levels,
                     nlpars = names(get(paste0("bf_", model))[[2]]),
                     disp = isTRUE(disp_by_level))
  disp_note <- if (isTRUE(disp_by_level) && has_disp_par(family)) {
    paste0(" and a separate ", disp_dpar(family), " per level")
  } else {
    ""
  }
  message("Refitting jointly as one ", model, " model with a separate ",
          paste0(level_spec$nlpars, collapse = ", "), " per level of \"",
          group_var, "\"", disp_note, ".")
  brm_args <- list(...)
  brm_args$family <- family
  refit <- fit_bayesnec(formula = formula, data = data, model = model,
                         brm_args = brm_args, level_spec = level_spec)
  out <- list(fit = refit$fit, model = model,
              bayesnecformula = refit$bayesnecformula, init = refit$init,
              group_var = group_var, levels = object$levels,
              disp_by_level = isTRUE(disp_by_level), data = data,
              family = family, model_weights = eq_weights,
              model_weight_share = share)
  allot_class(out, c("bayesnecjointfit", "bnecfit"))
}

#' The summed model weight of every equation in a grouped fit
#'
#' @param object An object of class \code{\link{bayesnecgroupfit}}.
#'
#' @details The \code{\link{bayesnecgroupfit}} analogue of
#' \code{\link{best_crossed}}. Each level's weights sum to one, so the summed
#' weight of an equation runs from zero to the number of levels and dividing by
#' that number gives the share of the available weight it holds. An equation
#' \code{check_models()} dropped from one level contributes zero there rather
#' than being excluded from the comparison, because a level that could not fit
#' it is evidence against fitting it everywhere.
#'
#' It does not go through \code{\link{crossed_group_weights}}, which refuses any
#' fit not weighted by pseudo-BMA. That refusal is about the crossed table,
#' whose factorisation holds for pseudo-BMA alone; a sum over levels needs no
#' such identity and is defined for stacking weights as well.
#'
#' @return A named \code{\link[base]{numeric}} vector, in decreasing order.
#'
#' @noRd
joint_equation_weights <- function(object) {
  per_level <- lapply(object$fits, fit_model_weights)
  models <- unique(unlist(lapply(per_level, names)))
  out <- vapply(models, function(m) {
    sum(vapply(per_level, function(w) {
      if (m %in% names(w)) w[[m]] else 0
    }, numeric(1)))
  }, numeric(1))
  sort(out, decreasing = TRUE)
}
