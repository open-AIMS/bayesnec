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
#' is the equation each level favours, composed into one model in which every
#' curve parameter takes a separate value per level of the grouping factor,
#' estimated in a single posterior.
#'
#' @param object An object of class \code{\link{bayesnechurdlefit}} returned by
#' \code{\link{bnec_hurdle}}, or of class \code{\link{bayesnecgroupfit}}
#' returned by \code{\link{bnec_group}}.
#' @param model An optional \code{\link[base]{character}} string naming the
#' equation to fit. For a hurdle fit it is the response block's equation and
#' defaults to the highest-weighted growth model in \code{object}. For a
#' grouped fit it forces one equation at every level; the default is instead
#' the equation each level's own weights favour, which may differ between
#' levels.
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
#' adds. That posterior supports a contrast between two levels conditional on
#' everything they share, which differencing the independent posteriors of a
#' \code{\link{bnec_group}} fit cannot.
#'
#' Each level is fitted the equation its own model weights favour, which is the
#' \code{\link{bayesnecgroupfit}} analogue of what \code{\link{best_crossed}}
#' does for a hurdle fit: that function returns the best growth equation and
#' the best survival equation separately, and the two blocks of one joint
#' hurdle model then carry different equations. One \pkg{brms} model can carry
#' a different functional form per level, because the mean is arbitrary
#' arithmetic over data columns and parameters: each level's equation is
#' multiplied by an indicator that is one on that level's rows and zero
#' elsewhere, and the terms are summed.
#'
#' Where every level favours the same equation the sum reduces to that equation
#' with each parameter's \code{~ 1} replaced by \code{~ 0 + <group_var>}, which
#' is dummy coding of the factor onto the curve, and that is what is built.
#' \code{model} forces one equation at every level, which is the same reduced
#' form.
#'
#' A group-level term cannot be added to a refit whose levels chose different
#' equations, because the levels then share no parameter for it to be written
#' on. Pass \code{model} to fit one equation everywhere, which is the form such
#' a term applies to.
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
  favoured <- joint_level_equations(object)
  if (is.null(model)) {
    # One equation per level, which is the bayesnecgroupfit analogue of
    # best_crossed(): that function returns the best growth equation and the
    # best survival equation separately, and bnec_joint() already composes two
    # different equations into one hurdle model from them. Taking the highest
    # summed weight instead would impose one form on levels that rejected it.
    models <- favoured
    # NA rather than a number. The summed-weight share describes an equation
    # imposed on every level, and no equation is imposed here, so reporting it
    # would put a figure on the object that says nothing about what was fitted.
    share <- NA_real_
  } else {
    if (!is.character(model) || length(model) != 1) {
      stop("`model` must name a single equation, which is then fitted at",
           " every level. Leave it out to fit each level the equation that",
           " level's weights favour; a set cannot be averaged over here, and",
           " the averaging belongs in the bnec_group() call that preceded it.",
           call. = FALSE)
    }
    models <- stats::setNames(rep(model, n_lev), object$levels)
    share <- if (model %in% names(eq_weights)) {
      unname(eq_weights[[model]]) / n_lev
    } else {
      NA_real_
    }
  }
  composed <- length(unique(models)) > 1
  if (is.null(formula)) {
    formula <- object$formula
  }
  formula <- bayesnecformula(formula, env = parent.frame())
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
  # The representative equation. The composed branch builds its own formula and
  # its own priors from `models`, but fit_bayesnec() still swaps one equation
  # into the crf() term for the model frame and the data check, and takes one
  # name for the failure record. The first level's is used because it is one of
  # the equations actually fitted rather than a stand-in for none of them.
  rep_model <- unname(models[[1]])
  formula <- swap_crf_model(formula, rep_model)
  level_spec <- list(group_var = group_var, levels = object$levels,
                     models = as.list(models), composed = composed,
                     nlpars = names(get(paste0("bf_", rep_model))[[2]]),
                     disp = isTRUE(disp_by_level))
  # `prior` and `init` name parameters, and on the composed branch the names
  # are the internal per-level ones (topaLv1 and the rest) that a caller has no
  # way to know before the call is made. Refused rather than silently
  # discarded: add_brm_defaults() validates a supplied prior against the
  # representative equation's parameter list, so a set written for the curve
  # parameters fails that check and is dropped without a word.
  dot_names <- names(list(...))
  if (composed && any(c("prior", "init") %in% dot_names)) {
    stop("`", paste0(intersect(c("prior", "init"), dot_names),
                     collapse = "` and `"),
         "` cannot be supplied where the levels favour different equations (",
         paste0("\"", object$levels, "\": ", unname(models), collapse = "; "),
         "), because each level's parameters are renamed for the composed",
         " model. Pass `model` to fit one equation at every level, which keeps",
         " the parameter names the equation's own.", call. = FALSE)
  }
  if (composed && !is.null(parse_group_terms(formula, rep_model))) {
    stop("The levels favour different equations (",
         paste0("\"", object$levels, "\": ", unname(models), collapse = "; "),
         "), so each level's curve is a separate set of parameters and a",
         " group-level term cannot be written across them. Pass `model` to",
         " fit one equation at every level, which is the form a group-level",
         " term applies to.", call. = FALSE)
  }
  disp_note <- if (isTRUE(disp_by_level) && has_disp_par(family)) {
    paste0(", and a separate ", disp_dpar(family), " per level")
  } else {
    ""
  }
  # Announced before the data are built rather than after, so that a failure in
  # building them is read against a call whose equation choice has been stated.
  if (composed) {
    message("Refitting jointly as one model composing ",
            paste0(unname(models), " at \"", object$levels, "\"",
                   collapse = ", "),
            ", each level with its own curve parameters", disp_note, ".")
  } else {
    message("Refitting jointly as one ", rep_model,
            " model with a separate ",
            paste0(names(get(paste0("bf_", rep_model))[[2]]),
                   collapse = ", "),
            " per level of \"", group_var, "\"", disp_note, ".")
  }
  if (composed) {
    level_spec <- c(level_spec,
                    compose_level_data(formula, data, object$levels,
                                       group_var))
    data <- level_spec$data
    level_spec$data <- NULL
    level_spec$nlpars <- unlist(lapply(object$levels, function(l) {
      paste0(names(get(paste0("bf_", models[[l]]))[[2]]),
             level_spec$tags[[l]])
    }))
  }
  brm_args <- list(...)
  brm_args$family <- family
  refit <- fit_bayesnec(formula = formula, data = data, model = rep_model,
                         brm_args = brm_args, level_spec = level_spec)
  out <- list(fit = refit$fit,
              model = if (composed) NA_character_ else rep_model,
              models = models, level_spec = level_spec,
              bayesnecformula = refit$bayesnecformula, init = refit$init,
              group_var = group_var, levels = object$levels,
              disp_by_level = isTRUE(disp_by_level), data = data,
              family = family, model_weights = eq_weights,
              level_weights = joint_level_weights(object, models),
              model_weight_share = share)
  allot_class(out, c("bayesnecjointfit", "bnecfit"))
}

#' The equation each level of a grouped fit favours
#'
#' @param object An object of class \code{\link{bayesnecgroupfit}}.
#'
#' @details The \code{\link{bayesnecgroupfit}} analogue of
#' \code{\link{best_crossed}}, which returns the best growth equation and the
#' best survival equation separately rather than one equation for the pair.
#'
#' @return A named \code{\link[base]{character}} vector, one equation per
#' level.
#'
#' @noRd
joint_level_equations <- function(object) {
  # Indexed by level rather than iterated over `fits` in whatever order they
  # are stored: the names attached afterwards would otherwise be a second,
  # independent ordering, and a mismatch would give a level the wrong
  # equation silently.
  out <- vapply(object$levels, function(l) {
    w <- fit_model_weights(object$fits[[l]])
    names(w)[which.max(w)]
  }, character(1))
  stats::setNames(out, object$levels)
}

#' The weight one equation holds at each level of a grouped fit
#'
#' @param object An object of class \code{\link{bayesnecgroupfit}}.
#' @param models A named \code{\link[base]{character}} vector, the equation to
#' report for each level.
#'
#' @details What the joint refit reports in place of the summed-weight share.
#' A level's own weights sum to one, so this is the share of that level's
#' evidence the equation fitted there holds, and it is a statement about the
#' fit rather than about an equation the fit does not use. An equation
#' \code{check_models()} dropped from a level holds zero there.
#'
#' @return A named \code{\link[base]{numeric}} vector, one per level.
#'
#' @noRd
joint_level_weights <- function(object, models) {
  out <- vapply(object$levels, function(l) {
    w <- fit_model_weights(object$fits[[l]])
    m <- unname(models[[l]])
    if (m %in% names(w)) unname(w[[m]]) else 0
  }, numeric(1))
  stats::setNames(out, object$levels)
}

#' The indicator columns and mask values a composed joint refit needs
#'
#' @param formula An object of class \code{\link{bayesnecformula}}.
#' @param data The data being fitted.
#' @param levels The factor levels, in coefficient order.
#' @param group_var The name of the factor column.
#'
#' @details Builds the three things \code{compose_level_formula()} reads that
#' are properties of the data rather than of the equations: a tag per level, an
#' indicator column per level written into the data, and the predictor value
#' each level's equation is evaluated at on the rows it does not own.
#'
#' The mask value is an \emph{observed} predictor value of that level, the
#' observation nearest its median, and it is that rather than the median itself
#' because the guard rests on it being a value the level already evaluates its
#' equation at. The median of an even number of observations is not one of
#' them.
#'
#' The predictor is taken from the model frame rather than from the data, so a
#' \code{crf(log(x), ...)} formula masks on the transformed scale the equation
#' is written on. Rows the model frame dropped are dropped from the level
#' vector with it, so the mask value is an observation the fit actually sees.
#'
#' @return A \code{\link[base]{list}} of \code{tags}, \code{inds},
#' \code{x_ref} and \code{data}.
#'
#' @importFrom stats model.frame median setNames
#'
#' @noRd
compose_level_data <- function(formula, data, levels, group_var) {
  tags <- level_par_tags(levels)
  inds <- setNames(paste0("bnecind", tags), levels)
  clash <- intersect(inds, names(data))
  if (length(clash) > 0) {
    stop("A joint refit across levels that favour different equations needs",
         " the column", if (length(clash) > 1) "s" else "", " ",
         paste0("\"", clash, "\"", collapse = ", "),
         ", which the data already ",
         if (length(clash) > 1) "hold" else "holds",
         ". Rename ", if (length(clash) > 1) "them" else "it",
         " and refit.", call. = FALSE)
  }
  mf <- model.frame(formula, data = data, run_par_checks = FALSE)
  keep <- seq_len(nrow(data))
  na_act <- attr(mf, "na.action")
  if (!is.null(na_act)) {
    keep <- keep[-as.integer(na_act)]
  }
  x_col <- retrieve_var(mf, "x_var", error = TRUE)
  lev_vec <- as.character(data[[group_var]])[keep]
  x_ref <- setNames(vapply(levels, function(l) {
    xv <- x_col[lev_vec == l & is.finite(x_col)]
    if (length(xv) == 0) {
      stop("Level \"", l, "\" has no usable predictor value, so a joint",
           " refit cannot be composed across the levels.", call. = FALSE)
    }
    xv[which.min(abs(xv - median(xv)))]
  }, numeric(1)), levels)
  for (l in levels) {
    data[[inds[[l]]]] <- as.numeric(as.character(data[[group_var]]) == l)
  }
  list(tags = as.list(tags), inds = as.list(inds), x_ref = as.list(x_ref),
       data = data)
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
