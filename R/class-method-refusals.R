#' Explain an unsupported fit-class method
#'
#' @param generic The generic name.
#' @param object The supplied fit.
#' @param ... Text describing a supported alternative.
#' @return Never returns; raises a class-specific error.
#' @noRd
refuse_fit_method <- function(generic, object, ...) {
  stop(generic, "() is unavailable for class ", class(object)[1], ". ",
       ..., call. = FALSE)
}

#' Check a formula before fitting it
#'
#' @param formula A fitted \code{bayesnecfit}.
#' @param ... Unused.
#' @return An error naming \code{bayesnecfit}; pass
#' \code{formula(fit)} and its data to \code{check_formula()}.
#' @method check_formula bayesnecfit
#' @export
check_formula.bayesnecfit <- function(formula, ...) {
  refuse_fit_method("check_formula", formula,
                    "Pass formula(fit) and the fitting data to check_formula().")
}

#' @rdname check_formula.bayesnecfit
#' @return An error naming \code{bayesmanecfit}; select one model with
#' \code{formula(fit, model = name)} and pass its data to
#' \code{check_formula()}.
#' @method check_formula bayesmanecfit
#' @export
check_formula.bayesmanecfit <- function(formula, ...) {
  refuse_fit_method("check_formula", formula,
                    "Select one model with formula(fit, model = name) and",
                    " pass its data to check_formula().")
}

#' @rdname check_formula.bayesnecfit
#' @return An error naming \code{bayesnechurdlefit}; check a component
#' formula and its data.
#' @method check_formula bayesnechurdlefit
#' @export
check_formula.bayesnechurdlefit <- function(formula, ...) {
  refuse_fit_method("check_formula", formula,
                    "Check formula(fit)$growth or formula(fit)$survival",
                    " with that component's data.")
}

#' @rdname check_formula.bayesnecfit
#' @return An error naming \code{bayesnecgroupfit}; check one level's
#' formula and data.
#' @method check_formula bayesnecgroupfit
#' @export
check_formula.bayesnecgroupfit <- function(formula, ...) {
  refuse_fit_method("check_formula", formula,
                    "Check formula(fit, level = name) with that level's data.")
}

#' @rdname check_formula.bayesnecfit
#' @return An error naming \code{bayesnecjointfit}; check the formula
#' before fitting with \code{bnec_joint()}.
#' @method check_formula bayesnecjointfit
#' @export
check_formula.bayesnecjointfit <- function(formula, ...) {
  refuse_fit_method("check_formula", formula,
                    "Check the formula and data before bnec_joint().")
}

#' Refuse posterior comparison on one ordinary fit
#'
#' @param x A \code{bayesnecfit}.
#' @param ... Unused.
#' @return An error naming \code{bayesnecfit}; supply a named list of fits.
#' @method compare_posterior bayesnecfit
#' @export
compare_posterior.bayesnecfit <- function(x, ...) {
  refuse_fit_method("compare_posterior", x,
                    "Pass a named list of fits to compare_posterior().")
}

#' @rdname compare_posterior.bayesnecfit
#' @return An error naming \code{bayesmanecfit}; supply a named list of fits.
#' @method compare_posterior bayesmanecfit
#' @export
compare_posterior.bayesmanecfit <- function(x, ...) {
  refuse_fit_method("compare_posterior", x,
                    "Pass a named list of fits to compare_posterior().")
}

#' @rdname compare_posterior.bayesnecfit
#' @return An error naming \code{bayesnechurdlefit}; supply a named list of fits.
#' @method compare_posterior bayesnechurdlefit
#' @export
compare_posterior.bayesnechurdlefit <- function(x, ...) {
  refuse_fit_method("compare_posterior", x,
                    "Pass a named list of fits to compare_posterior().")
}

#' Refuse a joint refit without separate level fits
#'
#' @param object A fitted ordinary or joint object.
#' @param ... Unused.
#' @return An error naming the supplied fit class and the required
#' grouped or hurdle input.
#' @method bnec_joint bayesnecfit
#' @export
bnec_joint.bayesnecfit <- function(object, ...) {
  refuse_fit_method("bnec_joint", object,
                    "Use a bayesnecgroupfit from bnec_group()",
                    " before bnec_joint().")
}

#' @rdname bnec_joint.bayesnecfit
#' @return An error naming \code{bayesmanecfit}; use grouped level fits.
#' @method bnec_joint bayesmanecfit
#' @export
bnec_joint.bayesmanecfit <- function(object, ...) {
  refuse_fit_method("bnec_joint", object,
                    "Use a bayesnecgroupfit from bnec_group()",
                    " before bnec_joint().")
}

#' @rdname bnec_joint.bayesnecfit
#' @return An error naming \code{bayesnecjointfit}; use
#' \code{update()} to refit it.
#' @method bnec_joint bayesnecjointfit
#' @export
bnec_joint.bayesnecjointfit <- function(object, ...) {
  refuse_fit_method("bnec_joint", object,
                    "Use update() to refit an existing joint fit.")
}

#' Refuse combination of joint refits
#'
#' @param x A \code{bayesnecjointfit}.
#' @param ... Other fits.
#' @return An error naming \code{bayesnecjointfit}; fit level model sets
#' with \code{bnec_group()} before a joint refit.
#' @method c bayesnecjointfit
#' @export
c.bayesnecjointfit <- function(x, ...) {
  refuse_fit_method("c", x,
                    "A joint refit cannot join a set. Fit candidate sets",
                    " with bnec_group() before bnec_joint().")
}

#' @rdname c.bayesnecjointfit
#' @param e1,e2 Fits to combine.
#' @return An error naming \code{bayesnecjointfit}; use
#' \code{bnec_group()} for candidate sets.
#' @method + bayesnecjointfit
#' @export
`+.bayesnecjointfit` <- function(e1, e2) {
  refuse_fit_method("+", e1,
                    "A joint refit cannot join a set. Fit candidate sets",
                    " with bnec_group() before bnec_joint().")
}

#' ECNSEC for an ordinary fit
#'
#' @param object A \code{bayesnecfit}.
#' @param ... Estimator settings.
#' @return A numeric summary or posterior vector for this fit.
#' @method ecnsec bayesnecfit
#' @export
ecnsec.bayesnecfit <- function(object, ...) ecnsec.bnecfit(object, ...)

#' @rdname ecnsec.bayesnecfit
#' @return A numeric model-averaged summary or posterior vector.
#' @method ecnsec bayesmanecfit
#' @export
ecnsec.bayesmanecfit <- function(object, ...) ecnsec.bnecfit(object, ...)

#' Exceedance probabilities for an ordinary fit
#'
#' @param object A \code{bayesnecfit}.
#' @param ... Threshold and estimator settings.
#' @return A one-row exceedance table for this fit.
#' @method exceedance bayesnecfit
#' @export
exceedance.bayesnecfit <- function(object, ...) exceedance.bnecfit(object, ...)

#' @rdname exceedance.bayesnecfit
#' @return A one-row model-averaged exceedance table.
#' @method exceedance bayesmanecfit
#' @export
exceedance.bayesmanecfit <- function(object, ...) {
  exceedance.bnecfit(object, ...)
}

#' @rdname exceedance.bayesnecfit
#' @return A one-row exceedance table for the combined hurdle endpoint.
#' @method exceedance bayesnechurdlefit
#' @export
exceedance.bayesnechurdlefit <- function(object, ...) {
  exceedance.bnecfit(object, ...)
}

#' Failed candidate models of a fit
#'
#' @param object A \code{bayesnecfit}.
#' @param ... Unused.
#' @return A \code{bnecfailures} list for this fit.
#' @method failed_models bayesnecfit
#' @export
failed_models.bayesnecfit <- function(object, ...) {
  failed_models.bnecfit(object, ...)
}

#' @rdname failed_models.bayesnecfit
#' @return A \code{bnecfailures} list for this model set.
#' @method failed_models bayesmanecfit
#' @export
failed_models.bayesmanecfit <- function(object, ...) {
  failed_models.bnecfit(object, ...)
}

#' @rdname failed_models.bayesnecfit
#' @return A \code{bnecfailures} list for this joint refit.
#' @method failed_models bayesnecjointfit
#' @export
failed_models.bayesnecjointfit <- function(object, ...) {
  failed_models.bnecfit(object, ...)
}

#' Dispatch addition across the supported fit classes
#'
#' All concrete fit classes share this function object so that R's Ops
#' dispatch reaches a class-specific refusal for a mixed pair.
#' @param e1,e2 Fits to combine.
#' @return A combined fit, or a class-specific error for an invalid pair.
#' @noRd
fit_add_dispatch <- function(e1, e2) {
  if (inherits(e1, "bayesnecjointfit") ||
      inherits(e2, "bayesnecjointfit")) {
    joint <- if (inherits(e1, "bayesnecjointfit")) e1 else e2
    return(refuse_fit_method("+", joint,
                             "A joint refit cannot join a set. Fit candidate",
                             " sets with bnec_group() before bnec_joint()."))
  }
  if (inherits(e1, "bayesnecgroupfit") ||
      inherits(e2, "bayesnecgroupfit")) {
    if (!inherits(e1, "bayesnecgroupfit") ||
        !inherits(e2, "bayesnecgroupfit")) {
      other <- if (inherits(e1, "bayesnecgroupfit")) e2 else e1
      stop("+ cannot combine bayesnecgroupfit with class ", class(other)[1],
           ". Combine two grouped fits with the same levels and data.",
           call. = FALSE)
    }
    return(c.bayesnecgroupfit(e1, e2))
  }
  if (inherits(e1, "bayesnechurdlefit") ||
      inherits(e2, "bayesnechurdlefit")) {
    if (!inherits(e1, "bayesnechurdlefit") ||
        !inherits(e2, "bayesnechurdlefit")) {
      other <- if (inherits(e1, "bayesnechurdlefit")) e2 else e1
      stop("+ cannot combine bayesnechurdlefit with class ",
           class(other)[1], ". Combine two hurdle fits instead.",
           call. = FALSE)
    }
    hurdle_check_pair(list(e1, e2))
    return(hurdle_rewrap(e1, e1$growth + e2$growth,
                         e1$survival + e2$survival))
  }
  add_bnec_fits(e1, e2)
}

`+.bayesnecfit` <- fit_add_dispatch
`+.bayesmanecfit` <- fit_add_dispatch
`+.bayesnechurdlefit` <- fit_add_dispatch
`+.bayesnecgroupfit` <- fit_add_dispatch
`+.bayesnecjointfit` <- fit_add_dispatch
