# Internals supporting the disp() term, which lets a family's dispersion
# parameter vary rather than being held constant across the whole curve.
#
# Two routes, told apart by what disp() is given:
#
#   disp(~x)        route (A) -- an ordinary brms distributional formula on the
#                   predictor. Descriptive: "noise is larger at high dose".
#   disp("power")   route (B) -- a variance function of the FITTED MEAN. A
#                   statement about the measurement process rather than about
#                   the dose axis, so it transports to a design with a
#                   different curve shape.
#
# The two are near-observationally equivalent for a monotone curve, since mu is
# then a monotone function of x. They separate under hormesis, or where a
# design revisits the same mu.
#
# Route (B) needs the curve expression written out a second time inside the
# dispersion formula, because mu is not in scope for another distributional
# parameter's formula in brms. Only the source is duplicated, not the fitted
# quantity: the curve parameters are shared, being declared once for the whole
# formula. bayesnec owns every curve expression in sysdata, so what is awkward
# by hand is mechanical here -- the same argument that make_hu_block() rests on.

#' Does this family have a free dispersion parameter?
#'
#' @param family Either a \code{\link[stats]{family}} object or a family tag.
#'
#' @return A \code{\link[base]{logical}}.
#'
#' @noRd
has_disp_par <- function(family) {
  fam_tag <- if (inherits(family, "family")) family$family else family
  isTRUE(fam_tag %in% names(disp_dpars))
}

#' Name brms gives the dispersion parameter for this family
#'
#' @param family Either a \code{\link[stats]{family}} object or a family tag.
#'
#' @return A \code{\link[base]{character}} string, or \code{NULL} where the
#' family has no free dispersion parameter.
#'
#' @noRd
disp_dpar <- function(family) {
  fam_tag <- if (inherits(family, "family")) family$family else family
  if (!has_disp_par(fam_tag)) {
    return(NULL)
  }
  unname(disp_dpars[[fam_tag]])
}

#' Extract the disp() specification from a bayesnec formula
#'
#' @param formula An object of class \code{\link{bayesnecformula}}.
#'
#' @return \code{NULL} where no disp() term is present, otherwise a
#' \code{\link[base]{list}} with elements \code{route} ("A" or "B") and
#' \code{value} (the sub-model formula for route A, the variance function name
#' for route B).
#'
#' @importFrom formula.tools rhs
#'
#' @noRd
parse_disp_term <- function(formula) {
  rhs_calls <- gsub("\\) \\+ ", ") impossiblestr ", deparse1(rhs(formula)))
  split_rhs_calls <- strsplit(rhs_calls, " impossiblestr ")[[1]]
  disp_str <- grep("disp(", split_rhs_calls, fixed = TRUE, value = TRUE)
  if (length(disp_str) == 0) {
    return(NULL)
  }
  if (length(disp_str) > 1) {
    stop("Your formula has more than one disp() term. Only one dispersion",
         " sub-model can be fitted. See ?bayesnecformula", call. = FALSE)
  }
  # The right-hand side is split on ") + ", which is how every other bayesnec
  # term is found. A disp() argument containing that sequence -- disp(~s(x) +
  # group) -- is therefore torn in half before it gets here. Report that rather
  # than let str2lang() fail on the fragment.
  arg <- try(
    str2lang(sub("^\\s*disp\\(", "", sub("\\)\\s*$", "", disp_str))),
    silent = TRUE
  )
  if (inherits(arg, "try-error")) {
    stop("Could not parse the disp() term \"", disp_str, "\". A sub-model",
         " combining a function call with another term, as in",
         " disp(~s(x) + group), cannot currently be written inline. Compute",
         " the term in your data first and pass it as a plain variable.",
         call. = FALSE)
  }
  if (inherits(arg, "formula") || identical(as.character(arg)[1], "~")) {
    # A one-sided formula, e.g. disp(~x): route (A). Kept as text and rebuilt
    # rather than evaluated, so that terms such as s(x) survive untouched for
    # brms to interpret.
    list(route = "A", value = deparse1(as.formula(arg)[[2]]))
  } else {
    list(route = "B", value = eval(arg))
  }
}

#' Validate a disp() specification against the family and the response
#'
#' @param spec The output of \code{\link{parse_disp_term}}.
#' @param family An object of class \code{\link[stats]{family}}.
#' @param response A \code{\link[base]{numeric}} vector, or \code{NULL} to skip
#' the data-dependent checks.
#'
#' @return \code{NULL}, invisibly. Called for its side effect of erroring.
#'
#' @noRd
check_disp_spec <- function(spec, family, response = NULL) {
  fam_tag <- if (inherits(family, "family")) family$family else family
  if (is_hurdle_family(fam_tag)) {
    stop("A disp() term is not currently supported for the two-block family ",
         fam_tag, ". Its dispersion parameter belongs to the response block",
         " alone, and coupling a variance function to one block of a joint fit",
         " needs a decision about the other that has not been made. Fit the",
         " response block on its own with bnec_hurdle() if you need this.",
         call. = FALSE)
  }
  if (!has_disp_par(fam_tag)) {
    stop("Family ", fam_tag, " has no free dispersion parameter, so there is",
         " nothing for disp() to model: its variance is a deterministic",
         " function of its mean. Over-dispersion here is remedied by changing",
         " family -- poisson to negbinomial, binomial to beta_binomial -- or",
         " by an observation-level group-level effect. See ?dispersion for the",
         " diagnostic that applies to these families.", call. = FALSE)
  }
  if (spec$route == "B") {
    if (!is.character(spec$value) || length(spec$value) != 1) {
      stop("The variance function passed to disp() must be a single character",
           " string naming one of: ",
           paste0("\"", names(disp_functions), "\"", collapse = ", "),
           ". To model dispersion on the predictor instead, pass a formula,",
           " e.g. disp(~x). See ?bayesnecformula", call. = FALSE)
    }
    if (!spec$value %in% names(disp_functions)) {
      stop("\"", spec$value, "\" is not a known variance function. bayesnec",
           " currently implements: ",
           paste0("\"", names(disp_functions), "\"", collapse = ", "), ".",
           call. = FALSE)
    }
    vf <- disp_functions[[spec$value]]
    if (!fam_tag %in% vf$families) {
      stop("Variance function \"", spec$value, "\" is not valid for the ",
           fam_tag, " family. It applies to: ",
           paste0(vf$families, collapse = ", "), ".", call. = FALSE)
    }
    # Every implemented form takes log(mu), so a fitted mean reaching zero is
    # undefined rather than merely awkward. Only the gaussian family can put mu
    # at or below zero: Gamma and negbinomial are positive by construction, and
    # the beta families live on (0, 1). Testing the response is a proxy for
    # testing the fitted curve -- a strictly positive response can still be
    # fitted with a negative lower asymptote -- so this catches the common case
    # and Stan reports the rest at initialisation.
    if (isTRUE(vf$positive_mu) && identical(fam_tag, "gaussian") &&
          !is.null(response) && any(response <= 0, na.rm = TRUE)) {
      stop("Variance function \"", spec$value, "\" takes log(mu), but your",
           " response reaches ", signif(min(response, na.rm = TRUE), 3),
           " so the fitted mean crosses zero. This is the growth-rate case:",
           " a specific growth rate, yield or increment can be negative, and a",
           " power law in mu is undefined there. Use disp(~x) to model",
           " dispersion on the predictor instead, or fit on a strictly",
           " positive scale and derive the rate afterwards.", call. = FALSE)
    }
  }
  invisible(NULL)
}

#' Non-linear parameter names introduced by a disp() specification
#'
#' @param spec The output of \code{\link{parse_disp_term}}.
#'
#' @return A \code{\link[base]{character}} vector, empty for route (A), which
#' introduces ordinary population-level terms rather than non-linear ones.
#'
#' @noRd
disp_pars <- function(spec) {
  if (is.null(spec) || spec$route != "B") {
    return(character(0))
  }
  disp_functions[[spec$value]]$pars
}

#' Build the dispersion sub-model for a bayesnec equation
#'
#' @param model A \code{\link[base]{character}} string naming a bayesnec model.
#' @param spec The output of \code{\link{parse_disp_term}}.
#' @param dpar The dispersion parameter's name, e.g. "sigma".
#' @param x_var The predictor column name, substituted for the generic "x".
#'
#' @return A \code{\link[base]{list}} with elements \code{nlf} (the block's
#' formula) and, for route (B), \code{lf} (the parameter formula).
#'
#' @importFrom stats as.formula
#'
#' @noRd
make_disp_block <- function(model, spec, dpar, x_var) {
  if (spec$route == "A") {
    return(list(nlf = as.formula(paste0(dpar, " ~ ", spec$value)), lf = NULL))
  }
  vf <- disp_functions[[spec$value]]
  bf_obj <- get(paste0("bf_", model))
  curve <- substitute_x_in_formula(x_var, deparse1(bf_obj$formula[[3]]))
  # Wrapped in parentheses because the curve is substituted into log(@MU@) and
  # into log(1 - (@MU@)); an unwrapped sum would rebind against the surrounding
  # operators for the second of those.
  rhs <- gsub("@MU@", paste0("(", curve, ")"), vf$expr, fixed = TRUE)
  list(nlf = as.formula(paste0(dpar, " ~ ", rhs)),
       lf = as.formula(paste0(paste0(vf$pars, collapse = " + "), " ~ 1")))
}

#' Append the dispersion sub-model to a brms formula
#'
#' @param brms_bf An object of class \code{\link[brms]{brmsformula}}.
#' @param model A \code{\link[base]{character}} string naming a bayesnec model.
#' @param spec The output of \code{\link{parse_disp_term}}.
#' @param family An object of class \code{\link[stats]{family}}.
#' @param x_var The predictor column name.
#'
#' @return An object of class \code{\link[brms]{brmsformula}}.
#'
#' @importFrom brms nlf lf
#'
#' @noRd
add_disp_block <- function(brms_bf, model, spec, family, x_var) {
  dpar <- disp_dpar(family)
  db <- make_disp_block(model, spec, dpar, x_var)
  if (spec$route == "A") {
    # lf() rather than nlf(): the right-hand side is an ordinary linear
    # predictor in the data, so brms should apply its usual design-matrix
    # handling and let terms such as s(x) work.
    return(brms_bf + lf(db$nlf))
  }
  brms_bf + nlf(db$nlf) + lf(db$lf)
}
