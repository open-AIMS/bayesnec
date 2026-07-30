# Internals supporting the joint hurdle families, where the response and the
# hurdle probability are two parameter blocks inside a single brms fit.
#
# The blocks are distinguished by a "hu" prefix on every non-linear parameter
# name: `top` belongs to mu, `hutop` to hu. Everything in this file exists to
# build, prime or take apart that second block.

#' Is this family one of the joint hurdle families?
#'
#' @param family Either a \code{\link[stats]{family}} object or a family tag.
#'
#' @return A \code{\link[base]{logical}}.
#'
#' @noRd
is_hurdle_family <- function(family) {
  fam_tag <- if (inherits(family, "family")) family$family else family
  isTRUE(fam_tag %in% hurdle_fams)
}

#' Non-linear parameter names of the hu block for a given model
#'
#' @param model A \code{\link[base]{character}} string naming a bayesnec model.
#'
#' @return A \code{\link[base]{character}} vector.
#'
#' @noRd
hu_pars <- function(model) {
  paste0("hu", names(get(paste0("bf_", model))$pforms))
}

#' Build the hu sub-model for a bayesnec equation
#'
#' Derives the hurdle-probability sub-model mechanically from an existing
#' bayesnec equation rather than requiring a hand-written counterpart for each
#' of the 23. Every non-linear parameter is given a \code{hu} prefix and the
#' right-hand side is wrapped as \code{1 - (...)}, so that the *survival* curve
#' declines with the predictor -- matching the sign convention of every
#' bayesnec equation -- while \code{hu} itself, the probability of a zero,
#' increases.
#'
#' @param model A \code{\link[base]{character}} string naming a bayesnec model.
#'
#' @return A \code{\link[base]{list}} with elements \code{nlf} (the \code{hu}
#' formula), \code{lf} (the parameter formula) and \code{pars}.
#'
#' @importFrom stats as.formula
#'
#' @noRd
make_hu_block <- function(model) {
  bf_obj <- get(paste0("bf_", model))
  pars <- names(bf_obj$pforms)
  rhs <- deparse1(bf_obj$formula[[3]])
  # Longest name first, so that "top" inside an already-substituted "hutop" is
  # never rewritten a second time. Word boundaries alone are not enough,
  # because bayesnec parameter names are substrings of one another (bot/top).
  for (p in pars[order(nchar(pars), decreasing = TRUE)]) {
    rhs <- gsub(paste0("(?<![[:alnum:]_])", p, "(?![[:alnum:]_])"),
                paste0("hu", p), rhs, perl = TRUE)
  }
  list(nlf = as.formula(paste0("hu ~ 1 - (", rhs, ")")),
       lf = as.formula(paste0(paste0("hu", pars, collapse = " + "), " ~ 1")),
       pars = paste0("hu", pars))
}

#' Append the hu block to a brms formula
#'
#' @param brms_bf An object of class \code{\link[brms]{brmsformula}}.
#' @param model A \code{\link[base]{character}} string naming a bayesnec model.
#' @param x_var The predictor column name, substituted for the generic "x".
#'
#' @return An object of class \code{\link[brms]{brmsformula}}.
#'
#' @importFrom brms nlf lf
#' @importFrom stats as.formula
#'
#' @noRd
add_hu_block <- function(brms_bf, model, x_var) {
  hb <- make_hu_block(model)
  rhs <- substitute_x_in_formula(x_var, deparse1(hb$nlf[[3]]))
  brms_bf + nlf(as.formula(paste0("hu ~ ", rhs))) + lf(hb$lf)
}

#' Survival proportion at each unique predictor value
#'
#' Used to prime the hu block's initial-value search: the hu sub-model is
#' written as \code{1 - survival}, so the curve being initialised is survival.
#'
#' @param predictor A \code{\link[base]{numeric}} vector.
#' @param response A \code{\link[base]{numeric}} vector; zero denotes a
#' non-survivor.
#'
#' @return A \code{\link[base]{list}} with elements \code{x} and \code{y}.
#'
#' @noRd
survival_by_x <- function(predictor, response) {
  ux <- sort(unique(predictor))
  p <- vapply(ux, function(z) mean(response[predictor == z] > 0), numeric(1))
  # Stan needs mu strictly inside (0, 1) under an identity link, and the
  # init-finder validates candidates against range(y); exact 0 and 1 make that
  # unsatisfiable. Clamp exactly as response_link_scale() does elsewhere.
  eps <- 1 / (2 * length(response))
  list(x = ux, y = pmin(pmax(p, eps), 1 - eps))
}

#' Split a hurdle response into its two component views
#'
#' @param predictor A \code{\link[base]{numeric}} vector.
#' @param response A \code{\link[base]{numeric}} vector.
#'
#' @return A \code{\link[base]{list}} with a \code{mu} element (predictor and
#' response restricted to survivors) and a \code{hu} element (unique predictor
#' values and the survival proportion at each).
#'
#' @noRd
split_hurdle_response <- function(predictor, response) {
  keep <- response > 0
  list(mu = list(x = predictor[keep], y = response[keep]),
       hu = survival_by_x(predictor, response))
}
