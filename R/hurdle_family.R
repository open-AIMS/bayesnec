# Internals supporting the joint two-block families, where the response and the
# probability of a zero are two parameter blocks inside a single brms fit.
#
# brms names that second block "hu" for the hurdle families and "zi" for the
# zero-inflated ones. Structurally they are the same model -- zero-inflation
# only differs from a hurdle when the base distribution can itself emit zeros,
# which neither Gamma nor Beta can -- so bayesnec treats them identically and
# carries the name through rather than branching on it. The block is
# distinguished in a fit by that name used as a prefix on every non-linear
# parameter: `top` belongs to mu, `hutop` or `zitop` to the second block.

#' Is this family one of the joint two-block (hurdle / zero-inflated) families?
#'
#' @param family Either a \code{\link[stats]{family}} object or a family tag.
#'
#' @return A \code{\link[base]{logical}}.
#'
#' @noRd
is_hurdle_family <- function(family) {
  fam_tag <- if (inherits(family, "family")) family$family else family
  isTRUE(fam_tag %in% names(hurdle_fams))
}

#' Name brms gives the second parameter block for this family
#'
#' @param family Either a \code{\link[stats]{family}} object or a family tag.
#'
#' @return A \code{\link[base]{character}} string, "hu" or "zi".
#'
#' @noRd
hurdle_dpar <- function(family) {
  fam_tag <- if (inherits(family, "family")) family$family else family
  unname(hurdle_fams[[fam_tag]])
}

#' Family whose defaults the mu block should reuse
#'
#' The mu block describes the response with the zeros set aside, so its priors
#' and initial values should come from whatever family that subset looks like:
#' Gamma for hurdle_gamma, Beta for zero_inflated_beta, and the corresponding
#' count family for hurdle_poisson and hurdle_negbinomial.
#'
#' @param family Either a \code{\link[stats]{family}} object or a family tag.
#'
#' @return An object of class \code{\link[stats]{family}}.
#'
#' @importFrom stats Gamma poisson
#' @importFrom brms Beta negbinomial
#'
#' @noRd
hurdle_mu_family <- function(family) {
  fam_tag <- if (inherits(family, "family")) family$family else family
  switch(unname(hurdle_mu_fams[[fam_tag]]),
         Gamma = Gamma(link = "identity"),
         beta = Beta(link = "identity"),
         poisson = poisson(link = "identity"),
         negbinomial = negbinomial(link = "identity"),
         stop("No mu family defined for ", fam_tag, ".", call. = FALSE))
}

#' Non-linear parameter names of the second block for a given model
#'
#' @param model A \code{\link[base]{character}} string naming a bayesnec model.
#' @param dpar The second block's name, "hu" or "zi".
#'
#' @return A \code{\link[base]{character}} vector.
#'
#' @noRd
hu_pars <- function(model, dpar) {
  paste0(dpar, names(get(paste0("bf_", model))$pforms))
}

#' Build the zero-probability sub-model for a bayesnec equation
#'
#' Derives the second block mechanically from an existing bayesnec equation
#' rather than requiring a hand-written counterpart for each of the 23. Every
#' non-linear parameter is given the block's prefix and the right-hand side is
#' wrapped as \code{1 - (...)}, so that the curve being modelled is the
#' probability of a *non*-zero -- declining with the predictor, matching the
#' sign convention of every bayesnec equation -- while the block itself, the
#' probability of a zero, increases.
#'
#' @param model A \code{\link[base]{character}} string naming a bayesnec model.
#' @param dpar The second block's name, "hu" or "zi".
#'
#' @return A \code{\link[base]{list}} with elements \code{nlf} (the block's
#' formula), \code{lf} (the parameter formula) and \code{pars}.
#'
#' @importFrom stats as.formula
#'
#' @noRd
make_hu_block <- function(model, dpar) {
  bf_obj <- get(paste0("bf_", model))
  pars <- names(bf_obj$pforms)
  rhs <- deparse1(bf_obj$formula[[3]])
  # Longest name first, so that "top" inside an already-substituted "hutop" is
  # never rewritten a second time. Word boundaries alone are not enough,
  # because bayesnec parameter names are substrings of one another (bot/top).
  for (p in pars[order(nchar(pars), decreasing = TRUE)]) {
    rhs <- gsub(paste0("(?<![[:alnum:]_])", p, "(?![[:alnum:]_])"),
                paste0(dpar, p), rhs, perl = TRUE)
  }
  list(nlf = as.formula(paste0(dpar, " ~ 1 - (", rhs, ")")),
       lf = as.formula(paste0(paste0(dpar, pars, collapse = " + "), " ~ 1")),
       pars = paste0(dpar, pars))
}

#' Append the zero-probability block to a brms formula
#'
#' @param brms_bf An object of class \code{\link[brms]{brmsformula}}.
#' @param model A \code{\link[base]{character}} string naming a bayesnec model.
#' @param x_var The predictor column name, substituted for the generic "x".
#' @param dpar The second block's name, "hu" or "zi".
#'
#' @return An object of class \code{\link[brms]{brmsformula}}.
#'
#' @importFrom brms nlf lf
#' @importFrom stats as.formula
#'
#' @noRd
add_hu_block <- function(brms_bf, model, x_var, dpar) {
  hb <- make_hu_block(model, dpar)
  rhs <- substitute_x_in_formula(x_var, deparse1(hb$nlf[[3]]))
  brms_bf + nlf(as.formula(paste0(dpar, " ~ ", rhs))) + lf(hb$lf)
}

#' Proportion of non-zero responses at each unique predictor value
#'
#' Used to prime the second block's initial-value search: the sub-model is
#' written as \code{1 - <non-zero probability>}, so the curve being initialised
#' is that probability.
#'
#' @param predictor A \code{\link[base]{numeric}} vector.
#' @param response A \code{\link[base]{numeric}} vector; zero denotes the
#' hurdle, e.g. an individual that did not survive.
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

#' Split a two-block response into its two component views
#'
#' @param predictor A \code{\link[base]{numeric}} vector.
#' @param response A \code{\link[base]{numeric}} vector.
#'
#' @return A \code{\link[base]{list}} with a \code{mu} element (predictor and
#' response restricted to non-zeros) and a \code{hu} element (unique predictor
#' values and the proportion non-zero at each).
#'
#' @noRd
split_hurdle_response <- function(predictor, response) {
  keep <- response > 0
  list(mu = list(x = predictor[keep], y = response[keep]),
       hu = survival_by_x(predictor, response))
}
