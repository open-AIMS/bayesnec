#' Class \code{bayesnecjointfit} of one model fitted across the levels of a
#' factor
#'
#' Returned by \code{\link{bnec_joint}} applied to a
#' \code{\link{bayesnecgroupfit}}. One equation is fitted, with every curve
#' parameter taking a separate value per level of the grouping factor within a
#' single posterior.
#'
#' @name bayesnecjointfit-class
#' @aliases bayesnecjointfit bayesnecjointfit-class
#' @docType class
#'
#' @details The object holds the fit and the level structure and nothing
#' derived from a prediction grid. \code{prediction_grid()} builds the
#' predictor and the columns a family needs and no level column, and a level
#' term is a population-level term, so \code{re_formula = NA} does not drop it
#' and \code{\link[brms]{posterior_epred}} stops on the column it cannot find.
#' \code{\link{ecx}}, \code{\link{nsec}}, \code{\link{nec}} and
#' \code{autoplot()} therefore do not report per level for a joint refit
#' yet. The failure is an error rather than an estimate for an unnamed level,
#' which is why the fit can be read for its parameters before those are
#' written.
#'
#' The coefficient one level holds on one parameter is named
#' \code{b_<parameter>_<group_var><level>} in the draws --- \code{b_top_sitea}
#' for level \code{"a"} of a grouping variable named \code{site}.
#'
#' See \code{methods(class = "bayesnecjointfit")} for an overview of available
#' methods.
#'
#' @slot fit The \code{\link[brms]{brmsfit}}.
#' @slot model The equation fitted at every level.
#' @slot bayesnecformula The \code{\link{bayesnecformula}} the fit was built
#' from, carrying the single equation.
#' @slot init The initial values the fit was given.
#' @slot group_var The name of the grouping column.
#' @slot levels The factor levels, in the order their coefficients are in.
#' @slot disp_by_level Whether the family's dispersion parameter was also
#' given a value per level.
#' @slot data The \code{\link[base]{data.frame}} fitted, all levels.
#' @slot family The family, carried over from the grouped fit.
#' @slot model_weights The summed model weight of every equation across the
#' levels of the grouped fit, in decreasing order.
#' @slot model_weight_share The share of the summed weight the fitted equation
#' holds, between 0 and 1, or \code{NA} where \code{model} named an equation
#' no level fitted.
#'
#' @seealso \code{\link{bnec_joint}}, \code{\link{bnec_group}},
#' \code{\link{bayesnecgroupfit}}
NULL

#' @noRd
is_bayesnecjointfit <- function(x) {
  inherits(x, "bayesnecjointfit")
}

#' @noRd
#' @method print bayesnecjointfit
#' @export
print.bayesnecjointfit <- function(x, ...) {
  cat("Object of class bayesnecjointfit\n\n")
  cat("  equation          :", x$model, "\n")
  cat("  grouping variable :", x$group_var, "\n")
  cat("  levels            :", paste0(x$levels, collapse = ", "), "\n")
  cat("  family            :", x$family$family,
      paste0("(link = ", x$family$link, ")"), "\n")
  cat("  dispersion        :",
      if (isTRUE(x$disp_by_level)) "per level" else "shared", "\n")
  if (!is.na(x$model_weight_share)) {
    cat("  summed weight     :", signif(x$model_weight_share, 3),
        "of the available weight across levels\n")
  }
  cat("\nEvery curve parameter takes a value per level in one posterior;\n",
      "read them from the draws as b_<parameter>_", x$group_var, "<level>.\n",
      "ecx(), nsec(), nec() and autoplot() do not yet report per level for\n",
      "a joint refit.\n", sep = "")
  invisible(x)
}
