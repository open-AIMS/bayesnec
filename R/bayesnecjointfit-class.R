#' Class \code{bayesnecjointfit} of one model fitted across the levels of a
#' factor
#'
#' Returned by \code{\link{bnec_joint}} applied to a
#' \code{\link{bayesnecgroupfit}}. Each level is fitted the equation its own
#' weights favour, with every curve parameter taking a separate value per level
#' of the grouping factor within a single posterior.
#'
#' @name bayesnecjointfit-class
#' @aliases bayesnecjointfit bayesnecjointfit-class
#' @docType class
#'
#' @details The object holds the fit and the level structure and nothing
#' derived from a prediction grid. A level term is a population-level term, so
#' \code{re_formula = NA} does not drop it and the prediction grid has to name
#' a level: \code{\link{bnec_newdata}} on a joint refit returns
#' \code{resolution} rows for each level rather than \code{resolution} rows,
#' with the level varying slowest.
#'
#' \code{\link{ecx}}, \code{\link{nsec}}, \code{\link{nec}} and
#' \code{\link{ecnsec}} therefore return a
#' \code{\link[base]{data.frame}} with one row per level, named in a
#' \code{level} column, rather than the named vector of three they return for a
#' \code{\link{bayesnecfit}}. That is the same table a
#' \code{\link{bayesnecgroupfit}} returns, so the two routes can be read
#' against each other. \code{autoplot()} draws one panel per level and
#' \code{\link{ggbnec_data}} names the level in a \code{panel} column, again
#' matching the grouped fit.
#'
#' Each level is estimated on its own grid, one level at a time, rather than
#' from one prediction spanning them all. A control posterior, an ECx crossing
#' and an NSEC search each read a single monotone curve, and a grid holding
#' every level concatenated is not one: the crossing search would find the
#' first level's crossing and report it for the fit.
#'
#' The coefficient one level holds on one parameter is named
#' \code{b_<parameter>_<group_var><level>} in the draws --- \code{b_top_sitea}
#' for level \code{"a"} of a grouping variable named \code{site} --- where
#' every level was fitted the same equation. Where they were not, each level's
#' parameters are \pkg{brms} non-linear parameters in their own right and the
#' name is \code{b_<parameter><level tag>_Intercept}, the tag being the level
#' name reduced to letters and digits followed by \code{Lv} and its rank in the
#' sorted level set. \code{print()} reports the tags.
#'
#' See \code{methods(class = "bayesnecjointfit")} for an overview of available
#' methods.
#'
#' @slot fit The \code{\link[brms]{brmsfit}}.
#' @slot model The equation fitted at every level, or \code{NA} where the
#' levels favoured different equations.
#' @slot models A named \code{\link[base]{character}} vector, the equation
#' fitted at each level.
#' @slot level_spec The level structure the fit was built from: the level
#' tags, indicator columns and mask values where the equations differ, and the
#' grouping variable and levels either way.
#' @slot level_weights The weight the equation fitted at each level holds at
#' that level, from the grouped fit. A level's own weights sum to one, so this
#' is the share of that level's evidence behind the equation used there.
#' @slot bayesnecformula The \code{\link{bayesnecformula}} the fit was built
#' from, carrying the equation of the first level.
#' @slot init The initial values the fit was given.
#' @slot group_var The name of the grouping column.
#' @slot levels The factor levels, in the order their coefficients are in.
#' @slot disp_by_level Whether the family's dispersion parameter was also
#' given a value per level.
#' @slot data The \code{\link[base]{data.frame}} fitted, all levels.
#' @slot family The family, carried over from the grouped fit.
#' @slot model_weights The summed model weight of every equation across the
#' levels of the grouped fit, in decreasing order.
#' @slot model_weight_share The share of the summed weight the forced equation
#' holds, between 0 and 1, where \code{model} named one. \code{NA} where each
#' level was fitted its own equation, because no equation was imposed on the
#' set for such a share to describe, and \code{NA} where \code{model} named an
#' equation no level fitted.
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
  if (length(unique(x$models)) == 1) {
    cat("  equation          :", unname(x$models[[1]]), "\n")
  } else {
    cat("  equations         :",
        paste0(x$levels, " = ", unname(x$models), collapse = ", "), "\n")
  }
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
  cat("  weight at level   :",
      paste0(x$levels, " ", signif(unname(x$level_weights), 3),
             collapse = ", "), "\n")
  if (length(unique(x$models)) == 1) {
    cat("\nEvery curve parameter takes a value per level in one posterior;\n",
        "read them from the draws as b_<parameter>_", x$group_var,
        "<level>.\n", sep = "")
  } else {
    cat("\nEach level has its own equation and its own curve parameters in\n",
        "one posterior; read them from the draws as\n",
        "b_<parameter><level tag>_Intercept, the tags being ",
        paste0(x$levels, " = ", unname(unlist(x$level_spec$tags)),
               collapse = ", "), ".\n", sep = "")
  }
  cat("ecx(), nsec() and nec() return one row per level, and autoplot()\n",
      "draws one panel per level.\n", sep = "")
  invisible(x)
}
