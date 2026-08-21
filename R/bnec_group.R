#' Fit a concentration-response model separately at each level of a factor
#'
#' Fits the model set independently within each level of a grouping factor,
#' model-averaging within each level, and returns a
#' \code{\link{bayesnecgroupfit}}. Crossed model weights across levels are then
#' available from the per-level weights alone.
#'
#' @param formula A \code{\link{bayesnecformula}}, as passed to
#' \code{\link{bnec}}.
#' @param data A \code{\link[base]{data.frame}} containing the data.
#' @param group_var A \code{\link[base]{character}} string naming the column of
#' \code{data} that carries the factor.
#' @param family A \code{\link[stats]{family}} function, string, or \code{NULL}.
#' If \code{NULL} it is chosen once from the \emph{whole} response and passed
#' down --- see Details.
#' @param ... Further arguments passed to \code{\link{bnec}} for every level.
#'
#' @details \bold{Why the levels can be fitted separately}
#'
#' Factor levels partition the data disjointly and share no parameters, so the
#' log likelihood is a sum over levels and the expected log predictive density
#' is additive. Under pseudo-BMA --- the package default --- the crossed model
#' weights are then exactly the outer product of the per-level weight vectors,
#' which is the same identity \code{\link{crossed_weights}} rests on for the two
#' blocks of a hurdle fit. See \code{\link{crossed_group_weights}}.
#'
#' \bold{The family is chosen once}
#'
#' \code{\link{set_distribution}} applied to each subset could select different
#' families at different levels, which would make the levels incomparable ---
#' their \code{elpd} contributions would not be on the same scale and the
#' crossed weights would be meaningless. So it is chosen from the whole response
#' and passed down.
#'
#' \bold{Dispersion is per level, deliberately}
#'
#' Separate fits give each level its own \code{sigma}/\code{shape}/\code{phi}.
#' That is a feature, not an oversight: a shared dispersion parameter would
#' break the factorisation the crossed weights depend on. A model with structure
#' spanning levels --- shared dispersion, a group-level effect crossing levels,
#' an explicit contrast --- is a different model, and is not what this function
#' fits.
#'
#' \bold{What this does not do}
#'
#' It does not refit the favoured combination jointly. That is stage 2 of #33
#' and lands in the post-processing that the \code{toxval} migration moves, so
#' it is deliberately out of scope here.
#'
#' @return An object of class \code{\link{bayesnecgroupfit}}.
#'
#' @seealso \code{\link{bnec}}, \code{\link{crossed_group_weights}},
#' \code{\link{bayesnecgroupfit}}
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' data(nec_data)
#' nec_data$site <- rep(c("a", "b"), length.out = nrow(nec_data))
#' fit <- bnec_group(y ~ crf(x, "nec3param"), data = nec_data,
#'                   group_var = "site")
#' nec(fit)
#' crossed_group_weights(fit)
#' }
#'
#' @export
bnec_group <- function(formula, data, group_var, family = NULL, ...) {
  if (!is.character(group_var) || length(group_var) != 1) {
    stop("`group_var` must be a single column name.", call. = FALSE)
  }
  if (!group_var %in% names(data)) {
    stop("`group_var` \"", group_var, "\" is not a column of `data`.",
         call. = FALSE)
  }
  # Coerced up front, as bnec() does. Without it `model.frame(formula, data)`
  # below dispatches to stats::model.frame, which evaluates crf(x, "nec3param")
  # as an ordinary call returning a length-1 string -- and reports "variable
  # lengths differ", which points nowhere near the actual cause.
  formula <- bayesnecformula(formula)
  grp <- data[[group_var]]
  if (is.numeric(grp)) {
    stop("The grouping column \"", group_var, "\" is numeric. A factor",
         " covariate must be a character or a factor, so that its levels are",
         " unambiguous; a numeric column is almost always a predictor that",
         " belongs in crf() instead.", call. = FALSE)
  }
  grp <- factor(grp)
  levs <- levels(grp)
  if (length(levs) < 2) {
    stop("The grouping column \"", group_var, "\" has ", length(levs),
         " level; there is nothing to compare. Use bnec() directly.",
         call. = FALSE)
  }
  counts <- table(grp)
  if (any(counts < 4)) {
    thin <- names(counts)[counts < 4]
    stop("Level(s) ", paste0("\"", thin, "\"", collapse = ", "),
         " have fewer than 4 observations. Each level is fitted as a complete",
         " concentration-response model in its own right, so it needs enough",
         " data to support one.", call. = FALSE)
  }
  # Chosen once, from the whole response, for the reason in Details.
  if (is.null(family)) {
    mod_dat <- model.frame(formula, data = data)
    y <- retrieve_var(mod_dat, "y_var", error = TRUE)
    tr <- retrieve_var(mod_dat, "trials_var")
    family <- set_distribution(y, support_integer = TRUE, trials = tr)
    message("Family chosen once from the whole response: ", family,
            ". Pass `family` to override.")
  }
  family <- validate_family(family)
  fits <- vector(mode = "list", length = length(levs))
  names(fits) <- levs
  for (i in seq_along(levs)) {
    message("Fitting level \"", levs[i], "\" (", counts[[levs[i]]],
            " observations).")
    fits[[i]] <- bnec(formula, data = data[grp == levs[i], , drop = FALSE],
                      family = family, ...)
  }
  out <- list(fits = fits, group_var = group_var, levels = levs,
              formula = formula, data = data, family = family,
              n = as.integer(counts[levs]))
  allot_class(out, c("bayesnecgroupfit", "bnecfit"))
}

#' Crossed model weights across the levels of a factor
#'
#' The weight of every combination of per-level models, and the two readings of
#' that table which answer different questions.
#'
#' @param object An object of class \code{\link{bayesnecgroupfit}}.
#'
#' @details Levels partition the data disjointly and share no parameters, so
#' \code{elpd} is additive across them:
#' \code{elpd(m_1, ..., m_G) = sum_g elpd_g(m_g)}. Under pseudo-BMA the crossed
#' weight of a combination is proportional to the product of its per-level
#' weights, so the whole table follows from the per-level vectors and never has
#' to be materialised --- which matters, because with 23 models and \emph{G}
#' levels it has \code{23^G} cells.
#'
#' \bold{This identity is specific to pseudo-BMA}, the package default.
#' Stacking optimises a different objective whose solution is not generally an
#' outer product; stacked crossed weights would require the full pointwise
#' matrix and are not computed here. The same caveat applies to
#' \code{\link{crossed_weights}} for hurdle fits, for the same reason.
#'
#' \bold{Two readings, both useful}
#'
#' The \strong{unrestricted} maximum picks the best model for each level
#' independently, and will typically assign different equations to different
#' levels. That is the direct answer to the question this function exists for:
#' whether the functional form of the response changes across levels.
#'
#' The \strong{diagonal} maximum, \code{w_m proportional to prod_g w_gm}, is a
#' comparison of \emph{common-form} models: which single equation best describes
#' every level. \code{bayesnec} could not answer that before, and it is often
#' the question a reader of the analysis actually has.
#'
#' @return A \code{\link[base]{list}} with the per-level weight vectors, the
#' unrestricted best combination and its weight, and the diagonal weights over
#' the models common to every level.
#'
#' @seealso \code{\link{bnec_group}}, \code{\link{crossed_weights}}
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' crossed_group_weights(fit)
#' }
#'
#' @importFrom stats setNames
#'
#' @export
crossed_group_weights <- function(object) {
  if (!is_bayesnecgroupfit(object)) {
    stop("crossed_group_weights requires an object of class",
         " bayesnecgroupfit.", call. = FALSE)
  }
  wt <- function(x) {
    if (inherits(x, "bayesmanecfit")) {
      setNames(x$mod_stats$wi, rownames(x$mod_stats))
    } else {
      setNames(1, x$model)
    }
  }
  per_level <- lapply(object$fits, wt)
  best <- vapply(per_level, function(w) names(w)[which.max(w)], character(1))
  best_weight <- prod(vapply(per_level, max, numeric(1)))
  # The diagonal is only defined over models every level actually fitted; a
  # model dropped from one level by check_models() cannot be the common form.
  common <- Reduce(intersect, lapply(per_level, names))
  diagonal <- if (length(common) > 0) {
    d <- vapply(common, function(m) {
      prod(vapply(per_level, function(w) w[[m]], numeric(1)))
    }, numeric(1))
    if (sum(d) > 0) d / sum(d) else d
  } else {
    numeric(0)
  }
  list(per_level = per_level,
       best_combination = best,
       best_weight = best_weight,
       common_models = common,
       diagonal = sort(diagonal, decreasing = TRUE))
}
