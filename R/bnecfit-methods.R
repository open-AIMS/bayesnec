#' Concatenate multiple \code{\link{bnecfit}} objects into one single
#' \code{\link{bayesmanecfit}} object containing Bayesian model averaging
#' statistics.
#'
#' @param x An object of class \code{\link{bnecfit}}.
#' @param ... Additional objects of class \code{\link{bnecfit}}.
#'
#' @return An object of class \code{\link{bayesmanecfit}}.
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' nec4param <- pull_out(manec_example, model = "nec4param")
#' ecx4param <- pull_out(manec_example, model = "ecx4param")
#' # Go from two bayesnecfit objects to a bayesmanecfit object.
#' #   In this example case it is redundant because it recovers the original
#' #   `manec_example`.
#' c(nec4param, ecx4param)
#' # Add a bayesnecfit object to an existing bayesmanecfit object
#' nechorme4 <- nec_data |>
#'   dplyr::mutate(y = qlogis(y)) |>
#'   (\(.)bnec(formula = y ~ crf(x, model = "nechorme4"),
#'             data = ., iter = 200, warmup = 150, chains = 2,
#'             stan_model_args = list(save_dso = FALSE)))()
#' c(nechorme4, manec_example)
#' }
#'
#' @export
c.bnecfit <- function(x, ...) {
  dots <- list(...)
  if (!all(c(is_bnecfit(x), sapply(dots, is_bnecfit)))) {
    stop("All objects must be an object fitted by bnec.")
  } else {
    mod_fits <- recover_prebayesnecfit(x)
    for (i in seq_along(dots)) {
      mod_fits <- c(mod_fits, recover_prebayesnecfit(dots[[i]]))
    }
    check_data_equality(mod_fits)
  }
  mod_fits <- mod_fits[!duplicated(names(mod_fits))]
  formulas <- lapply(mod_fits, extract_formula)
  out <- expand_manec(mod_fits, formulas)
  if (length(out) == 1) {
    x
  } else {
    allot_class(out, c("bayesmanecfit", "bnecfit"))
  }
}

#' "Add" multiple \code{\link{bnecfit}} objects into one single
#' \code{\link{bayesmanecfit}} object containing Bayesian model averaging
#' statistics.
#'
#' @param e1 An object of class \code{\link{bnecfit}}.
#' @param e2 An object of class \code{\link{bnecfit}}.
#'
#' @return An object of class \code{\link{bayesmanecfit}}.
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' nec4param <- pull_out(manec_example, model = "nec4param")
#' ecx4param <- pull_out(manec_example, model = "ecx4param")
#' # Go from two bayesnecfit objects to a bayesmanecfit object.
#' #   In this example case it is redundant because it recovers the original
#' #   `manec_example`.
#' nec4param + ecx4param
#' # Add a bayesnecfit object to an existing bayesmanecfit object
#' nechorme4 <- nec_data |>
#'   dplyr::mutate(y = qlogis(y)) |>
#'   (\(.)bnec(formula = y ~ crf(x, model = "nechorme4"),
#'             data = ., iter = 200, warmup = 150, chains = 2,
#'             stan_model_args = list(save_dso = FALSE)))()
#' nechorme4 + manec_example
#' }
#'
#' @export
`+.bnecfit` <- function(e1, e2) {
  if (is.null(e2)) {
    return(e1)
  }
  if (!all(sapply(list(e1, e2), is_bnecfit))) {
    stop("Cannot add \"", class(e2)[1], "\" objects.")
  }
  c(e1, e2)
}

#' Update an object of class \code{\link{bnecfit}} as fitted by function
#' \code{\link{bnec}}.
#'
#' @inheritParams bnec
#'
#' @param object An object of class \code{\link{bnecfit}} as fitted by function
#' \code{\link{bnec}}.
#' @param newdata Optional \code{\link[base]{data.frame}} to update the model
#' with new data. Data-dependent default priors will not be updated
#' automatically.
#' @param recompile A \code{\link[base]{logical}}, indicating whether the Stan
#' model should be recompiled. If \code{NULL} (the default), \code{update}
#' tries to figure out internally, if recompilation is necessary. Setting it to
#' \code{FALSE} will cause all Stan code changing arguments to be ignored.
#' @param force_fit Should model truly be updated in case either
#' \code{newdata} of a new family is provided?
#'
#' @return An object of class \code{\link{bnecfit}}. If one single model is
#' returned, then also an object of class \code{\link{bayesnecfit}}; otherwise,
#' if multiple models are returned, also an object of class
#' \code{\link{bayesmanecfit}}.
#'
#' @importFrom stats update
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' data(manec_example)
#' # due to package size issues, `manec_example` does not contain original
#' # stanfit DSO, so need to recompile here
#' smaller_manec <- update(manec_example, chains = 2, iter = 50,
#'                         recompile = TRUE)
#' # original `manec_example` is fit with a Gaussian
#' # change to Beta distribution by adding newdata with original `nec_data$y`
#' # function will throw informative message.
#' beta_manec <- update(manec_example, newdata = nec_data, recompile = TRUE,
#'                      chains = 2, iter = 50,
#'                      family = Beta(link = "identity"), force_fit = TRUE)
#' }
#'
#' @export
update.bnecfit <- function(object, newdata = NULL, recompile = NULL,
                           x_range = NA, resolution = 1000, sig_val = 0.01,
                           loo_controls, force_fit = FALSE, ...) {
  original_class <- grep("bayes", class(object), value = TRUE)
  if (!original_class %in% c("bayesnecfit", "bayesmanecfit")) {
    stop("Object is not of class bayesnecfit or bayesmanecfit.")
  }
  object <- recover_prebayesnecfit(object)
  dot_args <- list(...)
  simdat <- extract_simdat(object[[1]])
  if ("chains" %in% names(dot_args)) {
    if (dot_args$chains < simdat$chains) {
      stop(
        "The number of specified chains (", dot_args$chains, ")",
        " cannot be less than what the original model object contains",
        " (", simdat$chains, ") when the original model object has a seed."
      )
    }
  }
  if (!is.null(newdata) || "family" %in% names(dot_args)) {
    data_to_check <- if (is.null(newdata)) object[[1]]$fit$data else newdata
    changed_family <- has_family_changed(object, data_to_check, dot_args$family)
  } else {
    changed_family <- FALSE
  }
  if (changed_family) {
    if (!force_fit) {
      stop("You either input new data which might be best fitted with a\n",
           "  different distribution, or you indicated a new family/link.\n",
           "Either change might require different priors than originally\n",
           "  defined. If this was intentional, set `force_fit = TRUE`;\n",
           "  otherwise please use function `bnec` instead to redefine priors.",
           call. = FALSE)
    } else {
      message("You either input new data which might be best fitted with a\n",
              "  different distribution, or you indicated a new family/link.\n",
              "Either change might require different priors than originally\n",
              "  defined. You may want to consider refitting models from\n",
              "  scratch via function `bnec`.")
    }
  }
  for (i in seq_along(object)) {
    object[[i]]$fit <- try(update(object[[i]]$fit, formula. = NULL,
                                  newdata = newdata, recompile = recompile,
                                  ...), silent = FALSE)
    if (inherits(object[[i]]$fit, "try-error")) {
      class(object[[i]]) <- "somethingwentwrong"
    }
  }
  formulas <- lapply(object, extract_formula)
  if (length(object) > 1) {
    object <- expand_manec(object, formula = formulas, x_range = x_range,
                           resolution = resolution, sig_val = sig_val,
                           loo_controls = loo_controls)
    allot_class(object, c("bayesmanecfit", "bnecfit"))
  } else if (length(object) == 1) {
    if (inherits(object[[1]], "somethingwentwrong")) {
      stop("Your attempt to update the original model(s) failed. Perhaps you",
           " specified incorrect arguments? See ?update.bnecfit")
    }
    mod_fits <- expand_nec(object[[1]], formula = formulas[[1]],
                           x_range = x_range, resolution = resolution,
                           sig_val = sig_val, loo_controls = loo_controls,
                           model = names(object))
    allot_class(mod_fits, c("bayesnecfit", "bnecfit"))
  } else {
    stop("Stan failed to update your objects.")
  }
}
