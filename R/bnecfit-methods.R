#' Concatenate multiple \code{\link{bnecfit}} objects into one single
#' \code{\link{bayesmanecfit}} object containing Bayesian model averaging
#' statistics.
#'
#' @details The combined set is weighted by the method the objects being
#' combined were weighted by, where every input that records one names the same
#' method. Where none records a method --- combining two
#' \code{\link{bayesnecfit}} objects, for instance --- the \pkg{bayesnec}
#' default of "pseudobma" is used. Where two inputs name different methods the
#' default is used and a message reports the disagreement.
#' \code{\link[base]{c}} takes no \code{loo_controls} argument; use
#' \code{\link{amend}} to set the weighting method explicitly.
#'
#' Each equation keeps the record of the installation of \pkg{bayesnec} that
#' fitted it, and a message reports a combined set whose equations were fitted
#' by different installations. See \code{\link{bnec_version}}.
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
  inputs <- c(list(x), dots)
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
  # Preserve c()'s input-level compatibility rule even where duplicate model
  # names are removed above. expand_manec() reads these sources only to decide
  # whether one retained frame can describe the combined set.
  attr(mod_fits, "retained_data_sources") <- inputs
  formulas <- lapply(mod_fits, extract_formula)
  # c() has no loo_controls argument, so keep the weighting method where every
  # input that records one names the same method. Otherwise expand_manec()
  # supplies the default, and a disagreement is reported.
  methods_in <- unique(unlist(lapply(c(list(x), dots), fit_weights_method)))
  if (length(methods_in) > 1) {
    message("The objects being combined were weighted by different methods (",
            paste0(methods_in, collapse = ", "), "); using the default",
            " \"pseudobma\". Use ?amend to set the method explicitly.")
    methods_in <- NULL
  }
  loo_controls <- list(fitting = list(),
                       weights = weights_controls(methods_in))
  out <- expand_manec(mod_fits, formulas, loo_controls = loo_controls)
  if (length(out) == 1) {
    x
  } else {
    out <- allot_class(out, c("bayesmanecfit", "bnecfit"))
    out <- attach_version(out, do.call(rbind, lapply(inputs, stored_version)))
    report_mixed_versions(out)
    out
  }
}

#' "Add" multiple \code{\link{bnecfit}} objects into one single
#' \code{\link{bayesmanecfit}} object containing Bayesian model averaging
#' statistics.
#'
#' @details Shares the implementation of \code{\link[base]{c}}, including how
#' the weighting method of the combined set is decided.
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
#' @details The models are refitted with the family they were fitted with
#' unless \code{family} is supplied. No family is read off \code{newdata}, so
#' new data alone never change the family and never require
#' \code{force_fit = TRUE}. The new data are checked against the requirements
#' of the family the refit uses, as \code{\link{bnec}} checks its data. A value
#' at a boundary that family excludes, such as a zero for \code{Gamma} or a 1
#' for \code{beta}, is shifted off it, and the shift is reported and stored in
#' the fit's \code{\link{bnec_record}} where it has one. A value the family
#' admits is passed on as recorded, so a \code{gaussian} fit keeps an exact 1
#' and a joint \code{hurdle_gamma} fit keeps its zeros. A bounded response at
#' one of its bounds in every observation is refused before any model is
#' refitted, unless the fit is of the constant equation \code{ecxflat} alone.
#'
#' A \code{family} passed through \code{...} is read exactly as
#' \code{\link{bnec}} reads it: the link is assigned by \pkg{bayesnec} unless
#' the caller writes one, an unsupported link is refused before any model is
#' refitted, and the validated family is the one \pkg{brms} receives. See the
#' \emph{The link} section of \code{\link{bnec}}. Supplying a family that
#' differs from the fitted one requires \code{force_fit = TRUE}, because the
#' priors carried over from the original fit were built for the original family.
#' Without it the call is refused before the data are checked, so nothing is
#' reported as shifted.
#' Families are compared by name and links, so \code{Gamma(link = "identity")}
#' supplied for a Gamma fit is not a change of family.
#'
#' A family the stored formula cannot describe is refused before any model is
#' refitted, whether or not \code{force_fit} is set. A \code{disp()} term is
#' checked against it as \code{\link{bnec}} checks it, so \code{poisson} is
#' refused for a fit with a \code{disp()} term. A family that names its
#' dispersion parameter differently from the fitted one is refused as well,
#' such as \code{gaussian}, whose parameter is \code{sigma}, for a \code{Beta}
#' fit, whose parameter is \code{phi}. So is a change that adds or removes the
#' second block of a two-block family such as \code{hurdle_gamma}. Fit such a
#' model with \code{\link{bnec}}. New data are checked against a variance
#' function in \code{disp()} as \code{\link{bnec}} checks its data.
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
#' @param force_fit Whether to refit where \code{family} names a family or
#' link that differs from the one the model was fitted with. New data alone
#' never require it.
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
#' # original `manec_example` is fit with a Gaussian. Refitting it to the
#' # original `nec_data$y` with a Beta distribution changes the family, so it
#' # requires `force_fit = TRUE`, and a message says the priors may not suit it.
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
  # Check before the refit, which samples every model again.
  check_resolution(resolution)
  # Read before recover_prebayesnecfit() replaces `object`: expand_manec() and
  # expand_nec() build a new object, so the record must be re-attached at the
  # end.
  bnec_rec <- attr(object, "bnec_record")
  retained_data <- object[["retained_data"]]
  supplied_newdata <- !is.null(newdata)
  # update() refits a set; it does not reweight it. Keep the method the set
  # was built with unless the caller names another.
  old_method <- fit_weights_method(object)
  object <- recover_prebayesnecfit(object)
  # Fill in the method on both branches: a call that changes only a LOO
  # fitting argument names no method, and must not reweight the set.
  # amend_model_set() does the same, and the two must agree.
  if (missing(loo_controls)) {
    loo_controls <- list(fitting = list(),
                         weights = weights_controls(old_method))
  } else {
    loo_controls <- validate_loo_controls(loo_controls,
                                          object[[1]]$fit$family$family)
    if (is.null(loo_controls$weights$method)) {
      # Assigned into the existing list rather than replacing it, so any other
      # loo_model_weights() argument the caller set is kept.
      loo_controls$weights$method <- old_method
    }
  }
  dot_args <- list(...)
  # Validate the family here, not in brms::update(). Beta() and
  # Beta(link = "logit") are identical objects, so the caller's intent is
  # readable only from the unevaluated expression. Use substitute(), not
  # match.call(), which records a `...` forwarded from
  # update.bayesnechurdlefit() as `..1`. See family_link_source().
  if ("family" %in% names(dot_args)) {
    link_source <- family_link_source(substitute(list(...))[-1]$family,
                                      env = parent.frame())
    dot_args$family <- validate_family(dot_args$family,
                                       link_source = link_source)
  }
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
  # Before the force_fit guard, because setting force_fit cannot make the
  # stored formula fit a family it does not describe.
  if (!is.null(newdata) || "family" %in% names(dot_args)) {
    check_refit_family(
      extract_formula(object[[1]]), object[[1]]$fit$family,
      refit_family(object[[1]]$fit$family, dot_args$family),
      data = if (is.null(newdata)) object[[1]]$fit$data else newdata
    )
  }
  # Compare the families before checking the data, so that a refused call
  # reports no shift. New data keep the fitted family; only a supplied one
  # can differ.
  if (family_changed(dot_args$family, object[[1]]$fit$family)) {
    if (!force_fit) {
      stop("The family or link supplied differs from the one the model was\n",
           "  fitted with. The priors reused from the original fit were built\n",
           "  for the original family and may not suit the new one.\n",
           "If this was intentional, set `force_fit = TRUE`; otherwise\n",
           "  please use function `bnec` instead to redefine priors.",
           call. = FALSE)
    } else {
      message("The family or link supplied differs from the one the model was\n",
              "  fitted with. The priors reused from the original fit were\n",
              "  built for the original family and may not suit the new one.\n",
              "  You may want to consider refitting models from scratch via\n",
              "  function `bnec`.")
    }
  }
  if (!is.null(newdata) || "family" %in% names(dot_args)) {
    data_to_check <- if (is.null(newdata)) object[[1]]$fit$data else newdata
    checked <- check_update_data(object, data_to_check, dot_args$family,
                                 on_fit = !is.null(bnec_rec))
    # The substitutions describe the data this call fitted, so they replace
    # bnec()'s; the candidate set is unchanged. Assign with `[`, not `$<-`,
    # which deletes the name when the value is NULL: attach_bnec_record()
    # keeps the element present and NULL.
    if (!is.null(bnec_rec)) {
      bnec_rec["substitutions"] <- list(checked$substitutions)
    }
    # Pass the corrected frame to brms::update(), so that the boundary shift
    # check_data() reported is the one fitted. This applies on the family-only
    # route too, where the stored data were checked. Where nothing was
    # corrected, newdata stays NULL, which tells brms to reuse the stored data
    # and keeps the data_name attribute it prints.
    if (!is.null(newdata) || !identical(checked$data, data_to_check)) {
      newdata <- checked$data
    }
  }
  # The marker validate_family() uses to stay idempotent is bayesnec's own and
  # the family is stored in the brmsfit, so it is dropped before brms sees it.
  if ("family" %in% names(dot_args)) {
    dot_args$family <- unmark_family(dot_args$family)
  }
  for (i in seq_along(object)) {
    fit_i <- object[[i]]$fit
    # Build a call rather than forward `...`, which cannot be rewritten to hold
    # the validated family. Keep newdata a symbol: brms deparses it into the
    # data_name attribute it prints, and do.call() would inline the value.
    upd_call <- as.call(c(quote(update), quote(fit_i),
                          list(formula. = NULL, newdata = quote(newdata),
                               recompile = quote(recompile)), dot_args))
    object[[i]]$fit <- try(eval(upd_call), silent = FALSE)
    if (inherits(object[[i]]$fit, "try-error")) {
      class(object[[i]]) <- "somethingwentwrong"
    }
    object[[i]]$call_args <- update_call_args(object[[i]]$call_args, dot_args)
  }
  formulas <- lapply(object, extract_formula)
  if (length(object) > 1) {
    mod_fits <- expand_manec(object, formula = formulas, x_range = x_range,
                             resolution = resolution, sig_val = sig_val,
                             loo_controls = loo_controls)
    # Guard on the length of the result, not of the set: where all but one
    # model fails to refit, expand_manec() returns a one-element list that is
    # not a bayesmanecfit. Route the survivor through expand_nec(), as bnec()
    # and amend() do.
    if (length(mod_fits) > 1) {
      out <- allot_class(mod_fits, c("bayesmanecfit", "bnecfit"))
    } else {
      surviving <- names(mod_fits)
      mod_fits <- expand_nec(mod_fits[[1]], formula = formulas[[surviving]],
                             x_range = x_range, resolution = resolution,
                             sig_val = sig_val, loo_controls = loo_controls,
                             model = surviving)
      out <- allot_class(mod_fits, c("bayesnecfit", "bnecfit"))
    }
  } else if (length(object) == 1) {
    if (inherits(object[[1]], "somethingwentwrong")) {
      stop("Your attempt to update the original model(s) failed. Perhaps you",
           " specified incorrect arguments? See ?update.bnecfit")
    }
    mod_fits <- expand_nec(object[[1]], formula = formulas[[1]],
                           x_range = x_range, resolution = resolution,
                           sig_val = sig_val, loo_controls = loo_controls,
                           model = names(object))
    out <- allot_class(mod_fits, c("bayesnecfit", "bnecfit"))
  } else {
    stop("Stan failed to update your objects.")
  }
  # Re-attached rather than rebuilt: an object updated from one this version
  # fitted keeps its record, and one updated from an older fit has none to
  # keep, which is the documented meaning of a NULL record.
  if (!is.null(bnec_rec)) {
    attr(out, "bnec_record") <- bnec_rec
  }
  # update() refits every equation, so none keeps its earlier record.
  out <- attach_version(out)
  if (supplied_newdata) {
    out <- retain_unused_data(out, newdata)
  } else if (!is.null(retained_data)) {
    out$retained_data <- retained_data
  }
  out
}

#' Bring the record of a fitting call up to date after a refit
#'
#' @param call_args The \code{call_args} element of a
#' \code{\link{prebayesnecfit}}, or \code{NULL} for a fit that records none.
#' @param dot_args The \code{\link[brms]{brm}} arguments passed to
#' \code{\link{update.bnecfit}}.
#'
#' @return \code{call_args}, with a supplied \code{seed} in place of the
#' recorded one and a supplied \code{control} merged into the recorded one, as
#' \code{brms::update()} merges it. \code{NULL} where \code{call_args} is.
#'
#' @noRd
update_call_args <- function(call_args, dot_args) {
  if (is.null(call_args)) {
    return(NULL)
  }
  if ("seed" %in% names(dot_args)) {
    call_args["seed"] <- list(dot_args$seed)
  }
  if (!is.null(dot_args$control)) {
    control <- call_args$control
    control[names(dot_args$control)] <- dot_args$control
    call_args["control"] <- list(control)
  }
  call_args
}
