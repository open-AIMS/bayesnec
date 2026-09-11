#' Concatenate multiple \code{\link{bnecfit}} objects into one single
#' \code{\link{bayesmanecfit}} object containing Bayesian model averaging
#' statistics.
#'
#' @details The combined set is weighted by the method the objects being
#' combined were weighted by, where every input that records one names the same
#' method. Where none records a method --- combining two
#' \code{\link{bayesnecfit}} objects, for instance --- or where two inputs
#' disagree, the \pkg{bayesnec} default of "pseudobma" is used and the
#' disagreement is reported. \code{\link[base]{c}} takes no
#' \code{loo_controls} argument; use \code{\link{amend}} to set the
#' weighting method explicitly.
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
  # c() has no loo_controls argument, so the only place an explicit weighting
  # request can come from is the objects being combined. Inherited where every
  # input that records a method names the same one, which is what amend() and
  # pull_out() do for the set they operate on; otherwise the documented default
  # is supplied by expand_manec(). Two inputs weighted differently have no
  # answer that is right for both, so the fallback is reported rather than
  # chosen silently. See #320.
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
    allot_class(out, c("bayesmanecfit", "bnecfit"))
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
#' @details A \code{family} passed through \code{...} is read exactly as
#' \code{\link{bnec}} reads it: the link is assigned by \pkg{bayesnec} unless
#' the caller writes one, an unsupported link is refused before any model is
#' refitted, and the validated family is the one \pkg{brms} receives. See the
#' \emph{The link} section of \code{\link{bnec}}. Supplying a family that
#' differs from the fitted one requires \code{force_fit = TRUE}, because the
#' priors carried over from the original fit were built for the original family.
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
  # Read before recover_prebayesnecfit() replaces `object` with the list of
  # prebayesnecfits. expand_manec() and expand_nec() build a new object, so an
  # attribute not re-attached at the end of this method is lost, and
  # bnec_record() then returned NULL for a fit this version had recorded --
  # which is the one thing the documented NULL is supposed to rule out.
  bnec_rec <- attr(object, "bnec_record")
  # Read here for the same reason as the record above, and used only where the
  # caller named no loo_controls of their own. update() refits an existing set;
  # it is not a request to reweight it, so the method the set was built with is
  # the one it keeps. Without this the method was whatever loo defaulted to,
  # so a set the caller had asked to weight by stacking came back weighted by
  # something else. See #320.
  old_method <- fit_weights_method(object)
  object <- recover_prebayesnecfit(object)
  if (missing(loo_controls)) {
    loo_controls <- list(fitting = list(),
                         weights = weights_controls(old_method))
  }
  dot_args <- list(...)
  # The family is validated at this entry point rather than forwarded untouched
  # to brms::update(). Beta() and Beta(link = "logit") produce identical family
  # objects, so the only place the caller's intent can be read is the
  # unevaluated expression, and only the function it was written in can see it.
  # substitute() rather than match.call(): a `...` forwarded from
  # update.bayesnechurdlefit() is recorded by match.call() as the placeholder
  # `..1`. See family_link_source() and #256.
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
  if (!is.null(newdata) || "family" %in% names(dot_args)) {
    data_to_check <- if (is.null(newdata)) object[[1]]$fit$data else newdata
    checked <- check_update_data(object, data_to_check, dot_args$family,
                                 on_fit = !is.null(bnec_rec))
    changed_family <- checked$changed_family
    # The substitutions are those of the data this call fitted, so they replace
    # rather than add to the ones bnec() recorded. The candidate set is not
    # touched by update(), so `requested`, `attempted` and `excluded` are
    # carried through unchanged.
    #
    # Single-bracket assignment of a one-element list, not `$<-`: `$<-` with a
    # NULL value deletes the name, so an update that substituted nothing
    # returned a three-name record where bnec() and amend() return four. The
    # element has to stay present and NULL, which is how attach_bnec_record()
    # builds it.
    if (!is.null(bnec_rec)) {
      bnec_rec["substitutions"] <- list(checked$substitutions)
    }
    # The corrected frame replaces the caller's newdata, so a boundary shift
    # check_data() reported is the one brms::update() is given. Without this
    # the message was emitted and the correction thrown away. See #274.
    #
    # The family-only route needs the substitution too, and guarding this on
    # `!is.null(newdata)` alone left it out. update(family = Beta(link =
    # "identity"), force_fit = TRUE) on a fit whose stored response holds an
    # exact 0 or 1 checks object[[1]]$fit$data, reports the shift, and then
    # passed newdata = NULL to brms::update() -- so brms refitted the
    # unshifted stored data and Stan failed on the boundary just reported
    # repaired. That is #274's failure mode on the second of its two routes.
    #
    # NULL is still passed where nothing was corrected. That is what tells
    # brms to reuse the stored data rather than treat it as new, and it keeps
    # the data_name attribute brms deparses and prints.
    if (!is.null(newdata) || !identical(checked$data, data_to_check)) {
      newdata <- checked$data
    }
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
  # The marker validate_family() uses to stay idempotent is bayesnec's own and
  # the family is stored in the brmsfit, so it is dropped before brms sees it.
  if ("family" %in% names(dot_args)) {
    dot_args$family <- unmark_family(dot_args$family)
  }
  for (i in seq_along(object)) {
    fit_i <- object[[i]]$fit
    # Assembled as a call rather than forwarded through `...`, because the
    # family brms must receive is the validated one and `...` cannot be
    # rewritten. do.call() would work but would inline newdata as a value, and
    # brms deparses that argument's expression into the data_name attribute it
    # prints -- so newdata is kept as a symbol here.
    upd_call <- as.call(c(quote(update), quote(fit_i),
                          list(formula. = NULL, newdata = quote(newdata),
                               recompile = quote(recompile)), dot_args))
    object[[i]]$fit <- try(eval(upd_call), silent = FALSE)
    if (inherits(object[[i]]$fit, "try-error")) {
      class(object[[i]]) <- "somethingwentwrong"
    }
  }
  formulas <- lapply(object, extract_formula)
  if (length(object) > 1) {
    mod_fits <- expand_manec(object, formula = formulas, x_range = x_range,
                             resolution = resolution, sig_val = sig_val,
                             loo_controls = loo_controls)
    # Guard on the length of the result, not on the length of the set that went
    # in. Where all but one model fails to refit, expand_manec() returns a bare
    # one-element list of prebayesnecfit, which has none of a bayesmanecfit's
    # structure; classing that as one gave an object whose every method failed
    # on a missing `mod_fits`. bnec() and amend() already guard this way and
    # route the single survivor through expand_nec(); the three entry points
    # now agree, which is what stopped this being noticed. See #288.
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
  out
}
