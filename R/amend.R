#' Amends an existing \code{\link{bayesmanecfit}} or \code{\link{bayesnecfit}}
#' object
#'
#' Amends an existing \code{\link{bayesmanecfit}} or \code{\link{bayesnecfit}}
#' object, for example, by adding or removing fitted models.
#'
#' @inheritParams bnec
#'
#' @param object An object of class \code{\link{bayesmanecfit}} or
#' \code{\link{bayesnecfit}}, as returned by \code{\link{bnec}}.
#' @param drop A \code{\link[base]{character}} vector containing the names of
#' model types you which to exclude for the modified fit. A
#' \code{\link{bayesnecfit}} contains a single model, so there is nothing that
#' can be dropped from it and \code{drop} is an error in that case.
#' @param add A \code{\link[base]{character}} vector containing the names of
#' model types you which to include to the modified fit. Adding models to a
#' \code{\link{bayesnecfit}} promotes it to a \code{\link{bayesmanecfit}}.
#' @param priors An object of class \code{\link[brms]{brmsprior}} which
#' specifies user-desired prior distributions of model parameters.
#' If missing, \code{\link{amend}} will figure out a baseline prior for each
#' parameter. It can also be specified as a named \code{\link[base]{list}}
#' where each name needs to correspond to the same string as \code{model}. See
#' Details.
#' @param prior_type A \code{\link[base]{character}} string selecting the set of
#' default priors to build for any newly added models when \code{priors} is not
#' supplied. Either \code{"uninformative"} (the default) or
#' \code{"regularizing"}. See \code{\link{bnec}}. Note this is not automatically
#' inherited from the original fit; pass it explicitly to match the priors used
#' when the object was first fitted.
#' @param asymptote_observed A \code{\link[base]{logical}} declaring whether
#' the highest predictor level reached the lower asymptote, applied to the
#' default priors and the initial-value search of any newly added models. See
#' \code{\link{bnec}}. Not inherited from the original fit; pass it explicitly
#' to match the declaration that fit was built under.
#' @param predictor_scale A \code{\link[base]{character}} string declaring
#' whether the predictor is supplied as \code{"concentration"}, already
#' \code{"log"} transformed, or should use the existing \code{"auto"} rule for
#' priors generated for newly added models. See \code{\link{bnec}}. This is not
#' inherited from the original fit; pass it explicitly to match that fit.
#'
#' @details A model added through \code{add} is sampled with the settings of
#' the call that fitted \code{object}: \code{iter}, \code{warmup}, \code{thin},
#' \code{chains}, \code{sample_prior}, the backend, and any \code{control} or
#' \code{seed} that call passed to \code{\link[brms]{brm}}. They are read from
#' the first model of \code{object}. Other \code{\link[brms]{brm}} arguments,
#' such as \code{cores}, are not passed to the added model.
#'
#' Where \code{object} has a two-block family, such as \code{hurdle_gamma}, the
#' added model's survival block uses the equation the set was fitted with: the
#' one \code{model_survival} named, or the added model's own equation where
#' \code{model_survival} was not given.
#'
#' Where \code{object} does not record its call, as an object fitted by an
#' earlier version of \pkg{bayesnec} does not, no \code{control} or
#' \code{seed} is passed to the added model, and the survival-block equation
#' is read from the stored formulas. A single model whose survival block uses
#' its own equation is then read as fitted without \code{model_survival},
#' because the two calls store the same formula.
#' \code{\link{update.bnecfit}} creates a call record for such an object,
#' using the recovered survival-block equation and any \code{control} or
#' \code{seed} supplied to the update. Subsequent additions reuse that record.
#' \code{amend} also records the recovered equation on retained models, so
#' dropping models does not discard it.
#' When an update changes the backend or algorithm, \pkg{brms} does not reuse
#' the previous \code{control} or \code{seed}. Only settings supplied to that
#' update are then recorded for subsequent additions.
#'
#' @return All successfully fitted model fits. A \code{\link{bayesmanecfit}} if
#' more than one model remains, otherwise a \code{\link{bayesnecfit}}.
#'
#' @examples
#' library(bayesnec)
#' data(manec_example)
#' exmp <- amend(manec_example, drop = "nec4param")
#'
#' @export
amend <- function(object, drop, add, loo_controls, x_range = NA,
                  resolution = 1000, sig_val = 0.01, priors,
                  prior_type = "uninformative", asymptote_observed = TRUE,
                  timeout = Inf, predictor_scale = "auto") {
  UseMethod("amend")
}

#' amend.bayesmanecfit
#'
#' Modifies an existing \code{\link{bayesmanecfit}} object, for example, by
#' adding or removing fitted models.
#'
#' @inheritParams amend
#'
#' @inherit amend return examples
#'
#' @importFrom chk chk_character chk_numeric chk_number chk_flag
#'
#' @noRd
#'
#' @export
amend.bayesmanecfit <- function(object, drop, add, loo_controls, x_range = NA,
                                resolution = 1000, sig_val = 0.01, priors,
                                prior_type = "uninformative",
                                asymptote_observed = TRUE,
                                timeout = Inf, predictor_scale = "auto") {
  prior_type <- match.arg(prior_type, c("uninformative", "regularizing"))
  chk_flag(asymptote_observed)
  predictor_scale <- validate_predictor_scale(predictor_scale)
  chk_number(timeout)
  if (timeout <= 0) {
    stop("Argument `timeout` must be a positive number (or Inf).")
  }

  if (missing(drop) & missing(add) & missing(loo_controls)) {
    message(amend_general_error())
    return(object)
  }

  if (!missing(drop)) {chk_character(drop)}
  if (!missing(add)) {chk_character(add)}
  if (!is.na(x_range[1])) {chk_numeric(x_range)}
  chk_numeric(resolution)
  check_resolution(resolution)
  chk_numeric(sig_val)
  if(!inherits(object, "bayesmanecfit")){
    stop("object is not of class bayesmanecfit")
  }

  amend_model_set(
    object = object, mod_fits = object$mod_fits,
    old_method = fit_weights_method(object),
    drop = if (missing(drop)) NULL else drop,
    add = if (missing(add)) NULL else add,
    loo_controls = if (missing(loo_controls)) NULL else loo_controls,
    x_range = x_range, resolution = resolution, sig_val = sig_val,
    priors = if (missing(priors)) NULL else priors,
    prior_type = prior_type, asymptote_observed = asymptote_observed,
    predictor_scale = predictor_scale,
    timeout = timeout
  )
}

#' amend.bayesnecfit
#'
#' Modifies an existing \code{\link{bayesnecfit}} object, for example, by
#' adding further fitted models.
#'
#' @inheritParams amend
#'
#' @inherit amend return
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' data(manec_example)
#' single <- pull_out(manec_example, model = "nec4param")
#' both <- amend(single, add = "ecxexp")
#' }
#'
#' @importFrom chk chk_character chk_numeric chk_number chk_flag
#'
#' @noRd
#'
#' @export
amend.bayesnecfit <- function(object, drop, add, loo_controls, x_range = NA,
                              resolution = 1000, sig_val = 0.01, priors,
                              prior_type = "uninformative",
                              asymptote_observed = TRUE,
                              timeout = Inf, predictor_scale = "auto") {
  prior_type <- match.arg(prior_type, c("uninformative", "regularizing"))
  chk_flag(asymptote_observed)
  predictor_scale <- validate_predictor_scale(predictor_scale)
  chk_number(timeout)
  if (timeout <= 0) {
    stop("Argument `timeout` must be a positive number (or Inf).")
  }
  if (!missing(drop)) {
    chk_character(drop)
    # Refuse rather than ignore `drop`: a bayesnecfit holds one model, so
    # dropping it would leave nothing.
    stop("Cannot drop models from a bayesnecfit: it contains the single model",
         " \"", object$model, "\". Refit with bnec(..., model = ) to change",
         " which model is fitted.", call. = FALSE)
  }
  if (missing(add) & missing(loo_controls)) {
    message(amend_general_error())
    return(object)
  }
  if (!missing(add)) {chk_character(add)}
  if (!is.na(x_range[1])) {chk_numeric(x_range)}
  chk_numeric(resolution)
  check_resolution(resolution)
  chk_numeric(sig_val)
  # Promote the single fit to a one-model set and use the same worker as the
  # bayesmanecfit method, so that the two cannot differ.
  amend_model_set(
    object = object, mod_fits = recover_prebayesnecfit(object),
    old_method = NULL, drop = NULL,
    add = if (missing(add)) NULL else add,
    loo_controls = if (missing(loo_controls)) NULL else loo_controls,
    x_range = x_range, resolution = resolution, sig_val = sig_val,
    priors = if (missing(priors)) NULL else priors,
    prior_type = prior_type, asymptote_observed = asymptote_observed,
    predictor_scale = predictor_scale,
    timeout = timeout
  )
}

#' amend_general_error
#'
#' The message returned whenever there is nothing for \code{\link{amend}} to do.
#'
#' @return A \code{\link[base]{character}} string.
#'
#' @noRd
amend_general_error <- function() {
  paste(
    "Nothing to amend, please specify a proper model to either add or drop, or",
    "changes to loo_controls;\n Returning original model set."
  )
}

#' amend_model_set
#'
#' Workhorse behind the \code{\link{amend}} methods. Operates on a named
#' \code{\link[base]{list}} of \code{\link{prebayesnecfit}} objects so that a
#' single-model \code{\link{bayesnecfit}} and a multi-model
#' \code{\link{bayesmanecfit}} share one implementation.
#'
#' @param object The original object, returned unchanged when there is nothing
#' to amend.
#' @param mod_fits A named \code{\link[base]{list}} of
#' \code{\link{prebayesnecfit}} objects.
#' @param old_method The LOO weighting method used by \code{object}, or
#' \code{NULL}.
#'
#' @inheritParams amend
#'
#' @inherit amend return
#'
#' @noRd
amend_model_set <- function(object, mod_fits, old_method, drop = NULL,
                            add = NULL, loo_controls = NULL, x_range = NA,
                            resolution = 1000, sig_val = 0.01, priors = NULL,
                            prior_type = "uninformative",
                            asymptote_observed = TRUE,
                            predictor_scale = "auto", timeout = Inf) {
  general_error <- amend_general_error()
  if (!is.null(loo_controls)) {
    fam_tag <- mod_fits[[1]]$fit$family$family
    loo_controls <- validate_loo_controls(loo_controls, fam_tag)
    # Test is.null(), not the name, as define_loo_controls() does: a method
    # given as NULL is not a request for one.
    if (is.null(loo_controls$weights$method)) {
      loo_controls$weights$method <- old_method
    }
    is_new_method_old <- identical(loo_controls$weights$method, old_method)
    if (length(loo_controls$fitting) == 0 & is_new_method_old) {
      message("No new LOO fitting/weighting arguments have been specified;",
              " ignoring argument loo_controls.")
      if (is.null(drop) & is.null(add)) {
        message(general_error)
        return(object)
      }
    }
  } else {
    # A bayesnecfit records no weighting method. Leave `weights` empty rather
    # than pass method = NULL, which loo_model_weights() resolves to stacking;
    # expand_manec() fills it with the pseudo-BMA default.
    loo_controls <- list(fitting = list(),
                         weights = weights_controls(old_method))
  }
  bnec_rec <- attr(object, "bnec_record")
  model_set <- names(mod_fits)
  if (!is.null(drop)) {
    model_set <- handle_set(model_set, drop = drop)
  }
  if (!is.null(add)) {
    model_set <- handle_set(model_set, add = add)
  }
  if (any(model_set == "wrong_model_output")) {
    message(general_error)
    return(object)
  }
  simdat <- extract_simdat(mod_fits[[1]])
  data <- mod_fits[[1]]$fit$data
  family <- mod_fits[[1]]$fit$family
  formula <- mod_fits[[1]]$bayesnecformula
  # Narrow the formula's environment, as bnec() does: amend() sends the model
  # frame, which holds it, to every worker. A fit made by an earlier version
  # of bayesnec still holds the session it was fitted in.
  formula <- narrow_formula_environment(formula, data)
  bdat <- model.frame(formula, data = data)
  validate_predictor_scale(
    predictor_scale, retrieve_var(bdat, "x_var", error = TRUE)
  )
  call_args <- amend_call_args(mod_fits, bdat)
  model_set <- check_models(model_set, family, bdat, record = TRUE)
  # Strip the attribute once read: `model_set` is passed on, and the attribute
  # makes it compare unequal to a plain character vector.
  amend_excluded <- attr(model_set, "excluded")
  model_set <- as.character(model_set)
  # Exclude an added equation that keeps no group-level term on a grouping
  # variable, as bnec() does, and record it with the exclusions above. Only
  # additions are tested: an equation already in the set kept its terms when
  # it was fitted. A call adding only such equations is refused.
  added <- model_set[!vapply(model_set, function(model) {
    inherits(try(mod_fits[[model]], silent = TRUE), "prebayesnecfit")
  }, logical(1))]
  if (length(added) > 0) {
    group_check <- check_group_terms(added, formula)
    model_set <- setdiff(model_set, group_check$excluded$model)
    amend_excluded <- rbind(amend_excluded, group_check$excluded)
  }
  old_fits <- mod_fits
  mod_fits <- vector(mode = "list", length = length(model_set))
  names(mod_fits) <- model_set
  failed <- list()
  # Only models not already in the set are fitted, and only they decide
  # whether a parallel plan is worth using.
  needs_fit <- !vapply(model_set, function(model) {
    inherits(try(old_fits[[model]], silent = TRUE), "prebayesnecfit")
  }, logical(1))
  # check_data() never runs on this route, which fits with skip_check = TRUE,
  # so refuse a missing trials() term and a response at a bound here, once,
  # before the loop. Only the equations to be fitted are tested: ecxflat alone
  # passes, and a curve equation added to such a response is refused, because
  # amend() does not substitute ecxflat as bnec() does. The trials check comes
  # first, because the bound test reads the trials.
  if (any(needs_fit)) {
    check_trials_term(bdat, family)
    check_response_at_bound(bdat, family, model = model_set[needs_fit])
  }
  # Carry existing fits over here, in the parent. Returned from a worker, each
  # would be sent out and back under a parallel plan.
  for (m in which(!needs_fit)) {
    mod_fits[[model_set[m]]] <- old_fits[[model_set[m]]]
    if (is.null(mod_fits[[model_set[m]]]$call_args)) {
      mod_fits[[model_set[m]]]$call_args <- list(
        model_survival = call_args$model_survival, control = NULL, seed = NULL
      )
    }
  }
  # amend() rebuilds brm_args per model from the stored simdat rather than
  # taking one from the user, so what plan_model_set() decides has to be
  # passed into the loop body and merged there.
  set_plan <- plan_model_set(list(), sum(needs_fit), caller = "amend")
  # narrow_environment() keeps old_fits and the partly filled mod_fits, which
  # this frame holds, out of what future exports with the function.
  fit_one <- narrow_environment(
    function(m) {
      model <- model_set[m]
      # No `init`: simdat$init holds initial values named for another
      # equation's parameters. Leaving it out requests the initial-value search.
      brm_args <- c(
        list(
          family = family, iter = simdat$iter, thin = simdat$thin,
          warmup = simdat$warmup, chains = simdat$chains,
          sample_prior = simdat$sample_prior
        ),
        call_args$brm_args,
        set_plan$brm_args
      )
      brm_args$prior <- priors
      model_priors <- try(validate_priors(brm_args$prior, model),
                          silent = TRUE)
      if (inherits(model_priors, "try-error")) {
        x <- retrieve_var(bdat, "x_var", error = TRUE)
        y <- retrieve_var(bdat, "y_var", error = TRUE)
        custom_name <- check_custom_name(family)
        if (family$family == "binomial" || family$family == "beta_binomial") {
          tr <- retrieve_var(bdat, "trials_var", error = TRUE)
          y <- y / tr
        }
        # Divide by the rate denominator, as by trials above, so that the
        # priors are on the scale the original fit used.
        denom <- retrieve_var(bdat, "rate_var")
        if (!is.null(denom)) {
          y <- y / denom
        }
        # Pass define_prior() every argument fit_bayesnec() passes it. Build it
        # here, outside the try() below, so an error from define_prior() stops
        # amend() once.
        single_form <- single_model_formula(formula, model)
        brm_args$prior <- define_prior(
          model, family, x, y, prior_type = prior_type,
          asymptote_observed = asymptote_observed,
          predictor_scale = predictor_scale,
          model_survival = call_args$model_survival,
          disp_spec = parse_disp_term(single_form),
          group_spec = parse_group_terms(single_form, model),
          rate_response = !is.null(denom),
          truncated_response = isTRUE(attr(
            single_form, "bayesnec_internal_truncation")),
          tested_range = attr(single_form, "bayesnec_tested_range")
        )
      } else {
        brm_args$prior <- model_priors
      }
      try(
        fit_bayesnec(
          formula = formula, data = data, model = model,
          brm_args = brm_args, skip_check = TRUE, prior_type = prior_type,
          asymptote_observed = asymptote_observed,
          predictor_scale = predictor_scale,
          timeout = timeout, model_survival = call_args$model_survival
        ),
        silent = FALSE
      )
    },
    list(model_set = model_set, family = family, simdat = simdat,
         call_args = call_args, set_plan = set_plan, priors = priors,
         bdat = bdat,
         formula = formula, data = data, prior_type = prior_type,
         asymptote_observed = asymptote_observed,
         predictor_scale = predictor_scale, timeout = timeout)
  )
  attempts <- bnec_parallel_lapply(which(needs_fit), fit_one,
                                parallel = set_plan$parallel)
  # Assemble in the parent, as bnec() does: each attempt is the fit or a
  # try-error whose condition attribute failure_record() reads.
  for (i in seq_along(attempts)) {
    model <- model_set[which(needs_fit)[i]]
    fit_m <- attempts[[i]]
    if (!inherits(fit_m, "try-error")) {
      mod_fits[[model]] <- fit_m
    } else {
      mod_fits[[model]] <- NA
      failed[[model]] <- failure_record(model, attr(fit_m, "condition"))
    }
  }
  formulas <- lapply(mod_fits, extract_formula)
  mod_fits <- expand_manec(mod_fits, formula = formulas, x_range = x_range,
                           resolution = resolution, sig_val = sig_val,
                           loo_controls = loo_controls)
  if (length(mod_fits) > 1) {
    out <- allot_class(mod_fits, c("bayesmanecfit", "bnecfit"))
  } else {
    mod_fits <- expand_nec(mod_fits[[1]], formula = formula, x_range = x_range,
                           resolution = resolution, sig_val = sig_val,
                           loo_controls = loo_controls, model = names(mod_fits))
    out <- allot_class(mod_fits, c("bayesnecfit", "bnecfit"))
  }
  # Only the models this call attempted. Failures recorded on the object being
  # amended are not carried forward: a model that failed then may have been
  # dropped now, or is being retried here with different priors.
  out <- attach_failed_models(out, failed)
  # Rebuild the record, since the candidate set changed, but keep
  # `substitutions`: amend() fits the stored data with skip_check = TRUE, so
  # bnec()'s substitution still describes the response. An object with no
  # record gets none, rather than one that claims no substitution was made.
  if (!is.null(bnec_rec)) {
    out <- attach_bnec_record(
      out, amend_requested(bnec_rec$requested, add), model_set,
      amend_exclusions(bnec_rec, amend_excluded, model_set, add),
      bnec_rec$substitutions
    )
  }
  carry_retained_data(out, object)
}

#' The arguments of the original call that an added equation is fitted with
#'
#' Reads the record \code{fit_bayesnec()} stores on the first equation of the
#' set, as \code{extract_simdat()} reads the iteration settings. Where the
#' set records nothing, no \code{control} or \code{seed} is passed, and
#' \code{model_survival} is read from the stored formulas by
#' \code{stored_model_survival()}.
#'
#' @param mod_fits A named \code{\link[base]{list}} of
#' \code{\link{prebayesnecfit}} objects.
#' @param bdat The model frame of the set's formula and data.
#'
#' @return A \code{\link[base]{list}} with elements \code{model_survival},
#' \code{NULL} or one equation name, and \code{brm_args}, a named
#' \code{\link[base]{list}} holding \code{backend} and any \code{control} and
#' \code{seed} the original call supplied.
#'
#' @noRd
amend_call_args <- function(mod_fits, bdat) {
  first <- mod_fits[[1]]
  rec <- first$call_args
  brm_args <- list(backend = first$fit$backend)
  if (is.null(rec)) {
    model_survival <- stored_model_survival(mod_fits, bdat)
  } else {
    model_survival <- rec$model_survival
    brm_args$control <- rec$control
    brm_args$seed <- rec$seed
  }
  if (!is_hurdle_family(first$fit$family)) {
    model_survival <- NULL
  }
  list(model_survival = model_survival, brm_args = brm_args)
}

#' The survival-block equation of a set that records none
#'
#' Matches the second block of each stored \pkg{brms} formula against the block
#' each equation generates. Every equation of a set fitted with
#' \code{model_survival} has the same second block, and every equation of a set
#' fitted without it has its own.
#'
#' @param mod_fits A named \code{\link[base]{list}} of
#' \code{\link{prebayesnecfit}} objects.
#' @param bdat The model frame of the set's formula and data.
#'
#' @return \code{NULL}, or the name of the equation the set's second blocks
#' share. \code{NULL} for one equation whose second block is its own, because
#' a fit made with \code{model_survival} naming that equation stores the same
#' formula as one made without it.
#'
#' @noRd
stored_model_survival <- function(mod_fits, bdat) {
  family <- mod_fits[[1]]$fit$family
  if (!is_hurdle_family(family)) {
    return(NULL)
  }
  dpar <- hurdle_dpar(family)
  # Read the predictor column as wrangle_model_formula() does, or no
  # generated block matches the stored one.
  x_var <- names(bdat)[which(names(attr(bdat, "bnec_pop")) == "x_var")]
  generated <- vapply(mod_groups$all, function(eq) {
    deparse1(hu_block_formula(eq, x_var, dpar)[[3]])
  }, character(1))
  survival <- vapply(mod_fits, function(m) {
    stored <- m$fit$formula$pforms[[dpar]]
    hit <- if (is.null(stored)) {
      character()
    } else {
      names(generated)[generated == deparse1(stored[[3]])]
    }
    if (length(hit) == 1) hit else NA_character_
  }, character(1))
  survival <- unique(survival)
  if (length(survival) != 1 || is.na(survival) ||
        identical(names(mod_fits), survival)) {
    return(NULL)
  }
  survival
}

#' The candidate set an amended object was asked for
#'
#' @param requested The \code{requested} element of the record being amended.
#' @param add The equations added by this call, or \code{NULL}.
#'
#' @return A \code{\link[base]{character}} vector.
#' @noRd
amend_requested <- function(requested, add) {
  union(as.character(requested), as.character(add))
}

#' The exclusions of an amended object, keeping requested partitioned exactly
#'
#' An equation can be requested but not attempted because \code{check_models()}
#' declined it now, it was declined in the call being amended and not asked for
#' again, or the user dropped it here. All three are recorded, so that
#' \code{requested} is split exactly between the set attempted and this table.
#'
#' @param bnec_rec The record being amended.
#' @param excluded The \code{excluded} attribute from this call's
#' \code{check_models()}.
#' @param attempted The set this call attempted.
#' @param add The equations added by this call, or \code{NULL}.
#'
#' @return A \code{\link[base]{data.frame}} of equation and reason.
#' @noRd
amend_exclusions <- function(bnec_rec, excluded, attempted, add) {
  out <- if (is.null(excluded)) {
    data.frame(model = character(), reason = character(),
               stringsAsFactors = FALSE)
  } else {
    excluded
  }
  requested <- amend_requested(bnec_rec$requested, add)
  unaccounted <- setdiff(requested, c(attempted, out$model))
  if (length(unaccounted) == 0) {
    return(out)
  }
  prior <- bnec_rec$excluded
  reasons <- vapply(unaccounted, function(m) {
    hit <- which(prior$model == m)
    if (length(hit) > 0) {
      prior$reason[hit[1]]
    } else {
      "dropped by amend()"
    }
  }, character(1), USE.NAMES = FALSE)
  rbind(out, data.frame(model = unaccounted, reason = reasons,
                        stringsAsFactors = FALSE))
}
