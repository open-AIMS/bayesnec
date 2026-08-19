#' linear_rescale
#' @param x A \code{\link[base]{numeric}} vector.
#' @param r_out A \code{\link[base]{numeric}} vector of length 2 containing
#' the new range of values in x.
#' @return A \code{\link[base]{numeric}} vector.
#' @noRd
linear_rescale <- function(x, r_out) {
  p <- (x - min(x)) / (max(x) - min(x))
  r_out[[1]] + p * (r_out[[2]] - r_out[[1]])
}

#' check_custom_name
#' @param family An object of class \code{\link[stats]{family}} or
#' \code{\link[brms]{brmsfamily}}.
#' @return A \code{\link[base]{character}} vector containing the brms
#' custom family or NA.
#' @noRd
check_custom_name <- function(family) {
  custom_name <- "none"
  if (inherits(family, "customfamily")) {
    custom_name <- family$name
  }
  custom_name
}

#' extract_pars
#' @param x A \code{\link[base]{character}} vector.
#' @param model_fit An object of class \code{\link[brms]{brmsfit}}.
#' @return A named \code{\link[base]{numeric}} vector or NA.
#' @importFrom brms fixef
#' @noRd
extract_pars <- function(x, model_fit, prefix = "") {
  fef <- fixef(model_fit, robust = TRUE)
  # Anchored. Unanchored matching breaks for hurdle fits, where "top" also
  # matches "hutop_Intercept": the result becomes a 2-row matrix, tt["Estimate"]
  # returns NA, every parameter comes back NA, and expand_nec() then
  # misclassifies a nec model as an ecx one and reports an NSEC as the NEC.
  tt <- fef[grep(paste0("^", prefix, x, "_"), rownames(fef)),
            c("Estimate", "Q2.5", "Q97.5")]
  if (length(tt) == 0 || is.na(tt["Estimate"])) {
    NA
  } else {
    tt
  }
}

#' min_abs
#' @param x A \code{\link[base]{numeric}} vector.
#' @return A \code{\link[base]{numeric}} vector.
#' @noRd
min_abs <- function(x) {
  which.min(abs(x))
}

#' paste_normal_prior
#'
#' Creates prior string given a number
#'
#' @param mean A \code{\link[base]{numeric}} vector.
#' @param param A \code{\link[base]{character}} vector indicating the
#' target non-linear parameter.
#' @param sd A \code{\link[base]{numeric}} vector indicating the
#' standard deviation.
#' @param ... Additional arguments of \code{\link[brms]{prior_string}}.
#'
#' @return A \code{\link[base]{character}} vector.
#' @importFrom brms prior_string
#' @noRd
paste_normal_prior <- function(mean, param, sd = 1, ...) {
  prior_string(paste0("normal(", mean, ", ", sd, ")"), nlpar = param, ...)
}

#' @noRd
extract_dispersion <- function(x) {
  x$dispersion
}

#' @noRd
extract_loo <- function(x) {
  x$fit$criteria$loo
}

#' @noRd
extract_waic_estimate <- function(x) {
  x$fit$criteria$waic$estimates["waic", "Estimate"]
}

#' Realise the model-averaging draw once, reproducibly.
#'
#' Model averaging keeps \code{round(sample_size * wi)} of each component's
#' draws. Which draws those are used to be decided by an unseeded
#' \code{sample()} at every call site, so \code{predict()},
#' \code{posterior_epred()} and the summaries stored on the object were each a
#' different realisation and no two calls agreed. Seeding the draw from a value
#' carried on the \code{bayesmanecfit} makes realisation \emph{i} mean
#' "component m[i], iteration j[i]" for every quantity computed from that
#' object. See #216.
#'
#' Restores the caller's RNG state rather than calling \code{set.seed()}
#' outright: model averaging must not silently reset a user's simulation seed.
#'
#' \code{sample.kind} is pinned rather than left at whatever the session is
#' using. A seed alone does not fix a draw: R 3.6.0 changed the algorithm behind
#' \code{sample()}, and the same seed gives a different index either side of
#' that change. Left unpinned, a saved \code{bayesmanecfit} reloaded under a
#' different R would silently rebuild a different index and quietly stop
#' matching its own stored summaries -- which is the bug this is meant to close,
#' merely deferred. "Rejection" is the post-3.6.0 default, so pinning it changes
#' nothing today and holds the draw fixed for objects that outlive this R.
#'
#' @param model_set A \code{\link[base]{character}} vector of model names.
#' @param sample_size A \code{\link[base]{numeric}} vector of length 1, the
#' number of draws available per component.
#' @param mod_stats A \code{\link[base]{data.frame}} with a \code{wi} column
#' and model names as row names.
#' @param seed A \code{\link[base]{numeric}} vector of length 1, or NULL.
#'
#' @return A named \code{\link[base]{list}} of integer vectors, the draw
#' indices kept for each model.
#' @noRd
weighted_draw_index <- function(model_set, sample_size, mod_stats, seed) {
  if (is.null(seed)) {
    # Objects built before #216 carry no seed. A fixed fallback still makes
    # every call on such an object agree with every other, which is the point;
    # erroring would break saved objects and re-drawing would restore the bug.
    seed <- 216
  }
  old_kind <- RNGkind()
  has_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (has_seed) {
    get(".Random.seed", envir = globalenv(), inherits = FALSE)
  } else {
    NULL
  }
  on.exit({
    # RNGkind() first, then the seed: the generator kind is encoded in
    # .Random.seed[1], so restoring the seed last leaves both correct. Where
    # there was no seed to restore, RNGkind() is what puts sample.kind back --
    # removing .Random.seed on its own would not.
    suppressWarnings(RNGkind(old_kind[1], old_kind[2], old_kind[3]))
    if (is.null(old_seed)) {
      suppressWarnings(rm(".Random.seed", envir = globalenv()))
    } else {
      assign(".Random.seed", old_seed, envir = globalenv())
    }
  }, add = TRUE)
  set.seed(seed, sample.kind = "Rejection")
  out <- lapply(model_set, function(index) {
    size <- as.integer(round(sample_size * mod_stats[index, "wi"]))
    sample(seq_len(sample_size), size)
  })
  names(out) <- model_set
  out
}

#' The model-averaging index to use for a given number of available draws.
#'
#' Prefers the index realised when the object was built and stored on it. That
#' is exact and cannot drift: it survives being reloaded under a different R,
#' where regenerating from the seed is only as stable as \code{sample()}'s
#' algorithm. Rebuilding from the seed is the fallback, for two cases -- a
#' caller thinning to a different number of draws (\code{ndraws},
#' \code{draw_ids}), where the stored index does not apply, and objects saved
#' before the index was stored. See #216.
#'
#' @param object An object of class \code{\link{bayesmanecfit}}.
#' @param model_set A \code{\link[base]{character}} vector of model names.
#' @param sample_size A \code{\link[base]{numeric}} vector of length 1, the
#' number of draws available per component.
#'
#' @return A named \code{\link[base]{list}} of integer vectors.
#' @noRd
pull_draw_index <- function(object, model_set, sample_size) {
  idx <- object$w_draw_index
  # `==` rather than identical(): sample_size is a double off the object and an
  # integer off nrow(), and identical() would call those different and silently
  # take the fallback every time.
  same_n <- isTRUE(object$sample_size == sample_size)
  if (!is.null(idx) && same_n && all(model_set %in% names(idx))) {
    idx[model_set]
  } else {
    weighted_draw_index(model_set, sample_size, object$mod_stats,
                        object$w_draw_seed)
  }
}

#' @noRd
w_nec_calc <- function(index, mod_fits, draw_index) {
  mod_fits[[index]]$ne_posterior[draw_index[[index]]]
}

#' @noRd
w_pred_calc <- function(index, mod_fits, mod_stats) {
  mod_fits[[index]]$predicted_y * mod_stats[index, "wi"]
}

#' @noRd
w_pred_list_calc <- function(index, pred_list, draw_index) {
  pred_list[[index]][draw_index[[index]], ]
}

#' Compute one model's grid posterior and immediately thin it to its weight.
#'
#' Used by expand_manec(), where the posteriors are built rather than read off
#' the objects. Computing and thinning in the same step means only one model's
#' full matrix exists at a time; collecting them into a list first would hold
#' every model's at once. Thins through w_pred_list_calc() rather than
#' repeating the draw, so this and posterior_epred.bayesmanecfit() cannot
#' sample differently. See #180.
#'
#' @noRd
w_grid_pred_calc <- function(index, mod_fits, formulas, x_range, resolution,
                             draw_index) {
  pred_list <- list(posterior_on_grid(mod_fits[[index]]$fit, formulas[[index]],
                                      x_range = x_range,
                                      resolution = resolution))
  names(pred_list) <- index
  w_pred_list_calc(index, pred_list, draw_index)
}

#' @noRd
do_wrapper <- function(..., fct = "cbind") {
  do.call(fct, lapply(...))
}

#' @noRd
#' @importFrom stats median quantile
estimates_summary <- function(x) {
  x <- c(median(x), quantile(x, c(0.025, 0.975)))
  names(x) <- c("Estimate", "Q2.5", "Q97.5")
  x
}

#' @noRd
handle_set <- function(x, add, drop) {
  msets <- names(mod_groups)
  tmp <- x
  if (!missing(add)) {
    y <- add
    if (any(add %in% msets)) {
      y <- unname(unlist(mod_groups[intersect(add, msets)]))
      y <- setdiff(union(y, add), msets)
    }
    tmp <- union(tmp, y)
  }
  if (!missing(drop)) {
    y <- drop
    if (any(drop %in% msets)) {
      y <- unname(unlist(mod_groups[intersect(drop, msets)]))
    }
    tmp <- setdiff(tmp, y)
    if (length(tmp) == 0) {
      stop(
        "All models removed, nothing to return;\n",
        "Perhaps try calling function bnec with another ",
        "model set."
      )
    }
  }
  if (identical(sort(x), sort(tmp))) {
    message(
      "Nothing to amend, please specify a model to ",
      "either add or drop that differs from the original set."
    )
    "wrong_model_output"
  } else {
    tmp
  }
}

#' allot_class
#'
#' Assigns class to an object.
#'
#' @param x An object.
#' @param new_class The new object class.
#'
#' @return An object of class new_class.
#' @noRd
allot_class <- function(x, new_class) {
  class(x) <- new_class
  x
}

#' @noRd
expand_and_assign_nec <- function(x, ...) {
  allot_class(expand_nec(x, ...), c("bayesnecfit", "bnecfit"))
}

#' are_chains_correct
#'
#' Checks if number of chains in a \code{\link[brms]{brmsfit}} object are
#' correct.
#'
#' @param brms_fit An object of class \code{\link[brms]{brmsfit}}.
#' @param chains The expected number of correct chains.
#'
#' @return A \code{\link[base]{logical}} vector.
#' @noRd
are_chains_correct <- function(brms_fit, chains) {
  fit_chs <- brms_fit$fit@sim$chains
  if (is.null(fit_chs)) {
    FALSE
  } else {
    fit_chs == chains
  }
}

#' @noRd
get_init_predictions <- function(y, x, fct, .args) {
  y <- y[match(.args, names(y))]
  y <- lapply(y, as.numeric)
  y[["x"]] <- x
  do.call("fct", y)
}

#' @noRd
check_init_predictions <- function(x, limits) {
  min(x) > min(limits) &
    max(x) < max(limits) &
    !any(is.na(x)) &
    !any(is.infinite(x)) &
    !any(is.nan(x)) &
    x[1] > x[length(x)] &
    length(unique(x)) > 3
}

#' @noRd
clean_names <- function(x) {
  paste0("Q", gsub("%", "", names(x), fixed = TRUE))
}

#' @noRd
modify_posterior <- function(n, object, x_vec, p_samples, hormesis_def) {
  posterior_sample <- p_samples[n, ]
  if (hormesis_def == "max") {
    target <- x_vec[which.max(posterior_sample)]
    change <- x_vec < target
  } else if (hormesis_def == "control") {
    target <- posterior_sample[1]
    change <- posterior_sample >= target
  }
  posterior_sample[change] <- NA
  posterior_sample
}

#' extract_warnings
#'
#' Extract warnings from a \code{\link[brms]{brmsfit}} object.
#'
#' @param x An object of class \code{\link[brms]{brmsfit}}.
#'
#' @importFrom evaluate evaluate is.warning
#'
#' @return A \code{\link[base]{list}} containing all warning messages.
#' @noRd
extract_warnings <- function(x) {
  x <- evaluate("identity(x)", new_device = FALSE)
  to_extract <- which(sapply(x, is.warning))
  if (length(to_extract) > 0) {
    x[to_extract]
  } else {
    NULL
  }
}

#' @noRd
has_r_hat_warnings <- function(...) {
  x <- extract_warnings(...)
  any(grepl("some Rhats are > 1.05", x, fixed = TRUE))
}

#' @noRd
print_mat <- function(x, digits = 2) {
  fmt <- paste0("%.", digits, "f")
  out <- x
  for (i in seq_len(ncol(x))) {
    out[, i] <- sprintf(fmt, x[, i])
  }
  print(out, quote = FALSE, right = TRUE)
  invisible(x)
}

#' @noRd
clean_mod_weights <- function(x) {
  a <- x$mod_stats[, !sapply(x$mod_stats, function(z) all(is.na(z)))]
  as.matrix(a[, -1])
}

#' @noRd
clean_nec_vals <- function(x, all_models, ecx_models) {
  if (is_bayesnecfit(x)) {
    mat <- t(as.matrix(x$ne))
  } else if (is_bayesmanecfit(x)) {
    mat <- t(as.matrix(x$w_ne))
  } else {
    stop("Wrong input class.")
  }
  # ne_type is recorded when the fit is expanded and is the authority: for a
  # two-block (hurdle) fit the reported estimate describes the combined
  # endpoint, whose type depends on the equations used for both blocks and so
  # cannot be read off the model name alone.
  if (!is.null(x$ne_type)) {
    neclab <- x$ne_type
  } else {
    neclab <- "NEC"
    if (all(all_models %in% ecx_models)) {
      neclab <- "NSEC"
    } else if (!is.null(ecx_models)) {
      neclab <- "N(S)EC"
    }
  }
  rownames(mat) <- neclab
  mat
}

#' @noRd
nice_ecx_out <- function(ec, ecx_tag) {
  cat(ecx_tag)
  cat("\n")
  mat <- t(as.matrix(ec))
  rownames(mat) <- "Estimate"
  print_mat(mat)
}

#' @noRd
contains_zero <- function(x) {
  sum(x == 0, na.rm = TRUE) >= 1
}

#' @noRd
contains_one <- function(x) {
  sum(x == 1, na.rm = TRUE) >= 1
}

#' @noRd
contains_negative <- function(x) {
  any(x < 0, na.rm = TRUE)
}

#' @importFrom stats binomial
#' @noRd
response_link_scale <- function(response, family) {
  link_tag <- family$link
  min_z_val <- min(response[which(response > 0)]) / 100
  if (link_tag == "logit") {
    max_o_val <- max(response[which(response < 1)]) +
      (1 - max(response[which(response < 1)])) * 0.99
  }
  lr <- linear_rescale
  custom_name <- check_custom_name(family)
  if (link_tag %in% c("logit", "log")) {
    if (family$family %in% c("bernoulli", "binomial", "beta_binomial")) {
      if (contains_zero(response)) {
        response <- lr(response, r_out = c(min_z_val, max(response)))
      }
      if (contains_one(response)) {
        response <- lr(response, r_out = c(min(response), max_o_val))
      }
      response <- family$linkfun(response)
    } else {
      if (contains_zero(response)) {
        response <- lr(response, r_out = c(min_z_val, max(response)))
      }
      response <- family$linkfun(response)
    }
  } else if (
    link_tag == "identity" &&
      family$family %in% c("bernoulli", "binomial", "beta_binomial", "beta")
  ) {
    # For identity-link bounded families, mu must be strictly in (0, 1).
    # Clamp response away from the boundaries so that initial values
    # derived from it stay within the valid support.
    if (contains_zero(response)) {
      response <- lr(response, r_out = c(min_z_val, max(response)))
    }
    if (contains_one(response)) {
      max_o_val <- max(response[which(response < 1)]) +
        (1 - max(response[which(response < 1)])) * 0.99
      response <- lr(response, r_out = c(min(response), max_o_val))
    }
  }
  response
}

#' @noRd
rounded <- function(value, resolution = 1) {
  sprintf(paste0("%.", resolution, "f"), round(value, resolution))
}

#' @noRd
return_x_range <- function(x) {
  return_x <- function(object) {
    if (is_bayesmanecfit(object)) {
      object$w_pred_vals$data$x
    } else if (is_bayesnecfit(object)) {
      object$pred_vals$data$x
    } else if (is_bayesnechurdlefit(object)) {
      # The survival component sees every row, so it carries the full exposed
      # predictor range; the growth component stops short of any concentration
      # where nothing survived.
      return_x_range(list(object$survival))
    } else {
      stop("Not all objects in x are of class bayesnecfit, bayesmanecfit or",
           " bayesnechurdlefit.")
    }
  }
  lapply(x, return_x) |>
    unlist() |>
    range(na.rm = TRUE)
}

#' Guard against the two component-selection arguments being confused
#'
#' The two implementations of a hurdle model select a component differently: a
#' \code{\link{bayesnechurdlefit}} holds two separate fits and takes
#' \code{which}, while a joint two-block fit holds two parameter blocks inside
#' one model and takes \code{dpar}. Each argument used to fall into \code{...}
#' on the other's methods and be discarded, so the call returned the default
#' endpoint -- a wrong answer with no error and no warning. Erroring is cheap
#' and the alternative has already cost people time.
#'
#' @param dots The \code{...} of the calling method, as a list.
#' @param object The object the method was called on.
#'
#' @return Invisibly \code{TRUE}, or an error.
#'
#' @noRd
check_component_arg <- function(dots, object) {
  if (is_bayesnechurdlefit(object)) {
    if ("dpar" %in% names(dots)) {
      stop("`dpar` names a parameter block of a joint two-block fit. This is",
           " a bayesnechurdlefit, which holds two separate fits: use",
           " `which = \"growth\"`, \"survival\" or \"combined\".",
           call. = FALSE)
    }
  } else if ("which" %in% names(dots)) {
    stop("`which` selects a component of a bayesnechurdlefit, as returned by",
         " bnec_hurdle(). This object is a ", class(object)[1],
         ". For a joint two-block fit, name the parameter block instead:",
         " `dpar = \"mu\"` for the response block or `dpar = \"hu\"`",
         " (\"zi\" for zero-inflated families) for the survival block.",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Guard against `dpar` being passed to nec()
#'
#' Unlike \code{\link{ecx}} and \code{\link{nsec}}, \code{\link{nec}} has no
#' block selection for a joint two-block fit: what it returns is the combined
#' threshold, the per-draw minimum of the two blocks. The block-specific
#' posteriors are stored on the fit but are not exposed by an argument, so a
#' supplied \code{dpar} would otherwise be discarded and the combined value
#' returned in its place.
#'
#' @param dots The \code{...} of the calling method, as a list.
#'
#' @return Invisibly \code{TRUE}, or an error.
#'
#' @noRd
check_nec_no_dpar <- function(dots) {
  if ("dpar" %in% names(dots)) {
    stop("nec() has no block selection for a joint two-block fit; what it",
         " returns is the combined threshold, the per-draw minimum of the",
         " two blocks (see ?nec.bayesnechurdlefit). Use ecx() or nsec() with",
         " `dpar` for a single block.", call. = FALSE)
  }
  invisible(TRUE)
}

#' @noRd
return_nec_post <- function(m, xform) {
  if (is_bayesnecfit(m)) {
    out <- unname(m$ne_posterior)
  }
  if (is_bayesmanecfit(m)) {
    out <- unname(m$w_ne_posterior)
  }
  if (is_bayesnechurdlefit(m)) {
    # Defaults to the combined threshold, matching nec() on the same object.
    out <- unname(nec(m, posterior = TRUE))
  }
  if (inherits(xform, "function")) {
    out <- xform(out)
  }
  out
}

#' @noRd
gm_mean <- function(x, na_rm = TRUE, zero_propagate = FALSE) {
  if (any(x < 0, na.rm = TRUE)) {
    return(NaN)
  }
  if (zero_propagate) {
    if (any(x == 0, na.rm = TRUE)) {
      return(0)
    }
    exp(mean(log(x), na.rm = na_rm))
  } else {
    exp(sum(log(x[x > 0]), na.rm = na_rm) / length(x))
  }
}

#' @noRd
summarise_posterior <- function(mat, x_vec) {
  cbind(x = x_vec, data.frame(t(apply(mat, 2, estimates_summary))))
}

#' @noRd
is_character <- function(x) {
  if (is.na(x)) {
    x <- as.character(x)
  }
  is.character(x)
}

#' @noRd
expand_model_set <- function(model) {
  msets <- names(mod_groups)
  if (any(model %in% msets)) {
    group_mods <- intersect(model, msets)
    model <- union(model, unname(unlist(mod_groups[group_mods])))
    model <- setdiff(model, msets)
  }
  model
}

#' @noRd
retrieve_valid_family <- function(named_list, data) {
  if (!"family" %in% names(named_list)) {
    y <- retrieve_var(data, "y_var", error = TRUE)
    tr <- retrieve_var(data, "trials_var")
    family <- set_distribution(y, support_integer = TRUE, trials = tr)
  } else {
    family <- named_list$family
  }
  validate_family(family)
}

#' @noRd
define_loo_controls <- function(loo_controls, family_str) {
  if (missing(loo_controls)) {
    loo_controls <- list(fitting = list(), weights = list(method = "pseudobma"))
  } else {
    loo_controls <- validate_loo_controls(loo_controls, family_str)
    if (!"method" %in% names(loo_controls$weights)) {
      loo_controls$weights$method <- "pseudobma"
    }
  }
  loo_controls
}

#' @noRd
retrieve_var <- function(data, var, error = FALSE) {
  bnec_vars <- attr(data, "bnec_pop")
  bnec_pop <- names(bnec_vars)
  v_pos <- which(bnec_pop == var)
  out <- try(data[[v_pos]], silent = TRUE)
  if (inherits(out, "try-error")) {
    if (error) {
      stop(
        "The input variable \"",
        bnec_vars[[var]],
        "\" was not properly specified in formula. See ?bayesnecformula"
      )
    }
    NULL
  } else if (is.numeric(out)) {
    if (!is.vector(out)) {
      message(
        "You most likely provided a function to transform your \"",
        bnec_vars[[var]],
        "\" that does not return a vector. This is",
        " likely to cause issues with sampling in Stan. ",
        " Forcing it to be a vector..."
      )
    }
    as.vector(out)
  } else {
    stop("The input variable \"", bnec_vars[[var]], "\" is not numeric.")
  }
}

#' Retrieve the censoring indicator from a model frame, as -1/0/1/2
#'
#' Returns \code{NULL} when the formula carried no \code{cens()} term. Unlike
#' \code{\link{retrieve_var}} this cannot go through the numeric coercion there,
#' because a censoring indicator is usually a character or factor.
#'
#' @noRd
retrieve_cens <- function(data) {
  bnec_vars <- attr(data, "bnec_pop")
  v_pos <- which(names(bnec_vars) == "cens_var")
  if (length(v_pos) != 1) {
    return(NULL)
  }
  normalise_cens(data[[v_pos]])
}

#' Normalise a brms censoring indicator to the integer codes brms uses
#'
#' Mirrors the accepted encodings of \code{brms:::prepare_cens}: partially
#' matched strings, logicals, or the integer codes themselves. Reimplemented
#' rather than called via \code{:::} so that bayesnec does not depend on a brms
#' internal; the mapping is fixed by brms' documented formula syntax, so it is
#' not expected to drift. Anything unrecognised is returned as \code{NA} and
#' left for brms to reject, which keeps the authoritative error message in one
#' place.
#'
#' @noRd
normalise_cens <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (is.logical(x)) {
    return(ifelse(is.na(x), NA_integer_, ifelse(x, 1L, 0L)))
  }
  if (is.numeric(x)) {
    return(ifelse(x %in% c(-1, 0, 1, 2), as.integer(x), NA_integer_))
  }
  codes <- c(left = -1L, none = 0L, right = 1L, interval = 2L)
  vapply(as.character(x), function(i) {
    if (is.na(i) || !nzchar(i)) {
      return(NA_integer_)
    }
    hit <- which(startsWith(names(codes), i))
    if (length(hit) == 1) codes[[hit]] else NA_integer_
  }, integer(1), USE.NAMES = FALSE)
}

#' @noRd
add_brm_defaults <- function(
  brm_args,
  model,
  family,
  predictor,
  response,
  skip_check,
  custom_name,
  prior_type = "uninformative",
  model_survival = NULL,
  disp_spec = NULL
) {
  if (!("chains" %in% names(brm_args))) {
    brm_args$chains <- 4
  }
  if (!("sample_prior" %in% names(brm_args))) {
    brm_args$sample_prior <- "yes"
  }
  if (!("iter" %in% names(brm_args))) {
    brm_args$iter <- 1e4
  }
  if (!("warmup" %in% names(brm_args))) {
    brm_args$warmup <- floor(brm_args$iter / 5) * 4
  }
  priors <- try(validate_priors(brm_args$prior, model), silent = TRUE)
  if (inherits(priors, "try-error")) {
    brm_args$prior <- define_prior(
      model,
      family,
      predictor,
      response,
      prior_type = prior_type,
      model_survival = model_survival,
      disp_spec = disp_spec
    )
  } else {
    brm_args$prior <- priors
  }
  if (!("init" %in% names(brm_args)) || skip_check) {
    msg_tag <- family$family
    model_tag <- if (is.null(model_survival) || identical(model_survival,
                                                          model)) {
      model
    } else {
      paste0(model, " (response) and ", model_survival, " (survival)")
    }
    message(paste0(
      "Finding initial values which allow the response to be",
      " fitted using a ",
      model_tag,
      " model and a ",
      msg_tag,
      " distribution."
    ))
    response_link <- response_link_scale(response, family)
    init_seed <- NULL
    if ("seed" %in% names(brm_args)) {
      init_seed <- brm_args$seed
    }
    # A variance function adds parameters that belong to no curve. The init
    # search validates prior names against the model's own parameter set and
    # only ever evaluates the mean curve, so those are filtered out here rather
    # than taught to it: they play no part in getting the curve inside the
    # response range. They are added back at the constant-dispersion null once
    # the search has run -- see disp_inits(), and note that leaving them to
    # Stan's own draw is NOT benign.
    init_priors <- brm_args$prior
    disp_par_names <- disp_pars(disp_spec)
    if (length(disp_par_names) > 0) {
      init_priors <- init_priors[!init_priors$nlpar %in% disp_par_names, ]
    }
    inits <- if (is_hurdle_family(family)) {
      # Two blocks with differently-scaled responses, primed separately then
      # merged. response_link_scale() is a no-op for hurdle_gamma under an
      # identity link, so the raw response is what the split needs.
      make_good_hurdle_inits(
        model,
        predictor,
        response,
        priors = init_priors,
        chains = brm_args$chains,
        dpar = hurdle_dpar(family),
        seed = init_seed,
        model_survival = model_survival
      )
    } else {
      make_good_inits(
        model,
        predictor,
        response_link,
        priors = init_priors,
        chains = brm_args$chains,
        seed = init_seed
      )
    }
    if (length(inits) == 1 && "random" %in% names(inits)) {
      inits <- inits$random
    }
    # Only when the search returned per-chain values; where it fell back to
    # "random" there is no list to append to and Stan initialises everything.
    if (length(disp_par_names) > 0 && !is.character(inits)) {
      d_init <- disp_inits(disp_spec, family, response)
      inits <- lapply(inits, function(chain) c(chain, d_init))
    }
    brm_args$init <- inits
  }
  brm_args
}

#' @noRd
extract_formula <- function(x) {
  out <- try(x[["bayesnecformula"]], silent = TRUE)
  if (inherits(out, "try-error")) {
    NA
  } else {
    out
  }
}

#' @noRd
#' @importFrom stats model.frame
has_family_changed <- function(x, data, ...) {
  brm_args <- list(...)
  for (i in seq_along(x)) {
    formula <- extract_formula(x[[i]])
    bdat <- model.frame(formula, data = data, run_par_checks = TRUE)
    model <- get_model_from_formula(formula)
    family <- retrieve_valid_family(brm_args, bdat)
    model <- check_models(model, family, bdat)
    checked_df <- check_data(data = bdat, family = family, model = model)
  }
  out <- all.equal(
    checked_df$family,
    x[[1]]$fit$family,
    check.attributes = FALSE,
    check.environment = FALSE
  )
  if (is.logical(out)) {
    FALSE
  } else {
    TRUE
  }
}

#' @noRd
clean_aterms <- function(data) {
  aterms <- c("^trials\\(", "^me\\(", "^mi\\(", "^mo\\(", "^se\\(", "^cs\\(")
  for (i in seq_along(aterms)) {
    has_aterm <- grepl(aterms[i], names(data))
    if (any(has_aterm)) {
      names(data)[has_aterm] <- names(data)[has_aterm] |>
        str2lang() |>
        (`[[`)(2) |>
        all.vars()
    }
  }
  data
}

#' @noRd
find_transformations <- function(data) {
  bnec_pop_vars <- attr(data, "bnec_pop")
  # remove aterms from data name
  data <- clean_aterms(data)
  unname(bnec_pop_vars[!bnec_pop_vars %in% names(data)])
}

#' @noRd
cleaned_brms_summary <- function(brmsfit) {
  brmssummary <- summary(brmsfit, robust = TRUE)
  rownames(brmssummary$fixed) <- gsub(
    "\\_Intercept$",
    "",
    rownames(brmssummary$fixed)
  )
  brmssummary
}

#' @noRd
identical_value <- function(x, y) {
  if (identical(x, y)) {
    x
  } else {
    FALSE
  }
}

#' @noRd
#' @importFrom stats model.frame
check_data_equality <- function(mod_fits) {
  data_are_equal <- lapply(mod_fits, function(x) as.matrix(x$fit$data)) |>
    Reduce(f = identical_value) |>
    is.matrix()
  if (!data_are_equal) {
    stop(
      "Dataset values differ across fits. Datasets need to be identical ",
      "across the multiple fits."
    )
  }
  # this second check is needed for cases where a function is passed onto
  # one of the model variables via the formula, e.g. crf(log(x), ...)
  cols_are_equal <- lapply(mod_fits, function(x) {
    model.frame(x$bayesnecformula, x$fit$data) |>
      attr("terms") |>
      attr("factors") |>
      rownames() |>
      sort()
  }) |>
    Reduce(f = identical_value) |>
    is.character()
  if (!cols_are_equal) {
    stop(
      "Dataset column names differ across fits. Datasets need to be ",
      "identical across the multiple fits."
    )
  }
}

#' @noRd
#' @importFrom chk chk_numeric
check_args_newdata <- function(resolution, x_range) {
  chk_numeric(resolution)
  # The documented "not supplied" value, and the only NA accepted.
  if (length(x_range) == 1 && is.na(x_range)) {
    return(invisible(NULL))
  }
  chk_numeric(x_range)
  # A partially specified range used to be handled inconsistently, and
  # differently depending on which end was missing -- bnec_newdata() ignored it
  # silently, expand_nec() turned c(1, NA) into seq(NA, NA) but fell back to the
  # observed range for c(NA, 4). It is not a meaningful request either way, so
  # reject it rather than pick one. See #211.
  if (any(is.na(x_range))) {
    stop("x_range must be either NA or fully specified; ",
         "it cannot contain NA alongside a value.", call. = FALSE)
  }
  invisible(NULL)
}

#' @noRd
newdata_eval <- function(object, resolution, x_range) {
  # Just need one model to extract and generate data
  # since all models are considered to have the exact same raw data.
  # A hurdle fit has no single underlying brmsfit, so the grid is taken from
  # its survival component -- that one sees every row, and therefore the full
  # exposed predictor range, whereas the growth component stops short of any
  # concentration where nothing survived. posterior_epred() is still called on
  # the hurdle object itself, so the prediction remains the combined endpoint.
  if (is_bayesnechurdlefit(object)) {
    object <- object$survival
  }
  if (inherits(object, "bayesmanecfit")) {
    model_set <- names(object$mod_fits)
    object <- suppressMessages(pull_out(object, model = model_set[1]))
  }
  data <- model.frame(object$bayesnecformula, object$fit$data)
  bnec_pop_vars <- attr(data, "bnec_pop")
  newdata <- bnec_newdata(object, resolution = resolution, x_range = x_range)
  x_vec <- newdata[[bnec_pop_vars[["x_var"]]]]
  list(newdata = newdata, x_vec = x_vec)
}

#' @noRd
newdata_eval_fitted <- function(
  object,
  resolution,
  x_range,
  make_newdata,
  fct_eval,
  ...
) {
  # Just need one model to extract and generate data
  # since all models are considered to have the exact same raw data.
  # A hurdle fit has no single underlying brmsfit, so the grid comes from its
  # survival component -- that one sees every row and therefore the full
  # exposed predictor range. posterior_epred() is still called on the hurdle
  # object itself downstream, so predictions remain the combined endpoint.
  if (is_bayesnechurdlefit(object)) {
    object <- object$survival
  }
  if (inherits(object, "bayesmanecfit")) {
    model_set <- names(object$mod_fits)
    object <- suppressMessages(pull_out(object, model = model_set[1]))
  }
  data <- model.frame(object$bayesnecformula, object$fit$data)
  bnec_pop_vars <- attr(data, "bnec_pop")
  dot_list <- list(...)
  if ("newdata" %in% names(dot_list) && make_newdata) {
    stop(
      "You cannot provide a \"newdata\" argument and set",
      " make_newdata = TRUE at the same time. Please use one or another.",
      " See details in help file ?",
      fct_eval
    )
  }
  if (!("newdata" %in% names(dot_list))) {
    if (make_newdata) {
      newdata <- bnec_newdata(
        object,
        resolution = resolution,
        x_range = x_range
      )
      x_vec <- newdata[[bnec_pop_vars[["x_var"]]]]
      if ("re_formula" %in% names(dot_list)) {
        message(
          "Argument \"re_formula\" ignored and set to NA because",
          " function bnec_newdata cannot guess random effect structure."
        )
      }
      re_formula <- NA
    } else {
      newdata <- NULL
      x_vec <- pull_brmsfit(object)$data[[bnec_pop_vars[["x_var"]]]]
      resolution <- "from raw data"
      if (!("re_formula" %in% names(dot_list))) {
        re_formula <- NULL
      } else {
        re_formula <- dot_list$re_formula
      }
    }
  } else {
    newdata <- dot_list$newdata
    x_vec <- newdata[[bnec_pop_vars[["x_var"]]]]
    resolution <- "from user-specified newdata"
    if (!("re_formula" %in% names(dot_list))) {
      re_formula <- NULL
    } else {
      re_formula <- dot_list$re_formula
    }
  }
  list(
    newdata = newdata,
    x_vec = x_vec,
    resolution = resolution,
    re_formula = re_formula
  )
}

#' step
#' @param x A \code{\link[base]{numeric}} vector.
#' the new range of values in x.
#' @return A \code{\link[base]{numeric}} vector.
#' @details This function is currently exported to allow for non-linear
#' formula evaluation in brms.
#'
#' @export
step <- function(x) {
  as.numeric(x > 0)
}
