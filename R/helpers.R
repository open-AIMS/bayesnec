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
  # Anchor the pattern: unanchored, "top" also matches "hutop_Intercept" in a
  # hurdle fit, every parameter comes back NA, and expand_nec() then reports
  # an NSEC as the NEC.
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

#' Evaluate an expression with the caller's RNG state restored
#'
#' Saves the generator kinds and \code{.Random.seed} on entry and restores
#' them on exit, whether \code{expr} returns or errors. Use it wherever
#' \pkg{bayesnec} calls \code{\link[base]{set.seed}} outside of fitting, so
#' that the user's random stream is left where it was.
#'
#' Restore the kind before the seed: the kind is encoded in
#' \code{.Random.seed[1]}, so restoring it second would overwrite the seed.
#' Where the session had no seed, \code{.Random.seed} is removed rather than
#' assigned, and \code{\link[base]{RNGkind}} restores \code{sample.kind}. The
#' cached normal variate of \code{normal.kind = "Box-Muller"} cannot be
#' restored.
#'
#' \code{expr} is evaluated in the calling frame, so a \code{return()} inside
#' it returns from the caller.
#'
#' @param expr An expression to evaluate.
#'
#' @return The value of \code{expr}.
#' @noRd
with_preserved_rng_state <- function(expr) {
  old_kind <- RNGkind()
  has_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (has_seed) {
    get(".Random.seed", envir = globalenv(), inherits = FALSE)
  } else {
    NULL
  }
  on.exit({
    # suppressWarnings for the sample.kind = "Rounding" notice, which a session
    # set to the pre-3.6.0 sampler would otherwise have relayed once per call.
    suppressWarnings(RNGkind(old_kind[1], old_kind[2], old_kind[3]))
    if (is.null(old_seed)) {
      suppressWarnings(rm(".Random.seed", envir = globalenv()))
    } else {
      assign(".Random.seed", old_seed, envir = globalenv())
    }
  }, add = TRUE)
  expr
}

#' Realise the model-averaging draw once, reproducibly
#'
#' Model averaging keeps \code{round(sample_size * wi)} of each component's
#' draws. The draw is seeded from a value stored on the
#' \code{bayesmanecfit}, so that every quantity computed from one object uses
#' the same draws, and the caller's RNG state is restored afterwards.
#'
#' \code{sample.kind} is pinned to \code{"Rejection"}, because the same seed
#' gives a different index under another \code{sample()} algorithm, and a
#' saved object would then disagree with its own stored summaries.
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
    # Objects saved before the seed was stored have none. Any fixed value
    # makes every call on such an object agree.
    seed <- 216
  }
  with_preserved_rng_state({
    set.seed(seed, sample.kind = "Rejection")
    out <- lapply(model_set, function(index) {
      size <- as.integer(round(sample_size * mod_stats[index, "wi"]))
      sample(seq_len(sample_size), size)
    })
  })
  names(out) <- model_set
  out
}

#' The model-averaging index to use for a given number of available draws
#'
#' Returns the index stored on the object, which does not depend on the R
#' version. Rebuilds it from the seed where the caller thins to another
#' number of draws (\code{ndraws}, \code{draw_ids}), or where the object
#' predates the stored index.
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
  # `==`, not identical(): sample_size is a double on the object and an integer
  # from nrow(), and identical() would always take the fallback.
  same_n <- isTRUE(object$sample_size == sample_size)
  if (!same_n && undercounted_set(object, sample_size)) {
    # Rebuild from the stored count too when an older set has no index.
    sample_size <- object$sample_size
    same_n <- TRUE
  }
  if (!is.null(idx) && same_n && all(model_set %in% names(idx))) {
    idx[model_set]
  } else {
    weighted_draw_index(model_set, sample_size, object$mod_stats,
                        object$w_draw_seed)
  }
}

#' Whether an undercounted set is asked for every draw its fits hold
#'
#' Sets saved by earlier versions store as \code{sample_size} the count
#' \code{ceiling((iter - warmup) / thin * chains)}, which falls short of the
#' draws the fits hold when \code{thin} does not divide \code{iter - warmup}.
#' The stored index of such a set draws from its first \code{sample_size}
#' rows, so it remains valid for a caller holding every draw, and the stored
#' summaries were built from it.
#'
#' @param object An object of class \code{\link{bayesmanecfit}}.
#' @param sample_size A \code{\link[base]{numeric}} vector of length 1, the
#' number of draws available per component.
#'
#' @return A \code{\link[base]{logical}} of length 1.
#' @noRd
undercounted_set <- function(object, sample_size) {
  fits <- object$mod_fits
  if (length(fits) == 0) {
    return(FALSE)
  }
  sim <- extract_simdat(fits[[1]])
  formula_n <- ceiling((sim$iter - sim$warmup) / sim$thin * sim$chains)
  isTRUE(object$sample_size == formula_n) &&
    isTRUE(sample_size == set_draw_count(fits))
}

#' @noRd
w_nec_calc <- function(index, mod_fits, draw_index) {
  mod_fits[[index]]$ne_posterior[draw_index[[index]]]
}

#' @noRd
w_pred_calc <- function(index, mod_fits, mod_stats) {
  mod_fits[[index]]$predicted_y * mod_stats[index, "wi"]
}

#' Take one model's weighted share of rows from its posterior matrix
#'
#' Keep \code{drop = FALSE}. With a one-row \code{newdata} the matrix has one
#' column, which would drop to a vector and be bound by \code{rbind} as a row,
#' so the summary would be taken across models rather than over draws.
#'
#' @noRd
w_pred_list_calc <- function(index, pred_list, draw_index) {
  pred_list[[index]][draw_index[[index]], , drop = FALSE]
}

#' Compute one model's grid posterior and thin it to its weight at once
#'
#' Only one model's full matrix then exists at a time. Thins through
#' \code{w_pred_list_calc()}, so that this and
#' \code{posterior_epred.bayesmanecfit()} keep the same draws.
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
  cens <- attr(x, "censored")
  if (!has_censoring(cens)) {
    # A draw whose curve does not reach the target within the predictor range
    # is NA, hence na.rm. Keep median() rather than routing this through
    # summarise_censored(): quantile() can differ from it in the last bit, and
    # archived analyses are compared against this value.
    x <- c(median(x, na.rm = TRUE), quantile(x, c(0.025, 0.975), na.rm = TRUE))
    names(x) <- c("Estimate", "Q2.5", "Q97.5")
    return(x)
  }
  # A beyond-range draw keeps its rank but has no value, so a quantile that
  # falls among such draws is reported, and flagged, as the bound it lies
  # beyond.
  out <- summarise_censored(x, c(0.5, 0.025, 0.975), cens)
  names(out) <- c("Estimate", "Q2.5", "Q97.5")
  out
}

#' A record of which draws of a posterior lie beyond the prediction range
#'
#' Carried as attribute \code{"censored"} on the per-draw vector, from the
#' point the posterior is realised to the point it is summarised. The two ends
#' are recorded separately because an \code{NA} on its own does not say which
#' one produced it.
#'
#' @param upper,lower A \code{\link[base]{numeric}} value, the end of the
#' prediction grid on the \emph{fitted} predictor scale, which is the scale the
#' draws are on.
#' @param above,below A \code{\link[base]{logical}} vector, one element per
#' draw.
#'
#' @return A \code{\link[base]{list}}.
#' @noRd
censoring_record <- function(upper, lower, above, below) {
  above[is.na(above)] <- FALSE
  below[is.na(below)] <- FALSE
  list(upper = upper, lower = lower, above = above, below = below)
}

#' @noRd
has_censoring <- function(cens) {
  !is.null(cens) && (any(cens$above) || any(cens$below))
}

#' Quantiles of a posterior whose beyond-range draws have no value
#'
#' Places a censored draw at \code{Inf} or \code{-Inf} before the quantile is
#' taken, so that it contributes its rank and no value. A quantile that lands
#' on one is reported as the bound and flagged in the
#' \code{"censored_summary"} attribute of the result.
#'
#' @param x A \code{\link[base]{numeric}} vector of per-draw estimates.
#' @param probs A \code{\link[base]{numeric}} vector of probabilities.
#' @param cens A record from \code{censoring_record()}.
#'
#' @return A named \code{\link[base]{numeric}} vector, with attribute
#' \code{"censored_summary"} where any entry is a bound.
#' @noRd
#' @importFrom stats quantile
summarise_censored <- function(x, probs, cens = attr(x, "censored")) {
  vals <- as.numeric(x)
  if (!has_censoring(cens)) {
    return(quantile(vals, probs = probs, na.rm = TRUE))
  }
  if (length(cens$above) != length(vals) ||
      length(cens$below) != length(vals)) {
    stop("The censoring record covers ", length(cens$above), " draws and the ",
         "posterior has ", length(vals), ".", call. = FALSE)
  }
  ranked <- vals
  ranked[cens$above] <- Inf
  ranked[cens$below] <- -Inf
  # type = 1 returns an order statistic. The default type = 7 interpolates,
  # and an interpolation ending at a censored draw is infinite, so a quantile
  # only partly inside the censored draws would be reported as the bound.
  out <- quantile(ranked, probs = probs, na.rm = TRUE, type = 1)
  at_upper <- is.infinite(out) & out > 0
  at_lower <- is.infinite(out) & out < 0
  bound <- rep("", length(out))
  bound[at_upper] <- ">="
  bound[at_lower] <- "<="
  out[at_upper] <- cens$upper
  out[at_lower] <- cens$lower
  attr(out, "censored_summary") <- list(
    bound = bound, upper = cens$upper, lower = cens$lower,
    n_above = sum(cens$above), n_below = sum(cens$below),
    # Count only the draws the quantile was taken over. An NA the record does
    # not explain is dropped by na.rm; warn_censored_draws() reports it.
    n_draws = sum(!is.na(ranked))
  )
  out
}

#' Take one model's share of a censoring record
#'
#' @param cens A record from \code{censoring_record()}, or \code{NULL}.
#' @param idx An integer vector of draw positions.
#'
#' @return A record covering \code{idx}, or \code{NULL}.
#' @noRd
subset_censoring <- function(cens, idx) {
  if (is.null(cens)) {
    return(NULL)
  }
  cens$above <- cens$above[idx]
  cens$below <- cens$below[idx]
  cens
}

#' Take a subset of posterior draws together with their censoring record
#'
#' \code{[} drops every attribute but names, so a posterior subset with it
#' loses the record of which draws lie beyond the prediction range, and a
#' censored draw can no longer be told from one that is merely missing. This
#' keeps the share of the record that belongs to the draws taken.
#'
#' @param m A posterior vector, optionally carrying attribute
#' \code{"censored"}.
#' @param idx An integer vector of draw positions.
#'
#' @return \code{m[idx]}, with attribute \code{"censored"} where \code{m}
#' carries one.
#' @noRd
subset_draws <- function(m, idx) {
  out <- m[idx]
  # Assigning NULL leaves m[idx] unchanged where m has no record.
  attr(out, "censored") <- subset_censoring(attr(m, "censored"), idx)
  out
}

#' Put a censoring record on another predictor scale
#'
#' Maps the bounds through \code{xform}: the \code{crf()} transformation, or a
#' caller's \code{xform}. Which draws are censored does not change, but under
#' a decreasing map \code{above} and \code{below} exchange.
#'
#' Remap every record with this function. Transforming the bounds by hand
#' leaves \code{above} and \code{below} naming the ends of the old scale,
#' which is wrong under \code{crf(-x)}.
#'
#' @param cens A record from \code{censoring_record()}, or \code{NULL}.
#' @param xform A monotone \code{\link[base]{function}}.
#'
#' @return A record on the new scale, or \code{NULL}.
#' @noRd
xform_censoring <- function(cens, xform) {
  if (is.null(cens)) {
    return(NULL)
  }
  new_upper <- xform(cens$upper)
  new_lower <- xform(cens$lower)
  swapped <- isTRUE(attr(cens, "swapped"))
  if (record_map_decreasing(xform, cens$lower, cens$upper, new_lower,
                            new_upper)) {
    # A decreasing map takes the top of the old scale to the bottom of the new
    # one, so the draws censored above are now censored below.
    out <- censoring_record(new_lower, new_upper, cens$below, cens$above)
    swapped <- !swapped
  } else {
    out <- censoring_record(new_upper, new_lower, cens$above, cens$below)
  }
  # Toggle the flag rather than set it: a record can be remapped twice, and
  # two decreasing maps cancel. censored_end() reads it to name the end of the
  # reported scale the draws lie beyond.
  attr(out, "swapped") <- swapped
  out
}

#' Whether a map reverses the two ends of a censoring record
#'
#' Reads the map at two points strictly inside the range, not at the ends: a
#' range starting at 0 under \code{function(x) -log(x)} maps its lower end to
#' \code{Inf}. Where the probe gives no answer, the images of the two ends
#' decide, infinite values included.
#'
#' @param xform A monotone \code{\link[base]{function}}.
#' @param lower,upper The two ends of the record, on the scale \code{xform}
#' takes values from.
#' @param new_lower,new_upper Their images under \code{xform}.
#'
#' @return A \code{\link[base]{logical}} value.
#' @noRd
record_map_decreasing <- function(xform, lower, upper, new_lower, new_upper) {
  probe <- interior_probe(lower, upper)
  if (!is.null(probe)) {
    # Muffle the NaN warning of a map undefined at the probe, such as log()
    # below 0; the ends then decide.
    img <- suppressWarnings(xform(probe))
    if (length(img) == 2 && all(is.finite(img)) && img[[1]] != img[[2]]) {
      return(img[[2]] < img[[1]])
    }
  }
  isTRUE(new_upper < new_lower)
}

#' Two points in increasing order strictly inside a range
#'
#' @param lower,upper The ends of the range, either of which may be infinite.
#'
#' @return A \code{\link[base]{numeric}} vector of length 2, or \code{NULL}
#' where the range is a single point or an end is missing.
#' @noRd
interior_probe <- function(lower, upper) {
  lo <- min(lower, upper)
  hi <- max(lower, upper)
  if (!isTRUE(hi > lo)) {
    return(NULL)
  }
  # Any two interior points will do for a monotone map, so where one end is
  # infinite the probe sits one and two units from the finite end.
  if (is.finite(lo) && is.finite(hi)) {
    lo + (hi - lo) * c(1, 2) / 3
  } else if (is.finite(hi)) {
    hi - c(2, 1)
  } else if (is.finite(lo)) {
    lo + c(1, 2)
  } else {
    c(-1, 1)
  }
}

#' The bound a class of beyond-range draw is reported at
#'
#' \code{end} names the class as the search saw it: \code{"above"} for draws
#' the curve did not reach within the range, \code{"below"} for draws it had
#' already passed where the range began. Read the bound through this function,
#' not from \code{upper} or \code{lower}: after a reversing remap, those
#' classes lie at the opposite ends of the reported scale.
#'
#' @param cens A record from \code{censoring_record()}.
#' @param end Either \code{"above"} or \code{"below"}.
#'
#' @return A \code{\link[base]{numeric}} value.
#' @noRd
censored_end <- function(cens, end = c("above", "below")) {
  end <- match.arg(end)
  if (xor(identical(end, "above"), isTRUE(attr(cens, "swapped")))) {
    cens$upper
  } else {
    cens$lower
  }
}

#' Combine per-component censoring records into one, in draw order
#'
#' Concatenating in draw order weights the censored fraction by each
#' component's share of the draws. The bounds reported are those true of every
#' component, the smallest upper and the largest lower bound. They differ only
#' in a set assembled by hand from separately built fits.
#'
#' @param parts A \code{\link[base]{list}} of records, one per component, each
#' already subset to the draws that component contributes, with \code{NULL}
#' for a component carrying no record.
#' @param n_draws An \code{\link[base]{integer}} vector, the number of draws
#' each component contributes, used where its record is \code{NULL}.
#'
#' @return A record, or \code{NULL} where no component is censored.
#' @noRd
concat_censoring <- function(parts, n_draws) {
  present <- !vapply(parts, is.null, logical(1))
  if (!any(present)) {
    return(NULL)
  }
  above <- unlist(lapply(seq_along(parts), function(i) {
    if (present[i]) parts[[i]]$above else logical(n_draws[i])
  }))
  below <- unlist(lapply(seq_along(parts), function(i) {
    if (present[i]) parts[[i]]$below else logical(n_draws[i])
  }))
  if (!any(above) && !any(below)) {
    return(NULL)
  }
  uppers <- vapply(parts[present], function(p) p$upper, numeric(1))
  lowers <- vapply(parts[present], function(p) p$lower, numeric(1))
  out <- censoring_record(min(uppers), max(lowers), above, below)
  # The components share a formula, so take the flag from the first.
  # censored_end() and warn_censored_draws() read it from the combined record.
  attr(out, "swapped") <- isTRUE(attr(parts[present][[1]], "swapped"))
  out
}

#' Report the censoring of a summarised estimate
#'
#' One line stating how many draws lie beyond each end of the prediction range
#' and where that end is, printed beside the estimate it qualifies. The count
#' is what a reader needs in order to decide whether the estimate is usable,
#' and it is not recoverable from the printed quantiles.
#'
#' @param cens The \code{"censored_summary"} attribute of a summary.
#' @param label A \code{\link[base]{character}} naming the quantity.
#'
#' @return \code{NULL}, invisibly. Called for the output.
#' @noRd
print_censoring_note <- function(cens, label = "estimate") {
  if (is.null(cens)) {
    return(invisible(NULL))
  }
  if (cens$n_above > 0) {
    cat("NB: ", cens$n_above, " of ", cens$n_draws, " draws of the ", label,
        " lie above ", signif(cens$upper, 3), ", the upper bound\n",
        "    of the prediction range. An entry marked >= is that bound and",
        " not a quantile.\n", sep = "")
  }
  if (cens$n_below > 0) {
    cat("NB: ", cens$n_below, " of ", cens$n_draws, " draws of the ", label,
        " lie below ", signif(cens$lower, 3), ", the lower bound\n",
        "    of the prediction range. An entry marked <= is that bound and",
        " not a quantile.\n", sep = "")
  }
  invisible(NULL)
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

#' Whether one chain's initial curve is a usable starting point
#'
#' @param x A \code{\link[base]{numeric}} vector, the initial curve evaluated
#' over the sorted predictor.
#' @param limits A \code{\link[base]{numeric}} vector of length 2, the band
#' from \code{\link{init_limits}}.
#'
#' @details \code{isTRUE()} because the first two clauses are \code{NA} on a
#' curve containing \code{NA}, and \code{\link{refine_inits}} reads the value
#' in an \code{if}.
#'
#' @return A \code{\link[base]{logical}} of length 1.
#'
#' @noRd
check_init_predictions <- function(x, limits) {
  isTRUE(
    min(x) > min(limits) &
      max(x) < max(limits) &
      !any(is.na(x)) &
      !any(is.infinite(x)) &
      !any(is.nan(x)) &
      x[1] > x[length(x)] &
      length(unique(x)) > 3
  )
}

#' @noRd
clean_names <- function(x) {
  paste0("Q", gsub("%", "", names(x), fixed = TRUE))
}


#' The ">= " or "<= " that marks one entry of a summarised estimate
#'
#' Empty where the entry is an ordinary quantile, which is every entry of an
#' estimate that carries no censoring record.
#'
#' @param values A summarised estimate.
#' @param i The entry to mark.
#'
#' @return A \code{\link[base]{character}} value.
#' @noRd
bound_prefix <- function(values, i) {
  cens <- attr(values, "censored_summary")
  if (is.null(cens) || !nzchar(cens$bound[i])) {
    return("")
  }
  paste0(cens$bound[i], " ")
}

#' Collect the censoring records of estimates that are about to be stacked
#'
#' \code{rbind()} keeps the numbers and drops every attribute, so a table built
#' by stacking one estimate per row loses the marks that say which entries are
#' bounds. This returns the records in row order, for
#' \code{attr(mat, "censored_summary")}, and \code{NULL} where no row carries
#' one.
#'
#' @param estimates A \code{\link[base]{list}} of summarised estimates, in the
#' order their rows appear.
#'
#' @return A \code{\link[base]{list}} of records, or \code{NULL}.
#' @noRd
row_censoring <- function(estimates) {
  recs <- lapply(estimates, attr, "censored_summary")
  if (all(vapply(recs, is.null, logical(1)))) {
    return(NULL)
  }
  recs
}

#' Print a note for each row of a stacked table that carries a bound
#'
#' @param recs The \code{"censored_summary"} attribute of a stacked matrix.
#' @param labels The row names of that matrix.
#'
#' @return \code{NULL}, invisibly. Called for the output.
#' @noRd
print_row_censoring_notes <- function(recs, labels) {
  if (is.null(recs) || !is.null(recs$bound)) {
    return(invisible(NULL))
  }
  for (i in seq_along(recs)) {
    print_censoring_note(recs[[i]], labels[i])
  }
  invisible(NULL)
}

#' Print a matrix of estimates, marking any entry that is a bound
#'
#' A one-row matrix carrying a \code{"censored_summary"} attribute has each
#' censored entry prefixed with \code{">="} or \code{"<="}. The number printed
#' is then the end of the prediction range rather than a quantile of the
#' posterior, and the prefix is what says so.
#'
#' @noRd
print_mat <- function(x, digits = 2) {
  fmt <- paste0("%.", digits, "f")
  out <- x
  for (i in seq_len(ncol(x))) {
    out[, i] <- sprintf(fmt, x[, i])
  }
  # One record for a one-row matrix, or a list of one record per row for a
  # stacked table. rbind() drops the records, so a caller that stacks
  # estimates attaches them with row_censoring().
  cens <- attr(x, "censored_summary")
  if (!is.null(cens)) {
    recs <- if (is.null(cens$bound)) cens else list(cens)
    if (length(recs) == nrow(out)) {
      for (i in seq_len(nrow(out))) {
        b <- recs[[i]]$bound
        if (is.null(b) || length(b) != ncol(out)) {
          next
        }
        marked <- nzchar(b)
        out[i, marked] <- paste0(b[marked], " ", out[i, marked])
      }
    }
  }
  # Drop the record before printing, or print.default() lists it under the
  # matrix.
  attr(out, "censored_summary") <- NULL
  print(out, quote = FALSE, right = TRUE)
  invisible(x)
}

#' @noRd
clean_mod_weights <- function(x) {
  a <- x$mod_stats[, !sapply(x$mod_stats, function(z) all(is.na(z)))]
  as.matrix(a[, -1])
}

#' @noRd
clean_nec_vals <- function(x, all_models, ecx_models, vals = NULL) {
  if (!is_bayesnecfit(x) && !is_bayesmanecfit(x)) {
    stop("Wrong input class.")
  }
  # The stored summary, unless summary() supplies one transformed by an xform.
  # See summary_ne_vals().
  if (is.null(vals)) {
    vals <- if (is_bayesnecfit(x)) x$ne else x$w_ne
  }
  mat <- t(as.matrix(vals))
  # as.matrix() drops the censoring record, so copy it by hand, or summary()
  # prints the bound as though it were a quantile.
  attr(mat, "censored_summary") <- attr(vals, "censored_summary")
  # Prefer the recorded ne_type: for a hurdle fit the estimate's type depends
  # on the equations of both blocks, not on the model name alone.
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
  # as.matrix() keeps names and drops everything else, so the record ecx() set
  # is copied onto the matrix by hand, exactly as in clean_nec_vals().
  attr(mat, "censored_summary") <- attr(ec, "censored_summary")
  print_mat(mat)
  print_censoring_note(attr(ec, "censored_summary"), sub(" estimate:$", "",
                                                         ecx_tag))
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
  # Shift a response with every observation at one bound inside the support
  # by 1 / (2n), as survival_by_x() does, so that its top prior and initial
  # value are finite. Only ecxflat reaches here with such a response.
  if (isTRUE(family$family %in% c("bernoulli", "binomial", "beta_binomial",
                                  "beta")) &&
        length(response) > 0 && !anyNA(response) &&
        !is.na(response_bound_reached(response))) {
    eps <- 1 / (2 * length(response))
    response <- pmin(pmax(response, eps), 1 - eps)
  }
  # A function, so that it is evaluated only where used: on an all-zero
  # response, valid for a zero-inflated family, it warns and returns Inf.
  min_z_val <- function() min(response[which(response > 0)]) / 100
  if (link_tag == "logit") {
    max_o_val <- max(response[which(response < 1)]) +
      (1 - max(response[which(response < 1)])) * 0.99
  }
  lr <- linear_rescale
  custom_name <- check_custom_name(family)
  if (link_tag %in% c("logit", "log")) {
    if (family$family %in% c("bernoulli", "binomial", "beta_binomial")) {
      if (contains_zero(response)) {
        response <- lr(response, r_out = c(min_z_val(), max(response)))
      }
      if (contains_one(response)) {
        response <- lr(response, r_out = c(min(response), max_o_val))
      }
      response <- family$linkfun(response)
    } else {
      if (contains_zero(response)) {
        response <- lr(response, r_out = c(min_z_val(), max(response)))
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
      response <- lr(response, r_out = c(min_z_val(), max(response)))
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
#' A \code{\link{bayesnechurdlefit}} takes \code{which} and a joint two-block
#' fit takes \code{dpar}. Either argument passed to the other's method would
#' fall into \code{...} and be ignored, returning the default component with
#' no warning, so it is refused.
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
#' \code{\link{nec}} has no block selection for a joint two-block fit: it
#' returns the combined threshold, the per-draw minimum of the two blocks. A
#' supplied \code{dpar} would otherwise be ignored.
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

#' Guard against `no_effect` being passed to a class that cannot use it
#'
#' \code{no_effect} applies only where exactly one equation is fitted per
#' level, which is a \code{\link{bayesnecjointfit}}. A
#' \code{\link{bayesmanecfit}}, or a \code{\link{bayesnecgroupfit}} holding
#' one per level, averages over equations of both kinds and has no single type
#' to label. The argument is refused rather than ignored.
#'
#' @param dots The \code{...} of the calling method, as a list.
#' @param object The object the method was called on.
#'
#' @return Invisibly \code{TRUE}, or an error.
#'
#' @noRd
check_no_effect_arg <- function(dots, object) {
  if ("no_effect" %in% names(dots)) {
    stop("`no_effect` selects between the nec parameter and the no-effect",
         " estimate each level's own equation supports. It applies to a",
         " bayesnecjointfit, where exactly one equation is fitted per level.",
         " This object is a ", class(object)[1], ", whose no-effect estimate",
         " has one type already: see ?nec for what that type is, and nsec()",
         " for a NSEC from every model regardless of type.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Resolve which equations and whether the model average a method shows
#'
#' \code{predict()}, \code{plot()} and \code{autoplot()} on a
#' \code{\link{bayesmanecfit}} take \code{model}, naming equations of the set,
#' and \code{average}, choosing whether the model average is included. The
#' deprecated \code{all_models} is mapped onto the two.
#'
#' @param x A \code{\link{bayesmanecfit}}.
#' @param model \code{NULL}, or a character vector of equation names.
#' @param average A single logical value.
#' @param all_models \code{NULL}, or the deprecated argument as supplied.
#' @param new_supplied Whether the calling method was given \code{model} or
#' \code{average} explicitly.
#'
#' @return A list with elements \code{model}, \code{NULL} or a character
#' vector of equation names in the order given and without duplicates, and
#' \code{average}, a single logical value.
#'
#' @importFrom chk chk_flag
#'
#' @noRd
resolve_model_average <- function(x, model, average, all_models = NULL,
                                  new_supplied = FALSE) {
  if (!is.null(all_models)) {
    # Refused rather than resolved in favour of one side: a call giving both
    # asks for two things, and neither argument can be read as overriding the
    # other without guessing which the caller meant.
    if (new_supplied) {
      stop("`all_models` cannot be combined with `model` or `average`, ",
           "which replace it. Remove `all_models` from the call.",
           call. = FALSE)
    }
    chk_flag(all_models)
    # lifecycle is not in Imports, so this is a plain warning(). Its class
    # lets plot.bayesnecgroupfit(), which calls this once per level, muffle
    # all but the first.
    warning(structure(
      class = c("bayesnec_all_models_deprecated", "warning", "condition"),
      list(message = paste0(
        "`all_models` is deprecated and will be removed in a later ",
        "release. Use `model` to name the equations to show and ",
        "`average` to choose whether the model average is shown. ",
        "`all_models = TRUE` is `model = <fit>$success_models, ",
        "average = FALSE`, and `all_models = FALSE` is the default, ",
        "`model = NULL, average = TRUE`."
      ), call = NULL)
    ))
    # Keep the behaviour all_models had: TRUE shows every equation and no
    # model average, FALSE the model average alone.
    if (all_models) {
      return(list(model = x$success_models, average = FALSE))
    }
    return(list(model = NULL, average = TRUE))
  }
  chk_flag(average)
  if (!is.null(model)) {
    if (!is.character(model) || length(model) == 0 || anyNA(model) ||
        !all(nzchar(model))) {
      stop("`model` must be NULL or a character vector naming equations of ",
           "the set.", call. = FALSE)
    }
    model <- unique(model)
    unknown <- setdiff(model, x$success_models)
    if (length(unknown) > 0) {
      groups <- intersect(unknown, names(mod_groups))
      # `model` names equations, not groups, though pull_out() accepts a group.
      # Say so, or the refusal reads as though the group were absent.
      group_note <- if (length(groups) > 0) {
        paste0(" ", paste0("\"", groups, "\"", collapse = ", "),
               " names a group of equations (see ?models), not an ",
               "equation; name the equations themselves.")
      } else {
        ""
      }
      stop("`model` names ",
           if (length(unknown) == 1) "an equation" else "equations",
           " not in this set: ", paste0("\"", unknown, "\"", collapse = ", "),
           ". The set holds ",
           paste0("\"", x$success_models, "\"", collapse = ", "), ".",
           group_note, call. = FALSE)
    }
  }
  if (is.null(model) && !average) {
    stop("Nothing is selected: `model` is NULL and `average` is FALSE. Name ",
         "one or more equations in `model`, or set `average = TRUE`.",
         call. = FALSE)
  }
  list(model = model, average = average)
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
retrieve_valid_family <- function(named_list, data, link_source = "none") {
  if (!"family" %in% names(named_list)) {
    y <- retrieve_var(data, "y_var", error = TRUE)
    tr <- retrieve_var(data, "trials_var")
    family <- set_distribution(y, support_integer = TRUE, trials = tr)
    # Chosen by bayesnec, so the link is bayesnec's to assign too.
    link_source <- "none"
  } else {
    family <- named_list$family
  }
  validate_family(family, link_source = link_source)
}

#' The LOO controls a model-averaged fit is built with
#'
#' Supplies the default weighting method, pseudo-BMA, wherever the caller has
#' not named one. Every route that assembles a model set calls this, so that
#' the weighting does not depend on which function assembled the set.
#'
#' @param loo_controls The caller's \code{loo_controls}, which may be missing.
#' @param family_str A \code{\link[base]{character}} string naming the family,
#' used only by \code{validate_loo_controls()}.
#'
#' @return A named \code{\link[base]{list}} of two elements, whose
#' \code{weights$method} is always set.
#'
#' @noRd
define_loo_controls <- function(loo_controls, family_str) {
  if (missing(loo_controls)) {
    loo_controls <- list(fitting = list(), weights = list(method = "pseudobma"))
  } else {
    loo_controls <- validate_loo_controls(loo_controls, family_str)
    # is.null(), not a test for the name: a caller may pass method = NULL,
    # which loo::loo_model_weights() would resolve to stacking.
    if (is.null(loo_controls$weights$method)) {
      loo_controls$weights$method <- "pseudobma"
    }
  }
  loo_controls
}

#' \code{loo_controls$weights} for a method that may be unknown
#'
#' An empty list where the method is \code{NULL}, so that
#' \code{define_loo_controls()} supplies the default rather than the
#' caller passing \code{method = NULL} down to \code{loo}.
#'
#' @param method A length-1 \code{\link[base]{character}}, or \code{NULL}.
#'
#' @return A named \code{\link[base]{list}}, possibly empty.
#'
#' @noRd
weights_controls <- function(method) {
  if (is.null(method)) list() else list(method = method)
}

#' @noRd
retrieve_var <- function(data, var, error = FALSE) {
  bnec_vars <- attr(data, "bnec_pop")
  bnec_pop <- names(bnec_vars)
  v_pos <- which(bnec_pop == var)
  # A role the formula does not include has no entry in bnec_pop, and indexing
  # bnec_vars with it below would fail with "subscript out of bounds". Name
  # the missing term instead.
  if (length(v_pos) == 0) {
    if (error) {
      stop("The formula has no ", pop_var_role(var),
           ". See ?bayesnecformula", call. = FALSE)
    }
    return(NULL)
  }
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

#' The term of a formula that a role in \code{bnec_pop} stands for
#'
#' @param var The name of an entry of \code{attr(data, "bnec_pop")}, such as
#' \code{"trials_var"}.
#'
#' @return A \code{\link[base]{character}} string naming the term as a user
#' writes it, or \code{var} in quotes where the role is not one of these.
#'
#' @noRd
pop_var_role <- function(var) {
  roles <- c(y_var = "response", x_var = "predictor in crf()",
             trials_var = "trials() term", cens_var = "cens() term",
             cens_y2_var = "upper bound in its cens() term",
             rate_var = "rate() term")
  if (var %in% names(roles)) roles[[var]] else paste0("\"", var, "\"")
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
#' Accepts the encodings \code{brms:::prepare_cens} accepts: partially matched
#' strings, logicals, or the integer codes. Reimplemented so that bayesnec
#' does not depend on a brms internal. An unrecognised value is returned as
#' \code{NA}, for brms to reject.
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
  asymptote_observed = TRUE,
  predictor_scale = "auto",
  model_survival = NULL,
  disp_spec = NULL,
  group_spec = NULL,
  level_spec = NULL,
  rate_response = FALSE,
  truncated_response = FALSE
) {
  # Recorded before anything below fills them in. A composed joint refit
  # rebuilds both from the per-level equations, and it must not overwrite a set
  # the caller wrote themselves. See compose_level_defaults().
  init_supplied <- "init" %in% names(brm_args)
  prior_supplied <- !is.null(brm_args$prior)
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
  # Raise adapt_delta where a group-level offset can carry mu outside its
  # support: the likelihood then throws and Stan counts a divergent
  # transition. A smaller step size is the only remedy here, and it is costly,
  # so it is raised only where needed.
  #
  # It is not needed where every deviation is applied on a scale it cannot
  # leave and the equation's mean lies between bot and top. Test that with
  # mu_confined_by_pars(), not ogl_transform_kind(), which misses the hormesis
  # equations on (0, Inf). An ogl term is the exception: ogl is untransformed
  # where ogl_transform_kind() refuses, so its answer decides.
  group_is_bounded <- mu_confined_by_pars(model) &&
    (!isTRUE(group_spec$ogl) ||
       !identical(ogl_transform_kind(model, family), "none"))
  if (!is.null(group_spec) && mu_is_constrained(family) && !group_is_bounded) {
    ctrl <- if ("control" %in% names(brm_args)) brm_args$control else list()
    if (!("adapt_delta" %in% names(ctrl))) {
      ctrl$adapt_delta <- 0.99
      brm_args$control <- ctrl
    }
  }
  # The prior scale for the ogl intercept and its group-level standard
  # deviation depends on which scale the deviation is applied on, so the kind
  # is decided here, where family and group_spec are both in hand, and carried
  # into define_prior() on group_spec.
  if (!is.null(group_spec)) {
    group_spec$ogl_transform <- if (isTRUE(group_spec$ogl)) {
      ogl_transform_kind(model, family)
    } else {
      "none"
    }
  }
  build_defaults <- function() {
    define_prior(
      model,
      family,
      predictor,
      response,
      prior_type = prior_type,
      asymptote_observed = asymptote_observed,
      predictor_scale = predictor_scale,
      model_survival = model_survival,
      disp_spec = disp_spec,
      group_spec = group_spec,
      rate_response = rate_response,
      truncated_response = truncated_response
    )
  }
  priors <- try(validate_priors(brm_args$prior, model), silent = TRUE)
  if (inherits(priors, "try-error")) {
    # No usable prior from the user, so the defaults are the fit. If they cannot
    # be built the error is the right outcome and is allowed to propagate.
    brm_args$prior <- build_defaults()
  } else {
    # A failure to build the defaults must not block a user's own prior. Use
    # the user's set as supplied, without a warning: without the defaults
    # there is no way to tell whether it is incomplete.
    default_priors <- try(build_defaults(), silent = TRUE)
    brm_args$prior <- if (inherits(default_priors, "try-error")) {
      priors
    } else {
      fill_missing_priors(priors, default_priors, model)
    }
  }
  check_transformed_par_bounds(brm_args$prior, group_spec, family)
  # Search only where no `init` was supplied, whatever skip_check says: the
  # search result would overwrite the caller's values.
  if (!("init" %in% names(brm_args))) {
    # amend() passes the family stored in the fit, which records Gamma as gamma.
    msg_tag <- canonical_fam_tag(family)
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
    # Remove the disp() parameters before the init search, which evaluates
    # only the mean curve. disp_inits() adds them back below; leaving them to
    # Stan's own random initialisation is not safe.
    init_priors <- brm_args$prior
    disp_par_names <- disp_pars(disp_spec)
    if (length(disp_par_names) > 0) {
      init_priors <- init_priors[!init_priors$nlpar %in% disp_par_names, ]
    }
    # Remove the group-level parameters too: the standard deviations by class,
    # and the deviation intercepts (ogl, topgl, botgl) by name, because they
    # have class "b" and pass every class filter. The names are reserved, so
    # no row with one is a curve coefficient.
    init_priors <- init_priors[init_priors$class != "sd", ]
    init_priors <- init_priors[!init_priors$nlpar %in%
                                 generated_term_names(), ]
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
        family = family,
        dpar = hurdle_dpar(family),
        seed = init_seed,
        model_survival = model_survival,
        asymptote_observed = asymptote_observed
      )
    } else {
      make_good_inits(
        model,
        predictor,
        response_link,
        family = family,
        priors = init_priors,
        chains = brm_args$chains,
        seed = init_seed,
        asymptote_observed = asymptote_observed
      )
    }
    if (length(inits) == 1 && "random" %in% names(inits)) {
      inits <- inits$random
    }
    # Add the disp() inits only where the search returned per-chain values.
    # Pass the priors in force, the user's included, so that a bounded slope
    # starts inside its bounds: Stan refuses an initial value on the bound.
    if (length(disp_par_names) > 0 && !is.character(inits)) {
      d_init <- disp_inits(disp_spec, family, response, predictor = predictor,
                           prior_type = prior_type, priors = brm_args$prior,
                           rate_response = rate_response,
                           truncated_response = truncated_response)
      inits <- lapply(inits, function(chain) c(chain, d_init))
    }
    # Drop the init of a parameter whose prior is constant: Stan declares it a
    # transformed parameter, so the init initialises nothing. Read the priors
    # from brm_args$prior, so that a constant disp() prior is caught too.
    if (!is.character(inits)) {
      all_priors <- as.data.frame(brm_args$prior)
      # nzchar(), not !is.na(): brms records an absent nlpar as "", never NA.
      is_const <- is_constant_prior(all_priors$prior) &
        all_priors$class == "b" & nzchar(all_priors$nlpar)
      const_pars <- paste0("b_", all_priors$nlpar[is_const])
      if (length(const_pars) > 0) {
        inits <- lapply(inits, function(chain) {
          chain[!names(chain) %in% const_pars]
        })
      }
    }
    brm_args$init <- inits
  }
  # Last, so that the curve coefficients it replicates are whatever the search
  # above settled on and the dispersion prior it appends cannot reach
  # make_inits(). See add_level_defaults().
  if (!is.null(level_spec)) {
    brm_args <- add_level_defaults(brm_args, level_spec, family, response,
                                   predictor = predictor,
                                   prior_type = prior_type,
                                   predictor_scale = predictor_scale,
                                   init_supplied = init_supplied,
                                   prior_supplied = prior_supplied)
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

#' Check new data against the family a refit uses, and correct them for it
#'
#' @param x A \code{\link[base]{list}} of \code{\link{prebayesnecfit}}
#' objects, one per model of the set being refitted.
#' @param data The data the refit is given: the caller's \code{newdata}, or
#' the fit's stored data where only a family was supplied.
#' @param family The family passed to \code{update()}, or \code{NULL}.
#' @param on_fit Whether the caller returns an object that stores the
#' substitution record; see \code{report_substitutions()}.
#'
#' @details Every check follows one family, the one the refit uses (see
#' \code{refit_family()}). No family is read off the data, so new data never
#' produce a change of family, and \code{changed_family} is \code{TRUE} only
#' where the caller supplied a family that differs from the fitted one.
#'
#' @return A named \code{\link[base]{list}}: \code{changed_family}, the
#' corrected \code{data}, and the \code{substitutions} record.
#'
#' @noRd
#' @importFrom stats model.frame
check_update_data <- function(x, data, family = NULL, on_fit = TRUE) {
  # Validate once, before the loop. A family update.bnecfit() has validated
  # already is returned unchanged.
  if (!is.null(family)) {
    family <- validate_family(family)
  }
  substitutions <- NULL
  for (i in seq_along(x)) {
    formula <- extract_formula(x[[i]])
    bdat <- model.frame(formula, data = data, run_par_checks = TRUE)
    model <- get_model_from_formula(formula)
    # Check against the family the refit uses: the one supplied, otherwise the
    # fit's own, which brms::update() keeps. Never read a family off the data.
    fam <- refit_family(x[[i]]$fit$family, family)
    # Refuse missing values before the bound check, so that a missing value is
    # reported as one and not as a response at a bound. Pass the fit's own
    # equation: an ecxflat fit may be refitted to a response at a bound, and a
    # curve equation may not.
    check_complete_cases(bdat)
    check_response_at_bound(bdat, fam, model = model)
    model <- check_models(model, fam, bdat)
    checked_df <- check_data(data = bdat, family = fam, model = model)
    # Write the corrections back to `data`, which is what brms::update() is
    # given. Take the record from the first model only: after the write-back,
    # later models see a response already off the boundary and record nothing.
    if (i == 1) {
      substitutions <- checked_df$substitutions
    }
    data <- write_back_checks(data, bdat, "y_var", checked_df$mod_dat$y)
  }
  # Report once, outside the loop. on_fit says whether the returned object
  # stores the record: a fit saved before bnec_record() existed does not, and
  # the message must not point to it.
  report_substitutions(substitutions, on_fit = on_fit)
  # Only a supplied family can differ from the fitted one.
  list(
    changed_family = !is.null(family) &&
      !identical(family_signature(family),
                 family_signature(x[[1]]$fit$family)),
    data = data,
    substitutions = substitutions
  )
}

#' The family a refit uses, under the tag bayesnec's checks key on
#'
#' @param fitted The family stored in the \code{brmsfit} being refitted.
#' @param supplied The validated family passed to \code{update()}, or
#' \code{NULL}.
#'
#' @details \code{brms::update()} keeps the stored family unless given
#' another, so new data are checked against that family. Nothing is read off
#' the data.
#'
#' The stored \code{brmsfamily} records Gamma as \code{gamma}, while
#' \code{check_data()} and \code{check_models()} key on \code{Gamma}, so the
#' tag is rewritten through \code{canonical_fam_tag()} on a copy. Only those
#' checks read the copy; it is never handed to \code{brms}.
#'
#' The stored family is not passed through \code{validate_family()}, which
#' would refuse a fit made by an earlier version on a link this version does
#' not support.
#'
#' @return An object of class \code{\link[stats]{family}}.
#'
#' @noRd
refit_family <- function(fitted, supplied = NULL) {
  if (!is.null(supplied)) {
    return(supplied)
  }
  fitted$family <- canonical_fam_tag(fitted)
  fitted
}

#' The tag and the links, which is what "a different family" means here
#'
#' @param family A \code{\link[stats]{family}}.
#'
#' @details The whole objects cannot be compared: \pkg{brms} stores a
#' \code{brmsfamily}, while \code{mod_fams} builds some families from
#' \pkg{stats}, and the two carry different components and closures.
#'
#' The tag is read through \code{canonical_fam_tag()}, because \pkg{brms}
#' records Gamma as \code{gamma}. A link the object does not record takes its
#' \pkg{brms} default: \code{stats::Gamma(link = "identity")} records no
#' \code{link_shape}, while the stored \code{brmsfamily} records
#' \code{link_shape = "log"}.
#'
#' @return A named \code{\link[base]{character}} vector.
#'
#' @importFrom brms brmsfamily
#'
#' @noRd
family_signature <- function(family) {
  tag <- canonical_fam_tag(family)
  char_links <- function(f) {
    links <- f[grepl("^link", names(f))]
    unlist(links[vapply(links, is.character, logical(1))])
  }
  links <- char_links(family)
  # NULL for a tag brms does not know, in which case the links are compared as
  # recorded.
  stock <- tryCatch(brmsfamily(tag), error = function(e) NULL)
  if (!is.null(stock)) {
    stock_links <- char_links(stock)
    unset <- setdiff(names(stock_links), names(links))
    links <- c(links, stock_links[unset])
  }
  c(family = tag, links[order(names(links))])
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

#' Is one population variable written into the formula as a transformation?
#'
#' Answers for one variable, where \code{\link{find_transformations}} answers
#' for the whole formula: \code{crf(log(x))} must not mark the response as
#' transformed. The comparison is by position in \code{bnec_pop}, as
#' \code{\link{retrieve_var}} finds a variable; a test by name would call
#' \code{y} transformed in \code{y ~ crf(log(y))}.
#'
#' @noRd
pop_var_is_transformed <- function(data, var) {
  bnec_pop_vars <- attr(data, "bnec_pop")
  v_pos <- which(names(bnec_pop_vars) == var)
  if (length(v_pos) != 1 || v_pos > length(data)) {
    return(FALSE)
  }
  # trials(n) and the other brms aterms wrap a variable without transforming
  # it, so they are stripped before the comparison, as find_transformations()
  # does.
  !identical(names(clean_aterms(data))[v_pos], bnec_pop_vars[[v_pos]])
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
  data_are_equal <- lapply(mod_fits, function(x) {
    fit_data <- x$fit$data
    # brms orders stored columns by their first appearance in the equation.
    # Hormesis and non-hormesis equations can therefore store the same data in
    # different orders, so compare columns by name rather than by position.
    fit_data <- fit_data[, order(names(fit_data)), drop = FALSE]
    as.matrix(fit_data)
  }) |>
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

#' Retain columns omitted from a brms fit
#'
#' \code{brm()} stores only columns used by its model frame. Plotting may still
#' need a categorical design variable that the curve was pooled over, so keep
#' those omitted columns once on the returned bayesnec object. The rows are put
#' in fitted-data order here, while both frames are available, rather than left
#' for a later plotting call to align by position.
#'
#' @param object A \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} under construction.
#' @param data The data frame supplied for fitting.
#'
#' @return \code{object}, with a \code{retained_data} element when \code{data}
#' contains columns absent from the stored \code{brmsfit} data.
#' @noRd
retain_unused_data <- function(object, data) {
  fit_data <- if (is_bayesmanecfit(object)) {
    object$mod_fits[[1]]$fit$data
  } else {
    object$fit$data
  }
  unused <- setdiff(names(data), names(fit_data))
  if (length(unused) == 0) {
    object$retained_data <- NULL
    return(object)
  }
  rows <- match(rownames(fit_data), rownames(data))
  if (anyNA(rows)) {
    stop("The fitted rows could not be matched to the supplied data while ",
         "retaining unused columns. This should not happen; please report it.",
         call. = FALSE)
  }
  object$retained_data <- data[rows, unused, drop = FALSE]
  object
}

#' Retained columns on a derived fit
#'
#' Subsetting or reweighting a fitted model set does not change its rows. Keep
#' the unused columns without rebuilding them from a component \code{brmsfit},
#' where they are deliberately absent.
#'
#' @noRd
carry_retained_data <- function(object, source) {
  if (!is.null(source[["retained_data"]])) {
    object$retained_data <- source$retained_data
  }
  object
}

#' Retained columns shared by separately fitted models
#'
#' A reconstructed model set has one observation frame only when the retained
#' columns from its fitted models are identical. Keep that frame once on the
#' result. Different frames are reported and omitted because plotting could not
#' align one of them with every model in the set.
#'
#' Missing retained columns are ignored. This preserves the behaviour of
#' \code{c.bnecfit()} for objects saved before retained columns were introduced.
#'
#' @noRd
retain_shared_data <- function(object, fits) {
  retained <- Filter(Negate(is.null), lapply(fits, function(fit) {
    fit[["retained_data"]]
  }))
  if (length(retained) > 0 &&
      all(vapply(retained[-1], identical, logical(1), retained[[1]]))) {
    object$retained_data <- retained[[1]]
  } else if (length(retained) > 1) {
    message("The objects retain different unused data columns; the combined ",
            "fit will not retain them for plotting groups.")
  }
  object
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
  # A range with one end missing has no meaningful reading, so refuse it.
  if (any(is.na(x_range))) {
    stop("x_range must be either NA or fully specified; ",
         "it cannot contain NA alongside a value.", call. = FALSE)
  }
  invisible(NULL)
}

#' @noRd
newdata_eval <- function(object, resolution, x_range) {
  # Every model of a set shares the data, so one model gives the grid. A
  # hurdle fit takes it from the survival component, which sees every row and
  # so the full predictor range, and adds the growth component's disp()
  # columns, which the combined prediction needs.
  hurdle <- NULL
  if (is_bayesnechurdlefit(object)) {
    hurdle <- object
    object <- object$survival
  }
  if (inherits(object, "bayesmanecfit")) {
    model_set <- names(object$mod_fits)
    object <- suppressMessages(pull_out(object, model = model_set[1]))
  }
  data <- model.frame(object$bayesnecformula, object$fit$data)
  bnec_pop_vars <- attr(data, "bnec_pop")
  newdata <- bnec_newdata(object, resolution = resolution, x_range = x_range)
  if (!is.null(hurdle)) {
    newdata <- hurdle_disp_placeholders(newdata, hurdle)
  }
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
  # One model gives the grid, as in newdata_eval(), and a hurdle fit takes it
  # from its survival component.
  hurdle <- NULL
  if (is_bayesnechurdlefit(object)) {
    hurdle <- object
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
      if (!is.null(hurdle)) {
        newdata <- hurdle_disp_placeholders(newdata, hurdle)
      }
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

#' Refuse a transformed parameter whose prior does not bound it to the support
#'
#' @param priors The prior set the fit will use, after any user rows have been
#' merged with the defaults.
#' @param group_spec The output of \code{\link{parse_group_terms}}.
#' @param family A \code{\link[stats]{family}} object.
#'
#' @details A group-level deviation on a transformed parameter is applied
#' multiplicatively. On \code{(0, 1)} the form is
#' \code{m e^o / (1 - m + m e^o)}, which is safe only while \code{m} is inside
#' \code{(0, 1)}. \code{\link{define_prior}} bounds the priors it generates,
#' but \code{\link{fill_missing_priors}} keeps a user row as written, so a user
#' prior with no \code{lb} or \code{ub} leaves the parameter unbounded.
#' Outside \code{[0, 1]} the form has a pole and returns large finite values
#' that Stan accepts, so such a prior is refused.
#'
#' @return \code{NULL}, invisibly. Called for the error.
#' @noRd
check_transformed_par_bounds <- function(priors, group_spec, family) {
  kind <- par_transform_kind(family)
  if (is.null(group_spec) || identical(kind, "none") || is.null(priors)) {
    return(invisible(NULL))
  }
  transformed <- Filter(function(p) par_is_transformed(p, kind),
                        intersect(group_spec$nlpars, par_transform_pars()))
  if (length(transformed) == 0) {
    return(invisible(NULL))
  }
  pr <- as.data.frame(priors)
  support <- mu_support(family)
  need_ub <- is.finite(support[2])
  bad <- character(0)
  for (p in transformed) {
    row <- pr[pr$class == "b" & pr$nlpar == p, , drop = FALSE]
    if (nrow(row) == 0) {
      next
    }
    has_lb <- any(nzchar(row$lb) & !is.na(row$lb))
    has_ub <- any(nzchar(row$ub) & !is.na(row$ub))
    if (!has_lb || (need_ub && !has_ub)) {
      bad <- c(bad, p)
    }
  }
  if (length(bad) > 0) {
    bounds <- if (need_ub) "lb = 0 and ub = 1" else "lb = 0"
    stop("The prior(s) for ", paste0("\"", bad, "\"", collapse = "; "),
         " do not bound the parameter to the range the ",
         canonical_fam_tag(family),
         " mean is defined on. A group-level term on ",
         paste0("\"", bad, "\"", collapse = "; "),
         " applies its deviation multiplicatively, which is only defined while",
         " the parameter is inside that range. Add ", bounds,
         " to the prior, or drop the group-level term on it. See",
         " ?bayesnecformula.", call. = FALSE)
  }
  invisible(NULL)
}

#' Fill a user-supplied prior set from the bayesnec defaults
#'
#' \code{validate_priors()} checks only that a supplied prior is a
#' \code{brmsprior} for the right model, so a parameter missing from an
#' incomplete set would fall through to a flat \pkg{brms} prior. Each missing
#' row is filled from \code{defaults}, and the warning names every parameter
#' filled.
#'
#' @param priors A \code{\link[brms]{brmsprior}} supplied by the user.
#' @param defaults A \code{\link[brms]{brmsprior}} from
#' \code{define_prior()}.
#' @param model A \code{\link[base]{character}} string naming the model.
#'
#' @return An object of class \code{\link[brms]{brmsprior}}.
#' @noRd
fill_missing_priors <- function(priors, defaults, model) {
  if (is.null(defaults) || nrow(defaults) == 0) {
    return(priors)
  }
  # A prior is identified by class, nlpar and dpar. A user row that sets coef
  # is a coefficient-level prior beside the parameter-level one, not a
  # replacement for it.
  key <- function(p) paste(p$class, p$nlpar, p$dpar, sep = "\r")
  missing <- !key(defaults) %in% key(priors)
  if (!any(missing)) {
    return(priors)
  }
  add <- defaults[missing, , drop = FALSE]
  named <- add$nlpar
  named[!nzchar(named)] <- add$class[!nzchar(named)]
  warning("The prior supplied for model ", model, " has no entry for ",
          paste0(named, collapse = ", "),
          ". Using the bayesnec default for ",
          if (length(named) > 1) "those parameters" else "that parameter",
          " rather than leaving ",
          if (length(named) > 1) "them" else "it",
          " on a flat prior; see ?get_priors to inspect the full set.",
          call. = FALSE)
  out <- rbind(priors, add)
  rownames(out) <- NULL
  out
}

#' The predictor expression written in a formula's crf() term
#'
#' Shared by \code{sub_x_transformation()}, which applies it to an estimate,
#' and \code{inline_x_transform()}, which reports it, so that an estimate is
#' said to be on a transformed scale exactly where it has been put on one.
#'
#' @param formula A \code{\link{bayesnecformula}}.
#'
#' @return The expression as a call, such as \code{log(x + 1)}, or a symbol
#' where the predictor is named untransformed.
#'
#' @importFrom stats terms
#' @noRd
crf_x_call <- function(formula) {
  x_str <- grep("crf(", labels(terms(formula)), fixed = TRUE, value = TRUE)
  str2lang(eval(parse(text = x_str)))
}

#' Put an estimate read off the prediction grid back on the fitted scale
#'
#' The prediction grid is built on the raw predictor column, so an x value
#' read off it is on the raw scale. Applying the formula's own transformation
#' puts it on the fitted scale, so that an inline \code{crf(log(x))} and a
#' pre-computed \code{log_x} column agree.
#'
#' Substitute for the predictor variable, not for the first argument of the
#' call: in \code{crf(log(x + 1))} that argument is \code{x + 1}. A
#' \code{crf()} call naming more than one variable is refused, because there
#' is no single predictor to substitute for.
#'
#' @param value A \code{\link[base]{numeric}} vector on the raw predictor
#' scale.
#' @param formula A \code{\link{bayesnecformula}}.
#'
#' @return A \code{\link[base]{numeric}} vector on the fitted scale.
#'
#' @importFrom stats setNames
#' @noRd
sub_x_transformation <- function(value, formula) {
  x_call <- crf_x_call(formula)
  if (!inherits(x_call, "call")) {
    return(value)
  }
  x_vars <- all.vars(x_call)
  if (length(x_vars) != 1) {
    stop("The crf() term ", deparse(x_call), " names ", length(x_vars),
         " variables, so there is no single predictor to put the estimate ",
         "back on the scale of. Compute the transformation into its own ",
         "column and name that column in crf() instead.", call. = FALSE)
  }
  sub_list <- setNames(list(quote(.bnec_x_value)), x_vars)
  # Evaluate in the formula's environment, so that a function the user
  # defined, as in crf(squared(x)), is found.
  eval(do.call("substitute", list(x_call, sub_list)),
       list(.bnec_x_value = value), enclos = formula_env(formula))
}

#' The x value at which a curve first crosses a target response
#'
#' Interpolates linearly between the two grid points that bracket the
#' crossing. Returns \code{NA} where the curve does not reach the target
#' within the predictor range, since the estimate is not identified there.
#'
#' A curve at or below the target at the first grid point returns
#' \code{x_start}: a search for a sign change cannot tell it from a curve that
#' never reaches the target. This also covers a hormetic curve that starts
#' below the target, whose first crossing would be on its rising limb. A
#' curve that starts above a target anchored on the control crosses only on
#' its descending limb.
#'
#' @param y A \code{\link[base]{numeric}} vector, one draw's predicted curve
#' over \code{x_vec}.
#' @param target A \code{\link[base]{numeric}} value, the response level
#' sought.
#' @param x_vec A \code{\link[base]{numeric}} vector of predictor values.
#' @param x_start The value returned where the curve has already reached the
#' target at the first grid point. The default \code{NA_real_} suits an ECx
#' target, which cannot be met before the grid begins. The NSEC callers pass
#' the control concentration, which is the estimate for such a draw.
#' @return A \code{\link[base]{numeric}} value, or \code{NA}.
#'
#' @importFrom modelbased zero_crossings
#' @noRd
crossing_x <- function(y, target, x_vec, x_start = NA_real_) {
  if (all(is.na(y)) || is.na(target)) {
    return(NA_real_)
  }
  # y[1] rather than the first value that is not NA: x_start means the target is
  # already met where the grid begins, and a curve whose first prediction is
  # missing says nothing about that, so it falls through to the search.
  if (!is.na(y[1]) && y[1] <= target) {
    return(x_start)
  }
  # zero_crossings() stops on a single value instead of returning nothing.
  # nsec_from_posterior() can pass one where x_range leaves one grid point.
  if (length(y) < 2) {
    return(NA_real_)
  }
  val <- suppressWarnings(min(zero_crossings(y - target)))
  if (!is.finite(val)) {
    return(NA_real_)
  }
  floor_x <- x_vec[floor(val)]
  ceiling_x <- x_vec[ceiling(val)]
  floor_x + (val - floor(val)) * (ceiling_x - floor_x)
}

#' Refuse a resolution that defines no interval
#'
#' A grid of one point has no interval, so no estimate can be read off it.
#' Call this where a call that will build such a grid is first seen, so that
#' \code{bnec()} refuses before any model is fitted. It does not belong in
#' \code{check_args_newdata()}: \code{bnec_newdata()} accepts
#' \code{resolution = 1}.
#'
#' @param resolution The number of grid points requested.
#'
#' @return \code{NULL}, invisibly.
#' @noRd
check_resolution <- function(resolution) {
  if (resolution < 2) {
    stop("Argument `resolution` must be at least 2; a single grid point ",
         "defines no interval to read an estimate from.", call. = FALSE)
  }
  invisible(NULL)
}

#' The predictor value the control is read at
#'
#' The lowest \emph{observed} value of the predictor named in \code{crf()}, on
#' the scale the user supplied it on. It is read from the data, not from the
#' prediction grid, so that \code{x_range} does not change it or any estimate
#' anchored on it. A \code{\link{bayesnechurdlefit}} reads it from the
#' survival component, which sees every concentration tested.
#'
#' @param object A \code{\link{bayesnecfit}}, \code{\link{bayesmanecfit}} or
#' \code{\link{bayesnechurdlefit}}.
#'
#' @return A \code{\link[base]{numeric}} value.
#'
#' @importFrom stats model.frame
#' @noRd
control_x <- function(object) {
  grid_obj <- object
  if (is_bayesnechurdlefit(grid_obj)) {
    grid_obj <- grid_obj$survival
  }
  if (inherits(grid_obj, "bayesmanecfit")) {
    grid_obj <- suppressMessages(
      pull_out(grid_obj, model = names(grid_obj$mod_fits)[1])
    )
  }
  # One level of a joint refit reads its own lowest concentration, not the
  # lowest of every level together. See joint_level_x().
  if (inherits(grid_obj, "bayesnecjointlevel") && length(grid_obj$level_x)) {
    return(min(grid_obj$level_x))
  }
  mod_dat <- model.frame(grid_obj$bayesnecformula, grid_obj$fit$data)
  x_var <- attr(mod_dat, "bnec_pop")[["x_var"]]
  min(grid_obj$fit$data[[x_var]])
}

#' The NSEC of each draw
#'
#' The concentration at which each draw's curve reaches the reference, sought
#' from the control upward.
#'
#' A draw whose own control value is at or below the reference reaches it at
#' the control, and its NSEC is the control concentration, not a failure to
#' estimate one. These draws are found from the control posterior, not from
#' the first grid point, so that the result does not depend on
#' \code{x_range}.
#'
#' Only the grid above the control is searched, because the reference is
#' defined there. Where the grid reaches the control, the control posterior
#' becomes the first column, so that a draw crossing within the first grid
#' step is not lost.
#'
#' @param post A draws by grid \code{\link[base]{matrix}} of predicted means.
#' @param reference A \code{\link[base]{numeric}} value, the \code{sig_val}
#' quantile of the control posterior.
#' @param x_vec A \code{\link[base]{numeric}} vector of predictor values, the
#' columns of \code{post}.
#' @param x_control A \code{\link[base]{numeric}} value, the predictor value
#' the control is read at, from \code{control_x()}.
#' @param control A \code{\link[base]{numeric}} vector, the control posterior,
#' one value per row of \code{post}, being the predicted mean at
#' \code{x_control} and the vector \code{reference} is a quantile of.
#'
#' @return A \code{\link[base]{numeric}} vector, one value per draw, \code{NA}
#' where the curve does not reach the reference at any tested concentration.
#' @noRd
nsec_from_posterior <- function(post, reference, x_vec, x_control, control) {
  if (length(control) != nrow(post)) {
    stop("The control posterior has ", length(control), " values and the ",
         "prediction has ", nrow(post), " draws.", call. = FALSE)
  }
  keep <- x_vec > x_control
  if (!any(keep)) {
    # Reached by an x_range at or below the lowest observed value, or by a
    # resolution of 1. Refuse rather than return a vector of NA.
    stop("The prediction grid holds no concentration above ",
         signif(x_control, 3), ", the lowest observed value of the predictor, ",
         "so there is nothing above the control to read the NSEC from. Check ",
         "x_range, and that resolution is at least 2.", call. = FALSE)
  }
  if (min(x_vec) <= x_control) {
    # The grid reaches the control, so start the search there, with the
    # control posterior as the first column.
    x_kept <- c(x_control, x_vec[keep])
    post <- cbind(control, post[, keep, drop = FALSE])
  } else {
    # The grid begins above the control. The stretch between the two was not
    # asked for, so it is not searched and not interpolated across: a draw that
    # crossed there is NA, reported separately from the draws that never cross.
    x_kept <- x_vec
  }
  below_range <- logical(nrow(post))
  out <- vapply(seq_len(nrow(post)), function(i) {
    if (!is.na(control[i]) && control[i] <= reference) {
      return(x_control)
    }
    val <- crossing_x(post[i, ], reference, x_kept)
    if (is.na(val) && !is.na(post[i, 1]) && post[i, 1] <= reference) {
      below_range[i] <<- TRUE
    }
    val
  }, numeric(1))
  attr(out, "n_below_range") <- sum(below_range)
  # A censoring record needs to know which draws, not only how many.
  attr(out, "below_range") <- below_range
  attr(out, "x_searched_from") <- x_kept[1]
  out
}

#' Does the family have a lower bound on the response?
#'
#' Used to decide whether \code{type = "relative"} has a finite denominator
#' for an equation with no \code{bot} parameter. Such an equation tends to
#' zero, so its theoretical asymptote is 0 under any family whose support is
#' bounded below; under a family whose support is not, there is no bound and
#' no denominator. Of the families \code{\link{bnec}} accepts, \code{gaussian}
#' is the only one unbounded below.
#'
#' @param family A \code{\link[stats]{family}} object.
#'
#' @return A \code{\link[base]{logical}} value.
#' @noRd
family_has_lower_bound <- function(family) {
  !(family$family %in% c("gaussian", "student"))
}

#' The control posterior
#'
#' The predicted mean at the lowest \emph{observed} concentration, per draw.
#'
#' Read at the lowest observed predictor value, not at the first column of the
#' prediction grid, so that \code{x_range} does not change any reported
#' estimate.
#'
#' @param object A \code{\link{bayesnecfit}}.
#' @param newdata The prediction grid, used as the template for the columns
#' the grid pins (trials, rate denominators).
#' @param epred_fun A function taking a \code{newdata} and returning the
#' posterior expectation matrix, so that a two-block fit's \code{dpar}
#' handling reaches the control as well as the curve.
#' @param x_at The predictor value to read the control at. Defaults to
#' \code{NULL}, meaning this object's own lowest observed value. A
#' \code{\link{bayesnechurdlefit}} supplies it, because its two components are
#' fitted to different subsets --- growth to survivors only --- and the control
#' must be the same concentration on both sides or their product is not a
#' prediction at any one concentration.
#'
#' @return A \code{\link[base]{numeric}} vector, one control value per draw.
#'
#' @importFrom stats model.frame
#' @noRd
control_posterior <- function(object, newdata, epred_fun, x_at = NULL) {
  grid_obj <- object
  if (is_bayesnechurdlefit(grid_obj)) {
    grid_obj <- grid_obj$survival
  }
  if (inherits(grid_obj, "bayesmanecfit")) {
    grid_obj <- suppressMessages(pull_out(grid_obj, model = names(grid_obj$mod_fits)[1]))
  }
  mod_dat <- model.frame(grid_obj$bayesnecformula, grid_obj$fit$data)
  x_var <- attr(mod_dat, "bnec_pop")[["x_var"]]
  control_nd <- newdata[1, , drop = FALSE]
  if (is.null(x_at)) {
    # The same value control_x() returns. Read from grid_obj, which is already
    # resolved here, rather than by calling control_x() and repeating the
    # pull_out() and model.frame() this function has just done.
    x_at <- min(grid_obj$fit$data[[x_var]])
  }
  control_nd[[x_var]] <- x_at
  epred_fun(control_nd)[, 1]
}

#' Bring an estimate onto the scale the predictor axis is drawn on
#'
#' \code{nec()}, \code{ecx()} and \code{nsec()} return values on the fitted
#' scale, while \code{plot()} and \code{autoplot()} draw the predictor axis on
#' the recorded scale. Where the formula transforms the predictor, an
#' annotation drawn at the estimate's own value would be misplaced.
#'
#' Where the predictor is not transformed, \code{xform} is applied as it is to
#' the axis. Where it is, the estimate is inverted with the caller's
#' \code{xform}, or otherwise by interpolating on the grid. Either map is
#' applied through \code{remap_summary()}, so that a decreasing one keeps the
#' interval in order and the censoring marks correct.
#'
#' @param values A \code{\link[base]{numeric}} vector on the fitted scale.
#' @param bdat A model frame carrying the \code{bnec_pop} attribute.
#' @param formula A \code{\link{bayesnecformula}}.
#' @param x_grid_raw The prediction grid's predictor values, on the recorded
#' scale.
#' @param xform A function supplied by the caller.
#'
#' @return A \code{\link[base]{numeric}} vector on the axis scale.
#'
#' @noRd
to_axis_scale <- function(values, bdat, formula, x_grid_raw,
                          xform = identity) {
  if (!pop_var_is_transformed(bdat, "x_var")) {
    return(remap_summary(values, xform, x_grid_raw))
  }
  # Both branches below detect a decreasing map on the fitted-scale grid.
  fitted_grid <- sub_x_transformation(x_grid_raw, formula)
  if (!identical(xform, identity)) {
    return(remap_summary(values, xform, fitted_grid))
  }
  inverse <- grid_inverse(formula, x_grid_raw)
  if (is.null(inverse)) {
    return(values)
  }
  remap_summary(values, inverse, fitted_grid)
}

#' The inverse of an inline predictor transformation, read off the grid
#'
#' The prediction grid holds the same points on both scales, so interpolating
#' between them inverts any \code{crf()} transformation to within the grid's
#' resolution. Estimators that return values on the recorded scale call this
#' too, so that they and the plots agree. The returned function takes a
#' vector, so it can also be passed to \code{xform_censoring()}.
#'
#' @param formula A \code{\link{bayesnecformula}}.
#' @param x_grid_raw The prediction grid's predictor values, on the recorded
#' scale.
#'
#' @return A \code{\link[base]{function}} taking values on the fitted scale and
#' returning them on the recorded scale with their attributes kept, or
#' \code{NULL} where fewer than two grid points are finite on both scales.
#'
#' @details \code{approx(rule = 2)} clamps an estimate outside the grid to the
#' nearer end, so that an annotation is drawn at the axis limit rather than
#' dropped without a message. Only an already censored summary can lie outside
#' the grid.
#'
#' @importFrom stats approx
#' @noRd
grid_inverse <- function(formula, x_grid_raw) {
  fitted_grid <- sub_x_transformation(x_grid_raw, formula)
  keep <- is.finite(fitted_grid) & is.finite(x_grid_raw)
  if (sum(keep) < 2) {
    return(NULL)
  }
  fitted_grid <- fitted_grid[keep]
  raw_grid <- x_grid_raw[keep]
  function(values) {
    # Build from `values`, not rep(NA_real_, ...), to keep the attributes ecx()
    # sets: bind_ecx() reads attr(ecx_vals, "ecx_val").
    out <- values
    out[] <- NA_real_
    finite_v <- is.finite(values)
    out[finite_v] <- approx(x = fitted_grid, y = raw_grid,
                            xout = values[finite_v], rule = 2)$y
    out
  }
}

#' Whether a map reverses the order of a grid
#'
#' @param map A monotone \code{\link[base]{function}}.
#' @param grid A \code{\link[base]{numeric}} vector on the scale \code{map}
#' takes values from.
#'
#' @return A \code{\link[base]{logical}} value.
#' @noRd
map_is_decreasing <- function(map, grid) {
  grid <- grid[is.finite(grid)]
  if (length(grid) < 2) {
    return(FALSE)
  }
  ends <- map(range(grid))
  # isTRUE(): where the map gives NaN at an end, leave the order unchanged.
  isTRUE(ends[[1]] > ends[[2]])
}

#' Put a summarised estimate on another predictor scale
#'
#' The counterpart, for a summary, of \code{xform_censoring()}. A summary is
#' an estimate followed by the lower and upper limits of its interval. Under a
#' decreasing map the limits exchange, and so do the censoring marks: a draw
#' beyond the top of the old range lies below the foot of the new one.
#'
#' The direction is read from the mapped ends of the grid, not from the
#' record's bounds, because an uncensored summary has no record but its
#' interval must still be put in order. The summary is remapped rather than
#' the draws, so an unmarked entry can differ by one order statistic from the
#' estimator run with the same map.
#'
#' @param values A summarised estimate: the estimate, then the lower and upper
#' limits of its interval.
#' @param map A monotone \code{\link[base]{function}} taking \code{values} to
#' the new scale.
#' @param grid The prediction grid, on the scale \code{values} are on.
#'
#' @return \code{values} on the new scale, with the interval in order and the
#' record remapped.
#' @noRd
remap_summary <- function(values, map, grid) {
  decreasing <- map_is_decreasing(map, grid)
  out <- map(values)
  if (decreasing && length(out) >= 3) {
    out[2:3] <- out[3:2]
  }
  # Remap the record explicitly. Left to travel through the map, it would keep
  # the old scale's marks, or be dropped by a map that is not arithmetic.
  cens <- remap_censored_summary(attr(values, "censored_summary"), map,
                                 decreasing, summary_probs(values))
  if (decreasing && !is.null(cens) && length(cens$bound) == length(out)) {
    # A recomputed mark can fall on an entry whose value is a draw. Set every
    # marked entry to its bound, as summarise_censored() does.
    out[cens$bound == ">="] <- cens$upper
    out[cens$bound == "<="] <- cens$lower
  }
  attr(out, "censored_summary") <- cens
  out
}

#' The probability each entry of a summary is the quantile at
#'
#' Read from the names a summary is given: \code{"Estimate"} for the median
#' of \code{estimates_summary()}, \code{"Q"} followed by a percentage from
#' \code{clean_names()}, and a percentage followed by \code{"%"} as
#' \code{quantile()} writes it before \code{clean_names()} is applied. The
#' record does not keep the probabilities, and the names are the only place
#' they survive.
#'
#' @param values A summarised estimate.
#'
#' @return A \code{\link[base]{numeric}} vector, one probability per entry, or
#' \code{NULL} where any name is missing or of none of those forms.
#' @noRd
summary_probs <- function(values) {
  nms <- names(values)
  if (is.null(nms)) {
    return(NULL)
  }
  num <- "[0-9]*\\.?[0-9]+(e[-+]?[0-9]+)?"
  pct <- rep(NA_character_, length(nms))
  pct[nms == "Estimate"] <- "50"
  q_form <- grepl(paste0("^Q", num, "$"), nms)
  pct[q_form] <- sub("^Q", "", nms[q_form])
  pct_form <- grepl(paste0("^", num, "%$"), nms)
  pct[pct_form] <- sub("%$", "", nms[pct_form])
  probs <- as.numeric(pct) / 100
  if (anyNA(probs)) {
    return(NULL)
  }
  probs
}

#' The marks a censored summary gives each entry, from the counts alone
#'
#' Runs \code{quantile(type = 1)} over a sample holding only the ranks the
#' record states, \code{-Inf} for each draw below the range, \code{Inf} for
#' each above it and a finite value for the rest, so the index rule is the one
#' \code{summarise_censored()} applied and is not rewritten here.
#'
#' @param n_below,n_above,n_draws \code{\link[base]{integer}} counts from the
#' record.
#' @param probs The probability of each entry.
#'
#' @return A \code{\link[base]{character}} vector of marks.
#' @importFrom stats quantile
#' @noRd
marks_from_counts <- function(n_below, n_above, n_draws, probs) {
  ranked <- c(rep(-Inf, n_below), numeric(n_draws - n_below - n_above),
              rep(Inf, n_above))
  q <- quantile(ranked, probs = probs, type = 1, names = FALSE)
  out <- rep("", length(q))
  out[q == Inf] <- ">="
  out[q == -Inf] <- "<="
  out
}

#' Put the censoring record of a summary on another predictor scale
#'
#' Applies the rule of \code{xform_censoring()} to the record
#' \code{summarise_censored()} leaves on a summary. Under a decreasing map the
#' two bounds exchange, and so do the counts above and below.
#'
#' The marks are then recomputed from the exchanged counts, not reversed in
#' place, because a type 1 quantile is not symmetric under reflection where
#' the number of draws times the probability is a whole number. Where the
#' probabilities cannot be read from the names, the marks are reversed
#' instead.
#'
#' @param cens The \code{"censored_summary"} attribute of a summary, or
#' \code{NULL}.
#' @param map A monotone \code{\link[base]{function}}.
#' @param decreasing A \code{\link[base]{logical}} value, whether \code{map}
#' reverses the order of the grid.
#' @param probs The probability of each entry, from \code{summary_probs()}, or
#' \code{NULL}.
#'
#' @return A record on the new scale, or \code{NULL}.
#' @noRd
remap_censored_summary <- function(cens, map, decreasing, probs = NULL) {
  if (is.null(cens)) {
    return(NULL)
  }
  out <- cens
  new_upper <- map(cens$upper)
  new_lower <- map(cens$lower)
  if (!decreasing) {
    out$upper <- new_upper
    out$lower <- new_lower
    return(out)
  }
  out$upper <- new_lower
  out$lower <- new_upper
  out$n_above <- cens$n_below
  out$n_below <- cens$n_above
  counts <- c(out$n_below, out$n_above, cens$n_draws)
  if (length(probs) == length(cens$bound) && length(counts) == 3 &&
      all(is.finite(counts)) && counts[3] >= counts[1] + counts[2]) {
    out$bound <- marks_from_counts(out$n_below, out$n_above, cens$n_draws,
                                   probs)
    return(out)
  }
  reversed <- c(">=" = "<=", "<=" = ">=")
  bound <- cens$bound
  marked <- nzchar(bound)
  bound[marked] <- reversed[bound[marked]]
  if (length(bound) >= 3) {
    bound[2:3] <- bound[3:2]
  }
  out$bound <- unname(bound)
  out
}

#' Refuse an argument that has been removed, by name
#'
#' \code{\link{ecx}}, \code{\link{nsec}} and \code{\link{ecnsec}} take
#' \code{...}, so a removed argument such as \code{hormesis_def} would
#' otherwise be accepted and ignored.
#'
#' @param dots \code{list(...)} from the calling method.
#'
#' @return \code{NULL}, invisibly. Called for the error.
#' @noRd
check_removed_args <- function(dots) {
  if ("hormesis_def" %in% names(dots)) {
    stop("hormesis_def has been removed. Every ECx, NSEC and ECNSEC is now ",
         "measured from the control -- the predicted mean at the lowest ",
         "concentration -- which is what hormesis_def = \"control\" selected. ",
         "There is no longer an option to measure from the maximum of the ",
         "predicted curve. Drop the argument.", call. = FALSE)
  }
  invisible(NULL)
}

#' Report the draws whose estimate is not identified
#'
#' A no-effect or ECx estimate is \code{NA} for any draw whose curve does not
#' reach the target within the predictor range, so its summary is censored.
#' \code{ecx()} and \code{nsec()} warn as they build such a posterior. This
#' reports it for a path that summarises a posterior built elsewhere, such as
#' \code{nec()}, which reads what \code{expand_nec()} stored at fit time.
#'
#' @param values A \code{\link[base]{numeric}} vector of per-draw estimates.
#' @param estimate A \code{\link[base]{character}} label naming the quantity.
#' @param range_label A \code{\link[base]{character}} naming what the two
#' bounds are the ends of. Where \code{extrapolate} has replaced the bounds
#' they are not the ends of the prediction range, and the caller must say
#' what they are.
#'
#' @return \code{NULL}, invisibly. Called for the warning.
#' @noRd
warn_censored_draws <- function(values, estimate = "estimate", n_below = 0,
                               x_from = NULL, cens = attr(values, "censored"),
                               range_label = "prediction range") {
  # Classed, so that a method reporting its own censoring can muffle these
  # reports from its internal calls and nothing else they raise.
  raise <- function(msg) {
    warning(structure(class = c("bayesnec_censored", "warning", "condition"),
                      list(message = msg, call = NULL)))
  }
  if (is.null(cens)) {
    # Without a record, report from the count of NA draws.
    n_missing <- sum(is.na(values)) - n_below
    if (n_missing > 0) {
      raise(paste0("The ", estimate, " is not identified for ", n_missing,
                   " of ", length(values), " draws, whose curve does not ",
                   "reach the target anywhere in the predictor range. Those ",
                   "draws return NA and are excluded from the summary."))
    }
    if (n_below > 0) {
      raise(paste0("The ", estimate, " is not identified for ", n_below,
                   " of ", length(values), " draws, whose curve reached the ",
                   "reference below ", signif(x_from, 3), ", the lowest ",
                   "concentration in the prediction range. Those draws return ",
                   "NA and are excluded from the summary."))
    }
    return(invisible(NULL))
  }
  # Key each report on the end of the reported scale the draws lie beyond,
  # read from the same fields as print_censoring_note(), so that the two agree
  # after a decreasing crf() or xform. An NA the record does not explain is
  # reported on its own and left out of the denominator, as there.
  unexplained <- is.na(values) & !cens$above & !cens$below
  n_draws <- length(values) - sum(unexplained)
  if (sum(cens$above) > 0) {
    raise(paste0("The ", estimate, " is not identified for ",
                 sum(cens$above), " of ", n_draws, " draws, which lie at or ",
                 "above ", signif(cens$upper, 3), ", the upper bound of the ",
                 range_label, ". The summary is censored there: those ",
                 "draws keep their rank in it and are given no value, so a ",
                 "quantile falling among them is reported as a bound."))
  }
  # After the upper report, matching the order nsec.bayesnecfit() raises the
  # two in, so a call producing both reads the same way whichever method it
  # came through.
  if (sum(cens$below) > 0) {
    raise(paste0("The ", estimate, " is not identified for ",
                 sum(cens$below), " of ", n_draws, " draws, which lie at or ",
                 "below ", signif(cens$lower, 3), ", the lower bound of the ",
                 range_label, ". The summary is censored there: those ",
                 "draws keep their rank in it and are given no value, so a ",
                 "quantile falling among them is reported as a bound."))
  }
  if (sum(unexplained) > 0) {
    raise(paste0("The ", estimate, " could not be computed for ",
                 sum(unexplained), " of ", length(values), " draws, which lie ",
                 "beyond neither end of the prediction range. Those draws ",
                 "return NA and are left out of the summary and out of the ",
                 "fraction reported beside it."))
  }
  invisible(NULL)
}

#' Evaluate an expression, muffling only the censoring report
#'
#' For a method that raises its own \code{warn_censored_draws()} report about
#' the vector it returns, and reaches that vector through calls that each raise
#' one of their own about a part of it.
#'
#' @param expr An expression.
#'
#' @return The value of \code{expr}.
#' @noRd
without_censored_warning <- function(expr) {
  withCallingHandlers(
    expr,
    bayesnec_censored = function(w) invokeRestart("muffleWarning")
  )
}

#' The label a fit gives its no-effect estimate
#'
#' \code{expand_nec()} records whether the stored no-effect posterior is a
#' \emph{NEC}, an \emph{NSEC} or an N(S)EC, and which it is depends on the
#' equation and, for a two-block fit, on both blocks. Read it rather than
#' re-deriving it, and fall back only for an object built by an earlier version
#' that has no such slot.
#'
#' @param object A \code{\link{bnecfit}}.
#'
#' @return A \code{\link[base]{character}} value.
#' @noRd
ne_label <- function(object) {
  out <- object$ne_type
  if (is.null(out) || !is.character(out) || length(out) != 1) {
    return("no-effect estimate")
  }
  out
}

#' The ECx a plotting method annotates
#'
#' \code{plot()} and \code{autoplot()} annotate an EC10. For a gaussian response
#' they annotate the control-to-minimum span, \code{type = "range"}, rather than
#' the control-to-zero one: 0 is not a meaningful floor for a response that can
#' go negative, and an equation with no \code{bot} parameter has no finite
#' asymptote to measure towards under that family either, so \code{"relative"}
#' is refused for it.
#'
#' A \code{type} named by the caller always wins.
#'
#' @param object A \code{\link{bnecfit}}.
#' @param family The response family name.
#' @param dots The calling method's \code{list(...)}, forwarded to
#' \code{\link{ecx}}.
#'
#' @return The \code{\link{ecx}} return value.
#' @noRd
plot_ecx <- function(object, family, dots = list()) {
  if (!("type" %in% names(dots)) && identical(family, "gaussian")) {
    dots$type <- "range"
  }
  # Without the scale message: both callers put the estimate on the axis scale
  # with to_axis_scale(), so the message would describe a number the plot does
  # not show.
  without_scale_report(do.call(ecx, c(list(object), dots)))
}
