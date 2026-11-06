#' compare_posterior
#'
#' Compares posterior estimates from separate fits or levels of one joint refit.
#'
#' @param x A named \code{\link[base]{list}} of \code{\link{bayesnecfit}}
#' or \code{\link{bayesmanecfit}} objects, a \code{\link{bayesnecgroupfit}},
#' or a \code{\link{bayesnecjointfit}}.
#' @param comparison The posterior predictions to compare, takes values of
#' "nec", "n(s)ec", "nsec", "ecx" or "fitted".
#' @param make_newdata Only used if \code{comparison = "fitted"}. Should the
#' user allow the package to create \code{newdata} for predictions?
#' If so, arguments \code{resolution} and \code{x_range} will be used. Defaults
#' to TRUE. See details.
#' @param ... Further arguments that control posterior predictions via
#' \code{\link[brms]{posterior_epred}}.
#' @param seed A \code{\link[base]{numeric}} vector of length 1. Passed to
#' \code{\link[base]{set.seed}} before separate posteriors are paired or
#' joint fitted values are predicted, so that two calls on the same fits
#' return the same answer. The caller's saved RNG state is restored afterwards.
#' A \code{\link[base]{set.seed}} in the session therefore does not change the
#' result. A different value changes the random pairing of separate fits or
#' stochastic fitted predictions. Joint estimate contrasts retain their draw
#' pairing for every seed. \code{NULL} is refused, because
#' \code{set.seed(NULL)} re-initialises the stream from the clock.
#' Reproducibility assumes the same RNG kind; the sampling algorithm is fixed
#' to \code{sample.kind = "Rejection"}. The saved RNG state is restored,
#' but the cached normal variate used by \code{normal.kind = "Box-Muller"}
#' is not part of that state. With Box-Muller, the next normal draw can change
#' after an odd number of preceding normal draws. Use R's default
#' \code{normal.kind = "Inversion"} to preserve subsequent normal draws.
#' 
#' @inheritParams ecx
#' @inheritParams nsec
#'
#' @details \code{type} is passed to \code{\link{ecx}} and takes the same four
#' values, all measured from the control --- the predicted mean at the lowest
#' concentration in the supplied predictor, per posterior draw. "absolute" (the
#' default) measures to 0, "relative" to the equation's theoretical asymptote,
#' "range" to the lowest response the curve predicts, and "direct" takes a
#' response value rather than a percentage. See \code{\link{ecx}} for the full
#' definitions and for what changed at 2.2.0.
#'
#' The argument \code{make_newdata} is only used if
#' \code{comparison = "fitted"}. It is relevant to those who want the package
#' to create a data.frame from which to make predictions. This is done via
#' \code{\link{bnec_newdata}} and uses arguments \code{resolution} and
#' \code{x_range}. If \code{make_newdata = FALSE} and no additional
#' \code{newdata} argument is provided (via \code{...}), then the predictions
#' are made for the raw data. Else, to generate predictions for a specific
#' user-specific data.frame, set \code{make_newdata = FALSE} and provide
#' an additional data.frame via the \code{newdata} argument. For guidance
#' on how to structure \code{newdata}, see for example
#' \code{\link[brms]{posterior_epred}}.
#'
#' For separate fits, every comparison other than "fitted" is made by
#' \code{\link{compare_estimates}}, which describes the elements returned.
#' Joint-level estimates have the same output structure. A
#' posterior draw that lies beyond the prediction range is not deleted from
#' the comparison. Where the censoring leaves the sign of some paired
#' differences unknown, \code{prob_diff} reports the probability as a lower
#' and an upper value, \code{prob_lower} and \code{prob_upper}, with the
#' censored fraction of each posterior, and \code{prob} is \code{NA}. See the
#' \emph{Censored estimates} section of \code{\link{compare_estimates}}.
#'
#' @inheritSection compare_estimates What the comparison estimates
#'
#' @seealso \code{\link{bnec}} \code{\link{ecx}} \code{\link{nsec}}
#' \code{\link{nec}} \code{\link{bnec_newdata}}
#' \code{\link{compare_estimates}}
#'
#' @return A named \code{\link[base]{list}} containing posterior differences,
#' probabilities and the draws for each fit or level. See Details.
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' data(manec_example)
#' nec4param <- pull_out(manec_example, model = "nec4param")
#' ecx4param <- pull_out(manec_example, model = "ecx4param")
#' compare_posterior(list("nec4param" = nec4param, "ecx4param" = ecx4param),
#'                   ecx_val = 50)
#' }
#'
#' @export
compare_posterior <- function(x, ...) {
  UseMethod("compare_posterior")
}

#' @rdname compare_posterior
#' @order 2
#'
#' @method compare_posterior default
#'
#' @inherit compare_posterior description return examples
#'
#' @export
compare_posterior.default <- function(x, comparison = "n(s)ec", ecx_val = 10,
                                      type = "absolute",
                                      sig_val = 0.01, resolution,
                                      x_range = NA, make_newdata = TRUE,
                                      seed = 10, ...) {
  if (!is.list(x) | is.null(names(x))) {
    stop("Argument x must be a named list.")
  }
  if (!is.character(comparison)) {
    stop("Argument comparison must be a character vector.")
  }
  if (comparison != "fitted") {
    if (missing(resolution)) {
      resolution <- 500
    }
    out <- compare_estimates(x = x, comparison = comparison, ecx_val = ecx_val,
                             type = type,
                             sig_val = sig_val, resolution = resolution,
                             x_range = x_range, seed = seed)
  } else {
    if (missing(resolution)) {
      resolution <- 50
    }
    out <- compare_fitted(x = x, resolution = resolution, x_range = x_range,
                          make_newdata = make_newdata, seed = seed, ...)
  }
  out
}

#' @rdname compare_posterior
#' @order 3
#'
#' @method compare_posterior bayesnecgroupfit
#'
#' @inherit compare_posterior description return examples
#'
#' @details For a \code{\link{bayesnecgroupfit}} the comparison is across the
#' levels of the grouping factor: each level was fitted independently, so the
#' per-level fits are already the named list this function takes, and the method
#' is dispatch rather than new machinery.
#'
#' This is the natural companion to \code{\link{crossed_group_weights}}. That
#' answers which \emph{equation} best describes each level; this answers whether
#' the levels differ in the \emph{quantity being reported} --- the \emph{NEC},
#' an ECx, or the fitted curve. A factor covariate analysis usually wants both,
#' and they can disagree: two levels can favour the same model form while
#' differing in where the threshold falls, and vice versa.
#'
#' The levels are fitted to separate data and share no parameters, so the
#' posteriors are independent and the pairwise probabilities are read directly,
#' with no multiple-comparison adjustment implied.
#'
#' @export
compare_posterior.bayesnecgroupfit <- function(x, ...) {
  compare_posterior(x$fits, ...)
}

#' @rdname compare_posterior
#' @order 4
#'
#' @method compare_posterior bayesnecjointfit
#' @inherit compare_posterior description return examples
#'
#' @details For a \code{\link{bayesnecjointfit}}, differences are taken at
#' matching posterior draw positions. Each level is estimated from the same
#' fitted posterior. Where an observed group term spans concentrations, each
#' draw is assigned one observed level in proportion to its observation count;
#' the draw remains at its original position in every joint-level estimate.
#' The \code{seed} argument controls stochastic fitted predictions but does
#' not permute joint posterior draws. Observed-level mixture assignments are
#' reproducible from their level labels and observation counts.
#'
#' @export
compare_posterior.bayesnecjointfit <- function(x, comparison = "n(s)ec",
                                               ecx_val = 10,
                                               type = "absolute",
                                               sig_val = 0.01, resolution,
                                               x_range = NA,
                                               make_newdata = TRUE,
                                               seed = 10, ...) {
  choices <- c("nec", "n(s)ec", "nsec", "ecx", "fitted")
  if (!is.character(comparison) || length(comparison) != 1L ||
      is.na(comparison) || !comparison %in% choices) {
    stop("comparison must be one of ", paste(choices, collapse = ", "), ".",
         call. = FALSE)
  }
  if (length(x$levels) < 2L) {
    stop("A joint comparison needs at least two levels.", call. = FALSE)
  }
  chk_number(seed)
  if (missing(resolution)) {
    resolution <- if (identical(comparison, "fitted")) 50 else 500
  }
  if (identical(comparison, "fitted")) {
    return(compare_joint_fitted(x, resolution, x_range, make_newdata,
                                seed, ...))
  }
  chk_numeric(ecx_val)
  chk_numeric(sig_val)
  chk_numeric(resolution)
  type <- validate_ecx_type(type, match.call())
  warned <- options(bayesnec.relative_warned = TRUE)
  on.exit(options(warned), add = TRUE)
  posterior_list <- with_preserved_rng_state({
    set.seed(seed, sample.kind = "Rejection")
    switch(comparison,
      "nec" = nec(x, posterior = TRUE),
      "n(s)ec" = nec(x, no_effect = TRUE, posterior = TRUE,
                     sig_val = sig_val, resolution = resolution,
                     x_range = x_range),
      "nsec" = nsec(x, sig_val = sig_val, resolution = resolution,
                    x_range = x_range, posterior = TRUE),
      "ecx" = ecx(x, ecx_val = ecx_val, type = type,
                  resolution = resolution, x_range = x_range,
                  posterior = TRUE)
    )
  })
  if (length(unique(lengths(posterior_list))) != 1L) {
    stop("Joint levels returned different numbers of posterior draws.",
         call. = FALSE)
  }
  compare_estimate_draws(posterior_list, posterior_list)
}

#' Fitted-curve contrasts from one joint posterior
#' @param x A joint refit.
#' @param resolution,x_range,make_newdata,seed,... See
#' \code{compare_posterior()}.
#' @return The fitted comparison output of \code{compare_posterior()}.
#' @noRd
compare_joint_fitted <- function(x, resolution, x_range, make_newdata,
                                 seed, ...) {
  if (is.na(x_range[1])) {
    values <- unlist(lapply(x$levels, function(level) {
      joint_level_x(x, level)
    }))
    if (length(values)) x_range <- range(values)
  }
  first <- joint_level_fit(x, x$levels[1])
  grid <- newdata_eval_fitted(
    first, resolution = resolution, x_range = x_range,
    make_newdata = make_newdata, fct_eval = "compare_posterior", ...
  )
  base <- if (is.null(grid$newdata)) x$fit$data else grid$newdata
  n_x <- nrow(base)
  spec <- joint_level_spec(x)
  newdata <- add_grid_levels(base, spec, n_x)
  dot_list <- list(...)
  dot_list$object <- first
  dot_list$newdata <- newdata
  dot_list$re_formula <- grid$re_formula
  pred <- with_preserved_rng_state({
    set.seed(seed, sample.kind = "Rejection")
    do.call(posterior_epred, dot_list)
  })
  posterior_list <- lapply(seq_along(x$levels), function(i) {
    cols <- seq.int((i - 1L) * n_x + 1L, length.out = n_x)
    pred[, cols, drop = FALSE]
  })
  names(posterior_list) <- x$levels
  compare_fitted_draws(posterior_list, posterior_list, grid$x_vec)
}
