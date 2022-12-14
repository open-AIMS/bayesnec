#' Generates a summary for objects fitted by \code{\link{bnec}}
#'
#' Generates a summary for objects fitted by \code{\link{bnec}}.
#' \code{object} should be of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}.
#'
#' @name summary
#' @order 1
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}.
#' @param ... Unused.
#'
#' @return A summary of the fitted model. In the case of a
#' \code{\link{bayesnecfit}} object, the summary contains most of the original
#' contents of a \code{\link[brms]{brmsfit}} object with the addition of
#' an R2. In the case of a \code{\link{bayesmanecfit}} object, summary
#' displays the family distribution information, model weights and averaging
#' method, the estimated model-averaged NEC, and R2 estimates for each
#' individual model. Warning messages are also printed to screen in case
#' model fits are not satisfactory with regards to their Rhats.
#'
#' @examples
#' \donttest{
#' library(bayesnec)
#' summary(manec_example)
#' nec4param <- pull_out(manec_example, "nec4param")
#' summary(nec4param)
#' }
NULL

#' @rdname summary
#' @order 2
#'
#' @param ecx Should summary ECx values be calculated? Defaults to FALSE.
#' @param ecx_vals ECx targets (between 1 and 99). Only relevant if ecx = TRUE.
#' If no value is specified by the user, returns calculations for EC10, EC50,
#' and EC90.
#'
#' @method summary bayesnecfit
#'
#' @inherit summary description return examples
#'
#' @importFrom brms bayes_R2
#' @importFrom chk chk_numeric chk_lgl
#'
#' @export
summary.bayesnecfit <- function(object, ..., ecx = FALSE,
                                ecx_vals = c(10, 50, 90)) {
  chk_lgl(ecx)
  chk_numeric(ecx_vals)    
  x <- object
  ecs <- NULL
  if (ecx) {
    message("ECX calculation takes a few seconds per model, calculating...\n")
    ecs <- list()
    for (i in seq_along(ecx_vals)) {
      ecs[[i]] <- ecx(x, ecx_val = ecx_vals[i])
    }
    names(ecs) <- paste0("ECx (", ecx_vals, "%) estimate:")
  }
  out <- list(
    brmssummary = cleaned_brms_summary(x$fit),
    model = x$model,
    is_ecx = x$model %in% mod_groups$ecx,
    ecs = ecs,
    bayesr2 = bayes_R2(x$fit)
  )
  allot_class(out, "necsummary")
}

#' @rdname summary
#' @order 3
#'
#' @method summary bayesmanecfit
#'
#' @inherit summary description return examples
#'
#' @importFrom purrr map
#' @importFrom brms bayes_R2
#' @importFrom chk chk_lgl chk_numeric
#'
#' @export
summary.bayesmanecfit <- function(object, ..., ecx = FALSE,
                                  ecx_vals = c(10, 50, 90)) {
  chk_lgl(ecx)
  chk_numeric(ecx_vals)
  x <- object
  ecs <- NULL
  if (ecx) {
    message("ECX calculation takes a few seconds per model, calculating...\n")
    ecs <- list()
    for (i in seq_along(ecx_vals)) {
      ecs[[i]] <- ecx(x, ecx_val = ecx_vals[i])
    }
    names(ecs) <- paste0("ECx (", ecx_vals, "%) estimate:")
  }
  ecx_mods <- NULL
  if (any(x$success_models %in% mod_groups$ecx)) {
    ecx_mods <- x$success_models[x$success_models %in% mod_groups$ecx]
  }
  out <- list(
    models = x$success_models,
    family = capture_family(x),
    sample_size = x$sample_size,
    mod_weights = clean_mod_weights(x),
    mod_weights_method = class(x$mod_stats$wi),
    ecx_mods = ecx_mods,
    nec_vals = clean_nec_vals(x),
    ecs = ecs,
    bayesr2 = x$mod_fits |>
      lapply(function(y)bayes_R2(y$fit)) |>
      do.call(what = "rbind.data.frame"),
    rhat_issues = map(x$mod_fits, "fit") |>
      map(has_r_hat_warnings)
  )
  allot_class(out, "manecsummary")
}
