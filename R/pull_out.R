#' pull_out
#'
#' Subsets model(s) from an existing object of class \code{\link{bayesmanecfit}}
#'
#' @inheritParams bnec
#' 
#' @importFrom chk chk_character
#'
#' @param manec An object of class \code{\link{bayesmanecfit}} as returned by
#' \code{\link{bnec}}.
#' @param model A \code{\link[base]{character}} string indicating
#' which model or suite of models to pull out.
#' @param x_range,resolution,sig_val As for \code{\link{bnec}}. Each is taken
#' from \code{manec} unless supplied.
#' @param ... Additional arguments to \code{\link{expand_nec}} or
#' \code{\link{expand_manec}}.
#'
#' @details Nothing is refitted, so the records of the set are kept. Each
#' equation keeps its \code{\link{bnec_version}} record, the
#' \code{\link{failed_models}} record is kept unchanged, and
#' \code{\link{bnec_record}} lists each equation not kept as excluded, with the
#' reason "not kept by pull_out()".
#'
#' The estimates of the models kept are computed again with the
#' \code{x_range}, \code{resolution}, \code{sig_val} and
#' \code{loo_controls} "fitting" arguments \code{manec} was fitted with, unless
#' they are supplied. The \code{\link{bnec}} defaults are used where
#' \code{manec} records none, as an object fitted by an earlier version of
#' \pkg{bayesnec} does not.
#'
#' @return If \code{model} is a string representing a single model, an object
#' of class \code{\link{bayesnecfit}}; If \code{model} is instead a string
#' depicting a suite of models, and object of class \code{\link{bayesmanecfit}}.
#'
#' @seealso \code{\link{bnec}}, \code{\link{models}},
#' \code{\link{pull_best}}.
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' data(manec_example)
#' nec4param <- pull_out(manec_example, model = "nec4param")
#' # use "ecx" to get all ECx-containing models
#' # (only one ["ecx4param"] in this minimal example)
#' ecx_models <- pull_out(manec_example, model = "ecx")
#' }
#'
#' @export
pull_out <- function(manec, model, loo_controls, x_range = NULL,
                      resolution = NULL, sig_val = NULL, ...) {
  # Not an S3 generic, so a hurdle fit has to be handled here; registering a
  # pull_out.bayesnechurdlefit method would be inert. Reduces both components
  # to the requested model so the pair stays in step.
  if (is_bayesnechurdlefit(manec)) {
    args <- list(model = model, x_range = x_range, resolution = resolution,
                  sig_val = sig_val, ...)
    if (!missing(loo_controls)) args$loo_controls <- loo_controls
    manec$growth <- do.call(pull_out, c(list(manec$growth), args))
    manec$survival <- do.call(pull_out, c(list(manec$survival), args))
    return(manec)
  }
  if (length(model) > 1) {
    stop("Argument model can only take one value. See ?pull_out and ?models.")
  }
  old_method <- fit_weights_method(manec)
  supplied_loo <- NULL
  fits <- recover_prebayesnecfit(manec)
  fam_tag <- fits[[1]]$fit$family$family
  if (!missing(loo_controls)) {
    if (length(validate_loo_controls(loo_controls, fam_tag)$weights) > 0) {
      message("You have specified a list of arguments in loo_control$weights; ",
              "this is ignored in pull_out. Use function ?amend first if your",
              "intention is to modify the model averaging specs.")
    }
    # The set keeps its own weighting, so pass on the fitting arguments only.
    supplied_loo <- loo_controls[intersect(names(loo_controls), "fitting")]
  }
  dots <- list(...)
  expand_args <- recorded_expand_args(
    list(manec), x_range = x_range,
    resolution = resolution, sig_val = sig_val,
    loo_controls = supplied_loo, method = old_method, fam_tag = fam_tag
  )
  expand_call <- c(expand_args[c("x_range", "resolution", "sig_val",
                                 "loo_controls")], dots)
  existing <- names(fits)
  msets <- names(mod_groups)
  if (any(model %in% msets)) {
    group_mods <- intersect(model, msets)
    model <- union(model, unname(unlist(mod_groups[group_mods])))
    model <- setdiff(model, msets)
  }
  to_go <- intersect(model, existing)
  if (length(to_go) == 0) {
    message("Model(s) ", paste0(model, collapse = ", "),
            " non-existent in current set of models: ",
            paste0(existing, collapse = ", "), ".\n",
            "They may have been removed due to incompatibility with the",
            " modelled response distribution. If needed, add desired model(s)",
            " via function amend (see ?amend).\n Returning original object.")
    return(manec)
  } else if (!all(model %in% existing)) {
    non_existing <- setdiff(model, existing)
    message("Model(s) ", paste0(non_existing, collapse = ", "),
            " non-existent in current set of models: ",
            paste0(existing, collapse = ", "), ".\n",
            "They may have been removed due to incompatibility with the",
            " modelled response distribution. If needed, add desired model(s)",
            " via function amend (see ?amend).")
  }
  if (all(existing %in% to_go) && length(dots) == 0 &&
        identical(expand_args, recorded_expand_args(
          list(manec), method = old_method, fam_tag = fam_tag
        ))) {
    message("Current model(s) are 100% contained ",
            "within target model(s) to pull out.\n",
            "Returning original object.")
    return(manec)
  } else if (all(!(to_go %in% existing))) {
    message("Target model(s) are 100% contained ",
            "within target model(s) to pull out.\n",
            "Returning original object.")
    return(manec)
  }
  formulas <- lapply(fits[to_go], extract_formula)
  mod_fits <- do.call(expand_manec, c(list(fits[to_go],
                                           formula = formulas),
                                      expand_call)) |>
    suppressMessages() |>
    suppressWarnings()
  message("Pulling out model(s): ", paste0(to_go, collapse = ", "))
  if (length(mod_fits) > 1) {
    out <- allot_class(mod_fits, c("bayesmanecfit", "bnecfit"))
  } else {
    mod_fits <- do.call(expand_nec, c(list(
      mod_fits[[1]], model = to_go, formula = mod_fits[[1]]$bayesnecformula
    ), expand_call)) |>
    suppressMessages() |>
    suppressWarnings()
    out <- allot_class(mod_fits, c("bayesnecfit", "bnecfit"))
  }
  # Carry the record of failed models across, unlike amend(): pull_out()
  # refits nothing, so the record is still accurate. Read the element, not
  # failed_models(), so that an object fitted before the record existed is
  # left untouched.
  out <- attach_failed_models(out, manec[["failed_models"]])
  out <- copy_fit_records(out, list(manec), "not kept by pull_out()",
                          expand_args)
  carry_retained_data(out, manec)
}
