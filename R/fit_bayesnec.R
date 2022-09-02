#' fit_bayesnec
#'
#' Fits a concentration(dose)-response model using brms
#'
#' @inheritParams bnec
#'
#' @param brm_args A named \code{\link[base]{list}} containing further
#' arguments to \code{\link[brms]{brm}}.
#' @param skip_check Should data check via \code{\link{check_data}}
#' be avoided? Only relevant to function \code{\link{amend}}.
#' Defaults to FALSE.
#'
#' @importFrom brms brm
#' @importFrom stats model.frame
#'
#' @seealso \code{\link{bnec}}
#' @return An object of class \code{\link{prebayesnecfit}}.
#'
#' @noRd
fit_bayesnec <- function(formula, data, model = NA, brm_args,
                         skip_check = FALSE) {
  formula <- single_model_formula(formula, model)
  bdat <- model.frame(formula, data = data, run_par_checks = TRUE)
  x <- retrieve_var(bdat, "x_var", error = TRUE)
  y <- retrieve_var(bdat, "y_var", error = TRUE)
  tr <- retrieve_var(bdat, "trials_var")
  family <- brm_args$family
  if (!skip_check) {
    checked_df <- check_data(data = bdat, family = family, model = model)
    x <- checked_df$mod_dat$x
    y <- checked_df$mod_dat$y
    tr <- checked_df$mod_dat$trials
    family <- checked_df$family
    custom_name <- check_custom_name(family)
    brm_args$family <- family
    trans_vars <- find_transformations(bdat)
    # if no transformations are applied via formula (including on trials),
    # use the output of check_data
    if (length(trans_vars) == 0) {
      bnec_pop_vars <- attr(bdat, "bnec_pop")
      y_var <- bnec_pop_vars[[which(names(bnec_pop_vars) == "y_var")]]
      data[, y_var] <- y
      x_var <- bnec_pop_vars[[which(names(bnec_pop_vars) == "x_var")]]
      data[, x_var] <- x
      if (family$family == "binomial" || custom_name == "beta_binomial2") {
        t_var <- bnec_pop_vars[[which(names(bnec_pop_vars) == "trials_var")]]
        data[, t_var] <- tr
      }
    }
  }
  custom_name <- check_custom_name(family)
  if (family$family == "binomial" || custom_name == "beta_binomial2") {
    response <- y / tr
  } else {
    response <- y
  }
  brms_bf <- wrangle_model_formula(model, formula, bdat)
  brm_args <- add_brm_defaults(brm_args, model, family, x, response, skip_check,
                               custom_name)
  all_args <- c(list(formula = brms_bf, data = data), brm_args)
  if (custom_name == "beta_binomial2") {
    all_args <- c(list(stanvars = stanvars), all_args)
  }
  fit <- do.call(brm, all_args)
  pass <- are_chains_correct(fit, all_args$chains)
  if (!pass) {
    stop("Failed to fit model ", model, ".", call. = FALSE)
  }
  msg_tag <- ifelse(family$family == "custom", custom_name, family$family)
  message(paste0("Response variable modelled as a ", model, " model using a ",
                 msg_tag, " distribution."))
  out <- list(fit = fit, model = model, init = all_args$init,
              bayesnecformula = formula)
  allot_class(out, "prebayesnecfit")
}
