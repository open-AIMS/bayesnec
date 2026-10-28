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
#' @param timeout A positive \code{\link[base]{numeric}} giving the maximum
#' number of seconds allowed for the underlying \code{\link[brms]{brm}} call.
#' The default \code{Inf} imposes no limit. See \code{\link{bnec}}.
#'
#' @importFrom brms brm
#' @importFrom stats model.frame
#'
#' @seealso \code{\link{bnec}}
#' @return An object of class \code{\link{prebayesnecfit}}.
#'
#' @noRd
fit_bayesnec <- function(formula, data, model = NA, brm_args,
                         skip_check = FALSE, prior_type = "uninformative",
                         asymptote_observed = TRUE,
                         timeout = Inf, model_survival = NULL,
                         predictor_scale = "auto", level_spec = NULL) {
  formula <- single_model_formula(formula, model)
  # A backstop: bnec(), bnec_group() and amend() exclude such an equation
  # before their loops, but bnec_joint() fits through here directly.
  group_excluded <- group_term_exclusions(formula, model)
  if (nrow(group_excluded) > 0) {
    stop(group_terms_refusal(group_excluded), call. = FALSE)
  }
  bdat <- model.frame(formula, data = data, run_par_checks = TRUE)
  x <- retrieve_var(bdat, "x_var", error = TRUE)
  y <- retrieve_var(bdat, "y_var", error = TRUE)
  tr <- retrieve_var(bdat, "trials_var")
  denominator <- retrieve_var(bdat, "rate_var")
  family <- brm_args$family
  if (!skip_check) {
    checked_df <- check_data(data = bdat, family = family, model = model)
    x <- checked_df$mod_dat$x
    y <- checked_df$mod_dat$y
    tr <- checked_df$mod_dat$trials
    # Take the denominator from the checked frame, so that it stays aligned
    # with the response check_data() may have substituted into. NULL without a
    # rate() term.
    denominator <- checked_df$mod_dat$denom
    family <- checked_df$family
    custom_name <- check_custom_name(family)
    brm_args$family <- family
    # Write check_data()'s corrections back to the data brm() is given, one
    # variable at a time. check_data() does not correct a transformed
    # variable, so anything it changed can be written back.
    data <- write_back_checks(data, bdat, "y_var", y)
    # Write back the response only. check_data() corrects neither the predictor
    # nor the trials, and writing back `trials(n * 2)` would put the doubled
    # values into the user's `n`.
  }
  custom_name <- check_custom_name(family)
  if (family$family == "binomial" || family$family == "beta_binomial") {
    response <- y / tr
  } else {
    response <- y
  }
  # Divide out a rate() denominator, as trials is above. Under the identity
  # link the mean is the rate, so top, bot and nec are per unit exposure.
  # `response` reaches both define_prior() and the initial-value search.
  # amend() divides in the same way.
  if (!is.null(denominator)) {
    response <- response / denominator
  }
  brms_bf <- wrangle_model_formula(model, formula, bdat, family,
                                   model_survival = model_survival,
                                   level_spec = level_spec)
  group_spec <- parse_group_terms(formula, model)
  brm_args <- add_brm_defaults(brm_args, model, family, x, response,
                               skip_check, custom_name,
                               prior_type = prior_type,
                               asymptote_observed = asymptote_observed,
                               predictor_scale = predictor_scale,
                               model_survival = model_survival,
                               disp_spec = parse_disp_term(formula),
                               group_spec = group_spec,
                               level_spec = level_spec,
                               rate_response = !is.null(denominator),
                               truncated_response = isTRUE(attr(
                                 formula, "bayesnec_internal_truncation")))
  # A group-level term needs initial values as well as a prior: Stan's own
  # draw for a lower-bounded standard deviation ignores the prior, so the mean
  # can start outside a bounded response's support. Append them here, where
  # the brms formula and data make_standata() needs are available, and leave
  # names the caller supplied alone. Where the curve search fell back to
  # "random", `init` is a keyword with nothing to append to, so say so.
  if (!is.null(group_spec) && identical(brm_args$init, "random")) {
    message("bayesnec fell back to Stan's default initialisation for the curve",
            " parameters, so no initial values could be set for the",
            " group-level terms either. On a bounded response the fit may",
            " fail to initialise; see ?bayesnecformula for the group-level",
            " terms and ?bnec for supplying `init` directly.")
  }
  if (!is.null(group_spec) && is.list(brm_args$init)) {
    g_init <- group_inits(brms_bf, data, family, brm_args$prior,
                          group_spec = group_spec)
    # A deviation intercept fixed by a constant() prior is not declared by
    # Stan, so drop its init. add_brm_defaults() strips such inits before these
    # are appended, so strip them again here.
    const_prior <- as.data.frame(brm_args$prior)
    b_const <- is_constant_prior(const_prior$prior) &
      const_prior$class == "b" & nzchar(const_prior$nlpar)
    g_init <- g_init[!names(g_init) %in%
                       paste0("b_", const_prior$nlpar[b_const])]
    if (length(g_init) > 0) {
      brm_args$init <- lapply(brm_args$init, function(chain) {
        c(chain, g_init[setdiff(names(g_init), names(chain))])
      })
    }
  }
  all_args <- c(list(formula = brms_bf, data = quote(data)), brm_args)
  # Re-raise any failure with the priors and initial values this attempt was
  # given. They are built here, so a caller that catches the error, as bnec()
  # does for a model set, cannot otherwise recover them. See ?failed_models.
  fit_failed <- function(e) {
    stop(fit_failure_condition(model, conditionMessage(e),
                               all_args$prior, all_args$init))
  }
  if (is.finite(timeout)) {
    # R.utils::withTimeout() aborts brm() after `timeout` seconds, including
    # chains on worker processes, so that a caller's try() can move on.
    # R.utils is in Suggests, so check for it.
    if (!requireNamespace("R.utils", quietly = TRUE)) {
      stop("Package \"R.utils\" is required to use the `timeout` argument. ",
           "Please install it.", call. = FALSE)
    }
    fit <- tryCatch(
      R.utils::withTimeout(
        do.call(brm, all_args), timeout = timeout, onTimeout = "error"
      ),
      error = fit_failed
    )
  } else {
    fit <- tryCatch(do.call(brm, all_args), error = fit_failed)
  }
  pass <- are_chains_correct(fit, all_args$chains)
  if (!pass) {
    stop(fit_failure_condition(model,
                               paste0("Failed to fit model ", model, "."),
                               all_args$prior, all_args$init))
  }
  # amend() passes the family stored in the fit, which records Gamma as gamma.
  msg_tag <- canonical_fam_tag(family)
  message(paste0("Response variable modelled as a ", model, " model using a ",
                 msg_tag, " distribution."))
  out <- list(fit = fit, model = model, init = all_args$init,
              bayesnecformula = formula)
  allot_class(out, "prebayesnecfit")
}

#' Write a correction from \code{\link{check_data}} back into the fitted data
#'
#' Returns \code{data} unchanged where the variable is transformed in the
#' formula: \code{brm()} would re-evaluate the transformation from the column,
#' and \code{\link{check_data}} does not correct such a variable anyway.
#'
#' Rows are matched by name, because the model frame drops incomplete cases
#' and can be shorter than \code{data}.
#'
#' @noRd
write_back_checks <- function(data, bdat, var, values) {
  if (pop_var_is_transformed(bdat, var)) {
    return(data)
  }
  bnec_pop_vars <- attr(bdat, "bnec_pop")
  v_pos <- which(names(bnec_pop_vars) == var)
  if (length(v_pos) != 1) {
    return(data)
  }
  rows <- match(rownames(bdat), rownames(data))
  data[rows, bnec_pop_vars[[v_pos]]] <- values
  data
}
