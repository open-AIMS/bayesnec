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
                         timeout = Inf, model_survival = NULL) {
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
      if (family$family == "binomial" || family$family == "beta_binomial") {
        t_var <- bnec_pop_vars[[which(names(bnec_pop_vars) == "trials_var")]]
        data[, t_var] <- tr
      }
    }
  }
  custom_name <- check_custom_name(family)
  if (family$family == "binomial" || family$family == "beta_binomial") {
    response <- y / tr
  } else {
    response <- y
  }
  brms_bf <- wrangle_model_formula(model, formula, bdat, family,
                                   model_survival = model_survival)
  group_spec <- parse_group_terms(formula, model)
  brm_args <- add_brm_defaults(brm_args, model, family, x, response,
                               skip_check, custom_name,
                               prior_type = prior_type,
                               model_survival = model_survival,
                               disp_spec = parse_disp_term(formula),
                               group_spec = group_spec)
  # A group-level term needs initial values as well as a prior. Stan's own
  # draw for a lower-bounded standard deviation is uniform(-2, 2) on the
  # unconstrained scale and ignores whatever prior was declared, so a prior
  # alone does not stop the mean starting outside a bounded response's support.
  # Appended here rather than inside add_brm_defaults(), alongside the
  # dispersion inits, because the group-level indices are read from
  # make_standata(), which needs the brms formula and the data -- neither of
  # which that function is given. Names the caller already supplied are left
  # alone: a user who wrote their own initial values meant them. See #245.
  if (!is.null(group_spec) && is.list(brm_args$init)) {
    g_init <- group_inits(brms_bf, data, family, brm_args$prior,
                          ogl = group_spec$ogl)
    # The ogl intercept may itself be fixed with a constant() prior, which is
    # the cleanest way to remove its confounding with top and bot. Stan then
    # does not declare b_ogl at all, so an init for it has nothing to
    # initialise. add_brm_defaults() strips inits for constant parameters, but
    # it does so before these are appended, so the same strip is applied here.
    # Hygiene rather than a fix for a binding constraint, for the reasons #244
    # records at the original site.
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
  # Any failure from here on is re-raised carrying the priors and initial values
  # this attempt was given. Both are constructed inside this function rather
  # than supplied by the user, so a caller that catches the error and moves on
  # to the next model -- which is what bnec() does for a model set -- has no
  # other way to recover them. See ?failed_models.
  fit_failed <- function(e) {
    stop(fit_failure_condition(model, conditionMessage(e),
                               all_args$prior, all_args$init))
  }
  if (is.finite(timeout)) {
    # R.utils::withTimeout aborts the brm call once `timeout` seconds elapse
    # (including chains running on parallel worker processes), raising a
    # TimeoutException. This lets a caller's try() move on to the remaining
    # models rather than hanging on a single, highly divergent fit. R.utils is
    # only needed for this optional feature, so it is a Suggests dependency and
    # accessed conditionally.
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
  msg_tag <- family$family
  message(paste0("Response variable modelled as a ", model, " model using a ",
                 msg_tag, " distribution."))
  out <- list(fit = fit, model = model, init = all_args$init,
              bayesnecformula = formula)
  allot_class(out, "prebayesnecfit")
}
