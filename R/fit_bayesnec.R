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
    # The corrections check_data() makes have to reach the data frame brm() is
    # given, and whether they can is a property of one variable at a time. The
    # guard here used to be all-or-nothing -- an inline transformation on any
    # population variable suppressed the write-back for all of them -- so a
    # log() on the predictor discarded a shift applied to the response and
    # brm() then failed naming a condition the package had reported it had
    # repaired (#258). check_data() no longer corrects a transformed variable
    # at all, so anything it did change can be written back.
    data <- write_back_checks(data, bdat, "y_var", y)
    data <- write_back_checks(data, bdat, "x_var", x)
    # There is no trials write-back. check_data() never corrects the trials
    # variable -- for a binomial family it reads the column straight back off
    # the model frame -- so where the aterm names a bare column, writing it
    # back is a no-op. Where the aterm carries arithmetic it is worse than a
    # no-op: clean_aterms() maps `trials(n * 2)` back to `n`, so the doubled
    # values were written into the user's `n` and brm() then evaluated
    # `trials(n * 2)` against them -- a recorded 10 fitted as 40. The
    # write-back this replaces did both.
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
  #
  # The list test is a real constraint, not a formality. brms takes `init`
  # either as one list per chain or as a single keyword, so where the curve
  # search gave up and fell back to "random" there is nothing to append to and
  # the group-level protection is simply not available -- on the datasets where
  # the search struggled, which are the ones most likely to need it. Announced
  # rather than dropped quietly: this is the same outcome group_inits() warns
  # about, reached by a different route.
  if (!is.null(group_spec) && identical(brm_args$init, "random")) {
    message("bayesnec fell back to Stan's default initialisation for the curve",
            " parameters, so no initial values could be set for the",
            " group-level terms either. On a bounded response the fit may",
            " fail to initialise; see ?bayesnecformula for the group-level",
            " terms and ?bnec for supplying `init` directly.")
  }
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

#' Write a correction from \code{\link{check_data}} back into the fitted data
#'
#' Returns \code{data} unchanged where the variable is written into the formula
#' as a transformation: the correction was computed on the transformed scale,
#' which is not the scale of the column \code{brm()} would re-evaluate it from.
#' \code{\link{check_data}} does not correct such a variable, so there is
#' nothing to lose by skipping it.
#'
#' The rows are matched by name rather than assigned wholesale. The model frame
#' drops incomplete cases, so it is shorter than \code{data} wherever any
#' population variable carries an NA, and a wholesale assignment fails there
#' with "replacement has n rows, data has m".
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
