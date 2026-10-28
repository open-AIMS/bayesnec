#' Fit a factorised hurdle (or zero-inflated) concentration-response model
#'
#' Fits a two-part concentration-response model as a pair of ordinary
#' \code{\link{bnec}} fits: one for the response of the units that gave a
#' non-zero value, and a \code{\link[brms]{bernoulli}} model for the probability
#' of being non-zero. The two are returned together so that the combined
#' endpoint -- the expected response per unit *exposed* -- can be derived from
#' them.
#'
#' \bold{This covers the zero-inflated case too.} \pkg{brms} names the
#' equivalent joint families \code{hurdle_gamma} and
#' \code{zero_inflated_beta}, but the distinction is nominal: zero-inflation
#' differs from a hurdle only when the base distribution can itself produce
#' zeros, which neither the Gamma nor the Beta can. The Stan density
#' \pkg{brms} generates for \code{zero_inflated_beta} is the hurdle form, with
#' no mixture at zero. One function therefore serves both, and the appropriate
#' family is chosen from the data -- \code{Gamma} for a positive continuous
#' response, \code{Beta} for a proportion on (0, 1).
#'
#' Throughout the documentation the two parts are called "growth" and
#' "survival", after the case they were written for -- individuals that die
#' contribute a zero and the survivors contribute a measurement. Nothing in the
#' implementation is specific to that reading: any process producing exact
#' zeros alongside a positive response fits the same structure. Algal growth
#' rate expressed as a proportion of a ceiling, with replicates that failed
#' entirely, is the same model.
#'
#' @param formula Either a \code{\link[base]{character}} string defining an
#' R formula or an actual \code{\link[stats]{formula}} object. See
#' \code{\link{bayesnecformula}}. The response must be untransformed, and zero
#' values in it are taken to mean the individual did not survive. A
#' \code{cens()} aterm is allowed for continuous growth families. It is refused
#' for count growth families because \pkg{brms} does not combine left censoring
#' with zero truncation correctly. Other aterms are refused; see Details. A
#' \code{disp()} term applies to the growth component only; see Details.
#' @param data A \code{\link[base]{data.frame}} containing the data to use with
#' the \code{formula}. Every unit that entered the experiment must be present,
#' with \code{0} recorded for those that gave no response. Rows omitted rather
#' than zeroed cannot be distinguished from ones never run, and would be read
#' as a smaller experiment rather than as zeros.
#' @param model_survival An optional \code{\link[base]{character}} vector naming
#' the model or model group to use for the survival component. Defaults to
#' whatever \code{crf} specifies in \code{formula}, i.e. the same set as the
#' response component.
#' @param family_growth A \code{\link[stats]{family}} function for the response
#' of the non-zero subset. Defaults to \code{NULL}, in which case it is chosen
#' from that subset the same way \code{\link{bnec}} would: \code{Gamma} for a
#' positive continuous response, \code{Beta} for one bounded on (0, 1), and
#' \code{poisson} for counts. A
#' two-block family (\code{hurdle_gamma}, \code{zero_inflated_beta}) is refused,
#' because \code{bnec_hurdle} is itself the two-part model, and so are the
#' zero-inflated count families, which are mixtures rather than hurdles -- see
#' \code{\link{bnec}}. \code{binomial} and \code{beta_binomial} are refused as
#' well: each needs the number of trials in a \code{trials()} term, and
#' \code{bnec_hurdle} takes none. For \code{poisson} and \code{negbinomial}, the positive
#' subset is fitted with the corresponding zero-truncated likelihood. The
#' truncation is added internally because every row in that subset is known to
#' be positive; it is not a user-selectable response transformation. This path
#' requires \pkg{brms} 2.23.2 or later, where the fitted model and log
#' likelihood use the same inclusive lower bound. \pkg{bayesnec} computes the
#' exact conditional-positive expected response from the fitted count
#' parameters rather than using \pkg{brms}'s finite-grid approximation.
#' @param predictor_scale The predictor-scale declaration passed to both
#' \code{\link{bnec}} fits. It is checked against the complete predictor before
#' either component is fitted. See \code{\link{bnec}}.
#' @param ... Further arguments passed to both \code{\link{bnec}} calls.
#'
#' @details
#'
#' \bold{Why two fits rather than one}
#'
#' The hurdle log-likelihood separates exactly into a Bernoulli term over all
#' individuals and a zero-bounded term over the survivors only:
#'
#' \preformatted{
#' log p(y_i) = log Bernoulli(alive_i | 1 - hu_i)
#'            + 1[y_i > 0] * log Gamma(y_i | mu_i, shape)
#' }
#'
#' The two blocks share no parameters, so with independent priors the posterior
#' factorises and fitting them separately gives the same inference as fitting
#' them jointly. Doing so also makes the full crossed model comparison
#' tractable: \code{elpd(a, b) = elpd_growth(a) + elpd_survival(b)}, so all
#' combinations of the two model sets can be compared from the two fits alone
#' rather than by fitting every pair.
#'
#' The factorisation does \emph{not} hold if the two components are coupled, for
#' example by a shared group-level effect. Group-level terms supplied here apply
#' within each component independently.
#'
#' \bold{Coding of deaths}
#'
#' A \code{0} response means the individual did not survive; it is a label, not
#' a measurement. Where mortality is instead recorded by omitting rows, those
#' rows must be reinstated as zeros before calling this function.
#'
#' \bold{Censoring, and which aterms are allowed}
#'
#' \code{cens()} is the one aterm accepted on a continuous response.
#' \code{\link{bnec}}
#' itself carries three -- \code{trials()}, \code{weights()} and
#' \code{cens()} -- and of those \code{cens()} is the only one whose meaning
#' stays unambiguous once the response is split across two models. It is also
#' the one with a use here that nothing else covers. A growth endpoint can be
#' both
#' zero-bounded with structural zeros and left-censored at the recording
#' resolution, and only a two-part model with a censored response component can
#' tell the two apart: a death is a structural zero belonging to the Bernoulli
#' component, while a survivor measured below the limit is a real observation of
#' the growth component whose value is known only to lie at or below a bound.
#'
#' The declaration is routed accordingly. It is passed through to the growth
#' component, where the censoring indicator travels as an ordinary data column
#' and is subset along with everything else, and it is dropped from the survival
#' component, whose alive/dead response is observed exactly. A row that is both
#' zero and censored is refused rather than assigned to one of them.
#'
#' Under a Gamma growth component the censoring bound cannot be \code{0} --
#' \code{\link{bnec}} rejects that, correctly, because the censored likelihood
#' contribution there is \code{F(0) = 0}. The bound has to be the resolution
#' limit, so the encoding is "at most the smallest resolvable value" rather than
#' "at most zero".
#'
#' The other two are refused by name, with the reason. \code{trials()} has no
#' meaning for either component, and \code{weights()} is a modelling decision --
#' whether a weight applies to the growth component, the survival component or
#' both -- that this function will not make on the user's behalf. Aterms beyond
#' those three are refused here as well, though they would not reach \pkg{brms}
#' in any case: \code{\link{model.frame}} drops them for an ordinary
#' \code{\link{bnec}} fit too. Making the two \code{\link{bnec}} calls directly
#' remains available for anything outside this set. Count growth is an
#' exception: combining its required \code{trunc(lb = 1)} with left censoring
#' makes \pkg{brms} subtract the truncation normaliser from an unconditioned
#' cumulative probability, which can yield a likelihood contribution greater
#' than one. \code{bnec_hurdle} therefore refuses \code{cens()} with a count
#' growth family before fitting.
#'
#' \bold{Non-constant dispersion}
#'
#' A \code{disp()} term in \code{formula} models the dispersion parameter of
#' the growth component, as it does for \code{\link{bnec}}; see
#' \code{\link{bayesnecformula}}. It is removed from the survival component,
#' whose \code{\link[brms]{bernoulli}} likelihood has no dispersion parameter.
#' The parameters of the dispersion sub-model belong to the growth component
#' alone, so the factorisation described above still holds. The term is checked
#' against \code{family_growth} before either component is fitted, so a
#' specification that family cannot take is refused before anything is
#' sampled: any \code{disp()} term on a \code{poisson} growth component, for
#' example, or \code{disp("twosided")} on a \code{Gamma} one. A variance
#' function such as \code{disp("power")} is written in the mean of the growth
#' component as \pkg{brms} parameterises it. For a count growth family that is
#' the mean of the untruncated distribution, not the mean of the positive
#' counts that \pkg{bayesnec} reports.
#'
#' A \code{disp(~z)} term naming a column other than the predictor is refused
#' for a \code{negbinomial} growth component, before either component is
#' fitted. The curve of that component is the mean of the positive counts,
#' \code{mu / (1 - (shape / (mu + shape))^shape)}, which depends on the shape
#' parameter, so with the shape modelled on \code{z} the curve and every
#' estimate read from it would depend on the value \code{z} is given when
#' predicting. On a \code{Gamma} or \code{Beta} growth component the term is
#' fitted, and \code{z} is treated on a prediction grid as
#' \code{\link{bayesnecformula}} describes, with the values a predicted
#' reading takes drawn from the survivors the growth component is fitted to.
#'
#' @return An object of class \code{\link{bayesnechurdlefit}}.
#'
#' @seealso \code{\link{bnec}} for the equivalent joint fit via
#' \code{family = "hurdle_gamma"} or \code{family = "zero_inflated_beta"},
#' \code{\link{bayesnechurdlefit}}, \code{\link{crossed_weights}}
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' data(nec_data)
#' # code a few individuals as having died
#' nec_data$y[nec_data$x > 2.5] <- 0
#' fit <- bnec_hurdle(y ~ crf(x, "nec3param"), data = nec_data)
#' nec(fit)
#' ecx(fit, ecx_val = 10)
#' }
#'
#' @export
bnec_hurdle <- function(formula, data, model_survival = NULL,
                        family_growth = NULL, predictor_scale = "auto", ...) {
  # Read the family argument unevaluated, before `family_growth` is reassigned.
  growth_link_source <- family_link_source(substitute(family_growth),
                                           env = parent.frame())
  formula <- bayesnecformula(formula, env = parent.frame())
  predictor_scale <- validate_predictor_scale(predictor_scale)
  y_var <- hurdle_response_var(formula)
  aterms <- check_hurdle_aterms(formula)
  if (!y_var %in% names(data)) {
    stop("The response variable \"", y_var, "\" is not a column in \"data\".",
         call. = FALSE)
  }
  y <- data[[y_var]]
  if (!is.numeric(y)) {
    stop("The response variable \"", y_var, "\" must be numeric.",
         call. = FALSE)
  }
  if (anyNA(y)) {
    stop("The response variable \"", y_var, "\" contains NA values. Every",
         " individual that entered the experiment must be present, with 0",
         " recorded for those that died -- missing rows cannot be",
         " distinguished from deaths.", call. = FALSE)
  }
  if (any(y < 0)) {
    stop("The response variable \"", y_var, "\" contains negative values,",
         " which a zero-bounded growth component cannot represent. If these",
         " encode deaths, recode them as 0; if they are real declines, this",
         " family is not appropriate.", call. = FALSE)
  }
  n_dead <- sum(y == 0)
  if (n_dead == 0) {
    stop("The response variable \"", y_var, "\" contains no zeros, so there",
         " is no survival signal to model. Use bnec() directly.",
         call. = FALSE)
  }
  if (n_dead == length(y)) {
    stop("Every value of \"", y_var, "\" is zero.", call. = FALSE)
  }
  check_hurdle_cens(aterms, data, y, y_var)
  # Check the whole predictor here. The growth subset can omit the rows that
  # contradict the predictor_scale declaration, and the growth component is
  # fitted first, so it would sample before the survival fit refused them.
  full_frame <- model.frame(formula, data = data, run_par_checks = TRUE)
  # Keep this immediately after the frame. model.frame() drops a row with a
  # missing predictor, so the growth bound check below would otherwise refuse
  # the wrong condition.
  check_complete_cases(full_frame)
  validate_predictor_scale(
    predictor_scale, retrieve_var(full_frame, "x_var", error = TRUE)
  )

  # Survival component: one Bernoulli trial per individual, 1 = survived, so
  # the curve declines with concentration like every bayesnec equation and
  # hu = 1 - fitted survival. swap_response() drops any aterm on the response,
  # which is correct for cens(): survival is observed exactly. The disp() term
  # is dropped too, because a Bernoulli response has no dispersion parameter.
  surv_data <- data
  surv_data[[".alive"]] <- as.integer(y > 0)
  surv_formula <- swap_response(drop_disp_term(formula), ".alive")
  if (!is.null(model_survival)) {
    surv_formula <- swap_crf_model(surv_formula, model_survival)
  }
  if (is.null(family_growth)) {
    # Chosen from the non-zero subset, not the whole response: the zeros would
    # otherwise be read as part of a continuous distribution rather than as the
    # hurdle they are.
    family_growth <- validate_family(
      set_distribution(y[y > 0], silence_y_msgs = TRUE)
    )
  } else {
    family_growth <- validate_family(family_growth,
                                     link_source = growth_link_source)
    check_hurdle_growth_family(family_growth)
  }
  # Check group-level terms against each component's set after check_models()
  # has applied its family, not against the requested set, so that a refusal
  # comes before either component samples. The component bnec() calls repeat
  # and record the exclusions, so the reports here are silenced.
  growth_frame <- suppressMessages(model.frame(
    formula, data = data[y > 0, , drop = FALSE], run_par_checks = TRUE
  ))
  survival_family <- bernoulli(link = "identity")
  survival_frame <- suppressMessages(model.frame(
    surv_formula, data = surv_data, run_par_checks = TRUE
  ))
  growth_models <- suppressMessages(check_models(
    get_model_from_formula(formula), family_growth, growth_frame
  ))
  survival_models <- suppressMessages(check_models(
    get_model_from_formula(surv_formula), survival_family, survival_frame
  ))
  check_group_terms(growth_models, formula, report = FALSE)
  check_group_terms(survival_models, surv_formula, report = FALSE)
  # Check the disp() term against the growth family and the survivors'
  # response before anything is printed. The growth bnec() call repeats it.
  disp_spec <- parse_disp_term(formula)
  if (!is.null(disp_spec)) {
    check_disp_spec(disp_spec, family_growth, response = y[y > 0])
    # Before check_count_truncation_support(), so that a model refused for
    # what it is does not first tell the user to upgrade brms.
    if (identical(family_growth$family, "negbinomial")) {
      check_disp_count_truncation(formula)
    }
  }
  growth_formula <- formula
  if (family_growth$family %in% c("poisson", "negbinomial")) {
    check_count_truncation_support()
    if ("cens" %in% names(aterms)) {
      stop("bnec_hurdle cannot combine cens() with a count growth family.",
           " The positive counts require trunc(lb = 1), and brms does not",
           " condition its censored count likelihood on that lower bound.",
           " Use uncensored counts, or fit a validated custom likelihood.",
           call. = FALSE)
    }
    # The growth data contain only survivors, so their count distribution is
    # conditional on Y > 0. Adding the bound here makes that sampling decision
    # part of the likelihood without exposing trunc() as a user-facing aterm.
    growth_formula <- add_hurdle_truncation(growth_formula)
  }
  # The survivors can all sit at a bound of the growth family, such as a Beta
  # growth component of 1 throughout. Check here so that the message names the
  # growth component, not the whole response column. A growth set of ecxflat
  # alone passes, but unlike bnec() this does not substitute ecxflat for a
  # curve equation.
  check_response_at_bound(
    growth_frame,
    family_growth,
    subject = paste0("The growth component of the response \"", y_var,
                     "\" (its ", sum(y > 0), " non-zero values)"),
    model = get_model_from_formula(formula)
  )
  message("Fitting the growth component (", sum(y > 0), " survivors of ",
          length(y), ") with a ", family_growth$family, " distribution.")
  # User aterms pass through unchanged: a survivor below the recording limit is
  # censored growth, not a structural zero.
  # Predict growth over the whole concentration series, as for survival, so
  # that both components share their limits; an estimate above the last
  # survivor is inside the tested range. x_range is on the recorded scale, and
  # a caller's own x_range reaches both fits through `...` unchanged.
  if ("x_range" %in% ...names()) {
    growth_fit <- bnec(growth_formula, data = data[y > 0, , drop = FALSE],
                       family = family_growth,
                       predictor_scale = predictor_scale, ...)
  } else {
    series <- range(data[[attr(full_frame, "bnec_pop")[["x_var"]]]])
    growth_fit <- bnec(growth_formula, data = data[y > 0, , drop = FALSE],
                       family = family_growth,
                       predictor_scale = predictor_scale, x_range = series,
                       ...)
  }
  message("Fitting the survival component (", n_dead, " deaths of ",
          length(y), ").")
  survival_fit <- bnec(surv_formula, data = surv_data,
                       family = survival_family,
                       predictor_scale = predictor_scale, ...)

  out <- list(growth = growth_fit, survival = survival_fit,
              data = data, formula = formula, y_var = y_var,
              n_exposed = length(y), n_dead = n_dead)
  allot_class(out, c("bayesnechurdlefit", "bnecfit"))
}

#' Refuse a growth family that is itself a two-part or mixture family
#'
#' @param family An object of class \code{\link[stats]{family}}.
#'
#' @details A two-part growth family would nest a hurdle within this one.
#' A zero-inflated count likelihood cannot separate into these two fits. For
#' structural count zeros, use \code{poisson} or \code{negbinomial}; growth is truncated.
#'
#' @return \code{invisible(NULL)}, called for its side effect.
#'
#' @noRd
check_hurdle_growth_family <- function(family) {
  fam_tag <- family$family
  # Refuse here. Left to the growth fit, the call would ask for a trials()
  # term, which check_hurdle_aterms() then refuses.
  if (fam_tag %in% c("binomial", "beta_binomial")) {
    stop("bnec_hurdle cannot use ", fam_tag, " as the growth family: it needs",
         " the number of trials in a trials() term, and bnec_hurdle takes",
         " none, because the survival component has no trial count. For a",
         " proportion among the survivors, give the response as a proportion,",
         " for which Beta is chosen. For counts of successes out of a number",
         " of trials, use bnec() with a trials() term.", call. = FALSE)
  }
  if (fam_tag %in% c("zero_inflated_poisson", "zero_inflated_negbinomial")) {
    stop("bnec_hurdle cannot use ", fam_tag, " as the growth family. A hurdle",
         " treats every zero as structural, which is what lets the two",
         " components be fitted separately. A zero-inflated count model is a",
         " genuine mixture, because poisson and negbinomial can emit a zero of",
         " their own: a zero is evidence about both components at once and the",
         " likelihood does not factorise, so two separate fits would give you a",
         " different model. Use bnec(family = \"", fam_tag, "\") for the",
         " mixture. If every zero really is structural you want a hurdle on",
         " counts. Pass family_growth = ",
         sub("^zero_inflated_", "", fam_tag), "(); bnec_hurdle fits the",
         " required zero-truncated likelihood for the positive subset. See ?bnec.",
         call. = FALSE)
  }
  if (is_hurdle_family(fam_tag)) {
    stop("bnec_hurdle cannot use ", fam_tag, " as the growth family: it is",
         " already a two-block family, and bnec_hurdle is the two-part model.",
         " Pass the family of the non-zero responses -- Gamma, Beta, poisson",
         " or negbinomial -- or",
         " leave family_growth unset and it will be chosen from them. Use",
         " bnec(family = \"", fam_tag, "\") for the equivalent joint fit.",
         call. = FALSE)
  }
  invisible(NULL)
}

#' Check that brms uses inclusive bounds throughout count truncation
#'
#' @param version The installed \pkg{brms} version.
#'
#' @return \code{invisible(NULL)}, called for its side effect.
#'
#' @noRd
check_count_truncation_support <- function(
    version = utils::packageVersion("brms")) {
  if (version < "2.23.2") {
    stop("Factorised count hurdles require brms 2.23.2 or later, which",
         " includes the corrected inclusive lower bound in log_lik() and",
         " posterior_epred(). Installed brms is ", as.character(version), ".",
         " Until brms is updated, use bnec(family = \"hurdle_poisson\") or",
         " bnec(family = \"hurdle_negbinomial\") for a joint count hurdle.",
         call. = FALSE)
  }
  invisible(NULL)
}

#' Add the count-hurdle truncation to the growth formula
#'
#' @param formula An object of class \code{\link{bayesnecformula}}.
#'
#' @return \code{formula} with \code{trunc(lb = 1)} appended to its response.
#'
#' @noRd
add_hurdle_truncation <- function(formula) {
  parts <- hurdle_lhs_parts(lhs(formula))
  trunc_call <- quote(trunc(lb = 1))
  aterms <- c(parts$aterms, list(trunc_call))
  aterm_call <- Reduce(
    function(left, right) call("+", left, right), aterms
  )
  formula[[2]] <- call("|", parts$response, aterm_call)
  # check_formula() accepts trunc() only on an internally marked formula. A
  # caller cannot otherwise use this unvalidated aterm through bnec().
  attr(formula, "bayesnec_internal_truncation") <- TRUE
  formula
}

#' Extract the response variable name from a hurdle formula
#'
#' Refuses a transformed response, since a zero marks a death. Aterms beside
#' the response are left to \code{\link{check_hurdle_aterms}}.
#'
#' @param formula An object of class \code{\link{bayesnecformula}}.
#'
#' @return A \code{\link[base]{character}} string.
#'
#' @importFrom stats Gamma
#' @importFrom formula.tools lhs
#'
#' @noRd
hurdle_response_var <- function(formula) {
  resp_call <- hurdle_lhs_parts(lhs(formula))$response
  y_var <- all.vars(resp_call)
  if (length(y_var) != 1 || !identical(deparse1(resp_call), y_var)) {
    stop("bnec_hurdle requires a plain, untransformed response on the left",
         " of the formula, because zero values in it are used to identify",
         " deaths. You supplied \"", deparse1(resp_call), "\".",
         call. = FALSE)
  }
  y_var
}

#' Split a formula's left-hand side into the response and its aterms
#'
#' @param lhs_call The left-hand side of a formula, as a call.
#'
#' @return A \code{\link[base]{list}} with elements \code{response} (a call) and
#' \code{aterms} (a possibly empty \code{\link[base]{list}} of calls).
#'
#' @noRd
hurdle_lhs_parts <- function(lhs_call) {
  if (!(length(lhs_call) == 3 && identical(lhs_call[[1]], quote(`|`)))) {
    return(list(response = lhs_call, aterms = list()))
  }
  # Peel the "+" chain of aterms apart from the right. Test for `+`: a
  # two-argument aterm such as cens(indicator, upper_bound) also has length 3.
  aterm_call <- lhs_call[[3]]
  out <- list()
  while (length(aterm_call) == 3 && identical(aterm_call[[1]], quote(`+`))) {
    out[[length(out) + 1]] <- aterm_call[[3]]
    aterm_call <- aterm_call[[2]]
  }
  out[[length(out) + 1]] <- aterm_call
  names(out) <- vapply(out, function(tt) {
    nm <- if (is.call(tt)) deparse1(tt[[1]]) else deparse1(tt)
    # Strip a namespace qualifier, so that brms::cens() is read as cens(), as
    # the rest of the package reads it.
    sub("^.*:::?", "", nm)
  }, character(1))
  list(response = lhs_call[[2]], aterms = rev(out))
}

#' Validate the aterms on a hurdle formula's response
#'
#' @param formula An object of class \code{\link{bayesnecformula}}.
#'
#' @details Only \code{cens()} is accepted: it lets the growth component hold
#' a survivor measured below the recording limit, which is not a death. Every
#' other aterm is refused, with the reason given in \code{reasons} below.
#'
#' @return A \code{\link[base]{list}} of the accepted aterm calls, named by
#' function.
#'
#' @importFrom formula.tools lhs
#'
#' @noRd
check_hurdle_aterms <- function(formula) {
  aterms <- hurdle_lhs_parts(lhs(formula))$aterms
  if (length(aterms) == 0) {
    return(aterms)
  }
  reasons <- c(
    trials = paste("neither component takes a trial count: the growth",
                   "component is continuous, and the survival component is one",
                   "Bernoulli trial per individual. An aggregated count cannot",
                   "be split into survivors and deaths row by row"),
    weights = paste("whether a weight applies to the growth component, the",
                    "survival component or both is a modelling decision that",
                    "bnec_hurdle will not make for you. Make the two bnec()",
                    "calls directly if you need it"),
    rate = paste("the two components measure different things and only one of",
                 "them is a count. An exposure denominator applies to the",
                 "growth component, where it is a rate; the survival component",
                 "is one Bernoulli trial per individual and has no rate to",
                 "take a denominator. Fit the growth component with bnec() and",
                 "a rate() term directly if you need it")
  )
  bad <- setdiff(names(aterms), "cens")
  if (length(bad) > 0) {
    detail <- vapply(bad, function(nm) {
      why <- if (nm %in% names(reasons)) {
        reasons[[nm]]
      } else {
        paste("it has no validated behaviour in a two-block fit, where the",
              "response is split across two models")
      }
      paste0("\"", nm, "()\": ", why)
    }, character(1))
    stop("bnec_hurdle accepts only the cens() aterm on the response.",
         " Rejected: ", paste0(detail, collapse = "; "), ".", call. = FALSE)
  }
  aterms
}

#' Check that no structural zero is also declared censored
#'
#' @param aterms The accepted aterms, as returned by
#' \code{\link{check_hurdle_aterms}}.
#' @param data The user's \code{\link[base]{data.frame}}.
#' @param y The response vector.
#' @param y_var The response variable name.
#'
#' @details Refuses a row that is both 0 and censored. A 0 is a structural zero
#' of the survival component, and a censored row is a growth observation at or
#' below a bound.
#'
#' @return \code{invisible(NULL)}, called for its side effect.
#'
#' @noRd
check_hurdle_cens <- function(aterms, data, y, y_var) {
  if (!"cens" %in% names(aterms)) {
    return(invisible(NULL))
  }
  cens_args <- as.list(aterms[["cens"]])[-1]
  if (length(cens_args) == 0) {
    stop("cens() was supplied with no arguments, so there is no censoring",
         " indicator to check the zeros against. Pass the column holding the",
         " censoring codes, e.g. \"", y_var, " | cens(censored) ~ ...\".",
         call. = FALSE)
  }
  # Take the first argument by position, as split_calls() does. Matching by
  # name here could read a different column from the one the fit is given.
  cens_arg <- cens_args[[1]]
  cens_vals <- if (length(all.vars(cens_arg)) == 0) {
    # A literal such as cens("left"), which brms recycles over every row, and
    # so declares every zero censored.
    eval(cens_arg)
  } else {
    data[[all.vars(cens_arg)[1]]]
  }
  cens <- normalise_cens(cens_vals)
  bad <- which(y == 0 & is_censored(cens))
  if (length(bad) > 0) {
    stop("Row(s) ",
         paste0(bad[seq_len(min(10, length(bad)))], collapse = ", "),
         if (length(bad) > 10) ", ..." else "",
         " of \"", y_var, "\" are zero and also carry a censoring code other",
         " than \"none\". A structural zero and a censored observation are the",
         " two things a hurdle model exists to separate: a zero means the",
         " individual gave no response at all, while a censored row is a real",
         " observation known only to lie at or below a bound. Recode each row",
         " as one or the other.", call. = FALSE)
  }
  invisible(NULL)
}

#' Replace the response of a bayesnecformula, keeping the right-hand side
#'
#' @param formula An object of class \code{\link{bayesnecformula}}.
#' @param new_response A \code{\link[base]{character}} string.
#'
#' @return An object of class \code{\link{bayesnecformula}}.
#'
#' @importFrom formula.tools rhs
#' @importFrom stats as.formula
#'
#' @noRd
swap_response <- function(formula, new_response) {
  # Deparsing the right-hand side drops its environment. Pass it on, so that a
  # model set or transformation held in a variable still resolves.
  bayesnecformula(
    as.formula(paste0(new_response, " ~ ", deparse1(rhs(formula))),
               env = formula_env(formula))
  )
}

#' Remove a disp() term from the right-hand side of a bayesnecformula
#'
#' @param formula An object of class \code{\link{bayesnecformula}}.
#'
#' @details Rebuilds the right-hand side from its top-level \code{+} terms, so
#' the others keep their order. \code{update()} would pass it through
#' \code{terms()}, which can reorder and simplify them. A qualified
#' \code{bayesnec::disp()} counts as \code{disp()}.
#'
#' @return \code{formula} without its \code{disp()} term, with its class,
#' attributes and environment kept, or \code{formula} unchanged where it has
#' none.
#'
#' @importFrom formula.tools rhs
#'
#' @noRd
drop_disp_term <- function(formula) {
  split_sum <- function(expr) {
    if (is.call(expr) && length(expr) == 3 &&
          identical(expr[[1]], quote(`+`))) {
      return(c(split_sum(expr[[2]]), split_sum(expr[[3]])))
    }
    list(expr)
  }
  is_disp <- function(term) {
    is.call(term) &&
      identical(sub("^.*:::?", "", deparse1(term[[1]])), "disp")
  }
  rhs_terms <- split_sum(rhs(formula))
  keep <- !vapply(rhs_terms, is_disp, logical(1))
  if (all(keep)) {
    return(formula)
  }
  formula[[3]] <- Reduce(function(left, right) call("+", left, right),
                         rhs_terms[keep])
  formula
}

#' Replace the model argument inside a crf term
#'
#' Mirrors single_model_formula(), but takes a model set rather than a single
#' model so that a whole group (e.g. "nec") can be substituted.
#'
#' @param formula An object of class \code{\link{bayesnecformula}}.
#' @param model A \code{\link[base]{character}} vector of model names.
#'
#' @return An object of class \code{\link{bayesnecformula}}.
#'
#' @importFrom stats update terms
#'
#' @noRd
swap_crf_model <- function(formula, model) {
  x_str <- grep("crf(", labels(terms(formula)), fixed = TRUE, value = TRUE)
  x_term <- eval(parse(text = x_str))
  models <- paste0("c(", paste0("\"", model, "\"", collapse = ", "), ")")
  new_crf <- paste0("crf(", x_term, ", model = ", models, ")")
  to_eval <- paste0("update(formula, ~ . - ", x_str, " + ", new_crf, ")")
  bayesnecformula(eval(parse(text = to_eval)))
}
