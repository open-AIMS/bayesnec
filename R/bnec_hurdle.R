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
#' zeros alongside a continuous response fits the same structure. Algal growth
#' rate expressed as a proportion of a ceiling, with replicates that failed
#' entirely, is the same model.
#'
#' @param formula Either a \code{\link[base]{character}} string defining an
#' R formula or an actual \code{\link[stats]{formula}} object. See
#' \code{\link{bayesnecformula}}. The response must be untransformed, and zero
#' values in it are taken to mean the individual did not survive. A
#' \code{cens()} aterm is allowed alongside it; other aterms are refused, see
#' Details.
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
#' positive continuous response, \code{Beta} for one bounded on (0, 1).
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
#' \code{cens()} is the one \pkg{brms} aterm accepted on the response, because
#' it is the one a hurdle model needs. A growth endpoint can be both
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
#' Other aterms are refused by name, with the reason. \code{trials()} has no
#' meaning for either component, and \code{weights()} is a modelling decision --
#' whether a weight applies to the growth component, the survival component or
#' both -- that this function will not make on the user's behalf. Making the two
#' \code{\link{bnec}} calls directly remains available for anything outside this
#' set.
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
                        family_growth = NULL, ...) {
  formula <- bayesnecformula(formula)
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

  # Survival component: one Bernoulli trial per individual, 1 = survived. The
  # curve therefore declines with concentration, matching the sign convention
  # of every bayesnec equation, and hu = 1 - fitted survival.
  #
  # swap_response() rebuilds the formula from rhs() alone, so any aterm on the
  # response is dropped here rather than carried over. That is deliberate for
  # cens(): the Bernoulli response is alive/dead, which is observed exactly, so
  # there is nothing for a censoring declaration to bound. The censoring
  # indicator stays in surv_data as an ordinary unused column.
  surv_data <- data
  surv_data[[".alive"]] <- as.integer(y > 0)
  surv_formula <- swap_response(formula, ".alive")
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
  }
  message("Fitting the growth component (", sum(y > 0), " survivors of ",
          length(y), ") with a ", family_growth$family, " distribution.")
  # The formula is passed through unchanged, aterms included: a censoring
  # indicator is an ordinary data column, so the subset carries it along and the
  # declaration reaches the block it belongs to. A survivor measured below the
  # recording limit is an observation of *this* component, not a structural zero.
  growth_fit <- bnec(formula, data = data[y > 0, , drop = FALSE],
                     family = family_growth, ...)
  message("Fitting the survival component (", n_dead, " deaths of ",
          length(y), ").")
  survival_fit <- bnec(surv_formula, data = surv_data,
                       family = bernoulli(link = "identity"), ...)

  out <- list(growth = growth_fit, survival = survival_fit,
              data = data, formula = formula, y_var = y_var,
              n_exposed = length(y), n_dead = n_dead)
  allot_class(out, c("bayesnechurdlefit", "bnecfit"))
}

#' Extract the response variable name from a hurdle formula
#'
#' Errors informatively if the response itself is transformed, which makes the
#' zero-as-death convention ambiguous. Aterms alongside the response are left
#' alone here and validated by \code{\link{check_hurdle_aterms}}: what
#' \code{bnec_hurdle} needs is a bare response variable, not a bare left-hand
#' side.
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
  # aterms are joined by "+", so peel the chain apart from the right. The `+`
  # test matters for the same reason it does in split_calls(): without it a
  # two-argument aterm such as cens(indicator, upper_bound) is itself length 3
  # and would be destructured as though it were a chain.
  aterm_call <- lhs_call[[3]]
  out <- list()
  while (length(aterm_call) == 3 && identical(aterm_call[[1]], quote(`+`))) {
    out[[length(out) + 1]] <- aterm_call[[3]]
    aterm_call <- aterm_call[[2]]
  }
  out[[length(out) + 1]] <- aterm_call
  names(out) <- vapply(out, function(tt) {
    if (is.call(tt)) deparse1(tt[[1]]) else deparse1(tt)
  }, character(1))
  list(response = lhs_call[[2]], aterms = rev(out))
}

#' Validate the aterms on a hurdle formula's response
#'
#' @param formula An object of class \code{\link{bayesnecformula}}.
#'
#' @details Only \code{cens()} is accepted. A censored response is the case
#' \code{bnec_hurdle} needs it for: a growth endpoint can be both zero-bounded
#' with structural zeros and left-censored at the recording resolution, and only
#' a two-part model with a censored response component can tell a death from a
#' survivor measured below the limit.
#'
#' Every other aterm is refused by name. \code{trials()} has no meaning for
#' either component -- the growth component is continuous and the survival
#' component is one Bernoulli trial per individual -- and an aggregated count
#' could not be split into survivors and deaths row by row anyway.
#' \code{weights()} is refused because whether a weight belongs to the growth
#' component, the survival component or both is a modelling decision, and
#' guessing it would silently change the model; a user who knows which they want
#' can make the two \code{\link{bnec}} calls directly.
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
                    "calls directly if you need it")
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
#' @details The two kinds of zero a hurdle model exists to separate must stay
#' separate in the input. A \code{0} response is a structural zero belonging to
#' the Bernoulli component; a left-censored row is an observation of the growth
#' component whose value is known only to be at or below a bound. A row claiming
#' to be both is not a model this function can fit, and accepting it silently
#' would reproduce exactly the confusion \code{vignette("example6")} warns
#' about.
#'
#' @return \code{invisible(NULL)}, called for its side effect.
#'
#' @noRd
check_hurdle_cens <- function(aterms, data, y, y_var) {
  if (!"cens" %in% names(aterms)) {
    return(invisible(NULL))
  }
  cens_arg <- as.list(aterms[["cens"]])[-1][[1]]
  cens_vals <- if (length(all.vars(cens_arg)) == 0) {
    # A literal, e.g. cens("left"), which brms recycles over every row.
    # check_formula() already warns about this; here it still has to be checked
    # against the zeros, because recycled over a response containing zeros it
    # declares those zeros censored.
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
  bayesnecformula(
    as.formula(paste0(new_response, " ~ ", deparse1(rhs(formula))))
  )
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
