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
#' values in it are taken to mean the individual did not survive.
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

  # Survival component: one Bernoulli trial per individual, 1 = survived. The
  # curve therefore declines with concentration, matching the sign convention
  # of every bayesnec equation, and hu = 1 - fitted survival.
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
#' Errors informatively if the response is transformed or carries aterms, both
#' of which make the zero-as-death convention ambiguous.
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
  lhs_call <- lhs(formula)
  y_var <- all.vars(lhs_call)
  if (length(y_var) != 1 || !identical(deparse1(lhs_call), y_var)) {
    stop("bnec_hurdle requires a plain, untransformed response on the left",
         " of the formula, because zero values in it are used to identify",
         " deaths. You supplied \"", deparse1(lhs_call), "\".",
         call. = FALSE)
  }
  y_var
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
