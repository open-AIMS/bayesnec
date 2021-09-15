#' Set up a model formula for use in
#' \code{\link[bayesnec:bayesnec-package]{bayesnec}}
#'
#' Set up a model formula for use in the 
#' \code{\link[bayesnec:bayesnec-package]{bayesnec}} package allowing to
#' define linear and non-linear (potentially multilevel) concentration-response
#' models.
#'
#' @aliases bnf
#'
#' @param formula Either a \code{\link[base]{character}} string defining an
#' R formula or an actual \code{\link[stats]{formula}} object. See details.
#' @param ... Unused.
#'
#' @importFrom stats as.formula
#'
#' @details
#'
#' See \code{methods(class = "bayesnecformula")} for an overview of
#' available methods.
#'
#' \bold{General formula syntax}
#'
#' The \code{formula} argument accepts formulas of the following syntax:
#'   
#' \code{response | aterms ~ crf(x, model) + glterms}
#'
#' \bold{The population-level term: \code{crf}}
#'
#' \code{\link[bayesnec:bayesnec-package]{bayesnec}} uses a special internal
#' term called \code{crf} which sets the concentration-response equation
#' to be evaluated based on some \code{x} predictor. The equation itself is
#' defined by the argument \code{"model"}: a \code{\link[base]{character}}
#' vector containing a specific model, a concatenation of specific models,
#' or a single string defining a particular group of models
#' (or group of equations, see \code{\link{models}}). Internally
#' this argument is substituted by an actual \code{\link[brms]{brmsformula}}
#' which is then passed onto \code{\link[brms]{brm}} for model fitting.
#' 
#' \bold{Group-level terms: \code{glterms}}
#' 
#' The user has three options to define group-level effects in a
#' \code{\link{bayesnecformula}}: 1) a general "offset" group-level effect
#' defined by the term \code{ogl} (as in e.g. \code{ogl(group_variable)}). This
#' adds an additional population-level parameter \code{ogl} to the model defined
#' by \code{crf}, analogously to what an intercept-only group-level effect
#' would be in a classic linear model. 2) a group-level effect applied to all
#' parameters in a model at once. This is done by the special term \code{pgl},
#' (as in e.g. \code{pgl(group_variable)}) which comes in handy so the user
#' does not need to know the internal syntax and name of each parameter in the
#' model. 3) a more classic approach where the user can specify which
#' specific parameters --- NB: that requires prior knowledge on the model
#' structure and parameter names --- to vary according to a grouping variable
#' (as in e.g. \code{(bot | group_variable)}). \code{\link{bayesnecformula}}
#' will ignore this term should the parameter not exist in the specified model
#' or model suite. For example, the parameter \code{bot} exists in model
#' \code{"nec4param"} but not in \code{"nec3param"}, so if the user specifies
#' \code{model = "nec"} in \code{crf}, the term \code{(bot | group_variable)}
#' will be dropped in models where that parameter does not exist.
#'
#' \bold{Further brms terms (largely untested)}
#'
#' Currently \code{\link{bayesnecformula}} is quite agnostic about additional
#' terms that are valid for a \code{\link[brms]{brmsformula}}. These are
#' \code{aterms} and \code{pterms} (see \code{?\link[brms]{brmsformula}}).
#' The only capability that \code{\link{bayesnecformula}} does not allow is
#' the addition of \code{pterms} outside of the term \code{crf}. Although
#' \code{pterms} can be passed to predictor \code{x} within \code{crf}, we
#' strongly discourage the user because those functionalities have not
#' been tested yet. If this is extremely important to your research, please
#' raise an issue on GitHub and we will consider.
#' Currently, the only two \code{aterms} that have validated behaviour are:
#' 1) \code{trials()} which is essential in binomially-distributed data, e.g.
#' \code{y | trials(trials_variable)}, and 2) weights, e.g.
#' \code{y | weights(weights_variable)}, following \pkg{brms} formula syntax.
#' Please note that \pkg{brms} does not implement design weights as in other
#' standard \pkg{base} function. From their help page, \pkg{brms} "takes the
#' weights literally, which means that an observation with weight 2 receives 2
#' times more weight than an observation with weight 1. It also means that
#' using a weight of 2 is equivalent to adding the corresponding observation
#' twice to the data frame." Other \code{aterms} might be added, though we
#' cannot attest to their functionality within
#' \code{\link[bayesnec:bayesnec-package]{bayesnec}}, i.e. checks will
#' be done outside via \code{\link[brms]{brm}}.
#' 
#' **NB:** \code{aterms} other than \code{trials()} and \code{weights()} are
#' currently omitted from \code{\link{model.frame}} output. If you need other
#' \code{aterms} as part of that output please raise an issue on our GitHub
#' page.
#' 
#' \bold{Validation of formula}
#' Please note that the function only checks for the input nature of the
#' \code{formula} argument and adds a new class. This function **does not**
#' perform any validation on the model nor checks on its adequacy to work with
#' other functions in the package. For that please refer to the function
#' \code{\link{check_formula}} which requires the dataset associated with the
#' formula.
#'
#' @return An object of class \code{\link{bayesnecformula}} and
#' \code{\link[stats]{formula}}.
#'
#' @seealso
#'   \code{\link{check_formula}},
#'   \code{\link{model.frame}},
#'   \code{\link{models}},
#'   \code{\link{show_params}},
#'   \code{\link{make_brmsformula}}
#'
#' @examples
#' library(bayesnec)
#' 
#' bayesnecformula(y ~ crf(x, "nec3param"))
#' # or use shot alias bnf
#' bayesnecformula(y ~ crf(x, "nec3param")) == bnf(y ~ crf(x, "nec3param"))
#' bnf(y | trials(tr) ~ crf(sqrt(x), "nec3param"))
#' bnf(y | trials(tr) ~ crf(x, "nec3param") + ogl(group_1) + pgl(group_2))
#' bnf(y | trials(tr) ~ crf(x, "nec3param") + (nec + top | group_1))
#'
#' \donttest{
#' # complex transformations are not advisable because
#' # they are passed directly to Stan via brms
#' # and are likely to fail -- transform your variable beforehand!
#' try(bnf(y | trials(tr) ~ crf(scale(x, scale = TRUE), "nec3param")))
#' }
#' @export
bayesnecformula <- function(formula, ...) {
  if (is.character(formula)) {
    formula <- as.formula(formula)
  } else if (!inherits(formula, "formula")) {
    stop("Your formula must be either a valid character or a formula object.")
  }
  allot_class(formula, c("formula", "bayesnecformula"))
}

#' @export
bnf <- function(formula, ...) {
  bayesnecformula(formula = formula, ...)
}

#' Check if input model formula is appropriate to use with
#' \code{\link[bayesnec:bayesnec-package]{bayesnec}}
#'
#' Perform a series of checks to ensure that the input formula is valid
#' for usage within \code{\link[bayesnec:bayesnec-package]{bayesnec}}.
#'
#' @param formula An object of class \code{\link{bayesnecformula}} as returned
#' by function \code{\link{bayesnecformula}}.
#' @param data A \code{\link[base]{data.frame}} containing the variables
#' specified in \code{formula}.
#' @param run_par_checks See details. A \code{\link[base]{logical}} defining
#' whether random terms for specific parameters should be checked against the
#' underlying concentration-response model defined in \code{formula}.
#' Defaults to \code{FALSE}.
#'
#' @return A validated object of class \code{\link{bayesnecformula}} and
#' \code{\link[stats]{formula}}.
#'
#' @details This function allows the user to make sure that the input formula
#' will allow for a successful model fit with the function \code{\link{bnec}}.
#' Should all checks pass, the function returns the original formula. Otherwise
#' it will fail and requires that the user fixes it until they're able to use
#' it with \code{\link{bnec}}.
#'
#' The argument \code{run_par_checks} is irrelevant for most usages of this
#' package because it only applies if three conditions are met: 1) the user has
#' specified a group-level effect; 2) the group-level effects is parameter 
#' specific (e.g. \code{(par | group_variable)} rather than \code{pgl/ogl(group_variable)}); and 3) The user is keen to learn if the specified parameter
#' is found in the specified model (via argument \code{model} in the \code{crf} term -- see details in ?bayesnecformula).
#'
#' **NB:** \code{aterms} other than \code{trials()} and \code{weights()} are
#' currently omitted from \code{\link{model.frame}} output. If you need other
#' \code{aterms} as part of that output please raise an issue on our GitHub
#' page. See details about \code{aterms} in ?bayesnecformula.
#'
#' @seealso
#'   \code{\link{bnec}},
#'   \code{\link{bayesnecformula}}
#'
#' @examples
#' library(bayesnec)
#' nec3param <- function(beta, nec, top, x) {
#'   top * exp(-exp(beta) * (x - nec) *
#'     ifelse(x - nec < 0, 0, 1))
#' }
#' 
#' data <- data.frame(x = seq(1, 20, length.out = 10), tr = 100, wght = c(1, 2),
#'                    group_1 = sample(c("a", "b"), 10, replace = TRUE),
#'                    group_2 = sample(c("c", "d"), 10, replace = TRUE))
#' data$y <- nec3param(beta = -0.2, nec = 4, top = 100, data$x)
#' 
#' # returns error
#' # > f_1 <- y ~ crf(x, "nec3param") + z
#' # regular formula not allowed, wrap it with function bnf
#' # > check_formula(f_1, data)
#' # population-level covariates are not allowed
#' # > check_formula(bnf(f_1), data)
#'
#' \donttest{
#' # expect a series of messages for because not all
#' # nec models have the "bot" parameter
#' f_2 <- y | trials(tr) ~ crf(x, "nec") + (nec + bot | group_1)
#' check_formula(bnf(f_2), data, run_par_checks = TRUE)
#' }
#' # runs fine
#' f_3 <- "y | trials(tr) ~ crf(sqrt(x), \"nec3param\")"
#' check_formula(bnf(f_3), data)
#' f_4 <- y | trials(tr) ~ crf(x, "nec3param") + ogl(group_1) + pgl(group_2)
#' inherits(check_formula(bnf(f_4), data), "bayesnecformula")
#'
#' @export
check_formula <- function(formula, data, run_par_checks = FALSE) {
  UseMethod("check_formula")
}

#' Check if input model formula is appropriate to use with
#' \code{\link[bayesnec:bayesnec-package]{bayesnec}}
#'
#' Perform a series of checks to ensure that the input formula is appropriately
#' set up for usage within \code{\link[bayesnec:bayesnec-package]{bayesnec}}.
#'
#' @inheritParams check_formula
#' @inherit check_formula examples details return
#'
#' @export
#' @noRd
check_formula.bayesnecformula <- function(formula, data,
                                          run_par_checks = FALSE) {
  check_formula.default(formula = formula, data = data,
                        run_par_checks = run_par_checks)
}

#' Check if input model formula is appropriate to use with
#' \code{\link[bayesnec:bayesnec-package]{bayesnec}}
#'
#' Perform a series of checks to ensure that the input formula is appropriately
#' set up for usage within \code{\link[bayesnec:bayesnec-package]{bayesnec}}.
#'
#' @inheritParams check_formula
#' @inherit check_formula examples details return
#'
#' @importFrom formula.tools lhs rhs
#'
#' @export
#' @noRd
check_formula.default <- function(formula, data, run_par_checks = FALSE) {
  if (!inherits(formula, "bayesnecformula")) {
    stop("Your formula must be of class bayesnecformula.")
  }
  rhs_calls <- gsub("\\) \\+ ", ") impossiblestr ", deparse1(rhs(formula)))
  split_rhs_calls <- strsplit(rhs_calls, " impossiblestr ")[[1]]
  x_str <- grep("crf(", split_rhs_calls, fixed = TRUE, value = TRUE)
  if (length(x_str) == 0) {
    stop("You must specify which non-linear function to use with crf.",
         " See ?bayesnecformula")
  }
  crf_vars <- all.vars(str2lang(x_str))
  if (length(crf_vars) != 1) {
    stop("The `crf` term in your formula can only have one variable; you",
         " specified ", paste0(crf_vars, collapse = "; "), ".")
  } else if (!crf_vars %in% names(data)) {
    stop("Predictor variable \"", crf_vars, "\" not present in data.frame.")
  }
  all_models <- get_model_from_formula(formula)
  model <- all_models[1]
  brms_bf <- get(paste0("bf_", model))
  split_random_call <- setdiff(split_rhs_calls, x_str)
  if (any(grepl("pgl(", split_random_call, fixed = TRUE))) {
    str_calls <- grep("pgl(", split_random_call, fixed = TRUE, value = TRUE)
    vars <- all.vars(str2lang(paste0(str_calls, collapse = " + ")))
    for (i in seq_along(brms_bf[[2]])) {
      for (j in seq_along(vars)) {
        if (!vars[j] %in% names(data)) {
          stop("Group-level variable(s) ",
               paste0("\"", vars[j], "\"", collapse = "; "),
               " not found in dataset.")
        } else if (any(sapply(data[, vars[j]], is.numeric))) {
          stop("Group-level variables cannot be numeric.")
        }
      }
    }
    split_random_call <- setdiff(split_random_call, str_calls)
  }
  if (any(grepl("|", split_random_call, fixed = TRUE))) {
    str_calls <- grep("|", split_random_call, fixed = TRUE, value = TRUE)
    split_str_calls <- lapply(str_calls, clean_bar_glef)
    tmp_list <- list()
    for (i in seq_along(split_str_calls)) {
      if (any(grepl(" \\+ ", split_str_calls[[i]][2]))) {
        stop("Right-hand side of the \"|\" symbol in the group-level portion",
             " of your formula can only have one grouping variable. Issue",
             " found on: ", str_calls[i])
      }
      if (any(grepl(" \\+ ", split_str_calls[[i]][1]))) {
        extended_pars <- strsplit(split_str_calls[[i]][1], " \\+ ")[[1]]
        tmp_list_i <- vector(mode = "list", length = length(extended_pars))
        for (j in seq_along(tmp_list_i)) {
          tmp_list_i[[j]] <- c(extended_pars[j], split_str_calls[[i]][2])
        }
        tmp_list <- c(tmp_list, tmp_list_i)
      } else {
        tmp_list <- c(tmp_list, split_str_calls[i])
      }
    }
    split_str_calls <- tmp_list
    pars <- sapply(split_str_calls, `[[`, 1)
    vars <- sapply(split_str_calls, `[[`, 2)
    vars <- strsplit(vars, "\\/|\\:")[[1]]
    for (j in seq_along(vars)) {
      if (!vars[j] %in% names(data)) {
        stop("Group-level variable(s) ",
             paste0("\"", vars[j], "\"", collapse = "; "),
             " not found in dataset.")
      } else if (any(sapply(data[, vars[j]], is.numeric))) {
        stop("Group-level variables cannot be numeric.")
      }
    }
    if (run_par_checks) {
      message("Performing single parameter checks on all models...")
      for (h in seq_along(all_models)) {
        tmp_brms_bf <- get(paste0("bf_", all_models[h]))
        if (!all(pars %in% names(tmp_brms_bf[[2]]))) {
          to_flag <- pars[!pars %in% names(tmp_brms_bf[[2]])]
          message("The parameter(s) ",
                  paste0("\"", to_flag, "\"", collapse = "; "),
                  " not valid parameters in ", all_models[h], ". If this",
                  " was a mistake, check ?models and ?show_params, otherwise",
                  " ignore.")
        }
      }
    }
    split_random_call <- setdiff(split_random_call, str_calls)
  }
  if (any(grepl("ogl(", split_random_call, fixed = TRUE))) {
    str_calls <- grep("ogl(", split_random_call, fixed = TRUE, value = TRUE)
    vars <- all.vars(str2lang(paste0(str_calls, collapse = " + ")))
    for (i in seq_along(brms_bf[[2]])) {
      for (j in seq_along(vars)) {
        if (!vars[j] %in% names(data)) {
          stop("Group-level variable(s) ",
                  paste0("\"", vars[j], "\"", collapse = "; "),
                  " not found in dataset.")
        } else if (any(sapply(data[, vars[j]], is.numeric))) {
          stop("Group-level variables cannot be numeric.")
        }
      }
    }
    split_random_call <- setdiff(split_random_call, str_calls)
  }
  if (length(split_random_call) > 0) {
    stop("Term(s) ", paste0(split_random_call, collapse = "; "), "; are not",
         " allowed in a bayesnec formula. See ?bayesnecformula")
  }
  lhs_vars <- all.vars(lhs(formula))
  exist_all_left <- all(lhs_vars %in% names(data))
  if (!exist_all_left) {
    to_flag <- paste0(setdiff(lhs_vars, names(data)), collapse = "; ")
    stop("Variable(s) ", to_flag, " not present in data.frame")
  }
  lhs_calls <- gsub("\\) \\+ ", ") impossiblestr ", deparse1(lhs(formula)))
  split_lhs_calls <- strsplit(lhs_calls, " \\| | impossiblestr ")[[1]]
  no_resp <- split_lhs_calls[-1]
  if (length(no_resp) > 0) {
    if (sum(grepl("trials\\(|weights\\(", no_resp)) < length(no_resp)) {
      message("You have specified brms special aterms other than trials and",
              " weights. bnec may yield unexpected model fits, proceed at",
              " your own risk. See ?bayesnecformula.")
    }
  }
  formula
}

#' Extracting the model frame from a \code{\link{bayesnecformula}}
#'
#' Recovers evaluated \code{\link[base]{data.frame}} given input \code{data}
#' and a formula of class \code{\link{bayesnecformula}}.
#'
#' @param formula A formula of class \code{\link{bayesnecformula}}.
#' @param data A \code{\link[base]{data.frame}} containing the variables
#' specified in \code{formula}.
#' @param ... Additional arguments to be passed to 
#' \code{\link{check_formula}}.
#'
#' @importFrom stats model.frame na.omit
#'
#' @details If the formula contains transformations to variables x and y,
#' these are evaluated and returned as part of the 
#' \code{\link[base]{data.frame}}.
#'
#' @return A \code{\link[base]{data.frame}} with additional attributes
#' detailing the population-level variables (attribute \code{"bnec_pop"})
#' (response y, predictor x, and, if binomial a formula, trials) and, if
#' applicable, the group-level variables (attribute \code{"bnec_group"}).
#'
#' @examples
#' library(bayesnec)
#' nec3param <- function(beta, nec, top, x) {
#'   top * exp(-exp(beta) * (x - nec) *
#'     ifelse(x - nec < 0, 0, 1))
#' }
#' 
#' data <- data.frame(x = seq(1, 20, length.out = 10), tr = 100, wght = c(1, 2),
#'                    group_1 = sample(c("a", "b"), 10, replace = TRUE),
#'                    group_2 = sample(c("c", "d"), 10, replace = TRUE))
#' data$y <- nec3param(beta = -0.2, nec = 4, top = 100, data$x)
#' 
#' f_1 <- y ~ crf(x, "nec3param")
#' f_2 <- "y | trials(tr) ~ crf(sqrt(x), \"nec3param\")"
#' f_3 <- y | trials(tr) ~ crf(x, "nec3param") + ogl(group_1) + pgl(group_2)
#' f_4 <- y | trials(tr) ~ crf(x, "nec3param") + (nec + top | group_1)
#' 
#' m_1 <- model.frame(bnf(f_1), data)
#' attr(m_1, "bnec_pop")
#' model.frame(bnf(f_2), data)
#' m_3 <- model.frame(bnf(f_3), data)
#' attr(m_3, "bnec_group")
#' model.frame(bnf(f_4), data)
#' 
#' @export
model.frame.bayesnecformula <- function(formula, data, ...) {
  if (!inherits(data, "data.frame")) {
    stop("Argument data is not a data.frame.")
  }
  pre_list <- simplify_formula(formula, data, ...)
  data <- model.frame(pre_list$formula, data = data)
  bnec_pop <- na.omit(pre_list$pop_vars)
  names(bnec_pop) <- c("y_var", "x_var", "trials_var")[seq_along(bnec_pop)]
  attr(data, "bnec_pop") <- bnec_pop
  bnec_group <- pre_list$group_vars
  attr(data, "bnec_group") <- bnec_group
  data
}

#' @noRd
#' @importFrom formula.tools lhs rhs
#' @importFrom stats as.formula terms
simplify_formula <- function(formula, data, ...) {
  formula <- check_formula(formula, data, ...)
  formula_lhs <- lhs(formula)
  if (length(formula_lhs) < 3) {
    y_call <- formula_lhs
    t_call <- 1
    t_var <- NA
  } else if (length(formula_lhs) == 3) {
    y_call <- formula_lhs[[2]]
    split_terms <- split_calls(formula_lhs[[3]])
    t_call <- split_terms$t_call
    t_var <- split_terms$t_var
  }
  y_var <- all.vars(y_call)
  x_str <- grep("crf(", labels(terms(formula)), fixed = TRUE, value = TRUE)
  x_call <- str2lang(eval(parse(text = x_str)))
  x_var <- all.vars(x_call)
  r_vars <- intersect(names(data), setdiff(all.vars(rhs(formula)), x_var))
  if (length(r_vars) == 0) {
    r_call <- 1
    r_vars <- NA
  } else {
    r_call <- str2lang(paste0(r_vars, collapse = " + "))
  }
  short_form <- substitute(a ~ b + c + d, list(a = y_call, b = x_call, c = t_call, d = r_call))
  list(formula = as.formula(short_form), pop_vars = c(y_var, x_var, t_var),
       group_vars = r_vars)
}

#' @noRd
#' @importFrom formula.tools lhs
wrangle_model_formula <- function(model, formula, data) {
  brms_bf <- get(paste0("bf_", model))
  brms_bf[[1]][[2]] <- lhs(formula)
  bnec_pop_vars <- attr(data, "bnec_pop")
  new_x <- names(data)[which(names(bnec_pop_vars) == "x_var")]
  brms_rhs <- deparse1(brms_bf[[1]][[3]])
  tmp <- gsub("[x](?![aA-zZ])", new_x, brms_rhs, perl = TRUE)
  brms_bf[[1]][[3]] <- str2lang(tmp)
  bnec_group_vars <- attr(data, "bnec_group")
  if (any(!is.na(bnec_group_vars))) {
    brms_bf <- add_formula_glef(model, brms_bf, formula, data)
  }
  brms_bf
}

#' @noRd
#' @importFrom stats update terms
single_model_formula <- function(formula, model) {
  x_str <- grep("crf(", labels(terms(formula)), fixed = TRUE, value = TRUE)
  x_term <- eval(parse(text = x_str))
  new_crf <- paste0("crf(", x_term, ", model = \"", model, "\")")
  to_eval <- paste0("update(formula, ~ . - ", x_str, " + ", new_crf, ")")
  formula <- eval(parse(text = to_eval))
  bayesnecformula(formula)
}

#' @noRd
clean_bar_glef <- function(x) {
  x <- strsplit(x, split = " | ", fixed = TRUE)[[1]]
  gsub("\\(|\\)", "", x)
}

#' @noRd
#' @importFrom stats terms
#' @importFrom formula.tools rhs `rhs<-`
add_formula_glef <- function(model, brmform, bnecform, data) {
  crf_term <- grep("crf(", labels(terms(bnecform)), fixed = TRUE,
                   value = TRUE)
  to_eval <- paste0("update(bnecform, ~ . - ", crf_term, ")")
  random_call <- rhs(eval(parse(text = to_eval)))
  random_call <- gsub("\\) \\+ ", ") impossiblestr ", deparse1(random_call))
  split_random_call <- strsplit(random_call, " impossiblestr ")[[1]]
  if (any(grepl("pgl(", split_random_call, fixed = TRUE))) {
    str_calls <- grep("pgl(", split_random_call, fixed = TRUE, value = TRUE)
    vars <- all.vars(str2lang(paste0(str_calls, collapse = " + ")))
    for (i in seq_along(brmform[[2]])) {
      for (j in seq_along(vars)) {
        brmform[[2]][[i]] <- str2lang(paste0(deparse1(brmform[[2]][[i]]),
                                             " + (1 |", vars[j], ")"))
      }
    }
  }
  if (any(grepl("|", split_random_call, fixed = TRUE))) {
    str_calls <- grep("|", split_random_call, fixed = TRUE, value = TRUE)
    split_str_calls <- lapply(str_calls, clean_bar_glef)
    tmp_list <- list()
    for (i in seq_along(split_str_calls)) {
      if (any(grepl(" \\+ ", split_str_calls[[i]][1]))) {
        extended_pars <- strsplit(split_str_calls[[i]][1], " \\+ ")[[1]]
        tmp_list_i <- vector(mode = "list", length = length(extended_pars))
        for (j in seq_along(tmp_list_i)) {
          tmp_list_i[[j]] <- c(extended_pars[j], split_str_calls[[i]][2])
        }
        tmp_list <- c(tmp_list, tmp_list_i)
      } else {
        tmp_list <- c(tmp_list, split_str_calls[i])
      }
    }
    split_str_calls <- tmp_list
    pars <- sapply(split_str_calls, `[[`, 1)
    vars <- sapply(split_str_calls, `[[`, 2)
    if (!all(pars %in% names(brmform[[2]]))) {
      to_flag <- pars[!pars %in% names(brmform[[2]])]
      message("The parameter(s) ", paste0("\"", to_flag, "\"", collapse = "; "),
              " are not valid parameters in ", model, ". Ignoring...")
      split_str_calls <- split_str_calls[-match(to_flag, pars)]
      pars <- sapply(split_str_calls, `[[`, 1)
      vars <- sapply(split_str_calls, `[[`, 2)
    }
    if (length(split_str_calls) > 0) {
      for (k in seq_along(pars)) {
        tmp_rhs <- deparse1(rhs(brmform[[2]][[pars[k]]]))
        if (!grepl(vars[k], tmp_rhs)) {
          rhs(brmform[[2]][[pars[k]]]) <- str2lang(paste0(tmp_rhs, " + (1 |",
                                                          vars[k], ")"))
        }
      }
    }
  }
  if (any(grepl("ogl(", split_random_call, fixed = TRUE))) {
    str_calls <- grep("ogl(", split_random_call, fixed = TRUE, value = TRUE)
    vars <- all.vars(str2lang(paste0(str_calls, collapse = " + ")))
    tmp <- paste0("ogl + ", deparse1(brmform[[1]][[3]]))
    brmform[[1]][[3]] <- str2lang(tmp)
    brmform[[2]]$ogl <- ogl ~ 1
    for (j in seq_along(vars)) {
      brmform[[2]]$ogl <- str2lang(paste0(deparse1(brmform[[2]]$ogl),
                                          " + (1 |", vars[j], ")"))
    }
  }
  brmform
}

#' @noRd
#' @importFrom stats terms
get_model_from_formula <- function(formula) {
  x_str <- grep("crf(", labels(terms(formula)), fixed = TRUE, value = TRUE)
  if (length(x_str) == 0) {
    stop("You must specify which non-linear function to use with crf")
  }
  x_str <- paste0(substr(x_str, 1, nchar(x_str) - 1), ", \"model\")")
  expand_model_set(eval(parse(text = x_str)))
}

#' @noRd
split_calls <- function(formula_part) {
  t_call <- 1
  t_var <- NA
  if (length(formula_part) >= 2) {
    tmp_ <- list()
    n <- 0
    while (length(formula_part) == 3) {
      n <- n + 1
      tmp_[[n]] <- formula_part[[3]]
      formula_part <- formula_part[[2]]
    }
    tmp_[[n + 1]] <- formula_part
    if (any(grepl("trials(", tmp_, fixed = TRUE))) {
      t_call <- tmp_[[grep("trials(", tmp_, fixed = TRUE)]]
      t_var <- all.vars(t_call)
    }
  }
  list(t_call = t_call, t_var = t_var)
}

#' @noRd
crf <- function(x, model, arg_to_retrieve = "x") {
  mf <- match.call(expand.dots = FALSE)
  if (arg_to_retrieve == "x") {
    m <- match("x", names(mf), 0L)
    deparse(substitute(a, list(a = mf[[m]])))
  } else if (arg_to_retrieve == "model") {
    m <- match("model", names(mf), 0L)
    eval(mf[[m]])
  } else {
    stop("arg_to_retrieve must be either \"x\" or \"model\".")
  }
}

#' @noRd
trials <- function(...) {
  identity(...)
}

#' Expose the final \code{\link[brms]{brmsformula}}
#'
#' Checks the input formula according to
#' \code{\link[bayesnec:bayesnec-package]{bayesnec}} requirements and
#' expose the final \code{\link[brms]{brmsformula}} which is to be fitted via
#' package \pkg{brms}.
#'
#' @param formula Either a \code{\link[base]{character}} string defining an
#' R formula or an actual \code{\link[stats]{formula}} object. See details.
#' @param data A \code{\link[base]{data.frame}} containing the variables
#' specified in \code{formula}.
#'
#' @importFrom stats model.frame
#' 
#' @return A named \code{\link[base]{list}}, with each element containing the
#' final \code{\link[brms]{brmsformula}} to be passed to
#' \code{\link[brms]{brm}}.
#' 
#' @seealso
#'   \code{\link{bayesnecformula}},
#'   \code{\link{check_formula}}
#'
#' @examples
#' library(bayesnec)
#' nec3param <- function(beta, nec, top, x) {
#'   top * exp(-exp(beta) * (x - nec) *
#'     ifelse(x - nec < 0, 0, 1))
#' }
#' 
#' data <- data.frame(x = seq(1, 20, length.out = 10), tr = 100, wght = c(1, 2),
#'                    group_1 = sample(c("a", "b"), 10, replace = TRUE),
#'                    group_2 = sample(c("c", "d"), 10, replace = TRUE))
#' data$y <- nec3param(beta = -0.2, nec = 4, top = 100, data$x)
#'
#' # make one single model
#' f_1 <- "y | trials(tr) ~ crf(sqrt(x), \"nec3param\")"
#' make_brmsformula(f_1, data)
#' # make an entire class of models
#' f_2 <- y ~ crf(x, "ecx") + ogl(group_1) + pgl(group_2)
#' make_brmsformula(f_2, data)
#'
#' @export
make_brmsformula <- function(formula, data) {
  formula <- bnf(formula)
  all_models <- get_model_from_formula(formula)
  out <- list()
  for (i in seq_along(all_models)) {
    formula_i <- single_model_formula(formula, all_models[i])
    bdat_i <- model.frame(formula_i, data = data, run_par_checks = FALSE)
    out[[i]] <- wrangle_model_formula(all_models[i], formula_i, bdat_i)
  }
  names(out) <- all_models
  out
}
