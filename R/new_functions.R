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

trials <- function(...) {
  identity(...)
}

#' @noRd
formula_checks <- function(formula) {
  # make sure that whatever's on right hand side is either x or random variable!
  # make sure all variables actually exist in the dataframe!
  # check for class of group-level effects
  x_str <- grep("crf(", labels(terms(formula)), fixed = TRUE, value = TRUE)
  if (length(x_str) == 0) {
    stop("You must specify which non-linear function to use with crf.",
         " See ?bnec")
  }
  formula
}

#' @importFrom stats as.formula
bayesnecformula <- function(formula, ...) {
  if (is.character(formula)) {
    formula <- as.formula(formula)
  }
  formula <- formula_checks(formula)
  allot_class(formula, c("formula", "bayesnecformula"))
}

#' @importFrom stats model.frame na.omit
model.frame.bayesnecformula <- function(formula, ...) {
  dot_args <- list(...)
  if (!"data" %in% names(dot_args) && !inherits(dot_args[[1]], "data.frame")) {
    stop("Argument data is missing.")
  }
  pre_list <- simplify_formula(formula, dot_args[[1]])
  data <- model.frame(pre_list$formula, ...)
  bnec_pop <- na.omit(pre_list$pop_vars)
  names(bnec_pop) <- c("y_var", "x_var", "trials_var")[seq_along(bnec_pop)]
  attr(data, "bnec_pop") <- bnec_pop
  bnec_group <- pre_list$group_vars
  attr(data, "bnec_group") <- bnec_group
  data
}

#' @importFrom formula.tools lhs rhs
#' @importFrom stats as.formula terms model.frame
simplify_formula <- function(formula, data) {
  formula <- formula_checks(formula)
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
expand_model_set <- function(model) {
  msets <- names(mod_groups)
  if (any(model %in% msets)) {
    group_mods <- intersect(model, msets)
    model <- union(model, unname(unlist(mod_groups[group_mods])))
    model <- setdiff(model, msets)
  }
  model
}

#' @noRd
retrieve_valid_family <- function(named_list, data) {
  if (!"family" %in% names(named_list)) {
    y <- retrieve_var(data, "y_var", error = TRUE)
    tr <- retrieve_var(data, "trials_var")
    family <- set_distribution(y, support_integer = TRUE, trials = tr)
  } else {
    family <- named_list$family
  }
  validate_family(family)
}

#' @noRd
define_loo_controls <- function(loo_controls, family_str) {
  if (missing(loo_controls)) {
    loo_controls <- list(fitting = list(), weights = list(method = "pseudobma"))
  } else {
    loo_controls <- validate_loo_controls(loo_controls, family_str)
    if (!"method" %in% names(loo_controls$weights)) {
      loo_controls$weights$method <- "pseudobma"
    }
  }
  loo_controls
}

#' @noRd
retrieve_var <- function(data, var, error = FALSE) {
  bnec_vars <- attr(data, "bnec_pop")
  bnec_pop <- names(bnec_vars)
  v_pos <- which(bnec_pop == var)
  out <- try(data[[v_pos]], silent = TRUE)
  if (inherits(out, "try-error")) {
    if (error) {
      stop("The input variable \"", gsub("_var", "", var),
           "\" was not properly specified in formula. See ?bayesnecformula")
    }
    NULL
  } else if (is.numeric(out)) {
    out
  } else {
    stop("The input variable \"", gsub("_var", "", var),
         " is not numeric.")
  }
}

#' @importFrom brms bf
#' @importFrom stats update as.formula model.frame
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
  allot_class(formula, c("formula", "bayesnecformula"))
}

#' @noRd
clean_bar_glef <- function(x) {
  x <- strsplit(x, split = " | ", fixed = TRUE)[[1]]
  gsub("\\(|\\)", "", x)
}

#' @noRd
#' @importFrom formula.tools rhs
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
        if (!vars[j] %in% names(data)) {
          message("Group-level variable(s) ",
                  paste0("\"", vars[j], "\"", collapse = "; "),
                  " not found in dataset. Ignoring...")
        } else {
          brmform[[2]][[i]] <- str2lang(paste0(deparse1(brmform[[2]][[i]]),
                                               " + (1 |", vars[j], ")"))
        }
      }
    }
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
    if (!all(vars %in% names(data))) {
      to_flag <- vars[!vars %in% names(data)]
      stop("Group-level variable(s) ",
           paste0("\"", to_flag, "\"", collapse = "; "),
           " not found in dataset.")
    }
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
add_brm_defaults <- function(brm_args, model, family, predictor, response,
                             skip_check, custom_name) {
  if (!("chains" %in% names(brm_args))) {
    brm_args$chains <- 4
  }
  if (!("sample_prior" %in% names(brm_args))) {
    brm_args$sample_prior <- "yes"
  }
  if (!("iter" %in% names(brm_args))) {
    brm_args$iter <- 1e4
  }
  if (!("warmup" %in% names(brm_args))) {
    brm_args$warmup <- floor(brm_args$iter / 5) * 4
  }
  priors <- try(validate_priors(brm_args$prior, model), silent = TRUE)
  if (inherits(priors, "try-error")) {
    brm_args$prior <- define_prior(model, family, predictor, response)
  }
  if (!("inits" %in% names(brm_args)) || skip_check) {
    msg_tag <- ifelse(family$family == "custom", custom_name, family$family)
    message(paste0("Finding initial values which allow the response to be",
                   " fitted using a ", model, " model and a ", msg_tag,
                   " distribution."))
    response_link <- response_link_scale(response, family)
    inits <- make_good_inits(model, predictor, response_link,
                             priors = brm_args$prior, chains = brm_args$chains)
    if (length(inits) == 1 && "random" %in% names(inits)) {
      inits <- inits$random
    }
    brm_args$inits <- inits
  }
  brm_args
}
