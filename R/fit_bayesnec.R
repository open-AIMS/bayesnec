#' fit_bayesnec
#'
#' Fits a concentration(dose)-response model using brms
#'
#' @inheritParams bnec
#'
#' @param skip_check Should data check via \code{\link{check_data}}
#' be avoided? Only relevant to function \code{\link{amend}}.
#' Defaults to FALSE.
#'
#' @importFrom brms brm bf
#' @importFrom stats update as.formula
#'
#' @seealso \code{\link{bnec}}
#' @return An object of class \code{\link{prebayesnecfit}}.
fit_bayesnec <- function(data, x_var, y_var, trials_var = NA, family = NULL,
                         priors, model = NA, inits, skip_check = FALSE,
                         random = NA, random_vars = NA, weights = NA, ...) {
  if (skip_check) {
    mod_dat <- data
    custom_name <- check_custom_name(family)
    if (family$family == "binomial" || custom_name == "beta_binomial2") {
      response <- data$y / data$trials
    } else {
      response <- data$y
    }
    priors <- try(validate_priors(priors, model), silent = TRUE)
    if (inherits(priors, "try-error")) {
      priors <- define_prior(model = model, family = family, predictor = data$x,
                             response = response)
    }
    if ("weights" %in% names(data)) {
      weights <- "weights"
    }
  } else {
    data_check <- check_data(data = data, x_var = x_var, y_var = y_var,
                             trials_var = trials_var, family = family,
                             model = model, random_vars = random_vars,
                             weights = weights)
    mod_dat <- data_check$mod_dat
    family <- data_check$family
    priors <- try(validate_priors(priors, model), silent = TRUE)
    if (inherits(priors, "try-error")) {
      priors <- data_check$priors
    }
    custom_name <- check_custom_name(family)
    if (family$family == "binomial" || custom_name == "beta_binomial2") {
      response <- mod_dat$y / mod_dat$trials
    } else {
      response <- mod_dat$y
    }
  }
  brms_bf <- get(paste0("bf_", model))
  if (family$family == "binomial" || custom_name == "beta_binomial2") {
    brms_bf <- modify_formula(brms_bf, "|", "trials(trials)")
  }
  if (!is.na(weights)) {
    if (family$family == "binomial" || custom_name == "beta_binomial2") {
      symb <- "+"
    } else {
      symb <- "|"
    }
    brms_bf <- modify_formula(brms_bf, symb, "weights(weights)")
  }
  add_args <- list(...)
  if (!("chains" %in% names(add_args))) {
    add_args[["chains"]] <- 4
  }
  if (!("sample_prior" %in% names(add_args))) {
    add_args[["sample_prior"]] <- "yes"
  }
  chs <- add_args$chains
  if (family$family == "custom") {
    msg_tag <- custom_name
  } else {
    msg_tag <- family$family
  }
  message(paste0("Finding initial values which allow the response to be",
                 " fitted using a ", model, " model and a ", msg_tag,
                 " distribution."))
  if (missing(inits) | skip_check) {
    response_link <- response_link_scale(response, family)
    inits <- make_good_inits(model, mod_dat$x, response_link, priors = priors,
                             chains = chs)
    if (length(inits) == 1 && "random" %in% names(inits)) {
      inits <- inits$random
    }
  }
  if (!is.na(random[1])) {
    form_text <- as.character(brms_bf)
    rand_forms <- eval(parse(text = form_text[2]))
    nl_form <- form_text[1]
    rand_list <- intersect(names(random), names(rand_forms))
    add_ost <- max(grepl("ost", names(random))) == 1
    if (length(rand_list) > 0) {
      rand_forms[rand_list] <- random[rand_list]
    }
    if (add_ost) {
      rand_forms <- c(rand_forms, list(ost = random$ost))
      nl_form <- gsub("~", "~ ost + ", nl_form, fixed = TRUE)
    }
    brms_bf <- bf(as.formula(nl_form), rand_forms, nl = TRUE)
  }
  all_args <- c(list(formula = brms_bf, data = mod_dat, family = family,
                     prior = priors, inits = inits), add_args)
  if (custom_name == "beta_binomial2") {
    all_args <- c(list(stanvars = stanvars), all_args)
  }
  fit <- do.call(brm, all_args)
  pass <- are_chains_correct(fit, chs)
  if (!pass) {
    stop(paste0("Failed to fit model ", model, "."), call. = FALSE)
  }
  message(paste0("Response variable modelled as a ", model, " model using a ",
                 msg_tag, " distribution."))
  out <- list(fit = fit, model = model, inits = inits)
  allot_class(out, "prebayesnecfit")
}
