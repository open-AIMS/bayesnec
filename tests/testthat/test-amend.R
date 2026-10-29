test_that("input checks work correctly and return appropriate messages", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  general_error <- paste(
    "Nothing to amend, please specify a proper model to either add or drop, or",
    "changes to loo_controls;\n Returning original model set."
  )
  m_1 <- paste0("Nothing to amend, please specify a model to either add or",
                " drop that differs from the original set")
  m_2 <- "Returning original model set."
  m_3 <- paste0("No new LOO fitting/weighting arguments have been specified;",
                " ignoring argument loo_controls.")
  expect_message(amend(manec_example), general_error)
  amend(manec_example, drop = "nec3param") |>
    expect_message(general_error) |>
    expect_message(m_1)
  m_4 <- "loo_controls list names are incorrect. See ?bnec"
  manec_example |>
    amend(loo_controls = list(method = "somethingwrong")) |>
    expect_message(m_4) |>
    expect_error()
  expect_message(amend(manec_example, drop = "nec4param")) |>
    suppressWarnings()
  amend(manec_example, add = "nec3param") |>
    expect_message() |>
    expect_message("Fitted models are: nec4param ecx4param") |>
    suppressWarnings()
  amend(manec_example, add = "nec4param",
        loo_controls = list(weights = list(method = "pseudobma"))) |>
    expect_message(general_error) |>
    expect_message(m_1) |>
    expect_message(m_3)
  amend(manec_example,
        loo_controls = list(weights = list(method = "pseudobma"))) |>
    expect_message(general_error) |>
    expect_message(m_3)
})

test_that("amend.bayesnecfit rejects drop and no-ops cleanly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # Dropping the only model would have to return an empty object, so it is an
  # error rather than a silent no-op.
  expect_error(amend(nec4param, drop = "nec4param"),
               "Cannot drop models from a bayesnecfit")
  expect_error(amend(nec4param, drop = "ecx4param"),
               "Cannot drop models from a bayesnecfit")
  general_error <- paste(
    "Nothing to amend, please specify a proper model to either add or drop, or",
    "changes to loo_controls;\n Returning original model set."
  )
  expect_message(amend(nec4param), general_error)
  expect_identical(class(suppressMessages(amend(nec4param))),
                   c("bayesnecfit", "bnecfit"))
  expect_error(amend(nec4param, add = "ecx4param", timeout = -1),
               "must be a positive number")
})

test_that("amend.bayesnecfit adds models and promotes to bayesmanecfit", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # `manec_example` was fitted with iter = 200 and amend() inherits that, so the
  # added model occasionally fails to initialise -- it did once on ubuntu-devel
  # while passing on the other three platforms. Retry rather than assert on a
  # stochastic fit, and skip if this environment cannot fit it at all, so that a
  # failure to initialise reports as "could not fit" and not "amend is broken".
  added <- NULL
  for (attempt in 1:3) {
    set.seed(attempt * 101)
    added <- amend(nec4param, add = "ecx4param") |>
      suppressMessages() |>
      suppressWarnings()
    if (inherits(added, "bayesmanecfit")) {
      break
    }
  }
  # Whatever the sampler does, the call itself must work: this errored outright
  # before amend.bayesnecfit existed.
  expect_s3_class(added, "bnecfit")
  skip_if_not(inherits(added, "bayesmanecfit"),
              "ecx4param could not be initialised at manec_example's iter")
  expect_setequal(names(added$mod_fits), c("nec4param", "ecx4param"))
  expect_setequal(added$success_models, c("nec4param", "ecx4param"))
  # The pre-existing fit must be carried over rather than refitted; identical
  # draws is the check that matters, since the point of amend() is not paying
  # for the original fit twice.
  expect_identical(fixef(added$mod_fits$nec4param$fit),
                   fixef(nec4param$fit))
})

test_that("an object that records no weighting method gets the default", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # The shape of a set assembled by c() before #320, and of any set whose
  # attribute was dropped by row-subsetting of mod_stats. amend() preserves the
  # method the set was built with, and an unknown method used to be preserved
  # as NULL, which loo::loo_model_weights() resolves to stacking.
  unrecorded <- manec_example
  attr(unrecorded$mod_stats$wi, "method") <- NULL
  ctrl <- list(fitting = list(reloo = FALSE))
  out <- amend(unrecorded, loo_controls = ctrl) |>
    suppressMessages() |>
    suppressWarnings()
  expect_equal(attr(out$mod_stats$wi, "method"), "pseudobma")
  # A method named but NULL is not a request for a method. amend_model_set()
  # read the name alone, so this reweighted the set while
  # define_loo_controls() two calls later read the same value as unspecified.
  stacked <- amend(manec_example,
                   loo_controls = list(weights = list(method = "stacking"))) |>
    suppressMessages() |>
    suppressWarnings()
  kept <- amend(stacked,
                loo_controls = list(weights = list(method = NULL),
                                    fitting = list(reloo = FALSE))) |>
    suppressMessages() |>
    suppressWarnings()
  expect_equal(attr(kept$mod_stats$wi, "method"), "stacking")
})


test_that("amend refuses a resolution below 2", {
  # Both methods forward resolution to expand_nec(), where the no-effect
  # estimate of a smooth equation is read off the grid. Refused here so the
  # message names the argument rather than the grid. See #325.
  skip_on_cran()
  expect_error(amend(manec_example, drop = "nec4param", resolution = 1),
               "must be at least 2")
})

test_that("amend refuses stored data at a bound before fitting a model (#400)", {
  # The backstop for this route, which builds priors with define_prior() and
  # fits with skip_check = TRUE, so check_data() never runs on it. bnec()
  # refuses such a response, so the stored data are altered by hand here: the
  # packaged example is a gaussian fit, and it is given a beta family and a
  # response of 1 in every observation.
  m <- manec_example
  m$mod_fits[[1]]$fit$data$y <- 1
  m$mod_fits[[1]]$fit$family <- brms::Beta(link = "identity")
  calls <- 0L
  local_mocked_bindings(
    fit_bayesnec = function(...) {
      calls <<- calls + 1L
      stop("model fit should not start")
    },
    .package = "bayesnec"
  )
  expect_error(suppressMessages(amend(m, add = "nec3param")),
               "The response \"y\" is at the upper bound of a beta response",
               fixed = TRUE)
  expect_identical(calls, 0L)
})

test_that("amend lets ecxflat alone through at a bound, and names it (#419)", {
  # bnec() fits ecxflat alone to such a response, so a fit it made reaches this
  # backstop when an equation is added. A curve equation is refused, and the
  # message names ecxflat; adding ecxflat itself is let through to the fit.
  m <- manec_example
  m$mod_fits[[1]]$fit$data$y <- 1
  m$mod_fits[[1]]$fit$family <- brms::Beta(link = "identity")
  fitted <- character(0)
  local_mocked_bindings(
    fit_bayesnec = function(..., model) {
      fitted <<- c(fitted, model)
      stop("model fit reached")
    },
    .package = "bayesnec"
  )
  expect_error(suppressMessages(amend(m, add = "nec3param")),
               "is the only one that can be fitted to it", fixed = TRUE)
  expect_identical(fitted, character(0))
  res <- tryCatch(suppressWarnings(suppressMessages(amend(m, add = "ecxflat"))),
                  error = conditionMessage)
  expect_identical(fitted, "ecxflat")
  expect_false(grepl("upper bound", paste(res, collapse = " ")))
})

test_that("amend tests a zero_inflated_beta response at both bounds (#441)", {
  # D35 brings zero_inflated_beta into the rule beta follows. At 1 a curve
  # equation is refused and ecxflat let through; at 0 ecxflat is refused too,
  # because the beta block has no data. The stored data are altered by hand,
  # as in the beta test above.
  m <- manec_example
  m$mod_fits[[1]]$fit$data$y <- 1
  m$mod_fits[[1]]$fit$family <- brms::zero_inflated_beta(
    link = "identity", link_zi = "identity"
  )
  fitted <- character(0)
  local_mocked_bindings(
    fit_bayesnec = function(..., model) {
      fitted <<- c(fitted, model)
      stop("model fit reached")
    },
    .package = "bayesnec"
  )
  expect_error(suppressMessages(amend(m, add = "nec3param")),
               "upper bound of a zero_inflated_beta response", fixed = TRUE)
  res <- tryCatch(suppressWarnings(suppressMessages(amend(m, add = "ecxflat"))),
                  error = conditionMessage)
  expect_identical(fitted, "ecxflat")
  m$mod_fits[[1]]$fit$data$y <- 0
  expect_error(suppressMessages(amend(m, add = "ecxflat")),
               "so the beta block has no data", fixed = TRUE)
  expect_identical(fitted, "ecxflat")
})


# ---- #470, an added equation that keeps no group-level term ------------------

# manec_example's two equations given a term on bot, which both have. The term
# is written into each stored formula and plate into each stored data, which is
# what bnec() stores for such a fit. NB: the brmsfit components were not
# fitted with the term. amend() reads the formula and the data before it fits,
# which is what these tests assert, and nothing here reads a posterior
# quantity.
bot_plate_manec <- function() {
  m <- manec_example
  n <- nrow(m$mod_fits[[1]]$fit$data)
  for (eq in names(m$mod_fits)) {
    m$mod_fits[[eq]]$bayesnecformula <- bnf(stats::as.formula(paste0(
      "y ~ crf(x, model = \"", eq, "\") + (bot | plate)"
    )))
    m$mod_fits[[eq]]$fit$data$plate <- factor(rep(letters[1:4],
                                                  length.out = n))
  }
  attr(m, "bnec_record") <- list(
    requested = names(m$mod_fits), attempted = names(m$mod_fits),
    excluded = data.frame(model = character(), reason = character(),
                          stringsAsFactors = FALSE),
    substitutions = NULL
  )
  m
}

test_that("amend(add = ) excludes an added equation and records it (#470)", {
  # ecxexp has no bot, so it would keep no term on plate; ecxll4 has bot and
  # is fitted. The fit and the set assembly are replaced, so that what is
  # asserted is which equations reach the loop and what reaches the record.
  fitted <- character(0)
  seen <- NULL
  local_mocked_bindings(
    fit_bayesnec = function(..., model) {
      fitted <<- c(fitted, model)
      stop("model fit reached")
    },
    expand_manec = function(object, ...) {
      object[vapply(object, is_prebayesnecfit, logical(1))]
    },
    attach_bnec_record = function(out, requested, attempted, excluded,
                                  substitutions) {
      seen <<- list(requested = requested, attempted = attempted,
                    excluded = excluded)
      stop("record reached")
    },
    .package = "bayesnec"
  )
  expect_error(
    suppressWarnings(suppressMessages(
      amend(bot_plate_manec(), add = c("ecxexp", "ecxll4"))
    )),
    "record reached", fixed = TRUE
  )
  expect_identical(fitted, "ecxll4")
  expect_false("ecxexp" %in% seen$attempted)
  expect_identical(seen$excluded$model, "ecxexp")
  expect_match(seen$excluded$reason, "keeps no group-level term on \"plate\"",
               fixed = TRUE)
  expect_setequal(c(seen$attempted, seen$excluded$model), seen$requested)
})

test_that("amend(add = ) refuses a call adding only excluded ones (#470)", {
  calls <- 0L
  local_mocked_bindings(
    fit_bayesnec = function(...) {
      calls <<- calls + 1L
      stop("model fit reached")
    },
    .package = "bayesnec"
  )
  expect_error(suppressMessages(amend(bot_plate_manec(), add = "ecxexp")),
               "No equation requested can be fitted", fixed = TRUE)
  expect_identical(calls, 0L)
})


# ---- the priors of an added equation ---------------------------------------

# ecx4param with the family brms stores, a positive response and a formula
# holding `term`. The brmsfit was not fitted to these data, so read nothing
# from its posterior.
amend_fixture <- function(family, term = "",
                          data = data.frame(x = nec_data$x,
                                            y = nec_data$y * 10)) {
  fit <- ecx4param
  fit$fit$family <- brms::bf(y ~ x, family = unmark_family(family))$family
  fit$fit$data <- data
  fit$bayesnecformula <- bnf(stats::as.formula(
    paste0('y ~ crf(x, model = "ecx4param")', term)
  ))
  fit
}

# Records what each mocked brm() call is handed. The curve search returns a
# fixed start and records its inputs. The call then fails on the empty fit,
# which is not under test.
fit_inputs <- function(expr) {
  seen <- new.env(parent = emptyenv())
  seen$calls <- list()
  seen$search <- list()
  local_mocked_bindings(
    brm = function(formula, data, ...) {
      a <- list(...)
      seen$calls[[length(seen$calls) + 1]] <- list(
        prior = a$prior, init = a$init, family = a$family, formula = formula,
        control = a$control, seed = a$seed, backend = a$backend
      )
      structure(list(), class = "brmsfit")
    },
    are_chains_correct = function(...) TRUE,
    make_good_inits = function(model, x, y, family, ..., priors, chains) {
      seen$search[[length(seen$search) + 1]] <- list(
        model = model, x = x, y = y, priors = priors,
        tag = canonical_fam_tag(family), link = family$link,
        seed = list(...)$seed
      )
      replicate(chains, list(b_top = as.array(1)), simplify = FALSE)
    },
    make_good_hurdle_inits = function(model, predictor, response, priors,
                                      chains, family, ...) {
      seen$search[[length(seen$search) + 1]] <- list(
        model = model, x = predictor, y = response, priors = priors,
        tag = canonical_fam_tag(family), link = family$link,
        survival = list(...)$model_survival, seed = list(...)$seed
      )
      replicate(chains, list(b_top = as.array(1)), simplify = FALSE)
    },
    .package = "bayesnec"
  )
  warns <- msgs <- character()
  withCallingHandlers(
    try(expr, silent = TRUE),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  list(calls = seen$calls, search = seen$search, warnings = warns,
       messages = msgs)
}

expect_same_fit_inputs <- function(amended, fitted, label) {
  expect_length(amended$calls, 1)
  expect_length(fitted$calls, 1)
  expect_identical(amended$calls[[1]]$prior, fitted$calls[[1]]$prior,
                   label = label)
  expect_identical(amended$calls[[1]]$init, fitted$calls[[1]]$init,
                   label = label)
  expect_identical(amended$search, fitted$search, label = label)
  expect_false(any(grepl("has no entry for", amended$warnings)),
               label = label)
}

test_that("amend(add = ) on a Gamma fit builds the priors bnec() does (#436)", {
  # No disp() term and two variance functions, under both prior types.
  d <- data.frame(x = nec_data$x, y = nec_data$y * 10)
  for (term in c("", ' + disp("power")', ' + disp("loglinear")')) {
    for (pt in c("uninformative", "regularizing")) {
      label <- paste0("Gamma", term, ", ", pt)
      obj <- amend_fixture(validate_family("Gamma"), term)
      expect_identical(obj$fit$family$family, "gamma")
      amended <- fit_inputs(amend(obj, add = "nec4param", prior_type = pt))
      fitted <- fit_inputs(bnec(
        stats::as.formula(paste0('y ~ crf(x, "nec4param")', term)),
        data = d, family = Gamma(link = "identity"), chains = 2,
        prior_type = pt
      ))
      expect_same_fit_inputs(amended, fitted, label)
      # The family handed to brms is the one the fit stored, not a rebuilt one.
      expect_identical(amended$calls[[1]]$family, obj$fit$family,
                       label = label)
      if (nzchar(term)) {
        pr <- amended$calls[[1]]$prior
        expect_true(all(c("c0", "c1") %in% pr$nlpar), label = label)
        expect_true(all(c("b_c0", "b_c1") %in%
                          names(amended$calls[[1]]$init[[1]])), label = label)
      }
    }
  }
})

test_that("amend(add = ) passes the disp() term to the added prior (#436)", {
  # gaussian has no tag mismatch, so only the disp() term is under test.
  d <- data.frame(x = nec_data$x, y = nec_data$y * 10)
  for (pt in c("uninformative", "regularizing")) {
    obj <- amend_fixture(validate_family("gaussian"), ' + disp("power")')
    amended <- fit_inputs(amend(obj, add = "nec4param", prior_type = pt))
    fitted <- fit_inputs(bnec(y ~ crf(x, "nec4param") + disp("power"),
                              data = d, family = gaussian(), chains = 2,
                              prior_type = pt))
    expect_same_fit_inputs(amended, fitted, paste("gaussian", pt))
    c1 <- as.data.frame(amended$calls[[1]]$prior)
    c1 <- c1[c1$nlpar == "c1", ]
    expect_equal(as.numeric(c(c1$lb, c1$ub)), c(0, 1))
  }
})

test_that("amend(add = ) builds bnec()'s priors on a hurdle_gamma fit (#436)", {
  # The two-block branch of define_prior() builds its own disp() priors.
  d <- data.frame(x = nec_data$x, y = nec_data$y * 10)
  d$y[d$x > stats::quantile(d$x, 0.8)] <- 0
  for (pt in c("uninformative", "regularizing")) {
    obj <- amend_fixture(validate_family("hurdle_gamma"), ' + disp("power")',
                         data = d)
    amended <- fit_inputs(amend(obj, add = "nec4param", prior_type = pt))
    fitted <- fit_inputs(bnec(y ~ crf(x, "nec4param") + disp("power"),
                              data = d, family = "hurdle_gamma", chains = 2,
                              prior_type = pt))
    expect_same_fit_inputs(amended, fitted, paste("hurdle_gamma", pt))
    expect_true(all(c("c0", "c1") %in% amended$calls[[1]]$prior$nlpar))
  }
})

test_that("amend(add = ) keeps the truncation of a hurdle growth fit (#436)", {
  # Without truncation the regularizing c1 prior is centred on the replicate
  # slope, 0.29 on these counts, and with it on 0. So the comparison with
  # bnec() fails if amend() drops the truncation.
  set.seed(5)
  x <- rep(c(0.1, 0.5, 1, 2, 4), each = 20)
  mu <- 30 / (1 + (x / 1.5)^2) * rep(c(1, 2), length.out = length(x))
  y <- pmax(stats::rnbinom(length(x), mu = mu, size = 5), 1L)
  d <- data.frame(x = x, y = y)
  f <- add_hurdle_truncation(bnf(y ~ crf(x, "nec4param") + disp("power")))
  obj <- amend_fixture(validate_family("negbinomial"), ' + disp("power")',
                       data = d)
  obj$bayesnecformula <- add_hurdle_truncation(obj$bayesnecformula)
  amended <- fit_inputs(amend(obj, add = "nec4param",
                              prior_type = "regularizing"))
  fitted <- fit_inputs(bnec(f, data = d, family = "negbinomial", chains = 2,
                            prior_type = "regularizing"))
  expect_same_fit_inputs(amended, fitted, "truncated negbinomial")
  untruncated <- define_prior("nec4param", validate_family("negbinomial"),
                              d$x, d$y, prior_type = "regularizing",
                              disp_spec = parse_disp_term(f))
  expect_false(identical(amended$calls[[1]]$prior, untruncated))
})

test_that("amend(add = ) keeps the tested range of a hurdle growth fit (#483)", {
  # bnec_hurdle() records the whole series on the growth formula, here wider
  # than the data the component holds. An added equation is bounded by it, as
  # bnec() bounds one fitted from the same formula.
  d <- data.frame(x = nec_data$x, y = nec_data$y * 10)
  obj <- amend_fixture(validate_family("Gamma"), data = d)
  attr(obj$bayesnecformula, "bayesnec_tested_range") <- c(0, 10)
  f <- bnf(y ~ crf(x, "nec4param"))
  attr(f, "bayesnec_tested_range") <- c(0, 10)
  amended <- fit_inputs(amend(obj, add = "nec4param"))
  fitted <- fit_inputs(bnec(f, data = d, family = Gamma(link = "identity"),
                            chains = 2))
  expect_same_fit_inputs(amended, fitted, "tested range")
  pr <- as.data.frame(amended$calls[[1]]$prior)
  expect_equal(as.numeric(pr$ub[pr$nlpar == "nec"]), 10)
})

test_that("amend(add = ) drops what bnec() drops for a Gamma fit (#436)", {
  obj <- amend_fixture(validate_family("Gamma"))
  amended <- fit_inputs(amend(obj, add = c("neclin", "nec4param")))
  expect_length(amended$calls, 1)
  expect_identical(vapply(amended$search, `[[`, character(1), "model"),
                   "nec4param")
  expect_true(any(grepl(paste("Dropping the model neclin as they are not",
                              "valid in the case of a Gamma with identity",
                              "link"),
                        amended$messages, fixed = TRUE)))
  # No message names the stored tag. Match the word, so hurdle_gamma is allowed.
  expect_false(any(grepl("\\bgamma\\b", amended$messages, perl = TRUE)))
})


# ---- the call an added equation is fitted with (#489) -----------------------

# A positive response with zeros above the 80th percentile of x, for the
# two-block family tests.
hurdle_data <- function() {
  d <- data.frame(x = nec_data$x, y = nec_data$y * 10)
  d$y[d$x > stats::quantile(d$x, 0.8)] <- 0
  d
}

# The record fit_bayesnec() stores, with nothing supplied by default.
call_record <- function(model_survival = NULL, control = NULL, seed = NULL) {
  list(model_survival = model_survival, control = control, seed = seed)
}

hu_rhs <- function(model) {
  deparse1(hu_block_formula(model, "x", "hu")[[3]])
}

test_that("amend(add = ) fits the survival-block equation the set records", {
  d <- hurdle_data()
  obj <- amend_fixture(validate_family("hurdle_gamma"), data = d)
  obj$call_args <- call_record(model_survival = "nec3param")
  amended <- fit_inputs(amend(obj, add = "nec4param"))
  fitted <- fit_inputs(bnec(y ~ crf(x, "nec4param"), data = d,
                            family = "hurdle_gamma", chains = 2,
                            model_survival = "nec3param"))
  expect_same_fit_inputs(amended, fitted, "model_survival = nec3param")
  expect_equal(amended$calls[[1]]$formula, fitted$calls[[1]]$formula,
               ignore_formula_env = TRUE)
  expect_identical(deparse1(amended$calls[[1]]$formula$pforms$hu[[3]]),
                   hu_rhs("nec3param"))
  expect_identical(amended$search[[1]]$survival, "nec3param")
  # nec3param has no bot, so neither has the survival block's prior.
  hu_nlpars <- unique(grep("^hu", amended$calls[[1]]$prior$nlpar,
                           value = TRUE))
  expect_setequal(hu_nlpars, c("hutop", "hubeta", "hunec"))
})

test_that("amend(add = ) gives the survival block its own equation otherwise", {
  d <- hurdle_data()
  obj <- amend_fixture(validate_family("hurdle_gamma"), data = d)
  obj$call_args <- call_record()
  amended <- fit_inputs(amend(obj, add = "nec4param"))
  fitted <- fit_inputs(bnec(y ~ crf(x, "nec4param"), data = d,
                            family = "hurdle_gamma", chains = 2))
  expect_same_fit_inputs(amended, fitted, "no model_survival")
  expect_identical(deparse1(amended$calls[[1]]$formula$pforms$hu[[3]]),
                   hu_rhs("nec4param"))
  expect_null(amended$search[[1]]$survival)
})

test_that("a set that records no call has its survival block read", {
  d <- hurdle_data()
  fam <- validate_family("hurdle_gamma")
  f <- bnf(y ~ crf(x, "ecx4param"))
  bdat <- model.frame(f, data = d)
  stored_with <- function(model, survival, form = f) {
    list(fit = list(family = fam,
                    formula = wrangle_model_formula(
                      model, single_model_formula(form, model),
                      model.frame(form, data = d), fam,
                      model_survival = survival
                    )))
  }
  # Two equations sharing one survival block were fitted with it named.
  shared <- list(ecx4param = stored_with("ecx4param", "nec3param"),
                 nec4param = stored_with("nec4param", "nec3param"))
  expect_identical(stored_model_survival(shared, bdat), "nec3param")
  # So were two equations where one of them is the survival equation.
  with_own <- list(nec3param = stored_with("nec3param", "nec3param"),
                   nec4param = stored_with("nec4param", "nec3param"))
  expect_identical(stored_model_survival(with_own, bdat), "nec3param")
  # Each equation with its own block was fitted without it.
  own <- list(ecx4param = stored_with("ecx4param", NULL),
              nec4param = stored_with("nec4param", NULL))
  expect_null(stored_model_survival(own, bdat))
  # One equation with another equation's block was fitted with it named.
  one <- list(ecx4param = stored_with("ecx4param", "nec3param"))
  expect_identical(stored_model_survival(one, bdat), "nec3param")
  # ecxflat is read as a survival equation, since it is in "all".
  flat <- list(ecx4param = stored_with("ecx4param", "ecxflat"),
               nec4param = stored_with("nec4param", "ecxflat"))
  expect_identical(stored_model_survival(flat, bdat), "ecxflat")
  # One equation with its own block is read as fitted without it.
  alone <- list(ecx4param = stored_with("ecx4param", NULL))
  expect_null(stored_model_survival(alone, bdat))
  # The block is matched on the predictor column the formula names.
  f_log <- bnf(y ~ crf(log(x), "ecx4param"))
  logged <- list(ecx4param = stored_with("ecx4param", "nec3param", f_log))
  expect_identical(
    stored_model_survival(logged, model.frame(f_log, data = d)), "nec3param"
  )
  # A single-block family has no survival block to read.
  gaussian_set <- list(ecx4param = list(fit = ecx4param$fit))
  expect_null(stored_model_survival(gaussian_set, bdat))
  # amend() uses what it reads, as for a set that records model_survival.
  obj <- amend_fixture(fam, data = d)
  obj$fit$formula <- one$ecx4param$fit$formula
  expect_null(obj$call_args)
  amended <- fit_inputs(amend(obj, add = "nec4param"))
  expect_identical(deparse1(amended$calls[[1]]$formula$pforms$hu[[3]]),
                   hu_rhs("nec3param"))
})

test_that("amend(add = ) samples with the control, seed and backend recorded", {
  d <- data.frame(x = nec_data$x, y = nec_data$y * 10)
  ctrl <- list(adapt_delta = 0.95, max_treedepth = 12)
  obj <- amend_fixture(validate_family("gaussian"))
  obj$call_args <- call_record(control = ctrl, seed = 7)
  obj$fit$backend <- "cmdstanr"
  amended <- fit_inputs(amend(obj, add = "nec4param"))
  fitted <- fit_inputs(bnec(y ~ crf(x, "nec4param"), data = d,
                            family = gaussian(), chains = 2, control = ctrl,
                            seed = 7, backend = "cmdstanr"))
  expect_same_fit_inputs(amended, fitted, "control, seed and backend")
  for (arg in c("control", "seed", "backend")) {
    expect_identical(amended$calls[[1]][[arg]], fitted$calls[[1]][[arg]],
                     label = arg)
  }
  expect_identical(amended$calls[[1]]$control, ctrl)
  expect_identical(amended$calls[[1]]$seed, 7)
  expect_identical(amended$calls[[1]]$backend, "cmdstanr")
  # The initial-value search is seeded too, as in bnec().
  expect_identical(amended$search[[1]]$seed, 7)
  # Where the fit records no call, amend() passes the backend, which brms
  # stores, and no control or seed.
  obj$call_args <- NULL
  unrecorded <- fit_inputs(amend(obj, add = "nec4param"))
  expect_identical(unrecorded$calls[[1]]$backend, "cmdstanr")
  expect_null(unrecorded$calls[[1]]$control)
  expect_null(unrecorded$calls[[1]]$seed)
})

test_that("amend(add = ) keeps the adapt_delta raise an added equation needs", {
  # nechorme's mean can leave a positive support through its slope, so a
  # group-level term on a Gamma fit raises adapt_delta; ecx4param needs no
  # raise, so the set's own fit recorded no control.
  d <- data.frame(x = nec_data$x, y = nec_data$y * 10,
                  g = factor(rep(letters[1:4], length.out = nrow(nec_data))))
  obj <- amend_fixture(validate_family("Gamma"), " + ogl(g)", data = d)
  raised <- function(record) {
    obj$call_args <- record
    amended <- fit_inputs(amend(obj, add = "nechorme"))
    expect_length(amended$calls, 1)
    amended$calls[[1]]$control
  }
  fitted <- fit_inputs(bnec(y ~ crf(x, "nechorme") + ogl(g), data = d,
                            family = Gamma(link = "identity"), chains = 2))
  expect_identical(fitted$calls[[1]]$control$adapt_delta, 0.99)
  expect_identical(raised(call_record())$adapt_delta, 0.99)
  # A control without adapt_delta is kept, and the raise added to it.
  expect_identical(raised(call_record(control = list(max_treedepth = 12))),
                   list(max_treedepth = 12, adapt_delta = 0.99))
  # An adapt_delta the call set is kept, as bnec() keeps it.
  expect_identical(raised(call_record(control = list(adapt_delta = 0.9))),
                   list(adapt_delta = 0.9))
})

test_that("fit_bayesnec() records the call before adapt_delta is raised", {
  d <- data.frame(x = nec_data$x, y = nec_data$y * 10,
                  g = factor(rep(letters[1:4], length.out = nrow(nec_data))))
  local_mocked_bindings(
    brm = function(...) structure(list(), class = "brmsfit"),
    are_chains_correct = function(...) TRUE,
    make_good_inits = function(..., chains) {
      replicate(chains, list(b_top = as.array(1)), simplify = FALSE)
    },
    .package = "bayesnec"
  )
  out <- suppressMessages(fit_bayesnec(
    bnf(y ~ crf(x, "nechorme") + ogl(g)), data = d, model = "nechorme",
    brm_args = list(family = validate_family("Gamma"), chains = 2,
                    control = list(max_treedepth = 12), seed = 3),
    skip_check = TRUE
  ))
  expect_identical(out$call_args,
                   call_record(control = list(max_treedepth = 12), seed = 3))
})

test_that("the record of the call is recovered and kept up to date", {
  fit <- ecx4param
  fit$call_args <- call_record(control = list(max_treedepth = 12), seed = 7)
  expect_identical(recover_prebayesnecfit(fit)[[1]]$call_args, fit$call_args)
  expect_null(recover_prebayesnecfit(ecx4param)[[1]]$call_args)
  # update() passes control and seed to brms::update(), which merges control.
  rec <- update_call_args(fit$call_args,
                          list(control = list(adapt_delta = 0.95), seed = 1))
  expect_identical(rec, call_record(
    control = list(max_treedepth = 12, adapt_delta = 0.95), seed = 1
  ))
  expect_identical(update_call_args(fit$call_args, list(iter = 400)),
                   fit$call_args)
  expect_identical(update_call_args(call_record(),
                                    list(control = list(adapt_delta = 0.95))),
                   call_record(control = list(adapt_delta = 0.95)))
  expect_null(update_call_args(NULL, list(seed = 1)))
})

test_that("update records supplied settings on a fit saved without a call", {
  local_mocked_bindings(
    update = function(object, ...) object,
    expand_nec = function(object, ...) object,
    .package = "bayesnec"
  )
  obj <- amend_fixture(validate_family("gaussian"))
  ctrl <- list(adapt_delta = 0.97, max_treedepth = 12)
  updated <- update.bnecfit(obj, control = ctrl, seed = 19)
  expect_identical(updated$call_args, call_record(control = ctrl, seed = 19))
  amended <- fit_inputs(amend(updated, add = "nec4param"))
  expect_length(amended$calls, 1)
  expect_identical(amended$calls[[1]]$control, ctrl)
  expect_identical(amended$calls[[1]]$seed, 19)

  unchanged <- update.bnecfit(obj)
  expect_identical(unchanged$call_args, call_record())
  reset <- update.bnecfit(updated, seed = NULL)
  expect_null(reset$call_args$seed)
  expect_identical(reset$call_args$control, ctrl)
})

test_that("update recovers a legacy set's shared survival equation", {
  local_mocked_bindings(
    update = function(object, ...) object,
    expand_manec = function(object, ...) {
      list(mod_fits = object, success_models = names(object))
    },
    .package = "bayesnec"
  )
  d <- hurdle_data()
  fam <- validate_family("hurdle_gamma")
  obj <- manec_example
  for (model in names(obj$mod_fits)) {
    fit <- obj$mod_fits[[model]]
    fit$fit$data <- d
    fit$fit$family <- unmark_family(fam)
    fit$fit$formula <- wrangle_model_formula(
      model, fit$bayesnecformula, model.frame(fit$bayesnecformula, data = d),
      fam, model_survival = "nec4param"
    )
    obj$mod_fits[[model]] <- fit
  }
  updated <- update.bnecfit(obj, seed = 19)
  for (fit in updated$mod_fits) {
    expect_identical(fit$call_args,
                     call_record(model_survival = "nec4param", seed = 19))
  }
  # The matching response equation alone cannot identify a fixed block.
  survivor <- updated$mod_fits$nec4param
  class(survivor) <- c("bayesnecfit", "bnecfit")
  amended <- fit_inputs(amend(survivor, add = "nec3param"))
  expect_length(amended$calls, 1)
  expect_identical(deparse1(amended$calls[[1]]$formula$pforms$hu[[3]]),
                   hu_rhs("nec4param"))
  expect_identical(amended$calls[[1]]$seed, 19)
})

test_that("amend preserves a legacy survival equation after dropping models", {
  local_mocked_bindings(
    expand_manec = function(object, ...) object,
    expand_nec = function(object, ...) object,
    .package = "bayesnec"
  )
  d <- hurdle_data()
  fam <- validate_family("hurdle_gamma")
  obj <- manec_example
  for (model in names(obj$mod_fits)) {
    fit <- obj$mod_fits[[model]]
    fit$fit$data <- d
    fit$fit$family <- unmark_family(fam)
    fit$fit$formula <- wrangle_model_formula(
      model, fit$bayesnecformula, model.frame(fit$bayesnecformula, data = d),
      fam, model_survival = "nec4param"
    )
    obj$mod_fits[[model]] <- fit
  }
  reduced <- amend(obj, drop = "ecx4param")
  expect_s3_class(reduced, "bayesnecfit")
  expect_identical(reduced$call_args,
                   call_record(model_survival = "nec4param"))
  amended <- fit_inputs(amend(reduced, add = "nec3param"))
  expect_length(amended$calls, 1)
  expect_identical(deparse1(amended$calls[[1]]$formula$pforms$hu[[3]]),
                   hu_rhs("nec4param"))
})

test_that("update records only settings brms reuses after backend or algorithm changes", {
  obj <- ecx4param
  obj$call_args <- call_record(
    control = list(adapt_delta = 0.99, max_treedepth = 12), seed = 7
  )
  calls <- list()
  local_mocked_bindings(
    brm = function(...) {
      args <- list(...)
      calls[[length(calls) + 1]] <<- args
      out <- obj$fit
      out$backend <- args$backend
      out$algorithm <- args$algorithm
      out
    },
    .package = "brms"
  )
  local_mocked_bindings(
    expand_nec = function(object, ...) object,
    .package = "bayesnec"
  )
  for (changes in list(list(backend = "cmdstanr", recompile = TRUE),
                       list(backend = "cmdstanr", recompile = FALSE),
                       list(algorithm = "meanfield", recompile = TRUE))) {
    updated <- do.call(stats::update, c(list(object = obj), changes))
    received <- calls[[length(calls)]]
    expect_null(received$control)
    expect_null(received$seed)
    expect_identical(updated$call_args, call_record())
    added <- amend_call_args(recover_prebayesnecfit(updated), NULL)$brm_args
    expect_null(added$control)
    expect_null(added$seed)

    ctrl <- list(adapt_delta = 0.95)
    updated <- do.call(stats::update, c(
      list(object = obj, control = ctrl, seed = 29), changes
    ))
    received <- calls[[length(calls)]]
    expect_identical(received$control, ctrl)
    expect_identical(received$seed, 29)
    expect_identical(updated$call_args, call_record(control = ctrl, seed = 29))
  }
  for (changes in list(list(backend = "r"), list(algorithm = "s"),
                       list(backend = NULL, algorithm = NULL))) {
    updated <- do.call(stats::update, c(
      list(object = obj, recompile = TRUE), changes
    ))
    expect_identical(updated$call_args, obj$call_args)
  }
  legacy <- obj
  legacy$fit$backend <- NULL
  legacy$fit$version$brms <- package_version("2.13.1")
  legacy$fit$version$restructure <- NULL
  updated <- stats::update(legacy, recompile = TRUE)
  expect_identical(updated$call_args, obj$call_args)
})
