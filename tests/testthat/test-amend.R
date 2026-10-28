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
        prior = a$prior, init = a$init, family = a$family
      )
      structure(list(), class = "brmsfit")
    },
    are_chains_correct = function(...) TRUE,
    make_good_inits = function(model, x, y, family, ..., priors, chains) {
      seen$search[[length(seen$search) + 1]] <- list(
        model = model, x = x, y = y, priors = priors,
        tag = canonical_fam_tag(family), link = family$link
      )
      replicate(chains, list(b_top = as.array(1)), simplify = FALSE)
    },
    make_good_hurdle_inits = function(model, predictor, response, priors,
                                      chains, family, ...) {
      seen$search[[length(seen$search) + 1]] <- list(
        model = model, x = predictor, y = response, priors = priors,
        tag = canonical_fam_tag(family), link = family$link
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
