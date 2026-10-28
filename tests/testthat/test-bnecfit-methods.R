# Inspect family validation and refit arguments without sampling. Stubs
# halt update() at the brms entry point.

capture_brms_update <- function(expr) {
  # Replace and restore update.brmsfit in stats' S3 method table, where
  # dispatch finds it. Restoring only the brms binding can leave a traced
  # method active; use an option to capture values across environments.
  old <- getOption("bayesnec_test_capture")
  on.exit(options(bayesnec_test_capture = old), add = TRUE)
  options(bayesnec_test_capture = NULL)
  tbl <- get(".__S3MethodsTable__.", envir = asNamespace("stats"))
  orig_method <- get("update.brmsfit", envir = tbl)
  stub <- function(object, formula. = NULL, newdata = NULL, recompile = NULL,
                   ...) {
    options(bayesnec_test_capture = list(
      family = list(...)$family,
      newdata_expr = deparse(substitute(newdata)),
      newdata_null = is.null(newdata),
      newdata_val = newdata
    ))
    stop("halted by test")
  }
  assign("update.brmsfit", stub, envir = tbl)
  on.exit(assign("update.brmsfit", orig_method, envir = tbl), add = TRUE)
  # try(silent = FALSE) in the refit loop prints the stub's stop to stderr.
  invisible(capture.output(
    ignored <- tryCatch(suppressMessages(expr), error = function(e) NULL),
    type = "message"
  ))
  getOption("bayesnec_test_capture")
}

test_that("family_signature compares the tag and the links, not the object", {
  # Compare canonical family/link signatures across stats and brms objects.
  # Missing auxiliary links take the brms defaults, including log for sigma.
  expect_equal(bayesnec:::family_signature(gaussian()),
               c(family = "gaussian", link = "identity", link_sigma = "log"))
  expect_identical(
    bayesnec:::family_signature(validate_family("gaussian")),
    bayesnec:::family_signature(manec_example$mod_fits[[1]]$fit$family)
  )
  # Every link is read, and in a stable order.
  sig <- bayesnec:::family_signature(validate_family("Beta"))
  expect_equal(sig[["family"]], "beta")
  expect_true(all(c("link", "link_phi") %in% names(sig)))
  # A function-valued component is not a link.
  expect_false(any(c("linkfun", "linkinv") %in%
                     names(bayesnec:::family_signature(gaussian()))))
})

test_that("check_update_data reads the family it is given", {
  o <- recover_prebayesnecfit(manec_example)
  dat <- o[[1]]$fit$data
  # A supplied family must reach the family-change check when passed
  # positionally; inferring a family from data would test the wrong choice.
  expect_false(bayesnec:::check_update_data(o, dat, validate_family("gaussian"))$changed_family)
  expect_true(bayesnec:::check_update_data(o, dat, validate_family("Beta"))$changed_family)
})

test_that("family_signature names a stored Gamma family as bayesnec does", {
  # Canonicalise the stored lowercase Gamma tag and its auxiliary-link
  # defaults before comparison with a newly supplied family.
  stored <- brms::brmsfamily("Gamma", link = "identity")
  expect_identical(
    bayesnec:::family_signature(validate_family(Gamma(link = "identity"))),
    bayesnec:::family_signature(stored)
  )
  expect_identical(
    bayesnec:::family_signature(validate_family("hurdle_gamma")),
    bayesnec:::family_signature(
      brms::hurdle_gamma(link = "identity", link_hu = "identity")
    )
  )
  # A link that is recorded and differs is still a difference.
  expect_false(identical(
    bayesnec:::family_signature(validate_family(Beta(link_phi = "identity"))),
    bayesnec:::family_signature(validate_family("Beta"))
  ))
  expect_false(identical(
    bayesnec:::family_signature(validate_family(Gamma(link = "log"),
                                                link_source = "link")),
    bayesnec:::family_signature(stored)
  ))
})

test_that("check_update_data reads no family off new data (D32)", {
  # New data alone keep the fitted family, even when their range would lead
  # to a different automatically selected family.
  o <- recover_prebayesnecfit(manec_example)
  expect_false(bayesnec:::check_update_data(o, o[[1]]$fit$data)$changed_family)
  expect_false(bayesnec:::check_update_data(o, nec_data)$changed_family)
})

test_that("update refuses an unsupported link before refitting", {
  expect_error(update(manec_example, family = Beta(link = "probit")),
               "bayesnec fits on the")
  expect_error(update(manec_example, family = Gamma(link = "inverse")),
               "bayesnec fits on the")
  expect_error(update(manec_example, family = "notafamily"),
               "not currently implemented")
})

test_that("update stops on a family change unless force_fit is set", {
  expect_error(update(manec_example, family = Beta()), "force_fit = TRUE")
  # A supplied change of family still requires force_fit with new data.
  expect_error(update(manec_example, newdata = nec_data, family = Beta()),
               "force_fit = TRUE")
})

test_that("brms receives the validated family, not the one written", {
  got <- capture_brms_update(
    update(manec_example, family = Beta(), force_fit = TRUE)
  )
  # An unspecified mean link defaults to identity. top and bot then describe
  # the response scale; nec remains on the predictor scale.
  expect_equal(got$family$family, "beta")
  expect_equal(got$family$link, "identity")
  # The idempotence marker is bayesnec's own and must not be serialised into
  # the fit.
  expect_null(attr(got$family, "bayesnec_validated"))
  # brms deparses this argument's expression into the data_name it prints, so
  # it has to stay a symbol rather than be inlined as a data frame.
  expect_equal(got$newdata_expr, "newdata")
})

test_that("update(family =) hands brms the corrected frame (#274)", {
  # When a supplied family requires boundary shifts, pass the corrected
  # stored data to brms even if the caller supplied no newdata.
  fit <- manec_example
  for (i in seq_along(fit$mod_fits)) {
    d <- fit$mod_fits[[i]]$fit$data
    # Declining, and reaching zero at the top of the range: a response bnec
    # accepts, and the shape in which a boundary zero actually arises.
    d$y <- seq(0.9, 0.05, length.out = nrow(d))
    d$y[(nrow(d) - 2):nrow(d)] <- 0
    fit$mod_fits[[i]]$fit$data <- d
  }
  kept <- seq_len(nrow(fit$mod_fits[[1]]$fit$data) - 3)
  got <- capture_brms_update(update(fit, family = Beta(), force_fit = TRUE))
  expect_false(got$newdata_null)
  # The three zeros the message reports are shifted in the frame brms is given.
  expect_equal(sum(got$newdata_val$y == 0), 0)
  expect_true(all(got$newdata_val$y > 0))
  expect_equal(got$newdata_val$y[kept], fit$mod_fits[[1]]$fit$data$y[kept])
  # Still a symbol, so brms deparses the data_name it prints as before.
  expect_equal(got$newdata_expr, "newdata")
})

test_that("update(family =) leaves newdata NULL where nothing was corrected", {
  # NULL is what tells brms to reuse the stored data rather than treat it as
  # new, so the substitution above must not be made unconditionally.
  got <- capture_brms_update(
    update(manec_example, family = Beta(), force_fit = TRUE)
  )
  expect_true(got$newdata_null)
})

test_that("a link the caller writes is honoured on update", {
  got <- capture_brms_update(
    update(manec_example, family = Beta(link = "logit"), force_fit = TRUE)
  )
  expect_equal(got$family$link, "logit")
  # Positional too: link is the first argument of every family constructor.
  got <- capture_brms_update(
    update(manec_example, family = Beta("logit"), force_fit = TRUE)
  )
  expect_equal(got$family$link, "logit")
})

# These structural fixtures relabel a stored Gaussian fit and replace its
# response. Stop update() at the brms entry point: the stored posterior
# does not describe the new family or data.

gamma_fixture <- function() {
  f <- nec4param
  f$fit$family <- brms::brmsfamily("Gamma", link = "identity")
  f$fit$data$y <- plogis(f$fit$data$y)
  f
}

# A joint fit whose zeros are the hurdle: the top fifth of the concentrations.
joint_gamma_fixture <- function() {
  f <- gamma_fixture()
  f$fit$family <- brms::hurdle_gamma(link = "identity", link_hu = "identity")
  d <- f$fit$data
  d$y[d$x > stats::quantile(d$x, 0.8)] <- 0
  f$fit$data <- d
  f
}

test_that("a Gamma fit given new data is not a change of family (#435)", {
  # Treat stored gamma and supplied Gamma as the same family.
  g <- gamma_fixture()
  o <- recover_prebayesnecfit(g)
  expect_false(check_update_data(o, g$fit$data)$changed_family)
  expect_false(
    check_update_data(o, g$fit$data, Gamma(link = "identity"))$changed_family
  )
  # The refit is reached in both forms. With no family supplied none is passed
  # on, so brms keeps the one it stored.
  got <- capture_brms_update(update(g, newdata = g$fit$data))
  expect_false(is.null(got))
  expect_null(got$family)
  expect_equal(got$newdata_val$y, g$fit$data$y)
  got <- capture_brms_update(
    update(g, newdata = g$fit$data, family = Gamma(link = "identity"))
  )
  expect_false(is.null(got))
  expect_equal(got$family$family, "Gamma")
  expect_equal(got$family$link, "identity")
})

test_that("a Gamma fit's zeros are shifted under the stored family (#435)", {
  # Canonicalise the stored Gamma family before checking new data so boundary
  # zeros are shifted before brms receives them.
  g <- gamma_fixture()
  d <- g$fit$data
  d$y[1:3] <- 0
  res <- NULL
  expect_message(
    res <- check_update_data(recover_prebayesnecfit(g), d),
    "shifted to"
  )
  expect_false(res$changed_family)
  expect_equal(sum(res$data$y == 0), 0)
  expect_equal(res$data$y[-(1:3)], d$y[-(1:3)])
})

test_that("a joint hurdle_gamma fit keeps its zeros on update (#435)", {
  # Keep hurdle zeros when updating: selecting a family from the new data
  # instead would shift away observations the hurdle block must model.
  h <- joint_gamma_fixture()
  d <- h$fit$data
  d$y[d$y > 0] <- d$y[d$y > 0] * 1.01
  n_zero <- sum(d$y == 0)
  expect_gt(n_zero, 0)
  res <- NULL
  expect_no_message(res <- check_update_data(recover_prebayesnecfit(h), d))
  expect_false(res$changed_family)
  expect_null(res$substitutions)
  expect_equal(sum(res$data$y == 0), n_zero)
  # A supplied family naming the stored one is not a change either.
  expect_false(
    check_update_data(recover_prebayesnecfit(h), d, "hurdle_gamma")$changed_family
  )
  got <- capture_brms_update(update(h, newdata = d))
  expect_false(is.null(got))
  expect_equal(sum(got$newdata_val$y == 0), n_zero)
})

test_that("a gaussian fit keeps an exact 1 on update (#435)", {
  # A stored Gaussian fit keeps a response of one unchanged; inferring beta
  # from the new data would shift it.
  nd <- nec_data
  nd$y[which.max(nd$y)] <- 1
  res <- NULL
  expect_no_message(
    res <- check_update_data(recover_prebayesnecfit(manec_example), nd)
  )
  expect_false(res$changed_family)
  expect_equal(max(res$data$y), 1)
  for (force in c(FALSE, TRUE)) {
    got <- capture_brms_update(
      update(manec_example, newdata = nd, force_fit = force)
    )
    expect_false(is.null(got))
    expect_equal(max(got$newdata_val$y), 1)
  }
})

test_that("a family that differs still needs force_fit, and its checks (#435)", {
  # Require force_fit for a family change because the reused priors were
  # built for the original family.
  nd <- nec_data
  nd$y[which.max(nd$y)] <- 1
  # Refused before the data are checked, so the 1 is not reported as shifted.
  expect_no_message(expect_error(
    update(manec_example, newdata = nd, family = Beta()),
    "force_fit = TRUE"
  ))
  # With it, the data are adjusted for the family supplied, which is the one
  # the refit uses.
  got <- capture_brms_update(
    update(manec_example, newdata = nd, family = Beta(), force_fit = TRUE)
  )
  expect_equal(got$family$family, "beta")
  expect_equal(max(got$newdata_val$y), 0.999)
})

# The messages update() prints, stopped where brms would refit.
messages_before_refit <- function(expr) {
  msgs <- character()
  local_mocked_bindings(
    update = function(...) stop("halted by test"),
    .package = "bayesnec"
  )
  # try(silent = FALSE) in the refit loop prints the halt to stderr.
  invisible(capture.output(
    tryCatch(
      withCallingHandlers(expr, message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }),
      error = function(e) NULL
    ),
    type = "message"
  ))
  msgs
}

test_that("update() reports a shift without naming priors (#486)", {
  # update() reuses the fitted priors, so a message about priors derived from
  # the shifted response describes get_priors(), not this call.
  nd <- nec_data
  nd$y[which.max(nd$y)] <- 1
  msgs <- messages_before_refit(
    stats::update(manec_example, newdata = nd,
                  family = Beta(link = "identity"), force_fit = TRUE)
  )
  shift <- grep("shifted to", msgs, value = TRUE)
  expect_length(shift, 1)
  expect_match(shift, "1 value(s) at 1", fixed = TRUE)
  # manec_example holds no bnec_record(), so the message cannot point to it.
  expect_no_match(shift, "prior|bnec_record")
  # A fit that holds a record is pointed to it.
  with_record <- manec_example
  attr(with_record, "bnec_record") <- list(substitutions = NULL)
  msgs <- messages_before_refit(
    stats::update(with_record, newdata = nd,
                  family = Beta(link = "identity"), force_fit = TRUE)
  )
  shift <- grep("shifted to", msgs, value = TRUE)
  expect_match(shift, "see ?bnec_record.", fixed = TRUE)
  expect_no_match(shift, "prior")
})

test_that("update returns a bayesnecfit when only one model survives", {
  skip_on_cran()
  # Return the single surviving fit in the same class as bnec() and amend().
  # The stub preserves the first stored fit and fails on the second, reaching
  # that assembly path without sampling.
  tbl <- get(".__S3MethodsTable__.", envir = asNamespace("stats"))
  orig_method <- get("update.brmsfit", envir = tbl)
  on.exit(assign("update.brmsfit", orig_method, envir = tbl), add = TRUE)
  n_called <- 0
  stub <- function(object, formula. = NULL, newdata = NULL, recompile = NULL,
                   ...) {
    n_called <<- n_called + 1
    if (n_called == 1) {
      object
    } else {
      stop("halted by test")
    }
  }
  assign("update.brmsfit", stub, envir = tbl)
  # try(silent = FALSE) in the refit loop prints the stub's stop to stderr.
  invisible(capture.output(
    upd <- suppressMessages(update(manec_example)),
    type = "message"
  ))
  expect_s3_class(upd, "bayesnecfit")
  expect_false(inherits(upd, "bayesmanecfit"))
  expect_true(is_bayesnecfit(upd))
  expect_equal(upd$model, names(manec_example$mod_fits)[1])
  expect_error(suppressWarnings(summary(upd)), NA)
})

# Compare weighting methods across constructors using stored fits. Only
# the weights are recomputed; no models are sampled.

test_that("c() and + weight by the documented default", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  combined <- c(nec4param, ecx4param) |>
    suppressMessages() |>
    suppressWarnings()
  # Asserted on the method rather than on the weights, which are a Bayesian
  # bootstrap under pseudo-BMA and so differ between calls.
  expect_equal(attr(combined$mod_stats$wi, "method"), "pseudobma")
  expect_equal(class(combined$mod_stats$wi), "pseudobma_bb_weights")
  # Compare against the same equations fitted together in manec_example.
  expect_equal(attr(combined$mod_stats$wi, "method"),
               attr(manec_example$mod_stats$wi, "method"))
  added <- (nec4param + ecx4param) |>
    suppressMessages() |>
    suppressWarnings()
  expect_equal(attr(added$mod_stats$wi, "method"), "pseudobma")
})

test_that("c() compares stored fit data by column name", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  reordered <- ecx4param
  reordered$fit$data <- reordered$fit$data[rev(names(reordered$fit$data))]

  combined <- c(nec4param, reordered) |>
    suppressMessages() |>
    suppressWarnings()
  expect_s3_class(combined, "bayesmanecfit")

  changed <- reordered
  changed$fit$data[[1]][1] <- changed$fit$data[[1]][1] + 1
  expect_error(c(nec4param, changed), "Dataset values differ across fits")
})

test_that("c() inherits an explicit weighting method from its inputs", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  stacked <- amend(manec_example,
                   loo_controls = list(weights = list(method = "stacking"))) |>
    suppressMessages() |>
    suppressWarnings()
  expect_equal(attr(stacked$mod_stats$wi, "method"), "stacking")
  # c() takes no loo_controls, so the objects being combined are the only place
  # an explicit request can come from. Reverting to the default here would
  # discard it silently.
  inherited <- c(stacked, ecx4param) |>
    suppressMessages() |>
    suppressWarnings()
  expect_equal(attr(inherited$mod_stats$wi, "method"), "stacking")
  pseudo <- amend(manec_example,
                  loo_controls = list(weights = list(method = "pseudobma"))) |>
    suppressMessages() |>
    suppressWarnings()
  # Two inputs weighted differently have no answer that is right for both, so
  # the default is used and the disagreement reported.
  expect_message(suppressWarnings(c(stacked, pseudo)),
                 "weighted by different methods")
  mixed <- c(stacked, pseudo) |>
    suppressMessages() |>
    suppressWarnings()
  expect_equal(attr(mixed$mod_stats$wi, "method"), "pseudobma")
})

test_that("update() keeps the weighting method the set was built with", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # Return stored fits from the stub to exercise post-refit assembly without
  # Stan. Restore the S3 table entry, as in capture_brms_update().
  tbl <- get(".__S3MethodsTable__.", envir = asNamespace("stats"))
  orig_method <- get("update.brmsfit", envir = tbl)
  on.exit(assign("update.brmsfit", orig_method, envir = tbl), add = TRUE)
  assign("update.brmsfit", function(object, ...) object, envir = tbl)
  stacked <- amend(manec_example,
                   loo_controls = list(weights = list(method = "stacking"))) |>
    suppressMessages() |>
    suppressWarnings()
  # update() refits a set; it is not a request to reweight it.
  kept <- update(stacked, recompile = FALSE) |>
    suppressMessages() |>
    suppressWarnings()
  expect_equal(attr(kept$mod_stats$wi, "method"), "stacking")
  defaulted <- update(manec_example, recompile = FALSE) |>
    suppressMessages() |>
    suppressWarnings()
  expect_equal(attr(defaulted$mod_stats$wi, "method"), "pseudobma")
  # An explicit weighting request overrides the stored method.
  changed <- update(stacked, recompile = FALSE,
                    loo_controls = list(weights =
                                          list(method = "pseudobma"))) |>
    suppressMessages() |>
    suppressWarnings()
  expect_equal(attr(changed$mod_stats$wi, "method"), "pseudobma")
  # Changing a LOO fitting argument alone must preserve the recorded weights
  # method; loo_controls has separate fitting and weights entries.
  fitting_only <- update(stacked, recompile = FALSE,
                         loo_controls = list(fitting = list(reloo = FALSE))) |>
    suppressMessages() |>
    suppressWarnings()
  expect_equal(attr(fitting_only$mod_stats$wi, "method"), "stacking")
  # The same call on a set that records no method takes the default rather
  # than passing method = NULL down to loo.
  unrecorded <- manec_example
  attr(unrecorded$mod_stats$wi, "method") <- NULL
  defaulted_fitting <- update(unrecorded, recompile = FALSE,
                              loo_controls =
                                list(fitting = list(reloo = FALSE))) |>
    suppressMessages() |>
    suppressWarnings()
  expect_equal(attr(defaulted_fitting$mod_stats$wi, "method"), "pseudobma")
})


test_that("update refuses a resolution below 2 before refitting", {
  # Reject an unusable estimation grid before update() refits the model set.
  # This refusal test must reach no sampler.
  skip_on_cran()
  expect_error(update(manec_example, resolution = 1), "must be at least 2")
})
