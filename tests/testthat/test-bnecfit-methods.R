# update.bnecfit() applies the same link policy as bnec(). None of these tests
# need sampling: the family is validated, and the change guard runs, before any
# model is refitted, and the two tests that inspect what brms receives halt the
# refit at the brms entry point.

capture_brms_update <- function(expr) {
  # Substituted rather than mocked or traced, because the point is what brms
  # itself is handed. The stub writes to an option: it is dispatched to in
  # place of update.brmsfit(), so it cannot see anything in the test's own
  # environment.
  #
  # An S3 method is registered in the S3 methods table of the *generic's*
  # namespace, so brms's update.brmsfit is reached through stats, not through
  # brms. This used trace(..., where = asNamespace("brms")), which replaces
  # both that entry and the brms binding, while untrace() restores only the
  # binding: measured, after this file ran the brms binding was untraced and
  # getS3method("update", "brmsfit") was still the tracer, so the next real
  # update() in the session stopped with "halted by test". Writing the stub
  # into the table directly is what registerS3method() does, and it restores
  # exactly, so nothing survives the call.
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
  # brms stores a brmsfamily in the fit while mod_fams builds gaussian and
  # Gamma from stats, so the two objects never matched even for the same
  # family. See #256.
  # A link the object does not record is the one brms assigns: stats builds
  # gaussian() with `link` alone, and brms fits sigma on the log link. See
  # #435.
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
  # Collected by `...` and passed positionally, the family never reached
  # retrieve_valid_family(), so the guard could not detect a family change at
  # all -- it only ever compared the data-derived family. See #256.
  expect_false(bayesnec:::check_update_data(o, dat, validate_family("gaussian"))$changed_family)
  expect_true(bayesnec:::check_update_data(o, dat, validate_family("Beta"))$changed_family)
})

test_that("family_signature names a stored Gamma family as bayesnec does", {
  # brms records Gamma as "gamma", and the stored brmsfamily records
  # link_shape where stats::Gamma() does not, so a supplied
  # Gamma(link = "identity") differed from every Gamma fit on both counts.
  # See #435.
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
  # INVERTED. This pinned the family read off the new data: nec_data's
  # response lies in (0, 1), so it was read as beta and compared against the
  # gaussian fit. update(newdata = ) keeps the fitted family, so new data are
  # never a change of family. See #435.
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
  # New data alone are no longer a change of family (D32); with a family that
  # differs they still require force_fit. See #435.
  expect_error(update(manec_example, newdata = nec_data, family = Beta()),
               "force_fit = TRUE")
})

test_that("brms receives the validated family, not the one written", {
  got <- capture_brms_update(
    update(manec_example, family = Beta(), force_fit = TRUE)
  )
  # The caller wrote no link, so bayesnec assigns identity. Forwarded through
  # `...` untouched this was beta's own logit, and the curve was fitted to the
  # logit of the mean while top, bot and nec were reported on the response
  # scale.
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
  # The second of #274's two routes. check_update_data() runs on
  # object[[1]]$fit$data whenever a family is supplied, reports the boundary
  # shift, and the corrected frame was then thrown away because it was
  # substituted only when the caller had passed newdata. brms received NULL,
  # refitted the unshifted stored data, and Stan failed on the boundary just
  # reported repaired.
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

# ---- #435, D32: update(newdata = ) keeps the fitted family -------------------
# Structural fixtures built from the packaged nec4param fit: the family brms
# stored is relabelled and the stored response replaced by one that family
# admits. Nothing is sampled. Every update() below halts the refit at the brms
# entry point, so the stored posterior, which is a gaussian one, is never read.

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
  # The stored tag is "gamma" and the one bayesnec builds "Gamma", so every
  # Gamma fit was refused as a change of family, with or without a family
  # supplied.
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
  # With no family supplied the checks follow the stored one, whose tag brms
  # records as "gamma". check_data() keys its shift on "Gamma", so read under
  # the stored tag the zeros would reach brms unshifted and Stan would fail on
  # them.
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
  # Read off the data, the response was Gamma: the zeros were reported as
  # shifted, and under force_fit = TRUE the hurdle_gamma refit received a
  # response with no zeros for its hurdle block to model.
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
  # Read off the data, a response in [0, 1] was beta: the 1 was shifted to
  # 0.999, and under force_fit = TRUE the gaussian refit was made on 0.999.
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
  # D32 does not say whether a supplied family that differs from the fitted
  # one still needs force_fit = TRUE. It keeps the requirement, because the
  # priors reused from the original fit were built for the fitted family.
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
  # A refit that fails for one model of a set is the ordinary case
  # expand_manec()'s single-survivor branch exists to handle, so update() must
  # return what bnec() and amend() return for the same surviving set. It
  # classed the bare one-element list as a bayesmanecfit, and every method on
  # the result then failed on a missing `mod_fits`. See #288.
  #
  # The stub returns the stored fit unchanged for the first model and fails for
  # the second, which reaches the branch without sampling.
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

# The weighting method a set is built with must not depend on which function
# assembled it. None of these tests sample: the fits are the packaged ones and
# only the weighting is recomputed. See #320.

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
  # The comparison the issue is about: the same two models fitted in one bnec()
  # call, which is what manec_example is.
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
  # The stub returns the stored fit unchanged, so update.bnecfit() runs to
  # completion without Stan. Everything asserted here happens after the refit
  # loop. Registered into the S3 methods table directly, and restored from it,
  # for the reason capture_brms_update() records above.
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
  # An explicit request still wins over what the object records.
  changed <- update(stacked, recompile = FALSE,
                    loo_controls = list(weights =
                                          list(method = "pseudobma"))) |>
    suppressMessages() |>
    suppressWarnings()
  expect_equal(attr(changed$mod_stats$wi, "method"), "pseudobma")
  # loo_controls names `fitting` and `weights` separately, so a call changing a
  # LOO fitting argument names no method. Reading the argument's presence as a
  # request to reweight replaced the recorded method with the default, so this
  # call returned a pseudo-BMA-weighted set as a side effect of asking for
  # something else.
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
  # This method samples every model in the set again, so a resolution the
  # no-effect estimate cannot be read off must be refused before that work
  # rather than when expand_nec() reaches it. The test costs no fit for the
  # same reason. See #325.
  skip_on_cran()
  expect_error(update(manec_example, resolution = 1), "must be at least 2")
})
