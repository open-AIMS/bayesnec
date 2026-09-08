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
  expect_equal(bayesnec:::family_signature(gaussian()),
               c(family = "gaussian", link = "identity"))
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

test_that("check_update_data still derives the family from new data", {
  o <- recover_prebayesnecfit(manec_example)
  expect_false(bayesnec:::check_update_data(o, o[[1]]$fit$data)$changed_family)
  expect_true(bayesnec:::check_update_data(o, nec_data)$changed_family)
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
  expect_error(update(manec_example, newdata = nec_data), "force_fit = TRUE")
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
