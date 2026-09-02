# update.bnecfit() applies the same link policy as bnec(). None of these tests
# need sampling: the family is validated, and the change guard runs, before any
# model is refitted, and the two tests that inspect what brms receives halt the
# refit at the brms entry point.

capture_brms_update <- function(expr) {
  # Traced rather than mocked because the point is what brms itself is handed.
  # The tracer writes to an option: it is evaluated inside update.brmsfit(), so
  # it cannot see anything in the test's own environment.
  old <- getOption("bayesnec_test_capture")
  on.exit(options(bayesnec_test_capture = old), add = TRUE)
  options(bayesnec_test_capture = NULL)
  suppressMessages(
    trace("update.brmsfit", where = asNamespace("brms"), print = FALSE,
          tracer = quote({
            options(bayesnec_test_capture = list(
              family = list(...)$family,
              newdata_expr = deparse(substitute(newdata))
            ))
            stop("halted by test")
          }))
  )
  on.exit(suppressMessages(
    untrace("update.brmsfit", where = asNamespace("brms"))
  ), add = TRUE)
  # try(silent = FALSE) in the refit loop prints the tracer's stop to stderr.
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

test_that("has_family_changed reads the family it is given", {
  o <- recover_prebayesnecfit(manec_example)
  dat <- o[[1]]$fit$data
  # Collected by `...` and passed positionally, the family never reached
  # retrieve_valid_family(), so the guard could not detect a family change at
  # all -- it only ever compared the data-derived family. See #256.
  expect_false(bayesnec:::has_family_changed(o, dat, validate_family("gaussian")))
  expect_true(bayesnec:::has_family_changed(o, dat, validate_family("Beta")))
})

test_that("has_family_changed still derives the family from new data", {
  o <- recover_prebayesnecfit(manec_example)
  expect_false(bayesnec:::has_family_changed(o, o[[1]]$fit$data))
  expect_true(bayesnec:::has_family_changed(o, nec_data))
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
