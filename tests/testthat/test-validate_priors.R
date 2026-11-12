test_that("prior validation retains the supported default and list paths", {
  supplied <- brms::prior_string("beta(5, 1)", nlpar = "top")
  expect_null(validate_priors(NULL, "nec3param"))
  expect_identical(validate_priors(supplied, "nec3param"), supplied)
  expect_identical(validate_priors(list(nec3param = supplied), "nec3param"),
                   supplied)
  expect_message(
    expect_null(validate_priors(list(nec4param = supplied), "nec3param")),
    "Using bayesnec default priors"
  )
})

test_that("duplicate model names cannot silently discard a prior", {
  first <- brms::prior_string("beta(5, 1)", nlpar = "top")
  second <- brms::prior_string("beta(5, 2)", nlpar = "top")
  duplicate <- list(nec3param = first, nec3param = second)
  expect_error(validate_priors(duplicate, "nec3param"),
               "duplicate model name `nec3param`")
})

test_that("a missing prior uses defaults and a partial prior is filled", {
  x <- nec_data$x
  y <- nec_data$y
  family <- validate_family("Beta")
  defaults <- suppressMessages(add_brm_defaults(
    list(prior = NULL, init = "random"), "nec3param", family, x, y,
    skip_check = FALSE, custom_name = NULL
  ))$prior
  expect_s3_class(defaults, "brmsprior")
  partial <- brms::prior_string("beta(5, 1)", nlpar = "top")
  expect_warning(filled <- suppressMessages(add_brm_defaults(
    list(prior = partial, init = "random"), "nec3param", family, x, y,
    skip_check = FALSE, custom_name = NULL
  ))$prior, "Using the bayesnec default")
  expect_true(any(filled$nlpar == "top" & filled$prior == "beta(5, 1)"))
  expect_true(nrow(filled) > nrow(partial))
})
