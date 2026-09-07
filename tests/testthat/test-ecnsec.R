test_that("ecnsec returns expected object types", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ecnsec_summary <- ecnsec(manec_example, nsec = 1.5, resolution = 10) |>
    suppressWarnings()
  expect_length(ecnsec_summary, 3)
  expect_equal(names(ecnsec_summary), c("50%", "2.5%", "97.5%"))
  expect_true(is.numeric(ecnsec_summary))

  ecnsec_posterior <- ecnsec(manec_example, nsec = 1.5, posterior = TRUE,
                             resolution = 10) |>
    suppressWarnings()
  expect_true(is.numeric(ecnsec_posterior))
  expect_gt(length(ecnsec_posterior), length(ecnsec_summary))
})

test_that("ecnsec works for a bayesnecfit", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ecnsec1 <- ecnsec(nec4param, nsec = 1.5, resolution = 10) |>
    suppressWarnings()
  expect_length(ecnsec1, 3)
  expect_equal(names(ecnsec1), c("50%", "2.5%", "97.5%"))
})

test_that("ecnsec argument validation behaves as expected", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # prob_vals must supply central, lower and upper quantiles, in that order
  expect_error(ecnsec(manec_example, nsec = 1.5, prob_vals = 0.9))
  expect_error(ecnsec(manec_example, nsec = 1.5, prob_vals = c(0.6, 0.9, 0.1)))
  # hormesis_def must be one of "max" or "control"
  # hormesis_def is removed, and a call still passing it is refused by name
  # rather than absorbed by ... in silence. See D15 ruling 4.
  expect_error(ecnsec(manec_example, nsec = 1.5, hormesis_def = "invalid"),
               "hormesis_def has been removed")
  expect_error(ecnsec(manec_example, nsec = 1.5, hormesis_def = "control"),
               "hormesis_def has been removed")
  # xform must be a function
  expect_error(ecnsec(manec_example, nsec = 1.5, xform = "notafunction"))
})


test_that("ecnsec takes the ecx vocabulary and refuses direct", {
  # T8: ecnsec inverts the ecx reference construction under the same type, so
  # it takes the same four names minus "direct", which is a response value
  # rather than a percentage and has no percent effect to report.
  skip_on_cran()
  expect_length(ecnsec(nec4param, nsec = 1.5, resolution = 50), 3)
  expect_length(
    ecnsec(nec4param, nsec = 1.5, resolution = 50, type = "range"), 3
  )
  expect_error(ecnsec(nec4param, nsec = 1.5, resolution = 50, type = "direct"),
               "names a response value")
  expect_error(ecnsec(nec4param, nsec = 1.5, resolution = 50,
                      type = "nonsense"),
               "type must be one of")
})

test_that("the absolute ecnsec agrees with the ecx definition", {
  # The two must answer the same question of the same curve. Read the percent
  # effect at a concentration, then ask ecx for the concentration at that
  # percent effect, and the round trip must return where it started.
  skip_on_cran()
  target_x <- 1.5
  pct <- ecnsec(nec4param, nsec = target_x, resolution = 500)
  back <- ecx(nec4param, ecx_val = as.numeric(pct[1]), type = "absolute",
              resolution = 500)
  expect_equal(as.numeric(back[1]), target_x, tolerance = 0.05)
})


# ---- a model set under every type --------------------------------------------

# ecx_asymptote() read object$fit, which a bayesmanecfit does not have, so
# ecnsec(<set>, type = "relative") failed inside posterior with "Don't know how
# to transform an object of class 'NULL'" -- a message naming neither the
# argument nor the class. "absolute" and "range" worked on the same object, so
# the failure was in the asymptote alone.

test_that("ecnsec answers a model set under every percentage type", {
  skip_on_cran()
  out <- lapply(c("absolute", "relative", "range"), function(ty) {
    suppressWarnings(
      ecnsec(manec_example, nsec = 1.5, resolution = 50, type = ty)
    )
  })
  for (o in out) {
    expect_equal(length(o), 3)
    expect_false(anyNA(o))
  }
  names(out) <- c("absolute", "relative", "range")
  # The types order as their denominators do. manec_example's equations both
  # carry a bot well below the observed response, so the control-to-bot span is
  # the widest and the same decline is the smallest percentage of it.
  expect_lt(out$relative[1], out$range[1])
  expect_lt(out$range[1], out$absolute[1])
})

test_that("the model-averaged asymptote is one value per averaged draw", {
  skip_on_cran()
  a <- manec_asymptote(manec_example)
  # Stacked in the same order and thinned by the same index as the
  # model-averaged posterior, so the two pair row for row.
  expect_equal(length(a),
               nrow(posterior_epred(manec_example,
                                    newdata = bnec_newdata(manec_example,
                                                           resolution = 5),
                                    re_formula = NA)))
})
