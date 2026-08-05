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
  expect_error(ecnsec(manec_example, nsec = 1.5, hormesis_def = "invalid"))
  # xform must be a function
  expect_error(ecnsec(manec_example, nsec = 1.5, xform = "notafunction"))
})
