library(bayesnec)

test_that("prob_vals warnings behave as expected", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ecx(manec_example, prob_vals = c(0.6, 0.1, 0.9), precision = 10,
      type = "relative") |>
    expect_length(3) |>
    suppressWarnings()
  ecx(manec_example, prob_vals = 0.9, precision = 10, type = "relative") |>
    expect_error() |>
    suppressWarnings()
  ecx(manec_example, prob_vals = c(0.6, 0.9, 0.1), precision = 10,
      type = "relative") |>
    expect_error() |>
    suppressWarnings()
  ecx(nec4param, prob_vals = c(0.6, 0.1, 0.9), precision = 10,
      type = "relative") |>
    expect_length(3) |>
    suppressWarnings()
  expect_error(ecx(nec4param, prob_vals = 0.9, type = "relative",
                   precision = 10))
  expect_error(ecx(nec4param, prob_vals = c(0.6, 0.9, 0.1), precision = 10,
                   type = "relative"))
})

test_that("ecx_val warnings behave as expected", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  expect_error(
    ecx(manec_example, ecx_val = 0.9, type = "relative", precision = 10)
  ) |>
    suppressWarnings()
  expect_error(
    ecx(nec4param, ecx_val = 0.9, type = "relative", precision = 10)
  )
})

test_that("ecx returns expected object types and arguments pass correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ec50_summary <- ecx(manec_example, ecx_val = 50, type = "relative",
                      precision = 50)
  ec50_summary2 <- ecx(manec_example, ecx_val = 50, type = "relative",
                       precision = 50, xform = exp)
  ec50_posterior <- ecx(manec_example, ecx_val = 50,
                        type = "relative", posterior = TRUE, precision = 50)
  ec50n_summary <- ecx(nec4param, ecx_val = 50, type = "relative",
                       precision = 50)
  ec50n_summary2 <- ecx(nec4param, ecx_val = 50, type = "relative",
                        precision = 50, xform = exp)
  ec50n_posterior <- ecx(nec4param, ecx_val = 50, type = "relative",
                         posterior = TRUE, precision = 50)
  expect_equal(length(ec50_summary), 3)
  expect_gt(length(ec50_posterior), 3)
  expect_equal(length(ec50n_summary), 3)
  expect_gt(length(ec50n_posterior), 3)
  expect_equal(attributes(ec50_summary)$precision, 50)
  expect_equal(attributes(ec50_posterior)$precision, 50)
  expect_equal(attributes(ec50n_summary)$precision, 50)
  expect_equal(attributes(ec50n_posterior)$precision, 50)
  expect_gt(ec50_summary2[1], ec50_summary[1])
  expect_gt(ec50n_summary2[1], ec50n_summary[1])
})

test_that("works for bayesnecfit", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ecx1 <- ecx(ecx4param, precision = 10)
  expect_equal(length(ecx1), 3)
  expect_equal(names(ecx1), c("Q50", "Q2.5", "Q97.5"))
})

test_that("works for bayesmanecfit", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ecx1 <- ecx(manec_example, precision = 10)
  expect_equal(length(ecx1), 3)
  expect_equal(names(ecx1), c("Q50", "Q2.5", "Q97.5"))
})

test_that("xform passes correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ecx1 <- ecx(ecx4param, precision = 10)
  ecx2 <- ecx(ecx4param, xform = exp, precision = 10)
  expect_gt(ecx2[1], ecx1[2])
})

test_that("posterior passes correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ecx3 <- ecx(ecx4param, posterior = TRUE, precision = 10)
  expect_equal(length(ecx3), 100)
})

test_that("prob_vals passes correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ecx4 <- ecx(ecx4param, prob_vals = c(0.5, 0.3, 0.7), precision = 10)
  expect_equal(names(ecx4), c("Q50", "Q30", "Q70"))
})

test_that("ecx_val passes correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ecx4 <- ecx(ecx4param, prob_vals = c(0.5, 0.3, 0.7), ecx_val = 20,
              precision = 10)
  expect_equal(names(ecx4), c("Q50", "Q30", "Q70"))
})
