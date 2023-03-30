test_that("prob_vals warnings behave as expected", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  expect_length(
    nsec(manec_example, prob_vals = c(0.6, 0.1, 0.9), precision = 10), 3
  ) |>
    suppressWarnings()
  expect_error(
    nsec(manec_example, prob_vals = 0.9, precision = 10)
  ) |>
    suppressWarnings()
  expect_error(
    nsec(manec_example, prob_vals = c(0.6, 0.9, 0.1), precision = 10)
  ) |>
    suppressWarnings()
  expect_length(
    nsec(nec4param, prob_vals = c(0.6, 0.1, 0.9), precision = 10), 3
  )
  expect_error(nsec(nec4param, prob_vals = 0.9, precision = 10))
  expect_error(nsec(nec4param, prob_vals = c(0.6, 0.9, 0.1), precision = 10))
})

test_that(paste0("nsec returns expected object types and precision is",
                 " passing correctly"), {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  nsec_summary <- nsec(manec_example, sig_val = 0.01, precision = 50) |>
    suppressWarnings()
  nsec_summary2 <- nsec(manec_example, sig_val = 0.01, precision = 50,
                        xform = exp) |>
    suppressWarnings()
  nsec_posterior <- nsec(manec_example, sig_val = 0.01,
                         posterior = TRUE, precision = 50) |>
    suppressWarnings()
  nsecn_summary <- nsec(nec4param, sig_val = 0.01, precision = 50) |>
    suppressWarnings()
  nsecn_summary2 <- nsec(nec4param, sig_val = 0.01, precision = 50,
                         xform = exp) |>
    suppressWarnings()
  nsecn_posterior <- nsec(nec4param, sig_val = 0.01,
                          posterior = TRUE, precision = 50) |>
    suppressWarnings()
  expect_equal(length(nsec_summary), 3)
  expect_gt(length(nsec_posterior), 3)
  expect_equal(length(nsecn_summary), 3)
  expect_gt(length(nsecn_posterior), 3)
  expect_equal(attributes(nsec_summary)$precision, 50)
  expect_equal(attributes(nsec_posterior)$precision, 50)
  expect_equal(attributes(nsecn_summary)$precision, 50)
  expect_equal(attributes(nsecn_posterior)$precision, 50)
})

test_that("works for bayesnecfit", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  nsec1 <- nsec(ecx4param, precision = 10)
  expect_equal(length(nsec1), 3)
  expect_equal(names(nsec1), c("Q50", "Q2.5", "Q97.5"))
})

test_that("works for bayesmanecfit", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  nsec1 <- nsec(manec_example, precision = 10) |>
    suppressWarnings()
  expect_equal(length(nsec1), 3)
  expect_equal(names(nsec1), c("Q50", "Q2.5", "Q97.5"))
})

test_that("xform passes correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  nsec1 <- nsec(ecx4param, precision = 10)
  nsec2 <- nsec(ecx4param, xform = exp, precision = 10)
  expect_gt(nsec2[1], nsec1[2])
})

test_that("posterior passes correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  nsec3 <- nsec(ecx4param, posterior = TRUE, precision = 10)
  expect_equal(length(nsec3), 100)
})

test_that("prob_vals passes correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  nsec4 <- nsec(ecx4param, prob_vals = c(0.5, 0.3, 0.7), precision = 10)
  expect_equal(names(nsec4), c("Q50", "Q30", "Q70"))
})

test_that("sig_val passes correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  nsec4 <- nsec(ecx4param, prob_vals = c(0.5, 0.3, 0.7), sig_val = 0.05,
                precision = 10)
  expect_equal(names(nsec4), c("Q50", "Q30", "Q70"))
})
