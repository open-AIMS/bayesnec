library(bayesnec)

gamma_y_prior <- define_prior(model = "nec3param", family = gaussian(),
                              predictor = rnorm(100), response = 1:100)

test_that("model is always properly specified as character", {
  # model
  expect_error(define_prior(family = gaussian(),
                            predictor = rnorm(100), response = 1:100))
  expect_error(define_prior(model = NULL, family = gaussian(),
                            predictor = rnorm(100), response = 1:100))
  expect_error(define_prior(model = NA, family = gaussian(),
                            predictor = rnorm(100), response = 1:100))
  expect_error(define_prior(model = FALSE, family = gaussian(),
                            predictor = rnorm(100), response = 1:100))
  expect_error(define_prior(model = 10, family = gaussian(),
                            predictor = rnorm(100), response = 1:100))
  expect_error(define_prior(model = "none", family = gaussian(),
                            predictor = rnorm(100), response = 1:100))
  expect_error(define_prior(model = "all", family = gaussian(),
                            predictor = rnorm(100), response = 1:100))
  expect_error(define_prior(model = "ecx", family = gaussian(),
                            predictor = rnorm(100), response = 1:100))
  expect_s3_class(gamma_y_prior, "brmsprior")
  expect_s3_class(manec_gausian_identity, "bayesmanecfit")
})

test_that("check proper output structure", {
  expect_identical(sort(gamma_y_prior$nlpar), c("beta", "nec", "top"))
})

# family is a string
# family is not a family object
# either predictor or response is not numeric
# either predictor or response contains NA
# predictor and response have different lengths
