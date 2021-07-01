library(bayesnec)
library(brms)

pred_a <- rnorm(100)
pred_na <- add_na(pred_a)
resp_a <- 1:100
resp_na <- add_na(resp_a)
pred_b <- pred_a[-1]
resp_b <- resp_a[-1]

data(manec_example)

test_that("model is always properly specified as character", {
  p_a <- define_prior(model = "nec3param", family = gaussian(),
                      predictor = pred_a, response = resp_a)
  expect_error(define_prior(family = gaussian(),
                            predictor = pred_a, response = resp_a))
  expect_error(define_prior(model = NULL, family = gaussian(),
                            predictor = pred_a, response = resp_a))
  expect_error(define_prior(model = NA, family = gaussian(),
                            predictor = pred_a, response = resp_a))
  expect_error(define_prior(model = FALSE, family = gaussian(),
                            predictor = pred_a, response = resp_a))
  expect_error(define_prior(model = 10, family = gaussian(),
                            predictor = pred_a, response = resp_a))
  expect_error(define_prior(model = "none", family = gaussian(),
                            predictor = pred_a, response = resp_a))
  expect_error(define_prior(model = "all", family = gaussian(),
                            predictor = pred_a, response = resp_a))
  expect_error(define_prior(model = "ecx", family = gaussian(),
                            predictor = pred_a, response = resp_a))
  expect_s3_class(p_a, "brmsprior")
  expect_s3_class(manec_example, "bayesmanecfit")
})

test_that("family is a family object of correct family", {
  expect_error(define_prior(model = "nec3param", family = "gaussian",
                            predictor = pred_a, response = resp_a))
  expect_error(define_prior(model = "nec3param", family = gaussian,
                            predictor = pred_a, response = resp_a))
  expect_error(define_prior(model = "nec3param", family = inverse.gaussian(),
                            predictor = pred_a, response = resp_a))
  expect_s3_class(define_prior(model = "nec3param", family = poisson(),
                               predictor = pred_a, response = resp_a),
                  "brmsprior")
  expect_s3_class(define_prior(model = "nec3param", family = binomial(),
                               predictor = pred_a, response = resp_a),
                  "brmsprior")
  expect_s3_class(define_prior(model = "nec3param", family = Gamma(),
                               predictor = pred_a, response = resp_a),
                  "brmsprior")
})

test_that("either predictor or response contains NA", {
  expect_error(define_prior(model = "nec3param", family = gaussian(),
                            predictor = pred_na, response = resp_a))
  expect_error(define_prior(model = "nec3param", family = gaussian(),
                            predictor = pred_a, response = resp_na))
})

test_that("predictor and response have different lengths", {
  expect_s3_class(define_prior(model = "nec3param", family = gaussian(),
                               predictor = pred_b, response = resp_a),
                  "brmsprior")
  expect_s3_class(define_prior(model = "nec3param", family = gaussian(),
                               predictor = pred_a, response = resp_b),
                  "brmsprior")
})

test_that("check proper output structure", {
  p_a <- define_prior(model = "nec3param", family = gaussian(),
                      predictor = pred_a, response = resp_a)
  p_b <- define_prior(model = "nec4param", family = Beta(link = "logit"),
                      predictor = pred_a, response = rbeta(100, 1, 5))
  p_c <- define_prior(model = "nec4param", family = Beta(link = "identity"),
                      predictor = pred_a, response = rbeta(100, 1, 5))
  expect_identical(sort(p_a$nlpar), c("beta", "nec", "top"))
  expect_true(grepl("normal", p_a$prior[p_a$nlpar == "beta"]))
  expect_true(grepl("normal", p_a$prior[p_a$nlpar == "nec"]))
  expect_true(grepl("normal", p_a$prior[p_a$nlpar == "top"]))
  expect_true(p_b$bound[p_b$nlpar == "top"] == "")
  expect_true(p_b$bound[p_b$nlpar == "bot"] == "")
  expect_true(grepl("normal", p_b$prior[p_b$nlpar == "top"]))
  expect_true(grepl("normal", p_b$prior[p_b$nlpar == "bot"]))
  expect_false(p_c$bound[p_c$nlpar == "top"] == "")
  expect_false(p_c$bound[p_c$nlpar == "bot"] == "")
  expect_true(grepl("beta", p_c$prior[p_c$nlpar == "top"]))
  expect_true(grepl("beta", p_c$prior[p_c$nlpar == "bot"]))
})
