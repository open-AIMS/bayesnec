library(bayesnec)
library(brms)

data(manec_example)

test_that("predict is silent", {
  expect_silent(predict(manec_example))
})

test_that("predict/fitted is a matrix of appropriately name elements", {
  pred_p <- predict(manec_example)
  expect_equal(class(pred_p), c("matrix", "array"))
  expect_equal(dim(pred_p), c(100, 4))
  expect_equal(colnames(pred_p), c("Estimate", "Est.Error", "Q2.5", "Q97.5"))
  fitt_p <- fitted(manec_example)
  expect_equal(class(fitt_p), c("matrix", "array"))
  expect_equal(dim(fitt_p), c(100, 4))
  expect_equal(colnames(fitt_p), c("Estimate", "Est.Error", "Q2.5", "Q97.5"))
})

test_that("plot returns null, is invisible, and is silent", {
  expect_null(plot(manec_example))
  expect_silent(plot(manec_example))
  expect_invisible(plot(manec_example))
})

test_that("rhat behaves as expected", {
  rhat_p <- suppressMessages(rhat(manec_example))
  rhat2_p <-  rhat(manec_example, rhat_cutoff = 1)
  expect_message(rhat(manec_example, rhat_cutoff = 1))
  expect_equal(names(rhat2_p), manec_example$success_models)
  expect_equal(names(rhat_p[[1]]), c("rhat_vals", "failed"))
})

test_that("summary behaves as expected", {
  summary.p <- suppressWarnings(summary(manec_example))
  expect_equal(class(summary.p), "manecsummary")
  expect_equal(names(summary.p), c("models", "family", "sample_size",
                                   "mod_weights", "mod_weights_method",
                                   "ecx_mods", "nec_vals", "ecs", "bayesr2",
                                   "rhat_issues"))
})

test_that("formula behaves as expected", {
  expect_error(formula(manec_example))
  expect_error(formula(manec_example, "nec4param"))
  expect_s3_class(formula(manec_example, model = "nec4param"), "brmsformula")
  expect_s3_class(formula(manec_example, model = "ecx4param"), "brmsformula")
  expect_error(formula(manec_example, model = "ecxlin"))
})

test_that("model.frame behaves as expected", {
  expect_error(model.frame(manec_example))
  expect_error(model.frame(manec_example, "nec4param"))
  expect_s3_class(model.frame(manec_example, model = "nec4param"),
                  "data.frame")
  expect_s3_class(model.frame(manec_example, model = "ecx4param"),
                  "data.frame")
  expect_error(model.frame(manec_example, model = "ecxlin"))
})
