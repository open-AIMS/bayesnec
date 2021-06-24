library(bayesnec)
library(brms)

manec_gausian_identity <- bayesnec:::manec_gausian_identity

test_that("predict is silent", {
  expect_silent(predict(manec_gausian_identity))
})

test_that("predict output is a list of appropriately name elements", {
  pred_p <- predict(manec_gausian_identity)
  expect_equal(class(pred_p), "list")
  expect_equal(length(pred_p), 2)
  expect_equal(names(pred_p), c("data", "posterior"))
})

test_that("plot returns null, is invisible, and is silent", {
  expect_null(plot(manec_gausian_identity))
  expect_silent(plot(manec_gausian_identity))
  expect_invisible(plot(manec_gausian_identity))
})

test_that("rhat behaves as expected", {
  rhat_p <- rhat(manec_gausian_identity)
  rhat2_p <-  rhat(manec_gausian_identity, rhat_cutoff = 1)
  expect_message(rhat(manec_gausian_identity, rhat_cutoff = 1))
  expect_equal(names(rhat2_p), c("rhat_vals", "failed"))
  expect_equal(names(rhat_p$rhat_vals), names(manec_gausian_identity$mod_fits))
})

test_that("summary behaves as expected", {
  summary.p <- summary(manec_gausian_identity)
  expect_equal(class(summary.p), "manecsummary")
  expect_equal(names(summary.p), c("models", "family", "sample_size",
                                   "mod_weights", "mod_weights_method",
                                   "ecx_mods", "nec_vals", "ecs",
                                   "rhat_issues"))
})
