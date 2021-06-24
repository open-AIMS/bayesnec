library(bayesnec)
library(brms)

manec_gauss_id_2 <- bayesnec:::manec_gauss_id_2

test_that("predict is silent", {
  expect_silent(predict(manec_gauss_id_2))
})

test_that("predict output is a list of appropriately name elements", {
  pred_p <- predict(manec_gauss_id_2)
  expect_equal(class(pred_p), "list")
  expect_equal(length(pred_p), 2)
  expect_equal(names(pred_p), c("data", "posterior"))
})

test_that("plot returns null, is invisible, and is silent", {
  expect_null(plot(manec_gauss_id_2))
  expect_silent(plot(manec_gauss_id_2))
  expect_invisible(plot(manec_gauss_id_2))
})

test_that("rhat behaves as expected", {
  rhat_p <- rhat(manec_gauss_id_2)
  rhat2_p <-  rhat(manec_gauss_id_2, rhat_cutoff = 1)
  expect_message(rhat(manec_gauss_id_2, rhat_cutoff = 1))
  expect_equal(names(rhat2_p), c("rhat_vals", "failed"))
  expect_equal(names(rhat_p$rhat_vals), names(manec_gauss_id_2$mod_fits))
})

test_that("summary behaves as expected", {
  summary.p <- summary(manec_gauss_id_2)
  expect_equal(class(summary.p), "manecsummary")
  expect_equal(names(summary.p), c("models", "family", "sample_size",
                                   "mod_weights", "mod_weights_method",
                                   "ecx_mods", "nec_vals", "ecs",
                                   "rhat_issues"))
})
