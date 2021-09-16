library(bayesnec)
library(brms)

test_that("predict is silent", {
  expect_silent(predict(nec4param))
})

test_that("predict output is a list of appropriately name elements", {
  pred_p <- predict(nec4param)
  expect_equal(class(pred_p), "list")
  expect_equal(length(pred_p), 2)
  expect_equal(names(pred_p), c("data", "posterior"))
})

test_that("plot returns null, is invisible, and is silent", {
  expect_null(plot(nec4param))
  expect_silent(plot(nec4param))
  expect_invisible(plot(nec4param))
})

test_that("summary behaves as expected", {
  summary_p <- summary(nec4param) %>%
    suppressWarnings
  expect_equal(class(summary_p), "necsummary")
  expect_equal(names(summary_p), c("brmssummary", "model", "is_ecx", "ecs",
                                   "bayesr2"))
})

test_that("formula/model.frame behaves as expected", {
  expect_s3_class(formula(nec4param), "brmsformula")
  expect_s3_class(model.frame(nec4param), "data.frame")
})
