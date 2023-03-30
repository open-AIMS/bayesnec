test_that("predict is silent", {
  expect_silent(predict(nec4param))
})

test_that("predict output is a list of appropriately name elements", {
  pred_p <- predict(nec4param)
  expect_equal(class(pred_p), c("matrix", "array"))
  expect_equal(dim(pred_p), c(100, 4))
  expect_equal(colnames(pred_p), c("Estimate", "Est.Error", "Q2.5", "Q97.5"))
  fitt_p <- fitted(nec4param)
  expect_equal(class(fitt_p), c("matrix", "array"))
  expect_equal(dim(fitt_p), c(100, 4))
  expect_equal(colnames(fitt_p), c("Estimate", "Est.Error", "Q2.5", "Q97.5"))
})

test_that("plot returns null, is invisible, and is silent", {
  expect_null(plot(nec4param))
  expect_silent(plot(nec4param))
  expect_invisible(plot(nec4param))
})

test_that("summary behaves as expected", {
  summary_p <- suppressWarnings(summary(nec4param))
  expect_equal(class(summary_p), "necsummary")
  expect_equal(names(summary_p), c("brmssummary", "model", "is_ecx", "ecs",
                                   "bayesr2"))
})

test_that("formula/model.frame behaves as expected", {
  expect_s3_class(formula(nec4param), "bayesnecformula")
  expect_s3_class(model.frame(nec4param), "data.frame")
})
