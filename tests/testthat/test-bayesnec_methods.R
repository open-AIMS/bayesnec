library(bayesnec)
library(brms)

test_that("predict is silent", {
  expect_silent(predict(nec_gausian_identity))
})

test_that("predict output is a list of appropriately name elements", {
  pred.p <- predict(nec_gausian_identity)
  expect_equal(class(pred.p), "list")
  expect_equal(length(pred.p), 2)  
  expect_equal(names(pred.p), c("data", "posterior"))
})

test_that("plot returns null, is invisible, and is silent", {
  expect_null(plot(nec_gausian_identity))  
  expect_silent(plot(nec_gausian_identity))   
  expect_invisible(plot(nec_gausian_identity))   
})

test_that("summary behaves as expected", {
  summary.p <- summary(nec_gausian_identity)
  expect_equal(class(summary.p), "necsummary")
  expect_equal(names(summary.p), c("brmssummary", "model", "is_ecx", "ecs"))
})
