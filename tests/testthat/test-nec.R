library(bayesnec)
library(brms)

data(manec_example)

test_that("nec returns expected object types", {
  nec_summary <- nec(manec_example) %>%
    suppressMessages
  expect_equal(length(nec_summary), 3)
})

test_that("doesn't work for ecx models", {
  expect_error(nec(ecx4param), "nec is not a parameter in ecx model types.")
})
  
test_that("works for bayesnecfit", {
  nec1 <- nec(nec4param)
  expect_equal(length(nec1), 3)
  expect_equal(names(nec1), c("50%", "2.5%", "97.5%"))
})

test_that("works for bayesmanecfit", {
  mes <- paste0("bayesmanecfit contains ecx model types and therefore nec",
                " estimate includes nsec values.")
  nec1 <- nec(manec_example) %>%
    suppressMessages
  expect_message(nec(manec_example), mes)
  expect_equal(length(nec1), 3)
  expect_equal(names(nec1), c("50%", "2.5%", "97.5%"))
})

test_that("xform passes correctly", {
  nec1 <- nec(nec4param)
  nec2 <- nec(nec4param, xform = exp)
  expect_gt(nec2[1], nec1[2])
})

test_that("posterior passes correctly", {
  nec3 <- nec(nec4param, posterior = TRUE)
  expect_equal(length(nec3), 100)
})

test_that("prob_vals passes correctly", {
  nec4 <- nec(nec4param, prob_vals = c(0.3, 0.5, 0.7))
  expect_equal(names(nec4), c("30%", "50%", "70%"))
})
