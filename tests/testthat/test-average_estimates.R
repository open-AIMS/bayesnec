test_that("x must be a named list", {
  expect_error(average_estimates(list(ecx4param, nec4param)))
  expect_error(average_estimates(ecx4param, nec4param))
})

test_that("output is a vector of appropriately name elements", {
  ae1 <- average_estimates(list(ecx4param = ecx4param, nec4param = nec4param))
  expect_equal(length(ae1), 3)
  expect_equal(names(ae1), c("Q50", "Q2.5", "Q97.5"))
})

test_that("xform passes correctly", {
  ae1 <- average_estimates(list(ecx4param = ecx4param, nec4param = nec4param))
  ae2 <- average_estimates(list(ecx4param = ecx4param, nec4param = nec4param),
                           xform = exp)
  expect_gt(ae2[1], ae1[2])
})

test_that("posterior passes correctly", {
  ae3 <- average_estimates(list(ecx4param = ecx4param, nec4param = nec4param),
                           posterior = TRUE)
  expect_equal(length(ae3), 100)
})

test_that("prob_vals passes correctly", {
  ae4 <- average_estimates(list(ecx4param = ecx4param, nec4param = nec4param),
                           prob_vals = c(0.5, 0.3, 0.7))
  expect_equal(names(ae4), c("Q50", "Q30", "Q70"))
})
