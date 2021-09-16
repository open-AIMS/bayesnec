library(bayesnec)

test_that("x must be a named list", {
  expect_error(compare_fitted(list(ecx4param, nec4param)))
  expect_error(compare_fitted(ecx4param, nec4param))
})

test_that("output is a list of appropriately name elements", {
  cf <- compare_fitted(list(ecx4param = ecx4param, nec4param = nec4param),
                       precision = 10)
  expect_equal(class(cf), "list")
  expect_equal(length(cf), 5)
  expect_equal(names(cf), c("posterior_list", "posterior_data", "diff_list",
                            "diff_data", "prob_diff"))
})
