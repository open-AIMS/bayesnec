require(bayesnec)

test_that("input errors work as intended", {
  expect_error(compare_posterior(list(ecx4param, nec4param)), "Argument x must be a named list")
  expect_error(compare_posterior(ecx4param, nec4param), "Argument x must be a named list")
  expect_error(compare_posterior(list(g=ecx4param, h=12)), "Not all objects in x are of class bayesnecfit or bayesmanecfit")
})

test_that("output is a list of appropriately name elements", {
  cp <- compare_posterior(list(ecx4param = ecx4param, nec4param = nec4param))
  expect_equal(class(cp), "list")
  expect_equal(length(cp), 5)  
  expect_equal(names(cp), c("posterior_list", "posterior_data", "diff_list", "diff_data", "prob_diff"))
})


