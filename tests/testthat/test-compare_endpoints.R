library(bayesnec)

data(manec_example)
ecx4param <- pull_out(manec_example, model = "ecx4param")
nec4param <- pull_out(manec_example, model = "nec4param")

test_that("x must be a named list", {
  expect_error(compare_endpoints(list(ecx4param, nec4param)))
  expect_error(compare_endpoints(ecx4param, nec4param))
})

test_that("output is a list of appropriately name elements", {
  ce <- compare_endpoints(list(ecx4param = ecx4param, nec4param = nec4param))
  expect_equal(class(ce), "list")
  expect_equal(length(ce), 5)
  expect_equal(names(ce), c("posterior_list", "posterior_data", "diff_list",
                            "diff_data", "prob_diff"))
})
