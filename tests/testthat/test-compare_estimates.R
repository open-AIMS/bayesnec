test_that("x must be a named list", {
  expect_error(compare_estimates(list(ecx4param, nec4param)))
  expect_error(compare_estimates(ecx4param, nec4param))
})

test_that("output is a list of appropriately name elements", {
  ce <- compare_estimates(list(ecx4param = ecx4param, nec4param = nec4param))
  expect_equal(class(ce), "list")
  expect_equal(length(ce), 5)
  expect_equal(names(ce), c("posterior_list", "posterior_data", "diff_list",
                            "diff_data", "prob_diff"))
})


# ---- the four-value type vocabulary ------------------------------------------

# compare_estimates() and compare_posterior() forward type to ecx(), but kept a
# second copy of the vocabulary that still listed the 2.1.3 three-value set. It
# therefore refused "range" -- the name the rename warning gives callers for the
# behaviour they had -- so the migration the warning names was impossible from
# the two functions that exist to compare an ECx across fits.

test_that("compare_estimates accepts every type ecx accepts", {
  skip_on_cran()
  x <- list(ecx4param = ecx4param, nec4param = nec4param)
  for (ty in c("absolute", "range")) {
    ce <- compare_estimates(x, comparison = "ecx", type = ty, resolution = 50)
    expect_equal(names(ce), c("posterior_list", "posterior_data", "diff_list",
                              "diff_data", "prob_diff"))
  }
  expect_error(compare_estimates(x, comparison = "ecx", type = "nonsense"),
               "type must be one of")
})

test_that("the relative rename is warned once for the call, not once per fit", {
  skip_on_cran()
  x <- list(ecx4param = ecx4param, nec4param = nec4param)
  w <- testthat::capture_warnings(
    compare_estimates(x, comparison = "ecx", type = "relative",
                      resolution = 50)
  )
  expect_equal(sum(grepl("now measures from the control", w)), 1)
})

test_that("average_estimates validates type on the same vocabulary", {
  skip_on_cran()
  x <- list(ecx4param = ecx4param, nec4param = nec4param)
  expect_error(average_estimates(x, estimate = "ecx", type = "nonsense"),
               "type must be one of")
  out <- average_estimates(x, estimate = "ecx", type = "range",
                           resolution = 50)
  expect_equal(length(out), 3)
})
