test_that("bnec_newdata refuses a resolution that is not a single whole number", {
  for (bad in list(0, 2.5, c(2, 3), NA_real_, "10", Inf)) {
    expect_error(bnec_newdata(nec4param, resolution = bad),
                 "single whole number of at least 1")
  }
  expect_equal(nrow(bnec_newdata(nec4param, resolution = 1)), 1)
  expect_equal(nrow(bnec_newdata(nec4param, resolution = 7)), 7)
})

test_that("bnec and amend refuse a fractional resolution before fitting", {
  # The grid check would otherwise stop the call after every model had been
  # fitted.
  expect_error(bnec(y ~ crf(x, "nec3param"), data = nec_data,
                    resolution = 100.5), "must be one whole number")
  expect_error(amend(manec_example, drop = "nec4param", resolution = 10.5),
               "must be one whole number")
})
