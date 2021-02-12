


test_that("handle_set works", {
  expect_message(expect_false(bayesnec:::handle_set(c("nec4param", "nec3param"), add = "nec4param")),
                 "Nothing to amend, please specify a model to either add or drop that differs from the original set")
  expect_message(expect_false(bayesnec:::handle_set(c("nec4param", "nec3param"))),
                 "Nothing to amend, please specify a model to either add or drop that differs from the original set")
  expect_error(bayesnec:::handle_set(c("nec4param", "nec3param"), drop = c("nec4param", "nec3param")),
                 "All models removed, nothing to return")
  expect_equal(bayesnec:::handle_set(c("nec4param", "nec3param"), add = c("ecx4param", "ecxlin")),
               c("nec4param", "nec3param", "ecx4param", "ecxlin"))
  expect_equal(bayesnec:::handle_set(c("nec4param", "nec3param", "ecx4param",  "ecxlin"), 
                                     drop = c("ecxlin","nec4param", "nec3param")), c("ecx4param"))
})


