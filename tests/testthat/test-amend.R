require(bayesnec)
  
a1 <- amend(manec_gausian_identity, loo_controls = list(method = "stacking"))
a2 <- amend(manec_gausian_identity, add = "ecxlin")

test_that("input checks work correctly and return appropriate messages", {
 expect_message(amend(manec_gausian_identity), 
                 "Nothing to amend, please specify a model to either add or drop, or a weighting method via loo_controls")
 expect_message(expect_message(expect_message(amend(manec_gausian_identity, drop = "nec3param"), 
                 "Nothing to amend, please specify a model to either add or drop that differs from the original set"),
                 "Returning original model set"),  "weighting method not modified, please call amend and specify only loo_controls if you do not need to drop or add any models and simply want to update the weighting method.")
 expect_error(amend(manec_gausian_identity, loo_controls = list(method="somethingwrong")), 
              'The weighting method you have supplied is invalid, it must be one of "stacking" or "pseudobma"')
 expect_message(amend(manec_gausian_identity, drop = "nec4param"))
 expect_message(expect_message(amend(manec_gausian_identity, add = "nec3param")), "Fitted models are:  nec4param ecx4param")  
 expect_message(
  expect_message(
   expect_message(amend(manec_gausian_identity, add = "nec4param", loo_controls = list(method = "pseudobma"))),
               "Returning original model set"),
  "weighting method not modified, please call amend and specify only loo_controls if you do not need to drop or add any models and simply want to update the weighting method.")
expect_message(
  expect_message(
    amend(manec_gausian_identity, loo_controls = list(method = "pseudobma")),
    "Returning original model set"),   
  "weighting method specified is the same as the original.") 
})

test_that("loo_controls pass correctly", {
  expect_equal(class(a1$mod_stats$wi), "stacking_weights")
})

test_that("models drop and add work correctly", {
  expect_equal(class(a1$mod_stats$wi), "stacking_weights")
  expect_message(amend(a2, drop = "nec4param"), "Fitted models are:  ecx4param ecxlin")
  expect_equal(names(a2$mod_fits), c("nec4param", "ecx4param", "ecxlin"))
})







