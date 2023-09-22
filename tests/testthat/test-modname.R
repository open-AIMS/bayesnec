test_that("model names remain less than 15 charactes", {
  model_list <- unique(unlist(models()))
  expect_lte(max(sapply(model_list, nchar)), 15)
})
