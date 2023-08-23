library(bayesnec)

data <- data.frame(pred = 1:10, resp = 1:10)

test_that("correct classes", {
  all_models <- models("all")
  for (i in seq_along(all_models)) {
    model <- names(all_models)[i]
    form <- bnf(paste0("resp ~ crf(sqrt(pred), \"", model, "\")"))
    bdat <- model.frame(form, data)
    expect_s3_class(wrangle_model_formula(model, bnf(form), bdat),
                    "brmsformula")
  }
})
