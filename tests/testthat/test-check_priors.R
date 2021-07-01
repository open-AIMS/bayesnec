library(bayesnec)
library(ggplot2)

data(manec_example)
nec4param <- pull_out(manec_example, model = "nec4param")

test_that("returns ggplot for bayesnecfit objects", {
  p <- check_priors(nec4param)
  expect_error(print(p), NA)
  expect_silent(check_priors(nec4param))
  expect_equal(class(p), c("gg", "ggplot"))
})

test_that("returns pdf for bayesmanecfit objects", {
  filename <- random_filename(15)
  expect_invisible(check_priors(manec_example, filename = filename))
  on.exit(file.remove(paste(filename, ".pdf", sep = "")))
})
