library(bayesnec)

data(manec_example)

data(nec_data)
nec_data$y <- as.integer(round(nec_data$y * 100))
nec4param <- bnec(y ~ crf(x, "nec4param"), data = nec_data, chains = 2) %>%
  suppressMessages %>%
  suppressWarnings

ecx4param <- pull_out(manec_example, model = "ecx4param") %>%
  suppressMessages %>%
  suppressWarnings

test_that("dispersion fails because family is gaussian", {
  expect_length(dispersion(ecx4param), 0)
  expect_length(dispersion(ecx4param, summary = TRUE), 0)
})

test_that("dispersion works because family is binomial", {
  expect_length(dispersion(nec4param), 4e3)
  expect_length(dispersion(nec4param, summary = TRUE), 3)
  expect_type(dispersion(nec4param), "double")
  expect_type(dispersion(nec4param, summary = TRUE), "double")
})
