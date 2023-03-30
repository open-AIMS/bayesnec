test_that("dispersion works because family is binomial", {
  nec_data$y <- as.integer(round(nec_data$y * 100))
  nec4param <- bnec(y ~ crf(x, "nec4param"), data = nec_data, chains = 2) |>
    suppressMessages() |>
    suppressWarnings()
  expect_length(dispersion(nec4param), 4e3)
  expect_length(dispersion(nec4param, summary = TRUE), 3)
  expect_type(dispersion(nec4param), "double")
  expect_type(dispersion(nec4param, summary = TRUE), "double")
})
