library(bayesnec)

test_that("bayesnecformula is agnostic with respect to the formula structure", {
  expect_identical(bayesnecformula(y ~ crf(x, "nec3param")),
                   bnf(y ~ crf(x, "nec3param")))
  expect_s3_class(bnf(log(y) | trials(tr) ~ crf(sqrt(x), "nec3param")),
                  "bayesnecformula")
  expect_s3_class(bnf(log(y) | trials(tr) ~ crf(sqrt(x), "nec3param")),
                  "formula")
  expect_s3_class(bnf(y ~ x), "formula")
  expect_s3_class(bnf(y ~ x), "bayesnecformula")
  expect_s3_class(bnf(0 ~ x), "bayesnecformula")
  expect_s3_class(bnf("0 ~ x"), "bayesnecformula")
  expect_s3_class(bnf(y ~ crf(scale(x, scale = TRUE), "nec3param")),
                  "bayesnecformula")
})

test_that("bayesnecformula is equivalent to bnf", {
  expect_identical(bayesnecformula(y ~ crf(x, "nec3param")),
                   bnf(y ~ crf(x, "nec3param")))
  expect_identical(bayesnecformula(y ~ crf(x, "nec3param")),
                   bnf('y ~ crf(x, "nec3param")'), ignore_formula_env = TRUE)
})
