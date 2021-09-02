library(bayesnec)

data <- data.frame(pred = 1:10, resp = 1:10, tr = 10, wgt = 15,
                   g_1 = "a", g_2 = "b")

test_that("correct classes", {
  form <- bnf(resp ~ crf(sqrt(pred), "nec4param"))
  expect_s3_class(model.frame(form, data), "data.frame")
  expect_error(model.frame(form, as.matrix(data)), "is not a data.frame")
  a <- model.frame(form, data)
  expect_identical(names(a), c("resp", "sqrt(pred)"))
  expect_identical(names(attributes(a)), c("names", "terms", "row.names",
                                           "class", "bnec_pop", "bnec_group"))
  expect_identical(names(attributes(a)$bnec_pop), c("y_var", "x_var"))
  expect_equal(attributes(a)$bnec_pop, c("resp", "pred"), ignore_attr = TRUE)
  expect_equal(attributes(a)$bnec_group, NA)
  # trials is incorporated
  form2 <- bnf(resp | trials(tr) ~ crf(sqrt(pred), "nec4param"))
  expect_s3_class(model.frame(form2, data), "data.frame")
  b <- model.frame(form2, data)
  expect_identical(names(b), c("resp", "sqrt(pred)", "trials(tr)"))
  expect_identical(names(attributes(b)), c("names", "terms", "row.names",
                                           "class", "bnec_pop", "bnec_group"))
  expect_identical(names(attributes(b)$bnec_pop),
                   c("y_var", "x_var", "trials_var"))
  expect_equal(attributes(b)$bnec_pop, c("resp", "pred", "tr"),
               ignore_attr = TRUE)
  expect_equal(attributes(b)$bnec_group, NA)
  # other aterms are not incorporated--allows checks to be done via brms
  form3 <- bnf(resp | trials(tr) + weights(wgt) ~ crf(sqrt(pred), "nec4param"))
  expect_s3_class(model.frame(form3, data), "data.frame")
  c_ <- model.frame(form3, data)
  expect_identical(names(c_), c("resp", "sqrt(pred)", "trials(tr)"))
  expect_identical(names(attributes(c_)), c("names", "terms", "row.names",
                                           "class", "bnec_pop", "bnec_group"))
  expect_identical(names(attributes(c_)$bnec_pop),
                   c("y_var", "x_var", "trials_var"))
  expect_equal(attributes(c_)$bnec_pop, c("resp", "pred", "tr"),
               ignore_attr = TRUE)
  expect_equal(attributes(c_)$bnec_group, NA)
  # group-level effects are incorporated
  form4 <- bnf(resp ~ crf(sqrt(pred), "nec4param") + ogl(g_2) + pgl(g_1))
  expect_s3_class(model.frame(form4, data), "data.frame")
  d <- model.frame(form4, data)
  expect_identical(names(d), c("resp", "sqrt(pred)", "g_1", "g_2"))
  expect_identical(names(attributes(d)), c("names", "terms", "row.names",
                                           "class", "bnec_pop", "bnec_group"))
  expect_identical(names(attributes(d)$bnec_pop), c("y_var", "x_var"))
  expect_equal(attributes(d)$bnec_pop, c("resp", "pred"), ignore_attr = TRUE)
  expect_equal(attributes(d)$bnec_group, c("g_1", "g_2"))
})
