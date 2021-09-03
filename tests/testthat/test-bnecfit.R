library(bayesnec)
library(dplyr)

data(manec_example)
nec_ <- pull_out(manec_example, "nec4param")
ecx_ <- pull_out(manec_example, "ecx4param")

test_that("Adding only works if either if bnecfit", {
  expect_error(1 + nec_)
  expect_error(NA + nec_)
  expect_error(NULL + nec_)
  expect_error("a" + nec_)
  expect_s3_class(ecx_ + nec_, c("bayesmanecfit", "bnecfit")) %>%
    expect_message
  expect_s3_class(manec_example + nec_, c("bayesmanecfit", "bnecfit")) %>%
    expect_message %>%
    expect_warning %>%
    expect_warning %>%
    expect_warning %>%
    expect_warning
  expect_s3_class(manec_example + manec_example,
                  c("bayesmanecfit", "bnecfit")) %>%
    expect_message %>%
    expect_warning %>%
    expect_warning %>%
    expect_warning %>%
    expect_warning
  expect_s3_class(manec_example + nec_ + ecx_,
                  c("bayesmanecfit", "bnecfit")) %>%
    expect_message %>%
    expect_message %>%
    expect_warning %>%
    expect_warning %>%
    expect_warning %>%
    expect_warning %>%
    expect_warning %>%
    expect_warning %>%
    expect_warning %>%
    expect_warning
})

test_that("Concatenating only works if either if bnecfit", {
  expect_true(is.list(c(1, nec_)))
  expect_true(is.list(c(NA, nec_)))
  expect_true(is.list(c(NULL, nec_)))
  expect_true(is.list(c("a", nec_)))
  expect_s3_class(c(ecx_, nec_), c("bayesmanecfit", "bnecfit")) %>%
    expect_message
  expect_s3_class(c(manec_example, nec_), c("bayesmanecfit", "bnecfit")) %>%
    expect_message %>%
    expect_warning %>%
    expect_warning %>%
    expect_warning %>%
    expect_warning
  expect_s3_class(c(manec_example, manec_example),
                  c("bayesmanecfit", "bnecfit")) %>%
    expect_message %>%
    expect_warning %>%
    expect_warning %>%
    expect_warning %>%
    expect_warning
  expect_s3_class(c(manec_example, nec_, ecx_),
                  c("bayesmanecfit", "bnecfit")) %>%
    expect_message %>%
    expect_warning %>%
    expect_warning %>%
    expect_warning %>%
    expect_warning
})
