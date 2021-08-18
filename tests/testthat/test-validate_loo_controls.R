library(bayesnec)

test_that("rejects non-lists", {
  err <- "loo_controls should be a named list of two elements\\. See \\?bnec"
  fam_tag <- ""
  expect_error(validate_loo_controls(10, fam_tag), err)
  expect_error(validate_loo_controls(NA, fam_tag), err)
  expect_error(validate_loo_controls(NULL, fam_tag), err)
  expect_error(validate_loo_controls("a", fam_tag), err)
  expect_error(validate_loo_controls(TRUE, fam_tag), err)
})

test_that("rejects lists with incorrect names", {
  err <- "loo_controls list names are incorrect\\. See \\?bnec"
  expect_true(is.list(validate_loo_controls(list(), fam_tag)))
  expect_equal(names(validate_loo_controls(list(), fam_tag)),
               c("fitting", "weights"))
  expect_error(validate_loo_controls(list(wrong_name = NA), fam_tag), err)
  # even if both correct names are there
  expect_error(
    validate_loo_controls(
      list(wrong_name = NA, fitting = 1), fam_tag
    ),
    err
  )
  expect_error(
    validate_loo_controls(list(wrong_name = NA, fitting = 1, weights = 10),
                          fam_tag),
    err
  )
})

test_that("both names correct but structures are wrong", {
  e_f <- "Element \"fitting\" of loo_controls should be a list\\. See \\?bnec"
  e_w <- "Element \"weights\" of loo_controls should be a list\\. See \\?bnec"
  expect_error(
    validate_loo_controls(list(fitting = 1, weights = list()), fam_tag),
    e_f
  )
  expect_error(
    validate_loo_controls(list(fitting = "a", weights = list()), fam_tag),
    e_f
  )
  expect_error(
    validate_loo_controls(list(fitting = NA, weights = list()), fam_tag),
    e_f
  )
  expect_error(
    validate_loo_controls(list(fitting = NULL, weights = list()), fam_tag),
    e_f
  )
  expect_error(
    validate_loo_controls(list(fitting = list(), weights = 10), fam_tag),
    e_w
  )
  expect_true(
    is.list(
      validate_loo_controls(list(fitting = list(), weights = list()), fam_tag)
    )
  )
  expect_equal(
    names(
      validate_loo_controls(list(fitting = list(), weights = list()), fam_tag)
    ),
    c("fitting", "weights")
  )
})

test_that("fails for beta-binomial pointwise = TRUE", {
  e_ <- "You cannot currently set loo_controls"
  expect_error(
    validate_loo_controls(list(fitting = list(pointwise = TRUE)), "custom"),
    e_
  )
})

test_that("accepts one correct name", {
  expect_true(
    is.list(
      validate_loo_controls(list(fitting = list()), fam_tag)
    )
  )
  expect_true(
    is.list(
      validate_loo_controls(list(weights = list()), fam_tag)
    )
  )
  expect_equal(
    names(
      validate_loo_controls(list(fitting = list()), fam_tag)
    ),
    c("fitting", "weights")
  )
  expect_equal(
    names(
      validate_loo_controls(list(weights = list()), fam_tag)
    ),
    c("weights", "fitting")
  )
})
