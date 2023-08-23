

test_that("works for bayesnecfit", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  nsec1 <- nsec(fit, precision = 10)
  expect_equal(length(nsec1), 3)
  expect_equal(names(nsec1), c("Q50", "Q2.5", "Q97.5"))
})

test_that("works for brmsfit pulled from bayesnecfit", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  nsec1 <- nsec(fit_brms, precision = 10, x_var = "x") |>
    suppressWarnings()
  expect_equal(length(nsec1), 3)
  expect_equal(names(nsec1), c("Q50", "Q2.5", "Q97.5"))
})

test_that("works for brmsfit with interaction", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  nsec1 <- nsec(fit_brmsInt, precision = 10, x_var = "x", group_var = "herbicide") |>
    suppressWarnings()
  expect_equal(length(nsec1), 3)
  expect_equal(names(nsec1), c("Q50", "Q2.5", "Q97.5"))
})

test_that("fails for brmsfit when x_var is not specified", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  expect_error(nsec(fit_brms))
})

test_that("posterior passes correctly for brmsfit pulled from bayesnecfit", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  nsec3 <- nsec(fit_brms, posterior = TRUE, precision = 10, x_var = "x")
  expect_equal(length(nsec3), 8000)
})

test_that("fails for brmsfit when by_group is true and group_var is not specified", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  expect_error(nsec(fit_brmsInt, x_var = "x", by_group = TRUE, 
                    "You must specify a group_by variable if you want values returned by groups."))
})

test_that("works for brmsfit when by_group is true", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  nsec4 <- nsec(fit_brmsInt, x_var = "x", by_group = TRUE, group_var = "herbicide")
  expect_equal(nrow(nsec4), 7)
  expect_equal(ncol(nsec4), 4)
  expect_equal(colnames(nsec4), c("Qherbicide", "Q50", "Q2.5", "Q97.5"))
})
  
