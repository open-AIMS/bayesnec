test_that("predict is silent", {
  expect_silent(predict(manec_example))
})

test_that("predict/fitted is a matrix of appropriately name elements", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  pred_p <- predict(manec_example)
  expect_equal(class(pred_p), c("matrix", "array"))
  expect_equal(dim(pred_p), c(100, 4))
  expect_equal(colnames(pred_p), c("Estimate", "Est.Error", "Q2.5", "Q97.5"))
  fitt_p <- fitted(manec_example)
  expect_equal(class(fitt_p), c("matrix", "array"))
  expect_equal(dim(fitt_p), c(100, 4))
  expect_equal(colnames(fitt_p), c("Estimate", "Est.Error", "Q2.5", "Q97.5"))
})

test_that("plot returns null, is invisible, and is silent", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  expect_null(plot(manec_example))
  expect_silent(plot(manec_example))
  expect_invisible(plot(manec_example))
})

test_that("rhat behaves as expected", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  rhat_p <- suppressMessages(rhat(manec_example))
  rhat2_p <-  rhat(manec_example, rhat_cutoff = 1)
  expect_message(rhat(manec_example, rhat_cutoff = 1))
  expect_equal(names(rhat2_p), manec_example$success_models)
  expect_equal(names(rhat_p[[1]]), c("rhat_vals", "failed"))
})

test_that("summary behaves as expected", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  summary.p <- suppressWarnings(summary(manec_example))
  expect_equal(class(summary.p), "manecsummary")
  expect_equal(names(summary.p), c("models", "family", "sample_size",
                                   "mod_weights", "mod_weights_method",
                                   "ecx_mods", "nec_vals", "ecs", "bayesr2",
                                   "rhat_issues", "failed_models"))
})

test_that("formula behaves as expected", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  expect_error(formula(manec_example))
  expect_s3_class(formula(manec_example, "nec4param"), "bayesnecformula")
  expect_s3_class(formula(manec_example, model = "nec4param"),
                          "bayesnecformula")
  expect_s3_class(formula(manec_example, model = "ecx4param"),
                          "bayesnecformula")
  expect_null(formula(manec_example, model = "ecxlin"))
})

test_that("model.frame behaves as expected", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  expect_error(model.frame(manec_example))
  expect_s3_class(model.frame(manec_example, "nec4param"),
                  "data.frame")
  expect_s3_class(model.frame(manec_example, model = "nec4param"),
                  "data.frame")
  expect_s3_class(model.frame(manec_example, model = "ecx4param"),
                  "data.frame")
  expect_error(model.frame(manec_example, model = "ecxlin"))
})

test_that("model-averaged output does not change between identical calls", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # #216: every quantity taken through the averaged posterior used to redraw the
  # component index with an unseeded sample(), so no two calls agreed. Asserted
  # on the expectation and on the summaries built from it, not on stored
  # constants -- the point is that repeated calls agree, whatever the values.
  nd <- bnec_newdata(manec_example, resolution = 20)
  expect_identical(posterior_epred(manec_example, newdata = nd),
                   posterior_epred(manec_example, newdata = nd))
  expect_identical(fitted(manec_example, newdata = nd),
                   fitted(manec_example, newdata = nd))
  # Edge case: a resolution of 1 is a single column, where a dropped dimension
  # would show up as a length mismatch rather than a difference in values.
  nd_1 <- bnec_newdata(manec_example, resolution = 1)
  expect_identical(posterior_epred(manec_example, newdata = nd_1),
                   posterior_epred(manec_example, newdata = nd_1))
  # posterior_predict() stays stochastic on purpose: it simulates new
  # observations, exactly as brms does for a single fit, so it varies for a
  # bayesnecfit too. What #216 fixes is the weighting, which is now the same
  # draw every time -- so seeding the likelihood is enough to pin it down.
  set.seed(216)
  p1 <- predict(manec_example, newdata = nd)
  set.seed(216)
  expect_identical(p1, predict(manec_example, newdata = nd))
})
