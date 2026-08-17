# Structural tests for the bayesnechurdlefit methods. None require sampling:
# they use a mock built from the pieces each method actually reads.
mock_hurdle <- function() {
  mk <- function(model) structure(list(model = model), class = c("bayesnecfit", "bnecfit"))
  structure(list(growth = mk("nec3param"), survival = mk("nec3param"),
                 data = data.frame(x = 1:4, y = c(2, 1, 0, 0)),
                 formula = bnf(y ~ crf(x, "nec3param")),
                 y_var = "y", n_exposed = 4L, n_dead = 2L),
            class = c("bayesnechurdlefit", "bnecfit"))
}

test_that("which defaults to combined and rejects anything else", {
  expect_equal(bayesnec:::hurdle_check_which("combined"), "combined")
  expect_equal(bayesnec:::hurdle_check_which("growth"), "growth")
  expect_error(bayesnec:::hurdle_check_which("mu"), "should be one of")
  # plots additionally allow "all" for the three-panel form
  expect_equal(bayesnec:::hurdle_check_plot_which("all"), "all")
  expect_error(bayesnec:::hurdle_check_which("all"), "should be one of")
})

test_that("combined is the default for every curve method", {
  for (f in list(posterior_epred.bayesnechurdlefit, fitted.bayesnechurdlefit,
                 predict.bayesnechurdlefit, posterior_predict.bayesnechurdlefit,
                 nsec.bayesnechurdlefit, ecx.bayesnechurdlefit,
                 nec.bayesnechurdlefit)) {
    expect_equal(eval(formals(f)$which), "combined")
  }
  expect_equal(eval(formals(plot.bayesnechurdlefit)$which), "combined")
  expect_equal(eval(formals(autoplot.bayesnechurdlefit)$which), "combined")
})

test_that("hurdle_raw_data recovers the response and a transformed predictor", {
  o <- mock_hurdle()
  d <- bayesnec:::hurdle_raw_data(o)
  expect_equal(d$y, c(2, 1, 0, 0))
  expect_equal(d$x, 1:4)
  o$formula <- bnf(y ~ crf(log(x), "nec3param"))
  expect_equal(bayesnec:::hurdle_raw_data(o)$x, log(1:4))
})

test_that("delegating methods return one element per component", {
  o <- mock_hurdle()
  out <- bayesnec:::hurdle_delegate(o, function(z, ...) z$model)
  expect_equal(out, list(growth = "nec3param", survival = "nec3param"))
})

test_that("combining refuses objects that are not hurdle fits", {
  o <- mock_hurdle()
  plain <- structure(list(model = "nec3param"), class = c("bayesnecfit", "bnecfit"))
  expect_error(bayesnec:::hurdle_check_pair(list(o, plain)), "class bayesnechurdlefit")
  expect_true(bayesnec:::hurdle_check_pair(list(o, o)))
})

test_that("combining refuses fits on different responses", {
  o <- mock_hurdle(); p <- mock_hurdle(); p$y_var <- "z"
  expect_error(bayesnec:::hurdle_check_pair(list(o, p)), "same response")
})

test_that("update() re-splits newdata rather than passing it through", {
  # Zeros must reach the survival component as the hurdle and be withheld from
  # the growth component, exactly as bnec_hurdle() does when first fitting.
  # Checked on the split itself rather than through a fit.
  nd <- data.frame(x = 1:6, y = c(5, 4, 3, 0, 0, 0))
  y <- nd$y
  growth_rows <- nd[y > 0, , drop = FALSE]
  surv <- nd; surv$.alive <- as.integer(y > 0)
  expect_equal(nrow(growth_rows), 3)
  expect_false(any(growth_rows$y == 0))
  expect_equal(nrow(surv), 6)
  expect_equal(surv$.alive, c(1L, 1L, 1L, 0L, 0L, 0L))
  # and update() carries that split, not the raw frame
  # collapsed, because deparse() wraps mid-expression
  src <- paste(deparse(update.bayesnechurdlefit), collapse = " ")
  expect_match(src, "newdata\\[y >\\s*0")
  expect_match(src, "\\.alive")
})

test_that("update() requires the response column in newdata", {
  o <- mock_hurdle()
  expect_error(update(o, newdata = data.frame(x = 1:3)),
               "must contain the response column")
})

test_that("update() re-checks the zero-vs-censored invariant on newdata", {
  # The invariant is a property of the data, so a refit has to re-check it.
  # Otherwise a row that is both zero and censored is dropped from the growth
  # refit and coded as a death in the survival refit, with no message.
  o <- mock_hurdle()
  o$formula <- bnf(y | cens(cens) ~ crf(x, "nec3param"))
  bad <- data.frame(x = 1:6, y = c(5, 4, 3, 0, 0, 0),
                    cens = c(rep("none", 5), "left"))
  expect_error(update(o, newdata = bad),
               "zero and also carry a censoring code")
  # A censored survivor is a growth observation and must still get past the
  # check. The mock has no real component fit, so update() fails regardless --
  # the point is only that it does not fail on the invariant.
  ok <- bad
  ok$cens <- c("left", rep("none", 5))
  msg <- tryCatch(update(o, newdata = ok), error = conditionMessage)
  expect_false(grepl("zero and also carry", msg))
})

test_that("summary carries its own class and print method", {
  expect_true(!is.null(getS3method("print", "hurdlesummary", optional = TRUE)))
  expect_true(!is.null(getS3method("summary", "bayesnechurdlefit", optional = TRUE)))
})

test_that("every audited method has a bayesnechurdlefit method or a branch", {
  # Guards against a method being added for bayesnecfit later without a
  # matching hurdle path, which is how the silent failures arose originally.
  s3 <- c("summary", "print", "plot", "autoplot", "ggbnec_data", "predict",
          "fitted", "posterior_epred", "posterior_predict", "nsec", "nec",
          "ecx", "ecnsec", "rhat", "check_chains", "check_priors",
          "model.frame", "pull_brmsfit", "bnec_newdata",
          "update", "amend", "c")
  for (g in s3) {
    expect_true(!is.null(getS3method(g, "bayesnechurdlefit", optional = TRUE)),
                label = paste0(g, ".bayesnechurdlefit"))
  }
  # pull_out, dispersion and compare_fitted are plain functions, not generics,
  # so an S3 method for them would be silently inert -- they branch internally.
  for (f in list(bayesnec:::pull_out, bayesnec:::dispersion,
                 bayesnec:::newdata_eval_fitted)) {
    expect_true(any(grepl("is_bayesnechurdlefit", deparse(f))))
  }
  for (f in c("pull_out", "dispersion", "compare_fitted")) {
    expect_false(any(grepl("UseMethod", deparse(get(f)))), label = f)
  }
})
