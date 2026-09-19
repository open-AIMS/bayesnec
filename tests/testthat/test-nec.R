test_that("nec returns expected object types", {
  nec_summary <- suppressMessages(nec(manec_example))
  expect_equal(length(nec_summary), 3)
})

test_that("doesn't work for ecx models", {
  expect_error(nec(ecx4param), "nec is not a parameter in ecx model types.")
})
  
test_that("works for bayesnecfit", {
  nec1 <- nec(nec4param)
  expect_equal(length(nec1), 3)
  expect_equal(names(nec1), c("Q50", "Q2.5", "Q97.5"))
})

test_that("works for bayesmanecfit", {
  # A mixed model set returns the model-averaged N(S)EC rather than a NEC, and
  # says so; see ?nec.
  nec1 <- suppressMessages(nec(manec_example))
  expect_message(nec(manec_example), "model-averaged N\\(S\\)EC")
  expect_equal(length(nec1), 3)
  expect_equal(names(nec1), c("Q50", "Q2.5", "Q97.5"))
})

test_that("xform passes correctly", {
  nec1 <- nec(nec4param)
  nec2 <- nec(nec4param, xform = exp)
  expect_gt(nec2[1], nec1[2])
})

test_that("posterior passes correctly", {
  nec3 <- nec(nec4param, posterior = TRUE)
  expect_equal(length(nec3), 100)
})

test_that("prob_vals passes correctly", {
  nec4 <- nec(nec4param, prob_vals = c(0.5, 0.3, 0.7))
  expect_equal(names(nec4), c("Q50", "Q30", "Q70"))
})


# ---- #39, the censored no-effect posterior -----------------------------------

# estimates_summary() gained na.rm when nsec_off_curve() started returning NA
# for a draw whose curve never reaches the reference; nec() did not, so it
# raised "missing values and NaN's not allowed" on a posterior the package had
# itself written. The NA reach nec() by two routes: a joint two-block fit whose
# survival block is smooth, and a model set containing any smooth equation.
# Injected here rather than fitted, because producing a draw that never crosses
# needs a fit that does not converge to a decline.

test_that("nec summarises a censored posterior rather than erroring", {
  censored <- nec4param
  censored$ne_posterior[c(3, 17)] <- NA_real_
  out <- suppressWarnings(nec(censored))
  expect_equal(length(out), 3)
  expect_false(anyNA(out))
  # The two dropped draws are excluded, not replaced, so the summary is taken
  # over the 98 that are identified.
  expect_equal(as.numeric(out),
               as.numeric(quantile(censored$ne_posterior,
                                   probs = c(0.5, 0.025, 0.975),
                                   na.rm = TRUE)))
})

test_that("nec says how many draws are censored, on either class", {
  censored <- nec4param
  censored$ne_posterior[c(3, 17)] <- NA_real_
  expect_warning(nec(censored), "not identified for 2 of 100 draws")
  censored_manec <- manec_example
  censored_manec$w_ne_posterior[1] <- NA_real_
  expect_warning(suppressMessages(nec(censored_manec)),
                 "not identified for 1 of 100 draws")
})

test_that("an uncensored posterior is silent", {
  expect_silent(nec(nec4param))
})

test_that("nec reports an estimate at the fitted prior bound", {
  constrained <- nec4param
  upper <- unname(quantile(constrained$ne_posterior, 0.975))
  is_nec <- constrained$fit$prior$nlpar == "nec"
  constrained$fit$prior$ub[is_nec] <- upper
  expect_message(
    nec(constrained),
    "upper interval limit is at the upper bound of the fitted nec prior"
  )
  expect_message(
    nec(constrained, xform = exp),
    paste0("\\(", format(signif(exp(upper), 3), scientific = FALSE), "\\)")
  )
})

test_that("the prior-bound message is provenance-neutral", {
  constrained <- nec4param
  upper <- unname(quantile(constrained$ne_posterior, 0.975))
  is_nec <- constrained$fit$prior$nlpar == "nec"
  constrained$fit$prior$ub[is_nec] <- upper
  messages <- capture.output(nec(constrained), type = "message")
  expect_true(any(grepl("constrained by that prior", messages)))
  expect_false(any(grepl("tested predictor range", messages)))
})

test_that("a mixed NEC/NSEC average does not infer a common NEC bound", {
  mixed <- manec_example
  upper <- unname(quantile(mixed$w_ne_posterior, 0.975, na.rm = TRUE))
  for (model in names(mixed$mod_fits)) {
    is_nec <- grepl("nec$", mixed$mod_fits[[model]]$fit$prior$nlpar)
    mixed$mod_fits[[model]]$fit$prior$ub[is_nec] <- upper
  }
  messages <- capture.output(nec(mixed), type = "message")
  expect_false(any(grepl("upper bound of the fitted nec prior", messages)))
})

test_that("a joint threshold/smooth fit does not infer a NEC constraint", {
  mixed_blocks <- nec4param
  mixed_blocks$ne_type <- "N(S)EC"
  upper <- unname(quantile(mixed_blocks$ne_posterior, 0.975))
  is_nec <- mixed_blocks$fit$prior$nlpar == "nec"
  mixed_blocks$fit$prior$ub[is_nec] <- upper
  expect_silent(
    report_nec_prior_bound(
      mixed_blocks,
      quantile(mixed_blocks$ne_posterior, c(0.5, 0.025, 0.975))
    )
  )
})

test_that("a pure threshold average needs one common prior bound", {
  pure <- manec_example
  pure$mod_fits <- list(nec4param = nec4param, nec3param = nec4param)
  pure$mod_fits$nec3param$model <- "nec3param"
  first_nec <- pure$mod_fits$nec4param$fit$prior$nlpar == "nec"
  second_nec <- pure$mod_fits$nec3param$fit$prior$nlpar == "nec"
  pure$mod_fits$nec4param$fit$prior$ub[first_nec] <- 2
  pure$mod_fits$nec3param$fit$prior$ub[second_nec] <- 3
  expect_silent(report_nec_prior_bound(pure, c(Q50 = 2, Q2.5 = 1,
                                                Q97.5 = 2)))
})
