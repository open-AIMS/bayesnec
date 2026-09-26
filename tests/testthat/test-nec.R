test_that("nec returns expected object types", {
  nec_summary <- suppressMessages(nec(manec_example))
  expect_equal(length(nec_summary), 3)
})

test_that("doesn't work for ecx models", {
  expect_error(nec(ecx4param), "nec is not a parameter in ecx model types.")
})
  
test_that("the class is read off the parameters, not the name (#419)", {
  # nec() refused on the substring "ecx" in the equation's name. A smooth
  # equation spelled without the prefix would then have had its NSEC returned
  # as a NEC; and a threshold equation spelled with it would have been refused.
  # The test is now whether the fit samples a nec parameter.
  renamed_smooth <- ecx4param
  renamed_smooth$model <- "flat"
  expect_error(nec(renamed_smooth),
               "nec is not a parameter in ecx model types.")
  renamed_threshold <- nec4param
  renamed_threshold$model <- "ecxthreshold"
  expect_identical(nec(renamed_threshold), nec(nec4param))
  expect_false(bayesnec:::fit_has_nec(ecx4param$fit))
  expect_true(bayesnec:::fit_has_nec(nec4param$fit))
  # The model-averaged note reads the fits as well: renaming both equations so
  # that neither holds the substring leaves the smooth one smooth.
  renamed_set <- manec_example
  names(renamed_set$mod_fits) <- c("threshold", "smooth")
  expect_message(nec(renamed_set), "model-averaged N\\(S\\)EC")
  only_threshold <- manec_example
  only_threshold$mod_fits <- only_threshold$mod_fits["nec4param"]
  expect_no_message(suppressWarnings(nec(only_threshold)),
                    message = "contains smooth")
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
  # An NA the fit's own record explains at neither end: it could not be
  # computed rather than being known to lie beyond a bound, and it is reported
  # as such and left out of the fraction the other reports are over (#395).
  expect_warning(nec(censored), "could not be computed for 2 of 100 draws")
  censored_manec <- manec_example
  censored_manec$w_ne_posterior[1] <- NA_real_
  expect_warning(suppressMessages(nec(censored_manec)),
                 "not identified for 1 of 100 draws")
})

test_that("an uncensored posterior is silent", {
  expect_silent(nec(nec4param))
})
