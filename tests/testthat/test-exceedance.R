# exceedance() (#44, D25). The probability that an estimate exceeds a threshold,
# with draws beyond the prediction range counted from the censoring record.
#
# Most tests replace a stored posterior with draws and a record the test fixes,
# so that the expected probabilities can be counted by hand and nothing is
# sampled. The tests that read a curve off the packaged fits are marked
# skip_on_cran().

# A threshold fit whose stored nec posterior is the one given. nec() reads the
# stored posterior and its record, applies xform to both, and returns them, so
# this reproduces a fitted posterior with draws beyond the range.
with_ne_posterior <- function(fit, values, above = logical(length(values)),
                              below = logical(length(values)), upper = 10,
                              lower = 1) {
  attr(values, "censored") <- bayesnec:::censoring_record(upper, lower, above,
                                                          below)
  fit$ne_posterior <- values
  fit
}

# Eight draws on a prediction range of 1 to 10: five identified (2, 4, 6, 8,
# 9), two sampled above the range and marked censored above (12, 15), and one
# marked censored below (0.5).
eight_draws <- function(fit = nec4param) {
  with_ne_posterior(
    fit, c(2, 4, 6, 8, 9, 12, 15, 0.5),
    above = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE),
    below = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
  )
}

quiet_exceedance <- function(...) {
  suppressWarnings(suppressMessages(exceedance(...)))
}

test_that("the result is one row with the documented columns", {
  out <- quiet_exceedance(eight_draws(), threshold = 5)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1)
  expect_equal(names(out), c("threshold", "prob", "prob_lower", "prob_upper",
                             "n_above", "n_below", "n_draws"))
  expect_equal(out$threshold, 5)
  expect_equal(out$n_above, 2)
  expect_equal(out$n_below, 1)
  expect_equal(out$n_draws, 8)
})

test_that("a threshold inside the range is decided by the record", {
  # Inside the range every draw is decided: 6, 8 and 9 exceed 5, the two draws
  # censored above exceed it whatever their value, and the draw censored below
  # 1 does not. 5 of 8.
  out <- quiet_exceedance(eight_draws(), threshold = 5)
  expect_equal(out$prob, 5 / 8)
  expect_equal(out$prob_lower, 5 / 8)
  expect_equal(out$prob_upper, 5 / 8)
  # At the lower bound the draw censored below lies at or below it, so it does
  # not exceed it, and the answer is still exact: every other draw exceeds 1.
  at_lower <- quiet_exceedance(eight_draws(), threshold = 1)
  expect_equal(at_lower$prob, 7 / 8)
  expect_equal(at_lower$prob_lower, at_lower$prob_upper)
})

test_that("a threshold beyond the upper end is reported as an interval", {
  # [P(identified > T), P(identified > T) + P(censored above)]: no identified
  # draw exceeds 12, and the two censored above may or may not. Their sampled
  # values (12 and 15) are not read, because nec() reports them only as lying
  # at or above 10.
  out <- quiet_exceedance(eight_draws(), threshold = 12)
  expect_true(is.na(out$prob))
  expect_equal(out$prob_lower, 0)
  expect_equal(out$prob_upper, 2 / 8)
  expect_equal(out$n_above, 2)
  # A threshold equal to the upper bound leaves the draws censored above open,
  # because the record states them as at or above the bound.
  at_upper <- quiet_exceedance(eight_draws(), threshold = 10)
  expect_true(is.na(at_upper$prob))
  expect_equal(c(at_upper$prob_lower, at_upper$prob_upper), c(0, 2 / 8))
  # Where nothing is censored above, a threshold beyond the upper end is
  # decided: no draw exceeds it.
  none_above <- with_ne_posterior(nec4param, c(2, 4, 6))
  beyond <- quiet_exceedance(none_above, threshold = 12)
  expect_equal(beyond$prob, 0)
  expect_equal(beyond$prob_lower, beyond$prob_upper)
})

test_that("a threshold below the lower end leaves the draws below it open", {
  # Every identified draw and both draws censored above exceed 0.5; the draw
  # censored below 1 may lie on either side of it.
  out <- quiet_exceedance(eight_draws(), threshold = 0.5)
  expect_true(is.na(out$prob))
  expect_equal(out$prob_lower, 7 / 8)
  expect_equal(out$prob_upper, 1)
})

test_that("a draw the record does not account for is left out", {
  # An NA that is censored at neither end could not be computed, and is left
  # out of both probabilities and of the count they are taken over.
  fit <- with_ne_posterior(
    nec4param, c(2, 6, NA, 12),
    above = c(FALSE, FALSE, FALSE, TRUE)
  )
  out <- quiet_exceedance(fit, threshold = 5)
  expect_equal(out$n_draws, 3)
  expect_equal(out$prob, 2 / 3)
  # With no draw left the probability is not defined.
  empty <- with_ne_posterior(nec4param, c(NA_real_, NA_real_))
  none <- quiet_exceedance(empty, threshold = 5)
  expect_equal(none$n_draws, 0)
  expect_true(is.na(none$prob))
  expect_true(is.na(none$prob_lower))
  expect_true(is.na(none$prob_upper))
})

test_that("a posterior without a record is compared by its values", {
  # The packaged manec_example was built before the record existed, so its
  # no-effect posterior carries none and every draw is compared by value.
  post <- suppressMessages(nec(manec_example, posterior = TRUE))
  expect_null(attr(post, "censored"))
  out <- quiet_exceedance(manec_example, threshold = 1.5)
  expect_equal(out$prob, mean(as.numeric(post) > 1.5))
  expect_equal(c(out$n_above, out$n_below, out$n_draws), c(0, 0, 100))
})

test_that("the EC50 of nec4param on a capped range counts its censored draws", {
  skip_on_cran()
  # The measurement behind the design. With the prediction range capped at
  # 1.67, 57 of 100 EC50 draws of nec4param lie above it, and a threshold of
  # 1.65 is inside the range, so each of those draws exceeds it. The
  # probability then equals the one on the full range, where no draw is
  # censored.
  x_min <- min(nec4param$fit$data$x)
  capped <- quiet_exceedance(nec4param, threshold = 1.65, estimate = "ecx",
                             ecx_val = 50, x_range = c(x_min, 1.67))
  expect_equal(capped$prob, 0.83)
  expect_equal(capped$prob_lower, capped$prob_upper)
  expect_equal(capped$n_above, 57)
  full <- quiet_exceedance(nec4param, threshold = 1.65, estimate = "ecx",
                           ecx_val = 50)
  expect_equal(full$n_above, 0)
  expect_equal(capped$prob, full$prob)
  # Beyond the cap the same draws are open.
  beyond <- quiet_exceedance(nec4param, threshold = 1.7, estimate = "ecx",
                             ecx_val = 50, x_range = c(x_min, 1.67))
  expect_true(is.na(beyond$prob))
  expect_equal(c(beyond$prob_lower, beyond$prob_upper), c(0, 0.57))
})

test_that("the estimator's own report is raised, not muffled", {
  skip_on_cran()
  x_min <- min(nec4param$fit$data$x)
  expect_warning(
    exceedance(nec4param, threshold = 1.65, estimate = "ecx", ecx_val = 50,
               x_range = c(x_min, 1.67)),
    "57 of 100 draws"
  )
})

test_that("each estimate is read from its estimator's posterior", {
  skip_on_cran()
  # The draws compared are exactly the ones the estimator returns, so a
  # probability recomputed from that posterior is the one reported.
  post <- suppressMessages(nsec(ecx4param, posterior = TRUE))
  out <- quiet_exceedance(ecx4param, threshold = 1, estimate = "nsec")
  # No draw of this posterior is censored, so each is compared by its value.
  expect_equal(c(out$n_above, out$n_below), c(0, 0))
  expect_equal(out$prob, mean(as.numeric(post) > 1))
  expect_equal(out$n_draws, length(post))
  # nec() refuses a single smooth equation, and so does the default here.
  expect_error(exceedance(ecx4param, threshold = 1),
               "nec is not a parameter in ecx model types")
})

test_that("a model set is compared through its model-averaged posterior", {
  # The model-averaged no-effect posterior of a set, with a record, as
  # expand_manec() writes it. The same eight draws, so the same counts.
  manec <- manec_example
  post <- eight_draws()$ne_posterior
  manec$w_ne_posterior <- post
  out <- quiet_exceedance(manec, threshold = 5)
  expect_equal(out$prob, 5 / 8)
  beyond <- quiet_exceedance(manec, threshold = 12)
  expect_equal(c(beyond$prob_lower, beyond$prob_upper), c(0, 2 / 8))
  # The mixed-set report of nec() comes through, because the estimate compared
  # is the N(S)EC it describes.
  expect_message(suppressWarnings(exceedance(manec, threshold = 5)),
                 "model-averaged N\\(S\\)EC")
})

test_that("a model set's ECx is compared with its censored draws counted", {
  skip_on_cran()
  x_min <- min(nec4param$fit$data$x)
  post <- suppressWarnings(ecx(manec_example, ecx_val = 50, posterior = TRUE,
                               x_range = c(x_min, 1.67)))
  cens <- attr(post, "censored")
  expect_true(any(cens$above))
  out <- quiet_exceedance(manec_example, threshold = 1, estimate = "ecx",
                          ecx_val = 50, x_range = c(x_min, 1.67))
  expected <- mean(cens$above | (!cens$above & as.numeric(post) > 1))
  expect_equal(out$prob, expected)
  expect_equal(out$n_above, sum(cens$above))
  expect_equal(out$prob_lower, out$prob_upper)
})

test_that("a hurdle fit is compared through its combined threshold", {
  # Two threshold components on the same range of 1 to 10. The combined
  # threshold is the smaller of the two per draw, and a draw is censored above
  # only where both components are:
  #   growth    2  4  >=10  >=10  6     9  3     8
  #   survival  5  3  >=10  7     >=10  2  >=10  <=1
  #   combined  2  3  >=10  7     6     2  3     <=1
  growth <- with_ne_posterior(
    nec4param, c(2, 4, 12, 15, 6, 9, 3, 8),
    above = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  survival <- with_ne_posterior(
    nec4param, c(5, 3, 14, 7, 11, 2, 12, 0.5),
    above = c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    below = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
  )
  hurdle <- structure(list(growth = growth, survival = survival),
                      class = c("bayesnechurdlefit", "bnecfit"))
  # 7 and 6 exceed 5, as does the draw censored above; 3 of 8.
  out <- quiet_exceedance(hurdle, threshold = 5)
  expect_equal(out$prob, 3 / 8)
  expect_equal(c(out$n_above, out$n_below, out$n_draws), c(1, 1, 8))
  beyond <- quiet_exceedance(hurdle, threshold = 12)
  expect_true(is.na(beyond$prob))
  expect_equal(c(beyond$prob_lower, beyond$prob_upper), c(0, 1 / 8))
  # which reaches a component, as it does in nec(): growth has 6, 9 and 8 above
  # 5 and two draws censored above.
  grow <- quiet_exceedance(hurdle, threshold = 5, which = "growth")
  expect_equal(grow$prob, 5 / 8)
  expect_equal(grow$n_above, 2)
})

test_that("a hurdle fit on unequal ranges leaves an unresolved minimum open", {
  # Growth is predicted to 8 and survival to 10, as where nothing survived at
  # the top concentrations. In the second draw growth is known only to lie at
  # or above 8 and survival is identified at 9, so the combined threshold lies
  # somewhere from 8 to 9 and is marked censored above 8 (#415). Before that
  # correction the draw was returned as an identified 9, and exceedance() would
  # have counted it as exceeding 8.5.
  #   growth    2  >=8  5  >=8
  #   survival  6  9    3  >=10
  #   combined  2  >=8  3  >=8
  growth <- with_ne_posterior(
    nec4param, c(2, 9, 5, 9),
    above = c(FALSE, TRUE, FALSE, TRUE), upper = 8
  )
  survival <- with_ne_posterior(
    nec4param, c(6, 9, 3, 12),
    above = c(FALSE, FALSE, FALSE, TRUE), upper = 10
  )
  hurdle <- structure(list(growth = growth, survival = survival),
                      class = c("bayesnechurdlefit", "bnecfit"))
  # 8.5 is inside survival's range and beyond the combined one, so neither
  # draw censored above 8 is decided: the result is an interval.
  out <- quiet_exceedance(hurdle, threshold = 8.5)
  expect_true(is.na(out$prob))
  expect_equal(c(out$prob_lower, out$prob_upper), c(0, 2 / 4))
  expect_equal(out$n_above, 2)
  # Below 8 both are decided: each lies at or above 8 and so exceeds 7.
  inside <- quiet_exceedance(hurdle, threshold = 7)
  expect_equal(inside$prob, 2 / 4)
  expect_equal(inside$prob_lower, inside$prob_upper)
})

test_that("the threshold is read on the scale xform returns", {
  # An increasing xform: the threshold is given on the transformed scale.
  plain <- quiet_exceedance(eight_draws(), threshold = 5)
  logged <- quiet_exceedance(eight_draws(), threshold = log(5), xform = log)
  expect_equal(logged[, -1], plain[, -1])
})

test_that("a decreasing xform reverses the direction of exceedance", {
  # Under xform = -x the draws are -2, -4, -6, -8, -9; the two censored above
  # 10 on the fitted scale are censored below -10, and the one censored below 1
  # is censored above -1, as xform_censoring() remaps them.
  neg <- function(x) -x
  out <- quiet_exceedance(eight_draws(), threshold = -5, xform = neg)
  # -2 and -4 exceed -5, and so does the draw at or above -1; the draws at or
  # below -10 do not. 3 of 8, which is the share of fitted-scale draws known to
  # lie below 5.
  expect_equal(out$prob, 3 / 8)
  expect_equal(out$prob_lower, out$prob_upper)
  expect_equal(c(out$n_above, out$n_below), c(1, 2))
  # Beyond the foot of the transformed range the draws censored below -10 are
  # open: every other draw exceeds -12.
  beyond <- quiet_exceedance(eight_draws(), threshold = -12, xform = neg)
  expect_true(is.na(beyond$prob))
  expect_equal(c(beyond$prob_lower, beyond$prob_upper), c(6 / 8, 1))
})

test_that("a group fit is compared level by level", {
  fits <- list(a = manec_example, b = manec_example)
  fits$b$w_ne_posterior <- eight_draws()$ne_posterior
  group <- bayesnec:::allot_class(
    list(fits = fits, group_var = "site", levels = names(fits)),
    c("bayesnecgroupfit", "bnecfit")
  )
  out <- quiet_exceedance(group, threshold = 5)
  expect_equal(out$level, c("a", "b"))
  expect_equal(names(out), c("level", "threshold", "prob", "prob_lower",
                             "prob_upper", "n_above", "n_below", "n_draws"))
  expect_equal(out[1, -1], quiet_exceedance(manec_example, threshold = 5),
               ignore_attr = TRUE)
  expect_equal(out$prob[2], 5 / 8)
  expect_equal(rownames(out), c("1", "2"))
  # Arguments are refused before any level is read.
  expect_error(exceedance(group, threshold = 5, ecx_val = 50),
               "ecx_val applies only")
})

test_that("the arguments are validated", {
  fit <- eight_draws()
  expect_error(exceedance(fit, threshold = "a"), "threshold")
  expect_error(exceedance(fit, threshold = c(1, 2)), "threshold")
  expect_error(exceedance(fit, threshold = NA_real_), "threshold")
  expect_error(exceedance(fit, threshold = Inf), "finite")
  expect_error(exceedance(fit, threshold = 5, estimate = "ec50"),
               "should be one of")
  # An effect level with a no-effect estimate would otherwise be ignored.
  expect_error(exceedance(fit, threshold = 5, ecx_val = 50),
               "ecx_val applies only to estimate = \"ecx\"")
  expect_error(exceedance(fit, threshold = 5, estimate = "ecx",
                          ecx_val = c(10, 50)), "ecx_val")
  expect_error(exceedance(fit, threshold = 5, posterior = TRUE),
               "posterior is not an argument")
  expect_error(exceedance(data.frame(x = 1), threshold = 5),
               "no applicable method")
})
