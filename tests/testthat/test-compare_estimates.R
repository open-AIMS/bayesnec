test_that("x must be a named list", {
  expect_error(compare_estimates(list(ecx4param, nec4param)))
  expect_error(compare_estimates(ecx4param, nec4param))
})

test_that("output is a list of appropriately name elements", {
  ce <- compare_estimates(list(ecx4param = ecx4param, nec4param = nec4param))
  expect_equal(class(ce), "list")
  expect_equal(length(ce), 5)
  expect_equal(names(ce), c("posterior_list", "posterior_data", "diff_list",
                            "diff_data", "prob_diff"))
})


# ---- the four-value type vocabulary ------------------------------------------

# compare_estimates() and compare_posterior() forward type to ecx(), but kept a
# second copy of the vocabulary that still listed the 2.1.3 three-value set. It
# therefore refused "range" -- the name the rename warning gives callers for the
# behaviour they had -- so the migration the warning names was impossible from
# the two functions that exist to compare an ECx across fits.

test_that("compare_estimates accepts every type ecx accepts", {
  skip_on_cran()
  x <- list(ecx4param = ecx4param, nec4param = nec4param)
  for (ty in c("absolute", "range")) {
    ce <- compare_estimates(x, comparison = "ecx", type = ty, resolution = 50)
    expect_equal(names(ce), c("posterior_list", "posterior_data", "diff_list",
                              "diff_data", "prob_diff"))
  }
  expect_error(compare_estimates(x, comparison = "ecx", type = "nonsense"),
               "type must be one of")
})

test_that("the relative rename is warned once for the call, not once per fit", {
  skip_on_cran()
  x <- list(ecx4param = ecx4param, nec4param = nec4param)
  w <- testthat::capture_warnings(
    compare_estimates(x, comparison = "ecx", type = "relative",
                      resolution = 50)
  )
  expect_equal(sum(grepl("now measures from the control", w)), 1)
})

test_that("average_estimates validates type on the same vocabulary", {
  skip_on_cran()
  x <- list(ecx4param = ecx4param, nec4param = nec4param)
  expect_error(average_estimates(x, estimate = "ecx", type = "nonsense"),
               "type must be one of")
  out <- average_estimates(x, estimate = "ecx", type = "range",
                           resolution = 50)
  expect_equal(length(out), 3)
})


# ---- #39 and #404, a censored draw is compared through its record ------------

# A posterior vector carrying a censoring record, for the tests that exercise the
# pairing rule directly rather than through a fit.
with_record <- function(x, above = logical(length(x)),
                        below = logical(length(x)), upper = 10, lower = 1) {
  attr(x, "censored") <- bayesnec:::censoring_record(upper, lower, above,
                                                       below)
  x
}

# A threshold fit whose stored nec posterior has draws beyond the prediction
# range. A sampled nec draw there keeps its value and is marked in the record,
# which is what nec() reports as a bound; this reproduces that without a fit.
censor_nec_draws <- function(fit, idx, beyond = 1) {
  post <- fit$ne_posterior
  cens <- attr(post, "censored")
  post[idx] <- cens$upper + beyond
  cens$above[idx] <- TRUE
  attr(post, "censored") <- cens
  fit$ne_posterior <- post
  fit
}

test_that("the sign of a paired difference is read from the censoring record", {
  # One pair per position:
  #  1  5 against 3, both identified            positive
  #  2  3 against 5, both identified            not positive
  #  3  4 against 4, both identified            not positive: zero is not above
  #  4  at or above 10 against 3                positive, whatever the value
  #  5  3 against at or above 10                not positive
  #  6  at or above 10 against at or above 10   indeterminate
  #  7  at or below 1 against 3                 not positive
  #  8  at or below 1 against at or below 1     indeterminate
  #  9  at or above 10 against at or below 1    positive
  # 10  NA the record does not explain          missing, neither
  first <- with_record(
    c(5, 3, 4, NA, 3, NA, NA, NA, NA, NA),
    above = c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE,
              FALSE),
    below = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE,
              FALSE)
  )
  second <- with_record(
    c(3, 5, 4, 3, NA, NA, 3, NA, NA, 3),
    above = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE,
              FALSE),
    below = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE,
              FALSE)
  )
  s <- bayesnec:::difference_sign(first, second)
  expect_identical(s$positive, c(TRUE, FALSE, FALSE, TRUE, FALSE, NA, FALSE,
                                 NA, TRUE, NA))
  expect_identical(s$indeterminate, c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,
                                      FALSE, TRUE, FALSE, FALSE))
  # A difference with a censored draw in it has no value; the record is not
  # carried onto the differences, which are not on the scale it describes.
  expect_identical(s$diff, c(2, -2, 0, rep(NA_real_, 7)))
  expect_null(attributes(s$diff))
})

test_that("a draw censored at a bound is not compared by its value", {
  # The first draw is a sampled nec of 15 that the record marks as at or above
  # 10. Against an identified 12 from a component with a wider range, the value
  # would say positive, but nec() reports only that it is at or above 10, and
  # at or above 10 against 12 leaves the sign open. The second pair is fixed:
  # at or above 10 against 9 is positive.
  first <- with_record(c(15, 15), above = c(TRUE, TRUE))
  second <- with_record(c(12, 9), upper = 20)
  s <- bayesnec:::difference_sign(first, second)
  expect_identical(s$positive, c(NA, TRUE))
  expect_identical(s$indeterminate, c(TRUE, FALSE))
  expect_identical(s$diff, c(NA_real_, NA_real_))
  # A posterior with no record compares every draw by its value, as before.
  plain <- bayesnec:::difference_sign(c(15, 15), c(12, 9))
  expect_identical(plain$positive, c(TRUE, TRUE))
  expect_identical(plain$diff, c(3, 6))
})

test_that("a comparison with nothing censored returns what it returned before", {
  # The regression guard. prob is recomputed here by the expression the
  # function used before #404, and must agree to the last bit.
  skip_on_cran()
  x <- list(a = nec4param, b = ecx4param, m = manec_example)
  ce <- suppressWarnings(
    compare_estimates(x, comparison = "ecx", ecx_val = 10, resolution = 50)
  )
  expect_false(any(vapply(ce$posterior_list, function(p) {
    bayesnec:::has_censoring(attr(p, "censored"))
  }, logical(1))))
  old_prob <- vapply(ce$diff_list, function(m) {
    m[m > 0] <- 1
    m[m <= 0] <- 0
    mean(m, na.rm = TRUE)
  }, numeric(1))
  expect_identical(ce$prob_diff$prob, unname(old_prob))
  expect_identical(ce$prob_diff$prob_lower, ce$prob_diff$prob)
  expect_identical(ce$prob_diff$prob_upper, ce$prob_diff$prob)
  expect_identical(ce$prob_diff$censored_first, c(0, 0, 0))
  expect_identical(ce$prob_diff$censored_second, c(0, 0, 0))
  expect_named(ce$prob_diff, c("comparison", "prob", "prob_lower",
                               "prob_upper", "censored_first",
                               "censored_second"))
  expect_named(ce$diff_data, c("comparison", "diff", "positive",
                               "indeterminate"))
  expect_false(any(ce$diff_data$indeterminate))
  # The marks line up with the rows they describe.
  expect_identical(ce$diff_data$positive, ce$diff_data$diff > 0)
  expect_identical(
    ce$diff_data$diff[ce$diff_data$comparison == "a-m"],
    unname(ce$diff_list[["a-m"]])
  )
})

test_that("one component censored and one not gives one probability", {
  # Thirty of the first fit's nec draws are above the prediction range, and
  # every draw of the second is identified inside it. Each censored draw is
  # therefore above its partner whatever its value, so no pair is
  # indeterminate and the probability is a single number, computed over all
  # one hundred pairs rather than over the seventy with a value.
  skip_on_cran()
  a <- censor_nec_draws(nec4param, 1:30)
  ce <- suppressWarnings(suppressMessages(
    compare_estimates(list(a = a, b = nec4param), comparison = "nec")
  ))
  pd <- ce$prob_diff
  expect_false(is.na(pd$prob))
  expect_identical(pd$prob_lower, pd$prob)
  expect_identical(pd$prob_upper, pd$prob)
  expect_equal(pd$censored_first, 0.3)
  expect_equal(pd$censored_second, 0)
  dd <- ce$diff_data
  expect_identical(nrow(dd), 100L)
  expect_identical(sum(is.na(dd$diff)), 30L)
  expect_true(all(dd$positive[is.na(dd$diff)]))
  expect_false(any(dd$indeterminate))
  expect_equal(pd$prob, mean(dd$positive))
})

test_that("a sampled nec beyond the range is compared as nec() reports it", {
  # Both fits have forty draws above the range, at different values. Compared
  # by value, a pair of two such draws has a definite sign; nec() reports each
  # only as at or above the bound, so the pair is indeterminate. The difference
  # draws are kept and marked, and the probability becomes an interval.
  skip_on_cran()
  a <- censor_nec_draws(nec4param, 1:40, beyond = 1)
  b <- censor_nec_draws(nec4param, 1:40, beyond = 2)
  ce <- suppressWarnings(suppressMessages(
    compare_estimates(list(a = a, b = b), comparison = "nec")
  ))
  pd <- ce$prob_diff
  dd <- ce$diff_data
  n_indeterminate <- sum(dd$indeterminate)
  expect_gt(n_indeterminate, 0)
  expect_identical(nrow(dd), 100L)
  expect_true(is.na(pd$prob))
  expect_equal(pd$prob_upper - pd$prob_lower, n_indeterminate / 100)
  expect_equal(pd$prob_lower, sum(dd$positive, na.rm = TRUE) / 100)
  expect_equal(c(pd$censored_first, pd$censored_second), c(0.4, 0.4))
  # No difference is taken from a censored draw's value. The identified draws
  # lie within 0.25 of one another; a censored one would differ by 1 or more.
  expect_true(all(is.na(dd$diff) | abs(dd$diff) < 1))
  expect_true(all(is.na(dd$diff[dd$indeterminate])))
})

test_that("the ECx comparison of #404 reports an interval, not the survivors", {
  # The reproduction in #404: over this range 74 and 59 of the 100 ECx50 draws
  # are censored above it. The earlier na.rm kept the 10 pairs in which both
  # had a value and reported 0.6 over them. Pairs of two censored draws have
  # no known sign, so the probability is an interval, and every pair in which
  # one draw is censored and the other identified has a known sign.
  skip_on_cran()
  x <- list(a = nec4param, b = ecx4param)
  ce <- suppressWarnings(
    compare_estimates(x, comparison = "ecx", ecx_val = 50, resolution = 100,
                      x_range = c(0.03, 1.66))
  )
  pd <- ce$prob_diff
  expect_equal(c(pd$censored_first, pd$censored_second), c(0.74, 0.59))
  expect_true(is.na(pd$prob))
  expect_equal(c(pd$prob_lower, pd$prob_upper), c(0.37, 0.80))
  expect_identical(sum(ce$diff_data$indeterminate), 43L)
  expect_identical(nrow(ce$diff_data), 100L)
})

test_that("an NSEC censored in one component only is still one probability", {
  # Over this range almost every NSEC draw of the threshold fit is above the
  # range and none of the smooth fit's is. The earlier na.rm reported the
  # probability over the few pairs in which both had a value. The precondition
  # depends on where the packaged curves sit, so it is measured and the case
  # skipped if a regenerated fixture no longer meets it.
  skip_on_cran()
  x <- list(a = nec4param, b = ecx4param)
  out <- suppressWarnings(
    compare_estimates(x, comparison = "nsec", resolution = 50,
                      x_range = c(0, 1.4))
  )
  pd <- out$prob_diff
  skip_if_not(pd$censored_first > 0 && pd$censored_second == 0,
              "the packaged fixture no longer censors one component only")
  expect_false(is.na(pd$prob))
  expect_identical(pd$prob_lower, pd$prob_upper)
  expect_false(any(out$diff_data$indeterminate))
})
