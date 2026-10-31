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


# ECx type arguments

# Comparison wrappers must accept every ECx type, including range, so they
# do not block the migration named by the relative-type warning.

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


# Censoring records in paired comparisons

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
  # Include identified ties, opposite bounds, common bounds and unexplained
  # NA values. Common bounds leave the sign indeterminate; an unexplained
  # NA is missing rather than censored.
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
  # The record reports a draw only as >= 10, even if its sampled value is 15.
  # Its sign against 12 is unknown; against 9 it is positive.
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

test_that("two identified infinite draws are left out, as before", {
  # Inf - Inf is NaN and has no sign. Without a censoring record, such pairs
  # are missing rather than indeterminate.
  s <- bayesnec:::difference_sign(c(Inf, Inf, -Inf, 2), c(Inf, 3, -Inf, Inf))
  expect_identical(s$positive, c(NA, TRUE, NA, FALSE))
  expect_identical(s$indeterminate, c(FALSE, FALSE, FALSE, FALSE))
  expect_identical(s$diff, c(NaN, Inf, NaN, -Inf))
  # The same through compare_estimates(), on nec posteriors with infinite
  # draws that no record marks. prob is the value the previous expression
  # gives, not NA.
  skip_on_cran()
  a <- nec4param
  b <- nec4param
  a$ne_posterior[1:50] <- Inf
  b$ne_posterior[1:50] <- Inf
  ce <- suppressWarnings(suppressMessages(
    compare_estimates(list(a = a, b = b), comparison = "nec")
  ))
  m <- ce$diff_list[[1]]
  expect_true(any(is.nan(m)))
  m[m > 0] <- 1
  m[m <= 0] <- 0
  expect_identical(ce$prob_diff$prob, mean(m, na.rm = TRUE))
  expect_false(any(ce$diff_data$indeterminate))
})

test_that("a comparison with nothing censored returns what it returned before", {
  # For uncensored draws, retain bit-for-bit agreement with the direct
  # probability calculation below.
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
  # The censored draws are above every partner, so their signs are known.
  # Count all 100 pairs, not only the 70 with numerical differences.
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
  # Two draws above the same bound have unknown ordering even when their
  # sampled values differ. Keep and mark those pairs when constructing the
  # probability interval.
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
  # A restricted range censors draws in both fits. Pairs of censored draws
  # may be indeterminate; one censored and one identified draw can still have
  # a known sign. Do not compute probability from identified pairs alone.
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
  # This fixture must have censored NSEC draws in only one fit. Check that
  # precondition and skip if regenerated stored fits no longer meet it;
  # otherwise all pairs have known signs despite missing numerical differences.
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
