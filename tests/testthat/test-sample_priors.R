# #244: sample_priors() looks a prior's distribution name up in the same
# hard-coded sampling table make_inits() uses, so any prior set containing a
# constant() prior made the whole call error -- a user could not inspect the
# prior set they had just written a fixed parameter into.

const_prior_set <- function(lb = c("", ""), ub = c("", "")) {
  data.frame(
    prior = c("normal(1,1)", "constant(0)"), class = "b", coef = "",
    group = "", resp = "", dpar = "", nlpar = c("top", "bot"),
    lb = lb, ub = ub, stringsAsFactors = FALSE
  )
}

test_that("sample_priors returns the fixed value for a constant prior", {
  set.seed(1)
  out <- sample_priors(const_prior_set(), n_samples = 100, plot = NA)
  expect_setequal(names(out), c("b_top", "b_bot"))
  expect_length(out$b_bot, 100)
  expect_true(all(out$b_bot == 0))
  # the other parameters are still drawn from their own distributions
  expect_gt(length(unique(out$b_top)), 50)
})

test_that("bounds on a constant prior are ignored rather than filtered", {
  # The bound filter subsets the draws to those inside lb/ub and resamples. A
  # point mass outside its own bounds would leave nothing to resample from.
  out <- sample_priors(const_prior_set(lb = c("", "1"), ub = c("", "10")),
                       n_samples = 50, plot = NA)
  expect_true(all(out$b_bot == 0))
})

test_that("a constant prior does not stop the plot being drawn", {
  expect_s3_class(sample_priors(const_prior_set(), n_samples = 50), "ggplot")
})

# #244: plot = NA is documented as the option that returns the draws, but the
# guard tested `!plot %in% c("ggplot", "base")`, and `NA %in% ...` is FALSE, so
# the documented value was rejected and there was no route to the values at all.

test_that("plot = NA returns the draws rather than erroring", {
  out <- sample_priors(const_prior_set(), n_samples = 20, plot = NA)
  expect_type(out, "list")
  expect_false(inherits(out, "ggplot"))
  expect_length(out$b_top, 20)
})

test_that("plot still rejects a value that is neither NA nor a known option", {
  expect_error(
    sample_priors(const_prior_set(), n_samples = 20, plot = "nonsense"),
    "plot must be NA"
  )
  # NULL and a length-2 value reached `if` with something that is not a single
  # TRUE/FALSE, and failed with an R internals message rather than this one.
  expect_error(
    sample_priors(const_prior_set(), n_samples = 20, plot = NULL),
    "plot must be NA"
  )
  expect_error(
    sample_priors(const_prior_set(), n_samples = 20,
                  plot = c("ggplot", "base")),
    "plot must be NA"
  )
})

# #302: the sampling table was written out at each of the three places that
# needed it, so a distribution present at one was absent at the others, and an
# unrecognised name reached fcts[[dist]](...) as NULL and failed with "attempt
# to apply non-function", naming neither the prior nor the distribution.

test_that("lognormal priors can be sampled and drawn from", {
  ps <- data.frame(
    prior = c("normal(1,1)", "lognormal(0, 1)"), class = "b", coef = "",
    group = "", resp = "", dpar = "", nlpar = c("top", "nec"),
    lb = c("", "0"), ub = c("", "10"), stringsAsFactors = FALSE
  )
  set.seed(302)
  out <- sample_priors(ps, n_samples = 200, plot = NA)
  expect_setequal(names(out), c("b_top", "b_nec"))
  expect_length(out$b_nec, 200)
  # a lognormal is strictly positive, and the bounds are respected
  expect_true(all(out$b_nec > 0 & out$b_nec <= 10))
})

test_that("an unsupported prior distribution is named in the error", {
  ps <- data.frame(
    prior = "cauchy(0, 1)", class = "b", coef = "", group = "", resp = "",
    dpar = "", nlpar = "top", lb = "", ub = "", stringsAsFactors = FALSE
  )
  expect_error(sample_priors(ps, n_samples = 10, plot = NA),
               "cauchy")
  expect_error(sample_priors(ps, n_samples = 10, plot = NA),
               "gamma, normal, beta, uniform, lognormal")
})
