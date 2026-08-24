# #244: sample_priors() looks a prior's distribution name up in the same
# hard-coded sampling table make_inits() uses, so any prior set containing a
# constant() prior made the whole call error -- a user could not inspect the
# prior set they had just written a fixed parameter into.

test_that("sample_priors returns the fixed value for a constant prior", {
  priors <- data.frame(
    prior = c("normal(1,1)", "normal(0,5)", "constant(0)", "gamma(5,2)"),
    class = "b", coef = "", group = "", resp = "", dpar = "",
    nlpar = c("top", "beta", "bot", "nec"), lb = "", ub = "",
    stringsAsFactors = FALSE
  )
  set.seed(1)
  # the returned ggplot carries the draws, which is the only route to them
  # without the plot = NA branch (which errors -- see the test below)
  vals <- sample_priors(priors, n_samples = 100)$data
  bot <- vals$value[vals$param == "bot"]
  expect_length(bot, 100)
  expect_true(all(bot == 0))
  # the other parameters are still drawn from their own distributions
  expect_gt(length(unique(vals$value[vals$param == "top"])), 50)
})

test_that("bounds on a constant prior are ignored rather than filtered", {
  # The bound filter subsets the draws to those inside lb/ub and resamples. A
  # point mass outside its own bounds would leave nothing to resample from.
  priors <- data.frame(
    prior = c("normal(1,1)", "constant(0)"), class = "b", coef = "",
    group = "", resp = "", dpar = "", nlpar = c("top", "nec"),
    lb = c("", "1"), ub = c("", "10"), stringsAsFactors = FALSE
  )
  vals <- sample_priors(priors, n_samples = 50)$data
  expect_true(all(vals$value[vals$param == "nec"] == 0))
})
