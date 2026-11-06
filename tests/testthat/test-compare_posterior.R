test_that("input errors work as intended", {
  expect_error(compare_posterior(list(ecx4param, nec4param)),
               "Argument x must be a named list.")
  expect_error(compare_posterior(ecx4param, nec4param),
               "Argument comparison must be a character vector.")
  # message now names all three accepted classes, hurdle fits included
  m_0 <- "Not all objects in x are of class bayesnecfit, bayesmanecfit or"
  expect_error(compare_posterior(list(g = ecx4param, h = 12)), m_0)
})

test_that("output is a list of appropriately name elements", {
  cp <- compare_posterior(list(ecx4param = ecx4param, nec4param = nec4param))
  expect_equal(class(cp), "list")
  expect_equal(length(cp), 5)
  expect_equal(names(cp), c("posterior_list", "posterior_data", "diff_list",
                            "diff_data", "prob_diff"))
})

test_that("posterior_data keeps the names the fits were given", {
  cp <- compare_posterior(list("2018" = ecx4param, "level b" = nec4param))
  expect_setequal(unique(cp$posterior_data$model), c("2018", "level b"))
  expect_equal(cp$prob_diff$comparison, "2018-level b")
})

test_that("joint comparisons pair estimates and retain censoring", {
  a <- c(2, 11, 4, 5)
  attr(a, "censored") <- bayesnec:::censoring_record(
    10, 0, c(FALSE, TRUE, FALSE, FALSE), rep(FALSE, 4)
  )
  b <- c(1, 5, 5, 5)
  joint <- structure(list(levels = c("a", "b")),
                     class = "bayesnecjointfit")
  local_mocked_bindings(nec = function(...) list(a = a, b = b))
  result <- compare_posterior(joint, comparison = "nec")
  expect_identical(result$posterior_list$a, a)
  expect_equal(result$diff_list[["a-b"]], c(1, NA, -1, 0))
  expect_equal(result$prob_diff$prob, 0.5)
  expect_equal(result$prob_diff$censored_first, 0.25)
  expect_identical(compare_posterior(joint, comparison = "nec", seed = 99),
                   result)
})

test_that("joint fitted curves retain shared posterior rows", {
  joint <- joint_split()
  paired <- compare_posterior(joint, comparison = "fitted", resolution = 6,
                              ndraws = 20)
  expect_equal(dim(paired$diff_list[["low-high"]]), c(20L, 6L))
  expect_true(all(paired$diff_list[["low-high"]] == 0))
  set.seed(571)
  first <- paired$posterior_list$low[, 1]
  random_width <- diff(stats::quantile(first - sample(first),
                                       c(0.025, 0.975)))
  expect_gt(random_width, 0)
  raw <- compare_posterior(joint, comparison = "fitted",
                           make_newdata = FALSE,
                           newdata = data.frame(x = c(0.1, 0.5, 1)),
                           ndraws = 20)
  expect_true(all(raw$diff_list[["low-high"]] == 0))
  expect_equal(ncol(raw$posterior_list$low), 3L)
})

test_that("paired joint differences retain positive covariance", {
  joint <- structure(list(levels = c("a", "b")),
                     class = "bayesnecjointfit")
  a <- seq_len(100)
  b <- a + 5
  local_mocked_bindings(nec = function(...) list(a = a, b = b))
  paired <- compare_posterior(joint, comparison = "nec")
  expect_true(all(paired$diff_list[["a-b"]] == -5))
  set.seed(571)
  random <- a - sample(b)
  expect_gt(diff(stats::quantile(random, c(0.025, 0.975))),
            diff(stats::quantile(paired$diff_list[["a-b"]],
                                 c(0.025, 0.975))))
})

test_that("smooth joint levels compare their no-effect posteriors", {
  joint <- joint_split()
  joint$fit <- ecx4param$fit
  joint$bayesnecformula <- ecx4param$bayesnecformula
  joint$model <- "ecx4param"
  joint$data <- ecx4param$fit$data
  joint$data$site <- factor(ifelse(joint$data$x < 1, "low", "high"),
                            levels = joint$levels)
  result <- suppressWarnings(suppressMessages(compare_posterior(
    joint, comparison = "n(s)ec", resolution = 20
  )))
  direct <- suppressWarnings(suppressMessages(nsec(
    joint, posterior = TRUE, resolution = 20
  )))
  expect_equal(result$posterior_list$low, direct$low)
  expect_equal(result$posterior_list$high, direct$high)
})
