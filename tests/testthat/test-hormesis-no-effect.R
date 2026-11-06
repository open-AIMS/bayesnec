test_that("hormesis breakpoints are not classified as no-effect estimates", {
  hormesis <- intersect(bayesnec:::mod_groups$nec,
                        bayesnec:::mod_groups$hormesis)
  expect_setequal(hormesis, c("nechorme", "nechorme4", "neclinhorme",
                                 "nechormepwr", "nechorme4pwr",
                                 "nechormepwr01"))
  expect_false(any(vapply(hormesis,
                          bayesnec:::sampled_nec_is_no_effect, logical(1))))
  expect_true(bayesnec:::sampled_nec_is_no_effect("nec4param"))
  expect_false(bayesnec:::sampled_nec_is_no_effect("ecxflat"))
})

test_that("a hormesis threshold fit reports its declining-limb NSEC", {
  skip_on_cran()
  set.seed(570)
  x <- rep(c(0, 0.25, 0.5, 0.75, 1, 1.5, 2, 3), each = 8)
  y <- 1 + 0.35 * x - 0.8 * pmax(x - 1, 0) + rnorm(length(x), 0, 0.04)
  fit <- bnec(y ~ crf(x, model = "neclinhorme"),
              data = data.frame(x, y), family = gaussian(),
              chains = 2, cores = 2, iter = 800, warmup = 400,
              control = list(adapt_delta = 0.99), refresh = 0)

  expect_identical(fit$ne_type, "NSEC")
  peak <- curve_params(fit)
  peak <- peak$Estimate[peak$parameter == "nec"]
  reported <- nec(fit)
  expect_gt(unname(reported[1]), peak)
  expect_equal(as.numeric(reported),
               as.numeric(nsec(fit, resolution = 1000)), tolerance = 0.03)
  expect_gt(unname(ecnsec(fit, nsec = reported[1])[1]), 0)
  expect_identical(rownames(summary(fit, check_fit = FALSE)$nec_vals), "NSEC")
})
