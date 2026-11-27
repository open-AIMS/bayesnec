test_that("absolute ecnsec agrees with nsec on single fits and model sets", {
  skip_on_cran()
  for (fit in list(nec4param, ecx4param, manec_example)) {
    draws <- suppressWarnings(nsec(fit, posterior = TRUE, resolution = 50))
    effect <- suppressWarnings(ecnsec(fit, posterior = TRUE, resolution = 50))
    expect_identical(effect, attr(draws, "ecnsec_relativeP"))
    summary <- suppressWarnings(ecnsec(fit, resolution = 50))
    expect_identical(summary, stats::quantile(effect,
      c(0.5, 0.025, 0.975), na.rm = TRUE))
    expect_true(all(effect >= 0))
  }
})

test_that("controls at or below the reference have zero effect", {
  skip_on_cran()
  sig <- 0.2
  draws <- nsec(ecx4param, sig_val = sig, posterior = TRUE, resolution = 50)
  effect <- ecnsec(ecx4param, sig_val = sig, posterior = TRUE, resolution = 50)
  control <- min(ecx4param$fit$data$x)
  expect_identical(effect, attr(draws, "ecnsec_relativeP"))
  expect_true(all(effect[draws == control] == 0))
  expect_equal(sum(effect == 0), sig * length(effect))
})

test_that("relative and range effects use their own denominator", {
  skip_on_cran()
  absolute <- suppressWarnings(ecnsec(nec4param, posterior = TRUE,
                                       resolution = 50))
  relative <- suppressWarnings(ecnsec(nec4param, posterior = TRUE,
                                       resolution = 50, type = "relative"))
  ranged <- suppressWarnings(ecnsec(nec4param, posterior = TRUE,
                                     resolution = 50, type = "range"))
  expect_length(relative, length(absolute))
  expect_length(ranged, length(absolute))
  expect_true(all(relative >= 0))
  expect_true(all(ranged >= 0))
  expect_true(any(relative != absolute))
  expect_true(any(ranged != absolute))
})

test_that("range effects beyond the search grid are censored", {
  skip_on_cran()
  range <- c(0, 0.05)
  crossing <- suppressWarnings(nsec(ecx4param, x_range = range,
                                     resolution = 50, posterior = TRUE))
  effect <- suppressWarnings(ecnsec(ecx4param, x_range = range,
                                     resolution = 50, type = "range",
                                     posterior = TRUE))
  above <- is.infinite(crossing) & crossing > 0
  expect_true(any(above))
  expect_true(all(is.infinite(effect[above]) & effect[above] > 0))
  expect_identical(attr(effect, "censored")$above, above)
  summary <- suppressWarnings(ecnsec(ecx4param, x_range = range,
                                      resolution = 50, type = "range"))
  expect_true(all(is.finite(summary)))
  expect_lte(max(summary), 100)
})

test_that("ecnsec refuses removed and invalid arguments", {
  for (arg in c("nsec", "xform", "x_scale")) {
    call <- c(list(nec4param), stats::setNames(list(1), arg))
    expect_error(do.call(ecnsec, call),
                 paste0("has no `", arg, "` argument"), fixed = TRUE)
  }
  expect_error(ecnsec(nec4param, nsec = 1), "use epx()", fixed = TRUE)
  expect_error(ecnsec(nec4param, resolution = 1), "at least 2")
  expect_error(ecnsec(nec4param, sig_val = 1), "sig_val")
  expect_error(ecnsec(nec4param, type = "direct"), "response value")
  expect_error(ecnsec(nec4param, foo = 1),
               "ecnsec() does not use `foo`", fixed = TRUE)
})

test_that("ecnsec agrees with the NSEC attribute on a joint refit", {
  skip_on_cran()
  joint <- joint_split()
  draws <- suppressWarnings(nsec(joint, posterior = TRUE, resolution = 50))
  effect <- suppressWarnings(ecnsec(joint, posterior = TRUE, resolution = 50))
  expect_identical(names(effect), names(draws))
  for (level in names(draws)) {
    expect_identical(effect[[level]],
                     attr(draws[[level]], "ecnsec_relativeP"))
  }
})
