# Stored posterior fits keep these tests independent of Stan compilation.
epx_logged <- function(fit, model = "nec4param") {
  fit$bayesnecformula <- bnf(stats::as.formula(
    paste0('y ~ crf(log(x), model = "', model, '")')
  ))
  fit
}

epx_group <- function(fit) {
  fits <- list(a = fit, b = fit)
  structure(list(fits = fits, group_var = "site", levels = names(fits),
                 formula = fit$bayesnecformula, data = fit$fit$data,
                 family = fit$fit$family, n = c(50L, 50L)),
            class = c("bayesnecgroupfit", "bnecfit"))
}

epx_hurdle <- function(fit) {
  structure(list(growth = fit, survival = fit, data = fit$fit$data,
                 formula = fit$bayesnecformula, y_var = "y",
                 n_exposed = 4L, n_dead = 2L),
            class = c("bayesnechurdlefit", "bnecfit"))
}

test_that("each floor gives the draw-level percent effect", {
  skip_on_cran()
  fit <- nec4param
  x <- 2.5
  grid <- bayesnec:::extended_newdata_eval(fit, 50, NA, NULL)
  curve <- brms::posterior_epred(fit$fit, newdata = grid$newdata,
                                  re_formula = NA)
  at <- bayesnec:::newdata_eval(fit, 2, c(x, x))
  response <- brms::posterior_epred(fit$fit, newdata = at$newdata,
                                     re_formula = NA)[, 2]
  control <- bayesnec:::control_posterior(
    fit, grid$newdata,
    function(nd) brms::posterior_epred(fit$fit, newdata = nd,
                                       re_formula = NA),
    x_at = min(fit$fit$data$x))
  floors <- list(absolute = 0,
                 relative = bayesnec:::ecx_asymptote(fit, "relative"),
                 range = apply(curve[, grid$original, drop = FALSE], 1,
                               min, na.rm = TRUE))
  for (tp in names(floors)) {
    actual <- suppressWarnings(epx(fit, x, type = tp, resolution = 50,
                                    posterior = TRUE))
    expect_equal(as.numeric(actual),
                 as.numeric((control - response) / (control - floors[[tp]]) * 100),
                 tolerance = 1e-10)
  }
})

test_that("ecx crossings have their requested percentage on each draw", {
  skip_on_cran()
  for (tp in c("absolute", "relative", "range")) {
    crossing <- suppressWarnings(ecx(nec4param, ecx_val = 50, type = tp,
                                      resolution = 100, posterior = TRUE))
    picks <- head(which(is.finite(crossing)), 3)
    expect_length(picks, 3)
    for (d in picks) {
      result <- suppressWarnings(epx(nec4param, x = crossing[d], type = tp,
                                      resolution = 100, posterior = TRUE))
      expect_equal(unname(result[d]), 50, tolerance = 0.02)
    }
  }
})

test_that("the control is zero and the scale of x is explicit", {
  skip_on_cran()
  fit <- nec4param
  control <- min(fit$fit$data$x)
  expect_true(all(epx(fit, control, posterior = TRUE, resolution = 30) == 0))
  logged <- epx_logged(fit)
  expect_equal(as.numeric(epx(logged, 2, resolution = 30)),
               as.numeric(epx(logged, log(2), x_scale = "fitted",
                              resolution = 30)), tolerance = 1e-10)
})

test_that("a model set uses the saved equation draw index", {
  skip_on_cran()
  x <- 2.5
  idx <- bayesnec:::pull_draw_index(manec_example,
                                     names(manec_example$mod_fits),
                                     manec_example$sample_size)
  parts <- lapply(names(manec_example$mod_fits), function(m) {
    f <- bayesnec:::pull_equation(manec_example, m)
    epx(f, x, posterior = TRUE, resolution = 30)[idx[[m]]]
  })
  expect_equal(as.numeric(epx(manec_example, x, posterior = TRUE,
                               resolution = 30)),
               unlist(parts, use.names = FALSE), tolerance = 1e-10)
})

test_that("group, hurdle and joint methods return the ecx level shape", {
  skip_on_cran()
  fit <- nec4param
  group <- epx_group(fit)
  for (pool in c("none", "mixture", "mean_estimate", "mean_curve")) {
    out <- suppressWarnings(epx(group, 2, pool = pool, resolution = 20))
    ec <- suppressWarnings(ecx(group, 50, pool = pool, resolution = 20))
    expect_identical(is.data.frame(out), is.data.frame(ec))
    expect_true(!is.null(attr(out, "x_control")))
  }
  hurdle <- epx_hurdle(fit)
  for (part in c("combined", "growth", "survival")) {
    out <- suppressWarnings(epx(hurdle, 2, which = part, resolution = 20))
    expect_length(out, 3)
    expect_identical(attr(out, "component"), part)
  }
  joint <- joint_split()
  result <- suppressWarnings(epx(joint, 1.5, x_range = c(0.05, 2),
                                  resolution = 20))
  expected <- suppressWarnings(ecx(joint, 50, x_range = c(0.05, 2),
                                    resolution = 20))
  expect_identical(dim(result), dim(expected))
})

test_that("invalid and out-of-range concentrations are refused", {
  fit <- nec4param
  expect_error(epx(fit, c(1, 2)), "single|length|number")
  expect_error(epx(fit, 1, type = "direct"), "response value")
  expect_error(epx(fit, 10), "epx\\(\\).*x|outside")
  expect_error(epx(fit, 10, x_range = c(0.05, 3.5)), "outside")
  expect_length(epx(fit, 4, extrapolate = 5, resolution = 20), 3)
})
