# Stored posterior fits keep these tests independent of Stan compilation.
epx_logged <- function(fit, model = "nec4param", tr = "log(x)") {
  fit$bayesnecformula <- bnf(stats::as.formula(
    paste0('y ~ crf(', tr, ', model = "', model, '")')
  ))
  fit
}

epx_unknown <- function(x) log(x)

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
  expect_true(any(epx(fit, x, resolution = 50, posterior = TRUE) > 100))
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
  unknown <- epx_logged(fit, tr = "epx_unknown(x)")
  expect_equal(as.numeric(epx(unknown, 2, resolution = 30)),
               as.numeric(epx(unknown, log(2), x_scale = "fitted",
                              resolution = 30)), tolerance = 1e-4)
  expect_equal(as.numeric(epx(unknown, 4, extrapolate = 5,
                              resolution = 30)),
               as.numeric(epx(unknown, log(4), x_scale = "fitted",
                              extrapolate = 5, resolution = 30)),
               tolerance = 1e-4)
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
  expect_error(epx(group, c(1, 2), pool = "mean_curve"), "number")
  expect_equal(as.numeric(epx(group, 2, 20, pool = "mean_curve")),
               as.numeric(epx(group, 2, resolution = 20,
                              pool = "mean_curve")))
  expect_true(all(epx(group, 2, pool = "mean_curve",
                      x_control = 2, posterior = TRUE) == 0))
  expect_error(epx(group, 10, pool = "mean_curve"),
               "observed range of the predictor")
  expect_error(epx(group, 2, pool = "mean_curve", xform = identity),
               "does not use `xform`", fixed = TRUE)
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
  expect_error(epx(fit, Inf), "finite")
  expect_error(epx(fit, 1, type = "direct"), "response value")
  expect_error(epx(fit, 10), "epx\\(\\).*x|outside")
  expect_error(epx(fit, 10, x_range = c(0.05, 3.5)), "outside")
  expect_length(epx(fit, 4, extrapolate = 5, resolution = 20), 3)
  expect_equal(as.numeric(epx(fit, 2.5, type = "range",
                              extrapolate = 5, resolution = 20)),
               as.numeric(epx(fit, 2.5, type = "range",
                              resolution = 20)), tolerance = 1e-10)
})


test_that("a rising limb can give a negative percent effect", {
  skip_on_cran()
  fit <- nec4param
  fit$model <- "nechorme"
  local_mocked_bindings(joint_hurdle_epred = function(object, newdata,
                                                       dpar = NULL,
                                                       re_formula = NA) {
    response <- 10 + pmax(newdata$x - 0.2, 0)
    matrix(rep(response, each = 3), nrow = 3)
  }, .package = "bayesnec")
  out <- epx(fit, 1, posterior = TRUE, resolution = 20)
  expect_true(all(out < 0))
})

test_that("joint two-block curves select dpar and refuse combined relative", {
  skip_on_cran()
  object <- structure(
    list(model = "nec4param",
         fit = list(family = validate_family("hurdle_gamma")),
         bayesnecformula = bnf(y ~ crf(x, "nec4param"))),
    class = c("bayesnecfit", "bnecfit"))
  expect_error(epx(object, x = 1, type = "relative"),
               "combined curve of the hurdle_gamma family")
  control <- c(0.9, 0.8, 0.95)
  floor <- c(0.1, 0.2, 0.05)
  seen <- character()
  local_mocked_bindings(
    extended_newdata_eval = function(...) {
      x <- seq(0, 3, length.out = 20)
      list(newdata = data.frame(x = x), x_vec = x,
           original = rep(TRUE, length(x)))
    },
    newdata_eval = function(object, resolution, x_range) {
      list(newdata = data.frame(x = x_range))
    },
    resolve_x_control = function(...) list(value = 0, x_range = c(0, 3)),
    curve_extrapolate_limits = function(...) NULL,
    joint_hurdle_epred = function(object, newdata, dpar = NULL,
                                  re_formula = NA) {
      seen <<- c(seen, dpar)
      t(vapply(seq_along(control), function(i) {
        floor[i] + (control[i] - floor[i]) * exp(-newdata$x)
      }, numeric(nrow(newdata))))
    },
    control_posterior = function(...) control,
    ecx_asymptote = function(...) floor,
    count_positive_asymptote = function(object, dpar, asymptote,
                                         newdata) asymptote,
    .package = "bayesnec"
  )
  out <- suppressWarnings(epx(object, x = log(2), type = "relative",
                              dpar = "hu", posterior = TRUE))
  expect_equal(as.numeric(out), rep(50, 3), tolerance = 1e-10)
  expect_true(length(seen) >= 2 && all(seen == "hu"))
})
