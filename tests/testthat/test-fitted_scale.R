# Relabel stored formulas without refitting. Compare calls on the same
# posterior; the relabelled fits do not validate numerical estimates.
# The stored predictor is positive, so log(x) stays finite.
logged_x_fit <- function(fit, model, tr = "log(x)") {
  fit$bayesnecformula <- bnf(stats::as.formula(
    paste0("y ~ crf(", tr, ", model = \"", model, "\")")
  ))
  fit
}

logged_x_set <- function(manec, tr = "log(x)") {
  for (m in names(manec$mod_fits)) {
    manec$mod_fits[[m]] <- logged_x_fit(manec$mod_fits[[m]], m, tr)
  }
  manec
}

fake_scale_group <- function(fits) {
  structure(list(fits = fits, group_var = "site", levels = names(fits),
                 formula = fits[[1]]$bayesnecformula,
                 data = nec4param$fit$data, family = nec4param$fit$family,
                 n = rep(50L, length(fits))),
            class = c("bayesnecgroupfit", "bnecfit"))
}

fake_scale_hurdle <- function(growth, survival) {
  structure(list(growth = growth, survival = survival,
                 data = nec4param$fit$data,
                 formula = growth$bayesnecformula,
                 y_var = "y", n_exposed = 4L, n_dead = 2L),
            class = c("bayesnechurdlefit", "bnecfit"))
}

q <- function(expr) suppressWarnings(suppressMessages(expr))

test_that("the inverse is named in closed form only where one is known", {
  inv <- function(txt) bayesnec:::x_inverse_text(str2lang(txt))
  expect_identical(inv("log(x)"), "exp")
  expect_identical(inv("sqrt(x)"), "function(x) x^2")
  expect_identical(inv("log(x + 1)"), "function(x) exp(x) - 1")
  expect_identical(inv("log(1 + x)"), "function(x) exp(x) - 1")
  expect_identical(inv("log10(x - 0.5)"), "function(x) 10^x + 0.5")
  expect_identical(inv("log1p(x)"), "expm1")
  expect_identical(inv("exp(x)"), "log")
  # A second argument, a constant on the other side of a subtraction and a
  # function the table does not hold are not guessed at.
  expect_null(inv("log(x, 10)"))
  expect_null(inv("sqrt(1 - x)"))
  expect_null(inv("squared(x)"))
  expect_null(inv("-x"))
  # I() changes only the class, so what it wraps decides.
  expect_identical(inv("I(log(x))"), "exp")
})

test_that("the transformation is read from the crf() term", {
  expect_null(bayesnec:::inline_x_transform(nec4param))
  expect_null(bayesnec:::inline_x_transform(NULL))
  # Not a fit: left to the caller's validation, which names what it accepts.
  expect_null(bayesnec:::inline_x_transform(12))
  expect_error(compare_estimates(list(a = nec4param, b = 12)),
               "Not all objects in x are of class")
  tr <- bayesnec:::inline_x_transform(logged_x_fit(nec4param, "nec4param"))
  expect_identical(tr$label, "log(x)")
  expect_identical(tr$variable, "x")
  # The predictor is read as the model frame reads it, which fits crf(-x) on
  # x, so a fit holding it transforms nothing.
  neg <- logged_x_fit(nec4param, "nec4param", "-x")
  expect_null(bayesnec:::inline_x_transform(neg))
  expect_equal(bayesnec:::sub_x_transformation(2, neg$bayesnecformula), 2)
  # I(x) is the predictor itself, so there is nothing to invert. I() around a
  # transformation is inverted as the transformation is.
  as_is <- logged_x_fit(nec4param, "nec4param", "I(x)")
  expect_null(bayesnec:::inline_x_transform(as_is))
  expect_equal(bayesnec:::sub_x_transformation(2, as_is$bayesnecformula), 2,
               ignore_attr = TRUE)
  expect_identical(q(nec(as_is)), q(nec(nec4param)))
  wrapped <- logged_x_fit(nec4param, "nec4param", "I(log(x))")
  expect_identical(q(nec(wrapped)), q(nec(wrapped, xform = exp)))
})

test_that("xform is NULL or a function", {
  expect_error(nec(nec4param, xform = "exp"), "xform must be a function")
  expect_error(curve_params(nec4param, xform = 2), "xform must be a function")
  expect_no_error(nec(nec4param, xform = NULL))
})

test_that("nec() defaults to the recorded scale and keeps a supplied xform", {
  tf <- logged_x_fit(nec4param, "nec4param")
  # The closed-form inverse is base exp(), so the default and the explicit
  # call are the same computation, and nothing is printed about the scale.
  expect_silent(by_default <- nec(tf))
  expect_identical(by_default, nec(tf, xform = exp))
  # identity is the fitted scale, as an untransformed fit reports it.
  expect_identical(nec(tf, xform = identity), nec(nec4param))
  # A supplied xform is applied to the estimate on the fitted scale.
  expect_equal(as.numeric(nec(tf, xform = function(z) 2 * z)),
               2 * as.numeric(nec(tf, xform = identity)))
  expect_equal(as.numeric(nec(tf, posterior = TRUE)),
               exp(as.numeric(nec(nec4param, posterior = TRUE))))
  # A model set inverts its mixture of NEC and NSEC draws in the same way.
  tm <- logged_x_set(manec_example)
  expect_identical(q(nec(tm)), q(nec(tm, xform = exp)))
  expect_identical(q(nec(tm, xform = identity)), q(nec(manec_example)))
})

test_that("a fit with no inline transformation takes NULL as identity", {
  # NULL is the default; on these fits it must give exactly what identity, the
  # default it replaced, gives.
  expect_identical(nec(nec4param), nec(nec4param, xform = identity))
  expect_identical(q(nec(manec_example)), q(nec(manec_example,
                                                xform = identity)))
  expect_identical(curve_params(nec4param),
                   curve_params(nec4param, xform = identity))
  expect_identical(q(summary(nec4param))$nec_vals,
                   q(summary(nec4param, xform = identity))$nec_vals)
  skip_on_cran()
  expect_identical(q(ecx(ecx4param, resolution = 20)),
                   q(ecx(ecx4param, resolution = 20, xform = identity)))
  expect_identical(q(nsec(ecx4param, resolution = 20)),
                   q(nsec(ecx4param, resolution = 20, xform = identity)))
  expect_identical(ecnsec(nec4param, nsec = 1, resolution = 20),
                   ecnsec(nec4param, nsec = 1, resolution = 20,
                          xform = identity))
  expect_identical(q(ggbnec_data(nec4param, add_ecx = TRUE)),
                   q(ggbnec_data(nec4param, add_ecx = TRUE,
                                 xform = identity)))
})

test_that("ecx() and nsec() read the recorded grid without an inverse", {
  skip_on_cran()
  for (case in list(list(tr = "log(x)", inv = exp),
                    list(tr = "sqrt(x)", inv = function(x) x^2))) {
    te <- logged_x_fit(ecx4param, "ecx4param", case$tr)
    # The default skips the transformation; the explicit inverse applies it
    # and then undoes it, so the two agree to rounding.
    expect_equal(q(ecx(te, resolution = 20)),
                 q(ecx(te, resolution = 20, xform = case$inv)),
                 tolerance = 1e-12)
    expect_equal(q(nsec(te, resolution = 20)),
                 q(nsec(te, resolution = 20, xform = case$inv)),
                 tolerance = 1e-12)
    # The curve is searched on the same recorded grid whatever the formula
    # says, so the default is the untransformed fit's answer.
    expect_identical(q(ecx(te, resolution = 20)),
                     q(ecx(ecx4param, resolution = 20)))
    # identity gives the fitted scale, draw by draw.
    expect_equal(
      as.numeric(q(ecx(te, resolution = 20, xform = identity,
                       posterior = TRUE))),
      bayesnec:::sub_x_transformation(
        as.numeric(q(ecx(te, resolution = 20, posterior = TRUE))),
        te$bayesnecformula
      ),
      tolerance = 1e-12
    )
  }
  # The censoring record stays on the recorded scale: a draw the curve does
  # not bring to the ECx90 target is censored at the top of the recorded grid.
  te <- logged_x_fit(ecx4param, "ecx4param")
  e90 <- q(ecx(te, ecx_val = 90, resolution = 20, posterior = TRUE))
  expect_equal(attr(e90, "censored")$upper, max(ecx4param$fit$data$x))
})

test_that("an inline sqrt() is inverted exactly by every estimator", {
  sq <- function(x) x^2
  tf <- logged_x_fit(nec4param, "nec4param", "sqrt(x)")
  expect_identical(nec(tf), nec(tf, xform = sq))
  expect_equal(curve_params(tf), curve_params(tf, xform = sq))
  expect_equal(q(summary(tf))$nec_vals, q(summary(tf, xform = sq))$nec_vals)
  skip_on_cran()
  tm <- logged_x_set(manec_example, "sqrt(x)")
  expect_equal(curve_params(tm), curve_params(tm, xform = sq))
  expect_equal(q(nsec(tm, resolution = 20)),
               q(nsec(tm, resolution = 20, xform = sq)), tolerance = 1e-12)
})

test_that("ecnsec() reads the default nsec() at the same concentration", {
  skip_on_cran()
  for (case in list(list(tr = "log(x)", inv = exp),
                    list(tr = "sqrt(x)", inv = function(x) x^2))) {
    tf <- logged_x_fit(nec4param, "nec4param", case$tr)
    # One draw, so that a quantile taken on either scale does not differ in
    # the interpolation between two draws.
    by_default <- q(nsec(tf, resolution = 20, posterior = TRUE))[[1]]
    fitted <- q(nsec(tf, resolution = 20, xform = identity,
                     posterior = TRUE))[[1]]
    expect_equal(case$inv(fitted), by_default, tolerance = 1e-12)
    # NULL reads nsec as given, on the recorded scale, which is the scale
    # nsec() returns by default. The explicit inverse reads a fitted-scale
    # nsec at the same concentration.
    expect_equal(ecnsec(tf, nsec = by_default, resolution = 20),
                 ecnsec(tf, nsec = fitted, resolution = 20, xform = case$inv),
                 tolerance = 1e-10)
    h <- fake_scale_hurdle(tf, tf)
    expect_equal(ecnsec(h, nsec = by_default, resolution = 20),
                 q(ecnsec(h, nsec = fitted, resolution = 20,
                          xform = case$inv)),
                 tolerance = 1e-10)
  }
})

test_that("a transformation with no closed form is inverted on the grid", {
  # asinh() is not in x_inverse_text()'s table, so the inverse is read off the
  # stored prediction grid by linear interpolation. sinh() is the exact
  # inverse, supplied by hand for comparison.
  tf <- logged_x_fit(nec4param, "nec4param", "asinh(x)")
  expect_equal(nec(tf), nec(tf, xform = sinh), tolerance = 1e-5)
  expect_equal(curve_params(tf), curve_params(tf, xform = sinh),
               tolerance = 1e-5)
  tm <- logged_x_set(manec_example, "asinh(x)")
  expect_equal(q(nec(tm)), q(nec(tm, xform = sinh)), tolerance = 1e-5)
  skip_on_cran()
  # ecx() needs no inverse at all.
  te <- logged_x_fit(ecx4param, "ecx4param", "asinh(x)")
  expect_equal(q(ecx(te, resolution = 20)),
               q(ecx(te, resolution = 20, xform = sinh)), tolerance = 1e-12)
})

test_that("the grid is refined where it is coarse on the fitted scale", {
  # A grid even on the recorded scale is coarse on log(x) near its foot.
  # unknown(x) has no closed form, so the log is inverted on the grid.
  grid <- seq(0.001, 1000, length.out = 1000)
  unknown <- function(x) log(x)
  f <- bnf(y ~ crf(unknown(x), model = "nec4param"))
  tr <- bayesnec:::inline_x_transform(list(bayesnecformula = f))
  map <- bayesnec:::recorded_x_map(f, grid, tr)
  expect_false(is.null(attr(map, "grid_inverse")))
  at <- log(c(0.002, 0.05, 3, 700))
  expect_equal(map(at), exp(at), tolerance = 1e-5)
  # A transformation that turns back on itself has no inverse.
  folded <- bnf(y ~ crf(I((x - 500)^2), model = "nec4param"))
  tr <- bayesnec:::inline_x_transform(list(bayesnecformula = folded))
  expect_error(bayesnec:::recorded_x_map(folded, grid, tr),
               "not strictly monotone")
})

test_that("a draw beyond the grid of a numeric inverse is not given a value", {
  # asinh(x / 4) maps the grid to 0.008 to 0.73, below every nec draw of this
  # posterior, and the stored censoring record does not mark those draws.
  tf <- logged_x_fit(nec4param, "nec4param", "asinh(x / 4)")
  expect_error(nec(tf), "lie beyond that grid")
  expect_error(nec(tf), "Pass the inverse of asinh(x/4) as xform",
               fixed = TRUE)
  # curve_params() reports the rest of the table, with the nec row NA.
  expect_message(cp <- curve_params(tf), "nec of nec4param")
  expect_true(all(is.na(cp[cp$parameter == "nec", c("Estimate", "Q2.5",
                                                     "Q97.5")])))
  expect_false(anyNA(cp[cp$parameter != "nec", "Estimate"]))
  # The remedies the messages name both work.
  expect_no_error(nec(tf, xform = function(z) 4 * sinh(z)))
  expect_identical(nec(tf, xform = identity), nec(nec4param))
})

test_that("a decreasing transformation keeps the interval in order", {
  skip_on_cran()
  # I(4 - x) has no closed form in the table, so it goes through the grid,
  # which inverts a linear map exactly.
  back <- function(z) 4 - z
  tf <- logged_x_fit(nec4param, "nec4param", "I(4 - x)")
  by_default <- nec(tf)
  expect_equal(by_default, nec(tf, xform = back), tolerance = 1e-10)
  expect_lt(by_default[["Q2.5"]], by_default[["Q97.5"]])
  te <- logged_x_fit(ecx4param, "ecx4param", "I(4 - x)")
  for (v in c(10, 90)) {
    d <- q(ecx(te, ecx_val = v, resolution = 20))
    e <- q(ecx(te, ecx_val = v, resolution = 20, xform = back))
    expect_equal(d, e, tolerance = 1e-10)
    expect_identical(attr(d, "censored_summary")$bound,
                     attr(e, "censored_summary")$bound)
  }
})

test_that("extrapolate stays on the scale of the estimate it censors", {
  skip_on_cran()
  te <- logged_x_fit(ecx4param, "ecx4param")
  d <- q(nsec(te, resolution = 20, extrapolate = 5, posterior = TRUE))
  e <- q(nsec(te, resolution = 20, extrapolate = 5, xform = exp,
              posterior = TRUE))
  expect_equal(as.numeric(d), as.numeric(e), tolerance = 1e-12)
  # The limit is a concentration: 5, not log(5).
  expect_equal(attr(d, "censored")$upper, 5)
  expect_equal(attr(e, "censored")$upper, 5, tolerance = 1e-12)
  tm <- logged_x_set(manec_example)
  expect_equal(q(nec(tm, extrapolate = 5, resolution = 20)),
               q(nec(tm, extrapolate = 5, resolution = 20, xform = exp)))
})

test_that("a group, a hurdle pair and a joint refit follow the default", {
  tf <- logged_x_fit(nec4param, "nec4param")
  g <- fake_scale_group(list(a = tf, b = tf))
  expect_identical(q(nec(g)), q(nec(g, xform = exp)))
  h <- fake_scale_hurdle(tf, tf)
  expect_identical(q(nec(h)), q(nec(h, xform = exp)))
  expect_identical(q(nec(h, which = "growth", xform = identity)),
                   q(nec(fake_scale_hurdle(nec4param, nec4param),
                         which = "growth")))
  skip_on_cran()
  expect_equal(q(ecx(g, resolution = 20)),
               q(ecx(g, resolution = 20, xform = exp)), tolerance = 1e-12)
  expect_equal(q(ecx(h, resolution = 20)),
               q(ecx(h, resolution = 20, xform = exp)), tolerance = 1e-12)
  expect_equal(q(nsec(h, resolution = 20)),
               q(nsec(h, resolution = 20, xform = exp)), tolerance = 1e-12)
  j <- joint_split()
  j$bayesnecformula <- bnf(y ~ crf(log(x), model = "nec4param"))
  expect_equal(q(ecx(j, resolution = 20)),
               q(ecx(j, resolution = 20, xform = exp)), tolerance = 1e-12)
  expect_equal(q(nsec(j, resolution = 20)),
               q(nsec(j, resolution = 20, xform = exp)), tolerance = 1e-12)
})

test_that("summary() reports the no-effect row as nec() does", {
  tf <- logged_x_fit(nec4param, "nec4param")
  expect_equal(as.numeric(q(summary(tf))$nec_vals),
               as.numeric(bayesnec:::estimates_summary(exp(
                 nec4param$ne_posterior
               ))))
  # identity is the stored row on the fitted scale.
  expect_identical(q(summary(tf, xform = identity))$nec_vals,
                   q(summary(nec4param))$nec_vals)
  tm <- logged_x_set(manec_example)
  expect_equal(q(summary(tm, check_fit = FALSE))$nec_vals,
               q(summary(tm, xform = exp, check_fit = FALSE))$nec_vals)
  skip_on_cran()
  # With ecx = TRUE the ECx rows take the same default as the no-effect row.
  s <- q(summary(tf, ecx = TRUE, ecx_vals = 50, resolution = 20))
  expect_identical(s$ecs[[1]], q(ecx(tf, ecx_val = 50, resolution = 20,
                                     x_range = range(tf$pred_vals$data$x))))
})

test_that("compare_estimates() and exceedance() read the recorded scale", {
  skip_on_cran()
  tf <- logged_x_fit(nec4param, "nec4param")
  te <- logged_x_fit(ecx4param, "ecx4param")
  cmp <- q(compare_estimates(list(a = tf, b = te), comparison = "n(s)ec"))
  expect_equal(as.numeric(cmp$posterior_list$a),
               exp(as.numeric(nec4param$ne_posterior)))
  cmp <- q(compare_estimates(list(a = tf, b = te), comparison = "nsec",
                             resolution = 20))
  expect_equal(as.numeric(cmp$posterior_list$b),
               as.numeric(q(nsec(te, resolution = 20, posterior = TRUE,
                                 x_range = bayesnec:::return_x_range(
                                   list(tf, te)
                                 )))))
  # A threshold is read as a concentration by default, and as a log
  # concentration with xform = identity.
  at <- unname(q(nec(tf))[[1]])
  expect_equal(q(exceedance(tf, threshold = at))$prob,
               q(exceedance(tf, threshold = log(at), xform = identity))$prob)
})

test_that("the plots annotate the numbers the estimators return", {
  skip_on_cran()
  tf <- logged_x_fit(nec4param, "nec4param")
  g <- q(ggbnec_data(tf, add_ecx = TRUE))
  expect_equal(unique(g$nec_vals[!is.na(g$nec_vals)]),
               as.numeric(bayesnec:::summary_ne_vals(tf, NULL)))
  # plot_ecx() asks for type = "range" under a gaussian response.
  expect_equal(unique(g$ecx_vals[!is.na(g$ecx_vals)]),
               as.numeric(q(ecx(tf, type = "range"))))
  # identity on a transformed fit asks for nothing more than the default.
  expect_identical(g, q(ggbnec_data(tf, add_ecx = TRUE, xform = identity)))
  # A supplied xform is applied to the estimates on the fitted scale.
  g_exp <- q(ggbnec_data(tf, xform = exp))
  expect_equal(unique(g_exp$nec_vals[!is.na(g_exp$nec_vals)]),
               exp(as.numeric(nec4param$ne)))
  tm <- logged_x_set(manec_example)
  g <- q(ggbnec_data(tm))
  expect_equal(unique(g$nec_vals[!is.na(g$nec_vals)]),
               as.numeric(bayesnec:::summary_ne_vals(tm, NULL)))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(q(plot(tf, add_ec10 = TRUE)))
  expect_no_error(q(plot(tm, add_ec10 = TRUE)))
  h <- fake_scale_hurdle(tf, tf)
  drawn <- NULL
  local_mocked_bindings(abline = function(v = NULL, ...) drawn <<- v,
                        .package = "bayesnec")
  q(plot(h))
  expect_equal(as.numeric(drawn), as.numeric(q(nec(h))))
})

test_that("crf((log(x))) is read as crf(log(x)) on every path (#479)", {
  # The model frame and the brms formula drop the parentheses, so the
  # estimators must as well.
  bare <- logged_x_fit(nec4param, "nec4param")
  paren <- logged_x_fit(nec4param, "nec4param", "(log(x))")
  expect_identical(q(nec(paren)), q(nec(bare)))
  skip_on_cran()
  bare <- logged_x_fit(ecx4param, "ecx4param")
  paren <- logged_x_fit(ecx4param, "ecx4param", "(log(x))")
  expect_equal(q(ecx(paren, resolution = 20, xform = identity)),
               q(ecx(bare, resolution = 20, xform = identity)))
  expect_equal(unname(q(ecx(paren, resolution = 20, xform = identity))[1]),
               log(unname(q(ecx(ecx4param, resolution = 20))[1])),
               tolerance = 1e-3)
  expect_equal(q(nsec(paren, resolution = 20)),
               q(nsec(bare, resolution = 20)))
})


test_that("plot labels do not require a default inverse", {
  skip_on_cran()
  inverse <- function(z) 4 * sinh(z)
  tf <- logged_x_fit(nec4param, "nec4param", "asinh(x / 4)")
  tm <- logged_x_set(manec_example, "asinh(x / 4)")
  g <- fake_scale_group(list(a = tf, b = tf))
  # The default cannot place these draws, but an explicit inverse can.
  expect_error(nec(tf), "lie beyond that grid")
  expect_s3_class(q(autoplot(tf, xform = inverse)), "ggplot")
  expect_s3_class(q(autoplot(tm, xform = inverse)), "ggplot")
  expect_s3_class(q(autoplot(g, xform = inverse)), "ggplot")
  expect_s3_class(q(autoplot(tm, model = "nec4param", average = FALSE,
                            xform = inverse)), "ggplot")
  pages <- q(autoplot(tm, model = "nec4param", average = FALSE,
                      multi_facet = FALSE, plot = FALSE, xform = inverse))
  expect_s3_class(pages[[1]], "ggplot")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(q(plot(tf, xform = inverse)))
  expect_no_error(q(plot(tm, xform = inverse)))
})

test_that("disabled plot annotations do not require an inverse", {
  skip_on_cran()
  tf <- logged_x_fit(nec4param, "nec4param", "asinh(x / 4)")
  tm <- logged_x_set(manec_example, "asinh(x / 4)")
  g <- fake_scale_group(list(a = tf, b = tf))
  expect_s3_class(q(autoplot(tf, nec = FALSE)), "ggplot")
  expect_s3_class(q(autoplot(tm, nec = FALSE)), "ggplot")
  expect_s3_class(q(autoplot(g, nec = FALSE)), "ggplot")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(q(plot(tf, add_nec = FALSE)))
  expect_no_error(q(plot(tm, add_nec = FALSE)))
})


test_that("numeric inversion retains infinite fitted endpoints", {
  grid <- seq(0, 3.22, length.out = 1000)
  at <- c(0, 1e-10, 1e-5, 0.1, 1, 3)
  for (sign in c(1, -1)) {
    f <- bnf(stats::as.formula(paste0(
      "y ~ crf(I(", sign, " * (log(x) + 1)), model = \"ecx4param\")"
    )))
    tr <- bayesnec:::inline_x_transform(list(bayesnecformula = f))
    map <- bayesnec:::recorded_x_map(f, grid, tr)
    fitted <- sign * (log(at) + 1)
    mapped <- bayesnec:::map_draws(fitted, map)
    expect_equal(mapped[1], 0)
    expect_equal(mapped[-1] / at[-1], rep(1, length(at) - 1),
                 tolerance = 1e-5)
    expect_true(is.na(map(NA_real_)))
    expect_true(is.na(map(sign * Inf)))
    # Finite draws beyond the other end still require an explicit inverse.
    expect_error(bayesnec:::map_draws(sign * (log(4) + 1), map),
                 "lie beyond that grid")
    original <- bayesnec:::censoring_record(max(at), min(at),
                                           rep(FALSE, length(at)),
                                           c(TRUE, rep(FALSE, length(at) - 1)))
    cens <- bayesnec:::xform_censoring(original,
                                      function(z) sign * (log(z) + 1))
    recorded <- bayesnec:::xform_censoring(cens, map)
    expect_equal(recorded$lower, original$lower)
    expect_equal(recorded$upper, original$upper, tolerance = 1e-5)
    expect_identical(recorded$above, original$above)
    expect_identical(recorded$below, original$below)
  }
})

test_that("summary and comparisons invert a stored grid beginning at zero", {
  skip_on_cran()
  f <- bnf(y ~ crf(I(log(x) + 1), model = "ecx4param"))
  # Reuse the sampled curve and rebuild only its stored NSEC and grid.
  tf <- q(expand_nec(list(fit = ecx4param$fit, model = "ecx4param",
                          bayesnecformula = f),
                     formula = f, x_range = c(0, 3.22), resolution = 100,
                     sig_val = 0.1, model = "ecx4param"))
  tf <- bayesnec:::allot_class(tf, c("bayesnecfit", "bnecfit"))
  inverse <- function(z) exp(z - 1)
  expect_equal(q(summary(tf))$nec_vals,
               q(summary(tf, xform = inverse))$nec_vals, tolerance = 1e-5)
  expect_equal(as.numeric(bayesnec:::return_nec_post(tf)),
               inverse(as.numeric(tf$ne_posterior)), tolerance = 1e-5)
})
