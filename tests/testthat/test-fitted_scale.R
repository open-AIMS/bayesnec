# #299, D23. Where crf() transforms the predictor inline and no xform is
# supplied, the estimators say once per call that the result is on the
# transformed scale and name the xform that inverts it. The returned values are
# unchanged.
#
# The fixtures relabel a stored fit's formula, as transformed_response_fit() in
# setup.R and transformed_x_fit() in test-curve_params.R do. The stored fit was
# not fitted with this formula. That is safe here: the message reads the
# formula only, and every assertion is about the message or about the value
# being the one the unrelabelled fit returns. manec_example's x runs from 0.032
# to 3.22, so log() is finite throughout.
logged_x_fit <- function(fit, model, tr = "log(x)") {
  fit$bayesnecformula <- bnf(stats::as.formula(
    paste0("y ~ crf(", tr, ", model = \"", model, "\")")
  ))
  fit
}

logged_x_set <- function(manec) {
  for (m in names(manec$mod_fits)) {
    manec$mod_fits[[m]] <- logged_x_fit(manec$mod_fits[[m]], m)
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

# Only the scale message is counted, so that the censoring warnings and the
# other messages these calls raise cannot make a count pass or fail.
scale_messages <- function(expr) {
  msgs <- testthat::capture_messages(suppressWarnings(expr))
  grep("transforms its predictor inline", msgs, value = TRUE)
}

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
  # A leading minus is dropped by stats::model.frame(), so the model frame
  # calls crf(-x) untransformed while sub_x_transformation() negates the
  # estimate. The crf() term is what decides.
  neg <- logged_x_fit(nec4param, "nec4param", "-x")
  expect_identical(bayesnec:::inline_x_transform(neg)$label, "-x")
  expect_equal(bayesnec:::sub_x_transformation(2, neg$bayesnecformula), -2)
  # I(x) is the predictor itself, so there is nothing to report. I() around a
  # transformation still reports it.
  as_is <- logged_x_fit(nec4param, "nec4param", "I(x)")
  expect_null(bayesnec:::inline_x_transform(as_is))
  expect_equal(bayesnec:::sub_x_transformation(2, as_is$bayesnecformula), 2,
               ignore_attr = TRUE)
  expect_length(scale_messages(nec(as_is)), 0)
  wrapped <- logged_x_fit(nec4param, "nec4param", "I(log(x))")
  expect_match(scale_messages(nec(wrapped)), "Pass xform = exp to nec()",
               fixed = TRUE)
})

test_that("nec() reports once for a bare call and leaves the value alone", {
  tf <- logged_x_fit(nec4param, "nec4param")
  msgs <- scale_messages(out <- nec(tf))
  expect_length(msgs, 1)
  expect_match(msgs, "nec4param fit transforms its predictor inline as log(x)",
               fixed = TRUE)
  expect_match(msgs, "Pass xform = exp to nec()", fixed = TRUE)
  expect_equal(out, suppressWarnings(nec(nec4param)))
  # Silent where an inverse was supplied, and where nothing is transformed.
  expect_length(scale_messages(nec(tf, xform = exp)), 0)
  expect_length(scale_messages(nec(nec4param)), 0)
  # The gate is the caller's only for the length of the call.
  expect_null(getOption("bayesnec.xform_reported"))
  expect_length(scale_messages(nec(tf)), 1)
})

test_that("an error while reporting leaves no gate set for later calls", {
  # The caller registers the restore only once report_fitted_scale() returns,
  # so the option must not be set before everything that can fail has run.
  # Left set, it silenced every later call in the session.
  tf <- logged_x_fit(nec4param, "nec4param")
  local_mocked_bindings(inline_x_transform = function(object) stop("boom"),
                        .package = "bayesnec")
  expect_error(nec(tf), "boom")
  expect_null(getOption("bayesnec.xform_reported"))
})

test_that("the message names the inverse of the transformation found", {
  one <- function(tr) scale_messages(nec(logged_x_fit(nec4param, "nec4param",
                                                       tr)))
  expect_match(one("sqrt(x)"), "xform = function(x) x^2", fixed = TRUE)
  expect_match(one("log(x + 1)"), "xform = function(x) exp(x) - 1",
               fixed = TRUE)
  expect_match(one("log(x, 10)"),
               "Pass the inverse of log(x, 10) as xform to nec()",
               fixed = TRUE)
})

test_that("a model set reports once, not once per equation", {
  tm <- logged_x_set(manec_example)
  expect_length(scale_messages(nec(tm)), 1)
  expect_match(scale_messages(nec(tm)), "The fitted model set", fixed = TRUE)
  expect_length(scale_messages(nec(tm, xform = exp)), 0)
  expect_length(scale_messages(nec(manec_example)), 0)
  skip_on_cran()
  expect_length(scale_messages(ecx(tm, resolution = 20)), 1)
  expect_length(scale_messages(nsec(tm, resolution = 20)), 1)
  expect_length(scale_messages(ecx(tm, resolution = 20, xform = exp)), 0)
  expect_null(getOption("bayesnec.xform_reported"))
})

test_that("ecx(), nsec() and ecnsec() report once and not with an xform", {
  skip_on_cran()
  tf <- logged_x_fit(nec4param, "nec4param")
  msgs <- scale_messages(ecx(tf, resolution = 20))
  expect_length(msgs, 1)
  expect_match(msgs, "Pass xform = exp to ecx()", fixed = TRUE)
  expect_length(scale_messages(ecx(tf, resolution = 20, xform = exp)), 0)
  expect_length(scale_messages(ecx(nec4param, resolution = 20)), 0)
  expect_length(scale_messages(nsec(tf, resolution = 20)), 1)
  expect_length(scale_messages(nsec(tf, resolution = 20, xform = exp)), 0)
  # ecnsec() returns a percentage; the scale it is told about is the one its
  # nsec argument is read on.
  msgs <- scale_messages(ecnsec(tf, nsec = 1, resolution = 20))
  expect_length(msgs, 1)
  expect_match(msgs, "ecnsec() reads nsec on the scale of x", fixed = TRUE)
  expect_match(msgs, "pass xform = exp to ecnsec()", fixed = TRUE)
  expect_length(scale_messages(ecnsec(tf, nsec = 0, resolution = 20,
                                      xform = exp)), 0)
})

test_that("a comparison reports once, not once per fit", {
  skip_on_cran()
  tf <- logged_x_fit(nec4param, "nec4param")
  te <- logged_x_fit(ecx4param, "ecx4param")
  x <- list(a = tf, b = te)
  for (cmp in c("nsec", "ecx", "n(s)ec")) {
    msgs <- scale_messages(compare_estimates(x, comparison = cmp,
                                             resolution = 20))
    expect_length(msgs, 1)
    expect_match(msgs, "compare_estimates() takes no xform", fixed = TRUE)
  }
  expect_length(scale_messages(compare_posterior(x, comparison = "ecx",
                                                 resolution = 20)), 1)
  # Nothing transformed, nothing said.
  expect_length(scale_messages(
    compare_estimates(list(a = nec4param, b = ecx4param), comparison = "nsec",
                      resolution = 20)
  ), 0)
  expect_null(getOption("bayesnec.xform_reported"))
})

test_that("average_estimates() reports once, and not with an xform", {
  skip_on_cran()
  tf <- logged_x_fit(nec4param, "nec4param")
  te <- logged_x_fit(ecx4param, "ecx4param")
  msgs <- scale_messages(average_estimates(list(a = tf, b = tf)))
  expect_length(msgs, 1)
  expect_match(msgs, "Pass xform = exp to average_estimates()", fixed = TRUE)
  expect_length(scale_messages(
    average_estimates(list(a = tf, b = te), estimate = "ecx",
                      resolution = 20)
  ), 1)
  expect_length(scale_messages(
    average_estimates(list(a = tf, b = te), estimate = "ecx",
                      resolution = 20, xform = exp)
  ), 0)
})

test_that("a group and a hurdle pair report once for the call", {
  tf <- logged_x_fit(nec4param, "nec4param")
  g <- fake_scale_group(list(a = tf, b = tf))
  msgs <- scale_messages(nec(g))
  expect_length(msgs, 1)
  expect_match(msgs, "The fitted group", fixed = TRUE)
  expect_length(scale_messages(nec(g, xform = exp)), 0)
  # xform found by position and by partial name, as the per-level nec() call
  # finds it, so the group is not told about a scale already inverted.
  expect_length(scale_messages(nec(g, FALSE, exp)), 0)
  expect_length(scale_messages(nec(g, xfo = exp)), 0)
  expect_length(scale_messages(nec(g, FALSE)), 1)
  # The option is restored when the call stops after reporting.
  expect_error(suppressMessages(nec(g, posterior = TRUE)), "one row per level")
  expect_null(getOption("bayesnec.xform_reported"))
  h <- fake_scale_hurdle(tf, tf)
  msgs <- scale_messages(nec(h))
  expect_length(msgs, 1)
  expect_match(msgs, "The fitted hurdle pair", fixed = TRUE)
  expect_length(scale_messages(nec(h, xform = exp)), 0)
  skip_on_cran()
  expect_length(scale_messages(ecx(g, resolution = 20)), 1)
  expect_length(scale_messages(nsec(g, resolution = 20)), 1)
  expect_length(scale_messages(ecx(h, resolution = 20)), 1)
  expect_length(scale_messages(nsec(h, resolution = 20)), 1)
  expect_length(scale_messages(nsec(g, 0.01, 20, NA, exp)), 0)
  expect_length(scale_messages(ecx(g, 10, 20, FALSE, "absolute", NA, exp)), 0)
})

test_that("ecnsec() on a hurdle pair asks for nsec on the recorded scale", {
  # This method reads nsec on the recorded grid as supplied and applies xform
  # to the percentage it returns, so an nsec() value left on the transformed
  # scale is read as a concentration. The message names the xform to give
  # nsec() rather than one to give ecnsec(), and it is raised whatever xform
  # was given, because none changes the scale nsec is read on.
  skip_on_cran()
  tf <- logged_x_fit(nec4param, "nec4param")
  h <- fake_scale_hurdle(tf, tf)
  msgs <- scale_messages(ecnsec(h, nsec = 1, resolution = 20))
  expect_length(msgs, 1)
  expect_match(msgs, "reads nsec on the scale of x as supplied", fixed = TRUE)
  expect_match(msgs, "as nsec() returns it given xform = exp", fixed = TRUE)
  expect_false(grepl("to ecnsec()", msgs, fixed = TRUE))
  expect_length(scale_messages(ecnsec(h, nsec = 1, resolution = 20,
                                      xform = exp)), 1)
  expect_length(scale_messages(ecnsec(fake_scale_hurdle(nec4param, nec4param),
                                      nsec = 1, resolution = 20)), 0)
  expect_null(getOption("bayesnec.xform_reported"))
})

test_that("summary() reports once where it calls ecx(), and never otherwise", {
  tf <- logged_x_fit(nec4param, "nec4param")
  expect_length(scale_messages(summary(tf)), 0)
  skip_on_cran()
  msgs <- scale_messages(summary(tf, ecx = TRUE))
  expect_length(msgs, 1)
  expect_match(msgs, "Pass xform = exp to ecx()", fixed = TRUE)
  expect_length(scale_messages(summary(logged_x_set(manec_example),
                                       ecx = TRUE, check_fit = FALSE)), 1)
})

test_that("the plotting paths raise no scale message", {
  # They put every estimate on the axis scale themselves, so the message would
  # describe a number the plot does not show.
  skip_on_cran()
  tf <- logged_x_fit(nec4param, "nec4param")
  expect_length(scale_messages(ggbnec_data(tf, add_ecx = TRUE)), 0)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_length(scale_messages(plot(tf, add_ec10 = TRUE)), 0)
})

test_that("a model set's plotting paths raise no scale message (#120)", {
  # The model-average drawing moved into plot_manec_average() and
  # manec_average_plot_data() when model and average were added, and the
  # named equations are drawn through the single-fit paths. Each is asserted,
  # so that a panel drawn outside the gating would be seen. nec() on the same
  # set is the control: the fixture does raise the message.
  skip_on_cran()
  ts <- logged_x_set(manec_example)
  expect_length(scale_messages(nec(ts)), 1)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  both <- c("nec4param", "ecx4param")
  expect_length(scale_messages(plot(ts, add_ec10 = TRUE)), 0)
  expect_length(scale_messages(plot(ts, add_ec10 = TRUE, model = both)), 0)
  expect_length(scale_messages(autoplot(ts, ecx = TRUE)), 0)
  expect_length(scale_messages(autoplot(ts, ecx = TRUE, model = both)), 0)
  expect_length(scale_messages(
    autoplot(ts, ecx = TRUE, model = both, multi_facet = FALSE, ask = FALSE)
  ), 0)
})

test_that("curve_params() names the inverse as the estimators do", {
  tf <- logged_x_fit(nec4param, "nec4param")
  msgs <- scale_messages(curve_params(tf))
  expect_length(msgs, 1)
  expect_match(msgs, "Pass xform = exp to curve_params()", fixed = TRUE)
  expect_match(msgs, "nec and ec50 are on that transformed scale",
               fixed = TRUE)
})
