# #297. The curve parameters of a fitted set had no reporting function: for a
# bayesmanecfit nothing returned them at all. Everything here runs on the
# packaged manec_example or on a mock, because the quantity is read straight
# off the stored draws and nothing about it needs sampling to exercise.

nec4 <- suppressMessages(suppressWarnings(
  pull_out(manec_example, "nec4param")
))

test_that("the table has the documented columns and one row per parameter", {
  out <- parameters(manec_example)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("model", "wi", "dpar", "parameter", "Estimate", "Q2.5",
                      "Q97.5"))
  # nec4param estimates top, bot, beta and nec; ecx4param top, bot, beta and
  # ec50. Eight rows, and every row names an equation that was fitted.
  expect_equal(nrow(out), 8L)
  expect_true(all(out$model %in% manec_example$success_models))
  expect_equal(sort(out$parameter[out$model == "nec4param"]),
               c("beta", "bot", "nec", "top"))
  expect_equal(sort(out$parameter[out$model == "ecx4param"]),
               c("beta", "bot", "ec50", "top"))
})

test_that("the estimates are the fit's own, not a recomputation of them", {
  # The same median and equal-tailed interval fixef(robust = TRUE) reports, and
  # the same nec() returns. A table that disagreed with either would be a
  # second set of numbers for one quantity.
  out <- parameters(nec4)
  fef <- brms::fixef(nec4$fit, robust = TRUE)
  for (p in c("top", "bot", "beta", "nec")) {
    row <- out[out$parameter == p, ]
    expect_equal(row$Estimate, unname(fef[paste0(p, "_Intercept"), "Estimate"]))
    expect_equal(row$Q2.5, unname(fef[paste0(p, "_Intercept"), "Q2.5"]))
    expect_equal(row$Q97.5, unname(fef[paste0(p, "_Intercept"), "Q97.5"]))
  }
  expect_equal(out$Estimate[out$parameter == "nec"], unname(nec(nec4)[1]))
})

test_that("the weight is reported beside each equation and orders the table", {
  out <- parameters(manec_example)
  wi <- manec_example$mod_stats[manec_example$success_models, "wi"]
  names(wi) <- manec_example$success_models
  expect_equal(out$wi, unname(as.numeric(wi[out$model])))
  # Decreasing, so the equation holding most of the model average is read
  # first.
  expect_false(is.unsorted(rev(out$wi)))
  # The weights are the set's own and are not renormalised over the equations
  # sharing a parameter: nothing here is averaged, so nothing rescales them.
  expect_equal(sum(unique(out$wi)), 1, tolerance = 1e-8)
})

test_that("a single fit is reported at a weight of 1", {
  out <- parameters(nec4)
  expect_true(all(out$wi == 1))
  expect_true(all(out$model == "nec4param"))
})

test_that("summary = FALSE returns the draws the summary was computed from", {
  draws <- parameters(manec_example, summary = FALSE)
  expect_named(draws, c("nec4param", "ecx4param"))
  expect_equal(ncol(draws$nec4param), 4L)
  expect_equal(nrow(draws$nec4param), manec_example$sample_size)
  out <- parameters(manec_example)
  for (p in colnames(draws$nec4param)) {
    row <- out[out$model == "nec4param" & out$parameter == p, ]
    expect_equal(row$Estimate, unname(median(draws$nec4param[, p])))
    expect_equal(row$Q97.5,
                 unname(quantile(draws$nec4param[, p], 0.975)))
  }
})

test_that("xform applies to the predictor-scale parameters and to no other", {
  plain <- parameters(manec_example)
  sq <- parameters(manec_example, xform = function(x) x^2)
  on_x <- sq$parameter %in% c("nec", "ec50")
  expect_false(any(plain$Estimate[on_x] == sq$Estimate[on_x]))
  expect_equal(plain$Estimate[!on_x], sq$Estimate[!on_x])
  # Applied per draw rather than to the summary. For a monotone xform the
  # median agrees either way, so the interval is what separates them: here it
  # happens to agree as well, and what is asserted is that the transformed
  # draws are the source of both.
  draws <- parameters(manec_example, summary = FALSE,
                      xform = function(x) x^2)
  expect_equal(sq$Estimate[sq$model == "nec4param" & sq$parameter == "nec"],
               unname(median(draws$nec4param[, "nec"])))
})

test_that("xform and summary are validated", {
  expect_error(parameters(nec4, xform = "sqrt"), "xform must be a function")
  expect_error(parameters(nec4, summary = "yes"), "summary")
  expect_error(parameters(manec_example, xform = 2), "xform must be a function")
})

test_that("every parameter of every equation is one this function reports", {
  # A guard on curve_par_names(), which is fixed rather than derived. An
  # equation added with a parameter not in that vector would be reported with
  # the parameter silently missing from its rows, which is the failure mode
  # this whole function exists to remove.
  pars <- unique(unlist(lapply(models()$all, function(m) {
    all.vars(show_params(m)[[1]]$formula)
  })))
  pars <- setdiff(pars, c("x", "y"))
  expect_true(all(pars %in% bayesnec:::curve_par_names()))
})

test_that("a joint two-block fit reports both blocks", {
  # A hurdle_gamma fit from bnec() holds a second set of curve parameters named
  # b_hu<par>_Intercept. Exercised through the draws rather than by fitting
  # one: the plumbing under test is which names are read and how they are
  # labelled, and that is deterministic.
  fake <- list(family = brms::hurdle_gamma(link = "identity",
                                           link_hu = "identity"))
  local_mocked_bindings(
    curve_param_draws = function(fit, prefix = "") {
      if (identical(prefix, "")) {
        cbind(top = c(1, 2, 3, 4), nec = c(5, 6, 7, 8))
      } else {
        cbind(top = c(11, 12, 13, 14))
      }
    }
  )
  out <- bayesnec:::one_fit_parameters(fake, "nec3param", 1, TRUE, identity)
  expect_equal(out$dpar, c("mu", "mu", "hu"))
  expect_equal(out$parameter, c("top", "nec", "top"))
  expect_equal(out$Estimate, c(2.5, 6.5, 12.5))
  # The draws form names the second block's columns so that two blocks in one
  # matrix stay distinguishable.
  dr <- bayesnec:::one_fit_parameters(fake, "nec3param", 1, FALSE, identity)
  expect_equal(colnames(dr$nec3param), c("top", "nec", "hu_top"))
})

test_that("a link the caller supplied is reported, and identity is silent", {
  expect_silent(parameters(nec4))
  fake <- list(family = brms::hurdle_gamma(link = "log", link_hu = "logit"))
  expect_message(bayesnec:::report_link(bayesnec:::fit_links(fake), "x fit"),
                 "link scale")
  expect_message(bayesnec:::report_link(bayesnec:::fit_links(fake), "x fit"),
                 "mu: link = \"log\"")
  expect_silent(bayesnec:::report_link(c(mu = "identity"), "x fit"))
})

test_that("a hurdle fit returns one table per component", {
  o <- structure(list(growth = nec4, survival = nec4,
                      data = manec_example$mod_fits[[1]]$fit$data,
                      formula = manec_example$bayesnecformula,
                      y_var = "y", n_exposed = 4L, n_dead = 2L),
                 class = c("bayesnechurdlefit", "bnecfit"))
  expect_message(out <- parameters(o), "one element per component")
  expect_named(out, c("growth", "survival"))
  expect_equal(out$growth, out$survival)
  expect_equal(nrow(out$growth), 4L)
})

test_that("a group fit returns one table with a level column", {
  gf <- structure(list(fits = list(a = manec_example, b = nec4),
                       group_var = "site", levels = c("a", "b"),
                       formula = manec_example$bayesnecformula,
                       data = manec_example$mod_fits[[1]]$fit$data,
                       family = manec_example$mod_fits[[1]]$fit$family,
                       n = c(50L, 50L)),
                  class = c("bayesnecgroupfit", "bnecfit"))
  out <- parameters(gf)
  expect_equal(names(out)[1], "level")
  expect_equal(unique(out$level), c("a", "b"))
  expect_equal(nrow(out), 12L)
  # The draws form keeps the levels apart rather than binding them.
  dr <- parameters(gf, summary = FALSE)
  expect_named(dr, c("a", "b"))
  expect_named(dr$b, "nec4param")
})
