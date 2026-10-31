# Read parameters from the packaged fits or mocks. These reporting tests
# need stored draws or formula metadata, not new samples.

# Build a joint formula without sampling to check survival-equation labels.
# Embed equation names in the formula text so the mock does not depend on
# resolving a model variable from its formula environment.
joint_mock <- function(model = "nec3param", model_survival = "ecx4param",
                       x_term = "x") {
  d <- data.frame(x = c(1, 2, 3, 4, 1, 2, 3, 4),
                  y = c(2, 1, 0, 0, 3, 1, 0.5, 0))
  f <- bnf(stats::as.formula(
    paste0("y ~ crf(", x_term, ", model = \"", model, "\")")
  ))
  bdat <- model.frame(f, data = d, run_par_checks = TRUE)
  fam <- brms::hurdle_gamma(link = "identity", link_hu = "identity")
  bf <- bayesnec:::wrangle_model_formula(model, f, bdat, fam,
                                         model_survival = model_survival)
  structure(list(fit = list(formula = bf, family = fam, data = d),
                 model = model, bayesnecformula = f),
            class = c("bayesnecfit", "bnecfit"))
}

test_that("the table has the documented columns and one row per parameter", {
  out <- curve_params(manec_example)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("model", "wi", "dpar", "link", "parameter", "Estimate",
                      "Q2.5", "Q97.5"))
  # nec4param estimates top, bot, beta and nec; ecx4param top, bot, beta and
  # ec50. Eight rows, and every row names an equation that was fitted.
  expect_equal(nrow(out), 8L)
  expect_true(all(out$model %in% manec_example$success_models))
  expect_equal(sort(out$parameter[out$model == "nec4param"]),
               c("beta", "bot", "nec", "top"))
  expect_equal(sort(out$parameter[out$model == "ecx4param"]),
               c("beta", "bot", "ec50", "top"))
})

test_that("the rows are in the documented parameter order", {
  # Use the documented parameter order, independently of fixef()'s column
  # order or the order of terms in a formula.
  out <- curve_params(nec4param)
  expect_equal(out$parameter, c("top", "bot", "beta", "nec"))
  expect_equal(curve_params(ecx4param)$parameter,
               c("top", "bot", "beta", "ec50"))
})

test_that("the estimates are the fit's own, not a recomputation of them", {
  # The same median and equal-tailed interval fixef(robust = TRUE) reports, and
  # the same nec() returns. A table that disagreed with either would be a
  # second set of numbers for one quantity.
  out <- curve_params(nec4param)
  fef <- brms::fixef(nec4param$fit, robust = TRUE)
  for (p in c("top", "bot", "beta", "nec")) {
    row <- out[out$parameter == p, ]
    expect_equal(row$Estimate, unname(fef[paste0(p, "_Intercept"), "Estimate"]))
    expect_equal(row$Q2.5, unname(fef[paste0(p, "_Intercept"), "Q2.5"]))
    expect_equal(row$Q97.5, unname(fef[paste0(p, "_Intercept"), "Q97.5"]))
  }
  expect_equal(out$Estimate[out$parameter == "nec"],
               unname(nec(nec4param)[1]))
})

test_that("the weight is reported beside each equation and orders the table", {
  out <- curve_params(manec_example)
  wi <- manec_example$mod_stats$wi
  names(wi) <- manec_example$mod_stats$model
  expect_equal(out$wi, unname(as.numeric(wi[out$model])))
  # Decreasing, so the equation holding most of the model average is read
  # first.
  expect_false(is.unsorted(rev(out$wi)))
  # The weights are the set's own and are not renormalised over the equations
  # sharing a parameter: nothing here is averaged, so nothing rescales them.
  expect_equal(sum(unique(out$wi)), 1, tolerance = 1e-8)
})

test_that("the weight is matched on the model column, not on row names", {
  # The row names of mod_stats arrive incidentally, from the dispersion matrix
  # expand_manec() binds. Matching on them returned NA for every weight if they
  # were ever absent, with nothing said.
  x <- manec_example
  rownames(x$mod_stats) <- NULL
  out <- curve_params(x)
  expect_false(anyNA(out$wi))
  expect_equal(out$wi, curve_params(manec_example)$wi)
  # And a set whose table holds no row for a success model is an error rather
  # than a silent NA.
  y <- manec_example
  y$mod_stats$model[1] <- "not_a_model"
  rownames(y$mod_stats) <- NULL
  expect_error(curve_params(y), "holds no row for")
})

test_that("a single fit is reported at a weight of 1", {
  out <- curve_params(nec4param)
  expect_true(all(out$wi == 1))
  expect_true(all(out$model == "nec4param"))
})

test_that("summary = FALSE returns the draws the summary was computed from", {
  draws <- curve_params(manec_example, summary = FALSE)
  expect_named(draws, c("nec4param", "ecx4param"))
  expect_equal(ncol(draws$nec4param), 4L)
  expect_equal(nrow(draws$nec4param), manec_example$sample_size)
  # A matrix of draws has no column for the link, so the list records it.
  expect_equal(attr(draws, "link"), c(mu = "identity"))
  expect_equal(attr(curve_params(nec4param, summary = FALSE), "link"),
               c(mu = "identity"))
  out <- curve_params(manec_example)
  for (p in colnames(draws$nec4param)) {
    row <- out[out$model == "nec4param" & out$parameter == p, ]
    expect_equal(row$Estimate, unname(median(draws$nec4param[, p])))
    expect_equal(row$Q97.5,
                 unname(quantile(draws$nec4param[, p], 0.975)))
  }
})

test_that("xform applies to the predictor-scale parameters and to no other", {
  plain <- curve_params(manec_example)
  sq <- curve_params(manec_example, xform = function(x) x^2)
  on_x <- sq$parameter %in% c("nec", "ec50")
  expect_false(any(plain$Estimate[on_x] == sq$Estimate[on_x]))
  expect_equal(plain$Estimate[!on_x], sq$Estimate[!on_x])
})

test_that("xform is applied to the draws rather than to the summary", {
  # Transform draws before summarising them. Nonlinear transforms need not
  # commute with interpolated sample quantiles, including the median of an
  # even sample; the interval and folded transform exercise that distinction.
  sq <- curve_params(nec4param, xform = function(x) x^2)
  drawn <- curve_params(nec4param, summary = FALSE,
                      xform = function(x) x^2)$nec4param[, "nec"]
  wanted <- unname(quantile(drawn, 0.975))
  expect_equal(sq$Q97.5[sq$parameter == "nec"], wanted)
  plain <- curve_params(nec4param)
  expect_false(isTRUE(all.equal(wanted,
                                plain$Q97.5[plain$parameter == "nec"]^2)))
  # A non-monotone xform: the median of |nec - 1.45| is not |median - 1.45|.
  fold <- curve_params(nec4param, xform = function(x) abs(x - 1.45))
  raw <- curve_params(nec4param, summary = FALSE)$nec4param[, "nec"]
  expect_equal(fold$Estimate[fold$parameter == "nec"],
               unname(median(abs(raw - 1.45))))
})

test_that("xform and summary are validated", {
  expect_error(curve_params(nec4param, xform = "sqrt"),
               "xform must be a function")
  expect_error(curve_params(nec4param, summary = "yes"), "summary")
  expect_error(curve_params(manec_example, xform = 2),
               "xform must be a function")
})

test_that("the reported parameters are every parameter of every equation", {
  # Check the fixed parameter-name list against every equation so a new
  # parameter cannot disappear silently from the report.
  pars <- unique(unlist(lapply(models()$all, function(m) {
    all.vars(show_params(m)[[1]]$formula)
  })))
  pars <- setdiff(pars, c("x", "y"))
  expect_setequal(pars, bayesnec:::curve_par_names())
  # Keep reporting and fit extraction consistent even though their parameter
  # orders differ.
  expect_setequal(bayesnec:::extract_par_order(), bayesnec:::curve_par_names())
  expect_setequal(
    bayesnec:::equation_par_names("ecxll5"),
    c("top", "bot", "beta", "ec50", "f")
  )
  expect_equal(bayesnec:::equation_par_names(NA_character_), character(0))
})

test_that("only the mu block's columns are read for the mu block", {
  # Anchored, full-name matching. Prefix matching makes "top" also match
  # "hutop", which is the defect extract_pars() records: a hurdle fit then
  # reports the survival block's numbers as the response block's.
  draws <- data.frame(b_top_Intercept = c(1, 2), b_hutop_Intercept = c(9, 9),
                      b_nec_Intercept = c(3, 4), b_hunec_Intercept = c(8, 8),
                      sigma = c(1, 1), prior_b_top = c(0, 0))
  mu <- bayesnec:::select_curve_cols(draws, "")
  expect_equal(colnames(mu), c("top", "nec"))
  expect_equal(unname(mu[, "top"]), c(1, 2))
  hu <- bayesnec:::select_curve_cols(draws, "hu")
  expect_equal(colnames(hu), c("top", "nec"))
  expect_equal(unname(hu[, "top"]), c(9, 9))
  # Nothing that is not a curve parameter is picked up.
  expect_equal(ncol(bayesnec:::select_curve_cols(
    data.frame(sigma = 1, sd_g__bot_Intercept = 1, b_ogl_Intercept = 1), ""
  )), 0L)
})

test_that("a two-block fit names each block's own equation", {
  # bnec(model_survival = ) and bnec_joint() both pair a response equation with
  # a different survival one. Stamping the response block's name on both rows
  # reported an ec50 for nec3param, which does not estimate one.
  o <- joint_mock("nec3param", "ecx4param")
  expect_equal(bayesnec:::dpar_models(o),
               c(mu = "nec3param", hu = "ecx4param"))
  expect_equal(bayesnec:::dpar_models(joint_mock("nec3param", "nec3param")),
               c(mu = "nec3param", hu = "nec3param"))
  # The identification survives an inline transformation of the predictor,
  # which is substituted into the block's expression when it is built.
  expect_equal(bayesnec:::dpar_models(
    joint_mock("nec3param", "ecxexp", x_term = "log(x)")
  ), c(mu = "nec3param", hu = "ecxexp"))
  # An expression matching no equation is NA rather than the response block's
  # name, so a row is labelled only where the equation has been identified.
  bad <- joint_mock()
  bad$fit$formula$pforms$hu[[3]] <- str2lang("1 - (hutop * x)")
  expect_true(is.na(bayesnec:::dpar_models(bad)[["hu"]]))
})

test_that("a two-block fit reports both blocks, each under its own name", {
  o <- joint_mock("nec3param", "ecx4param")
  # Each block gets exactly the parameters its own equation estimates, in the
  # order curve_par_names() gives, which is what select_curve_cols() returns.
  local_mocked_bindings(
    curve_param_draws = function(fit, prefix = "") {
      if (identical(prefix, "")) {
        cbind(top = c(1, 2, 3, 4), beta = c(0, 1, 2, 3), nec = c(5, 6, 7, 8))
      } else {
        cbind(top = c(11, 12, 13, 14), bot = c(0, 0, 1, 1),
              beta = c(2, 2, 3, 3), ec50 = c(1, 2, 3, 4))
      }
    }
  )
  out <- expect_silent(
    bayesnec:::one_fit_params(o$fit, bayesnec:::dpar_models(o), 1, TRUE,
                                  identity)
  )
  expect_equal(out$dpar, c(rep("mu", 3), rep("hu", 4)))
  expect_equal(out$model, c(rep("nec3param", 3), rep("ecx4param", 4)))
  expect_equal(out$parameter,
               c("top", "beta", "nec", "top", "bot", "beta", "ec50"))
  expect_equal(out$Estimate[out$dpar == "mu"], c(2.5, 1.5, 6.5))
  expect_equal(out$Estimate[out$dpar == "hu"], c(12.5, 0.5, 2.5, 2.5))
  expect_true(all(out$link == "identity"))
  # The draws form names the second block's columns so that two blocks in one
  # matrix stay distinguishable, and is named for the response equation.
  dr <- bayesnec:::one_fit_params(o$fit, bayesnec:::dpar_models(o), 1,
                                      FALSE, identity)
  expect_named(dr, "nec3param")
  expect_equal(colnames(dr$nec3param),
               c("top", "beta", "nec", "hu_top", "hu_bot", "hu_beta",
                 "hu_ec50"))
})

test_that("a parameter the posterior does not hold is reported, not dropped", {
  # No route through bnec() is known to produce a fit missing one, so this is a
  # guard: the absence is stated rather than left to be noticed in a table that
  # looks complete.
  local_mocked_bindings(
    curve_param_draws = function(fit, prefix = "") cbind(top = c(1, 2))
  )
  fake <- list(family = stats::gaussian(link = "identity"))
  expect_message(
    out <- bayesnec:::one_fit_params(fake, c(mu = "nec4param"), 1, TRUE,
                                         identity),
    "bot, beta, nec"
  )
  expect_equal(out$parameter, "top")
})

test_that("the link is reported in a column and named when it is not identity", {
  out <- curve_params(manec_example)
  expect_true(all(out$link == "identity"))
  expect_silent(curve_params(nec4param))
  fake <- list(family = brms::hurdle_gamma(link = "log", link_hu = "logit"))
  expect_equal(unname(bayesnec:::fit_links(fake)), c("log", "logit"))
  expect_message(bayesnec:::report_link(bayesnec:::fit_links(fake), "x fit"),
                 "link scale")
  expect_message(bayesnec:::report_link(bayesnec:::fit_links(fake), "x fit"),
                 "mu: link = \"log\"")
  expect_silent(bayesnec:::report_link(c(mu = "identity"), "x fit"))
})

# Relabel the stored predictor formula without refitting. Compare calls on
# one posterior; its numbers do not validate a newly sampled fit.
transformed_x_fit <- function(fit, model) {
  fit$bayesnecformula <- bnf(stats::as.formula(
    paste0("y ~ crf(sqrt(x), model = \"", model, "\")")
  ))
  fit
}

test_that("nec and ec50 default to the recorded scale on a transformed fit", {
  tf <- transformed_x_fit(nec4param, "nec4param")
  # The default inverts sqrt() exactly, so it matches the inverse supplied by
  # hand, and says nothing.
  expect_silent(by_default <- curve_params(tf))
  expect_equal(by_default, curve_params(tf, xform = function(x) x^2))
  # xform = identity is the fitted scale, which is what the parameters were
  # sampled on and what an untransformed fit reports.
  fitted <- curve_params(tf, xform = identity)
  expect_identical(fitted, curve_params(nec4param))
  on_x <- fitted$parameter %in% c("nec", "ec50")
  expect_true(any(on_x))
  expect_false(isTRUE(all.equal(by_default[on_x, ], fitted[on_x, ])))
  expect_identical(by_default[!on_x, ], fitted[!on_x, ])
  # The draws form takes the same map.
  expect_equal(curve_params(tf, summary = FALSE)$nec4param[, "nec"],
               curve_params(nec4param, summary = FALSE)$nec4param[, "nec"]^2)
})

fake_group <- function(fits = list(a = manec_example, b = nec4param)) {
  structure(list(fits = fits, group_var = "site", levels = names(fits),
                 formula = nec4param$bayesnecformula,
                 data = nec4param$fit$data,
                 family = nec4param$fit$family,
                 n = rep(50L, length(fits))),
            class = c("bayesnecgroupfit", "bnecfit"))
}

fake_hurdle <- function(growth = nec4param, survival = nec4param) {
  structure(list(growth = growth, survival = survival,
                 data = nec4param$fit$data,
                 formula = nec4param$bayesnecformula,
                 y_var = "y", n_exposed = 4L, n_dead = 2L),
            class = c("bayesnechurdlefit", "bnecfit"))
}

test_that("a hurdle fit returns one table per component", {
  o <- fake_hurdle()
  expect_message(out <- curve_params(o), "one element per component")
  expect_named(out, c("growth", "survival"))
  expect_equal(out$growth, out$survival)
  expect_equal(nrow(out$growth), 4L)
})

test_that("a hurdle pair reports the link per component", {
  # The link is not shared: growth takes the family of the non-zero response
  # and survival is bernoulli, so each component reports its own, and each
  # paragraph names its component.
  g <- nec4param
  g$fit$family <- brms::brmsfamily("gaussian", link = "log")
  s <- nec4param
  s$fit$family <- brms::brmsfamily("bernoulli", link = "logit")
  msgs <- capture_messages(curve_params(fake_hurdle(g, s)))
  expect_equal(sum(grepl("link scale", msgs)), 2L)
  expect_true(any(grepl("growth component was made with mu: link = \"log\"",
                        msgs)))
  expect_true(any(grepl("survival component was made with mu: link = ",
                        msgs)))
})

test_that("the wrapper methods validate before they print", {
  # Both delegate, so without a check of their own a bad argument surfaced
  # after the delegation notice, or from inside a per-level call.
  expect_error(curve_params(fake_hurdle(), xform = "sqrt"),
               "xform must be a function")
  expect_error(curve_params(fake_hurdle(), summary = "yes"), "summary")
  expect_error(curve_params(fake_group(), summary = "yes"), "summary")
  expect_error(curve_params(fake_group(), xform = 2), "xform must be a function")
})

test_that("a group fit returns one table with a level column", {
  out <- curve_params(fake_group())
  expect_equal(names(out)[1], "level")
  expect_equal(unique(out$level), c("a", "b"))
  expect_equal(nrow(out), 12L)
  # The draws form keeps the levels apart rather than binding them.
  dr <- curve_params(fake_group(), summary = FALSE)
  expect_named(dr, c("a", "b"))
  expect_named(dr$b, "nec4param")
})

test_that("each level of a group takes the recorded scale", {
  tf <- transformed_x_fit(nec4param, "nec4param")
  gf <- fake_group(list(a = tf, b = tf, c = tf))
  expect_silent(out <- curve_params(gf))
  expect_equal(out, curve_params(gf, xform = function(x) x^2))
})
