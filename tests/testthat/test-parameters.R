# #297. The curve parameters of a fitted set had no reporting function: for a
# bayesmanecfit nothing returned them at all. Everything here runs on the
# packaged manec_example or on a mock, because the quantity is read straight
# off the stored draws and nothing about it needs sampling to exercise.
# nec4param and ecx4param are the fits setup.R pulls out of manec_example.

# A joint two-block fit, built as far as the brms formula and no further. The
# formula is what identifies the survival block's equation, and
# wrangle_model_formula() builds it deterministically, so the labelling can be
# pinned without sampling. The model name is pasted into the formula text
# rather than referenced, because crf() evaluates its model argument where the
# formula is used rather than where it is written.
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
  out <- parameters(manec_example)
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
  # Promised by the Rd, and not implied by anything brms returns: fixef() lists
  # nec4param as bot, top, beta, nec. A table whose row order moved with the
  # formula would read differently from one fit to the next.
  out <- parameters(nec4param)
  expect_equal(out$parameter, c("top", "bot", "beta", "nec"))
  expect_equal(parameters(ecx4param)$parameter,
               c("top", "bot", "beta", "ec50"))
})

test_that("the estimates are the fit's own, not a recomputation of them", {
  # The same median and equal-tailed interval fixef(robust = TRUE) reports, and
  # the same nec() returns. A table that disagreed with either would be a
  # second set of numbers for one quantity.
  out <- parameters(nec4param)
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
  out <- parameters(manec_example)
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
  out <- parameters(x)
  expect_false(anyNA(out$wi))
  expect_equal(out$wi, parameters(manec_example)$wi)
  # And a set whose table holds no row for a success model is an error rather
  # than a silent NA.
  y <- manec_example
  y$mod_stats$model[1] <- "not_a_model"
  rownames(y$mod_stats) <- NULL
  expect_error(parameters(y), "holds no row for")
})

test_that("a single fit is reported at a weight of 1", {
  out <- parameters(nec4param)
  expect_true(all(out$wi == 1))
  expect_true(all(out$model == "nec4param"))
})

test_that("summary = FALSE returns the draws the summary was computed from", {
  draws <- parameters(manec_example, summary = FALSE)
  expect_named(draws, c("nec4param", "ecx4param"))
  expect_equal(ncol(draws$nec4param), 4L)
  expect_equal(nrow(draws$nec4param), manec_example$sample_size)
  # A matrix of draws has no column for the link, so the list records it.
  expect_equal(attr(draws, "link"), c(mu = "identity"))
  expect_equal(attr(parameters(nec4param, summary = FALSE), "link"),
               c(mu = "identity"))
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
})

test_that("xform is applied to the draws rather than to the summary", {
  # The distinction the median cannot pin: for a monotone xform the median of
  # the transformed draws equals the transform of the median exactly. The
  # interval separates them, because quantile() interpolates between order
  # statistics, and a non-monotone xform separates them outright.
  sq <- parameters(nec4param, xform = function(x) x^2)
  drawn <- parameters(nec4param, summary = FALSE,
                      xform = function(x) x^2)$nec4param[, "nec"]
  wanted <- unname(quantile(drawn, 0.975))
  expect_equal(sq$Q97.5[sq$parameter == "nec"], wanted)
  plain <- parameters(nec4param)
  expect_false(isTRUE(all.equal(wanted,
                                plain$Q97.5[plain$parameter == "nec"]^2)))
  # A non-monotone xform: the median of |nec - 1.45| is not |median - 1.45|.
  fold <- parameters(nec4param, xform = function(x) abs(x - 1.45))
  raw <- parameters(nec4param, summary = FALSE)$nec4param[, "nec"]
  expect_equal(fold$Estimate[fold$parameter == "nec"],
               unname(median(abs(raw - 1.45))))
})

test_that("xform and summary are validated", {
  expect_error(parameters(nec4param, xform = "sqrt"),
               "xform must be a function")
  expect_error(parameters(nec4param, summary = "yes"), "summary")
  expect_error(parameters(manec_example, xform = 2),
               "xform must be a function")
})

test_that("the reported parameters are every parameter of every equation", {
  # A guard on curve_par_names(), which is fixed rather than derived. An
  # equation added with a parameter not in that vector would be reported with
  # the parameter silently missing from its rows, which is the failure mode
  # this whole function exists to remove.
  pars <- unique(unlist(lapply(models()$all, function(m) {
    all.vars(show_params(m)[[1]]$formula)
  })))
  pars <- setdiff(pars, c("x", "y"))
  expect_setequal(pars, bayesnec:::curve_par_names())
  # expand_nec() extracts the same set in a different order, which it keeps
  # because the extracted elements are appended to the bayesnecfit in it. The
  # two are asserted to agree here, so a parameter added for a new equation
  # cannot reach one and not the other.
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
    bayesnec:::one_fit_parameters(o$fit, bayesnec:::dpar_models(o), 1, TRUE,
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
  dr <- bayesnec:::one_fit_parameters(o$fit, bayesnec:::dpar_models(o), 1,
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
    out <- bayesnec:::one_fit_parameters(fake, c(mu = "nec4param"), 1, TRUE,
                                         identity),
    "bot, beta, nec"
  )
  expect_equal(out$parameter, "top")
})

test_that("the link is reported in a column and named when it is not identity", {
  out <- parameters(manec_example)
  expect_true(all(out$link == "identity"))
  expect_silent(parameters(nec4param))
  fake <- list(family = brms::hurdle_gamma(link = "log", link_hu = "logit"))
  expect_equal(unname(bayesnec:::fit_links(fake)), c("log", "logit"))
  expect_message(bayesnec:::report_link(bayesnec:::fit_links(fake), "x fit"),
                 "link scale")
  expect_message(bayesnec:::report_link(bayesnec:::fit_links(fake), "x fit"),
                 "mu: link = \"log\"")
  expect_silent(bayesnec:::report_link(c(mu = "identity"), "x fit"))
})

# A fit whose formula transforms the PREDICTOR, in the style of setup.R's
# transformed_response_fit(). The stored fit was not fitted with this formula;
# that is safe for report_scales(), which reads the formula only to decide
# whether the predictor was transformed, and it is not safe for reading any
# posterior quantity off the fit. sqrt() rather than log(), because
# manec_example's predictor reaches zero.
transformed_x_fit <- function(fit, model) {
  fit$bayesnecformula <- bnf(stats::as.formula(
    paste0("y ~ crf(sqrt(x), model = \"", model, "\")")
  ))
  fit
}

test_that("a predictor transformed inline is reported unless xform was given", {
  tf <- transformed_x_fit(nec4param, "nec4param")
  expect_message(parameters(tf), "nec and ec50 are on that transformed scale")
  expect_message(parameters(tf), "sqrt\\(x\\)")
  # A caller who supplied an inverse has already dealt with it.
  expect_silent(parameters(tf, xform = function(x) x^2))
  expect_silent(parameters(nec4param))
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
  expect_message(out <- parameters(o), "one element per component")
  expect_named(out, c("growth", "survival"))
  expect_equal(out$growth, out$survival)
  expect_equal(nrow(out$growth), 4L)
})

test_that("a hurdle pair reports the transformation once, the link per fit", {
  # The two components are fitted from one formula on one predictor, so a
  # second paragraph about the transformation says nothing the first did not.
  # The link is not shared: growth takes the family of the non-zero response
  # and survival is bernoulli, so each component reports its own.
  tf <- transformed_x_fit(nec4param, "nec4param")
  msgs <- capture_messages(parameters(fake_hurdle(tf, tf)))
  expect_equal(sum(grepl("transformed scale", msgs)), 1L)
  # The gate is restored, so the next call reports again.
  expect_message(parameters(tf), "transformed scale")
  # The half the split exists to protect: two components on different links
  # both report, and each paragraph names its component. Gating the link for
  # the pair, as the transformation is gated, would suppress one of them and
  # the suite would not notice.
  g <- nec4param
  g$fit$family <- brms::brmsfamily("gaussian", link = "log")
  s <- nec4param
  s$fit$family <- brms::brmsfamily("bernoulli", link = "logit")
  msgs <- capture_messages(parameters(fake_hurdle(g, s)))
  expect_equal(sum(grepl("link scale", msgs)), 2L)
  expect_true(any(grepl("growth component was made with mu: link = \"log\"",
                        msgs)))
  expect_true(any(grepl("survival component was made with mu: link = ",
                        msgs)))
})

test_that("the wrapper methods validate before they print", {
  # Both delegate, so without a check of their own a bad argument surfaced
  # after the delegation notice, or from inside a per-level call.
  expect_error(parameters(fake_hurdle(), xform = "sqrt"),
               "xform must be a function")
  expect_error(parameters(fake_hurdle(), summary = "yes"), "summary")
  expect_error(parameters(fake_group(), summary = "yes"), "summary")
  expect_error(parameters(fake_group(), xform = 2), "xform must be a function")
})

test_that("a group fit returns one table with a level column", {
  out <- parameters(fake_group())
  expect_equal(names(out)[1], "level")
  expect_equal(unique(out$level), c("a", "b"))
  expect_equal(nrow(out), 12L)
  # The draws form keeps the levels apart rather than binding them.
  dr <- parameters(fake_group(), summary = FALSE)
  expect_named(dr, c("a", "b"))
  expect_named(dr$b, "nec4param")
})

test_that("the scale is reported once for a group, not once per level", {
  # Every level is fitted with the same family and the same formula, so a
  # per-level report prints the same paragraph once per level: twelve times on
  # a twelve-level fit.
  tf <- transformed_x_fit(nec4param, "nec4param")
  gf <- fake_group(list(a = tf, b = tf, c = tf))
  msgs <- capture_messages(parameters(gf))
  expect_equal(sum(grepl("transformed scale", msgs)), 1L)
  # And the option it is gated on is restored, so the next call reports again.
  expect_message(parameters(tf), "transformed scale")
})
