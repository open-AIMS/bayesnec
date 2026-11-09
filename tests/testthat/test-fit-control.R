test_that("an unobserved control sets prior bounds and needs its own top", {
  d <- data.frame(x = rep(c(0.04, 0.08, 0.12, 0.20, 0.26), each = 3),
                  y = rep(c(212, 195, 170, 115, 65), each = 3))
  f <- y ~ crf(x, "nec4param")
  p <- get_priors(f, data = d, family = Gamma(link = "identity"),
                  x_control = 0)
  expect_equal(as.numeric(p$lb[p$nlpar == "nec"]), 0)
  expect_error(get_priors(f, data = d, family = Gamma(link = "identity"),
                          prior_type = "regularizing", x_control = 0),
               "Supply a prior for `top`")
  expect_no_error(bayesnec:::define_prior(
    "nec4param", Gamma(link = "identity"), d$x, d$y,
    prior_type = "regularizing", x_control = 0,
    supplied_pars = "top"
  ))
  expect_error(bayesnec:::check_fit_x_control(
    0, bnf(f), d$x, x_range = c(0.04, 0.26)
  ), "lower end of `x_range`")
})

test_that("the stored smooth no-effect estimate uses fit-time control", {
  skip_on_cran()
  pre <- bayesnec:::recover_prebayesnecfit(ecx4param)[[1]]
  pre$call_args <- list(x_control = 0.001)
  fit <- suppressMessages(suppressWarnings(bayesnec:::expand_nec(
    pre, pre$bayesnecformula, resolution = 100, sig_val = 0.01,
    model = "ecx4param"
  )))
  fit <- bayesnec:::allot_class(fit, c("bayesnecfit", "bnecfit"))
  expect_equal(min(bnec_newdata(fit, resolution = 100)$x), 0.001)
  expect_equal(bayesnec:::control_x(fit), 0.001)
  expect_equal(as.numeric(fit$ne_posterior),
               as.numeric(nsec(fit, posterior = TRUE, resolution = 100)),
               tolerance = 1e-4)
  expect_equal(nsec(fit, posterior = TRUE, resolution = 100),
               nsec(fit, posterior = TRUE, resolution = 100,
                    x_control = 0.001))
  checked <- suppressWarnings(check_fit(fit, group = 4, ndraws = 20))
  expect_false(any(checked$control))
})

test_that("older objects retain the observed control convention", {
  expect_null(bayesnec:::recorded_x_control(ecx4param))
  expect_equal(bayesnec:::control_x(ecx4param), min(ecx4param$fit$data$x))
  expect_equal(min(bnec_newdata(ecx4param, resolution = 5)$x),
               min(ecx4param$fit$data$x))
})

test_that("bnec accepts an unobserved control with a supplied top prior", {
  d <- data.frame(x = rep(c(0.04, 0.08, 0.12, 0.20, 0.26), each = 3),
                  y = rep(c(212, 195, 170, 115, 65), each = 3))
  f <- y ~ crf(x, "nec4param")
  top <- brms::prior_string("normal(213, 28.5)", nlpar = "top")
  called <- FALSE
  local_mocked_bindings(
    fit_bayesnec = function(..., x_control) {
      called <<- TRUE
      expect_equal(x_control, 0)
      stop("fit reached")
    },
    .package = "bayesnec"
  )
  expect_error(suppressMessages(bnec(
    f, d, family = Gamma(link = "identity"),
    prior_type = "regularizing", x_control = 0
  )), "Supply a prior for `top`")
  expect_false(called)
  expect_error(suppressMessages(bnec(
    f, d, family = Gamma(link = "identity"),
    prior_type = "regularizing", prior = top, x_control = 0
  )), "fit reached")
  expect_true(called)
})

test_that("grouped fits check each level before sampling", {
  d <- data.frame(x = rep(c(0, 0.04, 0.08, 0.12, 0.20), each = 6),
                  y = rep(c(212, 195, 170, 115, 65), each = 6),
                  site = rep(rep(c("a", "b"), each = 3), 5))
  d <- d[!(d$site == "b" & d$x == 0), ]
  reached <- FALSE
  received <- NULL
  local_mocked_bindings(
    bnec = function(...) {
      reached <<- TRUE
      received <<- list(...)
      stop("fit reached")
    },
    .package = "bayesnec"
  )
  expect_error(suppressMessages(bnec_group(
    y ~ crf(x, "nec4param"), d, group_var = "site",
    family = Gamma(link = "identity"),
    prior_type = "regularizing", x_control = 0
  )), "Supply a prior for `top`")
  expect_false(reached)
  top <- brms::prior_string("normal(213, 28.5)", nlpar = "top")
  expect_error(suppressMessages(bnec_group(
    y ~ crf(x, "nec4param"), d, group_var = "site",
    family = Gamma(link = "identity"), prior_type = "regularizing",
    prior = top, x_control = 0
  )), "fit reached")
  expect_equal(received$x_control, 0)
})

test_that("amend and update retain the fit-time control record", {
  pre <- bayesnec:::recover_prebayesnecfit(ecx4param)[[1]]
  pre$call_args <- list(x_control = 0.001, prior_type = "regularizing")
  frame <- model.frame(pre$bayesnecformula, data = pre$fit$data)
  record <- bayesnec:::amend_call_args(list(pre), frame)
  expect_equal(record$x_control, 0.001)
  expect_equal(bayesnec:::update_call_args(pre$call_args,
                                           list(seed = 17))$x_control, 0.001)
  pre$call_args <- NULL
  expect_null(bayesnec:::amend_call_args(list(pre), frame)$x_control)
})

test_that("model sets refuse equations fitted against different controls", {
  a <- nec4param
  b <- ecx4param
  a$call_args <- list(x_control = 0.001)
  b$call_args <- list(x_control = 0.002)
  expect_error(c(a, b), "different control concentrations")
})
