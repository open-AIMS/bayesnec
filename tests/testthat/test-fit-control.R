skip_unless_full_suite()

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

test_that("model sets accept equal controls despite different model names", {
  a <- nec4param
  b <- ecx4param
  a$call_args <- list(x_control = 0.001)
  b$call_args <- list(x_control = 0.001)
  combined <- suppressWarnings(suppressMessages(c(a, b)))
  expect_s3_class(combined, "bayesmanecfit")
  expect_equal(bayesnec:::control_x(combined), 0.001)
  # Objects saved before x_control existed use their common observed minimum.
  legacy <- suppressWarnings(suppressMessages(c(nec4param, ecx4param)))
  expect_s3_class(legacy, "bayesmanecfit")
  expect_equal(bayesnec:::control_x(legacy), min(nec4param$fit$data$x))
})

# nec_data$x spans 0.032 to 3.22, which is -3.43 to 1.17 on the log scale. A
# stated control is on the recorded scale, the model frame on the fitted one.
test_that("pre-fit control checks compare on the fitted scale", {
  f <- y ~ crf(log(x), "nec3param")
  p <- get_priors(f, data = nec_data, family = Beta(), x_control = 2)
  expect_equal(as.numeric(p$lb[p$nlpar == "nec"]), log(2))
  expect_error(get_priors(f, data = nec_data, family = Beta(), x_control = 4),
               "below the highest observed concentration")
  expect_error(get_priors(f, data = nec_data, family = Beta(),
                          x_control = -1),
               "not finite under the crf\\(\\) predictor transformation")
  expect_no_error(get_priors(f, data = nec_data, family = Beta(),
                             x_control = min(nec_data$x),
                             prior_type = "regularizing"))
  expect_error(get_priors(f, data = nec_data, family = Beta(),
                          x_control = 0.01, prior_type = "regularizing"),
               "Supply a prior for `top`")
  expect_error(bayesnec:::define_prior(
    "nec3param", Beta(), log(nec_data$x), nec_data$y,
    prior_type = "regularizing", x_control = 0.01,
    x_control_fitted = log(0.01)
  ), "needs an observed response")
  expect_no_error(bayesnec:::define_prior(
    "nec3param", Beta(), log(nec_data$x), nec_data$y,
    prior_type = "regularizing", x_control = min(nec_data$x),
    x_control_fitted = log(min(nec_data$x))
  ))
  frame <- model.frame(bnf(f), data = nec_data)
  expect_no_error(bayesnec:::check_control_top_prior(
    frame, Beta(), "nec3param", NULL, "regularizing", min(nec_data$x),
    bnf(f)
  ))
})

log_control_data <- function() {
  set.seed(5)
  x <- rep(c(0.5, 1, 2, 4, 8, 16), each = 4)
  data.frame(x = x, y = stats::rgamma(length(x), shape = 50,
                                      rate = 50 / (100 * exp(-0.2 * x))))
}

test_that("bnec reports a logged control on the fitted scale", {
  d <- log_control_data()
  f <- y ~ crf(log(x), "nec4param")
  local_mocked_bindings(
    fit_bayesnec = function(...) stop("fit reached"),
    .package = "bayesnec"
  )
  run <- function(x_control) {
    out <- character(0)
    withCallingHandlers(
      expect_error(bnec(f, d, family = Gamma(link = "identity"),
                        x_control = x_control), "fit reached"),
      message = function(m) {
        out <<- c(out, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    paste(out, collapse = "")
  }
  observed <- run(0.5)
  expect_match(observed, "is 4.48% of the control mean at log\\(x\\) = -0.693")
  expect_no_match(observed, "below the lowest observed concentration")
  below <- run(0.25)
  expect_match(below, "The control at 0.25 is below the lowest observed")
  expect_match(below, "percentage is unavailable because the mean at log\\(x\\)")
  expect_no_match(run(4), "below the lowest observed concentration")
})

test_that("grouped and hurdle pre-fit checks transform the control", {
  # Halve the series so that each level's logged predictor has a negative
  # value, which "auto" needs to treat it as logged.
  d <- log_control_data()
  d$x <- d$x / 2
  d$site <- rep(c("a", "b"), length.out = nrow(d))
  d <- d[!(d$site == "b" & d$x == 0.25), ]
  received <- NULL
  local_mocked_bindings(
    bnec = function(...) {
      received <<- list(...)
      stop("fit reached")
    },
    .package = "bayesnec"
  )
  top <- brms::prior_string("normal(90, 10)", nlpar = "top")
  msgs <- character(0)
  withCallingHandlers(
    expect_error(bnec_group(
      y ~ crf(log(x), "nec4param"), d, group_var = "site",
      family = Gamma(link = "identity"), prior_type = "regularizing",
      prior = top, x_control = 0.25
    ), "fit reached"),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_true(any(grepl("below the lowest observed concentration in 1 group",
                        msgs)))
  expect_equal(received$x_control, 0.25)
  expect_error(suppressMessages(bnec_group(
    y ~ crf(log(x), "nec4param"), d, group_var = "site",
    family = Gamma(link = "identity"), x_control = 8
  )), "below the highest observed concentration")

  h <- log_control_data()
  h$y[h$x == 16] <- 0
  h$y[h$x == 8][1:2] <- 0
  msgs <- character(0)
  withCallingHandlers(
    expect_error(bnec_hurdle(
      y ~ crf(log(x), "nec3param"), h, prior_type = "regularizing",
      x_control = 0.5
    ), "fit reached"),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_false(any(grepl("below the lowest observed", msgs)))
  msgs <- character(0)
  withCallingHandlers(
    expect_error(bnec_hurdle(y ~ crf(log(x), "nec3param"), h,
                             x_control = 0.25), "fit reached"),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_true(any(grepl("growth and survival component", msgs)))
})

test_that("a negative control is refused on a concentration-scale predictor", {
  f <- y ~ crf(x, c("nec3param", "ecxll4"))
  expect_error(get_priors(f, nec_data, family = Beta(), x_control = -0.5),
               "treated as a concentration")
  expect_error(get_priors(f, nec_data, family = Beta(), x_control = -0.5,
                          predictor_scale = "concentration"),
               "requires a non-negative predictor")
  # A logged predictor may have a negative control. Under "auto" nec_data$x
  # reads as a concentration, so the declaration is needed.
  p <- get_priors(f, nec_data, family = Beta(), x_control = -0.5,
                  predictor_scale = "log")
  expect_equal(as.numeric(p$nec3param$lb[p$nec3param$nlpar == "nec"]), -0.5)
  expect_no_error(get_priors(y ~ crf(log_x, "nec3param"), nec_data,
                             family = Beta(), x_control = -4))
})

test_that("a positive control is refused when its log is negative", {
  d <- data.frame(x = rep(c(1, 2, 4, 8, 16, 32), each = 4),
                  y = rep(c(0.9, 0.85, 0.5, 0.3, 0.2, 0.1), each = 4))
  expect_error(
    get_priors(y ~ crf(log(x), "nec3param"), d, family = Beta(),
               x_control = 0.5),
    "`x_control = 0.5` is -0.693 on the transformed predictor scale",
    fixed = TRUE
  )
})

test_that("a call-time control is compared with the lowest observation", {
  lowest <- min(ecx4param$fit$data$x)
  above <- ecx4param
  above$call_args <- list(x_control = 0.0484)
  expect_no_warning(bayesnec:::resolve_x_control(above, lowest, NA))
  below <- ecx4param
  below$call_args <- list(x_control = 0)
  expect_warning(bayesnec:::resolve_x_control(below, 0.01, NA),
                 paste0("0.01 is below the lowest observed concentration, ",
                        signif(lowest, 3)))
  other <- below
  other$fit$data$x <- other$fit$data$x + 0.1
  fits <- list(a = below, b = other)
  # The recorded control was reported at fitting, so it is not warned of again.
  expect_no_warning(
    dots <- bayesnec:::shared_level_control(fits, list(x_control = "shared"))
  )
  expect_equal(as.numeric(dots$x_control), 0)
  expect_no_warning(bayesnec:::resolve_x_control(below, 0, NA))
  expect_warning(
    bayesnec:::shared_level_control(fits, list(x_control = 0.01)),
    "below the lowest observation of 2 level\\(s\\)"
  )
  equal <- above
  equal$fit$data$x <- equal$fit$data$x + 0.1
  expect_no_warning(bayesnec:::shared_level_control(
    list(a = above, b = equal), list(x_control = lowest + 0.1)
  ))
})

# A control below nec_data's lowest concentration, 0.0323, moves the lower
# bound of the default nec prior to the control.
below_control_fit <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      cached <<- suppressWarnings(suppressMessages(bnec(
        y ~ crf(x, "nec4param"), data = nec_data, family = Beta(),
        x_control = 0.01, chains = 2, iter = 400, warmup = 200, seed = 591,
        refresh = 0
      )))
    }
    cached
  }
})

test_that("the prior-bound record reports a control below the data", {
  skip_on_cran()
  fit <- below_control_fit()
  expect_equal(bayesnec:::recorded_x_control(fit), 0.01)
  record <- attr(prior_bound_notes(curve_params(fit))$value,
                 "prior_bound_summary")
  lower <- record[record$parameter == "nec" & record$side == "lower", ]
  expect_equal(nrow(lower), 1)
  expect_equal(lower$fitted_bound, 0.01)
  expect_equal(lower$bound, 0.01)
  expect_equal(bayesnec:::ne_prior_bounds(fit$fit)$lb, 0.01)
})

# Three plates spanning the series, the lowest concentration 0.05, with a
# control stated below it.
plate_control_fit <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      set.seed(554)
      d <- expand.grid(x = c(0.05, 0.15, 0.4, 0.8, 1.5, 3),
                       plate = paste0("p", 1:3), replicate = 1:3)
      gain <- c(p1 = 0.85, p2 = 1, p3 = 1.2)
      d$y <- 3 * gain[as.character(d$plate)] *
        exp(-0.35 * pmax(d$x - 0.6, 0)) + stats::rnorm(nrow(d), 0, 0.12)
      d$plate <- as.character(d$plate)
      cached <<- suppressWarnings(suppressMessages(bnec(
        y ~ crf(x, "ecx4param") + ogl(plate), data = d, family = "gaussian",
        x_control = 0.01, sig_val = 0.2, resolution = 40, iter = 300,
        warmup = 150, chains = 2, seed = 554, refresh = 0
      )))
    }
    cached
  }
})

test_that("a spanning group fit stores its NSEC at the recorded control", {
  skip_on_cran()
  fit <- plate_control_fit()
  post <- suppressWarnings(nsec(fit, posterior = TRUE, resolution = 40,
                                sig_val = 0.2))
  expect_equal(unname(attr(post, "x_control")), rep(0.01, 3))
  expect_equal(as.numeric(fit$ne_posterior), as.numeric(post))
  own <- suppressWarnings(nsec(fit, pool = "none", resolution = 40,
                               sig_val = 0.2))
  shared <- suppressWarnings(nsec(fit, pool = "none", resolution = 40,
                                  sig_val = 0.2, x_control = "shared"))
  expect_equal(own, shared)
})

test_that("estimators measure from the recorded control by default", {
  skip_on_cran()
  fit <- ecx4param
  fit$call_args <- list(x_control = 0.001)
  lowest <- min(fit$fit$data$x)
  value <- function(x) as.numeric(x)
  expect_equal(value(ecx(fit, resolution = 50)),
               value(ecx(fit, resolution = 50, x_control = 0.001)))
  expect_false(isTRUE(all.equal(
    value(ecx(fit, resolution = 50)),
    value(ecx(fit, resolution = 50, x_control = lowest))
  )))
  expect_equal(value(nsec(fit, resolution = 50)),
               value(nsec(fit, resolution = 50, x_control = 0.001)))
  expect_equal(value(ecnsec(fit, nsec = 1, resolution = 50)),
               value(ecnsec(fit, nsec = 1, resolution = 50,
                            x_control = 0.001)))
})
