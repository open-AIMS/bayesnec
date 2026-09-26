test_that("Check for data when using formula syntax", {
  expect_error(bnec(y ~ crf(x, "ecxlin")), "argument \"data\" is missing")
})

test_that("user-supplied `prior` is not captured by partial matching to `prior_type`", {
  # Regression test: `prior_type` was added as a formal that `prior` (a common
  # brms argument) partial-matches, so `prior = <brmsprior>` was silently bound
  # to `prior_type`, tripping its match.arg() before any data checks. With an
  # explicit `prior` formal, `prior =` is matched exactly and reaches the normal
  # missing-data error instead of the spurious match.arg() error.
  mp <- brms::prior_string("beta(5, 1)", nlpar = "top")
  expect_true("prior" %in% names(formals(bnec)))
  expect_error(bnec(y ~ crf(x, "nec3param"), prior = mp),
               "argument \"data\" is missing")
})

test_that("predictor_scale is an explicit bnec argument (#317)", {
  expect_true("predictor_scale" %in% names(formals(bnec)))
  expect_error(
    bnec(y ~ crf(x, "nec3param"), data = nec_data,
         predictor_scale = "unknown"),
    "arg.*one of"
  )
  expect_error(
    bnec(y ~ crf(log_x, c("nec3param", "nec4param")), data = nec_data,
         predictor_scale = "concentration"),
    "requires a non-negative predictor"
  )
})

test_that("a model set states the missing-value refusal once, from bnec()", {
  # check_data() runs once per model inside fit_bayesnec(), and bnec() wraps
  # that call in try() for a model set, so a refusal raised from there was
  # printed once per model and the call then ended on the generic
  # all-models-failed advice, which names neither the missing values nor the
  # remedy. The default model argument is a set, so that is the common path.
  # The check is raised in bnec() instead, immediately after the model frame is
  # built. See #278.
  d <- nec_data
  d$y[3] <- NA
  msg <- tryCatch(bnec(y ~ crf(x, model = c("nec3param", "nec4param")),
                       data = d, family = Beta(link = "identity")),
                  error = conditionMessage)
  expect_match(msg, "1 row\\(s\\) with missing values")
  expect_false(grepl("None of the models fit successfully", msg))
})

test_that("the refusal precedes the family choice, so nothing is read off a subset", {
  # Placed immediately after the model frame is built rather than beside
  # check_normalisation(), so that nothing downstream, retrieve_valid_family()
  # included, is decided from a smaller sample than was supplied. Asserted by
  # the absence of the family-selection message, which is the only observable
  # thing that runs between the two positions on this input. See #278.
  d <- nec_data
  d$y[3] <- NA
  msgs <- capture.output(
    msg <- tryCatch(bnec(y ~ crf(x, model = "nec3param"), data = d),
                    error = conditionMessage),
    type = "message"
  )
  expect_match(msg, "1 row\\(s\\) with missing values")
  expect_length(msgs, 0)
})

test_that("Check models inappropriate for negative x are dropped", {
  # The family is given explicitly because nec_data's response is 0-1 bounded,
  # for which nechorme4pwr is now excluded up front (#177) -- the negative-x
  # rule would never be reached. Gamma leaves it in the set so that this test
  # still exercises the rule it is about.
  bnec(y ~ crf(log_x, "nechorme4pwr"), data = nec_data,
       family = Gamma(link = "identity")) |>
    expect_message("Dropping the model\\(s\\) nechorme4pwr as they are not valid for data with negative predictor \\(x\\) values\\.") |>
    expect_error("No valid models have been supplied for this data type.")
})


test_that("bnec refuses a resolution below 2 before fitting anything", {
  # A property of the call, fixed before any model is fitted. Left to arrive
  # from expand_nec(), where the no-effect estimate of a smooth equation is read
  # off the grid, it arrives only after every model in the set has compiled and
  # sampled. See #325.
  expect_error(
    bnec(y ~ crf(x, model = "nec3param"), data = nec_data, resolution = 1),
    "must be at least 2"
  )
})

test_that("a response at one bound is fitted as ecxflat alone (#400, #419)", {
  # Each case failed inside prior construction on the quantile() error, which
  # named neither the column nor the cause, except beta at 1, which was shifted
  # to 0.999 and fitted, and beta at 0, which was shifted to Inf. #400 refused
  # them; D31 fits the constant equation to them instead, decided once before
  # the model loop. The two-equation set asked for is replaced, so the call
  # takes the single-model branch and never reaches the model loop. Beta at 0
  # cannot be fitted by ecxflat either and is still refused.
  loop_calls <- 0L
  fitted <- character(0)
  local_mocked_bindings(
    bnec_parallel_lapply = function(...) {
      loop_calls <<- loop_calls + 1L
      stop("reached the model loop")
    },
    fit_bayesnec = function(..., model) {
      fitted <<- c(fitted, model)
      stop("fit reached")
    },
    .package = "bayesnec"
  )
  cases <- at_bound_cases()
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    msgs <- character(0)
    err <- tryCatch(
      withCallingHandlers(
        bnec(cs$formula, data = cs$data, family = cs$family),
        message = function(m) {
          msgs <<- c(msgs, conditionMessage(m))
          invokeRestart("muffleMessage")
        }
      ),
      error = conditionMessage
    )
    if (nm == "beta_zero") {
      expect_match(err, paste0("The response \"", cs$column, "\" is at the ",
                               cs$bound, " bound"), fixed = TRUE, info = nm)
      expect_match(err, "the constant equation ecxflat included",
                   fixed = TRUE, info = nm)
      # Refused before any report on the response, as #400 placed it.
      expect_length(msgs, 0)
      next
    }
    expect_identical(err, "fit reached", info = nm)
    expect_match(msgs[1], paste0("The response \"", cs$column, "\" is at the ",
                                 cs$bound, " bound"), fixed = TRUE, info = nm)
    expect_match(msgs[1], paste("fitted with the constant equation ecxflat",
                                "alone, and the 2 other equation(s) requested",
                                "are not fitted"), fixed = TRUE, info = nm)
  }
  expect_identical(fitted, rep("ecxflat", length(cases) - 1L))
  expect_identical(loop_calls, 0L)
})

test_that("ecxflat named alone at a bound fits without the report (#419)", {
  # Nothing was set aside, so there is nothing to say why.
  fitted <- character(0)
  local_mocked_bindings(
    fit_bayesnec = function(..., model) {
      fitted <<- c(fitted, model)
      stop("fit reached")
    },
    .package = "bayesnec"
  )
  cs <- at_bound_cases()$bernoulli_one
  msgs <- character(0)
  err <- tryCatch(
    withCallingHandlers(
      bnec(alive ~ crf(x, "ecxflat"), data = cs$data, family = cs$family),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    ),
    error = conditionMessage
  )
  expect_identical(err, "fit reached")
  expect_identical(fitted, "ecxflat")
  expect_false(any(grepl("ecxflat alone", msgs, fixed = TRUE)))
})

test_that("one observation off the bound reaches the model loop (#400)", {
  local_mocked_bindings(
    bnec_parallel_lapply = function(...) stop("reached the model loop"),
    .package = "bayesnec"
  )
  cases <- at_bound_cases()
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    expect_error(
      suppressWarnings(suppressMessages(
        bnec(cs$formula, data = cs$near, family = cs$family)
      )),
      "reached the model loop", info = nm
    )
  }
})

# ---- #419, the constant equation, fitted --------------------------------------

# Four fits, each made once and shared by every test below, because compiling
# the Stan program is what these tests cost. A bernoulli response of 1
# throughout, requested with the whole default set, which bnec() fits as
# ecxflat alone; a gaussian response with no concentration effect, fitted with
# ecxflat named beside nec3param, so that both take appreciable weight and the
# mixture holds ecxflat draws; and manec_example, which declines, amended with
# ecxflat, which is a response with an effect; and the first again on a logged
# predictor with an x_range reaching 0. Short chains at a fixed seed:
# what is asserted is where the draws lie, not their precision. amend() takes
# the sampler settings of the set it amends and has no seed argument.
flat_fixtures <- local({
  cache <- list()
  keep_messages <- function(expr) {
    msgs <- character(0)
    value <- withCallingHandlers(
      suppressWarnings(expr),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    list(fit = value, messages = msgs)
  }
  function(which) {
    if (is.null(cache[[which]])) {
      x <- rep(c(0.1, 0.5, 1, 3, 10, 30), each = 5)
      cache[[which]] <<- switch(
        which,
        at_bound = keep_messages(
          bnec(alive ~ crf(x, model = "all"),
               data = data.frame(x = x, alive = 1L), family = "bernoulli",
               chains = 2, iter = 300, warmup = 200, seed = 419, refresh = 0)
        ),
        no_effect = keep_messages({
          set.seed(419)
          flat <- data.frame(x = rep(c(0, 0.25, 0.5, 1, 2, 4), each = 6))
          flat$y <- stats::rnorm(nrow(flat), 2, 0.3)
          bnec(y ~ crf(x, c("ecxflat", "nec3param")), data = flat,
               family = gaussian(), chains = 2, iter = 300, warmup = 200,
               seed = 419, refresh = 0)
        }),
        effect = keep_messages(amend(manec_example, add = "ecxflat")),
        # An x_range reaching 0 under crf(log(x)) puts -Inf at the foot of the
        # prediction grid. With the mean written top + 0 * x that point was
        # NaN, the NSEC search stopped in quantile() after sampling, and under
        # the substitution the whole call failed with that unnamed error.
        log_range = keep_messages(
          bnec(alive ~ crf(log(x), model = "all"),
               data = data.frame(x = x, alive = 1L), family = "bernoulli",
               x_range = c(0, 30), chains = 2, iter = 300, warmup = 200,
               seed = 419, refresh = 0)
        )
      )
    }
    cache[[which]]
  }
})

# Every draw of an ECx censored above the upper end of the grid, for each type
# the family admits: "relative" needs a lower bound to measure towards, which
# a gaussian response does not have.
expect_ecx_all_above <- function(fit, types) {
  for (type in types) {
    e <- suppressWarnings(ecx(fit, posterior = TRUE, type = type))
    cens <- attr(e, "censored")
    expect_true(all(is.na(e)), info = type)
    expect_true(all(cens$above), info = type)
    expect_false(any(cens$below), info = type)
    est <- suppressWarnings(ecx(fit, type = type))
    expect_identical(attr(est, "censored_summary")$bound, rep(">=", 3),
                     info = type)
  }
}

# The NSEC of a constant: every draw above the reference never falls to it and
# is censored above, and a draw at or below it takes the control, the rule
# nsec() applies to every equation. For a constant the draw's control is its
# top, so those draws are exactly the ones whose top is at or below the
# sig_val quantile of top.
expect_nsec_above_but_control <- function(fit, sig_val = 0.01) {
  ns <- suppressWarnings(nsec(fit, posterior = TRUE, sig_val = sig_val))
  cens <- attr(ns, "censored")
  top <- brms::as_draws_df(fit$fit)$b_top_Intercept
  at_control <- !is.na(ns)
  expect_identical(sum(at_control),
                   sum(top <= stats::quantile(top, sig_val)))
  expect_true(all(ns[at_control] == min(fit$fit$data$x)))
  expect_identical(sum(cens$above), length(ns) - sum(at_control))
  expect_false(any(cens$below))
}

test_that("a bernoulli response of 1 throughout is fitted as ecxflat (#419)", {
  skip_on_cran()
  fx <- flat_fixtures("at_bound")
  fit <- fx$fit
  expect_s3_class(fit, "bayesnecfit")
  expect_identical(fit$model, "ecxflat")
  expect_true(any(grepl(
    "fitted with the constant equation ecxflat alone, and the 23 other",
    fx$messages, fixed = TRUE
  )))
  # The set asked for, the equation fitted and why the rest were not.
  rec <- bnec_record(fit)
  expect_setequal(rec$requested, names(models("all")))
  expect_identical(rec$attempted, "ecxflat")
  expect_setequal(rec$excluded$model, names(models("all")))
  expect_true(all(grepl("upper bound of a bernoulli response",
                        rec$excluded$reason, fixed = TRUE)))
  # Its only curve parameter is the level of the response, which sits near 1.
  expect_identical(bayesnec:::equation_par_names(fit$model), "top")
  expect_gt(unname(fit$top["Estimate"]), 0.8)
  expect_identical(fit$ne_type, "NSEC")
})

test_that("every ECx of ecxflat is censored above, with or without an effect (#419)", {
  skip_on_cran()
  expect_ecx_all_above(flat_fixtures("at_bound")$fit,
                       c("absolute", "relative", "range"))
  no_effect <- suppressMessages(
    pull_out(flat_fixtures("no_effect")$fit, model = "ecxflat")
  )
  expect_ecx_all_above(no_effect, c("absolute", "range"))
  effect <- suppressMessages(
    pull_out(flat_fixtures("effect")$fit, model = "ecxflat")
  )
  expect_ecx_all_above(effect, c("absolute", "range"))
})

test_that("the NSEC of ecxflat is censored above but for the control share (#419)", {
  skip_on_cran()
  fit <- flat_fixtures("at_bound")$fit
  expect_nsec_above_but_control(fit)
  expect_nsec_above_but_control(fit, sig_val = 0.05)
  # The stored no-effect estimate is that NSEC, read on the fit's own grid, and
  # at the default sig_val every entry of its summary is the bound.
  cens <- attr(fit$ne_posterior, "censored")
  expect_identical(sum(cens$above) + sum(!is.na(fit$ne_posterior)),
                   length(fit$ne_posterior))
  expect_identical(attr(fit$ne, "censored_summary")$bound, rep(">=", 3))
  effect <- suppressMessages(
    pull_out(flat_fixtures("effect")$fit, model = "ecxflat")
  )
  expect_nsec_above_but_control(effect)
})

test_that("nec() refuses a single ecxflat fit, and summary() labels an NSEC (#419)", {
  skip_on_cran()
  fit <- flat_fixtures("at_bound")$fit
  expect_error(nec(fit), "nec is not a parameter in ecx model types")
  s <- summary(fit)
  expect_true(s$is_ecx)
  expect_identical(rownames(s$nec_vals), "NSEC")
})

test_that("a set holding ecxflat mixes its NSEC draws into the N(S)EC (#419)", {
  skip_on_cran()
  fit <- flat_fixtures("no_effect")$fit
  expect_s3_class(fit, "bayesmanecfit")
  expect_identical(fit$ne_type, "N(S)EC")
  # On a response with no effect ecxflat takes appreciable weight, so the
  # mixture holds its draws: the first of the two segments, in the order of
  # success_models.
  expect_identical(fit$success_models, c("ecxflat", "nec3param"))
  seg <- seq_along(fit$w_draw_index$ecxflat)
  expect_gt(length(seg), 0)
  vals <- as.numeric(fit$w_ne_posterior)[seg]
  above <- attr(fit$w_ne_posterior, "censored")$above[seg]
  control <- min(fit$mod_fits$ecxflat$fit$data$x)
  expect_true(all(above | (!is.na(vals) & vals == control)))
  expect_gt(sum(above), 0)
  msgs <- character(0)
  withCallingHandlers(
    suppressWarnings(nec(fit)),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_true(any(grepl("contains smooth (ecx) models", msgs, fixed = TRUE)))
  expect_identical(summary(fit, check_fit = FALSE)$ecx_mods, "ecxflat")
})

test_that("ecxflat added to a declining response takes almost no weight (#419)", {
  skip_on_cran()
  fit <- flat_fixtures("effect")$fit
  expect_setequal(fit$success_models, c("nec4param", "ecx4param", "ecxflat"))
  expect_lt(fit$mod_stats["ecxflat", "wi"], 0.01)
  # Read off each fit's parameters: two smooth equations and one threshold.
  expect_identical(fit$ne_type, "N(S)EC")
})

test_that("ecxflat plots as a horizontal line with bound labels (#419)", {
  skip_on_cran()
  fit <- flat_fixtures("at_bound")$fit
  g <- suppressMessages(ggbnec_data(fit))
  expect_length(unique(g$y_e[!is.na(g$y_e)]), 1)
  labs <- g[!is.na(g$nec_labs), c("nec_labs", "nec_labs_l", "nec_labs_u")]
  expect_true(all(startsWith(unlist(labs), ">=")))
  expect_s3_class(suppressMessages(autoplot(fit)), "ggplot")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(plot(fit))
})

test_that("ecxflat estimates take the scale message once and exceedance() (#419)", {
  skip_on_cran()
  fit <- flat_fixtures("at_bound")$fit
  # The scale message of #299 reads the formula only, so the fit is relabelled
  # with an inline log, as test-fitted_scale.R relabels its fixtures. The stored
  # fit was not fitted with it; only the message is counted.
  logged <- fit
  logged$bayesnecformula <- bnf(alive ~ crf(log(x), model = "ecxflat"))
  scale_msgs <- function(expr) {
    msgs <- testthat::capture_messages(suppressWarnings(expr))
    sum(grepl("transforms its predictor inline", msgs, fixed = TRUE))
  }
  expect_identical(scale_msgs(ecx(logged)), 1L)
  expect_identical(scale_msgs(nsec(logged)), 1L)
  expect_identical(scale_msgs(ecx(logged, xform = exp)), 0L)
  # Inside the range every ECx draw is censored above, so it exceeds the
  # threshold and the probability is exact. At or above the top of the range
  # the record does not decide those draws, and the interval is reported.
  inside <- suppressWarnings(exceedance(fit, 3, estimate = "ecx"))
  expect_identical(inside$prob, 1)
  expect_equal(inside$n_above, inside$n_draws)
  beyond <- suppressWarnings(exceedance(fit, 100, estimate = "ecx"))
  expect_true(is.na(beyond$prob))
  expect_equal(c(beyond$prob_lower, beyond$prob_upper), c(0, 1))
  # The NSEC differs by the draws at the control, which exceed no threshold
  # above it.
  ns <- suppressWarnings(exceedance(fit, 100, estimate = "nsec"))
  expect_equal(ns$prob_upper, ns$n_above / ns$n_draws)
  # The default estimate is the NEC, which nec() refuses for a single fit
  # without a nec parameter.
  expect_error(exceedance(fit, 3), "nec is not a parameter")
})

test_that("ecxflat is fitted on a grid reaching -Inf under crf(log(x)) (#419)", {
  skip_on_cran()
  fit <- flat_fixtures("log_range")$fit
  expect_identical(fit$model, "ecxflat")
  # The mean is finite, and exactly top, at every grid point, the foot at
  # log(0) included, so the curve is still one horizontal line.
  expect_identical(min(fit$pred_vals$data$x), 0)
  expect_true(all(is.finite(fit$pred_vals$data$Estimate)))
  expect_length(unique(fit$pred_vals$data$Estimate), 1)
  expect_false(any(is.nan(fit$ne_posterior)))
  expect_identical(attr(fit$ne, "censored_summary")$bound, rep(">=", 3))
  e <- suppressWarnings(suppressMessages(
    ecx(fit, x_range = c(0, 30), posterior = TRUE)
  ))
  expect_true(all(attr(e, "censored")$above))
})

test_that("a misspelt equation at a bound is refused, not set aside (#419)", {
  # The set is replaced by ecxflat before check_models() sees it, so the name
  # is checked first; otherwise it was recorded as excluded for the bound and
  # the fit went on.
  local_mocked_bindings(
    fit_bayesnec = function(...) stop("fit reached"),
    .package = "bayesnec"
  )
  cs <- at_bound_cases()$bernoulli_one
  expect_error(
    suppressMessages(bnec(alive ~ crf(x, c("nec4param", "bogus")),
                          data = cs$data, family = cs$family)),
    "bogus; is not a valid model entry", fixed = TRUE
  )
})
