test_that("prob_vals warnings behave as expected", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ecx(manec_example, prob_vals = c(0.6, 0.1, 0.9), resolution = 10,
      type = "relative") |>
    expect_length(3) |>
    suppressWarnings()
  ecx(manec_example, prob_vals = 0.9, resolution = 10, type = "relative") |>
    expect_error() |>
    suppressWarnings()
  ecx(manec_example, prob_vals = c(0.6, 0.9, 0.1), resolution = 10,
      type = "relative") |>
    expect_error() |>
    suppressWarnings()
  ecx(nec4param, prob_vals = c(0.6, 0.1, 0.9), resolution = 10,
      type = "relative") |>
    expect_length(3) |>
    suppressWarnings()
  expect_error(ecx(nec4param, prob_vals = 0.9, type = "relative",
                   resolution = 10))
  expect_error(ecx(nec4param, prob_vals = c(0.6, 0.9, 0.1), resolution = 10,
                   type = "relative"))
})

test_that("ecx_val warnings behave as expected", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # Accept effects between zero and one and above 100 percent. For absolute
  # ECx with a positive control, effects above 100 target a negative response;
  # that can be meaningful for an unbounded response.
  expect_length(
    suppressWarnings(
      ecx(manec_example, ecx_val = 0.9, type = "relative", resolution = 10)
    ), 3
  )
  expect_length(
    suppressWarnings(
      ecx(nec4param, ecx_val = 0.9, type = "relative", resolution = 10)
    ), 3
  )
  expect_length(ecx(nec4param, ecx_val = 150, resolution = 10), 3)
  expect_error(ecx(nec4param, ecx_val = 0, resolution = 10),
               "greater than 0")
  expect_error(ecx(nec4param, ecx_val = -5, resolution = 10),
               "greater than 0")
})

test_that("ecx refuses arguments it cannot use", {
  expect_error(ecx(nec4param, resolution = 1), "at least 2")
  expect_error(ecx(nec4param, resolution = 0), "at least 2")
  expect_error(ecx(nec4param, resolution = 10.5), "whole number")
  expect_error(ecx(nec4param, ecx_val = NA_real_), "ecx_val")
  expect_error(ecx(nec4param, ecx_val = c(10, 50)), "ecx_val")
  expect_error(ecx(nec4param, posterior = "yes"), "posterior")
  expect_error(ecx(manec_example, posterior = NA), "posterior")
  expect_error(ecx(nec4param, pool = "bogus"), "`pool` must be one of")
  expect_error(ecx(nec4param, foo = 1), "ecx() does not use `foo`",
               fixed = TRUE)
  expect_error(ecx(manec_example, foo = 1), "does not use `foo`")
})

test_that("the relative rename warns once for a call over several levels", {
  skip_on_cran()
  # The joint_split() fit is gaussian with nec4param, which has no asymptote
  # for type = "relative"; measure towards 0 so that both levels are read.
  local_mocked_bindings(ecx_asymptote = function(object, type, dpar = NULL) 0)
  n <- 0
  withCallingHandlers(
    invisible(ecx(joint_split(), type = "relative", resolution = 20)),
    warning = function(w) {
      if (grepl("now measures", conditionMessage(w))) n <<- n + 1
      invokeRestart("muffleWarning")
    },
    message = function(m) invokeRestart("muffleMessage")
  )
  expect_equal(n, 1)
})

test_that("ecx returns expected object types and arguments pass correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ec50_summary <- ecx(manec_example, ecx_val = 50, type = "relative",
                      resolution = 50)
  ec50_summary2 <- ecx(manec_example, ecx_val = 50, type = "relative",
                       resolution = 50, xform = exp)
  ec50_posterior <- ecx(manec_example, ecx_val = 50,
                        type = "relative", posterior = TRUE, resolution = 50)
  ec50n_summary <- ecx(nec4param, ecx_val = 50, type = "relative",
                       resolution = 50)
  ec50n_summary2 <- ecx(nec4param, ecx_val = 50, type = "relative",
                        resolution = 50, xform = exp)
  ec50n_posterior <- ecx(nec4param, ecx_val = 50, type = "relative",
                         posterior = TRUE, resolution = 50)
  expect_equal(length(ec50_summary), 3)
  expect_gt(length(ec50_posterior), 3)
  expect_equal(length(ec50n_summary), 3)
  expect_gt(length(ec50n_posterior), 3)
  expect_equal(attributes(ec50_summary)$resolution, 50)
  expect_equal(attributes(ec50_posterior)$resolution, 50)
  expect_equal(attributes(ec50n_summary)$resolution, 50)
  expect_equal(attributes(ec50n_posterior)$resolution, 50)
  expect_gt(ec50_summary2[1], ec50_summary[1])
  expect_gt(ec50n_summary2[1], ec50n_summary[1])
})

test_that("works for bayesnecfit", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ecx1 <- ecx(ecx4param, resolution = 10)
  expect_equal(length(ecx1), 3)
  expect_equal(names(ecx1), c("Q50", "Q2.5", "Q97.5"))
})

test_that("endpoint extraction defaults to a 500-point grid", {
  expect_identical(formals(ecx)$resolution, 500)
  expect_identical(formals(nsec)$resolution, 500)
  expect_identical(formals(ecnsec)$resolution, 500)
  expect_identical(formals(ecx.bayesnecfit)$resolution, 500)
  expect_identical(formals(nsec.bayesnecfit)$resolution, 500)
})

test_that("works for bayesmanecfit", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ecx1 <- ecx(manec_example, resolution = 10)
  expect_equal(length(ecx1), 3)
  expect_equal(names(ecx1), c("Q50", "Q2.5", "Q97.5"))
})

test_that("xform passes correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ecx1 <- ecx(ecx4param, resolution = 10)
  ecx2 <- ecx(ecx4param, xform = exp, resolution = 10)
  expect_gt(ecx2[1], ecx1[2])
})

test_that("posterior passes correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ecx3 <- ecx(ecx4param, posterior = TRUE, resolution = 10)
  expect_equal(length(ecx3), brms::ndraws(ecx4param$fit))
})

test_that("prob_vals passes correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ecx4 <- ecx(ecx4param, prob_vals = c(0.5, 0.3, 0.7), resolution = 10)
  expect_equal(names(ecx4), c("Q50", "Q30", "Q70"))
})

test_that("ecx_val passes correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  ecx4 <- ecx(ecx4param, prob_vals = c(0.5, 0.3, 0.7), ecx_val = 20,
              resolution = 10)
  expect_equal(names(ecx4), c("Q50", "Q30", "Q70"))
})


# Control references and ECx types

test_that("the four types are accepted and give different references", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  a <- ecx(nec4param, ecx_val = 10, type = "absolute", resolution = 200)
  r <- ecx(nec4param, ecx_val = 10, type = "range", resolution = 200)
  rel <- suppressWarnings(
    ecx(nec4param, ecx_val = 10, type = "relative", resolution = 200)
  )
  d <- ecx(nec4param, ecx_val = 1.5, type = "direct", resolution = 200)
  for (v in list(a, r, rel, d)) {
    expect_length(v, 3)
  }
  # On this fixture, bot is below the predicted minimum and both are below
  # zero. A fixed percentage requires a later crossing over a wider span,
  # so absolute < range < relative; this ordering is not universal.
  expect_lt(a[1], r[1])
  expect_lt(r[1], rel[1])
  expect_error(ecx(nec4param, type = "nonsense"), "type must be one of")
  expect_error(ecx(nec4param, type = c("absolute", "range")),
               "type must be one of")
})

test_that("relative warns that it names a different quantity than in 2.1.3", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # Warn only when relative is explicitly requested; the default absolute
  # call must stay silent.
  expect_warning(ecx(nec4param, type = "relative", resolution = 100),
                 "now measures from the control")
  expect_silent(ecx(nec4param, resolution = 100))
})

test_that("relative is refused where the bound is infinite", {
  # Without bot, zero is the assumed asymptote. Refuse relative ECx where
  # the response family is not bounded below, naming valid alternatives.
  skip_on_cran()
  # Mock the draws lookup: changing the model name alone leaves the stored
  # bot column and fails to reach the no-bot branch.
  local_mocked_bindings(as_draws_df = function(...) data.frame(lp__ = 1),
                        .package = "bayesnec")
  fake <- nec4param
  fake$model <- "nec3param"
  fake$fit$family <- stats::gaussian()
  expect_error(ecx_asymptote(fake, "relative"), "no bot parameter")
  expect_error(ecx_asymptote(fake, "relative"), "not bounded below")
  # Bounded below: the asymptote is 0 and the request is answered.
  fake$fit$family <- stats::Gamma(link = "identity")
  expect_equal(ecx_asymptote(fake, "relative"), 0)
  # Any other type never asks for an asymptote.
  expect_true(is.na(ecx_asymptote(fake, "absolute")))
})

test_that("hormesis_def is refused by name rather than absorbed by dots", {
  skip_on_cran()
  expect_error(ecx(nec4param, hormesis_def = "max"),
               "hormesis_def has been removed")
  expect_error(ecx(manec_example, hormesis_def = "control"),
               "hormesis_def has been removed")
})

test_that("a target the curve never reaches returns NA with a warning", {
  # A direct target above every predicted response must yield no crossing,
  # not the nearest grid point or the control concentration.
  skip_on_cran()
  expect_warning(out <- ecx(nec4param, ecx_val = 100, type = "direct",
                            resolution = 100, posterior = TRUE),
                 "does not reach")
  expect_true(all(is.na(out)))
  # Censored draws contribute ranks, not numerical bound values. When every
  # rank lies above the range, all summary entries report the upper bound.
  expect_warning(est <- ecx(nec4param, ecx_val = 100, type = "direct",
                            resolution = 100))
  cens <- attr(est, "censored_summary")
  expect_identical(cens$bound, rep(">=", 3))
  expect_identical(cens$n_above, cens$n_draws)
  expect_true(all(est == cens$upper))
  expect_false(any(est == min(nec4param$pred_vals$data$x)))
})

test_that("the ecx reference is the control, not the maximum of the curve", {
  # Construct the hormetic curve directly to test the control reference
  # without involving a fitted model.
  x_vec <- seq(0, 10, length.out = 101)
  # Rises from a control of 10 to a peak of 15, then declines to 0.
  curve <- ifelse(x_vec < 2, 10 + 2.5 * x_vec, 15 * (1 - (x_vec - 2) / 8))
  p <- rbind(curve, curve)
  control <- c(10, 10)
  out <- ecx_from_posterior(p, x_vec, 10, "absolute", control, NA_real_)
  # The absolute EC10 target is 9, which the control-anchored curve reaches on
  # the descending limb. Measured from the maximum instead the target would be
  # 13.5, which the curve reaches while still rising and again while falling.
  expect_true(all(out > x_vec[which.max(curve)]))
  expect_equal(out[1], out[2])
  # An artificial maximum-based reference asks for the rising-limb target.
  # This internal call must return no ECx; public ecx() fixes the reference
  # at the control instead.
  from_max <- ecx_from_posterior(p, x_vec, 10, "absolute", c(15, 15), NA_real_)
  expect_true(all(is.na(from_max)))
})


test_that("the relative rename warns once for a set, not once per equation", {
  # A model-average call must warn once, even though it forwards an explicit
  # type to each equation.
  skip_on_cran()
  count_renames <- function(expr) {
    n <- 0
    withCallingHandlers(invisible(expr), warning = function(w) {
      if (grepl("now measures", conditionMessage(w))) n <<- n + 1
      invokeRestart("muffleWarning")
    })
    n
  }
  expect_equal(
    count_renames(ecx(manec_example, type = "relative", resolution = 50)), 1
  )
  expect_equal(
    count_renames(ecx(nec4param, type = "relative", resolution = 50)), 1
  )
  # The option is restored, so a later call in the same session still warns.
  expect_null(getOption("bayesnec.relative_warned"))
})

test_that("the estimate is insensitive to resolution above the default", {
  skip_on_cran()
  # For this stored fit, interpolation makes 200 and 1000 grid points agree
  # within the tolerances below. Compare numeric values only: the resolution
  # attribute necessarily differs.
  expect_equal(as.numeric(ecx(nec4param, resolution = 200)),
               as.numeric(ecx(nec4param, resolution = 1000)),
               tolerance = 1e-3)
  suppressWarnings({
    n200 <- as.numeric(nsec(nec4param, resolution = 200))
    n1000 <- as.numeric(nsec(nec4param, resolution = 1000))
  })
  expect_equal(n200, n1000, tolerance = 1e-3)
})

# Relative ECx on joint fits

# A stand-in for a joint fit. Tests mock the grid, the curve and the draws.
joint_relative_object <- function(family = "hurdle_gamma") {
  structure(list(model = "nec4param",
                 fit = list(family = validate_family(family)),
                 bayesnecformula = bnf(y ~ crf(x, "nec4param"))),
            class = c("bayesnecfit", "bnecfit"))
}

test_that("a relative ECx of a joint fit's combined curve is refused (#491)", {
  for (fam in c("hurdle_gamma", "zero_inflated_beta", "hurdle_poisson",
                "hurdle_negbinomial")) {
    block <- bayesnec:::hurdle_dpar(fam)
    expect_error(
      suppressWarnings(ecx(joint_relative_object(fam), type = "relative")),
      paste0("not defined for the combined curve of the ", fam,
             " family.*dpar = \"mu\" or dpar = \"", block, "\""),
      label = fam
    )
  }
  # every equation of a model set goes through the single-fit method
  set <- structure(list(mod_fits = list(a = joint_relative_object()),
                        success_models = "a", sample_size = 10),
                   class = c("bayesmanecfit", "bnecfit"))
  local_mocked_bindings(
    pull_draw_index = function(...) list(a = 1:10),
    pull_out = function(object, model, ...) object$mod_fits[[model]],
    .package = "bayesnec"
  )
  expect_error(suppressWarnings(ecx(set, type = "relative")),
               "combined curve of the hurdle_gamma family")
  # families with one block are not affected
  for (fam in c("gaussian", "Gamma", "negbinomial")) {
    expect_null(bayesnec:::check_combined_relative(validate_family(fam)),
                label = fam)
  }
})

test_that("the zero block's bot is the floor of the curve ecx reads (#491)", {
  # ecx(dpar = "hu") reads 1 - hu, so 1 - hu declines towards hubot
  d <- data.frame(x = rep(c(0, 1, 2, 3), each = 5), y = c(rep(1, 15), rep(0, 5)))
  for (fam in c("hurdle_gamma", "zero_inflated_beta")) {
    block <- bayesnec:::hurdle_dpar(fam)
    dd <- d
    if (fam == "zero_inflated_beta") dd$y <- ifelse(dd$y > 0, 0.5, 0)
    bf <- make_brmsformula(y ~ crf(x, "nec4param"), dd,
                           family = validate_family(fam))[[1]]
    expect_match(deparse1(bf$pforms[[block]][[3]]),
                 paste0("^1 - \\(", block, "bot \\+"), label = fam)
  }
})

test_that("a relative ECx of the zero block is measured towards its own bot (#491)", {
  grid <- seq(0, 5, length.out = 1001)
  ctrl <- c(0.9, 0.8, 0.95)
  floor_draws <- c(0.1, 0.2, 0.05)
  # each draw closes half its distance to the floor at x = log(2)
  curve <- t(vapply(1:3, function(i) {
    floor_draws[i] + (ctrl[i] - floor_draws[i]) * exp(-grid)
  }, numeric(length(grid))))
  # the response block's bot lies above every control here
  draws <- data.frame(b_bot_Intercept = c(2, 3, 1.5),
                      b_hubot_Intercept = floor_draws)
  local_mocked_bindings(
    newdata_eval = function(...) {
      list(newdata = data.frame(x = grid), x_vec = grid)
    },
    control_x = function(...) grid[1],
    joint_hurdle_epred = function(object, newdata, dpar = NULL,
                                  re_formula = NA) curve,
    control_posterior = function(...) ctrl,
    as_draws_df = function(x, ...) draws,
    .package = "bayesnec"
  )
  obj <- joint_relative_object()
  est <- suppressWarnings(ecx(obj, ecx_val = 50, type = "relative",
                              dpar = "hu", posterior = TRUE))
  expect_equal(as.numeric(est), rep(log(2), 3), tolerance = 0.01)
  for (fam in c("hurdle_gamma", "zero_inflated_beta", "hurdle_poisson",
                "hurdle_negbinomial")) {
    block <- bayesnec:::hurdle_dpar(fam)
    draws[[paste0("b_", block, "bot_Intercept")]] <- floor_draws
    full <- suppressWarnings(ecx(joint_relative_object(fam), ecx_val = 50,
                                  type = "relative", dpar = block,
                                  posterior = TRUE))
    abbreviated <- suppressWarnings(ecx(joint_relative_object(fam),
                                         ecx_val = 50, type = "relative",
                                         dpar = substr(block, 1, 1),
                                         posterior = TRUE))
    expect_equal(as.numeric(full), rep(log(2), 3), tolerance = 0.01)
    expect_equal(abbreviated, full, label = fam)
  }
  expect_equal(bayesnec:::ecx_asymptote(obj, "relative", "hu"), floor_draws)
  # the response block is still measured towards its own bot
  expect_equal(bayesnec:::ecx_asymptote(obj, "relative", "mu"), c(2, 3, 1.5))
  expect_equal(bayesnec:::ecx_asymptote(obj, "relative"), c(2, 3, 1.5))
  # an equation without bot in the zero block declines towards 0
  draws$b_hubot_Intercept <- NULL
  curve <- t(vapply(1:3, function(i) ctrl[i] * exp(-grid),
                    numeric(length(grid))))
  expect_identical(bayesnec:::ecx_asymptote(obj, "relative", "hu"), 0)
  est0 <- suppressWarnings(ecx(obj, ecx_val = 50, type = "relative",
                               dpar = "hu", posterior = TRUE))
  expect_equal(as.numeric(est0), rep(log(2), 3), tolerance = 0.01)
})

test_that("one level of a joint refit reads the zero block's bot (#491)", {
  obj <- joint_relative_object()
  class(obj) <- c("bayesnecjointlevel", class(obj))
  level_draws <- NULL
  local_mocked_bindings(
    joint_level_draws = function(object, par, ...) {
      if (identical(par, "hubot")) level_draws else NULL
    },
    as_draws_df = function(x, ...) {
      data.frame(b_bot_Intercept = 2, b_hubot_Intercept = 0.15)
    },
    .package = "bayesnec"
  )
  # the zero block takes no level term, so the plain name is read
  expect_equal(bayesnec:::ecx_asymptote(obj, "relative", "hu"), 0.15)
  # a level's own term is read where the refit has one
  level_draws <- c(0.3, 0.4)
  expect_equal(bayesnec:::ecx_asymptote(obj, "relative", "hu"), c(0.3, 0.4))
})

test_that("mean_scale puts bot on the response scale", {
  mean_scale <- bayesnec:::mean_scale
  as_fit <- function(family) list(fit = list(family = family))
  expect_equal(mean_scale(as_fit(brms::brmsfamily("Gamma", link = "log")),
                          log(c(2, 5))), c(2, 5))
  expect_equal(mean_scale(as_fit(brms::brmsfamily("Beta", link = "logit")),
                          0), 0.5)
  expect_identical(mean_scale(as_fit(brms::brmsfamily("gaussian")), -3), -3)
})

test_that("a relative ECx under a log link measures towards exp(bot)", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  # Simulated from nec4param on the log scale, so the fit mixes well.
  set.seed(578)
  x <- rep(c(0.1, 0.5, 1, 2, 4, 8, 16, 32), each = 8)
  eta <- log(2) + (log(20) - log(2)) * exp(-0.3 * pmax(x - 2, 0))
  y <- stats::rgamma(length(x), shape = 50, rate = 50 / exp(eta))
  fit <- suppressWarnings(suppressMessages(bnec(
    y ~ crf(x, "nec4param"), data = data.frame(x = x, y = y),
    family = Gamma(link = "log"), chains = 2, iter = 2000, seed = 578,
    refresh = 0
  )))
  expect_lt(max(brms::rhat(fit$fit), na.rm = TRUE), 1.05)
  bot <- as.numeric(brms::as_draws_df(fit$fit)[["b_bot_Intercept"]])
  expect_equal(bayesnec:::ecx_asymptote(fit, "relative"), exp(bot))
  est <- suppressWarnings(ecx(fit, ecx_val = 50, type = "relative",
                              resolution = 100, posterior = TRUE))
  # Each draw's curve on ecx()'s grid, measured from its control towards
  # exp(bot) and interpolated linearly between the bracketing grid points.
  grid <- bnec_newdata(fit, resolution = 100)
  pe <- posterior_epred(fit, newdata = grid, re_formula = NA)
  target <- pe[, 1] - (pe[, 1] - exp(bot)) * 0.5
  by_hand <- vapply(seq_len(nrow(pe)), function(i) {
    k <- which(pe[i, ] <= target[i])[1]
    if (is.na(k) || k == 1) return(NA_real_)
    x0 <- grid$x[k - 1]
    x0 + (target[i] - pe[i, k - 1]) / (pe[i, k] - pe[i, k - 1]) *
      (grid$x[k] - x0)
  }, numeric(1))
  expect_false(anyNA(by_hand))
  expect_equal(as.numeric(est), by_hand, tolerance = 1e-3)
})

test_that("a linear equation under a log link is measured towards 0", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  # ecxlin has no bot and is linear on the log scale, so its mean tends to 0.
  set.seed(578)
  x <- rep(c(0, 2.5, 5, 10, 20, 40), each = 8)
  mu <- exp(log(20) - 0.12 * x)
  y <- stats::rgamma(length(x), shape = 50, rate = 50 / mu)
  fit <- suppressWarnings(suppressMessages(bnec(
    y ~ crf(x, "ecxlin"), data = data.frame(x = x, y = y),
    family = Gamma(link = "log"), chains = 2, iter = 1000, seed = 578,
    refresh = 0
  )))
  expect_equal(bayesnec:::ecx_asymptote(fit, "relative"), 0)
  far <- posterior_epred(fit, newdata = data.frame(x = 200), re_formula = NA)
  expect_lt(stats::median(far), 0.01)
})
