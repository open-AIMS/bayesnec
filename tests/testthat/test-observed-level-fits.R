observed_fit_data <- function() {
  set.seed(554)
  d <- expand.grid(x = c(0.05, 0.15, 0.4, 0.8, 1.5, 3),
                   plate = paste0("p", 1:3), replicate = 1:3)
  d <- d[!(d$plate == "p3" & d$x == 0.05), , drop = FALSE]
  d <- rbind(d, data.frame(x = rep(1, 3), plate = "p4",
                           replicate = 1:3))
  gain <- c(p1 = 0.85, p2 = 1, p3 = 1.2, p4 = 1.1)
  d$y <- 3 * gain[as.character(d$plate)] *
    exp(-0.35 * pmax(d$x - 0.6, 0)) + stats::rnorm(nrow(d), 0, 0.12)
  d$plate <- as.character(d$plate)
  d
}

observed_fit_cache <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      d <- observed_fit_data()
      cached <<- suppressWarnings(suppressMessages(bnec(
        y ~ crf(x, c("nec3param", "ecx4param")) + ogl(plate),
        data = d, family = "gaussian", sig_val = 0.2,
        iter = 300, warmup = 150, chains = 2, seed = 554,
        refresh = 0, resolution = 40
      )))
    }
    cached
  }
})

logged_fit_cache <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      cached <<- suppressWarnings(suppressMessages(bnec(
        y ~ crf(log(x), "ecx4param") + ogl(plate),
        data = observed_fit_data(), family = "gaussian",
        sig_val = 0.2, iter = 300, warmup = 150,
        chains = 2, seed = 557, refresh = 0, resolution = 40
      )))
    }
    cached
  }
})

hurdle_fit_cache <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      d <- observed_fit_data()
      d$y[d$x == 3 & d$replicate <= 2] <- 0
      d$y[d$x == 1.5 & d$replicate == 1] <- 0
      cached <<- suppressWarnings(suppressMessages(bnec_hurdle(
        y ~ crf(x, c("nec3param", "ecx4param")) + pgl(plate), data = d,
        model_survival = "nec3param",
        iter = 300, warmup = 150, chains = 2, seed = 555,
        refresh = 0, resolution = 40
      )))
    }
    cached
  }
})

joint_fit_cache <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      cached <<- suppressWarnings(suppressMessages(bnec_joint(
        hurdle_fit_cache(), model = "nec3param",
        model_survival = "nec3param", iter = 300, warmup = 150,
        chains = 2, seed = 556, refresh = 0, resolution = 40
      )))
    }
    cached
  }
})

# Two toxicants, each tested on three plates of its own, refitted jointly with
# a plate term for each toxicant, as the copper and zinc fits of example8 are.
toxicant_joint_cache <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      d <- observed_fit_data()
      d <- d[d$plate != "p4", , drop = FALSE]
      d_b <- d
      d_b$y <- d_b$y * 0.7
      d$tox <- "a"
      d_b$tox <- "b"
      d$plate <- paste0("a", d$plate)
      d_b$plate <- paste0("b", d_b$plate)
      d <- rbind(d, d_b)
      d$tox <- factor(d$tox)
      grouped <- suppressWarnings(suppressMessages(bnec_group(
        y ~ crf(x, "ecx4param") + ogl(plate), data = d, group_var = "tox",
        family = "gaussian", iter = 300, warmup = 150, chains = 2,
        seed = 558, refresh = 0, resolution = 40
      )))
      cached <<- suppressWarnings(suppressMessages(bnec_joint(
        grouped, model = "ecx4param",
        formula = y ~ crf(x, "ecx4param") + ogl(gr(plate, by = tox)),
        iter = 300, warmup = 150, chains = 2, seed = 558, refresh = 0
      )))
    }
    cached
  }
})

test_that("fitted spanning levels exclude one-concentration levels", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  fit <- observed_fit_cache()
  spec <- bayesnec:::observed_level_spec(fit)
  expect_equal(spec$excluded, "plate=p4")
  expect_equal(nrow(spec$combinations), 3L)
  expect_equal(sum(spec$n), nrow(fit$mod_fits[[1]]$fit$data) - 3L)

  per <- suppressMessages(ecx(fit, pool = "none", resolution = 30))
  expect_equal(nrow(per), 3L)
  expect_false(any(grepl("p4", per$level)))
  shared <- suppressWarnings(suppressMessages(ecx(
    fit, pool = "none", x_control = "shared", resolution = 30
  )))
  expect_equal(unname(attr(per, "x_control")["plate=p3"]), 0.15)
  expect_equal(unname(attr(shared, "x_control")["plate=p3"]), 0.05)
  mix <- suppressMessages(ecx(fit, posterior = TRUE, resolution = 30))
  expect_true(all(is.finite(mix[!is.na(mix)])))
  expect_equal(length(mix), fit$sample_size)
  expect_null(names(mix))
  expect_true(all(is.finite(suppressMessages(nsec(fit, resolution = 30)))))
  expect_equal(nrow(suppressMessages(ecnsec(
    fit, nsec = 0.8, pool = "none", resolution = 30
  ))), 3L)
  expect_equal(nrow(suppressWarnings(suppressMessages(
    nec(fit, pool = "none")
  ))), 3L)
  expect_true(is.finite(unname(suppressMessages(ecx(
    fit, pool = "mean_estimate", resolution = 30
  ))[1])))
  expect_true(is.finite(unname(suppressMessages(ecx(
    fit, pool = "mean_curve", resolution = 30
  ))[1])))
})

test_that("a pooled estimate holds each level's own draw at its position", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  fit <- suppressMessages(pull_out(observed_fit_cache(), model = "ecx4param"))
  per_level <- suppressMessages(ecx(fit, pool = "none", posterior = TRUE,
                                    resolution = 30))
  pooled <- suppressMessages(ecx(fit, posterior = TRUE, resolution = 30))
  level <- attr(pooled, "level")
  own <- vapply(seq_along(pooled), function(d) {
    as.numeric(per_level[[level[d]]][d])
  }, numeric(1))
  expect_equal(as.numeric(pooled), own)

  spec <- bayesnec:::observed_level_spec(fit)
  nd <- bayesnec:::newdata_eval(fit, resolution = 5, x_range = NA)$newdata
  band <- bayesnec:::observed_grid_posterior(fit$fit, fit$bayesnecformula, nd,
                                             spec)
  n <- brms::ndraws(fit$fit)
  idx <- bayesnec:::mixture_draw_indices(
    rep(n, nrow(spec$combinations)), spec$n, n,
    labels = bayesnec:::level_labels(spec$combinations)
  )
  for (i in seq_along(idx)) {
    nd_i <- nd
    nd_i$plate <- spec$combinations$plate[i]
    full <- brms::posterior_epred(fit$fit, newdata = nd_i,
                                  re_formula = spec$re_formula)
    expect_equal(band[idx[[i]], ], full[idx[[i]], ])
  }
})

test_that("the toxicant mixtures of a joint refit pair by posterior draw", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  fit <- toxicant_joint_cache()
  pooled <- suppressMessages(ecx(fit, posterior = TRUE, resolution = 30))
  expect_named(pooled, c("a", "b"))
  n <- brms::ndraws(fit$fit)
  expect_equal(attr(pooled$a, "draw_id"), seq_len(n))
  expect_equal(attr(pooled$b, "draw_id"), seq_len(n))
  for (tox in c("a", "b")) {
    level <- bayesnec:::joint_level_fit(fit, tox)
    per_plate <- suppressMessages(ecx(level, pool = "none", posterior = TRUE,
                                      resolution = 30))
    plate <- attr(pooled[[tox]], "level")
    own <- vapply(seq_len(n), function(d) {
      as.numeric(per_plate[[plate[d]]][d])
    }, numeric(1))
    # Position d holds draw d of the plate it was given to.
    expect_equal(as.numeric(pooled[[tox]]), own)
  }
  # Each toxicant's plates are given draws independently of the other's, so
  # the first plate of one is not paired with the first plate of the other.
  rank_a <- as.integer(factor(attr(pooled$a, "level")))
  rank_b <- as.integer(factor(attr(pooled$b, "level")))
  expect_lt(mean(rank_a == rank_b), 0.6)
})

test_that("stored smooth no-effect draws use the recorded significance", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  fit <- observed_fit_cache()
  smooth <- suppressMessages(pull_out(fit, model = "ecx4param"))
  direct <- suppressMessages(nsec(smooth, sig_val = 0.2,
                                  resolution = 40, posterior = TRUE,
                                  xform = identity))
  expect_equal(as.numeric(smooth$ne_posterior), as.numeric(direct),
               tolerance = 1e-6)
  other <- suppressMessages(nsec(smooth, sig_val = 0.01,
                                 resolution = 40, posterior = TRUE,
                                 xform = identity))
  expect_false(isTRUE(all.equal(as.numeric(direct), as.numeric(other))))
  expect_true(isTRUE(smooth$pred_vals$observed_levels))
  mixture <- suppressMessages(ecx(smooth, posterior = TRUE,
                                  resolution = 30))
  shares <- table(attr(mixture, "level")) / length(mixture)
  spec <- bayesnec:::observed_level_spec(smooth)
  expect_equal(as.numeric(shares), spec$n / sum(spec$n),
               tolerance = 1 / length(mixture))
  stale <- smooth
  stale$pred_vals$observed_levels <- NULL
  expect_false(bayesnec:::needs_observed_rebuild(smooth))
  expect_true(bayesnec:::needs_observed_rebuild(stale))
  expect_warning(summary(stale), "saved before observed-level")
  rebuilt <- suppressWarnings(suppressMessages(amend(
    stale, resolution = 40
  )))
  expect_true(isTRUE(rebuilt$pred_vals$observed_levels))
})

test_that("logged predictors store no-effect draws on the fitted scale", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  fit <- logged_fit_cache()
  fitted <- suppressMessages(nsec(fit, sig_val = 0.2,
                                   posterior = TRUE, xform = identity,
                                   resolution = 40))
  recorded <- suppressMessages(nsec(fit, sig_val = 0.2,
                                     posterior = TRUE, resolution = 40))
  expect_equal(as.numeric(fit$ne_posterior), as.numeric(fitted),
               tolerance = 1e-6)
  expect_equal(exp(as.numeric(fitted)), as.numeric(recorded),
               tolerance = 1e-6)
  warnings <- character()
  error <- withCallingHandlers(
    tryCatch(ecx(fit, x_control = 0, resolution = 20),
             error = conditionMessage),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_match(error, "not finite under the crf\\(\\) predictor transformation")
  expect_length(warnings, 0L)
})

test_that("hurdle endpoints and band read observed levels", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  fit <- hurdle_fit_cache()
  spec <- bayesnec:::observed_level_spec(fit)
  expect_equal(spec$excluded, "plate=p4")
  expect_equal(nrow(spec$combinations), 3L)
  for (fun in list(
    function() ecx(fit, pool = "none", resolution = 25),
    function() nsec(fit, pool = "none", resolution = 25),
    function() ecnsec(fit, nsec = 0.8, pool = "none", resolution = 25),
    function() nec(fit, pool = "none")
  )) {
    expect_equal(nrow(suppressWarnings(suppressMessages(fun()))), 3L)
  }
  mixed <- suppressWarnings(suppressMessages(ecx(
    fit, posterior = TRUE, resolution = 25
  )))
  expect_true(all(is.finite(mixed[!is.na(mixed)])))
  expect_true(is.finite(unname(suppressWarnings(suppressMessages(ecx(
    fit, pool = "mean_estimate", resolution = 25
  )))[1])))
  shared <- suppressWarnings(suppressMessages(ecx(
    fit, pool = "none", x_control = "shared", resolution = 25
  )))
  expect_equal(unname(attr(shared, "x_control")["plate=p3"]), 0.05)
  band <- bayesnec:::hurdle_component_preds(fit, resolution = 12)
  expect_equal(dim(band$combined)[2], 12L)
  expect_equal(band$combined, band$growth * band$survival)
  level <- bayesnec:::observed_level_fit(fit, spec, 1)
  level_band <- bayesnec:::hurdle_component_preds(level, resolution = 12)
  expect_false(isTRUE(all.equal(band$combined, level_band$combined)))
  expect_true(is.finite(unname(suppressWarnings(suppressMessages(ecx(
    fit, pool = "mean_curve", resolution = 25
  )))[1])))
})

test_that("a joint refit reads its spanning group levels", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  fit <- joint_fit_cache()
  spec <- bayesnec:::observed_level_spec(fit)
  expect_equal(spec$excluded, "plate=p4")
  expect_equal(nrow(spec$combinations), 3L)
  expect_equal(nrow(suppressWarnings(suppressMessages(ecx(
    fit, pool = "none", resolution = 25
  )))), 3L)
  expect_equal(nrow(suppressWarnings(suppressMessages(nsec(
    fit, pool = "none", resolution = 25
  )))), 3L)
  expect_equal(nrow(suppressWarnings(suppressMessages(nec(
    fit, pool = "none"
  )))), 3L)
  level <- bayesnec:::observed_level_fit(fit, spec, 1, with_ne = TRUE)
  nd <- bayesnec:::newdata_eval(level, 2, NA)$newdata[1, , drop = FALSE]
  mu <- brms::posterior_linpred(
    fit$fit, newdata = nd, nlpar = "nec",
    re_formula = spec$re_formula, transform = TRUE
  )[, 1]
  hu <- brms::posterior_linpred(
    fit$fit, newdata = nd,
    nlpar = paste0(bayesnec:::hurdle_dpar(fit$fit$family), "nec"),
    re_formula = spec$re_formula, transform = TRUE
  )[, 1]
  expect_equal(as.numeric(level$ne_posterior), as.numeric(pmin(mu, hu)))
})
