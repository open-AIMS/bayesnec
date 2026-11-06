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

test_that("a saved observed-level hormesis fit reads each level's NSEC", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  fit <- suppressMessages(pull_out(observed_fit_cache(), model = "nec3param"))
  # Relabel the threshold fit as a hormesis equation saved before the change,
  # whose stored type is NEC.
  stale <- fit
  stale$model <- "nechorme"
  current <- stale
  current$ne_type <- "NSEC"
  expect_true(bayesnec:::needs_ne_rebuild(stale))
  expect_equal(suppressWarnings(suppressMessages(nec(stale, pool = "none"))),
               suppressWarnings(suppressMessages(nec(current, pool = "none"))))
  expect_false(isTRUE(all.equal(
    suppressWarnings(suppressMessages(nec(stale, pool = "none"))),
    suppressWarnings(suppressMessages(nec(fit, pool = "none")))
  )))
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

test_that("the block panels of a joint refit read its observed levels", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  fit <- joint_fit_cache()
  spec <- bayesnec:::observed_level_spec(fit)
  d <- suppressWarnings(suppressMessages(ggbnec_data(fit, which = "all")))
  grid <- fit$pred_vals$data$x
  nd <- bnec_newdata(fit, resolution = length(grid), x_range = range(grid))
  band <- function(dpar) {
    bayesnec:::observed_grid_posterior(fit$fit, fit$bayesnecformula, nd,
                                       spec, dpar = dpar)
  }
  growth <- band("mu")
  survival <- band("hu")
  # The block bands pair the levels and draws of the stored combined band.
  expect_equal(growth * survival, band(NULL))
  drawn <- function(name) d$y_e[d$panel == name & !is.na(d$y_e)]
  medians <- function(draws) unname(apply(draws, 2, stats::median))
  expect_equal(drawn("Response of survivors"), medians(growth))
  expect_equal(drawn("Probability of survival"), medians(survival))
  expect_equal(drawn("Response per individual exposed"),
               fit$pred_vals$data$Estimate)
  population <- bayesnec:::joint_hurdle_epred(fit, nd, "mu", re_formula = NA)
  expect_false(isTRUE(all.equal(drawn("Response of survivors"),
                                medians(population))))

  # Both blocks are threshold equations, so the stored combined NEC is the
  # smaller of the two block NECs, level by level and draw by draw.
  mu_ne <- bayesnec:::joint_block_ne(fit, "mu")
  hu_ne <- bayesnec:::joint_block_ne(fit, "hu")
  expect_equal(pmin(as.numeric(mu_ne), as.numeric(hu_ne)),
               as.numeric(fit$ne_posterior))
  lines <- function(name) {
    d$nec_vals[d$panel == name & !is.na(d$nec_vals)]
  }
  expect_equal(lines("Probability of survival"),
               unname(as.numeric(bayesnec:::estimates_summary(hu_ne))))
  expect_equal(lines("Response of survivors"),
               unname(as.numeric(bayesnec:::estimates_summary(mu_ne))))

  # With group, the survival panel draws the proportion alive in each level.
  grouped <- suppressWarnings(suppressMessages(
    ggbnec_data(fit, which = "all", add_nec = FALSE, group = "plate")
  ))
  expect_true(attr(grouped, "group_fitted"))
  alive <- grouped[grouped$panel == "Probability of survival" &
                     !is.na(grouped$y_r), , drop = FALSE]
  obs <- fit$fit$data
  expected <- do.call(rbind, lapply(split(obs, obs$plate), function(part) {
    s <- bayesnec:::survival_by_x(part$x, part$y)
    data.frame(plate = part$plate[1], x = s$x, y = s$y)
  }))
  expect_equal(as.character(alive$group), expected$plate)
  expect_equal(alive$x_r, expected$x)
  expect_equal(alive$y_r, expected$y)
  survivors <- grouped[grouped$panel == "Response of survivors" &
                         !is.na(grouped$y_r), , drop = FALSE]
  expect_equal(nrow(survivors), sum(obs$y > 0))
  p <- suppressWarnings(suppressMessages(
    autoplot(fit, which = "all", group = "plate")
  ))
  expect_no_error(ggplot2::ggplot_build(p))
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
