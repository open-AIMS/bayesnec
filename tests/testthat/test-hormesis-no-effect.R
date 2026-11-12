skip_unless_full_suite()

test_that("hormesis breakpoints are not classified as no-effect estimates", {
  hormesis <- intersect(bayesnec:::mod_groups$nec,
                        bayesnec:::mod_groups$hormesis)
  expect_setequal(hormesis, c("nechorme", "nechorme4", "neclinhorme",
                                 "nechormepwr", "nechorme4pwr",
                                 "nechormepwr01"))
  expect_identical(bayesnec:::hormesis_threshold_models(), hormesis)
  expect_false(any(vapply(hormesis,
                          bayesnec:::sampled_nec_is_no_effect, logical(1))))
  expect_true(bayesnec:::sampled_nec_is_no_effect("nec4param"))
  expect_false(bayesnec:::sampled_nec_is_no_effect("ecxflat"))
})

# A fit saved before the change stores the nec parameter of a hormesis
# equation as its no-effect estimate. Relabelling a monotone fixture as a
# hormesis equation reproduces that record without fitting.
saved_hormesis_fit <- function(model = "neclinhorme") {
  out <- nec4param
  out$model <- model
  out
}

# The same for a set: nec4param relabelled as nechorme, with no stored types.
saved_hormesis_set <- function() {
  out <- manec_example
  names(out$mod_fits)[names(out$mod_fits) == "nec4param"] <- "nechorme"
  out$mod_fits$nechorme$model <- "nechorme"
  out$success_models[out$success_models == "nec4param"] <- "nechorme"
  out
}

is_stale <- function(w) grepl("amend(x)", w, fixed = TRUE)

test_that("a saved hormesis fit warns, and keeps the type of its draws", {
  stale <- saved_hormesis_fit()
  expect_identical(bayesnec:::stale_ne_models(stale), "neclinhorme")
  expect_false(bayesnec:::needs_ne_rebuild(nec4param))
  expect_warning(out <- nec(stale), "amend\\(x\\)",
                 class = "bayesnec_stale_ne")
  # The stored draws are the nec parameter, so they keep the NEC label.
  expect_equal(as.numeric(out), as.numeric(nec(nec4param)))
  expect_identical(attr(out, "toxicity_estimate"), "nec")
  expect_identical(bayesnec:::necfit_ne_type(stale), "NEC")
  # The fixture also fails the Rhat check in summary(), so collect every
  # warning and look for this one.
  got <- collect_warnings(summary(stale))
  expect_identical(sum(is_stale(got$warnings)), 1L)
  expect_identical(rownames(got$value$nec_vals), "NEC")
  got <- collect_warnings(ggbnec_data(stale))
  expect_identical(sum(is_stale(got$warnings)), 1L)

  # An object saved before ne_type was recorded is stale too, and its draws
  # are still the sampled parameter.
  untyped <- stale
  untyped$ne_type <- NULL
  expect_true(bayesnec:::needs_ne_rebuild(untyped))
  expect_identical(bayesnec:::necfit_ne_type(untyped), "NEC")

  current <- stale
  current$ne_type <- "NSEC"
  expect_false(bayesnec:::needs_ne_rebuild(current))
  expect_identical(bayesnec:::necfit_ne_type(current), "NSEC")
  expect_identical(attr(nec(current), "toxicity_estimate"), "nsec")
  expect_identical(attr(nec(current, posterior = TRUE), "toxicity_estimate"),
                   "nsec")
})

test_that("a saved hormesis set labels its draws by the rule that built them", {
  stale <- saved_hormesis_set()
  expect_false(bayesnec:::needs_ne_rebuild(manec_example))
  expect_identical(bayesnec:::stale_ne_models(stale), "nechorme")
  # The stored mixture took the nec parameter of nechorme as a NEC.
  expect_identical(bayesnec:::manec_ne_types(stale), c("NEC", "NSEC"))
  expect_warning(out <- suppressMessages(nec(stale)),
                 class = "bayesnec_stale_ne")
  expect_equal(as.numeric(out),
               as.numeric(suppressMessages(nec(manec_example))))
  # The mixture holds NEC and NSEC draws, as summary() labels it.
  expect_identical(attr(out, "toxicity_estimate"), "n(s)ec")

  stale$mod_fits$nechorme$ne_type <- "NSEC"
  stale$mod_fits$ecx4param$ne_type <- "NSEC"
  expect_false(bayesnec:::needs_ne_rebuild(stale))
  expect_identical(bayesnec:::manec_ne_types(stale), c("NSEC", "NSEC"))
})

test_that("a hurdle fit with a saved hormesis component warns once", {
  stale <- saved_hormesis_fit()
  pair <- structure(
    list(growth = stale, survival = nec4param, data = nec4param$fit$data,
         formula = stale$bayesnecformula, y_var = "y",
         n_exposed = nrow(nec4param$fit$data), n_dead = 0L),
    class = c("bayesnechurdlefit", "bnecfit")
  )
  expect_identical(bayesnec:::stale_ne_models(pair), "neclinhorme")
  got <- collect_warnings(suppressMessages(nec(pair)))
  expect_identical(sum(is_stale(got$warnings)), 1L)
  got <- collect_warnings(suppressMessages(summary(pair)))
  expect_identical(sum(is_stale(got$warnings)), 1L)
  # A component that `which` does not read is not checked.
  got <- collect_warnings(suppressMessages(nec(pair, which = "growth")))
  expect_identical(sum(is_stale(got$warnings)), 1L)
  got <- collect_warnings(suppressMessages(nec(pair, which = "survival")))
  expect_identical(sum(is_stale(got$warnings)), 0L)
  pair$growth <- nec4param
  expect_false(bayesnec:::needs_ne_rebuild(pair))
})

test_that("a group with saved hormesis levels warns once and amends each", {
  group <- bayesnec:::allot_class(
    list(fits = list(a = saved_hormesis_fit(), b = saved_hormesis_set()),
         group_var = "g", levels = c("a", "b"), n = c(100L, 100L)),
    c("bayesnecgroupfit", "bnecfit")
  )
  expect_setequal(bayesnec:::stale_ne_models(group),
                  c("neclinhorme", "nechorme"))
  got <- collect_warnings(suppressMessages(nec(group)))
  expect_identical(sum(is_stale(got$warnings)), 1L)
  got <- collect_warnings(suppressMessages(summary(group)))
  expect_identical(sum(is_stale(got$warnings)), 1L)
  got <- collect_warnings(suppressMessages(ggbnec_data(group)))
  expect_identical(sum(is_stale(got$warnings)), 1L)

  seen <- list()
  local_mocked_bindings(
    amend = function(object, ...) {
      seen[[length(seen) + 1]] <<- list(...)
      object$ne_type <- "NSEC"
      for (m in names(object$mod_fits)) object$mod_fits[[m]]$ne_type <- "NSEC"
      object
    },
    .package = "bayesnec"
  )
  out <- bayesnec:::amend.bayesnecgroupfit(group, sig_val = 0.2)
  expect_length(seen, 2)
  expect_identical(vapply(seen, `[[`, numeric(1), "sig_val"), c(0.2, 0.2))
  expect_s3_class(out, "bayesnecgroupfit")
  expect_false(bayesnec:::needs_ne_rebuild(out))
})

test_that("an observed-level NSEC takes the caller's curve arguments", {
  seen <- NULL
  local_mocked_bindings(
    observed_level_spec = function(object) list(combinations = 1),
    estimate_observed_levels = function(object, spec, what, fun, ...) {
      fun(object, "own", NA)
    },
    nsec = function(object, ...) {
      seen <<- list(...)
      1
    },
    .package = "bayesnec"
  )
  fit <- saved_hormesis_fit()
  attr(fit, "bnec_expand_args") <- list(sig_val = 0.2, resolution = 40,
                                        x_range = c(0.1, 3))
  args <- c("sig_val", "resolution", "x_range")
  nec(fit)
  expect_identical(seen[args], list(sig_val = 0.2, resolution = 40,
                                    x_range = c(0.1, 3)))
  nec(fit, sig_val = 0.05, resolution = 30, x_range = c(0, 3))
  expect_identical(seen[args], list(sig_val = 0.05, resolution = 30,
                                    x_range = c(0, 3)))
  # An NA x_range, as compare_posterior() passes by default, means none.
  nec(fit, x_range = NA)
  expect_identical(seen$x_range, c(0.1, 3))
})

test_that("extrapolation re-reads an NSEC at the recorded sig_val", {
  seen <- NULL
  local_mocked_bindings(
    extrapolated_necfit_ne = function(object, lims, bounds, sig_val,
                                      resolution) {
      seen <<- sig_val
      object$ne_posterior
    },
    extrapolated_manec_ne = function(object, lims, bounds, sig_val,
                                     resolution) {
      seen <<- sig_val
      object$w_ne_posterior
    },
    .package = "bayesnec"
  )
  fit <- nec4param
  attr(fit, "bnec_expand_args") <- list(sig_val = 0.2)
  suppressWarnings(suppressMessages(nec(fit, extrapolate = c(0, 10))))
  expect_identical(seen, 0.2)
  suppressWarnings(suppressMessages(
    nec(fit, extrapolate = c(0, 10), sig_val = 0.05)
  ))
  expect_identical(seen, 0.05)
  set <- manec_example
  attr(set, "bnec_expand_args") <- list(sig_val = 0.2)
  suppressWarnings(suppressMessages(nec(set, extrapolate = c(0, 10))))
  expect_identical(seen, 0.2)
})

# One set fitted once for the tests below. Its neclinhorme equation is pulled
# out for the single-fit tests, so no equation is compiled twice.
hormesis_set_cache <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      set.seed(570)
      x <- rep(c(0, 0.25, 0.5, 0.75, 1, 1.5, 2, 3), each = 8)
      y <- 1 + 0.35 * x - 0.8 * pmax(x - 1, 0) + rnorm(length(x), 0, 0.04)
      cached <<- suppressWarnings(suppressMessages(bnec(
        y ~ crf(x, model = c("nec4param", "neclinhorme", "ecx4param")),
        data = data.frame(x, y), family = gaussian(),
        chains = 2, cores = 2, iter = 800, warmup = 400,
        control = list(adapt_delta = 0.99), refresh = 0, seed = 1
      )))
    }
    cached
  }
})

hormesis_single <- function() {
  suppressWarnings(suppressMessages(
    pull_out(hormesis_set_cache(), model = "neclinhorme")
  ))
}

test_that("a hormesis threshold fit reports its declining-limb NSEC", {
  skip_on_cran()
  fit <- hormesis_single()
  x <- fit$fit$data$x

  expect_identical(fit$ne_type, "NSEC")
  peak <- curve_params(fit)
  peak <- peak$Estimate[peak$parameter == "nec"]
  reported <- nec(fit)
  expect_gt(unname(reported[1]), peak)
  expect_identical(attr(reported, "toxicity_estimate"), "nsec")
  expect_equal(as.numeric(reported),
               as.numeric(nsec(fit, resolution = 1000)), tolerance = 0.03)
  expect_gt(unname(ecnsec(fit, nsec = reported[1])[1]), 0)
  expect_identical(rownames(summary(fit, check_fit = FALSE)$nec_vals), "NSEC")
  expect_error(nec(fit, extrapolate = TRUE), "read off a fitted curve")

  wide_range <- c(-0.25, max(x))
  pre <- list(fit = fit$fit, model = fit$model,
              bayesnecformula = fit$bayesnecformula)
  wide <- bayesnec:::allot_class(
    suppressWarnings(suppressMessages(bayesnec:::expand_nec(
      pre, pre$bayesnecformula, x_range = wide_range, resolution = 300,
      model = fit$model
    ))), c("bayesnecfit", "bnecfit")
  )
  expect_equal(as.numeric(suppressWarnings(nec(wide))),
               as.numeric(suppressWarnings(nsec(
                 wide, x_range = wide_range, resolution = 300
               ))), tolerance = 0.01)
  expect_equal(attr(wide$ne_posterior, "censored")$lower, min(x))
})

test_that("a model average with a hormesis equation has no negative ECNSEC", {
  skip_on_cran()
  set <- hormesis_set_cache()
  models <- c("nec4param", "neclinhorme", "ecx4param")
  expect_setequal(set$success_models, models)
  types <- setNames(bayesnec:::manec_component_types(set), set$success_models)
  expect_identical(unname(types[models]), c("NEC", "NSEC", "NSEC"))
  expect_identical(set$ne_type, "N(S)EC")
  expect_error(suppressMessages(nec(set, extrapolate = TRUE)),
               "neclinhorme.*read off a fitted curve")
  reported <- suppressMessages(nec(set))
  expect_identical(attr(reported, "toxicity_estimate"), "n(s)ec")
  expect_gte(unname(ecnsec(set, nsec = reported[1])[1]), 0)
})

test_that("amend() rebuilds a saved hormesis fit and set", {
  skip_on_cran()
  fit <- hormesis_single()
  # The record of a fit saved before the change: the nec parameter, labelled
  # NEC.
  old <- fit
  old$ne_type <- "NEC"
  draws <- as.numeric(brms::as_draws_df(old$fit)[["b_nec_Intercept"]])
  grid <- range(old$pred_vals$data$x)
  attr(draws, "censored") <- bayesnec:::censoring_record(
    grid[2], grid[1], draws >= grid[2], draws <= grid[1]
  )
  old$ne_posterior <- draws
  old$ne <- bayesnec:::estimates_summary(draws)
  expect_warning(old_nec <- nec(old), class = "bayesnec_stale_ne")
  expect_lt(unname(old_nec[1]), unname(nec(fit)[1]))
  rebuilt <- suppressWarnings(suppressMessages(amend(old)))
  expect_false(bayesnec:::needs_ne_rebuild(rebuilt))
  expect_identical(rebuilt$ne_type, "NSEC")
  expect_equal(nec(rebuilt), nec(fit))

  set <- hormesis_set_cache()
  stale <- set
  for (m in names(stale$mod_fits)) stale$mod_fits[[m]]$ne_type <- NULL
  expect_identical(bayesnec:::stale_ne_models(stale), "neclinhorme")
  expect_warning(suppressMessages(summary(stale, check_fit = FALSE)),
                 class = "bayesnec_stale_ne")
  expect_warning(suppressMessages(ggbnec_data(stale)),
                 class = "bayesnec_stale_ne")
  rebuilt <- suppressWarnings(suppressMessages(amend(stale)))
  expect_false(bayesnec:::needs_ne_rebuild(rebuilt))
  expect_identical(bayesnec:::manec_component_types(rebuilt),
                   bayesnec:::manec_component_types(set))

  # A group whose levels are the saved fit and set warns once, and amend()
  # rebuilds every level.
  group <- bayesnec:::allot_class(
    list(fits = list(a = old, b = stale), group_var = "g",
         levels = c("a", "b"), n = c(64L, 64L)),
    c("bayesnecgroupfit", "bnecfit")
  )
  got <- collect_warnings(suppressMessages(nec(group)))
  expect_identical(sum(is_stale(got$warnings)), 1L)
  rebuilt <- suppressWarnings(suppressMessages(amend(group)))
  expect_false(bayesnec:::needs_ne_rebuild(rebuilt))
  expect_equal(nec(rebuilt$fits$a), nec(fit))

  # A saved set of threshold equations only was labelled NEC. Its message and
  # summary agree with that label until the set is rebuilt.
  two <- suppressWarnings(suppressMessages(amend(set, drop = "ecx4param")))
  expect_identical(two$ne_type, "N(S)EC")
  for (m in names(two$mod_fits)) two$mod_fits[[m]]$ne_type <- NULL
  two$ne_type <- "NEC"
  msgs <- testthat::capture_messages(suppressWarnings(nec(two)))
  expect_false(any(grepl("N(S)EC", msgs, fixed = TRUE)))
  expect_identical(bayesnec:::manec_ne_types(two), c("NEC", "NEC"))
  expect_identical(rownames(suppressWarnings(suppressMessages(
    summary(two, check_fit = FALSE)
  ))$nec_vals), "NEC")
})

test_that("extrapolating a hormesis NSEC keeps the sig_val it was built with", {
  skip_on_cran()
  fit <- suppressWarnings(suppressMessages(
    amend(hormesis_single(), sig_val = 0.2, x_range = c(0, 1.6))
  ))
  expect_identical(bayesnec:::recorded_expand_args(list(fit))$sig_val, 0.2)
  expect_true(bayesnec:::has_censoring(attr(fit$ne_posterior, "censored")))
  wider <- suppressWarnings(suppressMessages(nec(fit, extrapolate = c(0, 3))))
  direct <- suppressWarnings(nsec(fit, sig_val = 0.2, x_range = c(0, 3),
                                  resolution = 1000))
  expect_equal(as.numeric(wider), as.numeric(direct), tolerance = 1e-6)
  at_default <- suppressWarnings(nsec(fit, sig_val = 0.01, x_range = c(0, 3),
                                      resolution = 1000))
  expect_false(isTRUE(all.equal(as.numeric(wider), as.numeric(at_default))))
})
