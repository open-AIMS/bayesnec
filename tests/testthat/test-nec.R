test_that("nec returns expected object types", {
  nec_summary <- suppressMessages(nec(manec_example))
  expect_equal(length(nec_summary), 3)
})

test_that("doesn't work for ecx models", {
  expect_error(nec(ecx4param), "nec is not a parameter in ecx model types.")
})
  
test_that("the class is read off the parameters, not the name (#419)", {
  # Decide NEC support from the sampled parameter, not an equation-name
  # substring, which can misclassify renamed smooth or threshold equations.
  renamed_smooth <- ecx4param
  renamed_smooth$model <- "flat"
  expect_error(nec(renamed_smooth),
               "nec is not a parameter in ecx model types.")
  renamed_threshold <- nec4param
  renamed_threshold$model <- "ecxthreshold"
  expect_identical(nec(renamed_threshold), nec(nec4param))
  expect_false(bayesnec:::has_nec_parameter("flat", ecx4param$fit))
  expect_true(bayesnec:::has_nec_parameter("ecxthreshold", nec4param$fit))
  # The equation's formula is read first, and needs no fit: several tests
  # build objects with no brmsfit, and reading the fit alone stopped in
  # fixef() on them.
  expect_true(bayesnec:::has_nec_parameter("nec3param", NULL))
  expect_false(bayesnec:::has_nec_parameter("ecxflat", NULL))
  expect_false(bayesnec:::has_nec_parameter("ecx4param", list()))
  # Only where neither is known does the prefix decide, as it always did.
  expect_true(bayesnec:::has_nec_parameter("threshold", NULL))
  expect_false(bayesnec:::has_nec_parameter("ecxsmooth", NULL))
  # The model-averaged note reads the fits as well: renaming both equations so
  # that neither holds the substring leaves the smooth one smooth.
  renamed_set <- manec_example
  names(renamed_set$mod_fits) <- c("threshold", "smooth")
  expect_message(nec(renamed_set), "model-averaged N\\(S\\)EC")
  only_threshold <- manec_example
  only_threshold$mod_fits <- only_threshold$mod_fits["nec4param"]
  expect_no_message(suppressWarnings(nec(only_threshold)),
                    message = "contains smooth")
})

test_that("works for bayesnecfit", {
  nec1 <- nec(nec4param)
  expect_equal(length(nec1), 3)
  expect_equal(names(nec1), c("Q50", "Q2.5", "Q97.5"))
})

test_that("works for bayesmanecfit", {
  # A mixed model set returns the model-averaged N(S)EC rather than a NEC, and
  # says so; see ?nec.
  nec1 <- suppressMessages(nec(manec_example))
  expect_message(nec(manec_example), "model-averaged N\\(S\\)EC")
  expect_equal(length(nec1), 3)
  expect_equal(names(nec1), c("Q50", "Q2.5", "Q97.5"))
})

test_that("xform passes correctly", {
  nec1 <- nec(nec4param)
  nec2 <- nec(nec4param, xform = exp)
  expect_gt(nec2[1], nec1[2])
})

test_that("posterior passes correctly", {
  nec3 <- nec(nec4param, posterior = TRUE)
  expect_equal(length(nec3), 100)
})

test_that("prob_vals passes correctly", {
  nec4 <- nec(nec4param, prob_vals = c(0.5, 0.3, 0.7))
  expect_equal(names(nec4), c("Q50", "Q30", "Q70"))
})


# Censored no-effect posteriors

# Inject unidentified draws to test summary handling without fitting a
# non-declining curve. Both a joint smooth-survival component and a mixed
# model set can produce these draws.

test_that("nec summarises a censored posterior rather than erroring", {
  censored <- nec4param
  censored$ne_posterior[c(3, 17)] <- NA_real_
  out <- suppressWarnings(nec(censored))
  expect_equal(length(out), 3)
  expect_false(anyNA(out))
  # The two dropped draws are excluded, not replaced, so the summary is taken
  # over the 98 that are identified.
  expect_equal(as.numeric(out),
               as.numeric(quantile(censored$ne_posterior,
                                   probs = c(0.5, 0.025, 0.975),
                                   na.rm = TRUE)))
})

test_that("nec says how many draws are censored, on either class", {
  censored <- nec4param
  censored$ne_posterior[c(3, 17)] <- NA_real_
  # An unexplained NA is a computation failure, not evidence beyond a
  # bound. Report it separately and exclude it from censoring fractions.
  expect_warning(nec(censored), "could not be computed for 2 of 100 draws")
  censored_manec <- manec_example
  censored_manec$w_ne_posterior[1] <- NA_real_
  expect_warning(suppressMessages(nec(censored_manec)),
                 "not identified for 1 of 100 draws")
})

test_that("an uncensored posterior is silent", {
  expect_silent(nec(nec4param))
})

# The rows of the stored nec prior that hold its bounds.
nec_bound_rows <- function(prior) {
  prior$nlpar == "nec" & prior$class == "b" & nzchar(prior$lb)
}

test_that("a bounded NEC prior reports a concentrated upper tail", {
  fit <- near_bound_fit(9)
  bounds <- bayesnec:::ne_prior_bounds(fit$fit)
  out <- prior_bound_notes(nec(fit))
  expect_length(out$notes, 1)
  expect_match(out$notes, "9 of 100 draws (9%) within the upper 5%",
               fixed = TRUE)
  expect_equal(as.numeric(out$value),
               as.numeric(quantile(fit$ne_posterior, c(0.5, 0.025, 0.975))))
  record <- attr(out$value, "prior_bound_summary")
  upper <- record[record$side == "upper", ]
  expect_equal(upper$share, 0.09)
  expect_equal(upper$n_draws, 100)
  expect_equal(upper$bound, bounds$ub)
  expect_equal(record$share[record$side == "lower"], 0)
})

test_that("an interior NEC posterior is returned and printed as before", {
  fit <- near_bound_fit(6)
  expect_silent(estimate <- nec(fit))
  expect_named(attributes(estimate), c("names", "toxicity_estimate"))
  expect_named(attributes(nec(nec4param)), c("names", "toxicity_estimate"))
})

test_that("draws near the lower bound are reported at that end", {
  fit <- near_bound_fit(12, side = "lower")
  out <- prior_bound_notes(nec(fit))
  expect_match(out$notes, "12 of 100 draws (12%) within the lower 5%",
               fixed = TRUE)
  record <- attr(out$value, "prior_bound_summary")
  expect_equal(record$share[record$side == "lower"], 0.12)
})

test_that("finite draws alone make up the share", {
  fit <- near_bound_fit(9)
  fit$ne_posterior <- c(fit$ne_posterior, NA_real_, Inf)
  record <- attr(suppressWarnings(prior_bound_notes(nec(fit))$value),
                 "prior_bound_summary")
  upper <- record[record$side == "upper", ]
  expect_equal(c(upper$n_near, upper$n_draws), c(9, 100))
})

test_that("one bounded end is measured over the prediction range", {
  # asymptote_observed = FALSE removes the upper bound of the nec prior.
  fit <- nec4param
  prior <- fit$fit$prior
  prior$ub[nec_bound_rows(prior)] <- ""
  fit$fit$prior <- prior
  lb <- bayesnec:::ne_prior_bounds(fit$fit)$lb
  window <- 0.05 * diff(range(fit$pred_vals$data$x))
  fit$ne_posterior <- c(rep(1.5, 85), rep(lb + 0.5 * window, 10),
                        rep(lb + 1.5 * window, 5))
  out <- prior_bound_notes(nec(fit))
  record <- attr(out$value, "prior_bound_summary")
  expect_equal(record$side, "lower")
  expect_equal(record$share, 0.1)
})

test_that("a user-supplied nec prior is read from the fit", {
  fit <- nec4param
  prior <- fit$fit$prior
  rows <- nec_bound_rows(prior)
  prior$lb[rows] <- "0.5"
  prior$ub[rows] <- "2"
  fit$fit$prior <- prior
  fit$ne_posterior <- c(rep(1.2, 80), rep(1.99, 20))
  record <- attr(prior_bound_notes(nec(fit))$value, "prior_bound_summary")
  expect_equal(record$fitted_bound, c(0.5, 2))
  expect_equal(record$share[record$side == "upper"], 0.2)
  # Without a bound there is no window to measure.
  prior$lb[rows] <- ""
  prior$ub[rows] <- ""
  fit$fit$prior <- prior
  expect_silent(estimate <- nec(fit))
  expect_null(attr(estimate, "prior_bound_summary"))
})

test_that("a group-level sd row on nec is not read as a bound", {
  fit <- near_bound_fit(10, side = "lower")
  lb <- bayesnec:::ne_prior_bounds(fit$fit)$lb
  prior <- fit$fit$prior
  # A (nec | g) term adds an sd row on nec, bounded below at zero.
  sd_row <- prior[nec_bound_rows(prior), ]
  sd_row$class <- "sd"
  sd_row$lb <- "0"
  sd_row$ub <- ""
  fit$fit$prior <- rbind(prior, sd_row)
  record <- attr(prior_bound_notes(nec(fit))$value, "prior_bound_summary")
  expect_equal(record$fitted_bound[record$side == "lower"], lb)
  expect_equal(record$share[record$side == "lower"], 0.1)
})

test_that("the bound is given on the scale of the returned estimate", {
  # Relabel the predictor without refitting, so that the stored draws and
  # prior bounds are read as log(x + 1).
  fit <- near_bound_fit(20)
  fit$bayesnecformula <- bnf(y ~ crf(log(x + 1), model = "nec4param"))
  ub <- bayesnec:::ne_prior_bounds(fit$fit)$ub
  out <- prior_bound_notes(nec(fit))
  record <- attr(out$value, "prior_bound_summary")
  upper <- record[record$side == "upper", ]
  expect_equal(upper$fitted_bound, ub)
  expect_equal(upper$bound, exp(ub) - 1)
  expect_gt(upper$bound, out$value[["Q97.5"]])
  expect_match(out$notes, paste0("ends at ", signif(exp(ub) - 1, 3), " (",
                                 signif(ub, 3), " on the fitted scale)"),
               fixed = TRUE)
  fitted <- attr(prior_bound_notes(nec(fit, xform = identity))$value,
                 "prior_bound_summary")
  expect_equal(fitted$bound, fitted$fitted_bound)
})

test_that("a joint fit reads the nec bounds of both blocks", {
  # A two-threshold joint fit reports the per-draw minimum of the two blocks'
  # nec, which can reach the smaller upper bound.
  fit <- nec4param
  fit$fit$family <- brms::hurdle_gamma(link = "identity",
                                       link_hu = "identity")
  prior <- fit$fit$prior
  hu <- prior[nec_bound_rows(prior), ]
  hu$nlpar <- "hunec"
  hu$ub <- "2"
  fit$fit$prior <- rbind(prior, hu)
  fit$hurdle <- list(ne_type = "NEC")
  fit$ne_type <- "NEC"
  fit$ne_posterior <- c(rep(1, 85), rep(1.99, 15))
  out <- prior_bound_notes(suppressMessages(nec(fit)))
  record <- attr(out$value, "prior_bound_summary")
  expect_equal(record$fitted_bound[record$side == "upper"], 2)
  expect_equal(record$share[record$side == "upper"], 0.15)
  # With a smooth second block the estimate is read off the curve.
  fit$hurdle$ne_type <- "N(S)EC"
  fit$ne_type <- "N(S)EC"
  expect_null(attr(suppressMessages(nec(fit)), "prior_bound_summary"))
})

test_that("a hurdle pair keeps the record on its components only", {
  growth <- near_bound_fit(20)
  pair <- structure(list(growth = growth, survival = nec4param,
                         data = nec4param$fit$data,
                         formula = nec4param$bayesnecformula,
                         y_var = "y", n_exposed = 4L, n_dead = 2L),
                    class = c("bayesnechurdlefit", "bnecfit"))
  combined <- prior_bound_notes(suppressMessages(nec(pair)))
  expect_length(combined$notes, 0)
  expect_null(attr(combined$value, "prior_bound_summary"))
  part <- prior_bound_notes(nec(pair$growth))
  expect_length(part$notes, 1)
  expect_false(is.null(attr(part$value, "prior_bound_summary")))
})

test_that("a fitted model set is read through each equation's own draws", {
  # manec_example is as fitted: its mod_fits hold prebayesnecfit objects,
  # which store no no-effect posterior.
  expect_s3_class(manec_example$mod_fits$nec4param, "prebayesnecfit")
  record <- bayesnec:::nec_prior_boundary(manec_example)
  expect_equal(unique(record$model), "nec4param")
  pulled <- suppressMessages(
    bayesnec:::pull_equation(manec_example, "nec4param")
  )
  expect_identical(
    bayesnec:::set_equation_ne_draws(manec_example$mod_fits$nec4param,
                                     "nec4param"),
    as.numeric(pulled$ne_posterior)
  )
  expect_equal(record$n_draws, rep(length(pulled$ne_posterior), 2))
  wi <- manec_example$mod_stats$wi[manec_example$mod_stats$model ==
                                     "nec4param"]
  expect_equal(record$wi, rep(wi, 2))
  # No share reaches the threshold, so nec() returns what it did before.
  out <- suppressMessages(prior_bound_notes(nec(manec_example)))
  expect_length(out$notes, 0)
  expect_named(attributes(out$value), c("names", "toxicity_estimate"))
})

test_that("a model set notes the model-weighted share of its equations", {
  set <- manec_example
  set$mod_fits$nec4param$fit <- tighten_upper_bound(
    set$mod_fits$nec4param$fit, "nec"
  )
  share <- upper_bound_share(set$mod_fits$nec4param$fit, "nec")
  wi <- set$mod_stats$wi[set$mod_stats$model == "nec4param"]
  out <- suppressMessages(prior_bound_notes(nec(set)))
  expect_length(out$notes, 1)
  expect_match(out$notes, "Weighted by model weight")
  expect_match(out$notes, paste0("nec4param: ", signif(100 * share, 2), "%"),
               fixed = TRUE)
  record <- attr(out$value, "prior_bound_summary")
  expect_equal(record$share[record$side == "upper"], share)
  expect_equal(attr(record, "weighted_share")[["upper"]], wi * share)
  expect_equal(as.numeric(out$value),
               as.numeric(suppressMessages(nec(manec_example))))
})

test_that("each level of a group keeps its record and is named", {
  group <- structure(list(fits = list(a = nec4param, b = near_bound_fit(20)),
                          group_var = "site", levels = c("a", "b"),
                          formula = nec4param$bayesnecformula,
                          data = nec4param$fit$data,
                          family = nec4param$fit$family,
                          n = c(50L, 50L)),
                     class = c("bayesnecgroupfit", "bnecfit"))
  out <- prior_bound_notes(nec(group))
  expect_length(out$notes, 1)
  expect_match(out$notes, "^Level b: The nec4param NEC posterior")
  record <- attr(out$value, "prior_bound_summary")
  expect_equal(unique(record$level), "b")
  expect_equal(record$share[record$side == "upper"], 0.2)
})

test_that("a group keeps the weighted share of a model-set level", {
  set <- manec_example
  set$mod_fits$nec4param$fit <- tighten_upper_bound(
    set$mod_fits$nec4param$fit, "nec"
  )
  group <- structure(list(fits = list(a = manec_example, b = set),
                          group_var = "site", levels = c("a", "b"),
                          formula = nec4param$bayesnecformula,
                          data = nec4param$fit$data,
                          family = nec4param$fit$family,
                          n = c(50L, 50L)),
                     class = c("bayesnecgroupfit", "bnecfit"))
  out <- suppressMessages(prior_bound_notes(nec(group)))
  record <- attr(out$value, "prior_bound_summary")
  shares <- attr(record, "weighted_share")
  expect_named(shares, "b")
  expect_equal(shares$b, attr(bayesnec:::nec_prior_boundary(set), "weighted_share"))
})

# No-effect argument validation

test_that("no_effect is refused rather than discarded off a joint refit", {
  # Only joint refits add the NSEC for smooth equations. Other classes reject
  # a supplied no_effect rather than silently absorb it in dots.
  expect_error(nec(nec4param, no_effect = TRUE),
               "`no_effect` adds the curve-read NSEC")
  expect_error(suppressMessages(nec(manec_example, no_effect = TRUE)),
               "bayesmanecfit")
  # Refused on its default value as well: the user typed it, so it was meant.
  expect_error(nec(nec4param, no_effect = FALSE),
               "`no_effect` adds the curve-read NSEC")
})
