gl_sd_columns <- c("model", "wi", "group", "parameter", "Estimate", "Q2.5",
                   "Q97.5")

# Two equations with a pgl() term, so each equation has a deviation on each of
# its own parameters. Fitted once per worker.
gl_sd_fixture <- local({
  fit <- NULL
  function() {
    if (is.null(fit)) {
      set.seed(529)
      d <- expand.grid(x = seq(-2, 3, length.out = 6), g = letters[1:5],
                       rep = 1:2)
      dev <- stats::setNames(stats::rnorm(5, 0, 3), letters[1:5])
      d$y <- 10 + 80 / (1 + exp((d$x - 0.5) * 2)) + dev[as.character(d$g)] +
        stats::rnorm(nrow(d), 0, 1)
      fit <<- suppressWarnings(suppressMessages(
        bnec(y ~ crf(x, c("ecx4param", "ecxlin")) + pgl(g), data = d,
             family = gaussian(), chains = 2, iter = 1000, seed = 529,
             refresh = 0)
      ))
    }
    fit
  }
})

test_that("a fit with no group-level term gives an empty table and a message", {
  expect_message(out <- group_level_sd(manec_example),
                 "No equation of this fit has a group-level term")
  expect_s3_class(out, "data.frame")
  expect_named(out, gl_sd_columns)
  expect_equal(nrow(out), 0)
  expect_message(out <- group_level_sd(nec4param),
                 "No equation of this fit has a group-level term")
  expect_named(out, gl_sd_columns)
  expect_equal(nrow(out), 0)
})

test_that("group_level_sd refuses an unsupported class and names VarCorr", {
  expect_error(group_level_sd(joint_split()), "brms::VarCorr\\(x\\$fit\\)")
  expect_error(group_level_sd(list()), "requires a bayesnecfit")
})

test_that("group_level_sd gives one row per equation, grouping and parameter", {
  skip_on_cran()
  fit <- gl_sd_fixture()
  out <- group_level_sd(fit)
  expect_named(out, gl_sd_columns)
  expect_identical(unique(out$model), fit$success_models)
  expect_identical(unique(out$group), "g")
  expect_setequal(out$parameter[out$model == "ecx4param"],
                  c("top", "bot", "ec50", "beta"))
  expect_setequal(out$parameter[out$model == "ecxlin"], c("top", "slope"))
  expect_equal(out$wi, fit$mod_stats[out$model, "wi"])
  expect_true(all(out$Q2.5 <= out$Estimate & out$Estimate <= out$Q97.5))
  # The gaussian family has a residual sigma, which VarCorr() also returns.
  expect_false("residual__" %in% out$group)
  # Estimate is the posterior median of the brms parameter.
  draws <- brms::as_draws_matrix(fit$mod_fits$ecx4param$fit,
                                 variable = "sd_g__ec50_Intercept")
  row <- out[out$model == "ecx4param" & out$parameter == "ec50", ]
  expect_equal(row$Estimate, stats::median(draws))
  expect_equal(c(row$Q2.5, row$Q97.5),
               unname(stats::quantile(draws, c(0.025, 0.975))))
})

test_that("a single equation is given a weight of 1", {
  skip_on_cran()
  fit <- gl_sd_fixture()
  one <- suppressMessages(pull_out(fit, model = "ecx4param"))
  out <- group_level_sd(one)
  set_rows <- group_level_sd(fit)
  set_rows <- set_rows[set_rows$model == "ecx4param", ]
  expect_equal(out$wi, rep(1, nrow(out)))
  expect_equal(out[, -2], set_rows[, -2], ignore_attr = TRUE)
})

test_that("a grouped fit gives one table with a leading level column", {
  skip_on_cran()
  fit <- gl_sd_fixture()
  one <- suppressMessages(pull_out(fit, model = "ecxlin"))
  grouped <- structure(list(fits = list(a = fit, b = one),
                            levels = c("a", "b")),
                       class = c("bayesnecgroupfit", "bnecfit"))
  out <- group_level_sd(grouped)
  expect_named(out, c("level", gl_sd_columns))
  expect_equal(nrow(out), nrow(group_level_sd(fit)) + nrow(group_level_sd(one)))
  expect_identical(out$level[out$model == "ecxlin"], c("a", "a", "b", "b"))
  expect_equal(out$wi[out$level == "b"], c(1, 1))
})

test_that("a hurdle fit gives one table per component", {
  skip_on_cran()
  fit <- gl_sd_fixture()
  hurdle <- structure(list(growth = fit, survival = manec_example),
                      class = c("bayesnechurdlefit", "bnecfit"))
  expect_message(out <- group_level_sd(hurdle),
                 "The survival component has no group-level term")
  expect_named(out, c("growth", "survival"))
  expect_equal(out$growth, group_level_sd(fit))
  expect_equal(nrow(out$survival), 0)
})
