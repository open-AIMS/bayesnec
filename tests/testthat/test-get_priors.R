test_that("get_priors returns only what bnec accepts back", {
  out <- get_priors(nec4param)
  expect_s3_class(out, "brmsprior")
  # The four curve parameters, once each. brms also stores a duplicated
  # coef = "Intercept" row per parameter and its own sigma default; handing
  # those back is what makes pull_prior() output unusable as a prior argument.
  expect_setequal(out$nlpar, c("top", "bot", "beta", "nec"))
  expect_equal(nrow(out), 4)
  expect_true(all(out$class == "b"))
  expect_false(any(out$coef == "Intercept"))
  expect_false("sigma" %in% out$class)
  # An absent bound is NA, as define_prior() writes it, not "".
  expect_true(all(is.na(out$lb[out$nlpar != "nec"])))
  # nec keeps the bounds it was given.
  expect_false(is.na(out$lb[out$nlpar == "nec"]))
  expect_false(is.na(out$ub[out$nlpar == "nec"]))
})

test_that("get_priors on a model set is a named list bnec can take", {
  out <- get_priors(manec_example)
  expect_type(out, "list")
  expect_named(out, names(manec_example$mod_fits))
  for (p in out) {
    expect_s3_class(p, "brmsprior")
  }
})

test_that("get_priors builds priors from a formula and data without fitting", {
  out <- get_priors(y ~ crf(x, "nec4param"), data = nec_data,
                    family = gaussian())
  expect_s3_class(out, "brmsprior")
  expect_setequal(out$nlpar, c("top", "bot", "beta", "nec"))
  # The three ways of naming a formula agree.
  expect_equal(
    out,
    get_priors(bnf(y ~ crf(x, "nec4param")), data = nec_data,
               family = gaussian())
  )
  expect_equal(
    out,
    get_priors("y ~ crf(x, \"nec4param\")", data = nec_data,
               family = gaussian())
  )
  # A model set comes back named, as bnec expects a named prior list.
  many <- get_priors(bnf(y ~ crf(x, c("nec4param", "ecx4param"))),
                     data = nec_data, family = gaussian())
  expect_named(many, c("nec4param", "ecx4param"))
  # The family is chosen from the data when not supplied, exactly as bnec does.
  expect_s3_class(get_priors(y ~ crf(x, "nec4param"), data = nec_data),
                  "brmsprior")
})

test_that("get_priors rejects what it cannot build priors from", {
  expect_error(get_priors(y ~ crf(x, "nec4param")), "`data` is required")
  expect_error(get_priors(1:3), "fitted by bnec")
  expect_error(get_priors(list()), "fitted by bnec")
  # A model invalid for the family is dropped, as it is at fit time.
  expect_error(
    get_priors(y ~ crf(x, "nec3param"), data = nec_data,
               family = gaussian()) |> suppressMessages(),
    "None of the model"
  )
})

test_that("get_priors splits the two components of a hurdle fit", {
  obj <- structure(list(growth = nec4param, survival = manec_example),
                   class = c("bayesnechurdlefit", "bnecfit"))
  out <- get_priors(obj)
  expect_named(out, c("growth", "survival"))
  expect_s3_class(out$growth, "brmsprior")
  expect_named(out$survival, names(manec_example$mod_fits))
})

test_that("a blank prior bound is read as absent", {
  # brms records an absent bound as "" in the prior a fitted object carries,
  # where define_prior() and brms::prior() write NA. All mean unbounded; before
  # this the "" form took a different path through make_inits() and errored
  # with "missing value where TRUE/FALSE needed".
  pr <- data.frame(prior = "normal(0, 1)", class = "b", nlpar = "top",
                   lb = "", ub = "", stringsAsFactors = FALSE)
  out <- bayesnec:::blank_bounds_to_na(pr)
  expect_true(is.na(out$lb))
  expect_true(is.na(out$ub))
  # An existing bound and an existing NA are both left alone.
  pr2 <- data.frame(prior = "gamma(5, 2)", class = "b", nlpar = "nec",
                    lb = "0.1", ub = NA_character_, stringsAsFactors = FALSE)
  out2 <- bayesnec:::blank_bounds_to_na(pr2)
  expect_equal(out2$lb, "0.1")
  expect_true(is.na(out2$ub))
  # And a prior carrying blank bounds -- the form a fitted object stores --
  # now produces inits rather than erroring.
  stored <- data.frame(
    prior = c("normal(1, 1)", "normal(0, 5)", "normal(0.5, 1)", "gamma(5, 2)"),
    class = "b", coef = "", group = "", resp = "", dpar = "",
    nlpar = c("top", "beta", "bot", "nec"), lb = "", ub = "",
    stringsAsFactors = FALSE
  )
  expect_true(all(!nzchar(stored$lb)))
  inits <- bayesnec:::make_inits(
    "nec4param", c("b_top", "b_beta", "b_bot", "b_nec"),
    priors = stored, chains = 2
  )
  expect_length(inits, 2)
  expect_setequal(names(inits[[1]]), c("b_top", "b_beta", "b_bot", "b_nec"))
})

test_that("get_priors round trips through bnec", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  fit <- bnec(y ~ crf(x, "nec4param"), data = nec_data, family = gaussian(),
              iter = 400, warmup = 200, chains = 2, seed = 141,
              refresh = 0, open_progress = FALSE) |>
    suppressMessages() |>
    suppressWarnings()
  key <- c("prior", "class", "nlpar", "lb", "ub")
  ord <- function(p) {
    p <- p[order(p$nlpar), key, drop = FALSE]
    rownames(p) <- NULL
    p
  }
  from_fit <- get_priors(fit)
  from_data <- get_priors(y ~ crf(x, "nec4param"), data = nec_data,
                          family = gaussian())
  # With no user override the two entry points answer the same question the
  # same way. This is what makes the formula-and-data form a usable preview.
  expect_equal(ord(from_fit), ord(from_data))
  # The round trip is the point of the issue: a returned object that looks
  # right but is not accepted by bnec(prior = ) would fail it.
  again <- bnec(y ~ crf(x, "nec4param"), data = nec_data, family = gaussian(),
                prior = from_fit, iter = 400, warmup = 200, chains = 2,
                seed = 141, refresh = 0, open_progress = FALSE) |>
    suppressMessages() |>
    suppressWarnings()
  expect_s3_class(again, "bayesnecfit")
  expect_equal(ord(get_priors(again)), ord(from_fit))
  # pull_prior(), by contrast, returns the whole brmsprior a fit carries and is
  # not accepted -- the difference the two functions exist to keep straight.
  whole <- pull_prior(fit)[[1]]
  expect_gt(nrow(whole), nrow(from_fit))
  expect_true("sigma" %in% whole$class)
})

test_that("a user prior makes the two entry points disagree", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  own <- get_priors(y ~ crf(x, "nec4param"), data = nec_data,
                    family = gaussian())
  own$prior[own$nlpar == "top"] <- "normal(0.8, 0.2)"
  fit <- bnec(y ~ crf(x, "nec4param"), data = nec_data, family = gaussian(),
              prior = own, iter = 400, warmup = 200, chains = 2, seed = 141,
              refresh = 0, open_progress = FALSE) |>
    suppressMessages() |>
    suppressWarnings()
  got <- get_priors(fit)
  # The fit reports what it used, override included ...
  expect_equal(got$prior[got$nlpar == "top"], "normal(0.8, 0.2)")
  # ... which is no longer what the defaults would generate.
  generated <- get_priors(y ~ crf(x, "nec4param"), data = nec_data,
                          family = gaussian())
  expect_false(identical(sort(got$prior), sort(generated$prior)))
})
