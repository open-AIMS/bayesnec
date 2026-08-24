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

test_that("a disp variance function's priors come back with the curve's", {
  # bnec() takes a supplied prior whole, so a set returned without the
  # parameters a route B disp term adds would leave brms to put a flat prior on
  # each -- which define_disp_prior() exists to avoid, c0 and the slope being
  # near-confounded without it. They are part of the model's own non-linear
  # formula and must round trip with the rest.
  out <- get_priors(y ~ crf(x, "nec4param") + disp("power"), data = nec_data,
                    family = gaussian())
  expect_setequal(out$nlpar, c("top", "bot", "beta", "nec", "c0", "c1"))
  # Route A is an ordinary distributional formula, left to the brms defaults,
  # so it adds nothing to the set.
  route_a <- get_priors(y ~ crf(x, "nec4param") + disp(~x), data = nec_data,
                        family = gaussian())
  expect_setequal(route_a$nlpar, c("top", "bot", "beta", "nec"))
})

test_that("the fit entry point keeps disp parameters and drops brms defaults", {
  # The stored form of a fitted route B model: bayesnec's own rows, the
  # vectorized duplicates brms adds, and a default brms would have chosen.
  # Asserted on the stored object rather than on a fit so that the filter is
  # covered without paying for a disp model to be compiled and sampled.
  stored <- data.frame(
    prior = c("normal(0, 5)", "normal(0, 5)", "normal(-1.515, 2)",
              "normal(-1.515, 2)", "normal(0, 2)", "normal(0, 2)",
              "student_t(3, 0, 2.5)"),
    class = c(rep("b", 6), "sigma"),
    coef = c("", "Intercept", "", "Intercept", "", "Intercept", ""),
    group = "", resp = "", dpar = "",
    nlpar = c("top", "top", "c0", "c0", "c1", "c1", ""),
    lb = "", ub = "",
    source = c("user", "(vectorized)", "user", "(vectorized)", "user",
               "(vectorized)", "default"),
    stringsAsFactors = FALSE
  )
  out <- bayesnec:::usable_prior(stored)
  expect_setequal(out$nlpar, c("top", "c0", "c1"))
  expect_false("sigma" %in% out$class)
  expect_false(any(out$coef == "Intercept"))
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
  skip_on_cran()
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
  skip_on_cran()
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

# #231: `zi` is a genuine class-"zi" brms parameter for the zero-inflated COUNT
# families, which bayesnec fits as a single block -- unlike hurdle_gamma and
# zero_inflated_beta, whose second block carries class "b" with a prefixed
# nlpar. usable_prior() dropped it, so get_priors() reported a set that was
# silently incomplete for the one parameter those families exist to estimate.

test_that("a user prior on zi round trips for a zero-inflated count family", {
  set.seed(1)
  x <- as.numeric(rep(1:10, each = 5))
  y <- as.numeric(rpois(50, 20) * rbinom(50, 1, 0.6))
  gp <- get_priors(y ~ crf(x, "nec4param"), data = data.frame(x = x, y = y),
                   family = "zero_inflated_poisson")
  with_zi <- gp + brms::prior_string("beta(2, 5)", class = "zi")
  out <- bayesnec:::usable_prior(with_zi)
  expect_true("zi" %in% out$class)
  expect_identical(out$prior[out$class == "zi"], "beta(2, 5)")
  # the curve's own rows are untouched
  expect_setequal(out$nlpar[out$class == "b"], c("beta", "top", "bot", "nec"))
})

test_that("a brms default on zi is still dropped", {
  # source == "user" is what keeps this safe: reporting a prior brms chose for
  # itself would suggest bayesnec had made a choice it did not make.
  pr <- data.frame(prior = c("normal(0, 5)", "beta(1, 1)"),
                   class = c("b", "zi"), coef = "", group = "", resp = "",
                   dpar = "", nlpar = c("beta", ""), lb = NA_character_,
                   ub = NA_character_, source = c("user", "default"),
                   stringsAsFactors = FALSE)
  out <- bayesnec:::usable_prior(pr)
  expect_false("zi" %in% out$class)
})

test_that("a zi prior does not disturb the initial-value search", {
  # make_inits() filters to class "b", so the zi row reaches brm() but plays no
  # part in the init search and gets no initial value -- Stan draws it.
  set.seed(2)
  x <- as.numeric(rep(1:10, each = 5))
  y <- as.numeric(rpois(50, 20) * rbinom(50, 1, 0.6))
  gp <- get_priors(y ~ crf(x, "nec4param"), data = data.frame(x = x, y = y),
                   family = "zero_inflated_poisson")
  with_zi <- gp + brms::prior_string("beta(2, 5)", class = "zi")
  fct_args <- c("b_top", "b_beta", "b_bot", "b_nec")
  out <- bayesnec:::make_inits("nec4param", fct_args, with_zi, chains = 2)
  expect_length(out, 2)
  expect_setequal(names(out[[1]]), fct_args)
  expect_false(any(grepl("zi", names(out[[1]]))))
})

test_that("the two-block families are unaffected", {
  # zero_inflated_beta carries its second block as class "b" with a prefixed
  # nlpar, so adding "zi" to the kept classes is a no-op there rather than a
  # conflict. Asserted so the no-op claim in auxiliary_classes() is not just an
  # assertion in a comment.
  pr <- data.frame(prior = rep("normal(0, 5)", 3), class = "b", coef = "",
                   group = "", resp = "", dpar = "",
                   nlpar = c("top", "zitop", "zinec"), lb = NA_character_,
                   ub = NA_character_, source = "user",
                   stringsAsFactors = FALSE)
  out <- bayesnec:::usable_prior(pr)
  expect_equal(nrow(out), 3)
  expect_setequal(out$nlpar, c("top", "zitop", "zinec"))
})

# --- #245: a group-level sd row is part of the record ------------------------
# bayesnec now generates an sd prior, so leaving it out of usable_prior() would
# make get_priors() a record of everything except the parameter a grouped model
# is hardest to get right -- the same reasoning #207 and #231 applied to the
# dispersion and mixing parameters.

test_that("a group-level sd prior round trips", {
  set.seed(245)
  x <- as.numeric(rep(1:10, each = 5))
  # declining, so check_data() has no cause to warn about the direction
  y <- plogis(2 - 0.4 * x + rnorm(50, 0, 0.2))
  d <- data.frame(x = x, y = y, tank = factor(rep(1:10, 5)))
  gp <- suppressMessages(
    get_priors(y ~ crf(x, "nec4param") + ogl(tank), data = d,
               family = Beta(link = "identity"))
  )
  expect_true("sd" %in% gp$class)
  out <- bayesnec:::usable_prior(gp)
  expect_true("sd" %in% out$class)
  expect_identical(out$prior[out$class == "sd"],
                   gp$prior[gp$class == "sd"])
  # the curve's own rows, and the ogl offset, come back untouched
  expect_setequal(out$nlpar[out$class == "b"],
                  c("beta", "top", "bot", "nec", "ogl"))
})

test_that("a brms default on sd is still dropped", {
  # As for zi: reporting a prior brms chose for itself would suggest bayesnec
  # had made a choice it did not make. This is the row the bug left behind.
  pr <- data.frame(prior = c("normal(0, 5)", "student_t(3, 0, 2.5)"),
                   class = c("b", "sd"), coef = "", group = c("", "tank"),
                   resp = "", dpar = "", nlpar = c("beta", "ogl"),
                   lb = NA_character_, ub = NA_character_,
                   source = c("user", "default"), stringsAsFactors = FALSE)
  out <- bayesnec:::usable_prior(pr)
  expect_false("sd" %in% out$class)
})
