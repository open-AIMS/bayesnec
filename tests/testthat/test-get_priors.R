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
  # Compare stored bounds after converting absent bounds from empty strings
  # to NA. This tests the round trip without assuming that the fixture uses
  # the current default priors.
  stored <- as.data.frame(nec4param$fit$prior)
  stored <- bayesnec:::blank_bounds_to_na(stored)
  stored <- stored[stored$class == "b" & stored$nlpar == "nec" &
                     nzchar(stored$prior), ]
  expect_equal(nrow(stored), 1L)
  expect_equal(out$lb[out$nlpar == "nec"], stored$lb)
  expect_equal(out$ub[out$nlpar == "nec"], stored$ub)
  # Require at least one bound so the round-trip assertion is not vacuous.
  expect_false(all(is.na(c(stored$lb, stored$ub))))
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
  # Model sets need named prior lists for bnec().
  many <- get_priors(bnf(y ~ crf(x, c("nec4param", "ecx4param"))),
                     data = nec_data, family = gaussian())
  expect_named(many, c("nec4param", "ecx4param"))
  expect_s3_class(get_priors(y ~ crf(x, "nec4param"), data = nec_data),
                  "brmsprior")
})

test_that("nec3param under gaussian now returns priors (#206)", {
  # An identity-link Gaussian fit can use a zero-bounded curve.
  out <- suppressMessages(
    get_priors(y ~ crf(x, "nec3param"), data = nec_data, family = gaussian())
  )
  expect_s3_class(out, "brmsprior")
  expect_setequal(out$nlpar[nzchar(out$nlpar)], c("top", "beta", "nec"))
})

test_that("get_priors rejects what it cannot build priors from", {
  expect_error(get_priors(y ~ crf(x, "nec4param")), "`data` is required")
  expect_error(get_priors(1:3), "fitted by bnec")
  expect_error(get_priors(list()), "fitted by bnec")
  # check_models() drops every zero-bounded equation under a log link,
  # whatever the data, so a set of nec3param alone is refused.
  expect_error(
    get_priors(y ~ crf(x, "nec3param"), data = nec_data,
               family = gaussian(link = "log")) |> suppressMessages(),
    "None of the model"
  )
})

test_that("a disp variance function's priors come back with the curve's", {
  # Return the variance-function coefficients with the curve's priors. A set
  # without c0 and c1 is incomplete, and bnec() refills them with a warning.
  out <- get_priors(y ~ crf(x, "nec4param") + disp("power"), data = nec_data,
                    family = gaussian())
  expect_setequal(out$nlpar, c("top", "bot", "beta", "nec", "c0", "c1"))
  # Route A is an ordinary distributional formula, left to the brms defaults,
  # so it adds nothing to the set.
  route_a <- get_priors(y ~ crf(x, "nec4param") + disp(~x), data = nec_data,
                        family = gaussian())
  expect_setequal(route_a$nlpar, c("top", "bot", "beta", "nec"))
})

test_that("get_priors reports the range of the disp() exponent (#472)", {
  # The bounds are part of the prior a fit uses, so a user reading the defaults
  # has to see them, and a set passed back through `prior` has to keep them.
  ranges <- list(gaussian = c(0, 1), Gamma = c(0, 2))
  for (fam in names(ranges)) {
    out <- suppressMessages(get_priors(
      y ~ crf(x, "nec4param") + disp("power"), data = nec_data,
      family = fam
    ))
    c1 <- out[out$nlpar == "c1", ]
    expect_identical(c1$prior, "normal(0, 2)")
    expect_equal(as.numeric(c(c1$lb, c1$ub)), ranges[[fam]], label = fam)
  }
})

test_that("the fit entry point keeps disp parameters and drops brms defaults", {
  # Construct the stored prior table directly to test filtering of brms
  # defaults and vectorised duplicates without compiling a dispersion model.
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
  # brms stores an absent bound as an empty string; prior construction uses
  # NA. Both representations must be treated as unbounded.
  pr <- data.frame(prior = "normal(0, 1)", class = "b", nlpar = "top",
                   lb = "", ub = "", stringsAsFactors = FALSE)
  out <- bayesnec:::blank_bounds_to_na(pr)
  expect_true(is.na(out$lb))
  expect_true(is.na(out$ub))
  pr2 <- data.frame(prior = "gamma(5, 2)", class = "b", nlpar = "nec",
                    lb = "0.1", ub = NA_character_, stringsAsFactors = FALSE)
  out2 <- bayesnec:::blank_bounds_to_na(pr2)
  expect_equal(out2$lb, "0.1")
  expect_true(is.na(out2$ub))
  # Initialisation must also accept the stored empty-string representation.
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
              refresh = 0) |>
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
  expect_equal(ord(from_fit), ord(from_data))
  # Pass the returned object back to bnec() to check usability, not just shape.
  again <- bnec(y ~ crf(x, "nec4param"), data = nec_data, family = gaussian(),
                prior = from_fit, iter = 400, warmup = 200, chains = 2,
                seed = 141, refresh = 0) |>
    suppressMessages() |>
    suppressWarnings()
  expect_s3_class(again, "bayesnecfit")
  expect_equal(ord(get_priors(again)), ord(from_fit))
  # pull_prior() exposes the full brms table, including rows bnec() cannot
  # reuse.
  whole <- pull_prior(fit)[[1]]
  expect_gt(nrow(whole), nrow(from_fit))
  expect_true("sigma" %in% whole$class)
})

test_that("get_priors honours an explicit predictor scale (#317)", {
  x <- rep(log(c(1, 3, 10, 30, 100)), each = 4)
  d <- data.frame(x = x, y = seq(8, 2, length.out = length(x)))
  automatic <- get_priors(y ~ crf(x, "nec3param"), data = d,
                          family = gaussian())
  logged <- get_priors(y ~ crf(x, "nec3param"), data = d,
                       family = gaussian(), predictor_scale = "log")
  auto_nec <- automatic$prior[automatic$nlpar == "nec"]
  log_nec <- logged$prior[logged$nlpar == "nec"]

  expect_match(auto_nec, "^lognormal\\(")
  expect_match(log_nec, "^normal\\(")
  # The logged normal prior has no bounds; logged concentrations may be
  # negative.
  expect_true(is.na(logged$lb[logged$nlpar == "nec"]))
  expect_true(is.na(logged$ub[logged$nlpar == "nec"]))
  expect_equal(as.numeric(automatic$lb[automatic$nlpar == "nec"]), 0)
  expect_true(is.na(automatic$ub[automatic$nlpar == "nec"]))
})

test_that("a user prior makes the two entry points disagree", {
  skip_on_cran()
  own <- get_priors(y ~ crf(x, "nec4param"), data = nec_data,
                    family = gaussian())
  own$prior[own$nlpar == "top"] <- "normal(0.8, 0.2)"
  fit <- bnec(y ~ crf(x, "nec4param"), data = nec_data, family = gaussian(),
              prior = own, iter = 400, warmup = 200, chains = 2, seed = 141,
              refresh = 0) |>
    suppressMessages() |>
    suppressWarnings()
  got <- get_priors(fit)
  expect_equal(got$prior[got$nlpar == "top"], "normal(0.8, 0.2)")
  generated <- get_priors(y ~ crf(x, "nec4param"), data = nec_data,
                          family = gaussian())
  expect_false(identical(sort(got$prior), sort(generated$prior)))
})

# For zero-inflated counts, keep a user prior on brms class zi. The joint
# hurdle_gamma and zero_inflated_beta fits instead give their second block
# class b with a prefixed nlpar.

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
  expect_setequal(out$nlpar[out$class == "b"], c("beta", "top", "bot", "nec"))
})

test_that("a brms default on zi is still dropped", {
  # Keep only user rows so the output does not imply that bayesnec chose
  # a brms default.
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
  # The zero-inflated-beta second block already uses class b and prefixed
  # parameter names; retaining class zi must not change it.
  pr <- data.frame(prior = rep("normal(0, 5)", 3), class = "b", coef = "",
                   group = "", resp = "", dpar = "",
                   nlpar = c("top", "zitop", "zinec"), lb = NA_character_,
                   ub = NA_character_, source = "user",
                   stringsAsFactors = FALSE)
  out <- bayesnec:::usable_prior(pr)
  expect_equal(nrow(out), 3)
  expect_setequal(out$nlpar, c("top", "zitop", "zinec"))
})

# The returned prior set must include the group-level standard deviation.

test_that("a group-level sd prior round trips", {
  set.seed(245)
  x <- as.numeric(rep(1:10, each = 5))
  # Use a declining response to avoid a direction warning in this prior test.
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
  expect_setequal(out$nlpar[out$class == "b"],
                  c("beta", "top", "bot", "nec", "ogl"))
})

test_that("a brms default on sd is still dropped", {
  # A brms default must not be reported as a bayesnec prior choice.
  pr <- data.frame(prior = c("normal(0, 5)", "student_t(3, 0, 2.5)"),
                   class = c("b", "sd"), coef = "", group = c("", "tank"),
                   resp = "", dpar = "", nlpar = c("beta", "ogl"),
                   lb = NA_character_, ub = NA_character_,
                   source = c("user", "default"), stringsAsFactors = FALSE)
  out <- bayesnec:::usable_prior(pr)
  expect_false("sd" %in% out$class)
})

test_that("nec is bounded by the support of its prior, not by the series", {
  # The lognormal support sets lb = 0 regardless of the lowest tested dose.
  # An upper bound at the highest dose would wrongly exclude higher thresholds.
  d <- data.frame(x = rep(c(0, 1, 10, 100), each = 5),
                  y = rep(c(8, 6, 3, 1), each = 5))
  pr <- suppressMessages(
    get_priors(y ~ crf(x, model = "nec3param"), data = d,
               family = Gamma(link = "identity"))
  )
  expect_match(pr$prior[pr$nlpar == "nec"], "^lognormal\\(")
  expect_equal(as.numeric(pr$lb[pr$nlpar == "nec"]), 0)
  expect_true(is.na(pr$ub[pr$nlpar == "nec"]))
  # Omit a zero control to distinguish distributional support from min(x).
  d2 <- data.frame(x = rep(c(1, 10, 100), each = 5),
                   y = rep(c(8, 3, 1), each = 5))
  pr2 <- suppressMessages(
    get_priors(y ~ crf(x, model = "nec3param"), data = d2,
               family = Gamma(link = "identity"))
  )
  expect_equal(as.numeric(pr2$lb[pr2$nlpar == "nec"]), 0)
  expect_true(is.na(pr2$ub[pr2$nlpar == "nec"]))
})

test_that("get_priors reports the substitution its priors are built from", {
  # Report response substitutions at this entry point before returning priors
  # derived from the substituted response.
  d <- data.frame(x = rep(c(0, 1, 10, 100), each = 5),
                  y = rep(c(8, 6, 3, 0), each = 5))
  expect_message(
    get_priors(y ~ crf(x, model = "nec3param"), data = d,
               family = Gamma(link = "identity")),
    "have been shifted"
  )
  # Reported once for the set, not once per member.
  msgs <- capture.output(
    invisible(get_priors(y ~ crf(x, model = c("nec3param", "nec4param")),
                         data = d, family = Gamma(link = "identity"))),
    type = "message"
  )
  expect_equal(sum(grepl("have been shifted", msgs)), 1)
  # This route produces priors, not a bnec_record.
  expect_false(any(grepl("bnec_record", msgs)))
  expect_true(any(grepl("priors below are derived", msgs)))
})

test_that("a response at one bound is refused by name, not by quantile() (#400)", {
  # Refuse an all-bound response before prior construction reaches NaN
  # quantiles.
  cases <- at_bound_cases()
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    msgs <- character(0)
    err <- tryCatch(
      withCallingHandlers(
        get_priors(cs$formula, data = cs$data, family = cs$family),
        message = function(m) {
          msgs <<- c(msgs, conditionMessage(m))
          invokeRestart("muffleMessage")
        }
      ),
      error = conditionMessage
    )
    expect_match(err, paste0("The response \"", cs$column, "\" is at the ",
                             cs$bound, " bound"), fixed = TRUE, info = nm)
    expect_false(grepl("na.rm", err, fixed = TRUE), info = nm)
    # Raised before the substitution report, which for beta at 1 would
    # otherwise announce a shift to 0.999 that is never made.
    expect_length(msgs, 0)
  }
})

test_that("ecxflat named alone gets its prior at a bound (#419)", {
  # ecxflat has a single level parameter. Shifting the response inside support
  # lets its prior be built even where a curve prior is undefined.
  cases <- at_bound_cases()
  for (nm in setdiff(names(cases), at_bound_refused)) {
    cs <- cases[[nm]]
    f <- stats::update(cs$formula, . ~ crf(x, "ecxflat"))
    # The zero-inflated-beta second block describes the proportion above zero.
    pars <- if (cs$family == "zero_inflated_beta") c("top", "zitop") else "top"
    for (type in c("uninformative", "regularizing")) {
      pr <- suppressMessages(
        get_priors(f, data = cs$data, family = cs$family, prior_type = type)
      )
      expect_s3_class(pr, "brmsprior")
      expect_identical(pr$nlpar, pars, info = paste(nm, type))
      expect_false(anyNA(pr$prior), info = paste(nm, type))
      expect_false(any(grepl("NaN|Inf", pr$prior)), info = paste(nm, type))
    }
  }
  # The regularizing entry is placed at the bound the response sits at.
  one <- suppressMessages(get_priors(
    alive ~ crf(x, "ecxflat"), data = cases$bernoulli_one$data,
    family = "bernoulli", prior_type = "regularizing"
  ))
  zero <- suppressMessages(get_priors(
    alive ~ crf(x, "ecxflat"), data = cases$bernoulli_zero$data,
    family = "bernoulli", prior_type = "regularizing"
  ))
  shapes <- function(p) as.numeric(regmatches(p, gregexpr("[0-9.]+", p))[[1]])
  expect_gt(shapes(one$prior)[1], shapes(one$prior)[2])
  expect_lt(shapes(zero$prior)[1], shapes(zero$prior)[2])
  # Beta at 0 cannot be fitted by any equation and is refused by name.
  bz <- cases$beta_zero
  expect_error(
    suppressMessages(get_priors(cover ~ crf(x, "ecxflat"), data = bz$data,
                                family = bz$family)),
    "A beta distribution cannot represent a zero", fixed = TRUE
  )
  # An all-zero response leaves no data for the positive Beta block.
  zz <- cases$zib_zero
  expect_error(
    suppressMessages(get_priors(cover ~ crf(x, "ecxflat"), data = zz$data,
                                family = zz$family)),
    "no observation of this response is above zero", fixed = TRUE
  )
  # ecxflat beside a curve equation is refused, and the message names ecxflat.
  expect_error(
    get_priors(alive ~ crf(x, c("ecxflat", "nec3param")),
               data = cases$bernoulli_one$data, family = "bernoulli"),
    "is the only one that can be fitted to it", fixed = TRUE
  )
})

test_that("a zero_inflated_beta response of 0 is refused without a warning (#441)", {
  # Refuse an all-zero zero-inflated-beta response before a positive-subset
  # minimum can warn and prior construction continues.
  zz <- at_bound_cases()$zib_zero
  for (f in list(zz$formula, cover ~ crf(x, "ecxflat"))) {
    err <- NULL
    expect_no_warning(
      err <- tryCatch(
        get_priors(f, data = zz$data, family = zz$family),
        error = conditionMessage
      )
    )
    expect_match(err, "lower bound of a zero_inflated_beta response",
                 fixed = TRUE)
    expect_match(err, paste("no observation of this response is above zero,",
                            "so the beta block has no data"), fixed = TRUE)
    expect_false(grepl("non-missing", err, fixed = TRUE))
  }
})

test_that("one observation off the bound still builds priors (#400)", {
  cases <- at_bound_cases()
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    pr <- suppressMessages(
      get_priors(cs$formula, data = cs$near, family = cs$family)
    )
    expect_named(pr, c("nec3param", "ecx4param"))
    for (m in names(pr)) {
      expect_s3_class(pr[[m]], "brmsprior")
      expect_false(anyNA(pr[[m]]$prior), info = paste(nm, m))
    }
  }
})

test_that("a missing value is refused before the bound is read (#400)", {
  # Check missing values first: model.frame() can drop the only zero with a
  # missing predictor and make an incomplete response appear to be all ones.
  x <- rep(c(0.1, 0.5, 1, 3, 10, 30), each = 5)
  d <- data.frame(x = x, alive = 1L)
  d$alive[30] <- 0L
  d$x[30] <- NA
  err <- expect_error(
    get_priors(alive ~ crf(x, "nec3param"), data = d, family = "bernoulli")
  )
  expect_match(conditionMessage(err), "row\\(s\\) with missing values")
  expect_false(grepl("upper bound", conditionMessage(err)))
})

test_that("a binomial formula with no trials() term is refused by name (#442)", {
  # Mock check_data() to ensure the missing-trials refusal occurs before it.
  local_mocked_bindings(
    check_data = function(...) stop("check_data reached"),
    .package = "bayesnec"
  )
  for (fam in c("binomial", "beta_binomial")) {
    err <- expect_error(
      get_priors(count ~ crf(x, c("nec3param", "nec4param")), data = nec_data,
                 family = fam)
    )
    expect_match(conditionMessage(err),
                 paste("The", fam, "family needs the number of trials"),
                 fixed = TRUE, info = fam)
    expect_match(conditionMessage(err), "has no trials() term", fixed = TRUE,
                 info = fam)
  }
})

test_that("a binomial formula with a trials() term still returns priors (#442)", {
  for (fam in c("binomial", "beta_binomial")) {
    out <- suppressWarnings(suppressMessages(
      get_priors(count | trials(trials) ~ crf(x, c("nec3param", "nec4param")),
                 data = nec_data, family = fam)
    ))
    expect_named(out, c("nec3param", "nec4param"))
    expect_s3_class(out$nec3param, "brmsprior")
  }
})


# The message bnec() refuses the call with. The fit is mocked, so nothing is
# fitted if bnec() accepts the call.
bnec_refusal <- function(formula, data, family) {
  local_mocked_bindings(
    fit_bayesnec = function(...) stop("fit reached"),
    .package = "bayesnec"
  )
  tryCatch(suppressWarnings(suppressMessages(
    bnec(formula, data = data, family = family)
  )), error = conditionMessage)
}

test_that("get_priors refuses a disp() term bnec() refuses, before its loop (#445)", {
  # define_prior() is mocked to record whether any prior is built first.
  d <- nec_data
  d$alive <- as.integer(d$y > 0.5)
  d$pos <- d$y * 10
  cases <- list(
    list(lhs = "count", family = "poisson", term = 'disp("power")',
         msg = "Family poisson has no free dispersion parameter"),
    list(lhs = "count", family = "poisson", term = "disp(~x)",
         msg = "Family poisson has no free dispersion parameter"),
    list(lhs = "alive", family = "bernoulli", term = 'disp("power")',
         msg = "Family bernoulli has no free dispersion parameter"),
    list(lhs = "count | trials(trials)", family = "binomial",
         term = "disp(~x)",
         msg = "Family binomial has no free dispersion parameter"),
    list(lhs = "pos", family = "gaussian", term = 'disp("twosided")',
         msg = "Variance function \"twosided\" is not valid for the gaussian"),
    list(lhs = "pos", family = "Gamma", term = 'disp("twosided")',
         msg = "Variance function \"twosided\" is not valid for the Gamma"),
    list(lhs = "count", family = "negbinomial", term = 'disp("twosided")',
         msg = "Variance function \"twosided\" is not valid for the negbinomial")
  )
  built <- 0L
  local_mocked_bindings(
    define_prior = function(...) {
      built <<- built + 1L
      stop("prior built")
    },
    .package = "bayesnec"
  )
  for (cs in cases) {
    f <- stats::as.formula(paste0(cs$lhs,
                                  ' ~ crf(x, c("nec3param", "ecx4param")) + ',
                                  cs$term))
    built <- 0L
    err <- tryCatch(suppressMessages(get_priors(f, data = d,
                                                family = cs$family)),
                    error = conditionMessage)
    label <- paste(cs$family, cs$term)
    expect_match(err, cs$msg, fixed = TRUE, label = label)
    expect_identical(built, 0L, label = label)
    expect_identical(err, bnec_refusal(f, d, cs$family), label = label)
  }
})

test_that("get_priors still returns priors for a disp() term bnec() takes (#445)", {
  d <- nec_data
  d$pos <- d$y * 10
  for (fam in c("gaussian", "Gamma")) {
    out <- suppressMessages(get_priors(pos ~ crf(x, "nec4param") +
                                         disp("power"),
                                       data = d, family = fam))
    expect_setequal(out$nlpar, c("top", "bot", "beta", "nec", "c0", "c1"))
  }
  out <- suppressMessages(get_priors(y ~ crf(x, "nec4param") +
                                       disp("twosided"),
                                     data = d, family = "beta"))
  expect_setequal(out$nlpar,
                  c("top", "bot", "beta", "nec", "c0", "c1", "c2"))
})

test_that("get_priors refuses a non-finite disp(~...) column as bnec() does (#445)", {
  d <- nec_data
  d$w <- d$x
  d$w[3] <- Inf
  f <- y ~ crf(x, "nec3param") + disp(~w)
  err <- tryCatch(suppressMessages(get_priors(f, data = d,
                                              family = "gaussian")),
                  error = conditionMessage)
  expect_identical(err, bnec_refusal(f, d, "gaussian"))
  expect_match(err, '"w"', fixed = TRUE)
})
