test_that("the unscaled power hormesis models are dropped for 0-1 bounded families", {
  # Below nec, the unscaled power increment is one at x = 1 for every slope.
  # The mean then exceeds one for positive top, so successful initial values
  # cannot establish support over all later sampler proposals.
  bounded <- list(
    bernoulli = validate_family(bernoulli(link = "identity")),
    beta = validate_family(Beta(link = "identity")),
    binomial = validate_family(binomial(link = "identity")),
    beta_binomial = validate_family(beta_binomial(link = "identity"))
  )
  for (fam in bounded) {
    got <- suppressMessages(check_models(models()$all, fam))
    expect_false("nechormepwr" %in% got)
    expect_false("nechorme4pwr" %in% got)
    # The sibling hormesis models are unaffected: their increment is scaled.
    expect_true("nechorme" %in% got)
    expect_true("nechorme4" %in% got)
    expect_true("nechormepwr01" %in% got)
  }
  # Unbounded and zero-bounded responses keep them.
  expect_true(
    "nechormepwr" %in%
      suppressMessages(check_models(models()$all,
                                    validate_family(Gamma(link = "identity"))))
  )
  expect_true(
    "nechorme4pwr" %in%
      suppressMessages(check_models(models()$all, validate_family("gaussian")))
  )
})

test_that("the two-block families drop them too", {
  # The zero-probability block is unit bounded regardless of the response
  # family, so these unscaled equations cannot be used for it.
  for (fam_tag in c("hurdle_gamma", "zero_inflated_beta")) {
    got <- suppressMessages(
      check_models(models()$all, validate_family(fam_tag))
    )
    expect_false("nechormepwr" %in% got)
    expect_false("nechorme4pwr" %in% got)
  }
  # Check the filtered zero-bounded group as well as the full equation set.
  zb <- suppressMessages(
    check_models(models()$zero_bounded, validate_family("hurdle_gamma"))
  )
  expect_false("nechormepwr" %in% zb)
  expect_false("nechormepwr01" %in% zb)
})

test_that("the exclusion is named and explained", {
  expect_message(
    check_models(c("nechormepwr", "nec3param"),
                 validate_family(Beta(link = "identity"))),
    "nechormepwr"
  )
  expect_message(
    check_models(c("nechormepwr", "nec3param"),
                 validate_family(Beta(link = "identity"))),
    "no scale parameter"
  )
  # Asking for nothing else is an error rather than an empty set.
  expect_error(
    check_models("nechormepwr", validate_family(Beta(link = "identity"))) |>
      suppressMessages(),
    "None of the model"
  )
})

test_that("initialisation confirms why they are excluded", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # A successful initial draw does not constrain later sampler proposals.
  # Below nec the decay factor is 1, so a concentration >= 1 gives a mean of
  # at least top + 1. The nec prior does not exclude that region.
  fam <- validate_family(bernoulli(link = "identity"))
  search <- function(model, x, y, priors = NULL) {
    if (is.null(priors)) {
      priors <- define_prior(model, fam, x, y)
    }
    ii <- suppressMessages(
      bayesnec:::make_good_inits(model, x, y, family = fam, n_trials = 200,
                                 seed = 1, priors = priors, chains = 2)
    )
    if (length(ii) == 1 && identical(ii$random, "random")) {
      return(NULL)
    }
    vapply(ii, function(z) as.numeric(z$b_nec), numeric(1))
  }
  found <- function(...) !is.null(search(...))
  above_one <- function(model, x, y) {
    pr <- define_prior(model, fam, x, y)
    pr$prior[pr$nlpar == "nec"] <- "uniform(1, 3.22)"
    pr$lb[pr$nlpar == "nec"] <- "1"
    pr
  }
  y <- nec_data$y
  x_over_one <- nec_data$x                                  # reaches 3.22
  x_under_one <- nec_data$x / max(nec_data$x) * 0.9         # stays below 1
  for (model in c("nechormepwr", "nechorme4pwr")) {
    # Confined to nec at or above 1, there is nothing to find.
    expect_false(found(model, x_over_one, y, above_one(model, x_over_one, y)),
                 info = model)
    # Left free, the search succeeds only by placing nec below 1, which is the
    # case the equation can represent and the one the sampler need not stay in.
    nec_found <- search(model, x_over_one, y)
    expect_false(is.null(nec_found), info = model)
    expect_true(all(nec_found < 1), info = model)
  }
  # Rescaling these data below one permits successful initial draws.
  # That avoids this support obstruction through the predictor units; it
  # does not establish a valid unit-interval model over all parameter draws.
  expect_true(found("nechormepwr", x_under_one, y))
  expect_true(found("nechorme4pwr", x_under_one, y))
  # The scaled siblings initialise either way.
  expect_true(found("nechorme", x_over_one, y))
  expect_true(found("nechormepwr01", x_over_one, y))
})
