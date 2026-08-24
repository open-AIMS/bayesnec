test_that("the unscaled power hormesis models are dropped for 0-1 bounded families", {
  # nechormepwr and nechorme4pwr carry the hormesis term x^(1/(1 + exp(slope))),
  # which has no coefficient. At x = 1 it contributes exactly 1 whatever slope
  # is, so below the threshold -- where the decay factor is exactly 1 -- the
  # fitted mean is at least top + 1. No parameter value keeps that inside
  # (0, 1) for a predictor that reaches 1. See #177.
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
  # The zero-probability block of a joint fit is 0-1 bounded whatever the
  # response family is, so a model that cannot be held inside (0, 1) cannot be
  # used for it. This is the "9 of 11 rather than 11" the issue reports.
  for (fam_tag in c("hurdle_gamma", "zero_inflated_beta")) {
    got <- suppressMessages(
      check_models(models()$all, validate_family(fam_tag))
    )
    expect_false("nechormepwr" %in% got)
    expect_false("nechorme4pwr" %in% got)
  }
  # zero_bounded under hurdle_gamma is now an honest count.
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
  # The evidence behind the exclusion, kept as a test so that a future change to
  # the init search is measured against it rather than assumed to have fixed it.
  fam <- validate_family(bernoulli(link = "identity"))
  found <- function(model, x, y) {
    pr <- define_prior(model, fam, x, y)
    ii <- suppressMessages(
      bayesnec:::make_good_inits(model, x, y, n_trials = 200, seed = 1,
                                 priors = pr, chains = 2)
    )
    !(length(ii) == 1 && identical(ii$random, "random"))
  }
  y <- nec_data$y
  x_over_one <- nec_data$x                                  # reaches 3.22
  x_under_one <- nec_data$x / max(nec_data$x) * 0.9         # stays below 1
  expect_false(found("nechormepwr", x_over_one, y))
  expect_false(found("nechorme4pwr", x_over_one, y))
  # Not a defect of the search: the same models initialise when the predictor
  # never reaches 1, which is the only case the equation can represent. That is
  # a property of the units the predictor happens to be in, not of the model,
  # which is why the exclusion is by family rather than conditional on the data.
  expect_true(found("nechormepwr", x_under_one, y))
  expect_true(found("nechorme4pwr", x_under_one, y))
  # The scaled siblings initialise either way.
  expect_true(found("nechorme", x_over_one, y))
  expect_true(found("nechormepwr01", x_over_one, y))
})
