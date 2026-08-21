pred_a <- rnorm(100)
pred_na <- add_na(pred_a)
resp_a <- 1:100
resp_na <- add_na(resp_a)
pred_b <- pred_a[-1]
resp_b <- resp_a[-1]

test_that("model is always properly specified as character", {
  p_a <- define_prior(model = "nec3param", family = gaussian(),
                      predictor = pred_a, response = resp_a)
  expect_error(define_prior(family = gaussian(),
                            predictor = pred_a, response = resp_a))
  expect_error(define_prior(model = NULL, family = gaussian(),
                            predictor = pred_a, response = resp_a))
  expect_error(define_prior(model = NA, family = gaussian(),
                            predictor = pred_a, response = resp_a))
  expect_error(define_prior(model = FALSE, family = gaussian(),
                            predictor = pred_a, response = resp_a))
  expect_error(define_prior(model = 10, family = gaussian(),
                            predictor = pred_a, response = resp_a))
  expect_error(define_prior(model = "none", family = gaussian(),
                            predictor = pred_a, response = resp_a))
  expect_error(define_prior(model = "all", family = gaussian(),
                            predictor = pred_a, response = resp_a))
  expect_error(define_prior(model = "ecx", family = gaussian(),
                            predictor = pred_a, response = resp_a))
  expect_s3_class(p_a, "brmsprior")
  expect_s3_class(manec_example, "bayesmanecfit")
})

test_that("family is a family object of correct family", {
  expect_error(define_prior(model = "nec3param", family = "gaussian",
                            predictor = pred_a, response = resp_a))
  expect_error(define_prior(model = "nec3param", family = gaussian,
                            predictor = pred_a, response = resp_a))
  expect_error(define_prior(model = "nec3param", family = inverse.gaussian(),
                            predictor = pred_a, response = resp_a))
  expect_s3_class(define_prior(model = "nec3param", family = poisson(),
                               predictor = pred_a, response = resp_a),
                  "brmsprior")
  expect_s3_class(define_prior(model = "nec3param", family = binomial(),
                               predictor = pred_a, response = resp_a/100),
                  "brmsprior")
  expect_error(define_prior(model = "nec3param", family = binomial(),
                               predictor = pred_a, response = resp_a))
  expect_s3_class(define_prior(model = "nec3param", family = Gamma(),
                               predictor = pred_a, response = resp_a),
                  "brmsprior")
})

test_that("either predictor or response contains NA", {
  expect_error(define_prior(model = "nec3param", family = gaussian(),
                            predictor = pred_na, response = resp_a))
  expect_error(define_prior(model = "nec3param", family = gaussian(),
                            predictor = pred_a, response = resp_na))
})

test_that("predictor and response have different lengths", {
  expect_s3_class(define_prior(model = "nec3param", family = gaussian(),
                               predictor = pred_b, response = resp_a),
                  "brmsprior")
  expect_s3_class(define_prior(model = "nec3param", family = gaussian(),
                               predictor = pred_a, response = resp_b),
                  "brmsprior")
})

test_that("check proper output structure", {
  p_a <- define_prior(model = "nec3param", family = gaussian(),
                      predictor = pred_a, response = resp_a)
  p_b <- define_prior(model = "nec4param", family = Beta(link = "logit"),
                      predictor = pred_a, response = rbeta(100, 1, 5))
  p_c <- define_prior(model = "nec4param", family = Beta(link = "identity"),
                      predictor = pred_a, response = rbeta(100, 1, 5))
  expect_identical(sort(p_a$nlpar), c("beta", "nec", "top"))
  expect_true(grepl("normal", p_a$prior[p_a$nlpar == "beta"]))
  expect_true(grepl("normal", p_a$prior[p_a$nlpar == "nec"]))
  expect_true(grepl("normal", p_a$prior[p_a$nlpar == "top"]))
  expect_true(all(is.na(p_b[p_b$nlpar == "top", c("lb", "ub")])))
  expect_true(all(is.na(p_b[p_b$nlpar == "bot", c("lb", "ub")])))
  expect_true(grepl("normal", p_b$prior[p_b$nlpar == "top"]))
  expect_true(grepl("normal", p_b$prior[p_b$nlpar == "bot"]))
  expect_false(all(is.na(p_c[p_c$nlpar == "top", c("lb", "ub")])))
  expect_false(all(is.na(p_c[p_c$nlpar == "bot", c("lb", "ub")])))
  expect_true(grepl("beta", p_c$prior[p_c$nlpar == "top"]))
  expect_true(grepl("beta", p_c$prior[p_c$nlpar == "bot"]))
})

test_that("prior_type selects between default prior sets", {
  resp <- 1:100
  unin <- define_prior(model = "nec4param", family = gaussian(),
                       predictor = pred_a, response = resp,
                       prior_type = "uninformative")
  regu <- define_prior(model = "nec4param", family = gaussian(),
                       predictor = pred_a, response = resp,
                       prior_type = "regularizing")
  deflt <- define_prior(model = "nec4param", family = gaussian(),
                        predictor = pred_a, response = resp)
  # default is the JSS "uninformative" set
  expect_identical(deflt$prior, unin$prior)
  # uninformative top/bot match JSS spec: 90th/10th pct, sd * 2.5
  expect_identical(unin$prior[unin$nlpar == "top"],
                   paste0("normal(", quantile(resp, 0.9), ", ", sd(resp) * 2.5, ")"))
  expect_identical(unin$prior[unin$nlpar == "bot"],
                   paste0("normal(", quantile(resp, 0.1), ", ", sd(resp) * 2.5, ")"))
  # regularizing differs and uses the narrower scaling (extreme pct, sd * 1)
  expect_false(identical(unin$prior, regu$prior))
  expect_identical(regu$prior[regu$nlpar == "top"],
                   paste0("normal(", quantile(resp, 1), ", ", sd(resp), ")"))
  # beta-family asymmetry differs between sets (identity link keeps beta priors)
  rb <- rbeta(100, 1, 5)
  unin_b <- define_prior("nec4param", Beta(link = "identity"), pred_a, rb,
                         prior_type = "uninformative")
  regu_b <- define_prior("nec4param", Beta(link = "identity"), pred_a, rb,
                         prior_type = "regularizing")
  expect_identical(unin_b$prior[unin_b$nlpar == "top"], "beta(5, 2)")
  expect_identical(regu_b$prior[regu_b$nlpar == "top"], "beta(5, 1)")
  # invalid value errors via match.arg
  expect_error(define_prior("nec4param", gaussian(), pred_a, resp,
                            prior_type = "nonsense"))
})

# #210: the `top` and `bot` gamma rates are set from quantiles of the response.
# Where a large share of the response is exactly zero those quantiles are zero
# and the rate either collapses onto the fudge term or divides by zero. These
# tests assert the priors stay finite and stay on the scale of the data, at the
# zero fractions the issue names.

# Helper: a nec4param-shaped count response with a true lower asymptote of 5,
# zero-inflated at rate `1 - p`. Seeded inside so each call is reproducible
# independently of test order.
zi_response <- function(p, seed = 1) {
  set.seed(seed)
  x <- as.numeric(rep(1:10, each = 15))
  mu <- 40 * exp(-0.35 * pmax(x - 2, 0)) + 5
  list(x = x,
       y = as.numeric(rpois(length(x), mu) * rbinom(length(x), 1, p)))
}

# Pull the numeric rate out of a "gamma(a, b)" prior string.
gamma_rate <- function(prior_df, par) {
  s <- prior_df$prior[prior_df$nlpar == par]
  as.numeric(sub("^gamma\\([^,]+,\\s*([^)]+)\\)$", "\\1", s))
}

test_that("top and bot priors stay finite across zero fractions", {
  fam <- validate_family("zero_inflated_poisson")
  # 30%, 50% and 80% zeros: the issue's three regimes, spanning the 25%
  # threshold where `bot` used to collapse and the 75% one where `top` became
  # gamma(2, Inf).
  for (p in c(0.7, 0.5, 0.2)) {
    d <- zi_response(p)
    for (type in c("uninformative", "regularizing")) {
      pr <- define_prior("nec4param", fam, d$x, d$y, prior_type = type)
      for (par in c("top", "bot")) {
        rate <- gamma_rate(pr, par)
        expect_true(is.finite(rate),
                    info = paste(type, par, "at", mean(d$y == 0), "zeros"))
        expect_gt(rate, 0)
      }
    }
  }
})

test_that("bot is not pinned at zero once a quarter of the response is zero", {
  # The specific failure in #210: at zi = 0.30 the prior mean for `bot` was
  # 0.03 against a true lower asymptote of 5. Asserted as an order-of-magnitude
  # sanity bound rather than a pinned constant -- the point is that the prior
  # is on the scale of the data, not that it takes any particular value.
  d <- zi_response(0.7)
  expect_gt(mean(d$y == 0), 0.25)
  pr <- define_prior("nec4param", validate_family("zero_inflated_poisson"),
                     d$x, d$y)
  # gamma(2, rate) has mean 2 / rate
  expect_gt(2 / gamma_rate(pr, "bot"), 0.5)
})

test_that("regularizing collapses for any zero, not just many", {
  # Under prior_type = "regularizing" the denominator was quantile(response, 0)
  # -- the minimum -- so a single zero was enough. Test with one zero only,
  # which the uninformative path would never have noticed.
  set.seed(42)
  x <- as.numeric(rep(1:10, each = 5))
  y <- as.numeric(rpois(length(x), 20))
  y[1] <- 0
  expect_equal(sum(y == 0), 1)
  pr <- define_prior("nec4param", validate_family("poisson"), x, y,
                     prior_type = "regularizing")
  expect_true(is.finite(gamma_rate(pr, "bot")))
  expect_gt(gamma_rate(pr, "bot"), 0)
})

test_that("a response with no zeros is left alone", {
  # The guard must not move priors for the ordinary case. Compared against the
  # same response with the guard bypassed, i.e. the raw quantile, which for a
  # strictly positive response is what positive_scale() returns anyway.
  set.seed(7)
  x <- as.numeric(rep(1:10, each = 5))
  y <- as.numeric(rpois(length(x), 20)) + 1
  expect_equal(sum(y == 0), 0)
  expect_identical(bayesnec:::positive_scale(y, 0.25),
                   unname(quantile(y, 0.25)))
  expect_identical(bayesnec:::positive_scale(y, 0.75),
                   unname(quantile(y, 0.75)))
})

test_that("an all-zero response errors rather than returning a broken prior", {
  # Edge case: there is no scale to put top and bot on. Erroring names the
  # problem; the previous behaviour was gamma(2, Inf), which brms would have
  # rejected far downstream with a much less useful message.
  x <- as.numeric(rep(1:10, each = 5))
  expect_error(
    define_prior("nec4param", validate_family("zero_inflated_poisson"),
                 x, rep(0, length(x))),
    "no positive values"
  )
})

test_that("response_link_scale does not warn on an all-zero response", {
  # response_link_scale() computed min(response[response > 0]) eagerly, so an
  # all-zero response warned even though the value is never used for an
  # identity-link count family. Surfaced by the test above.
  expect_silent(
    bayesnec:::response_link_scale(rep(0, 20),
                                   validate_family("zero_inflated_poisson"))
  )
})
