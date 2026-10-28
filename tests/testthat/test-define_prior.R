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
  # Use complete, constant dose groups so the endpoint locations are known
  # independently of the function under test. Normal location equals its mode.
  x_rep <- as.numeric(rep(1:10, each = 10))
  y_rep <- as.numeric(rep(seq(100, 10, length.out = 10), each = 10))
  reg_rep <- define_prior("nec4param", gaussian(), x_rep, y_rep,
                          prior_type = "regularizing")
  unin_rep <- define_prior("nec4param", gaussian(), x_rep, y_rep,
                           prior_type = "uninformative")
  expect_false(identical(unin_rep$prior, reg_rep$prior))
  # each dose group is constant here, so the standard error of the location is
  # zero and the floor does not bind
  expect_identical(reg_rep$prior[reg_rep$nlpar == "top"],
                   paste0("normal(100, ",
                          bayesnec:::regularizing_factor * sd(y_rep) * 2.5, ")"))
  expect_identical(reg_rep$prior[reg_rep$nlpar == "bot"],
                   paste0("normal(10, ",
                          bayesnec:::regularizing_factor * sd(y_rep) * 2.5, ")"))
  expect_false(identical(unin$prior, regu$prior))
  # The regularizing beta prior reads the response.
  rb <- rbeta(100, 1, 5)
  unin_b <- define_prior("nec4param", Beta(link = "identity"), pred_a, rb,
                         prior_type = "uninformative")
  regu_b <- define_prior("nec4param", Beta(link = "identity"), pred_a, rb,
                         prior_type = "regularizing")
  expect_identical(unin_b$prior[unin_b$nlpar == "top"], "beta(5, 2)")
  expect_true(grepl("^beta\\(", regu_b$prior[regu_b$nlpar == "top"]))
  expect_false(identical(regu_b$prior[regu_b$nlpar == "top"], "beta(5, 1)"))
  # invalid value errors via match.arg
  expect_error(define_prior("nec4param", gaussian(), pred_a, resp,
                            prior_type = "nonsense"))
})

# Zero-heavy responses must yield finite priors on the data scale.

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
  # Cross both zero fractions at which raw response quartiles collapse.
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
  # Check the scale against the generating bot, without pinning a prior mean.
  d <- zi_response(0.7)
  expect_gt(mean(d$y == 0), 0.25)
  pr <- define_prior("nec4param", validate_family("zero_inflated_poisson"),
                     d$x, d$y)
  # gamma(2, rate) has mean 2 / rate
  expect_gt(2 / gamma_rate(pr, "bot"), 0.5)
})

test_that("regularizing stays on the scale of the data for a single zero", {
  # A single zero must not collapse the regularizing rate. The location
  # comes from the highest-concentration observations and the spread uses
  # positive_scale().
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
  # A positive response must retain its raw quantile.
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
  # An all-zero count response supplies no scale for the gamma prior.
  x <- as.numeric(rep(1:10, each = 5))
  expect_error(
    define_prior("nec4param", validate_family("zero_inflated_poisson"),
                 x, rep(0, length(x))),
    "no positive values"
  )
})

test_that("response_link_scale does not warn on an all-zero response", {
  # Do not evaluate a minimum positive response when this family does not need it.
  expect_silent(
    bayesnec:::response_link_scale(rep(0, 20),
                                   validate_family("zero_inflated_poisson"))
  )
})

# Do not evaluate gamma scales for families whose priors do not use them.

test_that("a gaussian response with no positive values still builds priors", {
  # Negative Gaussian responses have valid quantile and standard-deviation priors.
  set.seed(1)
  x <- as.numeric(rep(1:10, each = 5))
  y <- -rexp(50, 1) - 0.5
  expect_equal(sum(y > 0), 0)
  pr <- define_prior("nec4param", validate_family("gaussian"), x, y)
  expect_s3_class(pr, "brmsprior")
  expect_setequal(pr$nlpar, c("beta", "top", "bot", "nec"))
  expect_true(all(grepl("^normal\\(", pr$prior[pr$nlpar %in% c("top", "bot")])))
})

test_that("the bounded families are unaffected by zeros in the response", {
  # Their top/bot priors are literals -- beta(5, 2), beta(2, 5) -- so nothing
  # about the response can make them unbuildable. Asserted because the eager
  # evaluation of the gamma-scaled strings reached them too, and a proportion
  # containing zeros is the ordinary case these families are used for.
  #
  # Note "Beta", not "beta": validate_family() dispatches on the brms
  # constructor name. Not tested on an all-zero response, which no bounded
  # family would be handed and which trips an unrelated pre-existing warning in
  # response_link_scale() -- see the comment at R/helpers.R on min_z_val.
  set.seed(3)
  x <- as.numeric(rep(1:10, each = 5))
  y <- c(rep(0, 12), runif(38, 0.05, 0.95))
  for (fam in c("Beta", "binomial", "bernoulli", "beta_binomial")) {
    pr <- define_prior("nec4param", validate_family(fam), x, y)
    expect_s3_class(pr, "brmsprior")
    expect_true(all(grepl("^beta\\(", pr$prior[pr$nlpar %in% c("top", "bot")])),
                info = fam)
  }
})

test_that("the count families still error when there is no scale to use", {
  # Count priors still require a positive response scale.
  x <- as.numeric(rep(1:10, each = 5))
  for (fam in c("poisson", "negbinomial", "zero_inflated_poisson")) {
    expect_error(
      define_prior("nec4param", validate_family(fam), x, rep(0, length(x))),
      "no positive values",
      info = fam
    )
  }
})

test_that("a supplied prior is not blocked by an unbuildable default", {
  # A complete supplied prior must bypass an unbuildable default.
  x <- as.numeric(rep(1:10, each = 5))
  y <- rep(0, length(x))
  up <- brms::prior_string("normal(0, 5)", nlpar = "beta") +
    brms::prior_string("gamma(2, 0.1)", nlpar = "top", lb = 0) +
    brms::prior_string("gamma(2, 0.5)", nlpar = "bot", lb = 0) +
    brms::prior_string("gamma(5, 0.36)", nlpar = "nec", lb = 1, ub = 10)
  # Supply init: the all-zero response gives the search no usable response
  # band, and this test concerns prior construction.
  out <- suppressMessages(
    bayesnec:::add_brm_defaults(list(prior = up, init = "random"), "nec4param",
                               validate_family("poisson"), x, y,
                               skip_check = FALSE, custom_name = NULL)
  )
  expect_s3_class(out$prior, "brmsprior")
  expect_setequal(out$prior$nlpar, c("beta", "top", "bot", "nec"))
})

# Finiteness alone misses priors far below the data scale. Check the
# whole zero-fraction range, including just below a raw-quantile collapse.

test_that("the top prior stays on the scale of the data at every zero fraction", {
  x <- as.numeric(rep(1:10, each = 15))
  mu <- 40 * exp(-0.35 * pmax(x - 2, 0)) + 5   # true top 40, true bot 5
  fam <- validate_family("zero_inflated_poisson")
  gamma_rate <- function(prior_df, par) {
    s <- prior_df$prior[prior_df$nlpar == par]
    as.numeric(sub("^gamma\\([^,]+,\\s*([^)]+)\\)$", "\\1", s))
  }
  for (p in c(1, 0.5, 0.35, 0.3, 0.28, 0.25, 0.2)) {
    set.seed(1)
    y <- as.numeric(rpois(length(x), mu) * rbinom(length(x), 1, p))
    pr <- define_prior("nec4param", fam, x, y)
    top_mean <- 2 / gamma_rate(pr, "top")
    # Require the prior mean to remain within a factor of three of the true top.
    expect_gt(top_mean, 40 / 3)
    expect_lt(top_mean, 40 * 3)
  }
})

test_that("a response with no zeros gets exactly the unrescaled quantile", {
  # The rescaling must be invisible when there is nothing to rescale for:
  # zero_frac = 0 makes 1 - (1 - p)(1 - 0) equal to p identically.
  set.seed(7)
  y <- as.numeric(rpois(50, 20)) + 1
  expect_equal(sum(y == 0), 0)
  for (p in c(0, 0.25, 0.5, 0.75, 1)) {
    expect_identical(bayesnec:::positive_scale(y, p),
                     unname(quantile(y, p)),
                     info = paste("probs =", p))
  }
})

test_that("the rescaled level is the conditional quantile, not its inverse", {
  # Pins the algebra. The dividing form 1 - (1 - p)/(1 - z) moves the level the
  # wrong way and goes negative past 75% zeros; the multiplying form recovers
  # the quantile of the positive part.
  y <- c(rep(0, 50), 1:50)
  expect_equal(mean(y == 0), 0.5)
  # 75th percentile of the positive part
  target <- unname(quantile(y[y > 0], 0.75))
  expect_equal(bayesnec:::positive_scale(y, 0.75), target, tolerance = 0.01)
  # and it is nowhere near what the dividing form would have given
  expect_gt(bayesnec:::positive_scale(y, 0.75), 30)
})

test_that("the rescaling degrades smoothly rather than in a jump", {
  # Check stability across the old guard threshold without pinning a value.
  x <- as.numeric(rep(1:10, each = 15))
  mu <- 40 * exp(-0.35 * pmax(x - 2, 0)) + 5
  scales <- vapply(c(0.5, 0.4, 0.35, 0.3, 0.28, 0.26, 0.24, 0.2), function(p) {
    set.seed(1)
    y <- as.numeric(rpois(length(x), mu) * rbinom(length(x), 1, p))
    bayesnec:::positive_scale(y, 0.75)
  }, numeric(1))
  # No adjacent pair may differ by more than a factor of two.
  expect_true(all(scales > 0))
  ratios <- scales[-1] / scales[-length(scales)]
  expect_true(all(ratios > 0.5 & ratios < 2))
})

# Check group-prior scales as well as row presence: an oversized deviation
# can put an identity-link bounded mean outside support.

test_that("parse_group_terms describes each accepted group-level form", {
  f_none <- bayesnecformula(y ~ crf(x, "nec4param"))
  expect_null(bayesnec:::parse_group_terms(f_none, "nec4param"))

  f_ogl <- bayesnecformula(y ~ crf(x, "nec4param") + ogl(tank))
  spec_ogl <- bayesnec:::parse_group_terms(f_ogl, "nec4param")
  expect_true(spec_ogl$ogl)
  expect_equal(spec_ogl$nlpars, "ogl")

  # pgl puts a group-level term on every parameter the model has at once
  f_pgl <- bayesnecformula(y ~ crf(x, "nec4param") + pgl(site))
  spec_pgl <- bayesnec:::parse_group_terms(f_pgl, "nec4param")
  expect_false(spec_pgl$ogl)
  expect_setequal(spec_pgl$nlpars, c("beta", "top", "bot", "nec"))

  f_bar <- bayesnecformula(y ~ crf(x, "nec4param") + (top + nec | site))
  expect_setequal(bayesnec:::parse_group_terms(f_bar, "nec4param")$nlpars,
                  c("top", "nec"))

  # a parameter the model does not have is dropped, matching what
  # add_formula_glef() does when it builds the sub-formula
  f_bad <- bayesnecformula(y ~ crf(x, "nec4param") + (zzz | site))
  expect_null(bayesnec:::parse_group_terms(f_bad, "nec4param"))

  # a disp() term is a variance function, not a grouping term
  f_disp <- bayesnecformula(y ~ crf(x, "nec4param") + disp("power"))
  expect_null(bayesnec:::parse_group_terms(f_disp, "nec4param"))
})

test_that("a group-level sd prior is scaled to the parameter it belongs to", {
  set.seed(245)
  x <- runif(100, 0, 10)
  y <- runif(100, 0.1, 0.9)
  spec_all <- list(nlpars = c("top", "bot", "nec", "beta"), ogl = FALSE)
  pr <- as.data.frame(bayesnec:::define_group_prior(spec_all, x, y))
  expect_true(all(pr$class == "sd"))
  get_scale <- function(p) {
    as.numeric(sub(".*, ([0-9.e+-]+)\\)$", "\\1", pr$prior[pr$nlpar == p]))
  }
  # response-scaled parameters take one tenth of the response range
  expect_equal(get_scale("top"), signif(diff(range(y)) / 10, 4))
  expect_equal(get_scale("bot"), signif(diff(range(y)) / 10, 4))
  # predictor-scaled parameters take one tenth of the predictor range
  expect_equal(get_scale("nec"), signif(diff(range(x)) / 10, 4))
  # the dimensionless ones take one tenth of their own normal(0, 5)
  expect_equal(get_scale("beta"), 0.5)
  expect_true(all(grepl("^student_t\\(3, 0, ", pr$prior)))
})

test_that("the ogl intercept is given a zero-centred prior of its own", {
  # The ogl intercept is confounded with a constant shift in top and bot.
  # A zero-centred prior anchors that decomposition.
  set.seed(245)
  x <- runif(100, 0, 10)
  y <- runif(100, 0.1, 0.9)
  pr <- as.data.frame(
    bayesnec:::define_group_prior(list(nlpars = "ogl", ogl = TRUE), x, y)
  )
  b_row <- pr[pr$class == "b" & pr$nlpar == "ogl", ]
  expect_equal(nrow(b_row), 1)
  expect_true(grepl("^normal\\(0, ", b_row$prior))
  expect_equal(as.numeric(sub(".*, ([0-9.e+-]+)\\)$", "\\1", b_row$prior)),
               signif(diff(range(y)) / 10, 4))
})

test_that("a degenerate response does not produce an unusable scale", {
  # diff(range()) is zero for a constant response, and a prior of scale zero is
  # not a prior. Degenerate input is left to fail on its own terms downstream
  # rather than here.
  pr <- as.data.frame(
    bayesnec:::define_group_prior(list(nlpars = "top", ogl = FALSE),
                                  rep(1, 10), rep(0.5, 10))
  )
  expect_equal(pr$prior, "student_t(3, 0, 0.5)")
})

test_that("no group-level term leaves the prior set untouched", {
  set.seed(245)
  x <- runif(100, 0, 10)
  y <- runif(100, 0.1, 0.9)
  expect_null(bayesnec:::define_group_prior(NULL, x, y))
  with_none <- bayesnec:::define_prior("nec4param", validate_family("gaussian"),
                                       x, y)
  with_null <- bayesnec:::define_prior("nec4param", validate_family("gaussian"),
                                       x, y, group_spec = NULL)
  expect_equal(with_none, with_null)
})

test_that("a hurdle family gets group-level priors too", {
  # The early hurdle return must also construct group-level priors.
  set.seed(245)
  x <- rep(seq(0, 4, length.out = 20), 4)
  y <- c(rep(0, 20), rgamma(60, 2, 1))
  spec <- list(nlpars = "ogl", ogl = TRUE)
  pr <- suppressWarnings(bayesnec:::define_prior(
    "nec3param", validate_family("hurdle_gamma"), x, y, group_spec = spec
  ))
  expect_true(any(pr$class == "sd" & pr$nlpar == "ogl"))
  expect_true(any(pr$class == "b" & pr$nlpar == "ogl"))
  # both blocks' own parameters are still there and untouched
  expect_true(all(c("top", "beta", "nec", "hutop", "hubeta", "hunec") %in%
                    pr$nlpar))
  # Scale from survivors only, then convert to the multiplicative deviation
  # scale: s_y / mean(y), capped at a coefficient of variation of 1.
  surv <- y[y > 0]
  s_y <- diff(range(surv)) / 10
  expected <- signif(min(s_y / mean(surv), 1), 4)
  scale_got <- as.numeric(sub(".*, ([0-9.e+-]+)\\)$", "\\1",
                              pr$prior[pr$class == "sd"]))
  expect_equal(scale_got, expected)
  # The survivors-only scaling is what is being asserted, so it is checked
  # against the whole response as well: including the structural zeros would
  # change both terms of the ratio.
  expect_false(isTRUE(all.equal(scale_got,
                               signif(min((diff(range(y)) / 10) / mean(y), 1),
                                      4))))
})

test_that("a hurdle mu block on a non-identity link is given link-scale priors", {
  # brms applies the inverse mean link to the whole curve. A log or logit
  # mu link therefore needs link-scale top and bot priors in the hurdle route.
  set.seed(302)
  x <- rep(seq(0, 4, length.out = 10), 6)
  y <- rbeta(60, 6, 2) * rbinom(60, 1, 0.8)
  fam <- validate_family(
    brms::zero_inflated_beta(link = "logit", link_zi = "identity"),
    link_source = "link"
  )
  pr <- suppressWarnings(
    bayesnec:::define_prior("nec4param", fam, x, y)
  )
  mu_rows <- pr[pr$nlpar %in% c("top", "bot"), ]
  expect_true(all(grepl("^normal\\(", mu_rows$prior)))
  expect_true(all(is.na(mu_rows$lb) | mu_rows$lb == ""))
  expect_true(all(is.na(mu_rows$ub) | mu_rows$ub == ""))
  # The second block is a probability and validate_family() requires link_zi to
  # be the identity, so it keeps the bounded beta entries.
  zi_rows <- pr[pr$nlpar %in% c("zitop", "zibot"), ]
  expect_true(all(grepl("^beta\\(", zi_rows$prior)))
  expect_equal(as.numeric(zi_rows$ub), rep(1, nrow(zi_rows)))
  # An identity-link fit of the same data is unchanged.
  pr_id <- suppressWarnings(bayesnec:::define_prior(
    "nec4param", validate_family("zero_inflated_beta"), x, y
  ))
  id_rows <- pr_id[pr_id$nlpar %in% c("top", "bot"), ]
  expect_true(all(grepl("^beta\\(", id_rows$prior)))
})

test_that("a group-level term on a hurdle reaches the mu block only", {
  # Group terms are attached before hu sub-formulas, so they reach only mu.
  set.seed(245)
  d <- data.frame(x = rep(seq(0, 4, length.out = 20), 4),
                  y = c(rep(0, 20), rgamma(60, 2, 1)),
                  site = factor(rep(1:5, 16)))
  f <- bayesnecformula(y ~ crf(x, "nec3param") + pgl(site))
  bdat <- suppressMessages(model.frame(f, data = d))
  bb <- suppressMessages(suppressWarnings(
    bayesnec:::wrangle_model_formula("nec3param", f, bdat,
                                     validate_family("hurdle_gamma"))
  ))
  subs <- vapply(bb$pforms, function(z) deparse1(z), character(1))
  # A transformed top groups through topgl. Check that grouping reaches mu
  # without being attached to the population parameter.
  expect_true(all(grepl("site", subs[c("topgl", "beta", "nec")])))
  expect_false(grepl("site", subs[["top"]]))
  expect_false(any(grepl("site", subs[c("hutop", "hubeta", "hunec")])))
})

test_that("prior_type narrows the group-level scales too", {
  # The regularizing prior type must also narrow group-level scales.
  set.seed(245)
  x <- runif(100, 0, 10)
  y <- runif(100, 0.1, 0.9)
  spec <- list(nlpars = c("top", "nec", "beta"), ogl = TRUE)
  get_scale <- function(pr, p, cls) {
    pr <- as.data.frame(pr)
    as.numeric(sub(".*, ([0-9.e+-]+)\\)$", "\\1",
                   pr$prior[pr$nlpar == p & pr$class == cls]))
  }
  u <- bayesnec:::define_group_prior(spec, x, y, prior_type = "uninformative")
  r <- bayesnec:::define_group_prior(spec, x, y, prior_type = "regularizing")
  # signif(., 4) is applied to each scale as it is built, so the halves are
  # compared after the same rounding rather than to an unrounded half.
  narrow <- bayesnec:::regularizing_factor
  for (p in c("top", "nec", "beta")) {
    expect_equal(get_scale(r, p, "sd"),
                 signif(get_scale(u, p, "sd") * narrow, 4))
  }
  # the ogl intercept narrows with them
  expect_equal(get_scale(r, "ogl", "b"),
               signif(get_scale(u, "ogl", "b") * narrow, 4))
  # and the default is unchanged
  expect_equal(bayesnec:::define_group_prior(spec, x, y), u)
})

test_that("prior_type reaches define_group_prior through define_prior", {
  set.seed(245)
  x <- as.numeric(rep(1:10, 5))
  y <- plogis(rnorm(50, 1, 1))
  spec <- list(nlpars = "top", ogl = FALSE)
  sd_row <- function(pt) {
    pr <- as.data.frame(bayesnec:::define_prior(
      "nec4param", validate_family("Beta"), x, y, prior_type = pt,
      group_spec = spec
    ))
    pr[pr$class == "sd", ]
  }
  # Beta groups top through topgl. Narrow the response-scale width before
  # the delta-method conversion, preserving the requested spread ratio.
  m_y <- mean(y)
  conv <- function(narrow) {
    signif(min((diff(range(y)) / (10 * narrow)) / (m_y * (1 - m_y)),
               1 / narrow), 4)
  }
  expect_equal(sd_row("uninformative")$nlpar, "topgl")
  expect_equal(sd_row("regularizing")$prior,
               paste0("student_t(3, 0, ",
                      conv(1 / bayesnec:::regularizing_factor), ")"))
  expect_equal(sd_row("uninformative")$prior,
               paste0("student_t(3, 0, ", conv(1), ")"))
})

test_that("a degenerate scale is not narrowed by prior_type", {
  # The 0.5 fallback stands in for a scale that could not be measured. Halving
  # it would regularize a measurement that was never taken.
  pr <- as.data.frame(bayesnec:::define_group_prior(
    list(nlpars = "top", ogl = FALSE), rep(1, 10), rep(0.5, 10),
    prior_type = "regularizing"
  ))
  expect_equal(pr$prior, "student_t(3, 0, 0.5)")
})

# Build the threshold prior from the concentration series, independently
# of replication, on the declared or inferred predictor scale.

# The prior string is written with paste0(), so comparing strings would pin the
# formatting of a double as well as the value. These read the numbers back.
prior_dist <- function(s) sub("\\(.*$", "", s)
prior_pars <- function(s) {
  as.numeric(strsplit(gsub("^[^(]*\\(|\\)$", "", s), ",[[:space:]]*")[[1]])
}
nec_prior_for <- function(x, family = Beta(link = "identity"), y = NULL) {
  if (is.null(y)) y <- seq(0.9, 0.1, length.out = length(x))
  pr <- suppressMessages(
    get_priors(y ~ crf(x, model = "nec3param"),
               data = data.frame(x = x, y = y), family = family)
  )
  pr$prior[pr$nlpar == "nec"]
}

test_that("the nec prior is finite when most observations are controls", {
  # The distinct positive doses have median 45 despite the repeated controls.
  # State the expected location independently of the prior builder.
  x <- c(rep(0, 12), 5, 15, 45, 135, 300)
  y <- rev(seq_along(x)) + 1
  pr <- define_prior(model = "nec3param", family = Gamma(link = "identity"),
                     predictor = x, response = y)
  s <- pr$prior[pr$nlpar == "nec"]
  expect_equal(prior_dist(s), "lognormal")
  expect_equal(exp(prior_pars(s)[1]), 45)
  expect_true(is.finite(prior_pars(s)[2]))
})

test_that("the nec prior location ignores replication, not just zeros", {
  # An unbalanced design has distinct-dose median 3 and observation median
  # 6.5. The prior must describe the concentration series.
  x <- c(0.5, rep(1, 3), rep(3, 6), rep(10, 8), rep(30, 2))
  y <- rev(seq_along(x)) + 1
  pr <- define_prior(model = "nec3param", family = Gamma(link = "identity"),
                     predictor = x, response = y)
  expect_equal(exp(prior_pars(pr$prior[pr$nlpar == "nec"])[1]), 3)
})

test_that("replication does not change the nec prior at all", {
  # Balanced and unbalanced replication of the same series must give
  # identical prior location and width.
  series <- c(0, 5, 15, 45, 135)
  even <- rep(series, each = 6)
  uneven <- rep(series, times = c(20, 2, 9, 2, 3))
  expect_equal(nec_prior_for(even), nec_prior_for(uneven))
})

test_that("the nec prior median is the median dose tested (#302)", {
  # For an odd number of distinct positive doses, the median after logging
  # is the log of the dose median; the lognormal median is that dose.
  x <- rep(c(0, 0.1, 1, 10, 100, 1000), each = 6)
  s <- nec_prior_for(x)
  expect_equal(prior_dist(s), "lognormal")
  expect_equal(qlnorm(0.5, prior_pars(s)[1], prior_pars(s)[2]), 10)
})

test_that("an even count of doses centres on their log midpoint (#302)", {
  # With an even number of distinct positive doses, the log midpoint gives
  # the geometric mean of the central doses. Odd counts cannot expose this
  # difference from the arithmetic median.
  x <- rep(c(0, 1, 10, 100, 1000), each = 6)
  s <- nec_prior_for(x)
  # 55 is the arithmetic median of the four doses; the prior's median is 31.6.
  expect_equal(qlnorm(0.5, prior_pars(s)[1], prior_pars(s)[2]), sqrt(10 * 100))
})

test_that("the nec prior covers every dose tested (#302)", {
  # Require the central 95% interval to reach both dose extremes on
  # log-asymmetric series. Half the log range alone can miss an extreme.
  data(nassarius, package = "bayesnec", envir = environment())
  series <- list(
    symmetric = c(0, 0.01, 0.1, 1, 10, 100),
    # median above the log mid-range: one dose far above the rest
    top_heavy = sort(unique(nassarius$dose[nassarius$contaminant == "A"])),
    # median below the log mid-range: the low doses are sparse
    bottom_heavy = sort(unique(nassarius$dose[nassarius$contaminant == "B"]))
  )
  for (nm in names(series)) {
    x <- rep(series[[nm]], each = 6)
    pos <- unique(x[x > 0])
    p <- prior_pars(nec_prior_for(x))
    # covers, not equals: one end is reached exactly and the other is passed,
    # because the width is the larger of the two half-widths.
    expect_lte(qlnorm(0.025, p[1], p[2]), min(pos) * (1 + 1e-8), label = nm)
    expect_gte(qlnorm(0.975, p[1], p[2]), max(pos) * (1 - 1e-8), label = nm)
  }
  # A half-range width misses the highest contaminant A dose.
  z <- log(unique(series$top_heavy)[unique(series$top_heavy) > 0])
  expect_lt(qlnorm(0.975, median(z), diff(range(z)) / (2 * qnorm(0.975))),
            max(series$top_heavy))
})

test_that("a low threshold on a wide series is inside the prior (#302)", {
  # Test a low threshold: designs with truths only in their upper halves
  # cannot detect exclusion near the bottom of a wide series.
  data(nassarius, package = "bayesnec", envir = environment())
  x <- rep(sort(unique(nassarius$dose[nassarius$contaminant == "B"])), each = 6)
  p <- prior_pars(nec_prior_for(x))
  lo <- min(x[x > 0])
  mass <- plnorm(max(x), p[1], p[2]) - plnorm(min(x), p[1], p[2])
  cdf_at <- function(q) (plnorm(q, p[1], p[2]) - plnorm(min(x), p[1], p[2])) / mass
  expect_gt(cdf_at(lo), 0.025)
  expect_gt(cdf_at(0.05), 0.025)
})

test_that("two distinct doses give a prior spanning both (#302)", {
  # The smallest design for which the rule is defined rather than falling back.
  x <- rep(c(0, 1, 100), each = 6)
  p <- prior_pars(nec_prior_for(x))
  # two doses are symmetric about their own median on the log axis, so both
  # ends are reached exactly.
  expect_equal(qlnorm(0.025, p[1], p[2]), 1)
  expect_equal(qlnorm(0.975, p[1], p[2]), 100)
})

test_that("the prior is built from the concentrations as recorded (#302)", {
  # A nominal positive control is treated as an applied dose and widens the
  # prior. Record an untreated control as zero.
  base <- c(0, 0.01, 0.1, 1, 10, 100)
  s0 <- prior_pars(nec_prior_for(rep(base, each = 6)))
  eps <- base
  eps[eps == 0] <- 1e-6
  s1 <- prior_pars(nec_prior_for(rep(eps, each = 6)))
  expect_gt(s1[2], s0[2] * 2)
  expect_equal(qlnorm(0.025, s1[1], s1[2]), 1e-6)
})

test_that("the nec prior reaches the top of a log-spaced series (#302)", {
  # Compare the old gamma entry directly: its fixed relative width can
  # exclude a threshold well inside a wide concentration series.
  x <- rep(c(0, 0.01, 0.02, 0.04, 0.08, 0.16, 0.31, 0.63, 1.25, 2.5, 20),
           each = 6)
  m <- median(unique(x))
  truth <- 1.25
  trunc_cdf <- function(f, lo, hi, q) (f(q) - f(lo)) / (f(hi) - f(lo))
  old <- trunc_cdf(function(q) pgamma(q, 5, 4 / m), 0, max(x), truth)
  expect_gt(old, 0.9999)
  p <- prior_pars(nec_prior_for(x))
  new <- trunc_cdf(function(q) plnorm(q, p[1], p[2]), 0, max(x), truth)
  expect_gt(new, 0.025)
  expect_lt(new, 0.975)
})

test_that("the predictor's support no longer selects the prior (#302)", {
  # Changing concentration units must shift the lognormal location by the
  # log conversion factor and preserve its width.
  mg <- rep(c(0, 1, 10, 100, 1000), each = 6)
  g <- mg / 1000
  p_mg <- prior_pars(nec_prior_for(mg))
  p_g <- prior_pars(nec_prior_for(g))
  expect_equal(prior_dist(nec_prior_for(g)), "lognormal")
  expect_equal(p_g[1], p_mg[1] - log(1000))
  expect_equal(p_g[2], p_mg[2])
})

test_that("a predictor spanning negative values keeps its published prior", {
  # For a predictor spanning negatives, use the published normal rule
  # with location and spread from distinct values.
  x <- rep(log(c(0.05, 0.1, 1, 10, 100)), each = 6)
  s <- nec_prior_for(x)
  expect_equal(prior_dist(s), "normal")
  expect_equal(prior_pars(s), c(median(unique(x)), 10 * sd(unique(x))))
})

test_that("the nec prior is chosen from the predictor, not the response", {
  # Family and link must not change the threshold prior. The prior type
  # changes its spread and is tested separately.
  x <- rep(c(0, 25, 50, 75, 100), each = 6)
  target <- nec_prior_for(x)
  expect_equal(prior_dist(target), "lognormal")
  expect_equal(nec_prior_for(x, Gamma(link = "identity"),
                             rep(c(9, 8, 5, 2, 1), each = 6)), target)
  expect_equal(nec_prior_for(x, gaussian(),
                             rep(c(9, 8, 5, 2, -1), each = 6)), target)
  expect_equal(nec_prior_for(x, poisson(link = "identity"),
                             as.integer(rep(c(90, 80, 50, 20, 5), each = 6))),
               target)
})

# The design list of notes/scripts/prior_audit.R, with the true nec and ec50 of
# each. Repeated here so that the cells the audit scores can be scored without
# running it; the two lists must be changed together.
audit_designs <- function() {
  list(
    linear = list(x = seq(0, 10, length.out = 11), nec = 4, ec50 = 5),
    linear_unit = list(x = seq(0, 1, length.out = 11), nec = 0.4, ec50 = 0.5),
    log_2fold = list(x = c(0, 0.078, 0.156, 0.3125, 0.625, 1.25, 2.5, 5, 10),
                     nec = 1.25, ec50 = 2.5),
    log_unit = list(x = c(0, 0.0039, 0.0078, 0.0156, 0.0312, 0.0625, 0.125,
                          0.25, 0.5, 1), nec = 0.25, ec50 = 0.5),
    log_wide = list(x = c(0, 0.01, 0.02, 0.04, 0.08, 0.16, 0.31, 0.63, 1.25,
                          2.5, 20), nec = 1.25, ec50 = 2)
  )
}

# State the spread rule independently of predictor_prior().
reg_spread <- function(z, q = 0.99) {
  z <- unique(z)
  max(median(z) - min(z), max(z) - median(z)) / qnorm(q)
}
# The predictor on that scale: the distinct positive values logged where
# concentrations are supplied as recorded, and the distinct values themselves
# where the user supplied them logged.
prior_scale <- function(x) {
  u <- unique(x)
  if (min(u) < 0) u else log(u[u > 0])
}
# The prior truncated to the tested range, as define_prior() truncates it.
trunc_prior_cdf <- function(s, x) {
  p <- prior_pars(s)
  cdf <- if (prior_dist(s) == "normal") {
    function(q) pnorm(q, p[1], p[2])
  } else {
    function(q) plnorm(q, p[1], p[2])
  }
  lo <- if (prior_dist(s) == "normal") min(x) else max(min(x), 0)
  flo <- cdf(lo)
  mass <- cdf(max(x)) - flo
  function(q) (cdf(q) - flo) / mass
}

test_that("prior_type narrows the nec and ec50 prior and nothing else (#314)", {
  # The regularizing spread makes the central 98% interval reach the
  # farthest tested dose, preserving the location and distribution.
  for (x in list(rep(c(0, 25, 50, 75, 100), each = 6),
                 rep(log(c(0.05, 0.1, 1, 10, 100)), each = 6))) {
    u <- bayesnec:::predictor_prior(x)
    r <- bayesnec:::predictor_prior(x, prior_type = "regularizing")
    expect_equal(prior_dist(r), prior_dist(u))
    expect_equal(prior_pars(r)[1], prior_pars(u)[1])
    expect_equal(prior_pars(r)[2], reg_spread(prior_scale(x)))
    expect_lt(prior_pars(r)[2], prior_pars(u)[2])
  }
  # Check nec and ec50 bounds against distributional support.
  x <- rep(c(0, 25, 50, 75, 100), each = 6)
  d <- data.frame(x = x, y = seq(0.9, 0.1, length.out = length(x)))
  reg <- suppressMessages(get_priors(
    y ~ crf(x, model = "ecx4param"), data = d,
    family = Beta(link = "identity"), prior_type = "regularizing"
  ))
  target <- bayesnec:::predictor_prior(x, prior_type = "regularizing")
  expect_equal(reg$prior[reg$nlpar == "ec50"], target)
  expect_equal(as.numeric(reg$lb[reg$nlpar == "ec50"]), 0)
  expect_true(is.na(reg$ub[reg$nlpar == "ec50"]))
})

# Literal uninformative prior strings cover all five audit designs and
# three transforms. The log transform replaces a zero control with half
# the lowest positive dose, as notes/scripts/prior_audit.R does.
audit_uninformative <- c(
  linear_identity = "lognormal(1.70059869083108, 0.867668336890465)",
  linear_sqrt = "lognormal(0.850299345415539, 0.433834168445233)",
  linear_log = "normal(1.6094379124341, 9.61783829511775)",
  linear_unit_identity = "lognormal(-0.601986402162968, 0.867668336890465)",
  linear_unit_sqrt = "lognormal(-0.300993201081484, 0.433834168445233)",
  linear_unit_log = "normal(-0.693147180559945, 9.61783829511775)",
  log_2fold_identity = "lognormal(-0.123430038965763, 1.23860256233049)",
  log_2fold_sqrt = "lognormal(-0.0617150194828814, 0.619301281165244)",
  log_2fold_log = "normal(-0.470003629245736, 18.9891958061889)",
  log_unit_identity = "lognormal(-2.77258872223978, 1.41542907190602)",
  log_unit_sqrt = "lognormal(-1.38629436111989, 0.707714535953011)",
  log_unit_log = "normal(-3.11996295320324, 20.993419114392)",
  log_wide_identity = "lognormal(-1.50188222262563, 2.29474344001024)",
  log_wide_sqrt = "lognormal(-0.750941111312814, 1.14737172000512)",
  log_wide_log = "normal(-1.83258146374831, 25.2367970797139)"
)

# The predictor of one audit cell, on the transform named.
audit_x <- function(d, transform) {
  xr <- d$x
  if (transform == "log") {
    nz <- min(xr[xr > 0])
    xr[xr == 0] <- nz / 2
  }
  rep(switch(transform, identity = xr, sqrt = sqrt(xr), log = log(xr)),
      each = 6)
}

test_that("the uninformative nec and ec50 entry is unchanged (#314)", {
  # Pin the uninformative entries across every audit cell, including the
  # normal branch selected by logged predictors.
  pp <- bayesnec:::predictor_prior
  for (dn in names(audit_designs())) {
    for (tn in c("identity", "sqrt", "log")) {
      x <- audit_x(audit_designs()[[dn]], tn)
      expect_equal(pp(x), unname(audit_uninformative[[paste0(dn, "_", tn)]]))
    }
  }
  # and two designs outside that list, one per branch.
  expect_equal(pp(rep(c(0, 25, 50, 75, 100), each = 6)),
               "lognormal(4.11475555948223, 0.457089896386165)")
  expect_equal(pp(rep(log(c(0.05, 0.1, 1, 10, 100)), each = 6)),
               "normal(0, 31.7284345811794)")
})

test_that("the regularizing entry is unchanged for recorded doses (#314)", {
  # Compare the algebraically equivalent spread rules numerically: the
  # extra operations can change rounding and therefore the prior string.
  factor_form <- function(x) {
    z <- prior_scale(x)
    reg_spread(z, 0.975) * (qnorm(0.975) / qnorm(0.99))
  }
  for (d in audit_designs()) {
    for (f in list(identity, sqrt)) {
      x <- rep(f(d$x), each = 6)
      r <- bayesnec:::predictor_prior(x, prior_type = "regularizing")
      expect_equal(prior_dist(r), "lognormal")
      expect_equal(prior_pars(r)[2], factor_form(x))
    }
  }
})

test_that("both routes agree below a lowest dose of 1 and not above (#314)", {
  # The routes agree only without a substituted zero control and when the
  # logged series spans negatives. Non-negative logged values are treated
  # as concentrations by the automatic discriminator and logged again.
  pp <- bayesnec:::predictor_prior
  below_one <- c(0.1, 0.3, 1, 3, 10, 30, 100)
  raw <- pp(rep(below_one, each = 6), "regularizing")
  logged <- pp(rep(log(below_one), each = 6), "regularizing")
  expect_equal(prior_dist(raw), "lognormal")
  expect_equal(prior_dist(logged), "normal")
  expect_equal(prior_pars(logged), prior_pars(raw))

  above_one <- round(exp(seq(log(10), log(10000), length.out = 7)), 3)
  raw2 <- pp(rep(above_one, each = 6), "regularizing")
  logged2 <- pp(rep(log(above_one), each = 6), "regularizing")
  # the logged series is taken for concentrations, so both are lognormal
  expect_equal(prior_dist(logged2), "lognormal")
  expect_equal(prior_dist(raw2), "lognormal")
  expect_false(isTRUE(all.equal(prior_pars(logged2), prior_pars(raw2))))
  expect_lt(prior_pars(logged2)[1], prior_pars(raw2)[1])
  expect_lt(prior_pars(logged2)[2], prior_pars(raw2)[2])
  # Check the misclassified prior still peaks inside this tested range
  # and still narrows under the regularizing type.
  lx <- rep(log(above_one), each = 6)
  mode <- exp(prior_pars(logged2)[1] - prior_pars(logged2)[2]^2)
  expect_gt(mode, min(lx))
  expect_lt(mode, max(lx))
  expect_lt(prior_pars(logged2)[2], prior_pars(pp(lx))[2])
})

test_that("an explicit log scale fixes a non-negative logged predictor (#317)", {
  pp <- bayesnec:::predictor_prior
  concentration <- round(exp(seq(log(10), log(10000), length.out = 7)), 3)
  logged <- log(concentration)

  for (type in c("uninformative", "regularizing")) {
    from_concentration <- pp(concentration, type,
                             predictor_scale = "concentration")
    from_log <- pp(logged, type, predictor_scale = "log")
    expect_equal(prior_dist(from_concentration), "lognormal")
    expect_equal(prior_dist(from_log), "normal")
    expect_equal(prior_pars(from_log)[1], prior_pars(from_concentration)[1])
    if (type == "regularizing") {
      expect_equal(prior_pars(from_log)[2], prior_pars(from_concentration)[2])
    } else {
      expect_equal(prior_pars(from_log)[2], 10 * sd(unique(logged)))
    }
    expect_false(identical(pp(logged, type), from_log))
  }
})

test_that("an explicit log scale retains log(1) and validates declarations (#317)", {
  pp <- bayesnec:::predictor_prior
  logged <- log(c(1, 3, 10, 30, 100))
  explicit <- pp(logged, predictor_scale = "log")
  expected <- paste0("normal(", median(logged), ", ", 10 * sd(logged), ")")

  expect_equal(explicit, expected)
  expect_false(identical(pp(logged), explicit))
  expect_error(pp(c(-1, 0, 1), predictor_scale = "concentration"),
               "requires a non-negative predictor")
  expect_error(pp(logged, predictor_scale = "unknown"),
               "arg.*one of")
})

test_that("the regularizing prior is not uniform once logged (#314)", {
  # Compare against the previous wide normal entry, whose truncated CDF
  # was nearly uniform across the tested range.
  conc <- c(0.1, 0.3, 1, 3, 10, 30, 100)
  x <- rep(log(conc), each = 6)
  z <- log(conc)
  position <- (z - min(z)) / (max(z) - min(z))
  old <- paste0("normal(", median(z), ", ",
                10 * sd(z) * qnorm(0.975) / qnorm(0.99), ")")
  new <- bayesnec:::predictor_prior(x, prior_type = "regularizing")
  expect_lt(max(abs(trunc_prior_cdf(old, x)(z) - position)), 1e-3)
  # The new truncated CDF must differ materially from a uniform one.
  got <- trunc_prior_cdf(new, x)(z)
  expect_lt(got[2], position[2] / 2)
  expect_gt(got[6], position[6] * 1.1)
})

test_that("the regularizing prior admits a low threshold (#314)", {
  # Keep mass below a low threshold when selecting the coverage quantile.
  # Divide doses by their maximum before logging so the automatic rule
  # recognises a logged predictor; this common shift preserves probabilities.
  designs <- list(
    wide = c(0.1, 0.3, 1, 3, 10, 30, 100),
    narrow = round(exp(seq(log(1), log(20), length.out = 5)), 3),
    decade = round(exp(seq(log(1), log(1000), length.out = 7)), 3),
    linear = 1:10
  )
  for (conc in designs) {
    scaled <- conc / max(conc)
    for (x in list(rep(conc, each = 6), rep(log(scaled), each = 6))) {
      r <- bayesnec:::predictor_prior(x, prior_type = "regularizing")
      d2 <- sort(unique(x))[2]
      expect_gt(trunc_prior_cdf(r, x)(d2), 0.025)
    }
  }
})

test_that("the regularizing prior keeps a true threshold inside it (#314)", {
  # Require both a changed regularizing entry and coverage of every audit
  # truth. Coverage alone also passes for the nearly uniform old entry.
  for (dn in names(audit_designs())) {
    d <- audit_designs()[[dn]]
    x <- audit_x(d, "log")
    r <- bayesnec:::predictor_prior(x, prior_type = "regularizing")
    before <- 10 * sd(unique(x)) * (qnorm(0.975) / qnorm(0.99))
    expect_lt(prior_pars(r)[2], before / 5)
    for (truth in log(c(d$nec, d$ec50))) {
      p <- trunc_prior_cdf(r, x)(truth)
      expect_gt(p, 0.025)
      expect_lt(p, 0.975)
    }
  }
  # Larger coverage quantiles exclude the log_unit ec50 truth from the
  # central 95% interval.
  d <- audit_designs()$log_unit
  x <- audit_x(d, "log")
  for (q in c(0.995, 0.999)) {
    narrow <- paste0("normal(", median(unique(x)), ", ",
                     reg_spread(prior_scale(x), q), ")")
    expect_gt(trunc_prior_cdf(narrow, x)(log(d$ec50)), 0.975)
  }
})

test_that("a degenerate predictor scale is not narrowed by prior_type (#305)", {
  # The fallback of 1 stands in for a spread that could not be measured, so
  # narrowing it would state a precision nothing in the data supports. Matches
  # what define_group_prior() does with its own fallback.
  expect_equal(bayesnec:::predictor_prior(rep(5, 10),
                                          prior_type = "regularizing"),
               paste0("lognormal(", log(5), ", 1)"))
  expect_equal(bayesnec:::predictor_prior(rep(-1.5, 10),
                                          prior_type = "regularizing"),
               "normal(-1.5, 1)")
})

test_that("ec50 reads the same predictor prior as nec", {
  x <- rep(c(0, 0.1, 1, 10, 100), each = 6)
  d <- data.frame(x = x, y = seq(0.9, 0.1, length.out = length(x)))
  pr <- suppressMessages(
    get_priors(y ~ crf(x, model = "ecx4param"), data = d,
               family = Beta(link = "identity"))
  )
  nec_pr <- nec_prior_for(x)
  expect_equal(pr$prior[pr$nlpar == "ec50"], nec_pr)
})

test_that("a design with no range to span still yields a usable prior (#302)", {
  # A single distinct positive dose cannot identify the curve. Use a finite
  # fallback spread of 1 instead of a zero or non-finite scale.
  pp <- bayesnec:::predictor_prior
  expect_equal(pp(rep(c(0, 5), 10)), paste0("lognormal(", log(5), ", 1)"))
  expect_equal(pp(rep(5, 10)), paste0("lognormal(", log(5), ", 1)"))
  expect_equal(pp(rep(-1.5, 10)), "normal(-1.5, 1)")
})

test_that("a predictor with no positive values is refused (#302)", {
  # There is no concentration scale to place the prior on. check_data() fails
  # first on a constant predictor, so this is a backstop rather than the
  # message a user normally sees.
  expect_error(bayesnec:::predictor_prior(rep(0, 10)), "no positive values")
})

test_that("a hurdle mu block reads the whole predictor for nec (#302)", {
  # Both hurdle blocks use the whole predictor series for their thresholds.
  # The survival block already has its distinct values; test both blocks
  # and require bounds from distributional support.
  set.seed(302)
  x <- rep(c(0, 1, 10, 100), each = 6)
  y_sub <- c(rgamma(18, 25, 25 / 8), rep(0, 6))     # survivors up to x = 10
  fam <- brms::hurdle_gamma(link = "identity", link_hu = "identity")
  pr <- bayesnec:::define_hurdle_prior("nec3param", fam, x, y_sub)
  whole <- bayesnec:::predictor_prior(x)
  expect_equal(pr$prior[pr$nlpar == "nec"], whole)
  expect_equal(pr$prior[pr$nlpar == "hunec"], whole)
  expect_equal(as.numeric(pr$lb[pr$nlpar == "nec"]), 0)
  expect_true(is.na(pr$ub[pr$nlpar == "nec"]))
})

test_that("a hurdle fit uses the declared scale for both blocks (#317)", {
  set.seed(317)
  x <- rep(log(c(1, 3, 10, 30, 100)), each = 6)
  y <- c(rgamma(24, 25, 25 / 8), rep(0, 6))
  fam <- brms::hurdle_gamma(link = "identity", link_hu = "identity")
  pr <- bayesnec:::define_hurdle_prior(
    "nec3param", fam, x, y, predictor_scale = "log"
  )

  expect_match(pr$prior[pr$nlpar == "nec"], "^normal\\(")
  expect_match(pr$prior[pr$nlpar == "hunec"], "^normal\\(")
  # The normal branch has neither bound, including for negative logged doses.
  expect_true(is.na(pr$lb[pr$nlpar == "nec"]))
  expect_true(is.na(pr$ub[pr$nlpar == "nec"]))
})

test_that("a hurdle mu block with no surviving dose still builds (#302)", {
  # Where every survivor sits at the zero control the mu block's own predictor
  # has no positive value, so a prior built from that subset could not be
  # constructed at all. Reading the whole predictor removes the refusal.
  set.seed(302)
  x <- rep(c(0, 1, 10, 100), each = 6)
  y_ctl <- c(rgamma(6, 25, 25 / 8), rep(0, 18))
  fam <- brms::hurdle_gamma(link = "identity", link_hu = "identity")
  pr <- bayesnec:::define_hurdle_prior("nec3param", fam, x, y_ctl)
  expect_equal(pr$prior[pr$nlpar == "nec"], bayesnec:::predictor_prior(x))
})

test_that("define_prior still refuses an integer predictor", {
  # Keep the integer-predictor refusal even though the distribution label
  # from set_distribution() no longer selects the threshold prior.
  x <- as.integer(rep(c(0L, 1L, 10L, 100L), each = 6))
  y <- rep(c(0.9, 0.6, 0.3, 0.1), each = 6)
  expect_error(
    define_prior("nec3param", Beta(link = "identity"), x, y),
    "does not currently support integer concentration"
  )
})

# Priors for transformed parameter-level deviations.

test_that("a transformed term declares its sd on the deviation, not the parameter", {
  # Keep bot’s population prior and place the group standard deviation on
  # botgl. An sd prior on bot would match no grouping term.
  set.seed(294)
  x <- runif(100, 0, 10)
  y <- runif(100, 0.1, 0.9)
  spec <- list(nlpars = c("top", "bot", "nec", "beta"), ogl = FALSE)
  pr <- as.data.frame(
    bayesnec:::define_group_prior(spec, x, y, par_transform = "logit")
  )
  sd_rows <- pr[pr$class == "sd", ]
  expect_setequal(sd_rows$nlpar, c("topgl", "botgl", "nec", "beta"))
  expect_false(any(sd_rows$nlpar %in% c("top", "bot")))
  # The transformed deviation has no population intercept: it would be
  # unidentified against bot. Its group-prior rows contain only sd.
  expect_true(all(pr$class == "sd"))
})

test_that("the transformed scale is the delta-method conversion, capped", {
  set.seed(294)
  x <- runif(100, 0, 10)
  y <- runif(100, 0.3, 0.7)
  spec <- list(nlpars = "bot", ogl = FALSE)
  get_scale <- function(pr, cls, p) {
    pr <- as.data.frame(pr)
    as.numeric(sub(".*, ([0-9.e+-]+)\\)$", "\\1",
                   pr$prior[pr$class == cls & pr$nlpar == p]))
  }
  s_y <- diff(range(y)) / 10
  m_y <- mean(y)
  # Convert the response-scale width by the logit Jacobian at the response mean.
  logit_pr <- bayesnec:::define_group_prior(spec, x, y, par_transform = "logit")
  expect_equal(get_scale(logit_pr, "sd", "botgl"),
               signif(s_y / (m_y * (1 - m_y)), 4))
  # log: a group-level coefficient of variation.
  log_pr <- bayesnec:::define_group_prior(spec, x, y, par_transform = "log")
  expect_equal(get_scale(log_pr, "sd", "botgl"), signif(min(s_y / m_y, 1), 4))
  # The prior set for a transformed term is one row, the standard deviation.
  expect_equal(nrow(as.data.frame(logit_pr)), 1)
  # Untransformed, the response-scale width is kept and the name is the
  # parameter's.
  none_pr <- bayesnec:::define_group_prior(spec, x, y, par_transform = "none")
  expect_equal(get_scale(none_pr, "sd", "bot"), signif(s_y, 4))
})

test_that("the parameter-level conversion is capped in both branches", {
  # Near a boundary, the logit Jacobian can make the converted width large.
  # The deviation acts on bot, not the response mean, so cap both parameter
  # transform branches rather than applying the ogl argument.
  set.seed(294)
  x <- runif(100, 0, 10)
  y <- c(runif(99, 0, 0.02), 1 - 1e-4)
  spec <- list(nlpars = "bot", ogl = FALSE)
  get_scale <- function(pr, p) {
    pr <- as.data.frame(pr)
    as.numeric(sub(".*, ([0-9.e+-]+)\\)$", "\\1",
                   pr$prior[pr$class == "sd" & pr$nlpar == p]))
  }
  s_y <- diff(range(y)) / 10
  m_y <- mean(y)
  expect_gt(s_y / (m_y * (1 - m_y)), 1)
  expect_equal(get_scale(bayesnec:::define_group_prior(spec, x, y,
                                                      par_transform = "logit"),
                         "botgl"), 1)
  # The cap scales with prior_type, or "regularizing" would be inert exactly
  # where the cap binds -- which is this case, the one the cap exists for.
  expect_equal(get_scale(bayesnec:::define_group_prior(
    spec, x, y, prior_type = "regularizing", par_transform = "logit"),
    "botgl"), bayesnec:::regularizing_factor)
  # Keep the separate ogl conversion rule.
  ogl_spec <- list(nlpars = "ogl", ogl = TRUE)
  expect_equal(get_scale(bayesnec:::define_group_prior(ogl_spec, x, y,
                                                       ogl_transform = "logit"),
                         "ogl"),
               signif(s_y / (m_y * (1 - m_y)), 4))
})

test_that("define_prior picks the parameter transform from the family", {
  set.seed(294)
  x <- runif(100, 0, 10)
  y <- runif(100, 0.1, 0.9)
  spec <- list(nlpars = c("bot", "nec"), ogl = FALSE)
  beta_pr <- as.data.frame(
    define_prior("nec4param", validate_family("Beta"), x, y, group_spec = spec)
  )
  expect_true("botgl" %in% beta_pr$nlpar[beta_pr$class == "sd"])
  # bot itself still gets its own bounded population-level prior.
  bot_row <- beta_pr[beta_pr$class == "b" & beta_pr$nlpar == "bot", ]
  expect_equal(nrow(bot_row), 1)
  expect_equal(bot_row$ub, "1")
  # gaussian is unconstrained, so nothing is transformed.
  gauss_pr <- as.data.frame(
    define_prior("nec4param", validate_family("gaussian"), x, y,
                 group_spec = spec)
  )
  expect_true("bot" %in% gauss_pr$nlpar[gauss_pr$class == "sd"])
  expect_false("botgl" %in% gauss_pr$nlpar)
})

# ---------------------------------------------------------------------------
# The regularizing prior set: one contract, applied to every branch. #305.
#
# The set is defined by a location quantile and a spread multiple, and each
# branch uses whichever distribution matches its parameter's support with its
# mode at the location and its standard deviation at the spread. These tests
# assert the two halves of that separately -- the ratio of spreads, and the
# placement of the mode -- so that a failure says which half moved.

# The mode and standard deviation of a prior string, whichever branch built it.
prior_moments <- function(s) {
  a <- prior_pars(s)
  switch(
    prior_dist(s),
    normal = c(mode = a[1], sd = a[2]),
    gamma = c(mode = (a[1] - 1) / a[2], sd = sqrt(a[1]) / a[2]),
    beta = c(mode = (a[1] - 1) / (a[1] + a[2] - 2),
             sd = bayesnec:::beta_sd(a[1], a[2])),
    lognormal = c(mode = exp(a[1] - a[2]^2),
                  sd = sqrt((exp(a[2]^2) - 1) * exp(2 * a[1] + a[2]^2)))
  )
}

# Simulate each branch with known asymptotes. A count minimum can be far
# below the bot that the prior should locate.
branch_response <- function(branch, seed = 305) {
  set.seed(seed)
  x <- as.numeric(rep(seq(0, 10, length.out = 11), each = 6))
  shape <- function(top, bot) bot + (top - bot) * exp(-0.8 * pmax(x - 4, 0))
  switch(
    branch,
    normal = list(x = x, y = rnorm(length(x), shape(10, 2), 0.6),
                  family = gaussian(), top = 10, bot = 2),
    gamma = list(x = x,
                 y = as.numeric(rpois(length(x), shape(40, 5))),
                 family = poisson(link = "identity"), top = 40, bot = 5),
    beta = list(x = x,
                y = rbeta(length(x), shape(0.9, 0.05) * 20,
                          (1 - shape(0.9, 0.05)) * 20),
                family = Beta(link = "identity"), top = 0.9, bot = 0.05)
  )
}

priors_for <- function(d, prior_type) {
  as.data.frame(define_prior("nec4param", validate_family(d$family), d$x, d$y,
                             prior_type = prior_type))
}

test_that("regularizing is never wider than uninformative, in any branch", {
  # Selecting the regularizing set must not increase the prior spread.
  for (branch in c("normal", "gamma", "beta")) {
    d <- branch_response(branch)
    u <- priors_for(d, "uninformative")
    r <- priors_for(d, "regularizing")
    for (par in c("top", "bot")) {
      su <- prior_moments(u$prior[u$nlpar == par])[["sd"]]
      sr <- prior_moments(r$prior[r$nlpar == par])[["sd"]]
      # the prior string keeps six significant figures, so the comparison is
      # made at that precision rather than exactly
      expect_lte(sr / su, 1 + 1e-4)
      expect_gte(sr / su, bayesnec:::regularizing_factor - 1e-4)
    }
  }
})

test_that("the regularizing spread is the stated factor for a precise anchor", {
  # A precise replicated anchor leaves the standard-error floor inactive,
  # so the spread ratio equals regularizing_factor.
  d <- branch_response("normal")
  u <- priors_for(d, "uninformative")
  r <- priors_for(d, "regularizing")
  for (par in c("top", "bot")) {
    expect_equal(prior_moments(r$prior[r$nlpar == par])[["sd"]] /
                   prior_moments(u$prior[u$nlpar == par])[["sd"]],
                 bayesnec:::regularizing_factor, tolerance = 1e-4,
                 info = par)
  }
})

test_that("the regularizing prior peaks at the level of the curve's own end", {
  # State the location through the mode for every distribution. A gamma
  # mean would locate its peak elsewhere.
  for (branch in c("normal", "gamma", "beta")) {
    d <- branch_response(branch)
    r <- priors_for(d, "regularizing")
    yl <- bayesnec:::response_link_scale(d$y, validate_family(d$family))
    # Compute the endpoint mean independently. Six replicates per dose
    # make the extreme group exactly one complete dose group.
    for (par in c("top", "bot")) {
      grp <- if (par == "top") d$x == min(d$x) else d$x == max(d$x)
      y_grp <- yl[grp]
      if (branch == "gamma" && par == "top") {
        y_grp <- y_grp[y_grp > 0]
      }
      expect_equal(prior_moments(r$prior[r$nlpar == par])[["mode"]],
                   mean(y_grp), tolerance = 1e-3, info = paste(branch, par))
    }
  }
})

test_that("regularizing_location reads the predictor end, not the tail", {
  # A pooled upper quantile can reflect count dispersion rather than top.
  # Use the mean at the predictor endpoint.
  d <- branch_response("gamma")
  loc <- bayesnec:::regularizing_location(d$x, d$y, "top",
                                          zero_bounded = TRUE)
  expect_lt(abs(loc[["location"]] - d$top), abs(quantile(d$y, 0.95) - d$top))
  expect_gt(loc[["se"]], 0)
  # the subset is the control group, which is larger than the minimum
  expect_equal(loc[["location"]], mean(d$y[d$x == min(d$x)]))
  # and bot reads the other end
  expect_equal(
    bayesnec:::regularizing_location(d$x, d$y, "bot",
                                     zero_bounded = TRUE)[["location"]],
    mean(d$y[d$x == max(d$x)])
  )
})

test_that("regularizing_location extends past one value without replication", {
  # On a continuous predictor every distinct value has one observation, so the
  # extreme group would be a single point. At least a twentieth of the
  # observations, and never fewer than three, are averaged instead.
  set.seed(305)
  x <- sort(runif(100, 0, 10))
  y <- 5 + 35 * exp(-0.8 * pmax(x - 4, 0)) + rnorm(100)
  loc <- bayesnec:::regularizing_location(x, y, "top")
  expect_equal(loc[["location"]], mean(y[seq_len(5)]))
  short <- bayesnec:::regularizing_location(x[1:20], y[1:20], "top")
  expect_equal(short[["location"]], mean(y[seq_len(3)]))
})

test_that("the regularizing prior does not exclude a count asymptote (#305)", {
  # Compare against the old minimum-based count anchor, which could place
  # the true bot above almost all the prior mass.
  d <- branch_response("gamma")
  expect_lt(min(d$y), d$bot)
  r <- priors_for(d, "regularizing")
  a <- prior_pars(r$prior[r$nlpar == "bot"])
  # bot is bounded below at zero and unbounded above, so the truncated CDF is
  # the untruncated one.
  expect_gt(pgamma(d$bot, a[1], a[2]), 0.02)
  expect_lt(pgamma(d$bot, a[1], a[2]), 0.98)
  # the released anchor, computed here, is what it is being compared against
  released <- 5 / (min(d$y) + min(d$y[d$y > 0]) / 10)
  expect_gt(pgamma(d$bot, 5, released), 0.98)
})

test_that("the beta branch reads the response under regularizing (#305)", {
  # Different control levels must change regularizing beta priors while
  # leaving the uninformative entries identical.
  set.seed(305)
  x <- as.numeric(rep(seq(0, 10, length.out = 11), each = 6))
  high <- rbeta(length(x), 18, 2)
  low <- rbeta(length(x), 6, 14)
  pr <- function(y, type) {
    p <- as.data.frame(define_prior("nec4param", Beta(link = "identity"), x, y,
                                    prior_type = type))
    p$prior[p$nlpar == "top"]
  }
  expect_identical(pr(high, "uninformative"), pr(low, "uninformative"))
  expect_false(identical(pr(high, "regularizing"), pr(low, "regularizing")))
  expect_gt(prior_moments(pr(high, "regularizing"))[["mode"]],
            prior_moments(pr(low, "regularizing"))[["mode"]])
})

test_that("gamma_from_mode_sd solves for the mode and standard deviation", {
  set.seed(305)
  for (i in seq_len(50)) {
    location <- runif(1, 0.01, 100)
    spread <- runif(1, 0.001, location * 5)
    p <- bayesnec:::gamma_from_mode_sd(location, spread)
    expect_gt(p[["shape"]], 1)
    expect_equal((p[["shape"]] - 1) / p[["rate"]], location)
    expect_equal(sqrt(p[["shape"]]) / p[["rate"]], spread)
  }
})

test_that("beta_from_mode_sd solves for the mode and standard deviation", {
  set.seed(305)
  for (i in seq_len(50)) {
    location <- runif(1, 0.02, 0.98)
    spread <- runif(1, 0.005, 0.25)
    p <- bayesnec:::beta_from_mode_sd(location, spread)
    expect_equal(bayesnec:::beta_sd(p[["shape1"]], p[["shape2"]]), spread,
                 tolerance = 1e-6)
    # Asserted unconditionally. Guarding it on both shapes exceeding 1 would
    # let a regression that returned boundary shapes pass in silence, so the
    # shapes are required to be interior as part of the assertion.
    expect_gt(p[["shape1"]], 1)
    expect_gt(p[["shape2"]], 1)
    expect_equal((p[["shape1"]] - 1) /
                   (p[["shape1"]] + p[["shape2"]] - 2), location,
                 tolerance = 1e-6)
  }
})

test_that("beta_from_mode_sd returns the uniform for an unattainable spread", {
  # The unit interval bounds how disperse a beta can be: no beta with an
  # interior mode has a standard deviation at or above 1/sqrt(12). The widest
  # available is returned rather than the request being met on another support.
  expect_equal(unname(bayesnec:::beta_from_mode_sd(0.9, 0.5)), c(1, 1))
  expect_equal(bayesnec:::beta_sd(1, 1), sqrt(1 / 12))
})

# These endpoint designs exercise cases absent from the archived prior audit.

test_that("a bot anchor survives complete effect at the highest concentration", {
  # Keep zero observations at the bot end. Removing them can empty the
  # anchor subset and relocate bot to a pooled-response quantile.
  set.seed(305)
  x <- as.numeric(rep(0:5, each = 6))
  mu <- c(41, 42, 41, 12, 1e-6, 1e-6)[x + 1]
  y <- as.numeric(rpois(length(mu), mu))
  expect_true(all(y[x == 5] == 0))
  pr <- as.data.frame(define_prior("nec4param", poisson(link = "identity"),
                                   x, y, prior_type = "regularizing"))
  a <- prior_pars(pr$prior[pr$nlpar == "bot"])
  mode <- if (a[1] > 1) (a[1] - 1) / a[2] else 0
  # a tenth of the smallest positive observation, which is the lowest level the
  # endpoint resolves; asserted as an upper bound rather than a pinned value
  expect_lt(mode, min(y[y > 0]))
  # and the prior does not exclude a bot below the smallest observation
  expect_gt(pgamma(min(y[y > 0]) / 2, a[1], a[2]), 0.02)
})

test_that("a bot anchor is the group mean, not the mean of its survivors", {
  # Filtering zeros from the bot group would inflate its mean and can
  # leave one survivor with a zero standard error.
  set.seed(305)
  x <- as.numeric(rep(0:5, each = 6))
  y <- as.numeric(c(rep(40, 24), rep(12, 6), 0, 0, 0, 0, 0, 4))
  loc <- bayesnec:::regularizing_location(x, y, "bot", zero_bounded = TRUE)
  expect_equal(loc[["location"]], mean(c(0, 0, 0, 0, 0, 4)))
  expect_gt(loc[["se"]], 0)
})

test_that("the top anchor still excludes structural zeros", {
  # The other end keeps the filter, for the reason positive_scale() records: a
  # zero at the control is a structural one, and top is the mean of the count
  # process rather than of the mixture.
  set.seed(305)
  x <- as.numeric(rep(0:5, each = 6))
  y <- as.numeric(c(0, 40, 41, 39, 0, 42, rep(40, 18), rep(5, 12)))
  loc <- bayesnec:::regularizing_location(x, y, "top", zero_bounded = TRUE)
  expect_equal(loc[["location"]], mean(c(40, 41, 39, 42)))
})

test_that("a hurdle second block reads one concentration, not three", {
  # The hurdle second block has one survival proportion per concentration.
  # Do not extend its endpoint group across most of a short dose series.
  set.seed(305)
  x <- as.numeric(rep(c(0, 0.3125, 0.625, 1.25, 2.5, 10), each = 6))
  surv <- c(0.99, 0.99, 0.95, 0.5, 0.05, 0.014)[match(x, sort(unique(x)))]
  y <- rgamma(length(x), 25, 25 / 8) * rbinom(length(x), 1, surv)
  parts <- bayesnec:::split_hurdle_response(x, y)
  expect_equal(length(parts$hu$y), length(unique(x)))
  loc <- bayesnec:::regularizing_location(parts$hu$x, parts$hu$y, "bot")
  expect_equal(loc[["location"]], parts$hu$y[which.max(parts$hu$x)])
  # and it reaches the priors the hurdle path builds
  pr <- as.data.frame(bayesnec:::define_hurdle_prior(
    "nec4param", brms::hurdle_gamma(link = "identity", link_hu = "identity"),
    x, y, prior_type = "regularizing"
  ))
  a <- prior_pars(pr$prior[pr$nlpar == "hubot"])
  expect_lt((a[1] - 1) / (a[1] + a[2] - 2), 0.1)
})

test_that("a group with no spread of its own still floors the prior", {
  # A homogeneous binary group does not estimate its proportion exactly.
  # Its zero empirical spread must not disable the uncertainty floor.
  set.seed(305)
  x <- as.numeric(rep(0:5, each = 6))
  y <- as.numeric(c(rep(1, 18), 1, 0, 1, 1, 0, 1, rep(0, 12)))
  loc <- bayesnec:::regularizing_location(x, y, "top")
  expect_equal(sd(y[x == 0]), 0)
  expect_gt(loc[["se"]], 0)
  pr <- as.data.frame(define_prior("nec4param", bernoulli(link = "identity"),
                                   x, y, prior_type = "regularizing"))
  u <- as.data.frame(define_prior("nec4param", bernoulli(link = "identity"),
                                  x, y, prior_type = "uninformative"))
  a <- prior_pars(pr$prior[pr$nlpar == "top"])
  # wider than the stated factor would give, and no wider than uninformative
  expect_gt(bayesnec:::beta_sd(a[1], a[2]),
            bayesnec:::regularizing_factor * bayesnec:::beta_sd(5, 2) * 1.01)
  expect_lte(bayesnec:::beta_sd(a[1], a[2]), bayesnec:::beta_sd(5, 2) + 1e-6)
  expect_identical(u$prior[u$nlpar == "top"], "beta(5, 2)")
})

test_that("prior_type is not inert for the ogl scale where the cap binds", {
  # When the ogl log-scale cap binds, its width must still narrow with
  # the regularizing prior type.
  set.seed(305)
  x <- runif(100, 0, 10)
  y <- c(rep(0.001, 50), runif(50, 0, 30))
  spec <- list(nlpars = "ogl", ogl = TRUE)
  scale_of <- function(pt) {
    pr <- as.data.frame(bayesnec:::define_group_prior(
      spec, x, y, prior_type = pt, ogl_transform = "log"))
    as.numeric(sub(".*, ([0-9.e+-]+)\\)$", "\\1", pr$prior[pr$class == "sd"]))
  }
  expect_equal(scale_of("uninformative"), 1 / 1 * min(
    diff(range(y)) / 10 / mean(y), 1), tolerance = 1e-3)
  expect_equal(scale_of("regularizing") / scale_of("uninformative"),
               bayesnec:::regularizing_factor, tolerance = 1e-3)
})

test_that("regularizing_entry falls back where the response has no spread", {
  # Every branch divides by the spread. A response with no spread is degenerate
  # input the fit cannot use in any case, so the entry stays well formed and
  # leaves the failure to the sampler.
  for (branch in c("normal", "gamma", "beta")) {
    loc <- switch(branch, normal = 5, gamma = 5, beta = 0.5)
    s <- bayesnec:::regularizing_entry(branch, location = loc,
                                       uninformative_sd = 0)
    expect_true(grepl(paste0("^", branch, "\\("), s), info = branch)
    expect_false(any(is.na(prior_pars(s))), info = branch)
  }
  expect_error(bayesnec:::regularizing_entry("weibull", 1, 1),
               "Unknown prior branch")
})

test_that("the uninformative entries are the ones Fisher et al. (2024) state", {
  # Reconstruct uninformative entries from their published rules,
  # independently of the helpers that build them.
  set.seed(305)
  x <- as.numeric(rep(seq(0, 10, length.out = 11), each = 6))
  cases <- list(
    list(fam = gaussian(), y = rnorm(66, 10, 2), branch = "normal"),
    list(fam = Gamma(link = "identity"), y = rgamma(66, 25, 25 / 10),
         branch = "gamma"),
    list(fam = Beta(link = "identity"), y = rbeta(66, 6, 4), branch = "beta")
  )
  for (cs in cases) {
    yl <- bayesnec:::response_link_scale(cs$y, validate_family(cs$fam))
    pr <- as.data.frame(define_prior("nec4param", cs$fam, x, cs$y,
                                     prior_type = "uninformative"))
    top <- pr$prior[pr$nlpar == "top"]
    bot <- pr$prior[pr$nlpar == "bot"]
    if (cs$branch == "normal") {
      expect_identical(top, paste0("normal(", quantile(yl, 0.9), ", ",
                                   sd(yl) * 2.5, ")"))
      expect_identical(bot, paste0("normal(", quantile(yl, 0.1), ", ",
                                   sd(yl) * 2.5, ")"))
    } else if (cs$branch == "gamma") {
      # The no-zero response makes stats::quantile() agree with the intended
      # scale rule, without calling positive_scale() in the expectation.
      expect_equal(sum(cs$y == 0), 0)
      expect_identical(top, paste0("gamma(2, ",
                                   1 / (unname(quantile(yl, 0.75)) / 2), ")"))
      expect_identical(bot, paste0("gamma(2, ",
                                   1 / ((unname(quantile(yl, 0.25)) +
                                     min(yl[yl > 0]) / 100) / 2), ")"))
    } else {
      expect_identical(top, "beta(5, 2)")
      expect_identical(bot, "beta(2, 5)")
    }
  }
})


test_that("an all-zero control does not put top at the detection floor", {
  # An all-zero control must not put top at the detection floor used for
  # a fully affected bot endpoint; that would collapse the two asymptotes.
  set.seed(305)
  x <- as.numeric(rep(0:5, each = 6))
  y <- as.numeric(rpois(36, c(40, 40, 40, 20, 6, 5)[x + 1]) *
                    rbinom(36, 1, 0.5))
  y[x == 0] <- 0
  loc <- bayesnec:::regularizing_location(x, y, "top", zero_bounded = TRUE)
  expect_equal(loc[["location"]], bayesnec:::positive_scale(y, probs = 0.95))
  expect_gt(loc[["location"]], min(y[y > 0]))
  pr <- as.data.frame(define_prior(
    "nec4param",
    brms::zero_inflated_poisson(link = "identity", link_zi = "identity"),
    x, y, prior_type = "regularizing"
  ))
  a <- prior_pars(pr$prior[pr$nlpar == "top"])
  expect_gt(pgamma(20, a[1], a[2], lower.tail = FALSE), 0.5)
  # and the two asymptotes are still distinguishable
  b <- prior_pars(pr$prior[pr$nlpar == "bot"])
  expect_gt((a[1] - 1) / a[2], (b[1] - 1) / b[2])
})

test_that("the extension limit is a fifth of the concentrations, and binds", {
  # A short unreplicated design and a hurdle survival block both supply
  # one value per concentration. The same extension limit must apply;
  # the prior can be relocated without being narrowed.
  x <- seq(0, 10, length.out = 8)
  set.seed(305)
  y <- 5 + 35 * exp(-0.5 * pmax(x - 3, 0)) + rnorm(8, 0, 3)
  loc <- bayesnec:::regularizing_location(x, y, "top")
  expect_equal(floor(0.2 * length(unique(x))), 1)
  expect_equal(loc[["location"]], y[1])
  # For this sparse design, the fallback uncertainty reaches the gamma
  # spread cap. For Gaussian data, sd(y) equals the regularizing spread
  # and leaves that width unchanged.
  r <- as.data.frame(define_prior("nec4param", Gamma(link = "identity"), x, y,
                                  prior_type = "regularizing"))
  u <- as.data.frame(define_prior("nec4param", Gamma(link = "identity"), x, y,
                                  prior_type = "uninformative"))
  ratio <- vapply(c("top", "bot"), function(np) {
    prior_moments(r$prior[r$nlpar == np])[["sd"]] /
      prior_moments(u$prior[u$nlpar == np])[["sd"]]
  }, numeric(1))
  expect_true(all(ratio >= bayesnec:::regularizing_factor - 1e-6))
  expect_true(all(ratio <= 1 + 1e-6))
  expect_gt(max(ratio), bayesnec:::regularizing_factor + 1e-3)
  # relocated even where it is not narrowed
  expect_false(identical(prior_moments(r$prior[r$nlpar == "top"])[["mode"]],
                         prior_moments(u$prior[u$nlpar == "top"])[["mode"]]))
  # a densely sampled unreplicated predictor is not limited by it
  set.seed(305)
  xd <- sort(runif(100, 0, 10))
  yd <- 5 + 35 * exp(-0.8 * pmax(xd - 4, 0)) + rnorm(100)
  expect_equal(bayesnec:::regularizing_location(xd, yd, "top")[["location"]],
               mean(yd[seq_len(5)]))
  # and a replicated design is untouched: the control group alone satisfies the
  # observation limit, so the subset is one concentration
  xr <- as.numeric(rep(seq(0, 10, length.out = 11), each = 6))
  yr <- rnorm(66, 10)
  expect_equal(bayesnec:::regularizing_location(xr, yr, "top")[["location"]],
               mean(yr[xr == min(xr)]))
})

# #389: the default top and bot priors for a rate() fit.
#
# bnec() fits on link = "identity", so brms writes a rate denominator
# multiplicatively on the response scale: the mean is the rate and top, bot and
# nec are counts per unit exposure. fit_bayesnec() built the defaults from the
# raw counts, which displaced every entry by the exposure and displaced the
# initial-value band with it, because `response` is what reaches both
# define_prior() and make_good_inits(). get_priors() and amend() already
# divided the denominator out, so the routes disagreed on the same data.

rate_prior_data <- function(seed = 389) {
  set.seed(seed)
  x <- rep(seq(0, 10, length.out = 10), 4)
  # Vary exposure so division cannot be replaced by a constant scaling.
  ex <- rep(c(1, 2, 4, 8), each = 10)
  nec3 <- function(beta, nec, top, x) {
    top * exp(-exp(beta) * (x - nec) * (x > nec))
  }
  data.frame(x = x, ex = ex,
             y = as.integer(stats::rpois(40, nec3(-0.5, 4, 20, x) * ex)))
}

# Capture the prior passed to brm(). Supply init to skip stochastic search.
# Keep this helper local because test files run in separate subprocesses.
rate_fitted_prior <- function(formula, data, family, model = "nec4param") {
  seen <- new.env(parent = emptyenv())
  local_mocked_bindings(
    brm = function(formula, data, ...) {
      seen$prior <- list(...)$prior
      structure(list(), class = "brmsfit")
    },
    are_chains_correct = function(...) TRUE,
    .package = "bayesnec"
  )
  suppressMessages(suppressWarnings(
    fit_bayesnec(formula = bnf(formula), data = data, model = model,
                 brm_args = list(family = family, init = list(list()),
                                 chains = 1, iter = 10))
  ))
  as.data.frame(seen$prior)
}

prior_entry <- function(prior, nlpar) {
  prior$prior[prior$nlpar == nlpar]
}

test_that("a rate denominator scales the default top and bot priors (#389)", {
  d <- rate_prior_data()
  fam <- validate_family("poisson")
  fitted_prior <- rate_fitted_prior(y | rate(ex) ~ crf(x, "nec4param"), d, fam)
  on_rates <- as.data.frame(define_prior("nec4param", fam, d$x, d$y / d$ex))
  on_counts <- as.data.frame(define_prior("nec4param", fam, d$x, d$y))
  for (np in c("top", "bot")) {
    expect_identical(prior_entry(fitted_prior, np), prior_entry(on_rates, np))
    expect_false(identical(prior_entry(fitted_prior, np),
                           prior_entry(on_counts, np)))
  }
  # Compare the gamma prior mean against the generating rate-scale top.
  prior_mean <- function(txt) {
    2 / as.numeric(sub("^gamma\\([^,]+,\\s*([^)]+)\\)$", "\\1", txt))
  }
  expect_equal(prior_mean(prior_entry(fitted_prior, "top")), 19.490625,
               tolerance = 1e-6)
  expect_equal(prior_mean(prior_entry(on_counts, "top")), 63.98125,
               tolerance = 1e-6)
})

test_that("a fit with no rate term is unchanged by the division (#389)", {
  # Without rate(), a NULL denominator must leave the prior unchanged.
  d <- rate_prior_data()
  fam <- validate_family("poisson")
  fitted_prior <- rate_fitted_prior(y ~ crf(x, "nec4param"), d, fam)
  plain <- as.data.frame(define_prior("nec4param", fam, d$x, d$y))
  expect_identical(fitted_prior$prior, plain$prior)
})

test_that("a rate denominator of one leaves the prior alone (#389)", {
  d <- rate_prior_data()
  d$ex <- 1
  fam <- validate_family("poisson")
  fitted_prior <- rate_fitted_prior(y | rate(ex) ~ crf(x, "nec4param"), d, fam)
  plain <- as.data.frame(define_prior("nec4param", fam, d$x, d$y))
  expect_identical(fitted_prior$prior, plain$prior)
})

test_that("bnec and get_priors agree on a rate default prior (#389)", {
  # The previewed default prior must match the prior used by a rate fit.
  d <- rate_prior_data()
  fam <- validate_family("negbinomial")
  fitted_prior <- rate_fitted_prior(y | rate(ex) ~ crf(x, "nec4param"), d, fam)
  previewed <- as.data.frame(suppressMessages(
    get_priors(y | rate(ex) ~ crf(x, "nec4param"), data = d,
               family = "negbinomial")
  ))
  expect_identical(fitted_prior$prior, previewed$prior)
})

test_that("bnec and amend build the same default rate prior (#389)", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # amend() rebuilds priors only for a newly added model. Fit one equation
  # with two chains and 400 iterations to reach that route; no posterior
  # quantity is used in the comparison.
  d <- rate_prior_data()
  fit <- suppressMessages(suppressWarnings(
    bnec(y | rate(ex) ~ crf(x, "nec3param"), data = d, family = "poisson",
         chains = 2, iter = 400, seed = 389)
  ))
  seen <- new.env(parent = emptyenv())
  local_mocked_bindings(
    brm = function(formula, data, ...) {
      seen$prior <- list(...)$prior
      structure(list(), class = "brmsfit")
    },
    are_chains_correct = function(...) TRUE,
    .package = "bayesnec"
  )
  # Capture the added model’s prior at brm(). The fake posterior makes
  # later model weighting fail, after the quantity under test is captured.
  suppressMessages(suppressWarnings(
    try(amend(fit, add = "nec4param"), silent = TRUE)
  ))
  expect_false(is.null(seen$prior))
  added <- as.data.frame(seen$prior)
  used <- as.data.frame(get_priors(fit))
  # top is the entry the two equations share; nec3param fixes bot at zero and
  # so has no entry for it.
  expect_identical(prior_entry(added, "top"), prior_entry(used, "top"))
  expect_identical(prior_entry(added, "nec"), prior_entry(used, "nec"))
})

test_that("the initial-value search reads the rate scale as well (#389)", {
  # Capture the response passed to initial-value search, avoiding the
  # stochastic search while checking it receives rates too.
  d <- rate_prior_data()
  fam <- validate_family("poisson")
  seen <- new.env(parent = emptyenv())
  local_mocked_bindings(
    brm = function(formula, data, ...) structure(list(), class = "brmsfit"),
    are_chains_correct = function(...) TRUE,
    make_good_inits = function(model, predictor, response, ...) {
      seen$response <- response
      list(random = "random")
    },
    .package = "bayesnec"
  )
  suppressMessages(suppressWarnings(
    fit_bayesnec(formula = bnf(y | rate(ex) ~ crf(x, "nec4param")), data = d,
                 model = "nec4param",
                 brm_args = list(family = fam, chains = 1, iter = 10))
  ))
  expect_equal(seen$response, response_link_scale(d$y / d$ex, fam))
  expect_false(isTRUE(all.equal(seen$response,
                                response_link_scale(d$y, fam))))
})

test_that("the amend path divides without check_data (#389)", {
  # skip_check = TRUE is the route amend() takes, and there the denominator is
  # read from the model frame because check_data() has not run. Covered
  # separately from the bnec() route, which reads it from the checked frame.
  d <- rate_prior_data()
  fam <- validate_family("poisson")
  seen <- new.env(parent = emptyenv())
  local_mocked_bindings(
    brm = function(formula, data, ...) {
      seen$prior <- list(...)$prior
      structure(list(), class = "brmsfit")
    },
    are_chains_correct = function(...) TRUE,
    make_good_inits = function(...) list(random = "random"),
    .package = "bayesnec"
  )
  suppressMessages(suppressWarnings(
    fit_bayesnec(formula = bnf(y | rate(ex) ~ crf(x, "nec4param")), data = d,
                 model = "nec4param", skip_check = TRUE,
                 brm_args = list(family = fam, chains = 1, iter = 10))
  ))
  skipped <- as.data.frame(seen$prior)
  on_rates <- as.data.frame(define_prior("nec4param", fam, d$x, d$y / d$ex))
  for (np in c("top", "bot")) {
    expect_identical(prior_entry(skipped, np), prior_entry(on_rates, np))
  }
})

# Threshold bounds follow distributional support.

test_that("the threshold priors are bounded by their own support (#393)", {
  # Keep lb = 0 for a lognormal so Stan does not propose negative values.
  # Leave the upper tail available beyond the highest tested concentration.
  x <- rep(c(0, 25, 50, 75, 100), each = 6)
  d <- data.frame(x = x, y = seq(0.9, 0.1, length.out = length(x)))
  for (ptype in c("uninformative", "regularizing")) {
    for (mod in c("nec4param", "ecx4param")) {
      np <- if (mod == "nec4param") "nec" else "ec50"
      pr <- as.data.frame(suppressMessages(get_priors(
        as.formula(sprintf("y ~ crf(x, \"%s\")", mod)), data = d,
        family = Beta(link = "identity"), prior_type = ptype
      )))
      row <- pr[pr$nlpar == np, ]
      expect_match(row$prior, "^lognormal\\(")
      expect_equal(as.numeric(row$lb), 0)
      expect_true(is.na(row$ub))
    }
  }
})

test_that("a logged predictor keeps neither threshold bound (#393)", {
  # A normal threshold prior has neither bound. Exercise both automatic
  # and explicit recognition of logged predictors.
  x_neg <- rep(log(c(0.05, 0.1, 1, 10, 100)), each = 6)
  x_pos <- rep(log(c(1, 3, 10, 30, 100)), each = 6)
  cases <- list(
    list(x = x_neg, scale = "auto"),
    list(x = x_pos, scale = "log")
  )
  for (case in cases) {
    d <- data.frame(x = case$x, y = seq(0.9, 0.1, length.out = length(case$x)))
    pr <- as.data.frame(suppressMessages(get_priors(
      y ~ crf(x, "nec4param"), data = d, family = Beta(link = "identity"),
      predictor_scale = case$scale
    )))
    row <- pr[pr$nlpar == "nec", ]
    expect_match(row$prior, "^normal\\(")
    expect_true(is.na(row$lb))
    expect_true(is.na(row$ub))
  }
})

test_that("the threshold prior places mass beyond the tested range (#393)", {
  # Compute the CDF with the actual prior bounds: removing truncation
  # changes support even when the distribution string stays identical.
  x <- rep(c(0, 0.31, 0.63, 1.25, 2.5, 5, 10, 40), each = 5)
  d <- data.frame(x = x, y = seq(0.9, 0.1, length.out = length(x)))
  pr <- as.data.frame(suppressMessages(get_priors(
    y ~ crf(x, "ecx4param"), data = d, family = gaussian()
  )))
  row <- pr[pr$nlpar == "ec50", ]
  pars <- as.numeric(strsplit(gsub("^[^(]*\\(|\\)$", "", row$prior),
                              ",[[:space:]]*")[[1]])
  # Assert bounds explicitly; the unchanged prior string alone cannot
  # detect restored tested-range truncation.
  expect_equal(as.numeric(row$lb), 0)
  expect_true(is.na(row$ub))
  # The true threshold exceeds the highest tested concentration.
  expect_gt(45, max(x))
  cdf <- function(q) stats::plnorm(q, pars[1], pars[2])
  lo <- if (is.na(row$lb)) 0 else as.numeric(row$lb)
  hi <- if (is.na(row$ub)) Inf else as.numeric(row$ub)
  mass <- (if (is.finite(hi)) cdf(hi) else 1) - cdf(lo)
  expect_lt((cdf(45) - cdf(lo)) / mass, 1)
  # The value notes/prior_audit.md part 4 reports for this cell.
  expect_equal(signif((cdf(45) - cdf(lo)) / mass, 4), 0.9795)
  # The spread rule of #314 puts the central 95 per cent of the uninformative
  # entry inside the tested range, so the mass above it is small and is not
  # zero.
  above <- 1 - cdf(max(x))
  expect_gt(above, 0)
  expect_lt(above, 0.05)
})

test_that("a single distinct predictor value builds and draws (#393)", {
  # Coincident lower and upper bounds make the rejection loop endless.
  # Use define_prior() directly: check_data() rejects this constant
  # predictor before formula-based prior construction can reach it.
  x <- rep(5, 12)
  y <- seq(0.9, 0.1, length.out = 12)
  pr <- as.data.frame(suppressMessages(
    define_prior("nec3param", validate_family("gaussian"), x, y)
  ))
  row <- pr[pr$nlpar == "nec", ]
  expect_match(row$prior, "^lognormal\\(")
  expect_equal(as.numeric(row$lb), 0)
  # This is the assertion that fails fast on a regression. The make_inits()
  # call below would hang rather than fail if the upper bound came back, since
  # the tautology it restores is an infinite while loop; do not remove this as
  # redundant.
  expect_true(is.na(row$ub))
  set.seed(393)
  inits <- bayesnec:::make_inits(
    "nec3param", c("b_top", "b_beta", "b_nec"),
    priors = pr[pr$class == "b", ], chains = 2
  )
  expect_length(inits, 2)
  expect_true(all(vapply(inits, function(z) is.finite(z$b_nec), logical(1))))
  expect_true(all(vapply(inits, function(z) z$b_nec > 0, logical(1))))
})

# Priors for a declared unobserved lower asymptote.

# Vary only the observed fraction of the curve by placing the threshold.
# Hold the dose series, replication and decay rate fixed, as in
# notes/scripts/prior_audit.R.
incomplete_design <- function(reached, family_tag, seed = 394, top = 0.9,
                              bot = 0.1) {
  x <- rep(c(0, 0.31, 0.63, 1.25, 2.5, 5, 10, 40), each = 5)
  rate <- 2.5
  nec <- max(x) + log(1 - reached) / rate
  mu <- bot + (top - bot) *
    exp(-rate * (x - nec) * ifelse(x - nec < 0, 0, 1))
  set.seed(seed)
  y <- switch(
    family_tag,
    gaussian = stats::rnorm(length(mu), mu, 0.04),
    Gamma = stats::rgamma(length(mu), 25, 25 / mu),
    poisson = as.integer(stats::rpois(length(mu), mu * 40)),
    negbinomial = as.integer(stats::rnbinom(length(mu), mu = mu * 40,
                                            size = 5)),
    Beta = stats::rbeta(length(mu), mu * 20, (1 - mu) * 20),
    bernoulli = as.integer(stats::rbinom(length(mu), 1, mu)),
    as.integer(stats::rbinom(length(mu), 20, mu))
  )
  data.frame(x = x, y = y, trials = 20)
}

incomplete_formula <- function(family_tag, model = "nec4param") {
  if (family_tag %in% c("binomial", "beta_binomial")) {
    stats::as.formula(sprintf("y | trials(trials) ~ crf(x, \"%s\")", model))
  } else {
    stats::as.formula(sprintf("y ~ crf(x, \"%s\")", model))
  }
}

# The bot distribution already respects its family support, so evaluate
# its quantiles without additional truncation.
prior_quantiles <- function(txt, probs = c(0.025, 0.975)) {
  fam <- sub("\\(.*$", "", txt)
  args <- as.numeric(strsplit(gsub("^[^(]*\\(|\\)$", "", txt),
                              ",[[:space:]]*")[[1]])
  switch(fam,
         normal = stats::qnorm(probs, args[1], args[2]),
         gamma = stats::qgamma(probs, args[1], args[2]),
         beta = stats::qbeta(probs, args[1], args[2]),
         stop("unhandled prior family ", fam))
}

prior_args <- function(txt) {
  as.numeric(strsplit(gsub("^[^(]*\\(|\\)$", "", txt), ",[[:space:]]*")[[1]])
}

test_that("the bot prior spans the floor to the endpoint mean (#394)", {
  # Check coverage by branch, against the floor and endpoint mean. The
  # gamma rule fixes only the upper endpoint; its lower quantile remains
  # positive and needs a different assertion.
  cases <- list(
    list(tag = "gaussian", branch = "normal"),
    list(tag = "Gamma", branch = "gamma"),
    list(tag = "poisson", branch = "gamma"),
    list(tag = "negbinomial", branch = "gamma"),
    list(tag = "Beta", branch = "beta"),
    list(tag = "bernoulli", branch = "beta"),
    list(tag = "binomial", branch = "beta"),
    list(tag = "beta_binomial", branch = "beta")
  )
  for (case in cases) {
    d <- incomplete_design(0.36, case$tag)
    fam <- validate_family(case$tag)
    for (ptype in c("uninformative", "regularizing")) {
      pr <- as.data.frame(suppressWarnings(suppressMessages(get_priors(
        incomplete_formula(case$tag), data = d, family = fam,
        prior_type = ptype, asymptote_observed = FALSE))))
      entry <- pr$prior[pr$nlpar == "bot"]
      expect_match(entry, paste0("^", case$branch, "\\("),
                   info = paste(case$tag, ptype))
      qs <- prior_quantiles(entry)
      # The endpoint mean the entry is built from, read through the same
      # anchor define_prior() uses so that the assertion is about the rule and
      # not about a second implementation of the endpoint.
      y <- d$y
      if (case$tag %in% c("binomial", "beta_binomial")) {
        y <- y / d$trials
      }
      yl <- response_link_scale(y, fam)
      e <- bayesnec:::regularizing_location(
        d$x, yl, "bot", zero_bounded = bayesnec:::zero_bounded_family(fam)
      )[["location"]]
      # The anchor is read through the implementation's own call, so this pins
      # what that call returns on this design: the mean over the observations
      # at the highest predictor value, which is what the rule names.
      expect_equal(e, mean(yl[d$x == max(d$x)]), info = paste(case$tag, ptype))
      # Stated per branch, because the three meet the rule to different
      # degrees and a single tolerance would hide which.
      if (identical(case$branch, "gamma")) {
        # With the 97.5th percentile at the endpoint mean and the shape fixed
        # at 2, the 2.5th is qgamma(0.025, 2) / qgamma(0.975, 2) of it.
        expect_match(entry, "^gamma\\(2, ", info = paste(case$tag, ptype))
        expect_equal(qs[2], e, tolerance = 1e-5)
        expect_equal(qs[1] / e,
                     stats::qgamma(0.025, 2) / stats::qgamma(0.975, 2),
                     tolerance = 1e-5)
        expect_equal(qs[1] / e, 0.0435, tolerance = 1e-3)
      } else if (identical(case$branch, "beta")) {
        expect_equal(qs[2], e, tolerance = 1e-5)
        expect_lt(qs[1], 0.05 * e)
      } else {
        # Exactly [0, e] up to the six significant figures the entry is
        # written to.
        expect_lt(abs(qs[1]), 1e-5 * e)
        expect_equal(qs[2], e, tolerance = 1e-5)
      }
      # The declaration governs bot and nothing else, so the entry either
      # prior_type produced for every other parameter survives unchanged.
      base <- as.data.frame(suppressWarnings(suppressMessages(get_priors(
        incomplete_formula(case$tag), data = d, family = fam,
        prior_type = ptype))))
      expect_identical(pr$prior[pr$nlpar != "bot"],
                       base$prior[base$nlpar != "bot"],
                       info = paste(case$tag, ptype))
    }
  }
})

test_that("every gamma-branch family ends its bot prior at the endpoint (#449)", {
  # Zero-inflated count anchors include endpoint zeros. Hurdle mu anchors
  # exclude them; both use the gamma entry.
  set.seed(449)
  x <- rep(c(0, 1, 2, 4, 8, 16), each = 5)
  mu <- 20 * exp(-0.1 * x) + 2
  counts <- as.integer(stats::rpois(length(x), mu))
  positive <- stats::rgamma(length(x), 20, 20 / (mu / 20))
  zeros <- c(7, 13, 18, length(x))
  counts[zeros] <- 0L
  positive[zeros] <- 0
  top_level <- x == max(x)
  ratio <- stats::qgamma(0.025, 2) / stats::qgamma(0.975, 2)
  for (tag in c("zero_inflated_poisson", "zero_inflated_negbinomial",
                "hurdle_poisson", "hurdle_negbinomial", "hurdle_gamma")) {
    y <- if (identical(tag, "hurdle_gamma")) positive else counts
    d <- data.frame(x = x, y = y)
    e <- if (grepl("^hurdle_", tag)) {
      mean(y[top_level & y > 0])
    } else {
      mean(y[top_level])
    }
    for (ptype in c("uninformative", "regularizing")) {
      pr <- as.data.frame(suppressWarnings(suppressMessages(get_priors(
        y ~ crf(x, "nec4param"), data = d, family = tag, prior_type = ptype,
        asymptote_observed = FALSE))))
      entry <- pr$prior[pr$nlpar == "bot"]
      expect_match(entry, "^gamma\\(2, ", info = paste(tag, ptype))
      qs <- prior_quantiles(entry)
      expect_equal(qs[2], e, tolerance = 1e-5, info = paste(tag, ptype))
      expect_equal(qs[1] / e, ratio, tolerance = 1e-5,
                   info = paste(tag, ptype))
      # The survival block of a hurdle family is a probability, and its bot
      # prior stays on the beta branch.
      hubot <- pr$prior[pr$nlpar == "hubot"]
      if (grepl("^hurdle_", tag)) {
        expect_match(hubot, "^beta\\(1, ", info = paste(tag, ptype))
      } else {
        expect_length(hubot, 0)
      }
    }
  }
})

test_that("the declared prior covers a bot the released sets exclude (#394)", {
  # On the flattest Gaussian design, check that the declaration admits the
  # true bot excluded by both default prior sets.
  d <- incomplete_design(0.02, "gaussian", seed = 386)
  bot_entry <- function(ptype, declared) {
    pr <- as.data.frame(suppressWarnings(suppressMessages(get_priors(
      y ~ crf(x, "nec4param"), data = d, family = gaussian(),
      prior_type = ptype, asymptote_observed = declared))))
    pr$prior[pr$nlpar == "bot"]
  }
  cdf_at <- function(txt, q) {
    a <- prior_args(txt)
    stats::pnorm(q, a[1], a[2])
  }
  for (ptype in c("uninformative", "regularizing")) {
    expect_lt(cdf_at(bot_entry(ptype, TRUE), 0.1), 1e-10)
    declared <- cdf_at(bot_entry(ptype, FALSE), 0.1)
    expect_gt(declared, 0.025)
    expect_lt(declared, 0.975)
  }
})

test_that("the regularizing cap does not hold the declared entry (#394)", {
  # A declared unobserved bot needs its own spread construction: the
  # regularizing cap would otherwise retain a prior that excludes the truth.
  d <- incomplete_design(0.02, "gaussian", seed = 386)
  bot_entry <- function(ptype, declared) {
    pr <- as.data.frame(suppressWarnings(suppressMessages(get_priors(
      y ~ crf(x, "nec4param"), data = d, family = gaussian(),
      prior_type = ptype, asymptote_observed = declared))))
    pr$prior[pr$nlpar == "bot"]
  }
  spread <- function(txt) prior_args(txt)[2]
  declared <- spread(bot_entry("uninformative", FALSE))
  expect_gt(declared, spread(bot_entry("uninformative", TRUE)))
  expect_gt(declared, spread(bot_entry("regularizing", TRUE)))
  # The two prior types give the same declared entry, because the rule
  # replaces the entry rather than narrowing whatever produced it.
  expect_identical(bot_entry("uninformative", FALSE),
                   bot_entry("regularizing", FALSE))
})

test_that("a response with no derivable floor is refused (#394)", {
  # Without a derivable Gaussian floor, report the missing prior rather
  # than inventing a bound for a log ratio or growth increment.
  d <- incomplete_design(0.36, "gaussian")
  d$y <- d$y - 1.2
  expect_true(any(d$y < 0))
  err <- expect_error(suppressWarnings(suppressMessages(get_priors(
    y ~ crf(x, "nec4param"), data = d, family = gaussian(),
    asymptote_observed = FALSE))))
  expect_match(conditionMessage(err), "\"bot\"", fixed = TRUE)
  expect_match(conditionMessage(err), "`prior` argument", fixed = TRUE)
  expect_match(conditionMessage(err), "spanning negative values", fixed = TRUE)
  # The same response under the default builds as it always did.
  built <- suppressWarnings(suppressMessages(get_priors(
    y ~ crf(x, "nec4param"), data = d, family = gaussian())))
  expect_s3_class(built, "brmsprior")
})

test_that("a non-identity link is refused as well (#394)", {
  # On a log or logit mean scale the support floor maps to -Inf. Even
  # non-negative logged responses provide no finite floor: they may only
  # reflect a change of response units.
  d <- incomplete_design(0.36, "gaussian")
  d$y <- d$y * 10
  err <- expect_error(suppressWarnings(suppressMessages(get_priors(
    y ~ crf(x, "nec4param"), data = d, family = Gamma(link = "log"),
    asymptote_observed = FALSE))))
  expect_match(conditionMessage(err), "\"log\" link", fixed = TRUE)
  expect_match(conditionMessage(err), "`prior` argument", fixed = TRUE)
})

test_that("the default leaves every generated entry bit-identical (#394)", {
  # Pin prior strings captured before the declaration was added, including
  # their rounding. Fixture provenance is in the test reasoning notes.
  released <- c(
    "gaussian|uninformative|top" =
      "normal(0.95277185372616, 0.697777486264628)",
    "gaussian|uninformative|bot" =
      "normal(0.161825553470704, 0.697777486264628)",
    "gaussian|uninformative|nec" =
      "lognormal(0.916290731874155, 1.41461207660427)",
    "gaussian|regularizing|top" =
      "normal(0.926694327915049, 0.279110994505851)",
    "gaussian|regularizing|bot" =
      "normal(0.0836645059283788, 0.279110994505851)",
    "gaussian|regularizing|nec" =
      "lognormal(0.916290731874155, 1.19182034345698)",
    "Gamma|uninformative|top" =
      "gamma(2, 2.00559762731528)",
    "Gamma|uninformative|bot" =
      "gamma(2, 2.68398226979542)",
    "Gamma|uninformative|nec" =
      "lognormal(0.916290731874155, 1.41461207660427)",
    "Gamma|regularizing|top" =
      "gamma(14.6468, 13.5687)",
    "Gamma|regularizing|bot" =
      "gamma(1.60172, 6.00479)",
    "Gamma|regularizing|nec" =
      "lognormal(0.916290731874155, 1.19182034345698)",
    "poisson|uninformative|top" =
      "gamma(2, 0.0512820512820513)",
    "poisson|uninformative|bot" =
      "gamma(2, 0.064998375040624)",
    "poisson|uninformative|nec" =
      "lognormal(0.916290731874155, 1.41461207660427)",
    "poisson|regularizing|top" =
      "gamma(14.8201, 0.348992)",
    "poisson|regularizing|bot" =
      "gamma(1.80208, 0.154246)",
    "poisson|regularizing|nec" =
      "lognormal(0.916290731874155, 1.19182034345698)",
    "negbinomial|uninformative|top" =
      "gamma(2, 0.0392253003187056)",
    "negbinomial|uninformative|bot" =
      "gamma(2, 0.100037514067775)",
    "negbinomial|uninformative|nec" =
      "lognormal(0.916290731874155, 1.41461207660427)",
    "negbinomial|regularizing|top" =
      "gamma(11.2195, 0.232262)",
    "negbinomial|regularizing|bot" =
      "gamma(1.93472, 0.245978)",
    "negbinomial|regularizing|nec" =
      "lognormal(0.916290731874155, 1.19182034345698)",
    "Beta|uninformative|top" =
      "beta(5, 2)",
    "Beta|uninformative|bot" =
      "beta(2, 5)",
    "Beta|uninformative|nec" =
      "lognormal(0.916290731874155, 1.41461207660427)",
    "Beta|regularizing|top" =
      "beta(30.2922, 7.41633)",
    "Beta|regularizing|bot" =
      "beta(3.45027, 23.1799)",
    "Beta|regularizing|nec" =
      "lognormal(0.916290731874155, 1.19182034345698)",
    "bernoulli|uninformative|top" =
      "beta(5, 2)",
    "bernoulli|uninformative|bot" =
      "beta(2, 5)",
    "bernoulli|uninformative|nec" =
      "lognormal(0.916290731874155, 1.41461207660427)",
    "bernoulli|regularizing|top" =
      "beta(4.18661, 1.03186)",
    "bernoulli|regularizing|bot" =
      "beta(1.03219, 4.18707)",
    "bernoulli|regularizing|nec" =
      "lognormal(0.916290731874155, 1.19182034345698)",
    "binomial|uninformative|top" =
      "beta(5, 2)",
    "binomial|uninformative|bot" =
      "beta(2, 5)",
    "binomial|uninformative|nec" =
      "lognormal(0.916290731874155, 1.41461207660427)",
    "binomial|regularizing|top" =
      "beta(25.2469, 4.32008)",
    "binomial|regularizing|bot" =
      "beta(3.09767, 22.2152)",
    "binomial|regularizing|nec" =
      "lognormal(0.916290731874155, 1.19182034345698)",
    "beta_binomial|uninformative|top" =
      "beta(5, 2)",
    "beta_binomial|uninformative|bot" =
      "beta(2, 5)",
    "beta_binomial|uninformative|nec" =
      "lognormal(0.916290731874155, 1.41461207660427)",
    "beta_binomial|regularizing|top" =
      "beta(25.2469, 4.32008)",
    "beta_binomial|regularizing|bot" =
      "beta(3.09767, 22.2152)",
    "beta_binomial|regularizing|nec" =
      "lognormal(0.916290731874155, 1.19182034345698)"
  )
  tags <- c("gaussian", "Gamma", "poisson", "negbinomial", "Beta",
            "bernoulli", "binomial", "beta_binomial")
  for (tag in tags) {
    # A complete design, at the `complete` setting of the prior audit: the
    # decay rate is held and the threshold placed so that exp(-5) of the span
    # from top to bot is left untravelled at the highest predictor value.
    d <- incomplete_design(1 - exp(-5), tag, seed = 394)
    fam <- validate_family(tag)
    for (ptype in c("uninformative", "regularizing")) {
      pr <- as.data.frame(suppressWarnings(suppressMessages(get_priors(
        incomplete_formula(tag), data = d, family = fam,
        prior_type = ptype))))
      for (np in c("top", "bot", "nec")) {
        key <- paste(tag, ptype, np, sep = "|")
        expect_identical(pr$prior[pr$nlpar == np], unname(released[[key]]),
                         info = key)
      }
    }
  }
})

test_that("bnec refuses the declaration before its model loop (#394)", {
  # Refuse before the model loop: an error inside its try() would be
  # repeated per equation and replaced by generic all-models-failed advice.
  d <- incomplete_design(0.36, "gaussian")
  d$y <- d$y - 1.2
  err <- expect_error(suppressWarnings(suppressMessages(bnec(
    y ~ crf(x, c("nec3param", "nec4param")), data = d,
    asymptote_observed = FALSE, chains = 1, iter = 10))))
  expect_match(conditionMessage(err), "`prior` argument", fixed = TRUE)
  expect_false(grepl("None of the models fit successfully",
                     conditionMessage(err), fixed = TRUE))
})

test_that("a mixed model set is reported under the declaration (#394)", {
  # Report a mixed set when the unobserved region prevents distinguishing
  # equations that estimate bot from those that do not. Whether an endpoint
  # can reach zero cannot be inferred from this incomplete design.
  d <- incomplete_design(0.36, "gaussian")
  bdat <- stats::model.frame(bnf(y ~ crf(x, c("nec3param", "nec4param"))),
                             data = d)
  fam <- validate_family("gaussian")
  msg <- tryCatch(
    bayesnec:::check_asymptote_declaration(
      bdat, fam, c("nec3param", "nec4param"), asymptote_observed = FALSE),
    message = conditionMessage)
  expect_identical(
    msg,
    paste0(
      "This set mixes equations that estimate a lower asymptote with ones that",
      " have no lower asymptote to estimate. With the asymptote unobserved the",
      " fit cannot distinguish them. Use `model = \"zero_bounded\"` if the",
      " response can reach zero for this endpoint; otherwise, exclude the",
      " equations in `models()$bot_free`.\n"
    )
  )
  # neclin, neclinhorme and ecxlin decay by subtraction and are unbounded
  # below, so the report must not claim that every bot_free equation falls to
  # zero. All three are kept for a gaussian family.
  expect_false(grepl("falls to zero", msg, fixed = TRUE))
  # A set on one side of the divide says nothing, and neither does the default.
  expect_silent(bayesnec:::check_asymptote_declaration(
    bdat, fam, c("nec3param", "ecxexp"), asymptote_observed = FALSE))
  expect_silent(bayesnec:::check_asymptote_declaration(
    bdat, fam, c("nec4param", "ecx4param"), asymptote_observed = FALSE))
  expect_silent(bayesnec:::check_asymptote_declaration(
    bdat, fam, c("nec3param", "nec4param")))
})

test_that("bnec and get_priors validate the declaration (#394)", {
  d <- incomplete_design(0.36, "gaussian")
  expect_error(suppressWarnings(suppressMessages(get_priors(
    y ~ crf(x, "nec4param"), data = d, family = gaussian(),
    asymptote_observed = NA))), "flag")
  expect_error(suppressWarnings(suppressMessages(bnec(
    y ~ crf(x, "nec4param"), data = d, asymptote_observed = "no",
    chains = 1, iter = 10))), "flag")
})

test_that("a fit is built with the declared prior (#394)", {
  # Capture the prior passed to brm(), with init supplied and the fit mocked.
  d <- incomplete_design(0.36, "gaussian")
  fam <- validate_family("gaussian")
  seen <- new.env(parent = emptyenv())
  local_mocked_bindings(
    brm = function(formula, data, ...) {
      seen$prior <- list(...)$prior
      structure(list(), class = "brmsfit")
    },
    are_chains_correct = function(...) TRUE,
    .package = "bayesnec"
  )
  suppressMessages(suppressWarnings(
    fit_bayesnec(formula = bnf(y ~ crf(x, "nec4param")), data = d,
                 model = "nec4param", asymptote_observed = FALSE,
                 brm_args = list(family = fam, init = list(list()),
                                 chains = 1, iter = 10))
  ))
  used <- as.data.frame(seen$prior)
  declared <- as.data.frame(suppressWarnings(suppressMessages(get_priors(
    y ~ crf(x, "nec4param"), data = d, family = fam,
    asymptote_observed = FALSE))))
  expect_identical(used$prior, declared$prior)
  default <- as.data.frame(suppressWarnings(suppressMessages(get_priors(
    y ~ crf(x, "nec4param"), data = d, family = fam))))
  expect_false(identical(prior_entry(used, "bot"),
                         prior_entry(default, "bot")))
})

test_that("amend honours the declaration for an added model (#394)", {
  # Mock both amend() rebuild and refit to check the declaration reaches
  # each route for an added model.
  seen <- new.env(parent = emptyenv())
  local_mocked_bindings(
    define_prior = function(model, family, predictor, response,
                            prior_type = "uninformative",
                            asymptote_observed = TRUE, ...) {
      seen$prior_declared <- asymptote_observed
      brms::prior_string("normal(0, 5)", nlpar = "top")
    },
    fit_bayesnec = function(..., asymptote_observed = TRUE) {
      seen$fit_declared <- asymptote_observed
      stop("not fitted in this test", call. = FALSE)
    },
    .package = "bayesnec"
  )
  for (declared in c(TRUE, FALSE)) {
    seen$prior_declared <- NULL
    seen$fit_declared <- NULL
    suppressMessages(suppressWarnings(try(
      amend(manec_example, add = "nechorme4",
            asymptote_observed = declared),
      silent = TRUE
    )))
    expect_identical(seen$prior_declared, declared)
    expect_identical(seen$fit_declared, declared)
  }
})

test_that("amend refuses a response with no derivable floor (#394)", {
  # amend() must also refuse an undeclared floor: its stored example
  # spans negative responses and cannot supply the requested bot prior.
  err <- expect_error(suppressMessages(suppressWarnings(
    amend(manec_example, add = "nechorme4", asymptote_observed = FALSE)
  )))
  expect_match(conditionMessage(err), "`prior` argument", fixed = TRUE)
})

test_that("the declaration reaches both blocks of a hurdle fit (#394)", {
  # The design misses both hurdle asymptotes. Replace both bot entries
  # from their own response blocks and preserve all other entries.
  set.seed(394)
  x <- rep(c(0, 0.31, 0.63, 1.25, 2.5, 5, 10, 40), each = 5)
  mu <- 2 + 8 * exp(-2.5 * (x - 39.82) * ifelse(x - 39.82 < 0, 0, 1))
  d <- data.frame(
    x = x,
    y = stats::rgamma(length(mu), 25, 25 / mu) *
      stats::rbinom(length(mu), 1, 0.8)
  )
  fam <- validate_family("hurdle_gamma")
  entries <- function(declared) {
    pr <- as.data.frame(suppressWarnings(suppressMessages(get_priors(
      y ~ crf(x, "nec4param"), data = d, family = fam,
      asymptote_observed = declared))))
    stats::setNames(pr$prior, pr$nlpar)
  }
  default <- entries(TRUE)
  declared <- entries(FALSE)
  # The mu block's bot is on the gamma branch and the survival block's on the
  # beta branch, so the two take different constructions from one declaration.
  expect_match(declared[["bot"]], "^gamma\\(2, ")
  expect_match(declared[["hubot"]], "^beta\\(1, ")
  expect_false(identical(declared[["bot"]], default[["bot"]]))
  expect_false(identical(declared[["hubot"]], default[["hubot"]]))
  moved <- c("bot", "hubot")
  expect_identical(declared[setdiff(names(declared), moved)],
                   default[setdiff(names(default), moved)])
})

test_that("the declaration reads a zero-inflated count block (#394)", {
  # prior_family_tag() rewrites zero_inflated_poisson onto the poisson entries,
  # because the mixture changes how many zeros are observed and not the scale
  # of mu, so the declaration takes the gamma branch there as well.
  set.seed(394)
  x <- rep(c(0, 0.31, 0.63, 1.25, 2.5, 5, 10, 40), each = 5)
  mu <- 8 + 32 * exp(-2.5 * (x - 39.82) * ifelse(x - 39.82 < 0, 0, 1))
  d <- data.frame(
    x = x,
    y = as.integer(stats::rpois(length(mu), mu) *
                     stats::rbinom(length(mu), 1, 0.8))
  )
  fam <- validate_family("zero_inflated_poisson")
  bot_of <- function(declared) {
    pr <- as.data.frame(suppressWarnings(suppressMessages(get_priors(
      y ~ crf(x, "nec4param"), data = d, family = fam,
      asymptote_observed = declared))))
    pr$prior[pr$nlpar == "bot"]
  }
  expect_match(bot_of(FALSE), "^gamma\\(2, ")
  expect_false(identical(bot_of(FALSE), bot_of(TRUE)))
})

test_that("the declared entry is built on the rate scale (#394)", {
  # bnec() fits on the identity link, so brms writes a rate() denominator
  # multiplicatively and the mean is the rate. The endpoint mean the entry is
  # built from is therefore a rate, which #389 established for the released
  # entries and this asserts for the declared one.
  set.seed(394)
  x <- rep(c(0, 0.31, 0.63, 1.25, 2.5, 5, 10, 40), each = 5)
  ex <- rep(c(1, 2, 4, 8), length.out = length(x))
  mu <- 6 + 14 * exp(-2.5 * (x - 39.82) * ifelse(x - 39.82 < 0, 0, 1))
  d <- data.frame(x = x, ex = ex,
                  y = as.integer(stats::rpois(length(mu), mu * ex)))
  fam <- validate_family("poisson")
  on_rates <- as.data.frame(define_prior("nec4param", fam, d$x, d$y / d$ex,
                                         asymptote_observed = FALSE))
  on_counts <- as.data.frame(define_prior("nec4param", fam, d$x, d$y,
                                          asymptote_observed = FALSE))
  fitted_prior <- as.data.frame(suppressWarnings(suppressMessages(get_priors(
    y | rate(ex) ~ crf(x, "nec4param"), data = d, family = "poisson",
    asymptote_observed = FALSE))))
  expect_identical(prior_entry(fitted_prior, "bot"),
                   prior_entry(on_rates, "bot"))
  expect_false(identical(prior_entry(fitted_prior, "bot"),
                         prior_entry(on_counts, "bot")))
})

test_that("a complete supplied bot prior is not refused (#394)", {
  # A complete supplied prior bypasses default construction. Gate early
  # refusal on whether a bot row will actually be generated.
  d <- incomplete_design(0.36, "gaussian")
  d$y <- d$y - 1.2
  fam <- validate_family("gaussian")
  bdat <- stats::model.frame(bnf(y ~ crf(x, "nec4param")), data = d)
  complete <- suppressWarnings(suppressMessages(get_priors(
    y ~ crf(x, "nec4param"), data = incomplete_design(0.36, "gaussian"),
    family = fam)))
  # A single equation, so the mixed-set report cannot fire and silence is the
  # whole outcome.
  expect_silent(bayesnec:::check_asymptote_declaration(
    bdat, fam, "nec4param", asymptote_observed = FALSE, prior = complete))
  # Omitting the bot row alone restores the refusal.
  partial <- complete[as.data.frame(complete)$nlpar != "bot", ]
  expect_error(
    bayesnec:::check_asymptote_declaration(
      bdat, fam, "nec4param", asymptote_observed = FALSE, prior = partial),
    "`prior` argument", fixed = TRUE)
  # And a set whose equations have no bot parameter generates no bot row, so
  # there is nothing to refuse either here or in define_prior(). Both are
  # bot_free, so the mixed-set report is silent for that reason as well.
  expect_silent(bayesnec:::check_asymptote_declaration(
    bdat, fam, c("nec3param", "ecxexp"), asymptote_observed = FALSE))
  built <- suppressWarnings(suppressMessages(
    define_prior("nec3param", fam, d$x, d$y, asymptote_observed = FALSE)))
  expect_s3_class(built, "brmsprior")
})

test_that("a supplied prior does not silence the mixed-set report (#394)", {
  # Supplying a bot prior does not make the unobserved region informative.
  # It must suppress only refusal, leaving the mixed-set report intact.
  d <- incomplete_design(0.36, "gaussian")
  fam <- validate_family("gaussian")
  bdat <- stats::model.frame(bnf(y ~ crf(x, c("nec3param", "nec4param"))),
                             data = d)
  complete <- suppressWarnings(suppressMessages(get_priors(
    y ~ crf(x, "nec4param"), data = d, family = fam)))
  expect_message(
    bayesnec:::check_asymptote_declaration(
      bdat, fam, c("nec3param", "nec4param"), asymptote_observed = FALSE,
      prior = complete),
    "model = \"zero_bounded\"", fixed = TRUE)
  # And it still raises no refusal, which is what the gate is for.
  expect_no_error(suppressMessages(bayesnec:::check_asymptote_declaration(
    bdat, fam, c("nec3param", "nec4param"), asymptote_observed = FALSE,
    prior = complete)))
})

test_that("a response with no value above the floor is refused early (#394)", {
  # Raise the no-positive-response refusal before the model loop can
  # replace it with generic all-models-failed advice.
  d <- incomplete_design(0.36, "gaussian")
  d$y <- 0
  fam <- validate_family("gaussian")
  bdat <- stats::model.frame(bnf(y ~ crf(x, "nec4param")), data = d)
  err <- expect_error(bayesnec:::check_asymptote_declaration(
    bdat, fam, "nec4param", asymptote_observed = FALSE))
  expect_match(conditionMessage(err), "no value above that floor", fixed = TRUE)
  expect_match(conditionMessage(err), "`prior` argument", fixed = TRUE)
  # For a zero-bounded family, report the declaration refusal instead of
  # the lower-level positive_scale() error.
  pois <- validate_family("poisson")
  d$y <- as.integer(d$y)
  bpois <- stats::model.frame(bnf(y ~ crf(x, "nec4param")), data = d)
  err2 <- expect_error(bayesnec:::check_asymptote_declaration(
    bpois, pois, "nec4param", asymptote_observed = FALSE))
  expect_match(conditionMessage(err2), "asymptote_observed", fixed = TRUE)
  expect_match(conditionMessage(err2), "no value above that floor",
               fixed = TRUE)
})

test_that("an endpoint group at the floor is not refused (#394)", {
  # Distinguish an endpoint with no positive observations from a whole
  # response with none. The endpoint can use a tenth of the smallest
  # positive observation elsewhere as its fallback scale.
  d <- incomplete_design(0.36, "Gamma")
  d$y[d$x == max(d$x)] <- 0
  fam <- validate_family("Gamma")
  yl <- response_link_scale(d$y, fam)
  expect_gt(bayesnec:::unobserved_endpoint_mean(d$x, yl, TRUE), 0)
  entry <- as.data.frame(suppressWarnings(suppressMessages(get_priors(
    y ~ crf(x, "nec4param"), data = d, family = fam,
    asymptote_observed = FALSE))))
  expect_match(entry$prior[entry$nlpar == "bot"], "^gamma\\(2, ")
})

test_that("the refusal counts the observations below the floor (#394)", {
  # One reading below zero prevents deriving a non-negative floor.
  # Report the count so a stray reading differs from an entirely negative
  # response.
  d <- incomplete_design(0.36, "gaussian")
  d$y[1] <- -0.001
  expect_true(min(d$y) < 0 && sum(d$y < 0) == 1)
  err <- expect_error(suppressWarnings(suppressMessages(get_priors(
    y ~ crf(x, "nec4param"), data = d, family = gaussian(),
    asymptote_observed = FALSE))))
  expect_match(conditionMessage(err), "1 of 40 observations are below zero",
               fixed = TRUE)
  expect_match(conditionMessage(err), "smallest being -0.001", fixed = TRUE)
})

test_that("the flatness report offers the declaration only where usable (#394)",
{
  # The report recommends the declaration, and the declaration is refused on a
  # response with no derivable floor. Recommending it there would send the user
  # to a hard error, so the sentence is omitted.
  d <- incomplete_design(0.36, "gaussian")
  expect_match(bayesnec:::declaration_advice(validate_family("gaussian"), d$y),
               "asymptote_observed = FALSE", fixed = TRUE)
  expect_identical(
    bayesnec:::declaration_advice(validate_family("gaussian"), d$y - 1.2), "")
  expect_identical(
    bayesnec:::declaration_advice(Gamma(link = "log"), d$y * 10), "")
})

# Family objects recovered from a stored fit.

test_that("a Gamma family read off a fit builds the priors bnec() builds (#436)", {
  # brms records Gamma as gamma, and amend() builds priors from that family.
  stored <- brms::bf(y ~ x, family = Gamma(link = "identity"))$family
  expect_identical(stored$family, "gamma")
  expect_identical(prior_family_tag(stored), "Gamma")
  expect_true(zero_bounded_family(stored))
  expect_identical(asymptote_floor(stored, nec_data$y), 0)
  validated <- validate_family("Gamma")
  x <- nec_data$x
  y <- nec_data$y * 10
  models <- suppressMessages(check_models(mod_groups$all, validated))
  specs <- list(none = NULL, power = list(route = "B", value = "power"),
                loglinear = list(route = "B", value = "loglinear"))
  for (m in models) for (pt in c("uninformative", "regularizing")) {
    for (s in names(specs)) {
      expect_identical(
        define_prior(m, stored, x, y, prior_type = pt, disp_spec = specs[[s]]),
        define_prior(m, validated, x, y, prior_type = pt,
                     disp_spec = specs[[s]]),
        label = paste(m, pt, s)
      )
    }
  }
})

# The disp() backstop on the single-block branch.

test_that("define_prior refuses a disp() term the family cannot take (#445)", {
  power <- list(route = "B", value = "power")
  expect_error(
    define_prior("nec3param", validate_family("poisson"), nec_data$x,
                 nec_data$count, disp_spec = power),
    "Family poisson has no free dispersion parameter", fixed = TRUE
  )
  expect_error(
    define_prior("nec3param", validate_family("bernoulli"), nec_data$x,
                 as.integer(nec_data$y > 0.5),
                 disp_spec = list(route = "A", value = "x")),
    "Family bernoulli has no free dispersion parameter", fixed = TRUE
  )
  expect_error(
    define_prior("nec3param", validate_family("gaussian"), nec_data$x,
                 nec_data$y, disp_spec = list(route = "B", value = "twosided")),
    "Variance function \"twosided\" is not valid for the gaussian family",
    fixed = TRUE
  )
  # A term the family takes is unchanged.
  pr <- define_prior("nec3param", validate_family("gaussian"), nec_data$x,
                     nec_data$y, disp_spec = power)
  expect_setequal(pr$nlpar, c("beta", "top", "nec", "c0", "c1"))
})
