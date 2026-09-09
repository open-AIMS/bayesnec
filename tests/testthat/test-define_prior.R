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

# #229: the #210 guard was evaluated for every family, not only the ones whose
# priors use it, so a response with no positive values errored even where the
# family's own top/bot priors are perfectly well defined on it.

test_that("a gaussian response with no positive values still builds priors", {
  # Ordinary gaussian input: log ratios, growth increments, anything expressed
  # as a change. The gaussian entries in the prior tables come from quantile()
  # and sd() and never read the gamma-scaled strings.
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
  # The #210 behaviour must survive: for these three the gamma rate genuinely
  # cannot be placed, so the informative error is still the right answer.
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
  # #207 made add_brm_defaults() build the defaults unconditionally, so a
  # failure inside define_prior() became a hard stop even for a user who had
  # supplied a complete set of their own. An all-zero poisson response is the
  # case that reaches it.
  x <- as.numeric(rep(1:10, each = 5))
  y <- rep(0, length(x))
  up <- brms::prior_string("normal(0, 5)", nlpar = "beta") +
    brms::prior_string("gamma(2, 0.1)", nlpar = "top", lb = 0) +
    brms::prior_string("gamma(2, 0.5)", nlpar = "bot", lb = 0) +
    brms::prior_string("gamma(5, 0.36)", nlpar = "nec", lb = 1, ub = 10)
  # `init` is supplied so the initial-value search is skipped. It is not being
  # avoided for speed: on an all-zero response no draw can put the curve inside
  # the response range, so make_good_inits() retries until it gives up, and this
  # test is about prior construction, not about inits.
  out <- suppressMessages(
    bayesnec:::add_brm_defaults(list(prior = up, init = "random"), "nec4param",
                               validate_family("poisson"), x, y,
                               skip_check = FALSE, custom_name = NULL)
  )
  expect_s3_class(out$prior, "brmsprior")
  expect_setequal(out$prior$nlpar, c("beta", "top", "bot", "nec"))
})

# #232: the #210 guard fired only on an exactly-zero quantile, while the
# collapse it fixes is continuous. These tests constrain the prior to be on the
# scale of the data across the whole zero-fraction range, which is what the
# #210 tests above do not do -- they assert only that the rate is finite and
# positive, and pass just as well on a prior centred at a sixth of the truth.

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
    # Within a factor of three of the true upper asymptote, both ways. Before
    # #232 the worst case -- just under 75% zeros, where the guard did not fire
    # -- was 6.8 against a true 40, which is outside this by a wide margin.
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
  # The specific defect: the old guard was a step function, so the prior was at
  # its worst immediately below the threshold and recovered discontinuously
  # above it. Asserted as monotone-ish stability rather than a pinned value.
  x <- as.numeric(rep(1:10, each = 15))
  mu <- 40 * exp(-0.35 * pmax(x - 2, 0)) + 5
  scales <- vapply(c(0.5, 0.4, 0.35, 0.3, 0.28, 0.26, 0.24, 0.2), function(p) {
    set.seed(1)
    y <- as.numeric(rpois(length(x), mu) * rbinom(length(x), 1, p))
    bayesnec:::positive_scale(y, 0.75)
  }, numeric(1))
  # no adjacent pair differs by more than a factor of two; the old guard's
  # threshold crossing was a factor of four in one step (6.75 -> 29)
  expect_true(all(scales > 0))
  ratios <- scales[-1] / scales[-length(scales)]
  expect_true(all(ratios > 0.5 & ratios < 2))
})

# --- #245: priors for the parameters a group-level term introduces -----------
# Without these, a group-level standard deviation falls through to the brms
# default student_t(3, 0, 2.5). Under the identity link bnec() forces, an offset
# at that scale puts a bounded mean outside its support and the fit cannot
# initialise. The tests below pin the scale, not just the presence of a row:
# a row carrying the wrong scale would pass a presence check and still fail.

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
  # ogl enters as an offset on the whole curve, so its population intercept is
  # confounded with top and bot: a constant added to ogl comes back out of
  # them with no change to the likelihood. brms leaves such a parameter flat,
  # so centring it at zero is what identifies the decomposition.
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
  # define_prior() returns early for a hurdle family, so without handling the
  # group priors there as well the hurdle families kept the whole of #245 --
  # which is exactly the fit vignette("example8") part 3 needs.
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
  # Scaled from the survivors, not from the whole response including the
  # structural zeros -- and then converted onto the scale the deviation is
  # applied on. The mu block of hurdle_gamma is (0, Inf), so #257 applies the
  # deviation multiplicatively and the width is the delta-method log-scale
  # conversion s_y / mean(y), capped at a coefficient of variation of 1,
  # rather than the response-scale s_y this used to assert.
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
  # brms applies the inverse mean link to the whole non-linear expression, so
  # under a log or logit mean link the mu block's top and bot are on that link
  # scale. define_prior() takes the unbounded normal entries for such a fit --
  # what Fisher et al. (2024) describe for any link mapping to the whole real
  # line -- and the hurdle route now does the same. Previously the mu family
  # was rebuilt on the identity link, so zero_inflated_beta(link = "logit")
  # returned beta(5, 2) bounded to [0, 1] for a top whose value on the logit
  # scale is outside that interval whenever the response plateau exceeds 0.73.
  # See #302.
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
  # add_formula_glef() runs before the hu sub-formulas are attached, so ogl and
  # pgl never see them. Pinned because the prior scaling above depends on it,
  # and because vignette("example8") tells the reader so.
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
  # top is transformed on a hurdle_gamma mu block, so its grouping is declared
  # on topgl rather than on top itself (#294). What is being asserted is that
  # the grouping reaches the mu block at all, whichever term it lands on.
  expect_true(all(grepl("site", subs[c("topgl", "beta", "nec")])))
  expect_false(grepl("site", subs[["top"]]))
  expect_false(any(grepl("site", subs[c("hutop", "hubeta", "hunec")])))
})

test_that("prior_type narrows the group-level scales too", {
  # Before this, "regularizing" narrowed top and bot and left the group-level
  # parameters at the uninformative scale -- inert on exactly the parameter a
  # user selecting the narrower set for a grouped fit is selecting it for.
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
  for (p in c("top", "nec", "beta")) {
    expect_equal(get_scale(r, p, "sd"), signif(get_scale(u, p, "sd") / 2, 4))
  }
  # the ogl intercept narrows with them
  expect_equal(get_scale(r, "ogl", "b"),
               signif(get_scale(u, "ogl", "b") / 2, 4))
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
  # Beta transforms top, so the standard deviation is declared on topgl and its
  # width is the delta-method conversion of the response-scale rule (#294).
  # narrow enters the response-scale width before the conversion, so the
  # regularizing scale is still half the uninformative one.
  m_y <- mean(y)
  conv <- function(narrow) {
    signif(min((diff(range(y)) / (10 * narrow)) / (m_y * (1 - m_y)), 1), 4)
  }
  expect_equal(sd_row("uninformative")$nlpar, "topgl")
  expect_equal(sd_row("regularizing")$prior,
               paste0("student_t(3, 0, ", conv(2), ")"))
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

# The nec and ec50 prior: one construction, on whichever scale the predictor
# was supplied on. #269 established that the prior describes the concentration
# series and not the replication of it; #302 replaced the three support-selected
# entries with a normal on the log of the predictor.

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
  # More than half the observations at a zero control made the observation
  # median zero, and under the gamma entry 1 / (0 / 4) put "gamma(5, Inf)" into
  # the prior table. The location is stated as a value rather than recomputed,
  # so that the test says what the prior should be and not how define_prior()
  # arrives at it: the distinct positive values are 5, 15, 45, 135, 300, whose
  # median is 45. See #269.
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
  # The location is taken from the concentration series, so an unbalanced
  # design gets a different prior from the one an observation median would
  # build whether or not a zero is present. Distinct values 0.5, 1, 3, 10, 30
  # have median 3, against an observation median of 6.5. See #269.
  x <- c(0.5, rep(1, 3), rep(3, 6), rep(10, 8), rep(30, 2))
  y <- rev(seq_along(x)) + 1
  pr <- define_prior(model = "nec3param", family = Gamma(link = "identity"),
                     predictor = x, response = y)
  expect_equal(exp(prior_pars(pr$prior[pr$nlpar == "nec"])[1]), 3)
  expect_equal(median(x), 6.5)
})

test_that("replication does not change the nec prior at all", {
  # Under the gamma entry the distinct-value median and the observation median
  # agreed only where replication was balanced. The width is now taken from the
  # range of the tested doses, which no amount of replication changes, and the
  # location from the distinct values, so the two designs below -- the same
  # concentration series replicated evenly and unevenly -- receive the identical
  # prior.
  series <- c(0, 5, 15, 45, 135)
  even <- rep(series, each = 6)
  uneven <- rep(series, times = c(20, 2, 9, 2, 3))
  expect_equal(nec_prior_for(even), nec_prior_for(uneven))
})

test_that("the nec prior median is the median dose tested (#302)", {
  # Fisher et al. (2024) specify maximum density at the median predictor. The
  # median is taken after logging, so for an odd number of distinct positive
  # doses it is the log of their median: the prior peaks at the median dose
  # measured on the log scale, and its median on the dose scale is that dose.
  x <- rep(c(0, 0.1, 1, 10, 100, 1000), each = 6)
  s <- nec_prior_for(x)
  expect_equal(prior_dist(s), "lognormal")
  expect_equal(qlnorm(0.5, prior_pars(s)[1], prior_pars(s)[2]), 10)
})

test_that("an even count of doses centres on their log midpoint (#302)", {
  # Taking the median after logging interpolates between the two central doses
  # on the log axis rather than on the dose axis, so it returns their geometric
  # mean and not their arithmetic one. That is the midpoint of the scale the
  # series is spaced on, and it is stated here because the two coincide for an
  # odd count and the difference is otherwise invisible.
  x <- rep(c(0, 1, 10, 100, 1000), each = 6)
  s <- nec_prior_for(x)
  expect_equal(qlnorm(0.5, prior_pars(s)[1], prior_pars(s)[2]), sqrt(10 * 100))
  expect_equal(median(c(1, 10, 100, 1000)), 55)
})

test_that("the nec prior spans the tested doses on the log scale (#302)", {
  # The width is set so the untruncated central 95% interval runs from the
  # lowest positive dose tested to the highest. That is the criterion the width
  # is chosen by, so it is asserted directly rather than through a constant.
  x <- rep(c(0, 0.01, 0.1, 1, 10, 100), each = 6)
  p <- prior_pars(nec_prior_for(x))
  expect_equal(qlnorm(0.025, p[1], p[2]), 0.01)
  expect_equal(qlnorm(0.975, p[1], p[2]), 100)
})

test_that("the nec prior reaches the top of a log-spaced series (#302)", {
  # gamma(5, 4/m) placed its central 95% interval at 0.41m to 2.56m whatever
  # the data were, so it reached the highest dose only where that dose was
  # within about 2.6 times the median. On the nassarius contaminant A series
  # the ratio is 125, and a true NEC of 1.25 sat beyond the 99.99th percentile
  # of the truncated prior. Both the old and the new prior are evaluated here,
  # so the test states the defect as well as the correction.
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
  # The three entries were selected by set_distribution(predictor, ...), which
  # reads the support: above 1 took gamma(5, 4/m), within [0, 1] took the fixed
  # beta(2, 2), and spanning negatives took the normal entry. Support is a
  # property of the units a dose is recorded in, so the same series in mg/L and
  # in g/L received priors differing roughly fiftyfold in width. It is now the
  # same construction rescaled, so the prior is equivariant under a change of
  # units: the g/L prior is the mg/L prior shifted by log(1000).
  mg <- rep(c(0, 1, 10, 100, 1000), each = 6)
  g <- mg / 1000
  p_mg <- prior_pars(nec_prior_for(mg))
  p_g <- prior_pars(nec_prior_for(g))
  expect_equal(prior_dist(nec_prior_for(g)), "lognormal")
  expect_equal(p_g[1], p_mg[1] - log(1000))
  expect_equal(p_g[2], p_mg[2])
})

test_that("a predictor spanning negative values keeps its published prior", {
  # normal(median(x), 10 sd(x)) is the entry Fisher et al. (2024) describe for a
  # predictor a user has log transformed, and it is unchanged. The location and
  # spread are read from the distinct values, as everywhere else, which is the
  # only part of this entry #302 alters.
  x <- rep(log(c(0.05, 0.1, 1, 10, 100)), each = 6)
  s <- nec_prior_for(x)
  expect_equal(prior_dist(s), "normal")
  expect_equal(prior_pars(s), c(median(unique(x)), 10 * sd(unique(x))))
})

test_that("the nec prior is chosen from the predictor, not the response", {
  # The prior is a function of the predictor alone: no family, link or prior
  # type changes it. Confirmed in the #302 sweep over all 48 family by link by
  # prior-type combinations; four families are asserted here.
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
  reg <- suppressMessages(get_priors(
    y ~ crf(x, model = "nec3param"),
    data = data.frame(x = x, y = seq(0.9, 0.1, length.out = length(x))),
    family = Beta(link = "identity"), prior_type = "regularizing"
  ))
  expect_equal(reg$prior[reg$nlpar == "nec"], target)
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
  # sd() is NA and the range is zero on a single distinct positive value, so
  # both would put a non-finite or zero sigma into the prior string. Such a
  # design cannot identify a concentration-response curve, so the fallback of 1
  # on the log scale is chosen to be harmless rather than to be right.
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

# ---- #294, priors for a transformed parameter-level deviation ---------------

test_that("a transformed term declares its sd on the deviation, not the parameter", {
  # bot keeps the population-level prior define_prior() already builds for it;
  # what the group-level term adds is a standard deviation on botgl. A prior on
  # nlpar "bot" of class "sd" would match nothing in the fit and brms would drop
  # it silently.
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
  # And it adds nothing else. The deviation has no population intercept to give
  # a prior to -- add_par_gl_term() writes botgl ~ 0 + (1 | group) precisely
  # because a free intercept would be exactly unidentified against bot, which
  # would stop b_bot_Intercept being the asymptote the population-level curve
  # declines towards. ogl is the other case and does keep an intercept prior.
  # define_group_prior() returns the group-level rows alone, and for a
  # transformed term that is the standard deviation and nothing else.
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
  # logit: the response-scale width divided by the Jacobian at the response
  # mean, which is the same conversion #257 uses for ogl.
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
  # The difference from the ogl conversion, which caps the log branch only.
  # A response whose own mean is close to a bound makes the logit Jacobian
  # small and the ratio large; the parameter the deviation is applied to is bot,
  # which is not the response mean, so the self-limiting argument #257 makes for
  # ogl does not hold for it, and the cap is applied to both branches.
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
    "botgl"), 0.5)
  # #257's ogl conversion is deliberately left as it was.
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
