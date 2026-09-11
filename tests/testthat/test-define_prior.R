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
  # regularizing places top at the mean response over the lowest predictor
  # values and bot at the mean over the highest, and takes regularizing_factor
  # of the uninformative spread. On this branch the normal's location parameter
  # is its mode as well as its mean, so no solving is involved (#305). A
  # replicated design is used so the extreme group is a whole dose group, and
  # the response is ordered against it so the expected values can be written
  # out rather than recovered from the function under test.
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
  # beta-family entries differ between sets, and the regularizing one now reads
  # the response rather than being a second constant (#305).
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

test_that("regularizing stays on the scale of the data for a single zero", {
  # Under prior_type = "regularizing" the location was quantile(response, 0) --
  # the minimum -- so a single zero was enough to collapse it. The location is
  # now read from the observations at the highest concentration (#305) and the
  # spread still passes through positive_scale(), so this asserts what it
  # always asserted: that one zero does not put the rate at zero or infinity.
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
  # Beta transforms top, so the standard deviation is declared on topgl and its
  # width is the delta-method conversion of the response-scale rule (#294).
  # narrow enters the response-scale width before the conversion, so the
  # regularizing scale is still regularizing_factor of the uninformative one.
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
  # 55 is the arithmetic median of the four doses; the prior's median is 31.6.
  expect_equal(qlnorm(0.5, prior_pars(s)[1], prior_pars(s)[2]), sqrt(10 * 100))
})

test_that("the nec prior covers every dose tested (#302)", {
  # The width is set so the untruncated central 95% interval reaches both ends
  # of the concentration series. That is the criterion the width is chosen by,
  # so it is asserted directly rather than through a constant, and it is
  # asserted on series that are asymmetric about their median on the log axis
  # in each direction -- which is where setting the width from half the range
  # instead leaves the interval correctly wide and wrongly centred.
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
  # Stated as a failing alternative so the reason for the rule is recorded: a
  # width taken from half the range stops at 9.96 on the contaminant A series,
  # against a highest dose applied of 20.
  z <- log(unique(series$top_heavy)[unique(series$top_heavy) > 0])
  expect_lt(qlnorm(0.975, median(z), diff(range(z)) / (2 * qnorm(0.975))),
            max(series$top_heavy))
})

test_that("a low threshold on a wide series is inside the prior (#302)", {
  # The design sweep behind this change placed every true value in the upper
  # half of its series, so it could not detect a prior that fails at the bottom.
  # The nassarius contaminant B series has sparse low doses, so its median sits
  # above its log mid-range and this is the direction at risk.
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
  # sigma is set by the two extreme doses, so recording a control as a nominal
  # small positive value states that the value was applied and widens the prior
  # to cover it. Pinned because it is a change from the gamma entry, whose rate
  # came from the median and barely noticed the substitution, and because the
  # remedy is to record a control as 0.
  base <- c(0, 0.01, 0.1, 1, 10, 100)
  s0 <- prior_pars(nec_prior_for(rep(base, each = 6)))
  eps <- base
  eps[eps == 0] <- 1e-6
  s1 <- prior_pars(nec_prior_for(rep(eps, each = 6)))
  expect_gt(s1[2], s0[2] * 2)
  expect_equal(qlnorm(0.025, s1[1], s1[2]), 1e-6)
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
  # The prior is a function of the predictor alone: no family and no link
  # changes it. Confirmed in the #302 sweep over all 48 family by link by
  # prior-type combinations; four families are asserted here. prior_type is the
  # one thing that does change it, and only its spread; see the test below.
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

# The spread the regularizing set states, read from the predictor on the scale
# the prior is written on. Stated here as the rule rather than taken from the
# package, so that the tests say what the spread should be and not how
# predictor_prior() arrives at it.
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
  # The predictor-scaled prior was identical under both prior types, so
  # selecting the regularizing set left the two parameters a user most often
  # selects it for untouched (#305). The regularizing spread is now stated as
  # its own coverage rule -- the width whose central 98% interval reaches the
  # farthest concentration tested -- with the location, the distribution and the
  # truncation unchanged. #305 stated it as a multiple of the uninformative
  # spread instead, which meant one thing on each of the two branches; see the
  # tests below.
  for (x in list(rep(c(0, 25, 50, 75, 100), each = 6),
                 rep(log(c(0.05, 0.1, 1, 10, 100)), each = 6))) {
    u <- bayesnec:::predictor_prior(x)
    r <- bayesnec:::predictor_prior(x, prior_type = "regularizing")
    expect_equal(prior_dist(r), prior_dist(u))
    expect_equal(prior_pars(r)[1], prior_pars(u)[1])
    expect_equal(prior_pars(r)[2], reg_spread(prior_scale(x)))
    expect_lt(prior_pars(r)[2], prior_pars(u)[2])
  }
  # and it reaches both nec and ec50 through define_prior(), with the bounds
  # left at the predictor range so that no part of the tested series is excluded
  x <- rep(c(0, 25, 50, 75, 100), each = 6)
  d <- data.frame(x = x, y = seq(0.9, 0.1, length.out = length(x)))
  reg <- suppressMessages(get_priors(
    y ~ crf(x, model = "ecx4param"), data = d,
    family = Beta(link = "identity"), prior_type = "regularizing"
  ))
  target <- bayesnec:::predictor_prior(x, prior_type = "regularizing")
  expect_equal(reg$prior[reg$nlpar == "ec50"], target)
  expect_equal(as.numeric(reg$lb[reg$nlpar == "ec50"]), 0)
  expect_equal(as.numeric(reg$ub[reg$nlpar == "ec50"]), 100)
})

# The uninformative entry for every cell of the #302 audit: five designs by
# three predictor transforms. Written out as the strings predictor_prior()
# returns, so that the gate is a pin on the released entry and not a
# restatement of the rule that produces it. The log column substitutes half the
# lowest non-zero dose for the zero control, as prior_audit.R does.
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
  # The regression gate of #314, as literal prior strings: the claim is that the
  # released entry did not change at all, on either branch. It is asserted over
  # every cell of the #302 audit rather than the two transforms whose
  # regularizing entry is also unchanged, because the log column is the normal
  # branch, and that is the branch #314 refactors -- half_width is now computed
  # before the branch rather than inside the lognormal arm.
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
  # The cancellation identity. #305 multiplied half_width / qnorm(0.975) by
  # qnorm(0.975) / qnorm(0.99); the rule is now stated as half_width /
  # qnorm(0.99), which is the same quantity with the qnorm(0.975) cancelled, so
  # a predictor on the recorded concentration scale receives the entry it
  # already received.
  #
  # Compared as numbers, not as literal strings. The two expressions are the
  # same quantity but not always the same double: the factor form rounds three
  # times and the stated form once. Measured over 200,000 randomly generated
  # dilution series the largest relative difference was 2.22e-16, exactly one
  # unit in the last place, and the 15 significant digits paste0() writes
  # differed on 1.6% of them. Two prior strings of the #302 audit are affected,
  # both the nassarius contaminant A series: 1.93333703285197 against
  # 1.93333703285198 read on the recorded scale and 0.966668516425987 against
  # 0.966668516425988 on the square-root scale, which is four of its 30 design
  # by transform by parameter cells because nec and ec50 share a string. A
  # literal string here would therefore pin the formatting of a double, which is
  # what the note at the top of this block of tests says these helpers exist to
  # avoid.
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
  # prior_type is a statement about belief, not about which column the user
  # passed. Under #305 the first series here received a spread of 20.96 supplied
  # as log(conc) against 1.51 supplied as conc, because the uninformative spread
  # it was a multiple of is the constant 10 sd(z) on that branch rather than a
  # coverage width.
  #
  # Two conditions are needed for the routes to agree and both are pinned here,
  # because neither is a property of the rule. Neither series has a zero
  # control, which is the first: the lognormal branch drops non-positive values
  # and a logged series must substitute for a control, so a design with one is
  # not the same predictor on the two routes.
  #
  # The second is the discriminator. spans_negative is min(u) < 0, so a logged
  # series whose lowest tested concentration is at or above 1 stays
  # non-negative, is not recognised as logged, and is logged a second time. The
  # location then differs as well as the spread, and both are asserted rather
  # than left to be rediscovered;
  # the discriminator is out of scope for #314 because correcting it would also
  # change the uninformative entry for those users.
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
  # and the misclassified entry is still peaked inside the tested range and is
  # still narrowed by prior_type, which is why this is second-order against the
  # uniform prior #314 removes.
  lx <- rep(log(above_one), each = 6)
  mode <- exp(prior_pars(logged2)[1] - prior_pars(logged2)[2]^2)
  expect_gt(mode, min(lx))
  expect_lt(mode, max(lx))
  expect_lt(prior_pars(logged2)[2], prior_pars(pp(lx))[2])
})

test_that("the regularizing prior is not uniform once logged (#314)", {
  # The defect #314 removes. On a 0.1 to 100 series supplied as log(conc) the
  # #305 entry had a spread of 20.96 against a tested range of 6.91, so the
  # truncated prior CDF at each dose was that dose's position within the range,
  # which is what a uniform prior gives, to three decimal places.
  conc <- c(0.1, 0.3, 1, 3, 10, 30, 100)
  x <- rep(log(conc), each = 6)
  z <- log(conc)
  position <- (z - min(z)) / (max(z) - min(z))
  old <- paste0("normal(", median(z), ", ",
                10 * sd(z) * qnorm(0.975) / qnorm(0.99), ")")
  new <- bayesnec:::predictor_prior(x, prior_type = "regularizing")
  expect_lt(max(abs(trunc_prior_cdf(old, x)(z) - position)), 1e-3)
  # and the stated rule is not uniform: it puts 0.052 below the second dose,
  # which is at 0.158 of the range, and 0.945 below the sixth, which is at
  # 0.826.
  got <- trunc_prior_cdf(new, x)(z)
  expect_lt(got[2], position[2] / 2)
  expect_gt(got[6], position[6] * 1.1)
})

test_that("the regularizing prior admits a low threshold (#314)", {
  # Why q = 0.99 and not a larger quantile: the prior mass below the
  # second-lowest concentration tested is what keeps a true threshold at the
  # bottom of the series admissible, and #302 exists because an entry that
  # excluded one shipped. Four designs, each supplied both ways.
  #
  # Each design is divided by its highest dose before being logged. That shifts
  # the prior, its truncation bounds and the doses together, so every
  # probability below is unchanged, and it puts min(log(x)) below zero, which is
  # what selects the branch for a predictor supplied logged.
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
  # The change gate of #314. The ten cells of the #302 audit whose regularizing
  # entry changes are its five designs read on the log scale, by nec and ec50,
  # and none may put the true value outside the central 95% of the truncated
  # prior. Measured here rather than by rerunning notes/scripts/prior_audit.R,
  # which builds the same priors through get_priors() over 12 families and both
  # links; the prior is a function of the predictor alone, so the families add
  # no cells. Measured range: 0.536 to 0.974, against 0.626 to 0.890 under #305.
  #
  # The gate has two halves and both are asserted: the ten cells must change,
  # and none may put the true value outside the central 95%. Without the first,
  # an implementation that changed nothing would pass, because the #305 entry
  # covers those cells as well -- it covers them by being uniform across the
  # tested range, which is the defect.
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
  # q = 0.99 rather than a larger quantile is settled by this gate and not only
  # by the prior mass below the lowest dose: at q = 0.995 and q = 0.999 the
  # log_unit ec50 cell sits at 0.9823 and 0.9929, outside the central 95%.
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

test_that("a hurdle mu block reads the whole predictor for nec (#302)", {
  # Both blocks of a hurdle fit are evaluated over the whole predictor range,
  # and their nec bounds are taken from it, so the prior inside those bounds is
  # taken from it too. Priming the mu block's nec from the survivors alone would
  # state that the threshold is below the highest concentration at which
  # anything survived, which is the failure this change removes elsewhere.
  #
  # Only the mu block moves. survival_by_x() returns sort(unique(predictor)), so
  # the second block already had the whole predictor's distinct values and its
  # hunec assertion below held before this change as well; it is kept because
  # the two blocks agreeing about their shared predictor is the property, and
  # one of the two would otherwise go unasserted.
  set.seed(302)
  x <- rep(c(0, 1, 10, 100), each = 6)
  y_sub <- c(rgamma(18, 25, 25 / 8), rep(0, 6))     # survivors up to x = 10
  fam <- brms::hurdle_gamma(link = "identity", link_hu = "identity")
  pr <- bayesnec:::define_hurdle_prior("nec3param", fam, x, y_sub)
  whole <- bayesnec:::predictor_prior(x)
  expect_equal(pr$prior[pr$nlpar == "nec"], whole)
  expect_equal(pr$prior[pr$nlpar == "hunec"], whole)
  expect_equal(as.numeric(pr$ub[pr$nlpar == "nec"]), 100)
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
  # set_distribution() is called for this error alone; its value is no longer
  # read. Pinned here so that a later change to set_distribution() cannot
  # remove the check from this route without a test noticing.
  x <- as.integer(rep(c(0L, 1L, 10L, 100L), each = 6))
  y <- rep(c(0.9, 0.6, 0.3, 0.1), each = 6)
  expect_error(
    define_prior("nec3param", Beta(link = "identity"), x, y),
    "does not currently support integer concentration"
  )
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
    "botgl"), bayesnec:::regularizing_factor)
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

# A nec4param response with known asymptotes, simulated on each branch's own
# scale. The counts are the case #305 exists for: the true bot of 5 is well
# above the smallest observation, which is what the released anchor used.
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
  # Measured before #305 the ratio ran 0.40 on the normal branch, 0.87 and 0.34
  # on the gamma branch for top and bot, 1.15 -- wider -- for a negbinomial top,
  # and 0.88 on the beta branch, and over the whole audit it reached 2.71. The
  # cap is what a user selecting the narrower set is entitled to.
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
  # The floor at the standard error of the location binds only where the
  # observations at one end of the predictor are few or individually
  # uninformative. A well replicated gaussian design is neither, so the ratio is
  # regularizing_factor exactly there.
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
  # The contract is stated on the mode, so that one rule means the same thing
  # whichever distribution a branch uses. A gamma stated by its mean peaks
  # somewhere else, which is the ambiguity that produced #273 and #302. The
  # location is the mean response at the end of the predictor where the
  # parameter is the level of the curve.
  for (branch in c("normal", "gamma", "beta")) {
    d <- branch_response(branch)
    r <- priors_for(d, "regularizing")
    yl <- bayesnec:::response_link_scale(d$y, validate_family(d$family))
    # Computed here rather than read back from regularizing_location(), so the
    # location half of the contract is pinned to a quantity this file states
    # independently. Every design in branch_response() is replicated six deep,
    # so the extreme group is one whole dose group.
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
  # A quantile of the pooled response is a proxy for the level of one plateau
  # whose quality depends on what share of the design sits on it, and the
  # over-dispersed count is where that fails: the 95th percentile of the pooled
  # response reached 72 against a true top of 40.
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
  # The released entry was gamma(5, 5 / (min(y) + min(y > 0) / 10)), whose
  # maximum density is at about 0.8 times the smallest observation. On a count
  # response the smallest observation sits well below the asymptote it is meant
  # to locate, so the prior excluded the value it was built to find: the
  # truncated CDF at the true bot ran 0.988 to 0.99999 across the audit's cells.
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
  # beta(5, 1) against beta(5, 2) changed the width by 12 per cent and did not
  # change what the prior was anchored to, because there was no anchor. Two
  # responses with different control levels must now receive different priors,
  # and the same uninformative one.
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

# The cases the review of PR #307 showed the first version of #305 handled worse
# than the released set. Each is a design the archived prior audit does not
# contain, which is why the sweep did not catch them.

test_that("a bot anchor survives complete effect at the highest concentration", {
  # The zero-bounded branch filtered the anchor subset to its positive
  # observations at both ends. Where every observation at the highest
  # concentration is zero that empties the subset, and the fallback was an
  # extreme quantile of the pooled response -- the anchor type this replaced.
  # It put the bot prior's maximum density at 10.8 against a true bot of zero.
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
  # A zero at the highest concentration is the endpoint responding, and it is
  # the observation that says most about how low bot is. Filtering left one
  # observation of six and reported a location six times the group mean, with a
  # standard error of zero, which also switched off the floor.
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
  # split_hurdle_response() primes the second block from one survival
  # proportion per concentration, so every group there is a single value and
  # the minimum-observation rule alone averaged the three most extreme
  # concentrations of a six-concentration design. That put the hubot prior's
  # maximum density at 0.34 against a true value of 0.014.
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
  # sd() is zero on a homogeneous group, which is the ordinary case for a
  # binary response: every control individual survived. That is not the same as
  # estimating the proportion exactly, and a standard error of zero switched off
  # the floor on exactly the group that says least about a proportion.
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
  # #294 capped a parameter-level conversion at 1 / narrow so that selecting
  # the regularizing set was not inert wherever the cap bound. The ogl branch
  # kept a constant cap of 1, so on a response whose range is more than 25 times
  # its mean both prior types returned student_t(3, 0, 1).
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
  # The claim that #305 changes nothing released is only as good as a test that
  # would fail if it did. Each entry is reconstructed from the published rule
  # rather than pinned as a literal string, so a change to how it is built has
  # to change the rule to pass.
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
      # gamma with shape 2 and its mean at the quartile. The quantile is taken
      # with stats::quantile() rather than through positive_scale(), which is an
      # internal of the code under test; the response here has no zeros, which
      # is the case positive_scale() is documented to leave untouched, so the
      # two agree and the assertion does not depend on it.
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

# Two further defects found in the second round of review of PR #307.

test_that("an all-zero control does not put top at the detection floor", {
  # The detection-floor fallback -- a tenth of the smallest positive
  # observation -- is the right location for bot where the highest
  # concentration is entirely zero. Applied at the other end it is the #210
  # collapse: a control of structural zeros, which a zero-inflated family
  # produces routinely, put top at 0.1 against a true top of 40 and made the
  # two asymptotes near-identical.
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
  # A design with few concentrations and no replication presents the same input
  # as the second block of a hurdle fit -- one observation per concentration,
  # and fewer than ten of them -- so the two cannot be told apart from the data
  # and the limit resolves both the same way. This pins that choice and the
  # consequence, which is that the entry is relocated rather than narrowed.
  x <- seq(0, 10, length.out = 8)
  set.seed(305)
  y <- 5 + 35 * exp(-0.5 * pmax(x - 3, 0)) + rnorm(8, 0, 3)
  loc <- bayesnec:::regularizing_location(x, y, "top")
  expect_equal(floor(0.2 * length(unique(x))), 1)
  expect_equal(loc[["location"]], y[1])
  # The standard error stands in for a group that cannot supply one. How far it
  # then widens the entry depends on the branch: on a Gamma response the
  # uninformative width is a fraction of a quartile and the floor reaches the
  # cap, while on a gaussian one the uninformative width is 2.5 sd(y) and the
  # stand-in of sd(y) is exactly the stated spread, so nothing changes there.
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
