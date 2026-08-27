# --- #256: bayesnec assigns the link unless the caller chose one -------------
# Before this, which link a fit used depended on how the family was written and
# the difference was silent: "Beta" gave identity, Beta and Beta() gave logit,
# Gamma() gave inverse. In the latter cases the curve was fitted to a transform
# of the mean while top, bot and nec were reported as though they were on the
# response scale.

test_that("family_link_source reads the expression, not the object", {
  f <- bayesnec:::family_link_source
  # a family named and nothing more
  expect_equal(f(quote("Beta")), "none")
  expect_equal(f(quote(Beta())), "none")
  expect_equal(f(quote(brms::Beta())), "none")
  expect_equal(f(NULL), "none")
  # a link chosen
  expect_equal(f(quote(Beta(link = "logit"))), "chosen")
  expect_equal(f(quote(binomial(link = "identity"))), "chosen")
  expect_equal(f(quote(hurdle_gamma(link = "log", link_hu = "identity"))),
               "chosen")
  # a symbol: either the constructor or a variable holding an object, and the
  # expression cannot tell which. validate_family() has the value.
  expect_equal(f(quote(Beta)), "symbol")
  expect_equal(f(quote(my_family)), "symbol")
})

test_that("naming a family leaves the link to bayesnec", {
  v <- bayesnec:::validate_family
  for (spec in list(list("Beta", "none"), list(Beta, "symbol"),
                    list(Beta(), "none"))) {
    fam <- v(spec[[1]], link_source = spec[[2]])
    expect_equal(fam$family, "beta")
    expect_equal(fam$link, "identity")
  }
  # Gamma's own default is inverse, and bernoulli's is logit
  expect_equal(v(Gamma())$link, "identity")
  expect_equal(v(brms::bernoulli())$link, "identity")
  expect_equal(v("poisson")$link, "identity")
})

test_that("a link the caller chose is honoured where bayesnec fits on it", {
  v <- bayesnec:::validate_family
  expect_equal(v(Beta(link = "logit"), link_source = "chosen")$link, "logit")
  expect_equal(v(Gamma(link = "log"), link_source = "chosen")$link, "log")
  expect_equal(v(Beta(link = "identity"), link_source = "chosen")$link,
               "identity")
})

test_that("a link bayesnec does not fit on is refused", {
  v <- bayesnec:::validate_family
  # These were reachable before and silently changed what was being fitted.
  expect_error(v(Gamma(link = "inverse"), link_source = "chosen"),
               "bayesnec fits on the")
  expect_error(v(binomial(link = "probit"), link_source = "chosen"),
               "bayesnec fits on the")
  expect_error(v(brms::Beta(link = "cloglog"), link_source = "chosen"),
               "bayesnec fits on the")
  # the message names the family and points at the fix
  expect_error(v(Gamma(link = "inverse"), link_source = "chosen"), "Gamma")
})

test_that("a family object reached through a variable is honoured, and said so", {
  # The expression is a symbol, so whether the link was chosen is unknowable.
  # Honouring it and announcing which link was taken is better than silently
  # replacing a link the caller may have meant.
  v <- bayesnec:::validate_family
  my_family <- Beta(link = "logit")
  expect_message(fam <- v(my_family, link_source = "symbol"), "logit")
  expect_equal(fam$link, "logit")
  # nothing is announced when the object already carries the link bayesnec
  # would have assigned
  quiet <- Beta(link = "identity")
  expect_silent(v(quiet, link_source = "symbol"))
})

test_that("the family tag brms reports is accepted as well as the constructor", {
  # mod_fams maps the tag to the constructor and the two differ for one family:
  # get("beta") resolves to base::beta(). A user reading the family off a
  # fitted object and passing it back previously got
  # `unused argument (link = "identity")`.
  v <- bayesnec:::validate_family
  expect_equal(v("beta")$family, "beta")
  expect_equal(v("beta")$link, "identity")
  expect_equal(v("Beta")$family, "beta")
  for (tag in names(bayesnec:::mod_fams)) {
    expect_s3_class(v(tag), "family")
  }
  expect_error(v("not_a_family"), "not currently")
})

test_that("bnec assigns the link from the expression it was given", {
  # The end-to-end path: get_priors() runs the same family resolution bnec()
  # does, so the prior it returns records which link was used. A logit fit
  # would give normal() priors on top rather than beta().
  set.seed(256)
  d <- data.frame(x = rep(seq(0, 4, length.out = 10), 3))
  d$y <- pmin(pmax(0.8 * exp(-exp(-0.5) * pmax(d$x - 2, 0)) +
                     rnorm(30, 0, 0.05), 0.02), 0.98)
  f <- y ~ crf(x, "nec3param")
  top_prior <- function(p) p$prior[p$class == "b" & p$nlpar == "top"]
  expect_equal(top_prior(get_priors(f, data = d, family = "Beta")), "beta(5, 2)")
  expect_equal(top_prior(get_priors(f, data = d, family = Beta)), "beta(5, 2)")
  expect_equal(top_prior(get_priors(f, data = d, family = Beta())), "beta(5, 2)")
  expect_error(get_priors(f, data = d, family = Gamma(link = "inverse")),
               "bayesnec fits on the")
})
