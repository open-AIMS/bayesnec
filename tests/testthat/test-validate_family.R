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
  expect_equal(f(quote(Beta(link = "logit"))), "link")
  expect_equal(f(quote(binomial(link = "identity"))), "link")
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

test_that("a link written positionally is a link the caller chose", {
  # `link` is the first formal of every family constructor, so binomial("probit")
  # states a link as plainly as binomial(link = "probit") does. Reading only
  # named arguments gave the two opposite outcomes -- one refused, one silently
  # refitted on identity, which is the substitution #256 exists to remove.
  f <- bayesnec:::family_link_source
  expect_equal(f(quote(Beta("logit"))), "link")
  expect_equal(f(quote(binomial("probit"))), "link")
  expect_equal(f(quote(stats::binomial("probit"))), "link")
  expect_equal(f(quote(Gamma("log"))), "link")
  expect_error(
    bayesnec:::validate_family(binomial("probit"),
                               link_source = f(quote(binomial("probit")))),
    "bayesnec fits on the"
  )
})

test_that("a link on the second block is a link the caller chose", {
  # hu and zi are written as `1 - <probability>` and bayesnec fits a curve
  # expression on them, so link_hu and link_zi are statements about the fit.
  # Without this the guard in validate_family() could not fire for the form a
  # caller would actually write to set one.
  f <- bayesnec:::family_link_source
  expect_equal(f(quote(hurdle_gamma(link_hu = "logit"))), "link_hu")
  expect_equal(f(quote(zero_inflated_beta(link_zi = "logit"))), "link_zi")
  # each block separately, so the one the caller left alone is still assigned
  expect_equal(f(quote(hurdle_gamma(link = "log", link_hu = "identity"))),
               c("link", "link_hu"))
  expect_error(
    bayesnec:::validate_family(
      brms::hurdle_gamma(link_hu = "logit"),
      link_source = f(quote(hurdle_gamma(link_hu = "logit")))
    ),
    "link_hu"
  )
})

test_that("a dispersion link is not a statement about the mean link", {
  # link_phi and link_shape carry no curve. Reading one as a chosen *mean* link
  # would leave Beta(link_phi = "log") on beta's default logit, which is the
  # misspecification this is all here to prevent.
  f <- bayesnec:::family_link_source
  expect_equal(f(quote(Beta(link_phi = "log"))), "none")
  expect_equal(f(quote(negbinomial(link_shape = "log"))), "none")
  fam <- bayesnec:::validate_family(Beta(link_phi = "log"),
                                    link_source = "none")
  expect_equal(fam$link, "identity")
})

test_that("a dispersion link the caller wrote is carried through, not reset", {
  # The mean link is bayesnec's to assign; the dispersion link is nobody's to
  # reassign. Rebuilding the family on the identity link must not quietly
  # return link_phi or link_shape to the family's own default, because a disp()
  # sub-model is then fitted on a link scale the caller did not ask for.
  v <- bayesnec:::validate_family
  f <- bayesnec:::family_link_source
  b <- v(Beta(link_phi = "identity"),
         link_source = f(quote(Beta(link_phi = "identity"))))
  expect_equal(b$link, "identity")
  expect_equal(b$link_phi, "identity")
  nb <- v(brms::negbinomial(link_shape = "identity"),
          link_source = f(quote(negbinomial(link_shape = "identity"))))
  expect_equal(nb$link, "identity")
  expect_equal(nb$link_shape, "identity")
  # the default is still the default when nothing was written
  expect_equal(v(Beta(), link_source = "none")$link_phi, "log")
})

test_that("a family that is not written as a constructor call is honoured", {
  # do.call() inlines the evaluated family into the call it builds, and
  # fit$family is how a family is read back off a fitted object. Neither can be
  # read for intent, so both are treated as a variable holding a family:
  # honoured, with a message, rather than having the link replaced in silence.
  f <- bayesnec:::family_link_source
  expect_equal(f(quote(fit$family)), "symbol")
  expect_equal(f(quote(do.call(Beta, list(link = "logit")))), "symbol")
  expect_equal(f(Gamma(link = "inverse")), "symbol")
  d <- data.frame(x = rep(1:4, each = 5), y = rep(c(1, 2, 3, 0), 5))
  g <- y ~ crf(x, "nec3param")
  expect_error(
    do.call(get_priors, list(g, data = d, family = Gamma(link = "inverse"))),
    "bayesnec fits on the"
  )
})

test_that("the idempotence marker cannot carry an unsupported link", {
  # Only the rebuild and the message are skipped for an already validated
  # family. The checks still run, so an object carrying the marker -- one taken
  # off a returned fit, say -- cannot be edited into an unsupported link and
  # passed back.
  v <- bayesnec:::validate_family
  tampered <- v("Beta")
  tampered$link <- "probit"
  expect_error(v(tampered, link_source = "symbol"), "bayesnec fits on the")
})

test_that("a function that is not a family constructor is refused clearly", {
  # bnec(family = mean) reached the constructor call and failed with that
  # function's own argument error.
  v <- bayesnec:::validate_family
  expect_error(v(mean, link_source = "symbol"), "does not")
  expect_error(v(sum, link_source = "symbol"), "does not")
  expect_error(v(length, link_source = "symbol"), "does not")
})

test_that("a string naming some other function is not read as a family", {
  # exists(ctor, mode = "function") alone admits any function in scope, so the
  # string reached do.call() and failed with `unused argument (link =
  # "identity")` -- the same error validate_family("beta") used to give.
  v <- bayesnec:::validate_family
  expect_error(v("t"), "not currently implemented")
  expect_error(v("mean"), "not currently implemented")
  expect_error(v("c"), "not currently implemented")
})

test_that("validating an already validated family changes nothing and says nothing", {
  # bnec_group() and bnec_hurdle() validate the family and then hand the object
  # to bnec(), where the expression is a symbol. Without idempotence the link
  # would be announced again -- once per group level, advising the caller to
  # drop a link they had chosen.
  v <- bayesnec:::validate_family
  once <- v(Beta(link = "logit"), link_source = "chosen")
  expect_silent(twice <- v(once, link_source = "symbol"))
  expect_equal(twice$link, "logit")
  expect_equal(twice$family, "beta")
})

test_that("each entry point reads the link from the expression it was given", {
  # bnec() takes the expression out of match.call(expand.dots = FALSE); the
  # others use substitute(). A refused link errors before anything is fitted,
  # so the capture can be tested without sampling: were the expression not
  # read, the link would be silently replaced with identity and no error
  # raised.
  d <- data.frame(x = rep(1:4, each = 5), y = rep(c(1, 2, 3, 0), 5),
                  site = rep(c("a", "b"), 10))
  f <- y ~ crf(x, "nec3param")
  expect_error(bnec(f, data = d, family = Gamma(link = "inverse")),
               "bayesnec fits on the")
  expect_error(bnec(f, data = d, family = Gamma("inverse")),
               "bayesnec fits on the")
  expect_error(
    bnec_group(f, data = d, group_var = "site",
               family = Gamma(link = "inverse")),
    "bayesnec fits on the"
  )
  hdat <- data.frame(x = rep(1:4, each = 5), y = c(rep(1, 15), rep(0, 5)))
  expect_error(
    bnec_hurdle(f, data = hdat, family_growth = Gamma(link = "inverse")),
    "bayesnec fits on the"
  ) |> suppressMessages()
})
