# Resolve links from the caller's expression so equivalent ways of naming
# a family use the same package defaults.

test_that("family_link_source reads the expression, not the object", {
  f <- bayesnec:::family_link_source
  expect_equal(f(quote("Beta")), "none")
  expect_equal(f(quote(Beta())), "none")
  expect_equal(f(quote(brms::Beta())), "none")
  expect_equal(f(NULL), "none")
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
  expect_error(v(Gamma(link = "inverse"), link_source = "chosen"),
               "bayesnec fits on the")
  expect_error(v(binomial(link = "probit"), link_source = "chosen"),
               "bayesnec fits on the")
  expect_error(v(brms::Beta(link = "cloglog"), link_source = "chosen"),
               "bayesnec fits on the")
  expect_error(v(Gamma(link = "inverse"), link_source = "chosen"), "Gamma")
})

test_that("a family object reached through a variable is honoured, and said so", {
  # A symbol does not reveal whether the caller chose a link. Honour the
  # family object and report a link that differs from the package default.
  v <- bayesnec:::validate_family
  my_family <- Beta(link = "logit")
  expect_message(fam <- v(my_family, link_source = "symbol"), "logit")
  expect_equal(fam$link, "logit")
  # Do not report an object whose link already matches the package default.
  quiet <- Beta(link = "identity")
  expect_silent(v(quiet, link_source = "symbol"))
})

test_that("a family object is rebuilt without changing its specification", {
  # Family constructors can retain promises to a caller's environment. Rebuild
  # from the specification to avoid serialising a whole knitr chunk to workers.
  make_family <- function() {
    big <- numeric(1e6)
    Gamma(link = "log")
  }
  supplied <- make_family()
  validated <- suppressMessages(
    bayesnec:::validate_family(supplied, link_source = "symbol")
  )
  expect_identical(
    bayesnec:::family_signature(validated),
    bayesnec:::family_signature(supplied)
  )
  expect_false(
    identical(
      environment(validated$variance), environment(supplied$variance)
    )
  )
})

test_that("the family tag brms reports is accepted as well as the constructor", {
  # Map the family tag to its constructor: beta names base::beta(), not Beta().
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
  # Use the returned top prior to distinguish identity and logit mean links
  # without fitting a model.
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
  # The first positional argument also chooses a mean link; it must receive
  # the same validation as a named link argument.
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
  # A link_hu or link_zi argument explicitly chooses the second block's link.
  f <- bayesnec:::family_link_source
  expect_equal(f(quote(hurdle_gamma(link_hu = "logit"))), "link_hu")
  expect_equal(f(quote(zero_inflated_beta(link_zi = "logit"))), "link_zi")
  # Resolve each block separately so an omitted link still gets its default.
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
  # Choosing a dispersion link does not choose the mean link.
  f <- bayesnec:::family_link_source
  expect_equal(f(quote(Beta(link_phi = "log"))), "none")
  expect_equal(f(quote(negbinomial(link_shape = "log"))), "none")
  fam <- bayesnec:::validate_family(Beta(link_phi = "log"),
                                    link_source = "none")
  expect_equal(fam$link, "identity")
})

test_that("a dispersion link the caller wrote is carried through, not reset", {
  # Preserve explicit dispersion links when rebuilding the family with an
  # assigned mean link.
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
  expect_equal(v(Beta(), link_source = "none")$link_phi, "log")
})

test_that("a family that is not written as a constructor call is honoured", {
  # Honour evaluated family objects and fit$family expressions: neither reveals
  # which links the caller explicitly chose.
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
  # The validation marker skips rebuilding and repeated messages, not checks.
  # An edited object must still be refused if its link is unsupported.
  v <- bayesnec:::validate_family
  tampered <- v("Beta")
  tampered$link <- "probit"
  expect_error(v(tampered, link_source = "symbol"), "bayesnec fits on the")
})

test_that("a function that is not a family constructor is refused clearly", {
  # Reject a non-constructor before its own argument checks run.
  v <- bayesnec:::validate_family
  expect_error(v(mean, link_source = "symbol"), "does not")
  expect_error(v(sum, link_source = "symbol"), "does not")
  expect_error(v(length, link_source = "symbol"), "does not")
})

test_that("a string naming some other function is not read as a family", {
  # Function existence alone is insufficient: only supported family
  # constructors may be resolved from a string.
  v <- bayesnec:::validate_family
  expect_error(v("t"), "not currently implemented")
  expect_error(v("mean"), "not currently implemented")
  expect_error(v("c"), "not currently implemented")
})

test_that("validating an already validated family changes nothing and says nothing", {
  # Validate idempotently so grouped and hurdle wrappers do not repeat the
  # link message when forwarding the family to bnec().
  v <- bayesnec:::validate_family
  once <- v(Beta(link = "logit"), link_source = "chosen")
  expect_silent(twice <- v(once, link_source = "symbol"))
  expect_equal(twice$link, "logit")
  expect_equal(twice$family, "beta")
})

test_that("each entry point reads the link from the expression it was given", {
  # A refused link tests expression capture without sampling: replacing it
  # with identity would suppress the refusal.
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

test_that("the constructor is resolved in function mode", {
  # Resolve constructor names in function mode. An unrelated numeric Beta
  # variable must not change the interpretation of Beta().
  f <- bayesnec:::family_link_source
  e <- new.env(parent = globalenv())
  expect_equal(f(quote(Beta()), env = e), "none")
  assign("Beta", 0.5, envir = e)
  assign("Gamma", 3, envir = e)
  expect_equal(f(quote(Beta()), env = e), "none")
  expect_equal(f(quote(Gamma(link = "log")), env = e), "link")
  # The returned prior also checks this through the public entry point.
  set.seed(256)
  d <- data.frame(x = rep(seq(0, 4, length.out = 10), 3))
  d$y <- pmin(pmax(0.8 * exp(-exp(-0.5) * pmax(d$x - 2, 0)) +
                     rnorm(30, 0, 0.05), 0.02), 0.98)
  local({
    Beta <- 0.5
    expect_equal(
      get_priors(y ~ crf(x, "nec3param"), data = d,
                 family = brms::Beta())$prior[
                   get_priors(y ~ crf(x, "nec3param"), data = d,
                              family = brms::Beta())$nlpar == "top"],
      "beta(5, 2)"
    )
  })
})

test_that("a family carrying the tag brms reports is keyed correctly", {
  # brms stores gamma while bayesnec keys on Gamma. Canonicalise the tag
  # before family-specific restrictions are applied.
  v <- bayesnec:::validate_family
  expect_equal(v("gamma")$family, "Gamma")
  expect_equal(v("gamma")$link, "identity")
  off_a_fit <- brms::brmsfamily("gamma", link = "identity")
  expect_equal(off_a_fit$family, "gamma")
  canonical <- v(off_a_fit, link_source = "symbol")
  expect_equal(canonical$family, "Gamma")
  expect_equal(canonical$link, "identity")
  expect_equal(length(check_models(models()$all, canonical)),
               length(check_models(models()$all, v("Gamma")))) |>
    suppressMessages()
  # Preserve the object's link while canonicalising its family tag.
  logit_off_a_fit <- brms::brmsfamily("gamma", link = "log")
  expect_message(kept <- v(logit_off_a_fit, link_source = "symbol"), "log")
  expect_equal(kept$family, "Gamma")
  expect_equal(kept$link, "log")
})

test_that("a dispersion link survives alongside an assigned second block", {
  # The two interact: link_shape is carried through while link_hu / link_zi is
  # assigned. Neither must disturb the other.
  v <- bayesnec:::validate_family
  f <- bayesnec:::family_link_source
  hg <- v(brms::hurdle_gamma(link_shape = "identity"),
          link_source = f(quote(hurdle_gamma(link_shape = "identity"))))
  expect_equal(hg$link, "identity")
  expect_equal(hg$link_hu, "identity")
  expect_equal(hg$link_shape, "identity")
  zinb <- v(brms::zero_inflated_negbinomial(link_shape = "identity"),
            link_source = f(quote(
              zero_inflated_negbinomial(link_shape = "identity")
            )))
  expect_equal(zinb$link, "identity")
  expect_equal(zinb$link_shape, "identity")
  # Zero-inflated counts estimate a constant zi, without a curve on that block.
  zip <- v(brms::zero_inflated_poisson(link_zi = "identity"),
           link_source = f(quote(zero_inflated_poisson(link_zi = "identity"))))
  expect_equal(zip$link, "identity")
  expect_equal(zip$link_zi, "identity")
})

test_that("the idempotence marker is private to bayesnec", {
  # It must not be serialised into a brmsfit or into a returned fit object.
  v <- bayesnec:::validate_family
  fam <- v("Beta")
  expect_true(isTRUE(attr(fam, "bayesnec_validated")))
  expect_null(attr(bayesnec:::unmark_family(fam), "bayesnec_validated"))
  expect_equal(bayesnec:::unmark_family(fam)$link, "identity")
})

test_that("a dispersion link survives on a family built by stats, or is refused", {
  # The stats constructors cannot accept dispersion-link arguments. Use a
  # constructor that preserves the link, or refuse it; setting the field later
  # is insufficient because brms rebuilds the family.
  v <- bayesnec:::validate_family
  g <- v(brms::brmsfamily("gaussian", link = "identity",
                          link_sigma = "identity"),
         link_source = "link")
  expect_equal(g$family, "gaussian")
  expect_equal(g$link, "identity")
  expect_equal(g$link_sigma, "identity")
  # Keep a tag recognised by the rest of bayesnec.
  expect_equal(length(check_models(models()$all, g)),
               length(check_models(models()$all, v("gaussian")))) |>
    suppressMessages()
  # Gamma cannot carry one: brmsfamily reports the tag `gamma`, which
  # check_models does not key on, and rewriting the tag breaks brms dispatch.
  # Refused rather than dropped.
  expect_error(
    v(brms::brmsfamily("Gamma", link = "identity", link_shape = "identity"),
      link_source = "link"),
    "cannot carry"
  )
})

test_that("an unimplemented family is named before its link is", {
  # For a family bayesnec does not implement at all, naming the link it was
  # given is the less useful of the two errors.
  expect_error(
    bayesnec:::validate_family(brms::student(link = "inverse"),
                               link_source = "symbol"),
    "not currently implemented"
  )
})

test_that("a family forwarded through dots is read from the caller's expression", {
  # match.call() can record forwarded dots as ..1. Follow the promise with
  # substitute() so a wrapper preserves the caller's family expression.
  d <- data.frame(x = rep(1:4, each = 5), y = rep(c(1, 2, 3, 0), 5))
  f <- y ~ crf(x, "nec3param")
  wrap <- function(...) bnec(f, data = d, ...)
  twice <- function(...) wrap(...)
  expect_error(wrap(family = Gamma(link = "inverse")), "bayesnec fits on the")
  expect_error(twice(family = Gamma(link = "inverse")), "bayesnec fits on the")
  # Check specifically for a spurious link-source message. This response
  # contains zeros, so a legitimate boundary-substitution message can occur.
  msgs <- capture.output(
    try(wrap(family = brms::Beta()), silent = TRUE), type = "message"
  )
  expect_false(any(grepl("taken from the family", msgs)))
})

# Canonicalise gamma and Gamma when reading stored family objects.

test_that("canonical_fam_tag names Gamma one way, from a tag or an object", {
  f <- bayesnec:::canonical_fam_tag
  expect_identical(f("gamma"), "Gamma")
  expect_identical(f("Gamma"), "Gamma")
  expect_identical(f(brms::brmsfamily("Gamma", link = "identity")), "Gamma")
  expect_identical(f(stats::Gamma(link = "identity")), "Gamma")
  # The two-block family keeps its own tag: it is not an alias of Gamma.
  expect_identical(f(brms::hurdle_gamma()), "hurdle_gamma")
  expect_identical(f("hurdle_gamma"), "hurdle_gamma")
  expect_identical(f(brms::Beta()), "beta")
  expect_identical(f(brms::bernoulli()), "bernoulli")
  expect_identical(f(NULL), NA_character_)
  expect_identical(f(structure(list(), class = "family")), NA_character_)
})

test_that("canonical_fam_tag is a no-op on every family bnec() fits with", {
  # Accepting objects as well as tags must leave already validated families
  # and their canonical tags unchanged.
  f <- bayesnec:::canonical_fam_tag
  for (tag in names(mod_fams)) {
    fam <- validate_family(tag)
    expect_identical(f(tag), tag, label = tag)
    expect_identical(f(fam), fam$family, label = tag)
    expect_identical(f(fam$family), fam$family, label = tag)
  }
})

test_that("canonical_fam_tag reads every stored family as validated (#436)", {
  # Gamma is the one family whose stored tag differs from the validated one.
  f <- bayesnec:::canonical_fam_tag
  differs <- character(0)
  for (tag in names(mod_fams)) {
    validated <- validate_family(tag)
    stored <- brms::bf(y ~ x, family = unmark_family(validated))$family
    expect_identical(f(stored), validated$family, label = tag)
    if (!identical(stored$family, validated$family)) {
      differs <- c(differs, tag)
    }
  }
  expect_identical(differs, "Gamma")
})
