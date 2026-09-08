test_that("returns ggplot for bayesnecfit objects", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  p <- check_priors(nec4param)
  expect_error(print(p), NA)
  expect_silent(check_priors(nec4param))
  # Check inheritance rather than the exact class vector: ggplot2 >= 4.0 builds
  # plots as S7 objects, so class(p) gained extra entries ("ggplot2::ggplot",
  # "S7_object", ...) and an exact-equality test breaks across versions.
  expect_s3_class(p, "ggplot")
})

test_that("returns pdf for bayesmanecfit objects", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  filename <- random_filename(15)
  expect_invisible(check_priors(manec_example, filename = filename))
  on.exit(file.remove(paste(filename, ".pdf", sep = "")))
})


# ---- #257, what the transform changes downstream of the formula --------------

test_that("adapt_delta is not raised for a transformed ogl term", {
  # The raise costs roughly fourteen times the gradient evaluations per
  # iteration and exists to mitigate excursions outside the support. A
  # multiplicative deviation cannot make those excursions, so there is nothing
  # left to mitigate. Everything else keeps needing it.
  set.seed(1)
  d <- data.frame(x = rep(c(1, 5, 20, 100), each = 8),
                  g = rep(letters[1:4], 8))
  d$y <- pmin(pmax(stats::rnorm(32, 0.9 - 0.8 * (d$x > 5), 0.05), 0.01), 0.99)
  f <- bnf(y ~ crf(x, model = "nec3param") + ogl(g))
  bdat <- model.frame(f, data = d, run_par_checks = TRUE)
  gs <- parse_group_terms(f, "nec3param")
  # The initial-value search is mocked away. This fixture cannot be initialised
  # at all, so the search runs to the 1e4 cap -- 577 seconds measured -- and
  # these assertions are about adapt_delta, not about inits. Since #290 an
  # init = "random" in the argument list would suppress the search here too;
  # the mock is kept because it states what these calls need directly, rather
  # than through a second argument whose effect on the search is incidental.
  local_mocked_bindings(
    make_good_inits = function(...) list(random = "random"),
    .package = "bayesnec"
  )
  args_for <- function(family, model = "nec3param", spec = gs) {
    add_brm_defaults(list(), model, validate_family(family),
                     d$x, d$y, skip_check = TRUE, custom_name = NULL,
                     group_spec = spec)$control$adapt_delta
  }
  # Transformed: no raise.
  expect_null(args_for("Beta"))
  # Not transformed because the mean is unconstrained: no raise either, which
  # is #245's existing behaviour and is unchanged.
  expect_null(args_for("gaussian"))
  # Not transformed because the equation's mean is unbounded below: the raise
  # is still needed and is still applied.
  gs_lin <- parse_group_terms(bnf(y ~ crf(x, model = "neclin") + ogl(g)),
                              "neclin")
  expect_equal(args_for("Beta", "neclin", gs_lin), 0.99)
  # #294 transforms a pgl term's deviation on top and bot as well, and on
  # nec3param the mean lies between zero and top, so no group-level term can
  # take it out of the support and the raise goes with it. Measured on
  # herbicide, Beta(link = "identity"), nec4param: 0 divergent transitions of
  # 2000 at Stan's default adapt_delta for a (bot | herbicide) term, against 51
  # at 0.95 before the transform.
  gs_pgl <- parse_group_terms(bnf(y ~ crf(x, model = "nec3param") + pgl(g)),
                              "nec3param")
  expect_null(args_for("Beta", "nec3param", gs_pgl))
  # It is kept where the mean can leave the support with every parameter inside
  # it: neclin is unbounded below, and the hormesis equations can exceed 1
  # through exp(slope) * x.
  gs_pgl_lin <- parse_group_terms(bnf(y ~ crf(x, model = "neclin") + pgl(g)),
                                  "neclin")
  expect_equal(args_for("Beta", "neclin", gs_pgl_lin), 0.99)
  gs_pgl_horme <- parse_group_terms(
    bnf(y ~ crf(x, model = "nechorme") + pgl(g)), "nechorme"
  )
  expect_equal(args_for("Beta", "nechorme", gs_pgl_horme), 0.99)
})

test_that("the ogl prior is widened onto the scale the deviation is applied on", {
  # s_y is a width on the response scale. Once the deviation is applied
  # multiplicatively it is on the log or log-odds scale, where s_y is the wrong
  # width. These are delta-method conversions evaluated at mean(y).
  y <- c(rep(0.9, 8), rep(0.6, 8), rep(0.3, 8), rep(0.1, 8))
  x <- rep(c(1, 5, 20, 100), each = 8)
  gs <- list(nlpars = "ogl", ogl = TRUE)
  # The kind is an argument rather than a field on group_spec: it used to be
  # set in add_brm_defaults() alone, so get_priors() and amend() fell through
  # to the response-scale width and disagreed with what bnec() fitted. See the
  # round-trip assertion in the next test.
  scale_of <- function(kind) {
    pr <- define_group_prior(gs, x, y, ogl_transform = kind)
    as.numeric(sub(".*normal\\(0, ([0-9.e+-]+)\\).*", "\\1",
                   pr$prior[pr$nlpar == "ogl" & pr$class == "b"]))
  }
  s_y <- diff(range(y)) / 10
  m_y <- mean(y)
  expect_equal(scale_of("none"), signif(s_y, 4))
  expect_equal(scale_of("log"), signif(s_y / m_y, 4))
  expect_equal(scale_of("logit"), signif(s_y / (m_y * (1 - m_y)), 4))
  # The logit conversion is the widest of the three here, because m(1-m) is
  # below 1 everywhere on the unit interval.
  expect_gt(scale_of("logit"), scale_of("none"))
  # The log conversion is capped at a coefficient of variation of 1, because
  # s_y does not shrink as the response mean approaches zero and the ratio is
  # otherwise unbounded there.
  tiny <- c(rep(0, 30), 1, 2, 3, 5, 20)
  spec <- gs
  pr <- define_group_prior(spec, x[seq_along(tiny)], tiny,
                           ogl_transform = "log")
  capped <- as.numeric(sub(".*normal\\(0, ([0-9.e+-]+)\\).*", "\\1",
                           pr$prior[pr$nlpar == "ogl" & pr$class == "b"]))
  expect_equal(capped, 1)
  expect_lt(capped, (diff(range(tiny)) / 10) / mean(tiny))
})

test_that("get_priors reports the ogl prior bnec() actually fits (#257)", {
  # group_spec$ogl_transform was set in add_brm_defaults() and nowhere else, so
  # get_priors() and amend() -- which build group_spec from parse_group_terms()
  # -- used the response-scale width while bnec() fitted the transformed one, a
  # factor of four apart on this fixture. get_priors() misreported the prior in
  # use, which the initial-value fallback message tells the user to trust, and
  # amend() fitted a model into an existing set with a prior no other member
  # had.
  skip_on_cran()
  set.seed(1)
  d <- data.frame(x = rep(c(1, 5, 20, 100), each = 8),
                  g = rep(letters[1:4], 8))
  d$y <- pmin(pmax(stats::rnorm(32, 0.9 - 0.8 * (d$x > 5), 0.05), 0.01), 0.99)
  f <- y ~ crf(x, model = "nec3param") + ogl(g)
  gs <- parse_group_terms(bnf(f), "nec3param")
  # Same mock, same reason: the prior is what is under test, not the inits.
  local_mocked_bindings(
    make_good_inits = function(...) list(random = "random"),
    .package = "bayesnec"
  )
  fitted_pr <- suppressMessages(add_brm_defaults(
    list(), "nec3param", validate_family("Beta"), d$x, d$y,
    skip_check = TRUE, custom_name = NULL, group_spec = gs
  ))$prior
  reported <- suppressMessages(
    get_priors(f, data = d, family = Beta(link = "identity"))
  )
  ogl_of <- function(p) sort(p$prior[p$nlpar == "ogl"])
  expect_equal(ogl_of(reported), ogl_of(fitted_pr))
})


# ---- #294, the bounds the multiplicative form on (0, 1) rests on -------------

test_that("a user prior that unbounds a transformed parameter is refused", {
  # m * exp(o) / (1 - m + m * exp(o)) is safe because m is inside (0, 1).
  # define_prior() guarantees that for the priors it generates, giving top and
  # bot lb = 0 and ub = 1, but fill_missing_priors() preserves a user row and
  # fills only what is absent -- so a user prior with no bounds merges with lb
  # and ub NA and brms declares b_bot unbounded. Outside [0, 1] the expression
  # has a pole at o = log((m - 1) / m) and changes sign across it, which the
  # additive form it replaces would not have done: that would have produced an
  # out-of-range mean Stan rejects visibly rather than a large finite number it
  # accepts.
  set.seed(294)
  d <- data.frame(y = runif(60, 0.05, 0.9),
                  x = rep(log(c(0.1, 1, 10, 100, 1000, 1e4)), 10),
                  g = factor(rep(1:5, each = 12)))
  beta <- validate_family("Beta")
  spec <- list(nlpars = "bot", ogl = FALSE)
  args_with <- function(pr, group_spec = spec, family = beta) {
    suppressMessages(suppressWarnings(add_brm_defaults(
      list(chains = 2, prior = pr), "nec4param", family, d$x, d$y,
      skip_check = TRUE, custom_name = NULL, group_spec = group_spec
    )))
  }
  unbounded <- brms::prior_string("normal(0.2, 0.5)", nlpar = "bot")
  expect_error(args_with(unbounded), "do not bound the parameter")
  expect_error(args_with(unbounded), "multiplicatively")
  # The remedy the message names works.
  bounded <- brms::prior_string("normal(0.2, 0.5)", nlpar = "bot",
                                lb = 0, ub = 1)
  expect_s3_class(args_with(bounded)$prior, "brmsprior")
  # And so does the other remedy: without a group-level term on bot there is no
  # transform, so an unbounded prior is the user's business.
  expect_s3_class(
    args_with(unbounded, group_spec = list(nlpars = "nec", ogl = FALSE))$prior,
    "brmsprior"
  )
  # gaussian is unconstrained, so nothing is transformed and nothing is refused.
  expect_s3_class(
    args_with(unbounded, family = validate_family("gaussian"))$prior,
    "brmsprior"
  )
  # On a positive-support family only the lower bound is required.
  gam <- validate_family("Gamma")
  dg <- transform(d, y = y * 10)
  lb_only <- brms::prior_string("gamma(2, 1)", nlpar = "bot", lb = 0)
  expect_s3_class(
    suppressMessages(suppressWarnings(add_brm_defaults(
      list(chains = 2, prior = lb_only), "nec4param", gam, dg$x, dg$y,
      skip_check = TRUE, custom_name = NULL, group_spec = spec
    )))$prior,
    "brmsprior"
  )
})

test_that("the adapt_delta raise is kept for the hormesis equations on every family", {
  # The first version of #294 delegated this to ogl_transform_kind(), which
  # tests can_exceed_one on the (0, 1) branch only, so the hormesis equations
  # lost the raise under Gamma and the counts, where 2.1.4 applied it. Their
  # mean can leave a (0, Inf) support too, and a deviation on slope is what
  # makes that more likely.
  set.seed(294)
  d <- data.frame(y = runif(60, 0.05, 0.9),
                  x = rep(log(c(0.1, 1, 10, 100, 1000, 1e4)), 10))
  spec <- list(nlpars = c("top", "slope", "nec", "beta"), ogl = FALSE)
  ad <- function(model, fam) {
    suppressMessages(add_brm_defaults(
      list(chains = 2), model, validate_family(fam), d$x, d$y,
      skip_check = TRUE, custom_name = NULL, group_spec = spec
    ))$control$adapt_delta
  }
  for (m in c("nechorme", "nechorme4", "ecxhormebc4", "ecxhormebc5",
              "nechormepwr", "nechormepwr01")) {
    for (fam in c("Beta", "Gamma", "poisson", "negbinomial")) {
      expect_equal(ad(m, fam), 0.99, label = paste(m, fam))
    }
  }
  # And is still dropped where the mean is confined by its own parameters.
  for (fam in c("Beta", "Gamma", "poisson")) {
    expect_null(ad("nec4param", fam))
    expect_null(ad("ecxwb1", fam))
  }
  # And still kept where the equation is unbounded below.
  for (fam in c("Beta", "Gamma")) {
    expect_equal(ad("neclin", fam), 0.99)
  }
})
