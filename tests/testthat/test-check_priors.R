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
  # init = "random" skips the initial-value search. It is not avoided for
  # speed alone: this fixture cannot be initialised at all, so the search runs
  # to the 1e4 cap -- 460 seconds measured -- and these assertions are about
  # adapt_delta, not about inits. test-define_prior.R does the same thing for
  # the same reason.
  args_for <- function(family, model = "nec3param", spec = gs) {
    add_brm_defaults(list(init = "random"), model, validate_family(family),
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
  # A pgl term is not transformed in this landing, so it keeps the raise.
  gs_pgl <- parse_group_terms(bnf(y ~ crf(x, model = "nec3param") + pgl(g)),
                              "nec3param")
  expect_equal(args_for("Beta", "nec3param", gs_pgl), 0.99)
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
  fitted_pr <- suppressMessages(add_brm_defaults(
    list(init = "random"), "nec3param", validate_family("Beta"), d$x, d$y,
    skip_check = TRUE, custom_name = NULL, group_spec = gs
  ))$prior
  reported <- suppressMessages(
    get_priors(f, data = d, family = Beta(link = "identity"))
  )
  ogl_of <- function(p) sort(p$prior[p$nlpar == "ogl"])
  expect_equal(ogl_of(reported), ogl_of(fitted_pr))
})
