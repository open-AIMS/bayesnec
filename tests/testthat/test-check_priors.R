test_that("returns ggplot for bayesnecfit objects", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  p <- check_priors(nec4param)
  expect_error(print(p), NA)
  expect_silent(check_priors(nec4param))
  # Check inheritance: ggplot objects can have additional S7 classes.
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

# A null PDF device stands in for the screen while the test checks that
# check_priors() restores the page prompt.
test_that("check_priors on a model set restores the page prompt it set", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  pdf(NULL)
  screen <- dev.cur()
  on.exit(if (screen %in% dev.list()) dev.off(screen), add = TRUE)
  devAskNewPage(TRUE)
  check_priors(manec_example, ask = FALSE)
  expect_true(devAskNewPage())
  devAskNewPage(FALSE)
  check_priors(manec_example, ask = TRUE)
  expect_false(devAskNewPage())
})

test_that("check_priors restores the prompt and closes its PDF on an error", {
  local_mocked_bindings(pull_out = function(...) stop("drawing failed"),
                        .package = "bayesnec")
  pdf(NULL)
  screen <- dev.cur()
  on.exit(if (screen %in% dev.list()) dev.off(screen), add = TRUE)
  devAskNewPage(FALSE)
  expect_error(check_priors(manec_example, ask = TRUE), "drawing failed")
  expect_false(devAskNewPage())
  before <- dev.list()
  filename <- tempfile()
  expect_error(check_priors(manec_example, filename = filename),
               "drawing failed")
  expect_identical(dev.list(), before)
  unlink(paste0(filename, ".pdf"))
})

test_that("check_priors with filename opens no device besides its PDF", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # Closing the PDF must not open a screen device while restoring the prompt.
  before <- dev.list()
  filename <- tempfile()
  on.exit(unlink(paste0(filename, ".pdf")), add = TRUE)
  expect_message(check_priors(manec_example, filename = filename),
                 "saved to file")
  expect_identical(dev.list(), before)
  expect_true(file.exists(paste0(filename, ".pdf")))
})



test_that("adapt_delta is not raised for a transformed ogl term", {
  # Support-preserving transforms remove the excursion that the adapt_delta
  # raise addresses. Keep the raise where the equation can still leave support.
  set.seed(1)
  d <- data.frame(x = rep(c(1, 5, 20, 100), each = 8),
                  g = rep(letters[1:4], 8))
  d$y <- pmin(pmax(stats::rnorm(32, 0.9 - 0.8 * (d$x > 5), 0.05), 0.01), 0.99)
  f <- bnf(y ~ crf(x, model = "nec3param") + ogl(g))
  bdat <- model.frame(f, data = d, run_par_checks = TRUE)
  gs <- parse_group_terms(f, "nec3param")
  # Mock the initial-value search: this fixture exhausts its search cap, and
  # these assertions concern adapt_delta.
  local_mocked_bindings(
    make_good_inits = function(...) list(random = "random"),
    .package = "bayesnec"
  )
  args_for <- function(family, model = "nec3param", spec = gs) {
    add_brm_defaults(list(), model, validate_family(family),
                     d$x, d$y, skip_check = TRUE, custom_name = NULL,
                     group_spec = spec)$control$adapt_delta
  }
  expect_null(args_for("Beta"))
  # An unconstrained Gaussian mean also needs no excursion-specific raise.
  expect_null(args_for("gaussian"))
  # Not transformed because the equation's mean is unbounded below: the raise
  # is still needed and is still applied.
  gs_lin <- parse_group_terms(bnf(y ~ crf(x, model = "neclin") + ogl(g)),
                              "neclin")
  expect_equal(args_for("Beta", "neclin", gs_lin), 0.99)
  # On nec3param the mean lies between zero and top, and the top deviation is
  # transformed, so no pgl deviation takes it outside and no raise applies.
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
  # Pass the transform kind explicitly so get_priors(), amend() and bnec()
  # use the same scale.
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
  # Compare the reported prior with the fitted prior: both routes must apply
  # the transform-scale conversion.
  skip_on_cran()
  set.seed(1)
  d <- data.frame(x = rep(c(1, 5, 20, 100), each = 8),
                  g = rep(letters[1:4], 8))
  d$y <- pmin(pmax(stats::rnorm(32, 0.9 - 0.8 * (d$x > 5), 0.05), 0.01), 0.99)
  f <- y ~ crf(x, model = "nec3param") + ogl(g)
  gs <- parse_group_terms(bnf(f), "nec3param")
  # Mock the search because this assertion concerns the prior, not inits.
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



test_that("a user prior that unbounds a transformed parameter is refused", {
  # The collapsed logit transform requires a population parameter in [0, 1].
  # A user prior can remove those bounds; outside them the expression has a
  # pole and can change sign, so refuse the unbounded prior before fitting.
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
  bounded <- brms::prior_string("normal(0.2, 0.5)", nlpar = "bot",
                                lb = 0, ub = 1)
  expect_s3_class(args_with(bounded)$prior, "brmsprior")
  # Without a group-level transform, this bound requirement does not apply.
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
  # Hormesis can leave positive as well as unit-interval support. A group
  # deviation on slope therefore still requires the adapt_delta raise.
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
  for (fam in c("Beta", "Gamma", "poisson")) {
    expect_null(ad("nec4param", fam))
    expect_null(ad("ecxwb1", fam))
  }
  for (fam in c("Beta", "Gamma")) {
    expect_equal(ad("neclin", fam), 0.99)
  }
})
