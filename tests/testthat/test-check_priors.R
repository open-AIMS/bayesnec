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
  args_for <- function(family, model = "nec3param", spec = gs) {
    add_brm_defaults(list(), model, validate_family(family), d$x, d$y,
                     skip_check = TRUE, custom_name = NULL,
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
  scale_of <- function(kind) {
    spec <- gs
    spec$ogl_transform <- kind
    pr <- define_group_prior(spec, x, y)
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
})
