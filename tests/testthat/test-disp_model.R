disp_dat <- data.frame(
  x = rep(c(0.1, 0.3, 1, 3, 10), each = 6),
  tank = rep(letters[1:3], 10)
)
set.seed(10)
disp_dat$y <- 0.7 - 0.6 / (1 + exp(-(log(disp_dat$x) - log(1)))) +
  rnorm(30, 0, 0.02)
disp_dat$prop <- pmin(pmax(disp_dat$y, 0.01), 0.99)
disp_dat$signed <- disp_dat$y - 0.4
disp_dat$count <- as.integer(round(disp_dat$y * 30))

test_that("has_disp_par and disp_dpar cover the right families", {
  expect_true(bayesnec:::has_disp_par("gaussian"))
  expect_true(bayesnec:::has_disp_par(Gamma(link = "identity")))
  expect_true(bayesnec:::has_disp_par(brms::Beta(link = "identity")))
  # the variance is a deterministic function of the mean for these
  expect_false(bayesnec:::has_disp_par("poisson"))
  expect_false(bayesnec:::has_disp_par("bernoulli"))
  expect_false(bayesnec:::has_disp_par("binomial"))
  expect_equal(bayesnec:::disp_dpar("gaussian"), "sigma")
  expect_equal(bayesnec:::disp_dpar("Gamma"), "shape")
  expect_equal(bayesnec:::disp_dpar("beta"), "phi")
  expect_null(bayesnec:::disp_dpar("poisson"))
})

test_that("parse_disp_term tells the two routes apart", {
  expect_null(bayesnec:::parse_disp_term(bnf(y ~ crf(x, "nec3param"))))
  a <- bayesnec:::parse_disp_term(bnf(y ~ crf(x, "nec3param") + disp(~x)))
  expect_equal(a$route, "A")
  expect_equal(a$value, "x")
  b <- bayesnec:::parse_disp_term(bnf(y ~ crf(x, "nec3param") + disp("power")))
  expect_equal(b$route, "B")
  expect_equal(b$value, "power")
})

test_that("parse_disp_term keeps a route A sub-model verbatim", {
  # the right-hand side is handed to brms untouched, so terms it understands
  # must survive parsing rather than being evaluated here
  a <- bayesnec:::parse_disp_term(bnf(y ~ crf(x, "nec3param") + disp(~log(x))))
  expect_equal(a$value, "log(x)")
})

test_that("only one disp term is allowed", {
  expect_error(
    bayesnec:::parse_disp_term(
      bnf(y ~ crf(x, "nec3param") + disp("power") + disp(~x))
    ),
    "more than one disp"
  )
})

test_that("an inline sub-model that the term splitter tears apart is reported", {
  # the rhs is split on ") + ", so disp(~s(x) + group) arrives here in pieces;
  # the failure should name the limitation rather than surface as a parse error
  expect_error(
    bayesnec:::parse_disp_term(
      bnf(y ~ crf(x, "nec3param") + disp(~s(x) + tank))
    ),
    "cannot currently be written inline"
  )
})

test_that("disp_pars reports the parameters each route introduces", {
  expect_equal(bayesnec:::disp_pars(NULL), character(0))
  # route A introduces ordinary population-level terms, not non-linear ones
  expect_equal(bayesnec:::disp_pars(list(route = "A", value = "x")),
               character(0))
  expect_equal(bayesnec:::disp_pars(list(route = "B", value = "power")),
               c("c0", "c1"))
  expect_equal(bayesnec:::disp_pars(list(route = "B", value = "twosided")),
               c("c0", "c1", "c2"))
})

test_that("make_disp_block duplicates the curve rather than the fitted value", {
  spec <- list(route = "B", value = "power")
  db <- bayesnec:::make_disp_block("ecx4param", spec, "sigma", "x")
  rhs <- deparse1(db$nlf[[3]])
  curve <- deparse1(bayesnec:::bf_ecx4param$formula[[3]])
  expect_true(grepl(curve, rhs, fixed = TRUE))
  # the curve parameters are shared with mu, so must NOT be renamed the way
  # the hurdle block renames them
  expect_true(grepl("top", rhs, fixed = TRUE))
  expect_false(grepl("sigmatop", rhs, fixed = TRUE))
  expect_equal(sort(all.vars(db$lf[[2]])), c("c0", "c1"))
})

test_that("make_disp_block wraps the curve so twosided binds correctly", {
  # log(1 - (curve)) must bracket the whole curve; without the parentheses a
  # curve that is a sum would rebind against the subtraction
  spec <- list(route = "B", value = "twosided")
  db <- bayesnec:::make_disp_block("ecx4param", spec, "phi", "x")
  rhs <- deparse1(db$nlf[[3]])
  curve <- deparse1(bayesnec:::bf_ecx4param$formula[[3]])
  expect_true(grepl(paste0("log(1 - ((", curve, ")))"), rhs, fixed = TRUE))
})

test_that("disp_centre returns the reference the form asks for", {
  y <- c(1, 10, 100, 1000)
  pw <- bayesnec:::disp_centre(list(route = "B", value = "power"), y)
  expect_named(pw, "LOGREF")
  # geometric median: the median on the scale the covariate is measured on
  expect_equal(unname(pw[["LOGREF"]]), signif(median(log(y)), 6))
  ll <- bayesnec:::disp_centre(list(route = "B", value = "loglinear"), y)
  expect_named(ll, "REF")
  expect_equal(unname(ll[["REF"]]), signif(median(y), 6))
  ts <- bayesnec:::disp_centre(list(route = "B", value = "twosided"),
                               c(0.1, 0.2, 0.8))
  expect_named(ts, c("LOGREF", "LOG1MREF"))
  # route A introduces no parameters and so needs no reference
  expect_length(bayesnec:::disp_centre(list(route = "A", value = "x"), y), 0)
})

test_that("the variance function covariate is centred on the response", {
  # uncentred, c0 is the dispersion parameter at mu = 1 (power) or mu = 0
  # (loglinear); both are far outside a response of order 1e4, which confounds
  # c0 with the slope. The reference must appear in the built expression.
  y <- c(15000, 18000, 20000, 400)
  spec <- list(route = "B", value = "power")
  db <- bayesnec:::make_disp_block("ecx4param", spec, "sigma", "x", y)
  rhs <- deparse1(db$nlf[[3]])
  expect_true(grepl(as.character(signif(median(log(y)), 6)), rhs, fixed = TRUE))
  # and it must be a literal constant, not a parameter to be estimated
  expect_equal(sort(all.vars(db$lf[[2]])), c("c0", "c1"))

  spec_ll <- list(route = "B", value = "loglinear")
  db_ll <- bayesnec:::make_disp_block("ecx4param", spec_ll, "sigma", "x", y)
  expect_true(grepl(as.character(signif(median(y), 6)),
                    deparse1(db_ll$nlf[[3]]), fixed = TRUE))

  # a large reference must not be rendered in scientific notation, which would
  # not parse back as part of a formula the way it is spliced in
  big <- bayesnec:::make_disp_block("ecx4param", spec_ll, "sigma", "x",
                                    c(1e6, 2e6))
  expect_false(grepl("e+", deparse1(big$nlf[[3]]), fixed = TRUE))
})

test_that("centring makes the model frame reachable end to end", {
  bf_b <- make_brmsformula(bnf(y ~ crf(x, "ecx4param") + disp("power")),
                           disp_dat, gaussian(link = "identity"))[[1]]
  rhs <- deparse1(bf_b$pforms$sigma[[3]])
  ref <- signif(median(log(disp_dat$y[disp_dat$y > 0])), 6)
  expect_true(grepl(as.character(ref), rhs, fixed = TRUE))
})

test_that("route B refuses a non-identity link, route A does not", {
  # the curve expression substituted into a variance function is the linear
  # predictor on the link scale, so it is the mean only under identity. Under
  # Gamma's inverse link the same fit runs, converges and returns the slope with
  # the wrong sign, so this has to be refused rather than left to the user.
  spec_b <- list(route = "B", value = "power")
  expect_error(
    bayesnec:::check_disp_spec(spec_b, Gamma(), response = c(1, 2, 3)),
    "identity"
  )
  expect_error(
    bayesnec:::check_disp_spec(spec_b, Beta(), response = c(0.2, 0.5, 0.8)),
    "identity"
  )
  expect_silent(
    bayesnec:::check_disp_spec(spec_b, Gamma(link = "identity"),
                               response = c(1, 2, 3))
  )
  # gaussian has only the identity link, so it can never trip this
  expect_silent(
    bayesnec:::check_disp_spec(spec_b, gaussian(), response = c(1, 2, 3))
  )
  # route A is an ordinary distributional formula and never touches the curve
  expect_silent(
    bayesnec:::check_disp_spec(list(route = "A", value = "x"), Gamma(),
                               response = c(1, 2, 3))
  )
})

test_that("a non-identity link is refused end to end", {
  expect_error(
    make_brmsformula(bnf(y ~ crf(x, "ecx4param") + disp("power")),
                     disp_dat, Gamma(link = "inverse")),
    "identity"
  )
})

test_that("disp_inits starts every slope at the constant-dispersion null", {
  y <- c(0.5, 1, 2, 4)
  ii <- bayesnec:::disp_inits(list(route = "B", value = "power"), gaussian(), y)
  expect_named(ii, c("b_c0", "b_c1"))
  # a slope of zero is the no-relationship model: the sign of a slope is tied
  # to the direction of the mean curve, so chains must not each pick their own
  expect_equal(as.numeric(ii$b_c1), 0)
  expect_equal(as.numeric(ii$b_c0), log(sd(y)))
  # brms wants one-dimensional arrays, not bare scalars
  expect_true(all(vapply(ii, function(z) !is.null(dim(z)), TRUE)))

  ts <- bayesnec:::disp_inits(list(route = "B", value = "twosided"),
                              Beta(), c(0.2, 0.5, 0.8))
  expect_named(ts, c("b_c0", "b_c1", "b_c2"))
  expect_equal(as.numeric(ts$b_c1), 0)
  expect_equal(as.numeric(ts$b_c2), 0)
  expect_equal(as.numeric(ts$b_c0), 4)

  # route A introduces no non-linear parameters, so there is nothing to seed
  expect_length(bayesnec:::disp_inits(list(route = "A", value = "x"),
                                      gaussian(), y), 0)
})

test_that("make_disp_block substitutes the real predictor name", {
  spec <- list(route = "B", value = "power")
  db <- bayesnec:::make_disp_block("nec3param", spec, "sigma", "conc")
  expect_true("conc" %in% all.vars(db$nlf))
  expect_false("x" %in% all.vars(db$nlf))
})

test_that("route A produces a plain distributional formula", {
  bf_a <- make_brmsformula(bnf(y ~ crf(x, "ecx4param") + disp(~x)),
                           disp_dat, gaussian(link = "identity"))[[1]]
  expect_true("sigma" %in% names(bf_a$pforms))
  expect_equal(deparse1(bf_a$pforms$sigma[[3]]), "x")
  # no new non-linear parameters
  expect_false(any(c("c0", "c1") %in% names(bf_a$pforms)))
})

test_that("route B produces a variance function on the fitted mean", {
  bf_b <- make_brmsformula(bnf(y ~ crf(x, "ecx4param") + disp("power")),
                           disp_dat, gaussian(link = "identity"))[[1]]
  expect_true(all(c("sigma", "c0", "c1") %in% names(bf_b$pforms)))
  expect_true(grepl("log(", deparse1(bf_b$pforms$sigma[[3]]), fixed = TRUE))
})

test_that("the dispersion parameter is named per family", {
  bf_g <- make_brmsformula(bnf(y ~ crf(x, "ecx4param") + disp("power")),
                           disp_dat, Gamma(link = "identity"))[[1]]
  expect_true("shape" %in% names(bf_g$pforms))
  bf_b <- make_brmsformula(bnf(prop ~ crf(x, "ecx4param") + disp("power")),
                           disp_dat, brms::Beta(link = "identity"))[[1]]
  expect_true("phi" %in% names(bf_b$pforms))
})

test_that("disp variables are not mistaken for group-level variables", {
  # without this they fall through to the group-level slot and silently
  # acquire a random effect on every curve parameter
  mf <- model.frame(bnf(y ~ crf(x, "ecx4param") + disp(~tank)), disp_dat)
  expect_true(all(is.na(attr(mf, "bnec_group"))))
  # a genuine group-level term still registers alongside a disp term
  mf2 <- model.frame(bnf(y ~ crf(x, "ecx4param") + ogl(tank) + disp("power")),
                     disp_dat)
  expect_equal(unname(attr(mf2, "bnec_group")), "tank")
})

test_that("disp is rejected for families with no dispersion parameter", {
  for (fam in list(poisson(link = "identity"), brms::bernoulli(link = "identity"))) {
    expect_error(
      make_brmsformula(bnf(count ~ crf(x, "ecx4param") + disp("power")),
                       disp_dat, fam),
      "no free dispersion parameter"
    )
  }
})

test_that("disp is rejected for the two-block families", {
  expect_error(
    make_brmsformula(bnf(y ~ crf(x, "ecx4param") + disp("power")), disp_dat,
                     brms::hurdle_gamma(link = "identity",
                                        link_hu = "identity")),
    "not currently supported for the two-block family"
  )
})

test_that("a variance function is rejected outside the families it suits", {
  expect_error(
    make_brmsformula(bnf(y ~ crf(x, "ecx4param") + disp("twosided")),
                     disp_dat, gaussian(link = "identity")),
    "not valid for the gaussian family"
  )
})

test_that("an unknown variance function is rejected at parse time", {
  expect_error(
    make_brmsformula(bnf(y ~ crf(x, "ecx4param") + disp("wibble")),
                     disp_dat, gaussian(link = "identity")),
    "one-sided formula"
  )
})

test_that("a route A variable must exist in the data", {
  expect_error(
    model.frame(bnf(y ~ crf(x, "ecx4param") + disp(~nope)), disp_dat),
    "not found in dataset"
  )
})

test_that("loglinear is linear in mu rather than in log(mu)", {
  spec <- list(route = "B", value = "loglinear")
  db <- bayesnec:::make_disp_block("ecx4param", spec, "sigma", "x", c(2, 4, 6))
  rhs <- deparse1(db$nlf[[3]])
  curve <- deparse1(bayesnec:::bf_ecx4param$formula[[3]])
  # the curve enters as (curve) - reference, never inside a log(). Asserted on
  # the pieces rather than the exact bracketing, which is an artefact of
  # substituting a parenthesised curve into a parenthesised slot.
  expect_true(grepl(curve, rhs, fixed = TRUE))
  expect_true(grepl(") - 4)", rhs, fixed = TRUE))
  expect_false(grepl("log(", rhs, fixed = TRUE))
})

test_that("loglinear is allowed where the response crosses zero", {
  # the growth-rate case the log forms cannot reach
  bf_l <- make_brmsformula(bnf(signed ~ crf(x, "ecx4param") + disp("loglinear")),
                           disp_dat, gaussian(link = "identity"))[[1]]
  expect_true(all(c("sigma", "c0", "c1") %in% names(bf_l$pforms)))
})

test_that("the loglinear slope prior is scaled to the response", {
  # c1 multiplies mu, so it carries units of 1/response; a fixed scale would
  # mean different things for differently-scaled responses
  spec <- list(route = "B", value = "loglinear")
  wide <- disp_dat$y * 1000
  pr_n <- bayesnec:::define_prior("ecx4param", gaussian(link = "identity"),
                                  disp_dat$x, disp_dat$y, disp_spec = spec)
  pr_w <- bayesnec:::define_prior("ecx4param", gaussian(link = "identity"),
                                  disp_dat$x, wide, disp_spec = spec)
  expect_false(identical(pr_n$prior[pr_n$nlpar == "c1"],
                         pr_w$prior[pr_w$nlpar == "c1"]))
  get_sd <- function(p) as.numeric(sub(".*, *([0-9.e+-]+)\\)$", "\\1", p))
  expect_equal(get_sd(pr_w$prior[pr_w$nlpar == "c1"]),
               get_sd(pr_n$prior[pr_n$nlpar == "c1"]) / 1000,
               tolerance = 1e-3)
  # the log forms stay dimensionless and so stay fixed
  pr_p <- bayesnec:::define_prior("ecx4param", gaussian(link = "identity"),
                                  disp_dat$x, wide,
                                  disp_spec = list(route = "B",
                                                   value = "power"))
  expect_equal(pr_p$prior[pr_p$nlpar == "c1"], "normal(0, 2)")
})

test_that("a power law is refused where the fitted mean crosses zero", {
  # the growth-rate case: specific growth rate, yield and increment can all be
  # negative, and log(mu) is undefined there
  expect_error(
    make_brmsformula(bnf(signed ~ crf(x, "ecx4param") + disp("power")),
                     disp_dat, gaussian(link = "identity")),
    "crosses zero"
  )
  # route A is unaffected, being a function of the predictor
  expect_silent(
    make_brmsformula(bnf(signed ~ crf(x, "ecx4param") + disp(~x)),
                     disp_dat, gaussian(link = "identity"))
  )
})

test_that("make_brmsformula needs a family to resolve a disp term", {
  expect_error(
    make_brmsformula(bnf(y ~ crf(x, "ecx4param") + disp("power")), disp_dat),
    "needs the model family"
  )
})

test_that("define_disp_prior covers every new parameter", {
  spec <- list(route = "B", value = "power")
  pr <- bayesnec:::define_prior("ecx4param", gaussian(link = "identity"),
                                disp_dat$x, disp_dat$y, disp_spec = spec)
  expect_true(all(c("c0", "c1") %in% pr$nlpar))
  # c1 centred on zero, i.e. on constant dispersion
  expect_equal(pr$prior[pr$nlpar == "c1"], "normal(0, 2)")
  # c0 is the dispersion parameter on the log scale, so tracks the response
  expect_true(grepl("^normal\\(", pr$prior[pr$nlpar == "c0"]))
  spec2 <- list(route = "B", value = "twosided")
  pr2 <- bayesnec:::define_prior("ecx4param", brms::Beta(link = "identity"),
                                 disp_dat$x, disp_dat$prop, disp_spec = spec2)
  expect_true(all(c("c0", "c1", "c2") %in% pr2$nlpar))
})

test_that("no disp priors are added without a route B term", {
  pr <- bayesnec:::define_prior("ecx4param", gaussian(link = "identity"),
                                disp_dat$x, disp_dat$y, disp_spec = NULL)
  expect_false(any(c("c0", "c1") %in% pr$nlpar))
  pr_a <- bayesnec:::define_prior("ecx4param", gaussian(link = "identity"),
                                  disp_dat$x, disp_dat$y,
                                  disp_spec = list(route = "A", value = "x"))
  expect_false(any(c("c0", "c1") %in% pr_a$nlpar))
})

test_that("every model yields a formula brms will accept with disp", {
  skip_on_cran()
  for (m in models()$all) {
    spec <- list(route = "B", value = "power")
    db <- bayesnec:::make_disp_block(m, spec, "sigma", "x")
    comb <- try(
      get(paste0("bf_", m), envir = asNamespace("bayesnec")) +
        brms::nlf(db$nlf) + brms::lf(db$lf),
      silent = TRUE
    )
    expect_false(inherits(comb, "try-error"),
                 label = paste("disp block for", m))
  }
})

test_that("loglinear recovers a slope on a response that crosses zero", {
  skip_on_cran()
  skip_on_ci()
  # the growth-rate case, scaled on c_proliferum x A: the curve runs from about
  # 0.12 at the control to -0.5 where the population collapses, and sigma rises
  # from 0.005 to 0.11 over that range, which is c1 = -5 and about a 20-fold
  # spread -- the ratio those tests actually show
  set.seed(202)
  x <- rep(c(0.1, 0.3, 1, 2, 5, 10, 20, 40), each = 15)
  mu <- -0.5 + (0.12 - -0.5) / (1 + exp((log(5) - log(x)) * exp(0.4)) ^ -1)
  sim <- data.frame(x = x, y = rnorm(length(mu), mu, exp(-4.7 - 5 * mu)))
  # the log forms are unusable here, which is the point of this form
  expect_error(
    bnec(y ~ crf(x, "ecx4param") + disp("power"), data = sim,
         family = gaussian(link = "identity")),
    "crosses zero"
  )
  fit <- bnec(y ~ crf(x, "ecx4param") + disp("loglinear"), data = sim,
              family = gaussian(link = "identity"), chains = 2, iter = 4000,
              warmup = 2000, seed = 202, control = list(adapt_delta = 0.95))
  drws <- as.data.frame(pull_brmsfit(fit))
  c1 <- drws[[grep("c1", names(drws), value = TRUE)[1]]]
  # dispersion falls as the growth rate rises, and is resolved as doing so
  expect_true(quantile(c1, 0.975) < 0)
})

test_that("a variance function recovers a known exponent", {
  skip_on_cran()
  skip_on_ci()
  # truth: sigma = exp(-3) * mu^0.8 on a declining ecx4param curve
  set.seed(101)
  x <- rep(c(0.1, 0.3, 1, 3, 10, 30), each = 20)
  mu <- 0.1 + (1.2 - 0.1) / (1 + exp((log(3) - log(x)) * exp(0.3)))
  mu <- rev(mu)
  sim <- data.frame(x = x, y = rnorm(length(mu), mu, exp(-3) * mu^0.8))
  fit <- bnec(y ~ crf(x, "ecx4param") + disp("power"), data = sim,
              family = gaussian(link = "identity"), chains = 2, iter = 4000,
              warmup = 2000, seed = 101, control = list(adapt_delta = 0.95))
  drws <- as.data.frame(pull_brmsfit(fit))
  c1 <- drws[[grep("c1", names(drws), value = TRUE)[1]]]
  expect_true(quantile(c1, 0.025) < 0.8 && quantile(c1, 0.975) > 0.8)
  # and it should be distinguishable from constant dispersion
  expect_true(quantile(c1, 0.025) > 0)
})
