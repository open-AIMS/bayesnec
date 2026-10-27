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

test_that("disp_inits starts every slope at the middle of its own prior", {
  y <- c(0.5, 1, 2, 4)
  ii <- bayesnec:::disp_inits(list(route = "B", value = "power"), gaussian(), y)
  expect_named(ii, c("b_c0", "b_c1"))
  # Every chain starts at one value: the sign of a slope is tied to the
  # direction of the mean curve, so chains must not each pick their own. A
  # gaussian c1 is bounded to [0, 1] (#472), so the constant-dispersion null is
  # the lower bound and Stan would refuse it; the chains start at the mean of
  # normal(0, 2) truncated to that interval instead.
  expect_equal(as.numeric(ii$b_c1),
               2 * (dnorm(0) - dnorm(0.5)) / (pnorm(0.5) - pnorm(0)))
  expect_equal(as.numeric(ii$b_c0), log(sd(y)))
  # an unbounded slope keeps the null
  ll <- bayesnec:::disp_inits(list(route = "B", value = "loglinear"),
                              gaussian(), y)
  expect_equal(as.numeric(ll$b_c1), 0)
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

test_that("hurdle_poisson refuses disp() and hurdle_negbinomial takes it (#410)", {
  f <- bnf(count ~ crf(x, "ecx4param") + disp("power"))
  expect_error(
    make_brmsformula(f, disp_dat, validate_family("hurdle_poisson")),
    "hurdle_poisson has no free dispersion parameter.*hurdle_negbinomial"
  )
  f_a <- bnf(count ~ crf(x, "ecx4param") + disp(~x))
  expect_error(
    make_brmsformula(f_a, disp_dat, validate_family("hurdle_poisson")),
    "no free dispersion parameter"
  )
  fam <- validate_family("hurdle_negbinomial")
  for (txt in c(' + disp("power")', ' + disp("loglinear")', " + disp(~x)")) {
    bf <- make_brmsformula(bnf(paste0('count ~ crf(x, "ecx4param")', txt)),
                           disp_dat, fam)[[1]]
    expect_true("shape" %in% names(bf$pforms))
  }
  # "twosided" suits the beta families only
  expect_error(
    make_brmsformula(bnf(count ~ crf(x, "ecx4param") + disp("twosided")),
                     disp_dat, fam),
    "not valid for the hurdle_negbinomial family.*zero_inflated_beta\\.$"
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


test_that("route B reads the curve the fit uses, not the template (#294)", {
  # make_disp_block() rebuilt the curve from bf_<model>, which discarded
  # everything add_formula_glef() had done to the main expression. With the
  # multiplicative deviation that meant phi was modelled as a function of the
  # population-level bot while mu used bnecbot, silently and with no message.
  # wrangle_model_formula() already adds the dispersion block last for exactly
  # this reason; the rebuild was what stopped that having any effect.
  set.seed(294)
  d <- data.frame(resp = runif(60, 0.05, 0.9),
                  pred = rep(log(c(0.1, 1, 10, 100, 1000, 1e4)), 10),
                  grp = factor(rep(1:5, each = 12)))
  fam <- validate_family("Beta")
  phi_of <- function(txt) {
    f <- bnf(paste0('resp ~ crf(pred, "nec4param") + ', txt))
    bdat <- suppressMessages(model.frame(f, d))
    bform <- suppressMessages(
      wrangle_model_formula("nec4param", f, bdat, family = fam)
    )
    deparse1(formula.tools::rhs(bform[[2]][["phi"]]))
  }
  # A transformed parameter deviation reaches the variance function.
  expect_true(grepl("bnecbot", phi_of('(bot | grp) + disp("twosided")'),
                    fixed = TRUE))
  expect_true(grepl("bnecbot", phi_of('pgl(grp) + disp("twosided")'),
                    fixed = TRUE))
  # #257's deviation on the whole curve does not. The variance function reads
  # the curve before it, bnecmu, rather than the fitted mean of the group, in
  # whose value depends on ogl (on the odds scale for this Beta family).
  # Reading the fitted mean produced a second posterior mode in the zinc fit.
  # See #468.
  under_ogl <- phi_of('ogl(grp) + disp("twosided")')
  expect_true(grepl("bnecmu", under_ogl, fixed = TRUE))
  expect_false(grepl("exp(ogl)", under_ogl, fixed = TRUE))
  expect_false(grepl("ogl", under_ogl, fixed = TRUE))
  # An ungrouped fit is unchanged: the curve it reads is the template's.
  ungrouped <- phi_of('disp("twosided")')
  expect_true(grepl("bot + (top - bot)", ungrouped, fixed = TRUE))
  expect_false(grepl("bnec", ungrouped, fixed = TRUE))
  # A term on an untransformed parameter reaches it through the nlpar, as it
  # always did, so the curve still names nec.
  expect_true(grepl("nec)", phi_of('(nec | grp) + disp("twosided")'),
                    fixed = TRUE))
})


# ---- #319, the disp() term resolves in the formula's environment -------------

test_that("a variance function named by a variable resolves", {
  build <- function() {
    vf <- "power"
    bayesnec:::parse_disp_term(bnf(y ~ crf(x, "nec3param") + disp(vf)))$value
  }
  expect_identical(build(), "power")
})

test_that("a disp sub-model reaches Stan with a locally defined function", {
  # brms resolves a distributional sub-model against the top-level
  # brmsformula, whose environment came from the bf_<model> template. Asserted
  # through make_stancode(), which generates the program without compiling or
  # sampling it.
  d <- nec_data
  d$y <- pmin(pmax(d$y, 0.01), 0.99)
  build <- function() {
    cent <- function(z) z - mean(z)
    bfs <- make_brmsformula(bnf(y ~ crf(x, "nec3param") + disp(~cent(x))),
                            d, family = validate_family("Beta"))
    brms::make_stancode(bfs[[1]], data = d,
                        family = brms::Beta(link = "identity"))
  }
  expect_match(build(), "phi", fixed = TRUE)
})


# ---- #397, the centring constant on the scale of the mean --------------------

# The constants are spliced into the variance function as "- <literal>)", which
# no curve expression contains, so this reads them back in order of appearance.
centring_literals <- function(rhs) {
  m <- regmatches(rhs, gregexpr("- -?[0-9.]+\\)", rhs))[[1]]
  as.numeric(gsub("^- |\\)$", "", m))
}

# disp_dat's curve recorded as counts out of varying trials. One row at each
# bound, so that the filters disp_centre() applies before taking a log are
# exercised on the proportion rather than on the count.
bb_dat <- disp_dat
bb_dat$n <- rep(c(20, 30, 40), 10)
bb_dat$count <- as.integer(round(bb_dat$y * bb_dat$n))
bb_dat$count[1] <- bb_dat$n[1]
bb_dat$count[30] <- 0L

bb_phi_rhs <- function(vf) {
  f <- bnf(paste0('count | trials(n) ~ crf(x, "ecx4param") + disp("', vf,
                  '")'))
  bf <- make_brmsformula(f, bb_dat, family = validate_family("beta_binomial"))
  deparse1(bf[[1]]$pforms$phi[[3]])
}

test_that("a trials fit centres its variance function on the proportion", {
  # Under the identity link the beta_binomial mean is the proportion of trials.
  # Centred on the counts, power's reference was the log of a count.
  p <- bb_dat$count / bb_dat$n
  expect_equal(centring_literals(bb_phi_rhs("power")),
               signif(median(log(p[p > 0])), 6))
})

test_that("twosided centres both terms on the proportion (#397)", {
  # log(1 - count) is undefined for every count above zero, so centred on the
  # counts the second reference fell back to 0 and c2 was left uncentred.
  p <- bb_dat$count / bb_dat$n
  refs <- centring_literals(bb_phi_rhs("twosided"))
  expect_equal(refs, c(signif(median(log(p[p > 0])), 6),
                       signif(median(log(1 - p[p < 1])), 6)))
  expect_false(refs[2] == 0)
})


# ---- #410, disp() on the positive block of a joint two-block family ----------

# nec_data with every response above x = 1.8 recorded as a zero, which is 16 of
# the 100 rows. `g` is the same response scaled for a Gamma block, so that its
# positive values are not all below one.
hurdle_dat <- nec_data[, c("x", "y")]
hurdle_dat$y[hurdle_dat$x > 1.8] <- 0
hurdle_dat$g <- hurdle_dat$y * 10

joint_bf <- function(resp, disp_txt, family, data = hurdle_dat) {
  f <- bnf(paste0(resp, ' ~ crf(x, "nec3param")', disp_txt))
  make_brmsformula(f, data, family = validate_family(family))[[1]]
}
pform_rhs <- function(bf, dpar) deparse1(bf$pforms[[dpar]][[3]])
block_rhs <- function(bf, prefix) {
  vapply(bf$pforms[grep(paste0("^", prefix), names(bf$pforms))], deparse1,
         character(1))
}

test_that("a two-block family reports its positive block's parameter", {
  expect_true(bayesnec:::has_disp_par("hurdle_gamma"))
  expect_true(bayesnec:::has_disp_par(validate_family("zero_inflated_beta")))
  expect_false(bayesnec:::has_disp_par("hurdle_poisson"))
  expect_equal(bayesnec:::disp_dpar("hurdle_gamma"), "shape")
  expect_equal(bayesnec:::disp_dpar("zero_inflated_beta"), "phi")
  expect_equal(bayesnec:::disp_dpar("hurdle_negbinomial"), "shape")
  expect_null(bayesnec:::disp_dpar("hurdle_poisson"))
})

test_that("hurdle_gamma takes disp() on shape and leaves hu alone", {
  bf0 <- joint_bf("g", "", "hurdle_gamma")
  bf_b <- joint_bf("g", ' + disp("power")', "hurdle_gamma")
  bf_a <- joint_bf("g", " + disp(~x)", "hurdle_gamma")
  expect_false("shape" %in% names(bf0$pforms))
  expect_true(all(c("shape", "c0", "c1") %in% names(bf_b$pforms)))
  expect_equal(pform_rhs(bf_a, "shape"), "x")
  # The hu block is the one built without the term, and no hu formula names
  # a dispersion parameter.
  expect_identical(block_rhs(bf_b, "hu"), block_rhs(bf0, "hu"))
  expect_identical(block_rhs(bf_a, "hu"), block_rhs(bf0, "hu"))
  expect_false(any(grepl("c0|c1|shape", block_rhs(bf_b, "hu"))))
  # The variance function is written in the positive block's curve, which is
  # brms's component mean, and not in the hu curve.
  shape_rhs <- pform_rhs(bf_b, "shape")
  expect_true(grepl(deparse1(bf0$formula[[3]]), shape_rhs, fixed = TRUE))
  expect_false(grepl("hutop", shape_rhs, fixed = TRUE))
})

test_that("brms accepts shape formulas beside the hu formula", {
  # make_stancode() generates the program without compiling or sampling it. A
  # constant shape is declared as a scalar parameter; a modelled one is a
  # vector over the observations.
  fam <- validate_family("hurdle_gamma")
  for (txt in c(' + disp("power")', " + disp(~x)")) {
    sc <- brms::make_stancode(joint_bf("g", txt, "hurdle_gamma"),
                              data = hurdle_dat, family = fam)
    expect_match(sc, "vector[N] shape", fixed = TRUE)
    hu_lines <- grep("hu\\[n\\] =", strsplit(sc, "\n")[[1]], value = TRUE)
    expect_length(hu_lines, 1)
    expect_false(grepl("c0|shape", hu_lines))
  }
})

test_that("zero_inflated_beta takes disp() on phi and leaves zi alone", {
  bf0 <- joint_bf("y", "", "zero_inflated_beta")
  fam <- validate_family("zero_inflated_beta")
  for (txt in c(' + disp("power")', ' + disp("twosided")', " + disp(~x)")) {
    bf <- joint_bf("y", txt, "zero_inflated_beta")
    expect_true("phi" %in% names(bf$pforms))
    expect_identical(block_rhs(bf, "zi"), block_rhs(bf0, "zi"))
    sc <- brms::make_stancode(bf, data = hurdle_dat, family = fam)
    expect_match(sc, "vector[N] phi", fixed = TRUE)
  }
})

test_that("a variance function is checked against the positive block", {
  # "twosided" is a form for the beta families, so zero_inflated_beta takes it
  # and hurdle_gamma, whose positive block is Gamma, does not.
  expect_error(joint_bf("g", ' + disp("twosided")', "hurdle_gamma"),
               "not valid for the hurdle_gamma family")
  # and the refusal names the two-block family that does take it
  expect_error(joint_bf("g", ' + disp("twosided")', "hurdle_gamma"),
               "applies to: beta, beta_binomial, zero_inflated_beta\\.")
  expect_silent(joint_bf("y", ' + disp("twosided")', "zero_inflated_beta"))
})

test_that("the centring constant is computed without the zeros", {
  g <- hurdle_dat$g
  pos <- g[g > 0]
  ll <- centring_literals(pform_rhs(joint_bf("g", ' + disp("loglinear")',
                                             "hurdle_gamma"), "shape"))
  expect_equal(ll, signif(median(pos), 6))
  # with the zeros the median is a different number, so the test can fail
  expect_false(isTRUE(all.equal(ll, signif(median(g), 6))))
  pw <- centring_literals(pform_rhs(joint_bf("g", ' + disp("power")',
                                             "hurdle_gamma"), "shape"))
  expect_equal(pw, signif(median(log(pos)), 6))
  # twosided's second reference is where a zero entered as log(1 - 0) = 0
  y <- hurdle_dat$y
  ts <- centring_literals(pform_rhs(joint_bf("y", ' + disp("twosided")',
                                             "zero_inflated_beta"), "phi"))
  expect_equal(ts, c(signif(median(log(y[y > 0])), 6),
                     signif(median(log(1 - y[y > 0])), 6)))
  expect_false(isTRUE(all.equal(ts[2], signif(median(log(1 - y)), 6))))
})

test_that("the joint block fits the growth component's variance function", {
  # bnec_hurdle() fits the growth component to the positive rows with the
  # positive block's family, so the two routes must build the same dispersion
  # sub-model, literal included.
  growth <- hurdle_dat[hurdle_dat$y > 0, ]
  for (txt in c(' + disp("power")', ' + disp("loglinear")')) {
    expect_identical(
      pform_rhs(joint_bf("g", txt, "hurdle_gamma"), "shape"),
      pform_rhs(joint_bf("g", txt, "Gamma", data = growth), "shape")
    )
  }
  expect_identical(
    pform_rhs(joint_bf("y", ' + disp("twosided")', "zero_inflated_beta"),
              "phi"),
    pform_rhs(joint_bf("y", ' + disp("twosided")', "beta", data = growth),
              "phi")
  )
})

test_that("a two-block family gets the positive block's disp() priors", {
  keep <- hurdle_dat$y > 0
  # The bounds are compared as well as the densities (#472).
  disp_rows <- function(pr) {
    as.data.frame(pr)[pr$nlpar %in% c("c0", "c1", "c2"),
                      c("prior", "nlpar", "lb", "ub")]
  }
  for (vf in c("power", "loglinear")) {
    spec <- list(route = "B", value = vf)
    joint <- bayesnec:::define_prior("nec3param",
                                     validate_family("hurdle_gamma"),
                                     hurdle_dat$x, hurdle_dat$g,
                                     disp_spec = spec)
    growth <- bayesnec:::define_prior("nec3param", validate_family("Gamma"),
                                      hurdle_dat$x[keep], hurdle_dat$g[keep],
                                      disp_spec = spec)
    expect_equal(disp_rows(joint), disp_rows(growth), ignore_attr = TRUE)
  }
  zib <- bayesnec:::define_prior("nec3param",
                                 validate_family("zero_inflated_beta"),
                                 hurdle_dat$x, hurdle_dat$y,
                                 disp_spec = list(route = "B",
                                                  value = "twosided"))
  expect_equal(zib$prior[zib$nlpar == "c0"], "normal(4, 3)")
  expect_true(all(c("c1", "c2") %in% zib$nlpar))
  # the chains start at the centre of those priors
  expect_equal(
    as.numeric(bayesnec:::disp_inits(list(route = "B", value = "power"),
                                     validate_family("hurdle_gamma"),
                                     hurdle_dat$g)$b_c0),
    2
  )
  expect_equal(
    as.numeric(bayesnec:::disp_inits(list(route = "B", value = "power"),
                                     "zero_inflated_beta", hurdle_dat$y)$b_c0),
    4
  )
})

test_that("get_priors refuses what bnec refuses on a two-block family", {
  # get_priors() builds no formula, so the check is made where the two-block
  # priors are built rather than left to wrangle_model_formula().
  d <- hurdle_dat
  d$n <- as.integer(round(d$y * 20))
  f <- n ~ crf(x, "nec3param") + disp("power")
  expect_error(suppressMessages(get_priors(f, data = d,
                                           family = "hurdle_poisson")),
               "hurdle_poisson has no free dispersion parameter")
  # hurdle_negbinomial takes the term but refuses a column other than the
  # predictor
  pr_nb <- suppressMessages(get_priors(f, data = d,
                                       family = "hurdle_negbinomial"))
  expect_true(all(c("c0", "c1") %in% pr_nb$nlpar))
  d$z <- factor(rep(c("a", "b"), length.out = nrow(d)))
  expect_error(
    suppressMessages(get_priors(n ~ crf(x, c("nec3param", "ecx4param")) +
                                  disp(~z), data = d,
                                family = "hurdle_negbinomial")),
    "\"z\" cannot be fitted on the two-block family hurdle_negbinomial"
  )
  pr <- suppressMessages(get_priors(g ~ crf(x, "nec3param") + disp("power"),
                                    data = hurdle_dat,
                                    family = "hurdle_gamma"))
  expect_true(all(c("c0", "c1") %in% pr$nlpar))
})


# ---- #468, the variance function reads the curve before ogl() ---------------

# The response block's curve and dispersion sub-formula of a built formula. No
# model is compiled or sampled.
disp_parts <- function(bf, dpar) {
  list(main = deparse1(bf$formula[[3]]),
       disp = deparse1(bf$pforms[[dpar]][[3]]))
}

test_that("a Gamma variance function reads bnecmu under ogl() (#468)", {
  fam <- Gamma(link = "identity")
  bf <- make_brmsformula(
    bnf(y ~ crf(x, "ecx4param") + ogl(tank) + disp("power")), disp_dat, fam
  )[[1]]
  parts <- disp_parts(bf, "shape")
  # The multiplicative form is in force, so the deviation is on the mean and
  # the assertions below are not met trivially.
  expect_identical(parts$main, "bnecmu * exp(ogl)")
  expect_true(grepl("log((bnecmu))", parts$disp, fixed = TRUE))
  expect_false(grepl("ogl", parts$disp, fixed = TRUE))
  # brms writes the shape from the intermediate, and the mean from the
  # intermediate and the deviation. make_stancode() generates the program
  # without compiling it.
  sc <- strsplit(brms::make_stancode(bf, data = disp_dat, family = fam),
                 "\n")[[1]]
  shape_line <- grep("shape[n] =", sc, fixed = TRUE, value = TRUE)
  expect_length(shape_line, 1)
  expect_match(shape_line, "log((nlp_bnecmu[n]))", fixed = TRUE)
  expect_false(grepl("ogl", shape_line, fixed = TRUE))
  expect_match(sc, "mu[n] = (nlp_bnecmu[n] * exp(nlp_ogl[n]))", fixed = TRUE,
               all = FALSE)
})

test_that("the additive ogl() form leaves the variance curve (#468)", {
  # Under the additive form the main right-hand side is ogl + <curve>, and the
  # variance function takes <curve> alone: the sub-formula is the one the same
  # formula without ogl() builds. gaussian is the family that always takes the
  # additive form; Beta takes it for nechormepwr, whose mean can exceed 1.
  cases <- list(
    list(model = "ecx4param", fam = gaussian(link = "identity"),
         dpar = "sigma"),
    list(model = "nec4param", fam = gaussian(link = "identity"),
         dpar = "sigma"),
    list(model = "nechormepwr", fam = validate_family("Beta"), dpar = "phi")
  )
  for (cs in cases) {
    build <- function(txt) {
      f <- bnf(paste0('y ~ crf(x, "', cs$model, '")', txt))
      disp_parts(make_brmsformula(f, disp_dat, cs$fam)[[1]], cs$dpar)
    }
    grouped <- build(' + ogl(tank) + disp("power")')
    ungrouped <- build(' + disp("power")')
    expect_identical(grouped$main, paste("ogl +", ungrouped$main))
    expect_true(grepl(ungrouped$main, grouped$disp, fixed = TRUE))
    expect_false(grepl("ogl", grouped$disp, fixed = TRUE))
    expect_identical(grouped$disp, ungrouped$disp)
  }
})

test_that("a malformed additive ogl() formula is refused", {
  bf <- make_brmsformula(
    bnf(y ~ crf(x, "ecx4param") + ogl(tank) + disp("power")),
    disp_dat, gaussian(link = "identity")
  )[[1]]
  bf$formula[[3]] <- str2lang("top + ogl")
  expect_error(bayesnec:::disp_curve(bf), "must start with ogl +", fixed = TRUE)
})

test_that("a formula without ogl() builds what it did before (#468)", {
  # Before #468 add_disp_block() passed the whole right-hand side of the main
  # formula as the curve. The sub-formula is rebuilt that way here, through
  # make_disp_block(), and compared with what the formula builder now gives,
  # for parameter-level terms that rewrite the curve and for none.
  fams <- list(gaussian(link = "identity"), Gamma(link = "identity"),
               validate_family("Beta"))
  terms <- c("", " + (bot | tank)", " + pgl(tank)", " + (nec | tank)")
  models <- c("nec4param", "ecx4param", "ecxll5", "nechorme4")
  for (fam in fams) {
    dpar <- bayesnec:::disp_dpar(fam)
    for (txt in terms) {
      for (m in models) {
        f <- bnf(paste0('y ~ crf(x, "', m, '")', txt, ' + disp("power")'))
        bf <- suppressMessages(make_brmsformula(f, disp_dat, fam))[[1]]
        old <- bayesnec:::make_disp_block(
          m, list(route = "B", value = "power"), dpar, "x",
          response = disp_dat$y, curve = deparse1(bf$formula[[3]])
        )
        expect_identical(deparse1(bf$pforms[[dpar]]), deparse1(old$nlf))
        expect_identical(bayesnec:::disp_curve(bf),
                         deparse1(bf$formula[[3]]))
      }
    }
  }
})

test_that("the two-block families read the curve before ogl() (#468)", {
  # The joint route, bnec() on hurdle_gamma or zero_inflated_beta, which
  # bnec_joint() on a hurdle fit also takes. The deviation is on the positive
  # block only, and the hu or zi block is the one built without it.
  d <- hurdle_dat
  d$tank <- factor(rep(1:5, length.out = nrow(d)))
  for (cs in list(list(resp = "g", fam = "hurdle_gamma", dpar = "shape",
                       vf = "power", block = "hu"),
                  list(resp = "y", fam = "zero_inflated_beta", dpar = "phi",
                       vf = "twosided", block = "zi"))) {
    vf_txt <- paste0(' + disp("', cs$vf, '")')
    grouped <- joint_bf(cs$resp, paste0(" + ogl(tank)", vf_txt), cs$fam,
                        data = d)
    ungrouped <- joint_bf(cs$resp, vf_txt, cs$fam, data = d)
    disp_rhs <- pform_rhs(grouped, cs$dpar)
    expect_true("bnecmu" %in% names(grouped$pforms))
    expect_true(grepl("log((bnecmu))", disp_rhs, fixed = TRUE))
    expect_false(grepl("ogl", disp_rhs, fixed = TRUE))
    expect_identical(block_rhs(grouped, cs$block),
                     block_rhs(ungrouped, cs$block))
  }
  # bnec_hurdle() fits its growth component with bnec() on the survivors and
  # the positive block's family, keeping the ogl() term, so the factorised
  # route builds the same sub-formula as the joint one.
  growth <- d[d$y > 0, ]
  f_txt <- ' + ogl(tank) + disp("power")'
  expect_identical(
    pform_rhs(joint_bf("g", f_txt, "hurdle_gamma", data = d), "shape"),
    pform_rhs(joint_bf("g", f_txt, "Gamma", data = growth), "shape")
  )
})

test_that("a joint refit across levels reads the curve before ogl() (#468)", {
  # bnec_joint() on a grouped fit with one equation at every level and
  # disp_by_level = FALSE, the one level-term route that takes ogl() and
  # disp() together. The level terms are applied after the dispersion block
  # and rewrite only the curve parameters, so bnecmu reads the per-level curve
  # and the variance function reads bnecmu.
  d <- disp_dat
  d$site <- factor(rep(c("a", "b"), 15))
  spec <- list(group_var = "site", levels = c("a", "b"),
               models = list(a = "ecx4param", b = "ecx4param"),
               composed = FALSE, disp = FALSE)
  build <- function(txt, fam) {
    f <- bnf(paste0('y ~ crf(x, "ecx4param")', txt))
    suppressMessages(
      wrangle_model_formula("ecx4param", f, model.frame(f, data = d), fam,
                            level_spec = spec)
    )
  }
  bf <- build(' + ogl(tank) + disp("power")', Gamma(link = "identity"))
  expect_identical(deparse1(bf$pforms$top), "top ~ 0 + site")
  expect_identical(deparse1(bf$formula[[3]]), "bnecmu * exp(ogl)")
  shape_rhs <- pform_rhs(bf, "shape")
  expect_true(grepl("log((bnecmu))", shape_rhs, fixed = TRUE))
  expect_false(grepl("ogl", shape_rhs, fixed = TRUE))
  # and the additive form, as for a single fit
  fam <- gaussian(link = "identity")
  grouped <- build(' + ogl(tank) + disp("power")', fam)
  ungrouped <- build(' + disp("power")', fam)
  expect_true(startsWith(deparse1(grouped$formula[[3]]), "ogl + "))
  expect_false(grepl("ogl", pform_rhs(grouped, "sigma"), fixed = TRUE))
  expect_identical(pform_rhs(grouped, "sigma"), pform_rhs(ungrouped, "sigma"))
})

# ---- #472, the range of c1 and its regularizing centre -----------------------

# A declining curve over six concentrations with `n` replicates each, and a
# response whose dispersion parameter is proportional to mu^c1: the Gamma
# shape, the gaussian sigma, or the negative binomial shape. The replicates at
# one concentration share one mean, so the slope disp_replicate_slope() reads
# from them estimates c1. Over 500 draws at n = 30 the estimate had a standard
# deviation of 0.12 (Gamma, c1 = 0.8), 0.06 (gaussian, c1 = 0.6) and 0.14
# (negbinomial, c1 = 0.5), which sets the tolerances below at about two of
# them. No model is compiled or sampled in this section.
slope_sim <- function(family, c1, n = 30, seed = 472) {
  set.seed(seed)
  x <- rep(c(0.1, 0.3, 1, 3, 10, 30), each = n)
  top <- if (identical(family, "negbinomial")) 100 else 10
  bot <- if (identical(family, "negbinomial")) 2 else 0.2
  mu <- bot + (top - bot) / (1 + exp((log(x) - log(3)) * 1.5))
  y <- switch(family,
    Gamma = {
      shape <- 30 * (mu / 2)^c1
      stats::rgamma(length(mu), shape = shape, rate = shape / mu)
    },
    gaussian = stats::rnorm(length(mu), mu, 0.05 * mu^c1),
    negbinomial = stats::rnbinom(length(mu), size = 2 * (mu / 20)^c1, mu = mu)
  )
  data.frame(x = x, y = y)
}

disp_fam <- function(tag) {
  switch(tag,
         Gamma = Gamma(link = "identity"),
         gaussian = gaussian(link = "identity"),
         negbinomial = validate_family("negbinomial"),
         beta = brms::Beta(link = "identity"),
         beta_binomial = validate_family("beta_binomial"))
}

c1_row <- function(pr) {
  as.data.frame(pr)[pr$nlpar == "c1", c("prior", "lb", "ub")]
}

test_that("c1 of power is bounded by family (#472)", {
  spec <- list(route = "B", value = "power")
  ranges <- list(Gamma = c(0, 2), gaussian = c(0, 1), negbinomial = c(0, 1),
                 beta = c(NA_real_, NA_real_),
                 beta_binomial = c(NA_real_, NA_real_))
  for (tag in names(ranges)) {
    y <- switch(tag, negbinomial = disp_dat$count,
                beta = , beta_binomial = disp_dat$prop, disp_dat$y)
    for (pt in c("uninformative", "regularizing")) {
      row <- c1_row(bayesnec:::define_prior("ecx4param", disp_fam(tag),
                                            disp_dat$x, y, prior_type = pt,
                                            disp_spec = spec))
      expect_equal(as.numeric(c(row$lb, row$ub)), ranges[[tag]],
                   label = paste(tag, pt))
    }
    # the density inside the bounds is the one c1 had before them
    row <- c1_row(bayesnec:::define_prior("ecx4param", disp_fam(tag),
                                          disp_dat$x, y, disp_spec = spec))
    expect_identical(row$prior, "normal(0, 2)")
  }
  # a two-block family takes its positive block's range
  expect_equal(bayesnec:::disp_slope_bounds(spec, "hurdle_gamma"),
               c(lb = 0, ub = 2))
  expect_true(all(is.na(
    bayesnec:::disp_slope_bounds(spec, "zero_inflated_beta")
  )))
  # route A has no c1
  expect_true(all(is.na(
    bayesnec:::disp_slope_bounds(list(route = "A", value = "x"), "Gamma")
  )))
})

test_that("twosided and loglinear are unchanged by the range (#472)", {
  for (pt in c("uninformative", "regularizing")) {
    for (tag in c("beta", "beta_binomial")) {
      pr <- as.data.frame(bayesnec:::define_prior(
        "ecx4param", disp_fam(tag), disp_dat$x, disp_dat$prop,
        prior_type = pt, disp_spec = list(route = "B", value = "twosided")
      ))
      slopes <- pr[pr$nlpar %in% c("c1", "c2"), ]
      expect_identical(slopes$prior, rep("normal(0, 2)", 2))
      expect_true(all(is.na(slopes$lb) & is.na(slopes$ub)))
    }
    for (tag in c("Gamma", "gaussian", "negbinomial")) {
      y <- if (tag == "negbinomial") disp_dat$count else disp_dat$y
      row <- c1_row(bayesnec:::define_prior(
        "ecx4param", disp_fam(tag), disp_dat$x, y, prior_type = pt,
        disp_spec = list(route = "B", value = "loglinear")
      ))
      # the string define_disp_prior() wrote before #472
      expect_identical(row$prior,
                       paste0("normal(0, ", signif(2 / sd(y), 4), ")"))
      expect_true(is.na(row$lb) && is.na(row$ub))
    }
  }
})

test_that("the regularizing centre recovers a known exponent (#472)", {
  spec <- list(route = "B", value = "power")
  truth <- c(Gamma = 0.8, gaussian = 0.6, negbinomial = 0.5)
  tol <- c(Gamma = 0.25, gaussian = 0.15, negbinomial = 0.3)
  for (tag in names(truth)) {
    d <- slope_sim(tag, truth[[tag]])
    slope <- bayesnec:::disp_replicate_slope(tag, d$x, d$y)
    expect_lt(abs(slope - truth[[tag]]), tol[[tag]])
    reg <- c1_row(bayesnec:::define_prior("ecx4param", disp_fam(tag), d$x, d$y,
                                          prior_type = "regularizing",
                                          disp_spec = spec))
    expect_identical(reg$prior, paste0("normal(", signif(slope, 4), ", 2)"))
    # the default set does not read the replicates
    uni <- c1_row(bayesnec:::define_prior("ecx4param", disp_fam(tag), d$x, d$y,
                                          disp_spec = spec))
    expect_identical(uni$prior, "normal(0, 2)")
    expect_equal(as.numeric(c(reg$lb, reg$ub)), as.numeric(c(uni$lb, uni$ub)))
  }
})

test_that("the regularizing centre is clamped into the bounds (#472)", {
  spec <- list(route = "B", value = "power")
  # a Gamma shape rising as mu^3 is a variance falling as the response rises
  d <- slope_sim("Gamma", 3)
  expect_gt(bayesnec:::disp_replicate_slope("Gamma", d$x, d$y), 2)
  row <- c1_row(bayesnec:::define_prior("ecx4param", disp_fam("Gamma"), d$x,
                                        d$y, prior_type = "regularizing",
                                        disp_spec = spec))
  expect_identical(row$prior, "normal(2, 2)")
  # a gaussian sigma falling as the response rises
  d <- slope_sim("gaussian", -0.5)
  expect_lt(bayesnec:::disp_replicate_slope("gaussian", d$x, d$y), 0)
  row <- c1_row(bayesnec:::define_prior("ecx4param", disp_fam("gaussian"),
                                        d$x, d$y, prior_type = "regularizing",
                                        disp_spec = spec))
  expect_identical(row$prior, "normal(0, 2)")
})

test_that("the regularizing centre falls back without replicates (#472)", {
  spec <- list(route = "B", value = "power")
  reg_c1 <- function(fam, x, y) {
    c1_row(bayesnec:::define_prior("ecx4param", fam, x, y,
                                   prior_type = "regularizing",
                                   disp_spec = spec))$prior
  }
  # an unreplicated series
  d <- slope_sim("Gamma", 0.8, n = 1)
  d$x <- d$x * seq(1, 1.5, length.out = nrow(d))
  expect_true(is.na(bayesnec:::disp_replicate_slope("Gamma", d$x, d$y)))
  expect_identical(reg_c1(disp_fam("Gamma"), d$x, d$y), "normal(0, 2)")
  # two replicated concentrations among unreplicated ones
  d <- slope_sim("Gamma", 0.8, n = 1)
  d <- rbind(d, d[1:2, ])
  expect_true(is.na(bayesnec:::disp_replicate_slope("Gamma", d$x, d$y)))
  expect_identical(reg_c1(disp_fam("Gamma"), d$x, d$y), "normal(0, 2)")
  # a count divided by a rate() denominator is not read
  d <- slope_sim("negbinomial", 0.5)
  expect_false(is.na(bayesnec:::disp_replicate_slope("negbinomial", d$x,
                                                     d$y)))
  expect_true(is.na(bayesnec:::disp_replicate_slope("negbinomial", d$x,
                                                    d$y / 7)))
  # Dividing counts by an exposure can also leave whole numbers. The formula,
  # not the numeric appearance of the rates, determines this fallback.
  d <- slope_sim("negbinomial", 0.5)
  d$ex <- 2
  d$y <- 2 * d$y
  expect_false(is.na(bayesnec:::disp_replicate_slope("negbinomial", d$x,
                                                     d$y / d$ex)))
  row <- get_priors(y | rate(ex) ~ crf(x, "ecx4param") + disp("power"),
                    data = d, family = "negbinomial",
                    prior_type = "regularizing")
  expect_identical(c1_row(row)$prior, "normal(0, 2)")
  ii <- bayesnec:::disp_inits(spec, disp_fam("negbinomial"), d$y / d$ex,
                              d$x, prior_type = "regularizing",
                              rate_response = TRUE)
  expect_equal(as.numeric(ii$b_c1),
               bayesnec:::truncated_normal_mean(0, 2, 0, 1))
  # The growth component of bnec_hurdle() keeps only positive counts and uses
  # a zero-truncated likelihood. Its sample variance is not the ordinary
  # negative binomial variance used by the replicate statistic.
  d <- slope_sim("negbinomial", 0.5)
  d <- d[d$y > 0, ]
  expect_gt(bayesnec:::disp_replicate_slope("negbinomial", d$x, d$y), 0.1)
  tf <- bayesnec:::add_hurdle_truncation(
    bnf(y ~ crf(x, "ecx4param") + disp("power"))
  )
  row <- get_priors(tf, data = d, family = "negbinomial",
                    prior_type = "regularizing")
  expect_identical(c1_row(row)$prior, "normal(0, 2)")
  ii <- bayesnec:::disp_inits(spec, disp_fam("negbinomial"), d$y, d$x,
                              prior_type = "regularizing",
                              truncated_response = TRUE)
  expect_equal(as.numeric(ii$b_c1),
               bayesnec:::truncated_normal_mean(0, 2, 0, 1))
  # beta has no statistic and no bound, so its slope is not centred
  expect_true(is.na(bayesnec:::disp_replicate_slope("beta", disp_dat$x,
                                                    disp_dat$prop)))
  expect_identical(reg_c1(disp_fam("beta"), disp_dat$x, disp_dat$prop),
                   "normal(0, 2)")
  # nor is the slope of loglinear, which is unbounded
  d <- slope_sim("Gamma", 0.8)
  row <- c1_row(bayesnec:::define_prior(
    "ecx4param", disp_fam("Gamma"), d$x, d$y, prior_type = "regularizing",
    disp_spec = list(route = "B", value = "loglinear")
  ))
  expect_identical(row$prior, paste0("normal(0, ", signif(2 / sd(d$y), 4), ")"))
})

test_that("a two-block family centres c1 on its survivors (#472)", {
  spec <- list(route = "B", value = "power")
  d <- slope_sim("Gamma", 0.8)
  d$y[d$x == 30][1:20] <- 0
  pos <- d$y > 0
  joint <- c1_row(bayesnec:::define_prior("nec3param",
                                          validate_family("hurdle_gamma"),
                                          d$x, d$y,
                                          prior_type = "regularizing",
                                          disp_spec = spec))
  growth <- c1_row(bayesnec:::define_prior("nec3param", disp_fam("Gamma"),
                                           d$x[pos], d$y[pos],
                                           prior_type = "regularizing",
                                           disp_spec = spec))
  expect_equal(joint, growth, ignore_attr = TRUE)
  expect_identical(joint$prior, paste0(
    "normal(", signif(bayesnec:::disp_replicate_slope("Gamma", d$x[pos],
                                                      d$y[pos]), 4), ", 2)"
  ))
})

test_that("make_stancode declares the bounds on c1 (#472)", {
  f <- bnf(y ~ crf(x, "ecx4param") + disp("power"))
  decl <- c(Gamma = "vector<lower=0,upper=2>[K_c1] b_c1;",
            gaussian = "vector<lower=0,upper=1>[K_c1] b_c1;")
  for (tag in names(decl)) {
    fam <- disp_fam(tag)
    bf <- make_brmsformula(f, disp_dat, fam)[[1]]
    pr <- suppressMessages(get_priors(f, data = disp_dat, family = fam))
    sc <- brms::make_stancode(bf, data = disp_dat, family = fam, prior = pr)
    expect_true(grepl(decl[[tag]], sc, fixed = TRUE), label = tag)
    # the density is normalised over the interval, not over the real line
    ub <- if (tag == "Gamma") 2 else 1
    expect_true(grepl(paste0("normal_lcdf(", ub, " | 0, 2)"), sc, fixed = TRUE),
                label = tag)
  }
  # an unbounded slope stays unbounded
  fb <- bnf(prop ~ crf(x, "ecx4param") + disp("twosided"))
  fam <- disp_fam("beta")
  sc <- brms::make_stancode(
    make_brmsformula(fb, disp_dat, fam)[[1]], data = disp_dat, family = fam,
    prior = suppressMessages(get_priors(fb, data = disp_dat, family = fam))
  )
  expect_true(grepl("vector[K_c1] b_c1;", sc, fixed = TRUE))
})

test_that("initial values for c1 lie inside its bounds (#472)", {
  spec <- list(route = "B", value = "power")
  inside <- function(v, lb, ub) v > lb && v < ub
  for (tag in c("Gamma", "gaussian", "negbinomial")) {
    d <- slope_sim(tag, 0.5)
    bnd <- bayesnec:::disp_slope_bounds(spec, tag)
    for (pt in c("uninformative", "regularizing")) {
      ii <- bayesnec:::disp_inits(spec, disp_fam(tag), d$y, d$x,
                                  prior_type = pt)
      expect_true(inside(as.numeric(ii$b_c1), bnd[["lb"]], bnd[["ub"]]),
                  label = paste(tag, pt))
    }
  }
  # a centre clamped onto a bound still starts the chains inside it
  d <- slope_sim("Gamma", 3)
  ii <- bayesnec:::disp_inits(spec, disp_fam("Gamma"), d$y, d$x,
                              prior_type = "regularizing")
  expect_true(inside(as.numeric(ii$b_c1), 0, 2))
  # a two-block family, whose slope prior is built from the survivors
  d$y[d$x == 30] <- 0
  ii <- bayesnec:::disp_inits(spec, validate_family("hurdle_gamma"), d$y, d$x,
                              prior_type = "regularizing")
  expect_true(inside(as.numeric(ii$b_c1), 0, 2))
})

test_that("add_brm_defaults starts c1 inside the bounds in force (#472)", {
  # The curve search is mocked to return one list per chain, so that the
  # dispersion initial values are appended as they are for a fit. Nothing is
  # compiled.
  local_mocked_bindings(
    make_good_inits = function(..., chains) {
      replicate(chains, list(b_top = as.array(1)), simplify = FALSE)
    },
    .package = "bayesnec"
  )
  spec <- list(route = "B", value = "power")
  d <- slope_sim("Gamma", 0.8, n = 6)
  fam <- disp_fam("Gamma")
  run <- function(brm_args) {
    suppressMessages(suppressWarnings(bayesnec:::add_brm_defaults(
      c(list(chains = 2), brm_args), "ecx4param", fam, d$x, d$y,
      skip_check = TRUE, custom_name = NULL, prior_type = "regularizing",
      disp_spec = spec
    )))
  }
  out <- run(list())
  c1 <- c1_row(out$prior)
  expect_equal(as.numeric(c(c1$lb, c1$ub)), c(0, 2))
  starts <- vapply(out$init, function(ch) as.numeric(ch$b_c1), numeric(1))
  expect_true(all(starts > 0 & starts < 2))
  # A user prior on c1 replaces the default row whole, bounds included, and the
  # chains start inside the bounds it declares.
  defaults <- bayesnec:::define_prior("ecx4param", fam, d$x, d$y,
                                      disp_spec = spec)
  user <- defaults[defaults$nlpar != "c1", ] +
    brms::prior_string("normal(-0.5, 0.5)", nlpar = "c1", lb = -1, ub = 0)
  out <- run(list(prior = user))
  c1 <- c1_row(out$prior)
  expect_identical(c1$prior, "normal(-0.5, 0.5)")
  expect_equal(as.numeric(c(c1$lb, c1$ub)), c(-1, 0))
  starts <- vapply(out$init, function(ch) as.numeric(ch$b_c1), numeric(1))
  expect_true(all(starts > -1 & starts < 0))
  # A user prior with no bounds leaves c1 unbounded and starts it at the
  # centre of the default.
  user <- defaults[defaults$nlpar != "c1", ] +
    brms::prior_string("normal(0, 1)", nlpar = "c1")
  out <- run(list(prior = user))
  c1 <- c1_row(out$prior)
  expect_true(is.na(c1$lb) && is.na(c1$ub))
  starts <- vapply(out$init, function(ch) as.numeric(ch$b_c1), numeric(1))
  centre <- bayesnec:::disp_slope_priors(spec, fam, d$y, d$x,
                                         prior_type = "regularizing")$c1$centre
  expect_equal(starts, rep(centre, 2))
})

# #443. A disp(~z) term naming a column other than the predictor. None of the
# blocks below samples: the grid is built from a list holding the data and the
# family, which is all prediction_grid() reads, and the refusals are reached
# before anything is fitted.
disp_z_dat <- data.frame(
  x = rep(c(0.1, 1, 3, 10), 3),
  y = c(2.1, 1.9, 1.2, 0.4, 2.0, 1.7, 1.1, 0.5, 2.2, 1.8, 1.0, 0.3),
  z = factor(rep(c("b", "a", "c"), each = 4), levels = c("b", "a", "c")),
  w = c(5, 1, 2, 9, 3, 3, 4, 8, 7, 6, 2, 1)
)
disp_z_grid <- function(rhs, data = disp_z_dat, ...) {
  fake <- list(data = data, family = list(family = "Gamma"))
  f <- bnf(stats::as.formula(paste0('y ~ crf(x, "nec3param")', rhs)))
  bayesnec:::prediction_grid(fake, f, resolution = 5, ...)$newdata
}

test_that("disp_covariates names the columns other than the predictor (#443)", {
  cov <- function(txt) {
    bayesnec:::disp_covariates(bnf(stats::as.formula(
      paste0('y ~ crf(x, "nec3param")', txt)
    )))
  }
  expect_identical(cov(" + disp(~z)"), "z")
  expect_identical(cov(" + disp(~z + w)"), c("z", "w"))
  expect_identical(cov(" + disp(~x + z)"), "z")
  expect_identical(cov(" + disp(~x)"), character(0))
  expect_identical(cov(" + disp(~log(x))"), character(0))
  expect_identical(cov(' + disp("power")'), character(0))
  expect_identical(cov(""), character(0))
  # the predictor is read through a transformation in crf()
  expect_identical(bayesnec:::disp_covariates(
    bnf(y ~ crf(log(x), "nec3param") + disp(~x + z))
  ), "z")
  expect_identical(bayesnec:::disp_covariates(
    bnf(y ~ crf(log(x), "nec3param") + disp(~x))
  ), character(0))
})

test_that("the grid holds a disp(~z) factor at its first level (#443)", {
  g <- disp_z_grid(" + disp(~z)")
  expect_named(g, c("x", "z"))
  expect_s3_class(g$z, "factor")
  # every level is kept, so that brms builds the same contrast as in the fit
  expect_identical(levels(g$z), c("b", "a", "c"))
  expect_true(all(g$z == "b"))
  expect_identical(attr(g, "bnec_disp_placeholders"), "z")
})

test_that("the grid holds a numeric disp(~w) column at its median (#443)", {
  g <- disp_z_grid(" + disp(~w)")
  expect_identical(unique(g$w), stats::median(disp_z_dat$w))
  expect_identical(attr(g, "bnec_disp_placeholders"), "w")
  # several columns, each at its own fixed value, all marked
  g2 <- disp_z_grid(" + disp(~z + w)")
  expect_true(all(g2$z == "b") && all(g2$w == 3.5))
  expect_identical(attr(g2, "bnec_disp_placeholders"), c("z", "w"))
  # a character column takes the first of its sorted values, the level brms
  # codes it against
  chr <- disp_z_dat
  chr$z <- as.character(chr$z)
  expect_identical(unique(disp_z_grid(" + disp(~z)", data = chr)$z), "a")
})

test_that("a grid with no disp(~z) column is the grid it was (#443)", {
  for (rhs in c("", " + disp(~x)", " + disp(~log(x))", ' + disp("power")')) {
    g <- disp_z_grid(rhs)
    expect_named(g, "x")
    expect_null(attr(g, "bnec_disp_placeholders"))
    expect_identical(names(attributes(g)), c("names", "class", "row.names"))
  }
})

test_that("the level column of a joint refit is not a placeholder (#443)", {
  d <- disp_z_dat
  d$g <- factor(rep(c("A", "B"), 6))
  spec <- list(group_var = "g", levels = c("A", "B"))
  g <- disp_z_grid(" + disp(~z)", data = d, level_spec = spec)
  expect_equal(nrow(g), 10)
  expect_true(all(g$z == "b"))
  expect_identical(attr(g, "bnec_disp_placeholders"), "z")
  # a disp() term on the level factor itself: the grid writes every level and
  # the column is not marked, because its values are not a placeholder
  g2 <- disp_z_grid(" + disp(~g)", data = d, level_spec = spec)
  expect_identical(as.character(g2$g), rep(c("A", "B"), each = 5))
  expect_null(attr(g2, "bnec_disp_placeholders"))
})

test_that("a hurdle grid takes the growth component's disp(~z) value (#443)", {
  f <- bnf(y ~ crf(x, "nec3param") + disp(~z))
  # the survivors the growth component was fitted to, whose first level of z
  # is the one the grid takes
  growth_dat <- disp_z_dat[disp_z_dat$z != "b", ]
  growth_dat$z <- factor(growth_dat$z, levels = c("c", "a"))
  growth <- structure(list(fit = list(data = growth_dat)),
                      class = c("bayesnecfit", "bnecfit"))
  hurdle <- list(formula = f, growth = growth)
  nd <- bayesnec:::hurdle_disp_placeholders(data.frame(x = 1:3), hurdle)
  expect_true(all(nd$z == "c"))
  expect_identical(attr(nd, "bnec_disp_placeholders"), "z")
  # and from the first equation of a model set
  hurdle$growth <- structure(
    list(mod_fits = list(nec3param = list(fit = list(data = growth_dat)))),
    class = c("bayesmanecfit", "bnecfit")
  )
  nd2 <- bayesnec:::hurdle_disp_placeholders(data.frame(x = 1:3), hurdle)
  expect_identical(nd2, nd)
  # unchanged without such a term
  hurdle$formula <- bnf(y ~ crf(x, "nec3param") + disp("power"))
  expect_identical(
    bayesnec:::hurdle_disp_placeholders(data.frame(x = 1:3), hurdle),
    data.frame(x = 1:3)
  )
})

hurdle_count_dat <- function() {
  set.seed(443)
  d <- data.frame(x = rep(0:4, each = 8),
                  z = factor(rep(c("a", "b"), 20)))
  d$y <- stats::rnbinom(nrow(d), mu = 8 * exp(-0.4 * d$x), size = 3)
  d$y[d$x == 4][1:3] <- 0L
  d
}

test_that("disp(~z) is refused on a negbinomial growth component (#443)", {
  f <- bnf(y ~ crf(x, "nec3param") + disp(~z))
  expect_error(bayesnec:::check_disp_count_truncation(f),
               paste0("\"z\" cannot be fitted on a negbinomial growth",
                      ".*depends on the shape parameter.*disp\\(~x\\)"))
  # a term on the predictor alone, and a variance function, are not refused
  for (ok in list(bnf(y ~ crf(x, "nec3param") + disp(~x)),
                  bnf(y ~ crf(x, "nec3param") + disp(~log(x + 1))),
                  bnf(y ~ crf(x, "nec3param") + disp("power")))) {
    expect_null(bayesnec:::check_disp_count_truncation(ok))
  }
  d <- hurdle_count_dat()
  calls <- 0L
  local_mocked_bindings(
    bnec = function(...) {
      calls <<- calls + 1L
      stop("no component should be fitted")
    },
    .package = "bayesnec"
  )
  # before either component is fitted, and before the brms version check,
  # which would otherwise ask for an upgrade to fit a model that is refused
  expect_error(
    bnec_hurdle(y ~ crf(x, "nec3param") + disp(~z), data = d,
                family_growth = negbinomial()),
    "cannot be fitted on a negbinomial growth component"
  )
  expect_equal(calls, 0L)
  # a Gamma growth component takes the term and reaches its fit
  d$y <- ifelse(d$y > 0, d$y + 0.5, 0)
  expect_error(
    suppressMessages(bnec_hurdle(y ~ crf(x, "nec3param") + disp(~z),
                                 data = d, family_growth = Gamma())),
    "no component should be fitted"
  )
  expect_equal(calls, 1L)
})

test_that("bnec() and the formula builder refuse it as backstops (#443)", {
  d <- hurdle_count_dat()
  d <- d[d$y > 0, ]
  tf <- bayesnec:::add_hurdle_truncation(
    bnf(y ~ crf(x, "nec3param") + disp(~z))
  )
  fam <- validate_family("negbinomial")
  expect_error(
    bayesnec:::wrangle_model_formula("nec3param", tf,
                                     model.frame(tf, data = d), family = fam),
    "cannot be fitted on a negbinomial growth component"
  )
  fitted <- 0L
  local_mocked_bindings(
    fit_bayesnec = function(...) {
      fitted <<- fitted + 1L
      stop("nothing should be fitted")
    },
    .package = "bayesnec"
  )
  expect_error(
    suppressMessages(bnec(tf, data = d, family = fam)),
    "cannot be fitted on a negbinomial growth component"
  )
  expect_equal(fitted, 0L)
  # the untruncated negbinomial of an ordinary bnec() fit has a mean free of
  # the shape, so the term is not refused there
  expect_error(
    suppressMessages(bnec(y ~ crf(x, "nec3param") + disp(~z), data = d,
                          family = fam)),
    "nothing should be fitted"
  )
})

# ---- a Gamma family read off a fit -----------------------------------------
# brms records Gamma as gamma. No model is compiled or sampled.

test_that("the disp() readers take a stored Gamma family as Gamma (#436)", {
  stored <- brms::bf(y ~ x, family = Gamma(link = "identity"))$family
  expect_identical(stored$family, "gamma")
  power <- list(route = "B", value = "power")
  expect_identical(bayesnec:::disp_family_tag(stored), "Gamma")
  expect_true(bayesnec:::has_disp_par(stored))
  expect_identical(bayesnec:::disp_dpar(stored), "shape")
  expect_identical(bayesnec:::disp_slope_bounds(power, stored),
                   c(lb = 0, ub = 2))
  expect_identical(bayesnec:::disp_intercept_centre(stored, disp_dat$y), 2)
  expect_null(bayesnec:::check_disp_spec(power, stored, response = disp_dat$y))
  expect_identical(
    bayesnec:::define_disp_prior(power, stored, disp_dat$y, disp_dat$x),
    bayesnec:::define_disp_prior(power, Gamma(link = "identity"), disp_dat$y,
                                 disp_dat$x)
  )
  # A refusal names the family as written, not the stored tag.
  expect_error(
    bayesnec:::check_disp_spec(list(route = "B", value = "twosided"), stored),
    "Variance function \"twosided\" is not valid for the Gamma family",
    fixed = TRUE
  )
  # The formula amend() builds for an added equation is the one bnec() builds.
  f <- bnf(y ~ crf(x, "ecx4param") + disp("power"))
  built <- make_brmsformula(f, disp_dat, stored)
  expected <- make_brmsformula(f, disp_dat, Gamma(link = "identity"))
  expect_identical(deparse1(built$ecx4param$pforms$shape),
                   deparse1(expected$ecx4param$pforms$shape))
})

test_that("every family bayesnec fits reads as before through the tag (#436)", {
  # Reading through canonical_fam_tag() changes nothing for a validated
  # family, or for a stored one other than Gamma.
  for (tag in names(mod_fams)) {
    validated <- validate_family(tag)
    stored <- brms::bf(y ~ x, family = unmark_family(validated))$family
    for (f in list(validated, stored)) {
      expect_identical(bayesnec:::disp_family_tag(f),
                       if (is_hurdle_family(tag)) {
                         unname(hurdle_mu_fams[[tag]])
                       } else {
                         tag
                       }, label = tag)
      expect_identical(bayesnec:::has_disp_par(f),
                       bayesnec:::disp_family_tag(tag) %in% names(disp_dpars),
                       label = tag)
    }
  }
})

# ---- disp() on the joint hurdle_negbinomial ----------------------------------
# Only the last test compiles a model.

nb_hurdle_dat <- function() {
  d <- slope_sim("negbinomial", 0.5)
  d$y[d$x == max(d$x)][1:5] <- 0L
  d$z <- factor(rep(c("a", "b"), length.out = nrow(d)))
  d
}

# A stand-in for a joint hurdle_negbinomial fit. Tests mock as_draws_df() and
# ndraws() to supply its draws.
nb_disp_object <- function(disp_txt, family = "hurdle_negbinomial",
                           d = nb_hurdle_dat()) {
  f <- bnf(paste0('y ~ crf(x, "nec4param")', disp_txt))
  fam <- if (is.character(family)) validate_family(family) else family
  bf <- make_brmsformula(f, d, family = fam)[[1]]
  structure(list(model = "nec4param",
                 fit = list(family = fam, formula = bf),
                 bayesnecformula = f),
            class = c("bayesnecfit", "bnecfit"))
}
nb_draws <- data.frame(b_c0_Intercept = c(1.2, 2.5, 0.4),
                       b_c1_Intercept = c(0.3, 0.75, 0.05))

test_that("shape_in_reported_mean names the two truncated routes (#410)", {
  expect_true(bayesnec:::shape_in_reported_mean(NULL, "hurdle_negbinomial"))
  expect_true(bayesnec:::shape_in_reported_mean(
    NULL, validate_family("hurdle_negbinomial")
  ))
  tf <- bayesnec:::add_hurdle_truncation(bnf(y ~ crf(x, "nec3param")))
  expect_true(bayesnec:::shape_in_reported_mean(
    tf, validate_family("negbinomial")
  ))
  # an untruncated negbinomial, and the two-block families whose mean is
  # mu * (1 - hu), report a mean free of the dispersion parameter
  expect_false(bayesnec:::shape_in_reported_mean(
    bnf(y ~ crf(x, "nec3param")), validate_family("negbinomial")
  ))
  # a truncated poisson growth component has no shape
  expect_false(bayesnec:::shape_in_reported_mean(
    tf, validate_family("poisson")
  ))
  for (tag in c("hurdle_gamma", "zero_inflated_beta", "hurdle_poisson")) {
    expect_false(bayesnec:::shape_in_reported_mean(NULL, tag), label = tag)
  }
})

test_that("disp(~z) is refused on the joint hurdle_negbinomial (#410)", {
  f <- bnf(y ~ crf(x, "nec3param") + disp(~z))
  expect_error(
    bayesnec:::check_disp_count_truncation(f, "hurdle_negbinomial"),
    paste0("\"z\" cannot be fitted on the two-block family hurdle_negbinomial",
           ".*1 - hu, so both depend on the shape parameter.*disp\\(~x\\)")
  )
  # the growth component keeps its own message
  expect_error(bayesnec:::check_disp_count_truncation(f),
               "\"z\" cannot be fitted on a negbinomial growth component")
  d <- nb_hurdle_dat()
  fam <- validate_family("hurdle_negbinomial")
  # the formula builder is the backstop
  expect_error(make_brmsformula(f, d, family = fam),
               "cannot be fitted on the two-block family hurdle_negbinomial")
  # bnec() refuses once, before its model loop
  fitted <- 0L
  local_mocked_bindings(
    fit_bayesnec = function(...) {
      fitted <<- fitted + 1L
      stop("nothing should be fitted")
    },
    .package = "bayesnec"
  )
  expect_error(
    suppressMessages(bnec(y ~ crf(x, c("nec3param", "ecx4param")) + disp(~z),
                          data = d, family = fam)),
    "cannot be fitted on the two-block family hurdle_negbinomial"
  )
  expect_equal(fitted, 0L)
  # a term on the predictor alone, and a variance function, are fitted
  for (txt in c(" + disp(~x)", " + disp(~log(x + 1))", ' + disp("power")')) {
    expect_error(
      suppressMessages(bnec(bnf(paste0('y ~ crf(x, "nec3param")', txt)),
                            data = d, family = fam)),
      "nothing should be fitted", label = txt
    )
  }
  # hurdle_gamma's curve does not involve its shape, so z is accepted there
  d$g <- ifelse(d$y > 0, d$y + 0.5, 0)
  expect_silent(make_brmsformula(bnf(g ~ crf(x, "nec3param") + disp(~z)), d,
                                 family = validate_family("hurdle_gamma")))
})

test_that("disp_at_mean evaluates the sub-model at a stated mean (#410)", {
  local_mocked_bindings(
    as_draws_df = function(x, variable = NULL, ...) nb_draws[, variable],
    .package = "bayesnec"
  )
  mu <- c(3, 8.5, 0.7)
  for (vf in c("power", "loglinear")) {
    obj <- nb_disp_object(paste0(' + disp("', vf, '")'))
    ref <- centring_literals(pform_rhs(obj$fit$formula, "shape"))
    eta <- switch(vf,
      power = nb_draws$b_c0_Intercept +
        nb_draws$b_c1_Intercept * (log(mu) - ref),
      loglinear = nb_draws$b_c0_Intercept +
        nb_draws$b_c1_Intercept * (mu - ref)
    )
    got <- bayesnec:::disp_at_mean(obj$fit, list(route = "B", value = vf), mu)
    expect_equal(got$value, exp(eta), label = vf)
    expect_equal(unname(got$centre), ref, label = vf)
    expect_identical(got$link, "log")
  }
  # a negative centring literal, written "- -0.5", is read as -0.5
  d <- nb_hurdle_dat()
  d$y <- d$y / 100
  obj <- nb_disp_object(' + disp("power")', d = d)
  ref <- centring_literals(pform_rhs(obj$fit$formula, "shape"))
  expect_lt(ref, 0)
  got <- bayesnec:::disp_at_mean(obj$fit, list(route = "B", value = "power"),
                                 mu)
  expect_equal(unname(got$centre), ref)
  # the dispersion link is the family's own
  fam <- validate_family(brms::hurdle_negbinomial(link = "identity",
                                                  link_shape = "softplus",
                                                  link_hu = "identity"))
  obj <- nb_disp_object(' + disp("power")', family = fam)
  ref <- centring_literals(pform_rhs(obj$fit$formula, "shape"))
  got <- bayesnec:::disp_at_mean(obj$fit, list(route = "B", value = "power"),
                                 mu)
  eta <- nb_draws$b_c0_Intercept + nb_draws$b_c1_Intercept * (log(mu) - ref)
  expect_equal(got$value, log1p(exp(eta)))
  # a formula of another form is refused rather than misread
  obj <- nb_disp_object(' + disp("loglinear")')
  expect_error(
    bayesnec:::disp_at_mean(obj$fit, list(route = "B", value = "power"), mu),
    "does not have the form of disp\\(\"power\"\\)"
  )
})

test_that("disp_at_mean reads the formula a brmsfit stores (#410)", {
  # brm(empty = TRUE) validates the formula as a fit does, without compiling.
  local_mocked_bindings(
    as_draws_df = function(x, variable = NULL, ...) nb_draws[, variable],
    .package = "bayesnec"
  )
  obj <- nb_disp_object(' + disp("power")')
  d <- nb_hurdle_dat()
  empty <- suppressMessages(brms::brm(obj$fit$formula, data = d,
                                      family = obj$fit$family, empty = TRUE))
  mu <- c(3, 8.5, 0.7)
  spec <- list(route = "B", value = "power")
  expect_equal(bayesnec:::disp_at_mean(empty, spec, mu),
               bayesnec:::disp_at_mean(obj$fit, spec, mu))
})

test_that("the asymptote shape under disp() is the sub-model at bot (#410)", {
  bot <- c(4, 2.5, 6)
  local_mocked_bindings(
    as_draws_df = function(x, variable = NULL, ...) nb_draws[, variable],
    ndraws = function(x) nrow(nb_draws),
    posterior_epred = function(...) stop("the control is not read"),
    .package = "bayesnec"
  )
  obj <- nb_disp_object(' + disp("power")')
  ref <- centring_literals(pform_rhs(obj$fit$formula, "shape"))
  shape_bot <- exp(nb_draws$b_c0_Intercept +
                     nb_draws$b_c1_Intercept * (log(bot) - ref))
  expected <- as.numeric(bayesnec:::hurdle_positive_mean(
    bot, "hurdle_negbinomial", shape_bot
  ))
  got <- bayesnec:::count_positive_asymptote(obj, "mu", bot,
                                             data.frame(x = 0))
  expect_equal(got, expected)
  # read at a larger mean, the control's, the shape is larger and the
  # positive-count asymptote lower, which is what the control reading gave
  shape_ctrl <- exp(nb_draws$b_c0_Intercept +
                      nb_draws$b_c1_Intercept * (log(10 * bot) - ref))
  expect_true(all(got > as.numeric(bayesnec:::hurdle_positive_mean(
    bot, "hurdle_negbinomial", shape_ctrl
  ))))
  # the factorised growth component reads it the same way
  tf_obj <- obj
  tf_obj$fit$family <- validate_family("negbinomial")
  tf_obj$bayesnecformula <- bayesnec:::add_hurdle_truncation(
    obj$bayesnecformula
  )
  tf_obj$fit$formula <- make_brmsformula(
    tf_obj$bayesnecformula, nb_hurdle_dat()[nb_hurdle_dat()$y > 0, ],
    family = tf_obj$fit$family
  )[[1]]
  ref_g <- centring_literals(pform_rhs(tf_obj$fit$formula, "shape"))
  expect_equal(ref_g, ref)
  expect_equal(
    bayesnec:::count_positive_asymptote(tf_obj, NULL, bot, data.frame(x = 0)),
    expected
  )
  # the combined endpoint and the hu block read no shape, and are unchanged
  expect_identical(
    bayesnec:::count_positive_asymptote(obj, NULL, bot, data.frame(x = 0)),
    bot
  )
  expect_identical(
    bayesnec:::count_positive_asymptote(obj, "hu", bot, data.frame(x = 0)),
    bot
  )
})

test_that("an equation without bot takes the limit at a mean of zero (#410)", {
  draws <- data.frame(b_c0_Intercept = c(1.2, 2.5, 0.4, 1),
                      b_c1_Intercept = c(0.3, 0.999, 1, 1.4))
  local_mocked_bindings(
    as_draws_df = function(x, variable = NULL, ...) draws[, variable],
    ndraws = function(x) nrow(draws),
    .package = "bayesnec"
  )
  obj <- nb_disp_object(' + disp("power")')
  ref <- centring_literals(pform_rhs(obj$fit$formula, "shape"))
  got <- bayesnec:::count_positive_asymptote(obj, "mu", 0, data.frame(x = 0))
  # mu / shape tends to 0 for c1 < 1, to exp(LOGREF - c0) at c1 = 1, and
  # without bound above it
  r <- exp(ref - draws$b_c0_Intercept[3])
  expect_equal(got, c(1, 1, r / log1p(r), Inf))
  # the shape of "loglinear" is finite at a mean of zero
  obj <- nb_disp_object(' + disp("loglinear")')
  expect_equal(
    bayesnec:::count_positive_asymptote(obj, "mu", 0, data.frame(x = 0)),
    rep(1, 4)
  )
})

test_that("a relative estimate under disp(~x) is refused (#410)", {
  local_mocked_bindings(
    posterior_epred = function(...) stop("the shape is not read"),
    .package = "bayesnec"
  )
  obj <- nb_disp_object(" + disp(~x)")
  expect_error(
    bayesnec:::count_positive_asymptote(obj, "mu", c(4, 2.5),
                                        data.frame(x = 0)),
    paste0("type = \"relative\" measures towards the asymptote.*disp\\(~x\\)",
           ".*has no single shape.*type = \"absolute\".*disp\\(\"power\"\\)")
  )
  # the estimators that read no asymptote reach no refusal
  expect_identical(
    bayesnec:::count_positive_asymptote(obj, "mu", NA_real_,
                                        data.frame(x = 0)),
    NA_real_
  )
  expect_identical(
    bayesnec:::count_positive_asymptote(obj, NULL, c(4, 2.5),
                                        data.frame(x = 0)),
    c(4, 2.5)
  )
})

test_that("disp(~1) reads its one shape at the control (#410)", {
  shape <- matrix(c(2, 5), ncol = 1)
  local_mocked_bindings(
    posterior_epred = function(...) shape,
    .package = "bayesnec"
  )
  obj <- nb_disp_object(" + disp(~1)")
  expect_equal(
    bayesnec:::count_positive_asymptote(obj, "mu", c(4, 2.5),
                                        data.frame(x = 0)),
    as.numeric(bayesnec:::hurdle_positive_mean(c(4, 2.5),
                                               "hurdle_negbinomial", shape))
  )
})

test_that("a model set reads each equation's shape at its own bot (#410)", {
  parts <- list(a = nb_disp_object(' + disp("power")'),
                b = nb_disp_object(' + disp("power")'))
  object <- structure(list(
    mod_fits = list(a = list(fit = 1), b = list(fit = 2))
  ), class = c("bayesmanecfit", "bnecfit"))
  bot <- c(4, 2.5, 6)
  draw_index <- list(a = c(1L, 3L), b = 2L)
  local_mocked_bindings(
    as_draws_df = function(x, variable = NULL, ...) {
      if (is.null(variable)) nb_draws else nb_draws[, variable]
    },
    ndraws = function(x) nrow(nb_draws),
    pull_draw_index = function(...) draw_index,
    pull_out = function(object, model) parts[[model]],
    ecx_asymptote = function(object, type) bot,
    .package = "bayesnec"
  )
  ref <- centring_literals(pform_rhs(parts$a$fit$formula, "shape"))
  each <- as.numeric(bayesnec:::hurdle_positive_mean(
    bot, "hurdle_negbinomial",
    exp(nb_draws$b_c0_Intercept + nb_draws$b_c1_Intercept * (log(bot) - ref))
  ))
  expect_equal(
    bayesnec:::count_positive_asymptote(object, "mu", rep(0, 3),
                                        data.frame(x = 0)),
    c(each[draw_index$a], each[draw_index$b])
  )
})

test_that("a joint count hurdle centres c1 as a truncated count (#410)", {
  # The replicate centre falls back to 0, as it does for a growth component.
  spec <- list(route = "B", value = "power")
  d <- nb_hurdle_dat()
  pos <- d$y > 0
  # the replicates do show a slope, so the test can fail
  expect_gt(bayesnec:::disp_replicate_slope("negbinomial", d$x[pos], d$y[pos]),
            0.1)
  for (pt in c("uninformative", "regularizing")) {
    row <- c1_row(bayesnec:::define_prior(
      "nec4param", validate_family("hurdle_negbinomial"), d$x, d$y,
      prior_type = pt, disp_spec = spec
    ))
    expect_identical(row$prior, "normal(0, 2)", label = pt)
    expect_equal(as.numeric(c(row$lb, row$ub)), c(0, 1), label = pt)
  }
  pr <- suppressMessages(get_priors(y ~ crf(x, "nec4param") + disp("power"),
                                    data = d, family = "hurdle_negbinomial",
                                    prior_type = "regularizing"))
  expect_identical(c1_row(pr)$prior, "normal(0, 2)")
  expect_equal(as.numeric(c(c1_row(pr)$lb, c1_row(pr)$ub)), c(0, 1))
  ii <- bayesnec:::disp_inits(spec, validate_family("hurdle_negbinomial"),
                              d$y, d$x, prior_type = "regularizing")
  expect_equal(as.numeric(ii$b_c1),
               bayesnec:::truncated_normal_mean(0, 2, 0, 1))
  expect_equal(as.numeric(ii$b_c0), 2)
})

test_that("a joint hurdle_negbinomial fit reads its shape at bot (#410)", {
  skip_on_cran()
  skip_on_ci()
  d <- nb_hurdle_dat()
  fit <- suppressWarnings(suppressMessages(
    bnec(y ~ crf(x, "nec4param") + disp("power"), data = d,
         family = "hurdle_negbinomial", chains = 1, iter = 600, warmup = 300,
         seed = 410, refresh = 0)
  ))
  drws <- brms::as_draws_df(fit$fit)
  ref <- centring_literals(pform_rhs(fit$fit$formula, "shape"))
  bot <- drws$b_bot_Intercept
  shape_bot <- exp(drws$b_c0_Intercept +
                     drws$b_c1_Intercept * (log(bot) - ref))
  expect_equal(
    bayesnec:::count_positive_asymptote(fit, "mu", bot, data.frame(x = 0)),
    as.numeric(bayesnec:::hurdle_positive_mean(bot, "hurdle_negbinomial",
                                               shape_bot))
  )
  est <- suppressWarnings(ecx(fit, type = "relative", dpar = "mu",
                              ecx_val = 50))
  expect_true(all(is.finite(est)))
})
