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

test_that("disp is still rejected for the two count hurdles (#410)", {
  # hurdle_gamma and zero_inflated_beta now take the term; see the #410 section
  # at the end of this file. hurdle_poisson has no dispersion parameter, and
  # hurdle_negbinomial waits on how the shape is read at the asymptote.
  f <- bnf(count ~ crf(x, "ecx4param") + disp("power"))
  expect_error(
    make_brmsformula(f, disp_dat, validate_family("hurdle_poisson")),
    "hurdle_poisson has no free dispersion parameter"
  )
  expect_error(
    make_brmsformula(f, disp_dat, validate_family("hurdle_negbinomial")),
    "not yet supported .* hurdle_negbinomial.*pending a decision"
  )
  # route A as well: the refusal is of the term, not of a variance function
  f_a <- bnf(count ~ crf(x, "ecx4param") + disp(~x))
  expect_error(
    make_brmsformula(f_a, disp_dat, validate_family("hurdle_negbinomial")),
    "pending a decision"
  )
  expect_error(
    make_brmsformula(f_a, disp_dat, validate_family("hurdle_poisson")),
    "no free dispersion parameter"
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
  # So does #257's deviation on the whole curve, which route B had never seen.
  expect_true(grepl("bnecmu", phi_of('ogl(grp) + disp("twosided")'),
                    fixed = TRUE))
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
  disp_rows <- function(pr) {
    as.data.frame(pr)[pr$nlpar %in% c("c0", "c1", "c2"),
                      c("prior", "nlpar")]
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
