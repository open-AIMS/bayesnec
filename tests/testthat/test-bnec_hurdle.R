test_that("hurdle_response_var accepts a plain response", {
  expect_equal(
    bayesnec:::hurdle_response_var(bnf(y ~ crf(x, "nec3param"))), "y"
  )
})

test_that("hurdle_response_var rejects a transformed response", {
  # A transformed response makes the zero-as-death convention ambiguous:
  # log(0) is not a death, it is -Inf.
  expect_error(
    bayesnec:::hurdle_response_var(bnf(log(y) ~ crf(x, "nec3param"))),
    "plain, untransformed response"
  )
  expect_error(
    bayesnec:::hurdle_response_var(bnf(sqrt(y) ~ crf(x, "nec3param"))),
    "plain, untransformed response"
  )
})

test_that("hurdle_response_var reads the response past an aterm", {
  # What bnec_hurdle needs is a bare *response*, not a bare left-hand side: an
  # aterm alongside it does not make the zero-as-death convention ambiguous.
  expect_equal(
    bayesnec:::hurdle_response_var(bnf(y | cens(cens) ~ crf(x, "nec3param"))),
    "y"
  )
  expect_equal(
    bayesnec:::hurdle_response_var(
      bnf(y | cens(cens, ub) ~ crf(x, "nec3param"))
    ),
    "y"
  )
  expect_equal(
    bayesnec:::hurdle_response_var(
      bnf(y | trials(n) + cens(cens) ~ crf(x, "nec3param"))
    ),
    "y"
  )
  # A transformed response is still rejected, and the message names the
  # response rather than the whole left-hand side.
  expect_error(
    bayesnec:::hurdle_response_var(bnf(log(y) | cens(cens) ~ crf(x, "n"))),
    "You supplied \"log\\(y\\)\""
  )
})

test_that("check_hurdle_aterms accepts cens and rejects everything else", {
  expect_length(
    bayesnec:::check_hurdle_aterms(bnf(y ~ crf(x, "nec3param"))), 0
  )
  expect_named(
    bayesnec:::check_hurdle_aterms(bnf(y | cens(cens) ~ crf(x, "nec3param"))),
    "cens"
  )
  expect_named(
    bayesnec:::check_hurdle_aterms(
      bnf(y | cens(cens, ub) ~ crf(x, "nec3param"))
    ),
    "cens"
  )
  # Each rejected aterm is named, with the reason it cannot apply.
  expect_error(
    bayesnec:::check_hurdle_aterms(bnf(y | weights(w) ~ crf(x, "nec3param"))),
    "\"weights\\(\\)\".*modelling decision"
  )
  expect_error(
    bayesnec:::check_hurdle_aterms(bnf(y | trials(n) ~ crf(x, "nec3param"))),
    "\"trials\\(\\)\".*Bernoulli trial per individual"
  )
  # An unrecognised aterm still gets named rather than falling through.
  expect_error(
    bayesnec:::check_hurdle_aterms(bnf(y | se(sigma) ~ crf(x, "nec3param"))),
    "\"se\\(\\)\""
  ) |>
    suppressMessages()
  # Rejected alongside an accepted one.
  expect_error(
    bayesnec:::check_hurdle_aterms(
      bnf(y | cens(cens) + weights(w) ~ crf(x, "nec3param"))
    ),
    "\"weights\\(\\)\""
  )
  # A namespace-qualified aterm is the same aterm. The rest of the package
  # matches on the bare name, so accepting brms::cens() keeps bnec_hurdle in
  # step with what bnec() already takes.
  expect_named(
    bayesnec:::check_hurdle_aterms(
      bnf(y | brms::cens(cens) ~ crf(x, "nec3param"))
    ),
    "cens"
  )
  expect_error(
    bayesnec:::check_hurdle_aterms(
      bnf(y | brms::weights(w) ~ crf(x, "nec3param"))
    ),
    "\"weights\\(\\)\".*modelling decision"
  )
})

test_that("check_hurdle_cens separates structural from censored zeros", {
  dat <- data.frame(x = rep(1:4, each = 5), y = c(rep(2, 15), rep(0, 5)))
  dat$cens <- "none"
  aterms <- bayesnec:::check_hurdle_aterms(
    bnf(y | cens(cens) ~ crf(x, "nec3param"))
  )
  # A left-censored survivor is fine: it is an observation of the growth
  # component, known only to lie at or below a bound.
  ok <- dat
  ok$y[1:2] <- 0.05
  ok$cens[1:2] <- "left"
  expect_null(bayesnec:::check_hurdle_cens(aterms, ok, ok$y, "y"))
  # A row that is both zero and censored is refused: it claims to be both kinds
  # of zero at once, which is what the hurdle exists to tell apart.
  bad <- dat
  bad$cens[bad$y == 0][1:2] <- "left"
  expect_error(
    bayesnec:::check_hurdle_cens(aterms, bad, bad$y, "y"),
    "zero and also carry a censoring code"
  )
  # A recycled literal declares every row censored, zeros included.
  lit <- bayesnec:::check_hurdle_aterms(
    bnf(y | cens("left") ~ crf(x, "nec3param"))
  )
  expect_error(
    bayesnec:::check_hurdle_cens(lit, dat, dat$y, "y"),
    "zero and also carry a censoring code"
  )
  # No cens() term at all is a no-op.
  expect_null(
    bayesnec:::check_hurdle_cens(list(), dat, dat$y, "y")
  )
  # An empty cens() has no indicator to check against, and should say so
  # rather than fall over indexing an empty argument list.
  empty <- bayesnec:::check_hurdle_aterms(
    bnf(y | cens() ~ crf(x, "nec3param"))
  )
  expect_error(
    bayesnec:::check_hurdle_cens(empty, dat, dat$y, "y"),
    "cens\\(\\) was supplied with no arguments"
  )
})

test_that("bnec_hurdle rejects a censored structural zero", {
  dat <- data.frame(x = rep(1:4, each = 5), y = c(rep(2, 15), rep(0, 5)),
                    cens = "none")
  dat$cens[dat$y == 0][1] <- "left"
  expect_error(
    bnec_hurdle(y | cens(cens) ~ crf(x, "nec3param"), data = dat),
    "zero and also carry a censoring code"
  )
  expect_error(
    bnec_hurdle(y | weights(x) ~ crf(x, "nec3param"), data = dat),
    "only the cens\\(\\) aterm"
  )
})

test_that("bnec_hurdle validates the full predictor before either fit (#317)", {
  dat <- data.frame(x = c(1, 2, -1, -2), y = c(2, 1, 0, 0))
  calls <- 0L
  local_mocked_bindings(
    bnec = function(...) {
      calls <<- calls + 1L
      stop("component fit should not start")
    },
    .package = "bayesnec"
  )

  expect_error(
    bnec_hurdle(y ~ crf(x, "nec3param"), data = dat,
                predictor_scale = "concentration"),
    "requires a non-negative predictor"
  )
  expect_equal(calls, 0L)
})

test_that("bnec_hurdle fits a censored response and routes it correctly", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  set.seed(17)
  dat <- nec_data
  dat$y[dat$x > 2.5] <- 0
  dat$cens <- "none"
  # Left-censored *survivors*: real observations of the growth component, known
  # only to lie at or below the recording limit. They must not be routed to the
  # hurdle block.
  lod <- unname(quantile(dat$y[dat$y > 0], 0.15))
  is_lo <- dat$y > 0 & dat$y < lod
  dat$y[is_lo] <- lod
  dat$cens[is_lo] <- "left"
  expect_gt(sum(is_lo), 0)
  fit <- bnec_hurdle(y | cens(cens) ~ crf(x, "nec3param"), data = dat,
                     iter = 400, warmup = 200, chains = 2, seed = 17,
                     refresh = 0) |>
    suppressMessages() |>
    suppressWarnings()
  expect_s3_class(fit, "bayesnechurdlefit")
  # Every survivor, censored or not, is in the growth component.
  expect_equal(nrow(fit$growth$fit$data), sum(dat$y > 0))
  expect_equal(sum(fit$survival$fit$data$.alive), sum(dat$y > 0))
  # The declaration reached the growth block and only the growth block.
  expect_true("cens" %in% names(fit$growth$fit$data))
  expect_false(is.null(standata(fit$growth$fit)$cens))
  expect_true(is.null(standata(fit$survival$fit)$cens))
})

test_that("swap_response drops an aterm from the survival component", {
  # The Bernoulli response is alive/dead, observed exactly, so there is nothing
  # for a censoring declaration to bound. Incidental in the original
  # implementation; asserted here so it stays deliberate.
  out <- bayesnec:::swap_response(
    bnf(y | cens(cens) ~ crf(x, "nec3param")), ".alive"
  )
  expect_false(grepl("cens(", deparse1(out), fixed = TRUE))
  expect_equal(bayesnec:::hurdle_response_var(out), ".alive")
})

test_that("swap_response replaces the lhs and keeps the rhs", {
  out <- bayesnec:::swap_response(bnf(y ~ crf(x, "nec3param")), ".alive")
  expect_s3_class(out, "bayesnecformula")
  expect_equal(bayesnec:::hurdle_response_var(out), ".alive")
  expect_equal(bayesnec:::get_model_from_formula(out), "nec3param")
})

test_that("swap_response preserves group-level terms", {
  out <- bayesnec:::swap_response(
    bnf(y ~ crf(x, "nec3param") + pgl(tank)), ".alive"
  )
  expect_true(grepl("pgl(tank)", deparse1(out), fixed = TRUE))
})

# #410. A disp() term models the growth component's dispersion and is removed
# from the survival component, which is bernoulli and has no dispersion
# parameter. Passed to both, the survival fit refused it after the growth
# component had been sampled.

test_that("drop_disp_term removes the disp() term and keeps the rest (#410)", {
  f <- bnf(y ~ crf(x, "nec3param") + (nec | tank) + disp(~x) + pgl(tank))
  out <- bayesnec:::drop_disp_term(f)
  expect_s3_class(out, "bayesnecformula")
  expect_null(bayesnec:::parse_disp_term(out))
  # The other terms are kept in the order they were written.
  expect_equal(deparse1(out),
               "y ~ crf(x, \"nec3param\") + (nec | tank) + pgl(tank)")
  expect_identical(environment(out), environment(f))
  expect_null(bayesnec:::parse_disp_term(
    bayesnec:::drop_disp_term(bnf(y ~ crf(x, "nec3param") + disp("power")))
  ))
  # A formula without the term is returned as it was.
  f0 <- bnf(y ~ crf(x, "nec3param"))
  expect_identical(bayesnec:::drop_disp_term(f0), f0)
})

test_that("bnec_hurdle passes a disp() term to the growth fit only (#410)", {
  dat <- data.frame(x = rep(1:4, each = 5),
                    y = c(seq(1.1, 3.9, length.out = 15), rep(0, 5)))
  seen <- list()
  local_mocked_bindings(
    bnec = function(formula, data, family, ...) {
      seen[[length(seen) + 1]] <<- list(formula = formula,
                                        family = family$family)
      structure(list(model = "nec3param"), class = c("bayesnecfit", "bnecfit"))
    },
    .package = "bayesnec"
  )
  for (term in c("disp(\"power\")", "disp(~x)")) {
    seen <- list()
    f <- stats::as.formula(paste("y ~ crf(x, \"nec3param\") +", term))
    out <- suppressMessages(bnec_hurdle(f, data = dat))
    expect_length(seen, 2)
    expect_equal(seen[[1]]$family, "Gamma")
    expect_equal(bayesnec:::parse_disp_term(seen[[1]]$formula),
                 bayesnec:::parse_disp_term(bnf(f)))
    expect_equal(seen[[2]]$family, "bernoulli")
    expect_null(bayesnec:::parse_disp_term(seen[[2]]$formula))
    # The stored formula is the one the user wrote, term included.
    expect_false(is.null(bayesnec:::parse_disp_term(out$formula)))
  }
})

test_that("a disp() term the growth family cannot take is refused first (#410)", {
  dat <- data.frame(x = rep(1:4, each = 5),
                    y = c(seq(1.1, 3.9, length.out = 15), rep(0, 5)))
  calls <- 0L
  local_mocked_bindings(
    bnec = function(...) {
      calls <<- calls + 1L
      stop("component fit should not start")
    },
    .package = "bayesnec"
  )
  # poisson has no dispersion parameter. Refused ahead of the count-truncation
  # check, which would otherwise answer first on a brms without the fix.
  expect_error(
    bnec_hurdle(y ~ crf(x, "nec3param") + disp("power"), data = dat,
                family_growth = poisson()),
    "Family poisson has no free dispersion parameter"
  )
  # A variance function outside the ones the growth family takes.
  expect_error(
    bnec_hurdle(y ~ crf(x, "nec3param") + disp("twosided"), data = dat),
    "not valid for the Gamma family"
  )
  expect_equal(calls, 0L)
})

# Fitted once and reused across the blocks below: bnec_hurdle() compiles two
# Stan programs per call. The growth response is simulated with a shape that
# rises with the mean, which is what disp("power") describes, so the fit has a
# dispersion trend to estimate.
disp_hurdle_fixture <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      set.seed(410)
      x <- rep(seq(0, 5, length.out = 15), each = 6)
      alive <- stats::rbinom(length(x), 1, 1 / (1 + exp(-(2.5 - 0.9 * x))))
      mu <- pmax(3 - 0.35 * x, 0.3)
      shape <- 2 * mu^1.5
      g <- stats::rgamma(length(x), shape = shape, rate = shape / mu)
      dat <- data.frame(x = x, y = ifelse(alive == 1, g, 0))
      fit <- suppressWarnings(suppressMessages(
        bnec_hurdle(y ~ crf(x, "nec3param") + disp("power"), data = dat,
                    iter = 400, warmup = 200, chains = 2, seed = 410,
                    refresh = 0)
      ))
      cached <<- list(dat = dat, fit = fit)
    }
    cached
  }
})

test_that("a disp() term is fitted in the growth component only (#410)", {
  skip_on_cran()
  fit <- disp_hurdle_fixture()$fit
  expect_s3_class(fit, "bayesnechurdlefit")
  expect_true("shape" %in% names(fit$growth$fit$formula$pforms))
  expect_true(all(c("b_c0_Intercept", "b_c1_Intercept") %in%
                    brms::variables(fit$growth$fit)))
  expect_false("shape" %in% names(fit$survival$fit$formula$pforms))
  expect_false(any(grepl("^b_c[01]_", brms::variables(fit$survival$fit))))
  expect_null(bayesnec:::parse_disp_term(fit$survival$bayesnecformula))
  # The growth predictions read the sub-model: the shape differs across the
  # range, where a constant-dispersion fit would return one value.
  shp <- brms::posterior_epred(fit$growth$fit, dpar = "shape",
                               newdata = data.frame(x = c(0, 5)))
  expect_false(isTRUE(all.equal(shp[, 1], shp[, 2])))
})

test_that("the hurdle methods run on a growth disp() term (#410)", {
  skip_on_cran()
  f <- disp_hurdle_fixture()
  fit <- f$fit
  res <- suppressWarnings(check_fit(fit))
  expect_named(res, c("growth", "survival", "combined"))
  expect_equal(sum(as.data.frame(res$combined)$n), nrow(f$dat))
  for (w in c("combined", "growth", "survival")) {
    pp <- posterior_predict(fit, which = w, resolution = 10)
    expect_equal(ncol(pp), 10)
    expect_true(all(is.finite(pp)))
    fv <- fitted(fit, which = w, resolution = 10)
    expect_equal(nrow(fv), 10)
    expect_true(all(is.finite(fv)))
  }
  # dispersion() computes its statistic for poisson and binomial only, so for
  # a Gamma growth component and a bernoulli survival one it returns no draws.
  d <- dispersion(fit)
  expect_named(d, c("growth", "survival"))
  expect_length(d$growth, 0)
  expect_true(all(is.finite(nec(fit))))
  expect_s3_class(autoplot(fit), "ggplot")
})

test_that("bnec_joint builds the growth disp() term into the joint model (#410)", {
  skip_on_cran()
  # The joint refit of the fitted fixture, with bnec() replaced so that the
  # joint model is built but not sampled. Its shape sub-model has to be the one
  # the growth component was fitted with: the same curve and the same
  # centring literal, computed from the survivors in both routes.
  f <- disp_hurdle_fixture()
  captured <- NULL
  local_mocked_bindings(
    bnec = function(formula, data, family, model_survival, ...) {
      captured <<- list(formula = formula, data = data, family = family)
      invisible(NULL)
    },
    .package = "bayesnec"
  )
  suppressMessages(bnec_joint(f$fit))
  expect_equal(captured$family, "hurdle_gamma")
  joint <- make_brmsformula(captured$formula, captured$data,
                            family = validate_family("hurdle_gamma"))[[1]]
  expect_identical(deparse1(joint$pforms$shape[[3]]),
                   deparse1(f$fit$growth$fit$formula$pforms$shape[[3]]))
  # the hu block gains no dispersion term
  hu_rhs <- vapply(joint$pforms[grep("^hu", names(joint$pforms))], deparse1,
                   character(1))
  expect_false(any(grepl("c0|c1|shape", hu_rhs)))
})

test_that("swap_crf_model swaps a single model and a model group", {
  f <- bnf(y ~ crf(x, "nec3param"))
  expect_equal(
    bayesnec:::get_model_from_formula(bayesnec:::swap_crf_model(f, "ecxexp")),
    "ecxexp"
  )
  expect_setequal(
    bayesnec:::get_model_from_formula(bayesnec:::swap_crf_model(f, "nec")),
    models()$nec
  )
  expect_setequal(
    bayesnec:::get_model_from_formula(
      bayesnec:::swap_crf_model(f, c("nec3param", "ecxexp"))
    ),
    c("nec3param", "ecxexp")
  )
})

test_that("swap_crf_model preserves a transformed predictor", {
  f <- bnf(y ~ crf(log(x), "nec3param"))
  out <- bayesnec:::swap_crf_model(f, "ecxexp")
  expect_true(grepl("log(x)", deparse1(out), fixed = TRUE))
})

test_that("bnec_hurdle rejects inputs that break the zero-as-death convention", {
  dat <- data.frame(x = rep(1:4, each = 5), y = c(rep(1, 15), rep(0, 5)))
  # no zeros: nothing died, so there is no survival component to fit
  expect_error(
    bnec_hurdle(y ~ crf(x, "nec3param"), data = transform(dat, y = 1)),
    "no zeros"
  )
  # all zeros
  expect_error(
    bnec_hurdle(y ~ crf(x, "nec3param"), data = transform(dat, y = 0)),
    "Every value"
  )
  # NA cannot be distinguished from a death
  na_dat <- dat; na_dat$y[1] <- NA
  expect_error(
    bnec_hurdle(y ~ crf(x, "nec3param"), data = na_dat), "contains NA"
  )
  # negatives cannot be represented by a zero-bounded growth component
  neg_dat <- dat; neg_dat$y[1] <- -3
  expect_error(
    bnec_hurdle(y ~ crf(x, "nec3param"), data = neg_dat), "negative values"
  )
  # response absent from data
  expect_error(
    bnec_hurdle(z ~ crf(x, "nec3param"), data = dat), "not a column"
  )
  # non-numeric response
  chr_dat <- dat; chr_dat$y <- as.character(chr_dat$y)
  expect_error(
    bnec_hurdle(y ~ crf(x, "nec3param"), data = chr_dat), "must be numeric"
  )
})

test_that("a growth component at a bound is named as that component (#400)", {
  # Every survivor is 1, so the growth component, a Beta response here, does
  # not vary although the response as a whole does. Left to the growth bnec()
  # call, the refusal named the response and said that every value of it is 1,
  # of a column that also holds the zeros.
  dat <- data.frame(x = as.numeric(rep(1:4, each = 5)),
                    y = c(rep(1, 15), rep(0, 5)))
  calls <- 0L
  local_mocked_bindings(
    bnec = function(...) {
      calls <<- calls + 1L
      stop("component fit should not start")
    },
    .package = "bayesnec"
  )
  err <- expect_error(suppressMessages(
    bnec_hurdle(y ~ crf(x, "nec3param"), data = dat)
  ))
  expect_match(conditionMessage(err),
               paste0("The growth component of the response \"y\" (its 15",
                      " non-zero values) is at the upper bound of a beta",
                      " response in every observation: every value is 1."),
               fixed = TRUE)
  expect_identical(calls, 0L)
})

test_that("a missing predictor is refused before the growth bound (#400)", {
  # model.frame() drops the row, so read after it the survivors were all 1 and
  # the call was refused as a growth component at the bound.
  dat <- data.frame(x = as.numeric(rep(1:4, each = 5)),
                    y = c(rep(1, 14), 0.6, rep(0, 5)))
  dat$x[15] <- NA
  calls <- 0L
  local_mocked_bindings(
    bnec = function(...) {
      calls <<- calls + 1L
      stop("component fit should not start")
    },
    .package = "bayesnec"
  )
  err <- expect_error(suppressMessages(
    bnec_hurdle(y ~ crf(x, "nec3param"), data = dat)
  ))
  expect_match(conditionMessage(err), "row\\(s\\) with missing values")
  expect_match(conditionMessage(err), "at row\\(s\\) 15")
  expect_false(grepl("upper bound", conditionMessage(err)))
  expect_identical(calls, 0L)
})

test_that("crossed_weights is the outer product of component weights", {
  # Mock two model-averaged fits: crossed_weights only reads mod_stats.
  mock_manec <- function(w) {
    structure(list(mod_stats = data.frame(wi = w, row.names = names(w))),
              class = "bayesmanecfit")
  }
  obj <- structure(
    list(growth = mock_manec(c(nec3param = 0.7, nec4param = 0.3)),
         survival = mock_manec(c(ecxexp = 0.25, ecxll3 = 0.75))),
    class = c("bayesnechurdlefit", "bnecfit")
  )
  w <- crossed_weights(obj)
  expect_equal(dim(w), c(2L, 2L))
  expect_equal(sum(w), 1)
  expect_equal(unname(w["nec3param", "ecxll3"]), 0.7 * 0.75)
  expect_equal(unname(w["nec4param", "ecxexp"]), 0.3 * 0.25)
  expect_equal(dimnames(w),
               list(growth = c("nec3param", "nec4param"),
                    survival = c("ecxexp", "ecxll3")))
})

test_that("crossed_weights handles single-model components", {
  obj <- structure(
    list(growth = structure(list(model = "nec3param"), class = "bayesnecfit"),
         survival = structure(list(model = "ecxexp"), class = "bayesnecfit")),
    class = c("bayesnechurdlefit", "bnecfit")
  )
  w <- crossed_weights(obj)
  expect_equal(unname(w), matrix(1, 1, 1))
  expect_equal(dimnames(w), list(growth = "nec3param", survival = "ecxexp"))
})

test_that("crossed_weights rejects other classes", {
  expect_error(crossed_weights(list()), "bayesnechurdlefit")
})

test_that("nec and ecx generics accept extra arguments", {
  # The bayesnechurdlefit methods add a `which` argument, which requires the
  # generics to carry `...`. Guards against that being removed.
  expect_true("..." %in% names(formals(nec)))
  expect_true("..." %in% names(formals(ecx)))
})
