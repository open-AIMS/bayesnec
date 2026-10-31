# Share one Gamma nec3param fit with shape 60 at tight and 4 at loose;
# 70% of rows are tight. The roughly fourfold spread contrast makes
# dispersion marginalisation distinguishable from either level alone.
disp_z_fixture <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      set.seed(443)
      x <- rep(seq(0, 4, length.out = 8), each = 10)
      z <- factor(rep(c("tight", "tight", "tight", "loose", "tight", "tight",
                        "loose", "tight", "tight", "loose"), 8),
                  levels = c("tight", "loose"))
      mu <- ifelse(x < 1.5, 2, 2 * exp(-0.8 * (x - 1.5)))
      shape <- ifelse(z == "tight", 60, 4)
      dat <- data.frame(x = x, z = z,
                        y = stats::rgamma(length(x), shape = shape,
                                          rate = shape / mu))
      fit <- suppressWarnings(suppressMessages(
        bnec(y ~ crf(x, "nec3param") + disp(~z), data = dat,
             family = Gamma(link = "identity"), iter = 400, warmup = 200,
             chains = 2, seed = 443, refresh = 0)
      ))
      cached <<- list(dat = dat, fit = fit)
    }
    cached
  }
})

test_that("a Gamma nec3param fit with disp(~z) on a factor completes (#443)", {
  skip_on_cran()
  fit <- disp_z_fixture()$fit
  expect_s3_class(fit, "bayesnecfit")
  expect_true("shape" %in% names(pull_brmsfit(fit)$formula$pforms))
  expect_true(all(is.finite(as.matrix(fit$pred_vals$data))))
  # the shape the fit estimates differs between the levels, so the value the
  # grid holds matters for the dispersion even though it cannot for the mean
  nd <- bnec_newdata(fit, resolution = 2)
  nd_loose <- nd
  nd_loose$z <- factor("loose", levels = c("tight", "loose"))
  shp <- brms::posterior_epred(pull_brmsfit(fit), newdata = nd, dpar = "shape")
  shp_loose <- brms::posterior_epred(pull_brmsfit(fit), newdata = nd_loose,
                                     dpar = "shape")
  expect_gt(stats::median(shp[, 1]), 4 * stats::median(shp_loose[, 1]))
})

test_that("estimates read from the mean ignore the fixed value (#443)", {
  skip_on_cran()
  fit <- disp_z_fixture()$fit
  read_all <- function() {
    suppressWarnings(suppressMessages(list(
      grid = bnec_newdata(fit, resolution = 20),
      epred = posterior_epred(fit, newdata = bnec_newdata(fit, resolution = 20),
                              re_formula = NA),
      ecx = ecx(fit, ecx_val = 10, posterior = TRUE),
      ecx50 = ecx(fit, ecx_val = 50),
      nec = nec(fit, posterior = TRUE),
      nsec = nsec(fit, posterior = TRUE)
    )))
  }
  first <- read_all()
  expect_true(all(first$grid$z == "tight"))
  local_mocked_bindings(
    disp_fixed_value = function(x) factor(levels(x)[2], levels = levels(x)),
    .package = "bayesnec"
  )
  second <- read_all()
  expect_true(all(second$grid$z == "loose"))
  for (nm in c("epred", "ecx", "ecx50", "nec", "nsec")) {
    expect_identical(second[[nm]], first[[nm]], label = nm)
  }
})

test_that("marginalised readings reproduce the design's mix of z (#443)", {
  skip_on_cran()
  fit <- disp_z_fixture()$fit
  brms_fit <- pull_brmsfit(fit)
  nd <- bnec_newdata(fit, resolution = 5)
  set.seed(1)
  res <- bayesnec:::disp_marginal_draws(brms_fit, nd, "z")
  n <- length(res$rows)
  expect_equal(dim(res$draws), c(n, 5))
  assigned <- brms_fit$data$z[res$rows]
  # each draw takes the level of a fitted row, in the proportion the design
  # holds it, within four binomial standard errors of 0.7
  share <- mean(assigned == "tight")
  expect_lt(abs(share - 0.7), 4 * sqrt(0.7 * 0.3 / n))
  # and each level's draws have the spread of a reading at that level, read
  # against a prediction with the level written on the grid by hand
  spread <- function(m) sqrt(mean(apply(m, 2, stats::var)))
  ratio <- c()
  for (lev in levels(assigned)) {
    fixed <- nd
    fixed$z <- factor(lev, levels = levels(assigned))
    attr(fixed, "bnec_disp_placeholders") <- NULL
    ref <- posterior_predict(fit, newdata = fixed)
    ratio[lev] <- spread(res$draws[assigned == lev, , drop = FALSE]) /
      spread(ref)
  }
  expect_true(all(ratio > 0.8 & ratio < 1.25), label = paste(ratio))
  # the mixture's spread lies between the two levels' spreads
  mix <- spread(res$draws)
  expect_gt(mix, spread(res$draws[assigned == "tight", , drop = FALSE]))
  expect_lt(mix, spread(res$draws[assigned == "loose", , drop = FALSE]))
})

test_that("posterior_predict() and predict() marginalise a marked grid (#443)", {
  skip_on_cran()
  fit <- disp_z_fixture()$fit
  nd <- bnec_newdata(fit, resolution = 5)
  set.seed(2)
  pp <- posterior_predict(fit, newdata = nd)
  set.seed(2)
  direct <- bayesnec:::disp_marginal_draws(pull_brmsfit(fit), nd, "z")$draws
  expect_identical(pp, direct)
  # reproducible under a seed, as brms's own simulation is
  set.seed(2)
  expect_identical(posterior_predict(fit, newdata = nd), pp)
  # and the newdata argument binds positionally, as brms's does
  set.seed(2)
  expect_identical(posterior_predict(fit, nd), pp)
  # a subset of draws, in the order brms returns them
  set.seed(3)
  sub <- posterior_predict(fit, newdata = nd, draw_ids = c(300, 5, 120))
  expect_equal(dim(sub), c(3, 5))
  expect_error(posterior_predict(fit, newdata = nd, ndraws = 0),
               "should be between 1")
  # predict() summarises the same draws in the form brms gives
  set.seed(2)
  pr <- predict(fit, newdata = nd)
  expect_equal(dim(pr), c(5, 4))
  expect_identical(colnames(pr), c("Estimate", "Est.Error", "Q2.5", "Q97.5"))
  expect_equal(unname(pr[, "Estimate"]), unname(colMeans(pp)))
  set.seed(2)
  expect_identical(predict(fit, newdata = nd, summary = FALSE), pp)
  set.seed(2)
  rob <- predict(fit, newdata = nd, robust = TRUE, probs = c(0.1, 0.9))
  expect_identical(colnames(rob), c("Estimate", "Est.Error", "Q10", "Q90"))
})

test_that("a newdata without z is refused with the column named (#443)", {
  skip_on_cran()
  fit <- disp_z_fixture()$fit
  nd <- data.frame(x = c(0, 1, 2))
  msg <- "`newdata` has no column \"z\", which the disp\\(\\) term"
  expect_error(posterior_predict(fit, newdata = nd), msg)
  expect_error(predict(fit, newdata = nd), msg)
  expect_error(posterior_epred(fit, newdata = nd), msg)
  expect_error(posterior_epred(fit, nd), msg)
  expect_error(fitted(fit, newdata = nd), msg)
})

test_that("positional newdata is checked before prediction for both fit classes", {
  formula <- bnf(y ~ crf(x, "nec3param") + disp(~z))
  single <- structure(list(bayesnecformula = formula), class = "bayesnecfit")
  model_set <- structure(
    list(mod_fits = list(nec3param = list(bayesnecformula = formula))),
    class = "bayesmanecfit"
  )
  for (fit in list(single, model_set)) {
    expect_error(posterior_epred(fit, data.frame(x = 1)),
                 "`newdata` has no column \"z\"")
  }
})

test_that("a z the user wrote is used as given (#443)", {
  skip_on_cran()
  fit <- disp_z_fixture()$fit
  brms_fit <- pull_brmsfit(fit)
  # unmarked
  nd <- data.frame(x = c(0, 1, 2),
                   z = factor("loose", levels = c("tight", "loose")))
  set.seed(4)
  a <- posterior_predict(fit, newdata = nd)
  set.seed(4)
  expect_identical(a, brms::posterior_predict(brms_fit, newdata = nd))
  # marked, but set by the user to a value other than the placeholder: the
  # attribute survives the assignment, and the choice is still the user's
  grid <- bnec_newdata(fit, resolution = 3)
  grid$z <- factor("loose", levels = c("tight", "loose"))
  expect_identical(attr(grid, "bnec_disp_placeholders"), "z")
  set.seed(4)
  b <- predict(fit, newdata = grid)
  set.seed(4)
  expect_identical(b, stats::predict(brms_fit, newdata = grid))
})

test_that("prediction without newdata is over the fitted rows (#443)", {
  skip_on_cran()
  fit <- disp_z_fixture()$fit
  brms_fit <- pull_brmsfit(fit)
  set.seed(5)
  a <- posterior_predict(fit)
  set.seed(5)
  expect_identical(a, brms::posterior_predict(brms_fit))
  set.seed(5)
  b <- predict(fit)
  set.seed(5)
  expect_identical(b, stats::predict(brms_fit))
})

test_that("a numeric z is marginalised exactly, one call per value (#443)", {
  # Mock posterior_predict() to return draw IDs and grid w values.
  # This checks reassembly without sampling; 0.1 + 0.2 and 0.3 print
  # alike but must remain distinct binary values.
  obs <- data.frame(x = seq_len(60), w = c(rep(0.1 + 0.2, 3), 0.3, 1:56))
  fake <- list(data = obs)
  nd <- data.frame(x = c(1, 2), w = stats::median(obs$w))
  attr(nd, "bnec_disp_placeholders") <- "w"
  calls <- list()
  local_mocked_bindings(
    posterior_predict = function(object, newdata, draw_ids, ...) {
      calls[[length(calls) + 1]] <<- list(w = unique(newdata$w),
                                          ids = draw_ids)
      cbind(draw_ids, rep(newdata$w[1], length(draw_ids)))
    },
    .package = "bayesnec"
  )
  set.seed(6)
  ids <- sample(500, 300)
  res <- bayesnec:::disp_marginal_draws(fake, nd, "w", draw_ids = ids)
  # rows in increasing order of draw, each with the value of its own row
  expect_identical(res$draws[, 1], as.numeric(sort(ids)))
  expect_identical(res$draws[, 2], obs$w[res$rows])
  # one call per distinct value sampled, each with increasing ids, and the
  # two values that print alike kept apart
  sampled <- unique(sprintf("%a", obs$w[res$rows]))
  expect_length(calls, length(sampled))
  expect_true(all(vapply(calls, function(cl) !is.unsorted(cl$ids),
                         logical(1))))
  values <- vapply(calls, function(cl) cl$w, numeric(1))
  expect_true(any(values == 0.3) && any(values == 0.1 + 0.2))
  expect_equal(sum(vapply(calls, function(cl) length(cl$ids), integer(1))),
               300)
})

test_that("joint character values with separators remain distinct", {
  obs <- data.frame(z1 = c("a\r", "a"), z2 = c("b", "\rb"))
  fake <- list(data = obs)
  nd <- data.frame(x = 1, z1 = "a\r", z2 = "b")
  calls <- integer(0)
  local_mocked_bindings(
    posterior_predict = function(object, newdata, draw_ids, ...) {
      value <- if (newdata$z1[1] == "a\r") 1 else 2
      calls <<- c(calls, value)
      matrix(rep(value, length(draw_ids)), ncol = 1)
    },
    .package = "bayesnec"
  )
  set.seed(474)
  res <- bayesnec:::disp_marginal_draws(fake, nd, c("z1", "z2"),
                                        draw_ids = seq_len(100))
  expect_setequal(calls, c(1L, 2L))
  expect_identical(res$draws[, 1], as.numeric(res$rows))
})

test_that("prediction for a fit with no disp(~z) column is unchanged (#443)", {
  skip_on_cran()
  nd <- bnec_newdata(nec4param, resolution = 10)
  expect_null(attr(nd, "bnec_disp_placeholders"))
  brms_fit <- pull_brmsfit(nec4param)
  set.seed(7)
  a <- posterior_predict(nec4param, newdata = nd)
  set.seed(7)
  expect_identical(a, brms::posterior_predict(brms_fit, newdata = nd))
  set.seed(7)
  b <- predict(nec4param, newdata = nd)
  set.seed(7)
  expect_identical(b, stats::predict(brms_fit, newdata = nd))
  set.seed(7)
  c0 <- posterior_predict(nec4param)
  set.seed(7)
  expect_identical(c0, brms::posterior_predict(brms_fit))
})
