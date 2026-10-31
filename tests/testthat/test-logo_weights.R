test_that("gauss_legendre integrates polynomials on [-1, 1]", {
  gl <- bayesnec:::gauss_legendre(10)
  expect_equal(sum(gl$w), 2)
  expect_equal(sum(gl$w * gl$x^2), 2 / 3)
  expect_equal(sum(gl$w * gl$x^18), 2 / 19)
})

# A normal observation model, whose integral over a normal deviation is a
# multivariate normal density.
normal_groups <- function(n_draws = 40, n_levels = 6, n_obs = 4) {
  set.seed(529)
  mu <- matrix(rnorm(n_draws * n_levels), n_draws, n_levels)
  sigma <- 0.1
  sd <- runif(n_draws, 0.4, 1)
  y <- matrix(rnorm(n_levels * n_obs, 0.5, 0.6), n_obs, n_levels)
  h <- function(z) {
    out <- 0
    for (i in seq_len(n_obs)) {
      out <- out + dnorm(rep(y[i, ], each = n_draws), mu + z, sigma, log = TRUE)
    }
    out
  }
  # Draws from each integrand's normalised density, as a fit supplies.
  prec <- 1 / sd^2 + n_obs / sigma^2
  mean_z <- (n_obs / sigma^2) *
    (matrix(colMeans(y), n_draws, n_levels, byrow = TRUE) - mu) / prec
  r0 <- mean_z + rnorm(n_draws * n_levels) / sqrt(prec)
  exact <- matrix(NA_real_, n_draws, n_levels)
  for (s in seq_len(n_draws)) {
    cov <- diag(sigma^2, n_obs) + sd[s]^2
    for (l in seq_len(n_levels)) {
      r <- y[, l] - mu[s, l]
      exact[s, l] <- -0.5 * (n_obs * log(2 * pi) +
                               as.numeric(determinant(cov)$modulus) +
                               sum(r * solve(cov, r)))
    }
  }
  list(h = h, r0 = r0, sd = sd, exact = exact)
}

test_that("logo_integrate matches the closed form for normal groups", {
  g <- normal_groups()
  out <- bayesnec:::logo_integrate(g$h, g$r0, g$sd)
  expect_equal(dim(out), dim(g$exact))
  expect_equal(out, g$exact, tolerance = 1e-6)
})

test_that("logo_integrate handles a censored group", {
  # Left-censored observations: the integrand has a plateau as the deviation
  # falls and a cliff as it rises.
  set.seed(1)
  n_draws <- 30
  mu <- matrix(rnorm(n_draws * 3, 0, 0.3), n_draws, 3)
  sd <- runif(n_draws, 0.3, 1.5)
  r0 <- matrix(rnorm(n_draws * 3, -0.5, 0.3), n_draws, 3)
  # One observation, whose integral is a normal probability.
  out <- bayesnec:::logo_integrate(
    function(z) pnorm(0, mu + z, 0.2, log.p = TRUE), r0, sd
  )
  expect_equal(out, pnorm(0, mu, sqrt(0.2^2 + sd^2), log.p = TRUE),
               tolerance = 1e-3)
  # Four observations, against numerical integration.
  out <- bayesnec:::logo_integrate(
    function(z) 4 * pnorm(0, mu + z, 0.2, log.p = TRUE), r0, sd
  )
  exact <- out
  for (s in seq_len(n_draws)) {
    for (l in 1:3) {
      exact[s, l] <- log(stats::integrate(function(z) {
        exp(4 * pnorm(0, mu[s, l] + z, 0.2, log.p = TRUE)) *
          dnorm(z, 0, sd[s])
      }, -Inf, Inf, rel.tol = 1e-10)$value)
    }
  }
  expect_lt(max(abs(out - exact)), 2e-3)
})

# A model set as weight_criteria() reads it, without a fit: each equation's
# group-level terms, data and stored criteria.
fake_set <- function(ranef_rows, data, logo = NULL) {
  one <- function() {
    fit <- list(ranef = ranef_rows, data = data,
                criteria = list(loo = structure(list(), class = "psis_loo")))
    if (!is.null(logo)) fit$criteria$logo <- logo
    list(fit = fit)
  }
  list(ecx4param = one(), ecxlin = one())
}

ranef_row <- function(group, nlpar = "ogl", coef = "Intercept",
                      dist = "gaussian") {
  data.frame(group = group, nlpar = nlpar, dpar = "", coef = coef,
             dist = dist)
}

lum_like <- function() {
  d <- expand.grid(conc = c(0.1, 1, 10), plate = c("a", "b"), well = 1:2)
  d$cell <- paste(d$plate, d$conc)
  d
}

test_that("logo_grouping finds a grouping observed at one concentration per level", {
  d <- lum_like()
  set <- fake_set(rbind(ranef_row("cell"), ranef_row("plate", "top")), d)
  expect_identical(bayesnec:::logo_grouping(set, d$conc),
                   list(group = "cell", reason = NULL))
  # A grouping that spans the concentrations is held, not left out.
  set <- fake_set(ranef_row("plate"), d)
  expect_null(bayesnec:::logo_grouping(set, d$conc)$group)
  expect_null(bayesnec:::logo_grouping(set, d$conc)$reason)
  # An interaction grouping is read from its variables.
  set <- fake_set(ranef_row("plate:conc"), d)
  expect_identical(bayesnec:::logo_grouping(set, d$conc)$group, "plate:conc")
})

test_that("logo_grouping gives a reason where the deviations are not one per level", {
  d <- lum_like()
  # pgl() on the grouping: a deviation on every parameter.
  set <- fake_set(rbind(ranef_row("cell", "top"), ranef_row("cell", "ec50")), d)
  out <- bayesnec:::logo_grouping(set, d$conc)
  expect_null(out$group)
  expect_match(out$reason, "more than one parameter in ecx4param")
  set <- fake_set(ranef_row("cell", dist = "student"), d)
  expect_match(bayesnec:::logo_grouping(set, d$conc)$reason,
               "other than a normal intercept")
  # Two nested groupings, as ogl(cell/well) gives.
  d$cell_well <- paste(d$cell, d$well)
  set <- fake_set(rbind(ranef_row("cell"), ranef_row("cell_well")), d)
  expect_match(bayesnec:::logo_grouping(set, d$conc)$reason,
               "more than one grouping \\(cell, cell_well\\)")
})

test_that("weight_criteria reuses a stored criterion for the same grouping", {
  d <- lum_like()
  held <- structure(list(pointwise = matrix(0, 6, 1)), group = "cell")
  set <- fake_set(ranef_row("cell"), d, logo = held)
  local_mocked_bindings(logo_criterion = function(...) stop("recomputed"))
  out <- bayesnec:::weight_criteria(set, d$conc)
  expect_identical(out$leave_out, "cell")
  expect_identical(out$criteria$ecxlin, held)
  # A criterion for another grouping is not reused.
  attr(set$ecxlin$fit$criteria$logo, "group") <- "plate"
  expect_error(bayesnec:::weight_criteria(set, d$conc), "recomputed")
})

test_that("weight_criteria leaves out observations where asked or unsupported", {
  d <- lum_like()
  set <- fake_set(ranef_row("cell"), d)
  local_mocked_bindings(logo_criterion = function(...) stop("recomputed"))
  out <- bayesnec:::weight_criteria(set, d$conc, "observation")
  expect_identical(out$leave_out, "observation")
  expect_null(out$logo)
  set <- fake_set(rbind(ranef_row("cell", "top"), ranef_row("cell", "ec50")), d)
  expect_warning(out <- bayesnec:::weight_criteria(set, d$conc),
                 "leaving out each observation, because the grouping cell")
  expect_identical(out$leave_out, "observation")
})

test_that("report_logo_k names contributing equations only", {
  l <- function(k) structure(list(pointwise = matrix(0, 4, 1),
                                  diagnostics = list(pareto_k = k)),
                             group = "cell")
  logo <- list(ecxlin = l(c(0.2, 0.9, 0.3, 0.1)),
               ecx4param = l(c(0.8, 0.9, 0.3, 0.1)),
               ecxexp = l(c(0.2, 0.3, 0.3, 0.1)))
  expect_message(bayesnec:::report_logo_k(logo, c(0.005, 0.9, 0.095)),
                 "2 of 4 levels of cell in ecx4param \\(weight 0.9\\)\\.")
  expect_no_message(bayesnec:::report_logo_k(logo, c(0.005, 0, 0.995)))
  expect_no_message(bayesnec:::report_logo_k(NULL, 1))
})

test_that("loo_controls takes leave_out", {
  expect_identical(
    bayesnec:::define_loo_controls(family_str = "gaussian")$leave_out, "group"
  )
  out <- bayesnec:::define_loo_controls(
    list(leave_out = "observation", weights = list(BB = FALSE)), "gaussian"
  )
  expect_identical(names(out), c("fitting", "weights", "leave_out", "refit"))
  expect_identical(out$leave_out, "observation")
  expect_error(bayesnec:::validate_loo_controls(list(leave_out = "plate"),
                                                "gaussian"),
               "should be \"group\" or \"observation\"")
  expect_error(bayesnec:::validate_loo_controls(list(leave_out = TRUE),
                                                "gaussian"),
               "should be \"group\" or \"observation\"")
})

test_that("a model set that records no leave_out was scored by observation", {
  set <- structure(list(), class = c("bayesmanecfit", "bnecfit"))
  args <- bayesnec:::recorded_expand_args(list(set), method = "pseudobma",
                                          fam_tag = "gaussian")
  expect_identical(args$loo_controls$leave_out, "observation")
  attr(set, "bnec_expand_args") <- list(
    loo_controls = list(fitting = list(), weights = list(method = "pseudobma"),
                        leave_out = "group")
  )
  args <- bayesnec:::recorded_expand_args(list(set), method = "pseudobma",
                                          fam_tag = "gaussian")
  expect_identical(args$loo_controls$leave_out, "group")
  # A single fit holds no weights, so its record is not read.
  single <- structure(list(), class = c("bayesnecfit", "bnecfit"))
  args <- bayesnec:::recorded_expand_args(list(single), method = "pseudobma",
                                          fam_tag = "gaussian")
  expect_identical(args$loo_controls$leave_out, "group")
  # The caller's value is used.
  args <- bayesnec:::recorded_expand_args(
    list(structure(list(), class = c("bayesmanecfit", "bnecfit"))),
    loo_controls = list(leave_out = "group"), method = "pseudobma",
    fam_tag = "gaussian"
  )
  expect_identical(args$loo_controls$leave_out, "group")
})

test_that("loo_controls takes refit under leave_out = \"group\" only", {
  expect_false(bayesnec:::define_loo_controls(family_str = "gaussian")$refit)
  expect_true(
    bayesnec:::define_loo_controls(list(refit = TRUE), "gaussian")$refit
  )
  expect_error(bayesnec:::validate_loo_controls(list(refit = NA), "gaussian"),
               "should be TRUE or FALSE")
  expect_error(bayesnec:::validate_loo_controls(list(refit = "yes"),
                                                "gaussian"),
               "should be TRUE or FALSE")
  expect_error(
    bayesnec:::define_loo_controls(
      list(refit = TRUE, leave_out = "observation"), "gaussian"
    ),
    "applies only where \"leave_out\" is \"group\""
  )
})

test_that("a model set passes on the refit it records", {
  record <- function(leave_out, refit) {
    list(loo_controls = list(fitting = list(),
                             weights = list(method = "pseudobma"),
                             leave_out = leave_out, refit = refit))
  }
  set <- structure(list(), class = c("bayesmanecfit", "bnecfit"),
                   bnec_expand_args = record("group", TRUE))
  args <- function(...) {
    bayesnec:::recorded_expand_args(list(set), method = "pseudobma",
                                    fam_tag = "gaussian", ...)$loo_controls
  }
  expect_true(args()$refit)
  expect_false(args(loo_controls = list(refit = FALSE))$refit)
  # Leaving out each observation drops the inherited refit.
  expect_false(args(loo_controls = list(leave_out = "observation"))$refit)
  # A set that records none was not refitted.
  attr(set, "bnec_expand_args") <- NULL
  expect_false(args(loo_controls = list(leave_out = "group"))$refit)
})

# A criterion over four levels whose Pareto k is set by the caller.
fake_logo <- function(k) {
  set.seed(1)
  ll <- matrix(stats::rnorm(4000, -2, 0.1), 1000, 4)
  out <- suppressWarnings(loo::loo(ll, r_eff = rep(1, 4)))
  out$diagnostics$pareto_k <- k
  out$pointwise[, "influence_pareto_k"] <- k
  attr(out, "group") <- "cell"
  attr(out, "levels") <- c("a", "b", "c", "d")
  out
}

test_that("logo_reloo replaces the scores of the levels it refits", {
  level <- factor(rep(c("a", "b", "c", "d"), each = 3))
  logo <- fake_logo(c(0.2, 0.9, 0.8, 0.1))
  local_mocked_bindings(logo_refit_level = function(base, o, group, level, g) {
    if (g == "c") NULL else list(elpd = -5, rhat = 1, fit = base)
  })
  o <- list(fit = "a brmsfit")
  expect_message(
    expect_warning(
      out <- bayesnec:::logo_reloo(o, logo, level, "ecxlin"),
      "failed without 1 level of cell \\(c\\), so its score remains"
    ),
    "Refitting ecxlin 2 times, leaving out each level of cell"
  )
  expect_identical(attr(out, "refitted"), "b")
  expect_identical(attr(out, "refit_failed"), "c")
  expect_identical(unname(out$pointwise[2, "elpd_loo"]), -5)
  expect_equal(out$pointwise[2, "p_loo"],
               sum(logo$pointwise[2, c("elpd_loo", "p_loo")]) + 5,
               ignore_attr = TRUE)
  expect_identical(out$pointwise[-2, "elpd_loo"], logo$pointwise[-2, "elpd_loo"])
  expect_identical(out$diagnostics$pareto_k, c(0.2, 0, 0.8, 0.1))
  expect_equal(out$estimates["elpd_loo", "Estimate"],
               sum(out$pointwise[, "elpd_loo"]))
  expect_equal(out$estimates["looic", "Estimate"],
               -2 * sum(out$pointwise[, "elpd_loo"]))
  expect_identical(attr(out, "group"), "cell")
  # A rebuilt set refits neither level again.
  expect_no_message(again <- bayesnec:::logo_reloo(o, out, level, "ecxlin"))
  expect_identical(again, out)
})

test_that("logo_reloo warns where a refit did not converge", {
  level <- factor(rep(c("a", "b", "c", "d"), each = 3))
  local_mocked_bindings(logo_refit_level = function(base, o, group, level, g) {
    list(elpd = -5, rhat = 1.2, fit = base)
  })
  expect_warning(
    suppressMessages(bayesnec:::logo_reloo(list(fit = NULL),
                                           fake_logo(c(0.2, 0.9, 0.3, 0.1)),
                                           level, "ecxlin")),
    "Rhat above 1.01 \\(b\\)"
  )
})

test_that("weight_criteria refits only where asked", {
  d <- lum_like()
  held <- structure(list(pointwise = matrix(0, 6, 1)), group = "cell")
  set <- fake_set(ranef_row("cell"), d, logo = held)
  local_mocked_bindings(logo_reloo = function(o, logo, level, model) {
    attr(logo, "refitted") <- model
    logo
  })
  out <- bayesnec:::weight_criteria(set, d$conc)
  expect_null(attr(out$criteria$ecxlin, "refitted"))
  out <- bayesnec:::weight_criteria(set, d$conc, refit = TRUE)
  expect_identical(attr(out$criteria$ecxlin, "refitted"), "ecxlin")
})

test_that("report_logo_k suggests a refit only where none was asked for", {
  logo <- list(ecxlin = fake_logo(c(0.2, 0.9, 0.3, 0.1)))
  expect_message(bayesnec:::report_logo_k(logo, 1),
                 "list\\(refit = TRUE\\) replaces them")
  msg <- tryCatch(bayesnec:::report_logo_k(logo, 1, refit = TRUE),
                  message = conditionMessage)
  expect_match(msg, "1 of 4 levels of cell in ecxlin")
  expect_no_match(msg, "refit = TRUE")
})

test_that("refit_inits sizes the deviations to the refit's levels", {
  fit <- list(ranef = data.frame(id = c(1, 2), group = c("plate", "plate:cell"),
                                 stringsAsFactors = FALSE))
  train <- data.frame(plate = rep(1:2, each = 3), cell = rep(1:3, 2))
  init <- list(list(b_top = 1, z_1 = matrix(0, 1, 2), z_2 = matrix(0, 1, 7)))
  out <- bayesnec:::refit_inits(init, fit, train)
  expect_identical(dim(out[[1]]$z_1), c(1L, 2L))
  expect_identical(dim(out[[1]]$z_2), c(1L, 6L))
  expect_identical(out[[1]]$b_top, 1)
  expect_identical(bayesnec:::refit_inits("random", fit, train), "random")
})

# Five groups at each of six concentrations, three observations each. The
# true curve is sigmoid, and ecxlin is the curve of the wrong shape.
logo_fixture <- local({
  fit <- NULL
  function() {
    if (is.null(fit)) {
      set.seed(529)
      d <- expand.grid(x = seq(-2, 3, length.out = 6), g = 1:5, rep = 1:3)
      d$g <- factor(paste(d$x, d$g))
      dev <- stats::setNames(stats::rnorm(nlevels(d$g), 0, 3), levels(d$g))
      d$y <- 10 + 80 / (1 + exp((d$x - 0.5) * 2)) + dev[as.character(d$g)] +
        stats::rnorm(nrow(d), 0, 1)
      fit <<- suppressWarnings(suppressMessages(
        bnec(y ~ crf(x, c("ecx4param", "ecxlin")) + ogl(g), data = d,
             family = gaussian(), chains = 2, iter = 2000, seed = 529,
             refresh = 0)
      ))
    }
    fit
  }
})

test_that("a group nested within concentration is left out whole", {
  skip_on_cran()
  fit <- logo_fixture()
  expect_identical(attr(fit$mod_stats$wi, "leave_out"), "g")
  expect_identical(attr(fit, "bnec_expand_args")$loo_controls$leave_out,
                   "group")
  expect_gt(fit$mod_stats["ecx4param", "wi"], 0.99)
  # Scored by observation, the wrong curve keeps a share of the weight,
  # because each group's deviation absorbs the curve's misfit.
  pointwise <- suppressWarnings(suppressMessages(amend(
    fit, loo_controls = list(leave_out = "observation",
                             weights = list(method = "pseudobma", BB = FALSE))
  )))
  expect_identical(attr(pointwise$mod_stats$wi, "leave_out"), "observation")
  expect_gt(pointwise$mod_stats["ecxlin", "wi"], 0.1)
  expect_gt(fit$mod_stats["ecx4param", "elpd"] -
              fit$mod_stats["ecxlin", "elpd"], 20)
  # The short chains of this fixture fail the Rhat check that print() reports.
  expect_output(suppressWarnings(suppressMessages(print(summary(fit)))),
                "each level of g left out")
})

test_that("the left-out score equals the exact normal density", {
  skip_on_cran()
  fit <- logo_fixture()
  f <- fit$mod_fits$ecx4param$fit
  # Under a gaussian family the ogl() deviation is added to the mean, so a
  # group's observations are jointly normal given the population parameters.
  mu <- brms::posterior_epred(f, re_formula = NA)
  sigma <- as.vector(brms::as_draws_matrix(f, variable = "sigma"))
  sd_g <- as.vector(brms::as_draws_matrix(f, variable = "sd_g__ogl_Intercept"))
  level <- bayesnec:::logo_levels(f, "g")
  exact <- sapply(levels(level), function(l) {
    i <- which(level == l)
    vapply(seq_along(sigma), function(s) {
      cov <- diag(sigma[s]^2, length(i)) + sd_g[s]^2
      r <- f$data$y[i] - mu[s, i]
      -0.5 * (length(i) * log(2 * pi) + as.numeric(determinant(cov)$modulus) +
                sum(r * solve(cov, r)))
    }, numeric(1))
  })
  r_eff <- loo::relative_eff(exp(sweep(exact, 2, apply(exact, 2, max))),
                             chain_id = rep(1:2, each = brms::ndraws(f) / 2))
  reference <- suppressWarnings(loo::loo(exact, r_eff = r_eff))
  expect_equal(f$criteria$logo$pointwise[, "elpd_loo"],
               reference$pointwise[, "elpd_loo"], tolerance = 1e-4,
               ignore_attr = TRUE)
})

test_that("a refitted level is scored by the exact normal density", {
  skip_on_cran()
  fit <- logo_fixture()
  o <- fit$mod_fits$ecx4param
  level <- bayesnec:::logo_levels(o$fit, "g")
  g <- levels(level)[4]
  out <- suppressWarnings(
    bayesnec:::logo_refit_level(o$fit, o, "g", level, g)
  )
  refit <- out$fit
  expect_identical(nlevels(refit$data$g), nlevels(level) - 1L)
  expect_equal(refit$prior, o$fit$prior, ignore_attr = TRUE)
  # The left-out group is a new level of the refit, so its observations are
  # jointly normal given the refit's population parameters.
  i <- which(level == g)
  mu <- brms::posterior_epred(refit, newdata = o$fit$data[i, ], re_formula = NA)
  sigma <- as.vector(brms::as_draws_matrix(refit, variable = "sigma"))
  sd_g <- as.vector(brms::as_draws_matrix(refit,
                                          variable = "sd_g__ogl_Intercept"))
  exact <- vapply(seq_along(sigma), function(s) {
    cov <- diag(sigma[s]^2, length(i)) + sd_g[s]^2
    r <- o$fit$data$y[i] - mu[s, ]
    -0.5 * (length(i) * log(2 * pi) + as.numeric(determinant(cov)$modulus) +
              sum(r * solve(cov, r)))
  }, numeric(1))
  m <- max(exact)
  expect_equal(out$elpd, m + log(mean(exp(exact - m))), tolerance = 1e-5)
})

test_that("amend() records a refit and reuses the stored scores", {
  skip_on_cran()
  fit <- logo_fixture()
  # No level of this fixture has k above 0.7, so nothing is refitted.
  expect_true(all(fit$mod_fits$ecx4param$fit$criteria$logo$diagnostics$pareto_k
                  <= 0.7))
  local_mocked_bindings(logo_refit_level = function(...) stop("refitted"))
  out <- suppressWarnings(suppressMessages(
    amend(fit, loo_controls = list(refit = TRUE))
  ))
  expect_true(attr(out, "bnec_expand_args")$loo_controls$refit)
  expect_equal(out$mod_stats$elpd, fit$mod_stats$elpd)
})
