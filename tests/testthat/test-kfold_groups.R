test_that("level folds are stratified, stored and accepted from the caller", {
  level <- factor(rep(paste0("cell", seq_len(18)), each = 2))
  concentration <- rep(rep(1:3, each = 6), each = 2)
  set.seed(70)
  before <- .Random.seed
  folds <- bayesnec:::kfold_assign(level, concentration, 3, NULL, 17)
  expect_identical(.Random.seed, before)
  expect_named(folds, levels(level))
  expect_identical(bayesnec:::kfold_assign(level, concentration, 3, NULL, 17),
                   folds)
  for (value in 1:3) {
    cells <- levels(level)[vapply(levels(level), function(g) {
      unique(concentration[level == g]) == value
    }, logical(1))]
    expect_identical(as.integer(table(folds[cells])), rep(2L, 3))
  }
  supplied <- stats::setNames(rep(1:3, each = 6), rev(levels(level)))
  expect_identical(bayesnec:::kfold_assign(level, concentration, 10,
                                           supplied, 17),
                   stats::setNames(as.integer(supplied[levels(level)]),
                                   levels(level)))
  expect_error(bayesnec:::kfold_assign(level, concentration, 3,
                                       supplied[-1], 17),
               "assign every level")
})

test_that("separate folds combine to a comparable exact criterion", {
  data <- data.frame(y = seq_len(8), x = rep(1:2, each = 4))
  cell <- paste0("c", seq_len(8))
  fit <- list(data = data,
              ranef = data.frame(group = character()), criteria = list())
  object <- structure(list(fit = fit, model = "ecxlin",
                           bayesnecformula = bayesnecformula(
                             y ~ crf(x, model = "ecxlin")),
                           retained_data = data.frame(cell = cell)),
                      class = c("bayesnecfit", "bnecfit"))
  assignment <- stats::setNames(rep(1:2, 4), cell)
  local_mocked_bindings(kfold_fit_settings = function(fit)
                          c(iter = 300, warmup = 150),
                        kfold_refit = function(o, data, group, level,
                                                fold_map, fold, settings) {
    names <- names(fold_map)[fold_map == fold]
    list(elpd = stats::setNames(-as.numeric(sub("c", "", names)), names),
         diagnostics = list(max_rhat = 1, divergent = 0,
                            share_max_treedepth = 0, seconds = 1,
                            error = NULL))
  })
  full <- kfold_groups(object, group = "cell", folds = assignment, seed = 7)
  parts <- lapply(1:2, function(k) kfold_groups(
    object, group = "cell", folds = assignment, fold = k, seed = 7
  ))
  combined <- do.call(kfold_combine, parts)
  expect_identical(full, combined)
  expect_s3_class(combined, "kfold")
  expect_identical(rownames(combined$pointwise), cell)
  expect_identical(attr(combined, "folds"), assignment)
  expect_identical(attr(combined, "group"), "cell")
  expect_identical(attr(combined, "refit_settings")$iter, 300L)
  expect_identical(attr(combined, "refit_settings")$warmup, 150L)
  expect_equal(unname(loo::loo_compare(full, combined)[, "elpd_diff"]),
               c(0, 0))
  expect_error(kfold_combine(parts[[1]], parts[[1]]), "duplicate fold")
  other <- parts[[2]]
  other$settings$iter <- 100L
  expect_error(kfold_combine(parts[[1]], other), "different settings")
})

test_that("a failed fold leaves its levels without a score", {
  assignment <- c(a = 1L, b = 2L)
  base <- list(group = "cell", folds = assignment,
               settings = list(iter = 100L), signature = list(ecxlin = "same"))
  first <- structure(c(base, list(fold = 1L, results = list(
    ecxlin = list(elpd = c(a = -1), diagnostics = list(max_rhat = 1))
  ))), class = "kfold_groups_part")
  second <- structure(c(base, list(fold = 2L, results = list(
    ecxlin = list(error = "sampler failed")
  ))), class = "kfold_groups_part")
  expect_warning(out <- kfold_combine(first, second), "no score")
  expect_s3_class(out, "kfold_groups_part")
  expect_true(is.na(out$results$ecxlin$elpd["b"]))
  expect_identical(out$results$ecxlin$diagnostics[["2"]]$error,
                   "sampler failed")
  repaired <- second
  repaired$results$ecxlin <- list(elpd = c(b = -2),
                                   diagnostics = list(max_rhat = 1))
  expect_s3_class(kfold_combine(out, repaired), "kfold")
})

test_that("a fold that loses another grouping's level is refused", {
  data <- data.frame(y = 1:8, x = rep(1:2, each = 4),
                     plate = rep(c("p1", "p2"), each = 4))
  fit <- list(data = data,
              ranef = data.frame(group = "plate", id = 1,
                                 nlpar = "top", dpar = "", coef = "Intercept",
                                 dist = "gaussian"))
  object <- structure(list(fit = fit, model = "ecxlin",
                           bayesnecformula = bayesnecformula(
                             y ~ crf(x, model = "ecxlin")),
                           retained_data = data.frame(cell = letters[1:8])),
                      class = c("bayesnecfit", "bnecfit"))
  assignment <- stats::setNames(rep(1:2, each = 4), letters[1:8])
  expect_error(kfold_groups(object, group = "cell", folds = assignment),
               "removes a whole level of plate")
})

test_that("a refit error is retained with the failed fold", {
  data <- data.frame(y = 1:4, x = c(1, 1, 2, 2))
  object <- structure(list(
    fit = list(data = data, ranef = data.frame(group = character())),
    model = "ecxlin",
    bayesnecformula = bayesnecformula(y ~ crf(x, model = "ecxlin")),
    retained_data = data.frame(cell = letters[1:4])),
    class = c("bayesnecfit", "bnecfit"))
  local_mocked_bindings(
    kfold_fit_settings = function(fit) c(iter = 300, warmup = 150),
    kfold_refit = function(...) stop("sampler stopped")
  )
  expect_warning(part <- kfold_groups(
    object, group = "cell", folds = c(a = 1L, b = 2L, c = 1L, d = 2L),
    fold = 1
  ), "Fold 1 of ecxlin failed: sampler stopped")
  expect_identical(part$results$ecxlin$error, "sampler stopped")
  expect_identical(part$results$ecxlin$diagnostics$error, "sampler stopped")
  expect_true(is.finite(part$results$ecxlin$diagnostics$seconds))
})

test_that("complete exact scores are selected and kept for model weights", {
  data <- data.frame(y = 1:4, x = c(1, 1, 2, 2), cell = letters[1:4])
  make_fit <- function() list(data = data, ranef = data.frame(group = character()),
                              criteria = list(loo = list()))
  set <- list(ecxlin = list(fit = make_fit()),
              ecxexp = list(fit = make_fit()))
  assignment <- c(a = 1L, b = 2L, c = 1L, d = 2L)
  for (m in names(set)) {
    set[[m]]$fit$criteria$kfold <- bayesnec:::kfold_criterion(
      c(a = -1, b = -2, c = -3, d = -4), "cell", assignment,
      list(iter = 100L), list(), "key")
  }
  out <- bayesnec:::weight_criteria(set, data$x, "kfold")
  expect_identical(out$leave_out, "kfold")
  expect_s3_class(out$criteria$ecxlin, "kfold")
  expect_identical(out$criteria, out$kfold)
  set$ecxexp$fit$criteria$kfold <- NULL
  expect_error(bayesnec:::weight_criteria(set, data$x, "kfold"),
               "required for every equation")
})

test_that("exact weights survive rebuilding and combining a stored set", {
  skip_on_cran()
  utils::data("manec_example", package = "bayesnec", envir = environment())
  fit <- manec_example
  n <- nrow(fit$mod_fits[[1]]$fit$data)
  levels <- paste0("row", seq_len(n))
  folds <- stats::setNames(rep(1:2, length.out = n), levels)
  data <- bayesnec:::kfold_set_data(fit, fit$mod_fits)
  criteria <- lapply(fit$mod_fits, function(o) {
    elpd <- stats::setNames(o$fit$criteria$loo$pointwise[, "elpd_loo"],
                            levels)
    bayesnec:::kfold_criterion(
      elpd, "row", folds, list(), list(),
      bayesnec:::kfold_fit_key(o$fit, data)
    )
  })
  weighted <- suppressWarnings(suppressMessages(kfold_weights(fit, criteria)))
  expect_s3_class(weighted, "bayesmanecfit")
  expect_identical(attr(weighted$mod_stats$wi, "leave_out"), "kfold")
  expect_identical(weighted$mod_fits[[1]]$fit$criteria$kfold,
                   criteria[[1]])
  amended <- suppressWarnings(suppressMessages(amend(
    weighted, loo_controls = list(weights = list(BB = FALSE))
  )))
  expect_identical(attr(amended$mod_stats$wi, "leave_out"), "kfold")
  extracted <- lapply(names(weighted$mod_fits), function(m) {
    suppressWarnings(suppressMessages(pull_out(weighted, model = m)))
  })
  combined <- suppressWarnings(suppressMessages(do.call(base::c, extracted)))
  expect_identical(attr(combined$mod_stats$wi, "leave_out"), "kfold")
  expect_identical(combined$mod_fits[[1]]$fit$criteria$kfold,
                   criteria[[1]])
})

test_that("two independent deviations are accepted and correlated ones refused", {
  re <- data.frame(id = c(1, 2), group = "cell",
                   nlpar = c("ogl", "ec50"), dpar = "",
                   coef = "Intercept", dist = "gaussian")
  slot <- bayesnec:::logo_slot(list(ranef = re), "cell")
  expect_length(slot$slots, 2)
  expect_null(slot$reason)
  re$id <- c(1, 1)
  expect_match(bayesnec:::logo_slot(list(ranef = re), "cell")$reason,
               "correlated deviations")
  re$id <- c(1, 2)
  re$coef[2] <- "slope"
  expect_match(bayesnec:::logo_slot(list(ranef = re), "cell")$reason,
               "normal intercept")
})

test_that("nested integration agrees with a fine grid for censored levels", {
  set.seed(8)
  n <- 8
  mu <- matrix(runif(n, -0.2, 0.2), n, 1)
  r1 <- matrix(rnorm(n, 0, 0.2), n, 1)
  r2 <- matrix(rnorm(n, 0, 0.2), n, 1)
  s1 <- runif(n, 0.2, 0.5)
  s2 <- runif(n, 0.2, 0.5)
  h <- function(z1, z2) {
    3 * stats::pnorm(0, mu + z1 + z2, 0.2, log.p = TRUE)
  }
  actual <- bayesnec:::logo_integrate(function(z1) {
    bayesnec:::logo_integrate(function(z2) h(z1, z2), r2, s2)
  }, r1, s1)
  reference <- vapply(seq_len(n), function(i) {
    z <- seq(-6, 6, by = 0.04)
    grid <- outer(z, z, function(a, b) {
      exp(3 * stats::pnorm(0, mu[i, 1] + s1[i] * a + s2[i] * b,
                           0.2, log.p = TRUE)) *
        stats::dnorm(a) * stats::dnorm(b)
    })
    log(sum(grid) * 0.04^2)
  }, numeric(1))
  expect_lt(max(abs(as.vector(actual) - reference)), 2e-3)
})

two_deviation_kfold_fixture <- local({
  fit <- NULL
  function() {
    if (is.null(fit)) {
      set.seed(100)
      data <- expand.grid(x = c(1, 2, 4), cell_rep = 1:4, obs = 1:3)
      data$cell <- factor(paste(data$x, data$cell_rep, sep = "_"))
      shift <- stats::setNames(stats::rnorm(nlevels(data$cell), 0, 2),
                               levels(data$cell))
      data$y <- 10 + 50 / (1 + (data$x / 2)^2) +
        shift[as.character(data$cell)] +
        stats::rnorm(nrow(data), 0, 1)
      fit <<- suppressWarnings(suppressMessages(bnec(
        y ~ crf(x, model = "ecx4param") + ogl(cell) + (ec50 | cell),
        data = data, family = gaussian(), chains = 2, iter = 300,
        warmup = 150, seed = 100, init = 0, refresh = 0
      )))
    }
    fit
  }
})

test_that("a fitted equation scores a fold with two cell deviations", {
  skip_unless_full_suite()
  fit <- two_deviation_kfold_fixture()
  part <- kfold_groups(fit, group = "cell", K = 2, fold = 1,
                       iter = 200, warmup = 100, seed = 11)
  expect_s3_class(part, "kfold_groups_part")
  expect_length(part$results$ecx4param$elpd, 6)
  expect_true(all(is.finite(part$results$ecx4param$elpd)))
  expect_true(is.finite(part$results$ecx4param$diagnostics$seconds))
  expect_null(part$results$ecx4param$error)
  level <- bayesnec:::logo_levels(fit$fit, "cell")
  logo <- bayesnec:::logo_criterion(fit$fit, "cell", level, chunk = 100)
  expect_s3_class(logo, "psis_loo")
  expect_identical(attr(logo, "levels"), levels(level))
  expect_true(all(is.finite(logo$pointwise[, "elpd_loo"])))
})
