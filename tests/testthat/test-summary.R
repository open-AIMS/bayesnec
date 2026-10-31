# Forward ECx arguments once, including replacement x_range or NULL
# for its default. The no-effect row keeps the fit-time grid and draws;
# apply xform to both kinds of estimate.

# Replaces ecx() with a recorder. Each call's arguments are kept as supplied,
# so an argument passed twice appears twice rather than stopping the call as
# the real generic does.
record_ecx_calls <- function(env = parent.frame()) {
  calls <- new.env()
  calls$ecx <- list()
  estimate <- c(Q50 = 1, Q2.5 = 0.5, Q97.5 = 2)
  local_mocked_bindings(
    ecx = function(object, ...) {
      calls$ecx[[length(calls$ecx) + 1]] <- list(...)
      estimate
    },
    .package = "bayesnec",
    .env = env
  )
  calls
}

n_named <- function(args, name) sum(names(args) == name)

# Disable posterior simulation only for the model-set method, whose
# formal check_fit consumes it. On single fits it would reach ecx()
# through dots. Muffle stored-fit convergence and censoring warnings.
summarise <- function(fit, ...) {
  if (inherits(fit, "bayesmanecfit")) {
    suppressWarnings(suppressMessages(summary(fit, ..., check_fit = FALSE)))
  } else {
    suppressWarnings(suppressMessages(summary(fit, ...)))
  }
}

stored_grid <- function(fit) {
  if (inherits(fit, "bayesmanecfit")) fit$w_pred_vals else fit$pred_vals
}

# Cut only the stored grid to x <= 1, distinct from the observed range
# 0.032 to 3.22. Mocked ecx() reads no other grid-derived quantity.
narrowed_grid <- function(fit) {
  grid <- stored_grid(fit)
  grid$data <- grid$data[grid$data$x <= 1, , drop = FALSE]
  if (inherits(fit, "bayesmanecfit")) {
    fit$w_pred_vals <- grid
  } else {
    fit$pred_vals <- grid
  }
  fit
}

test_that("a supplied x_range reaches each summary ECx call once (#439)", {
  for (fit in list(nec4param, manec_example)) {
    calls <- record_ecx_calls()
    out <- summarise(fit, ecx = TRUE, ecx_vals = c(10, 50),
                     x_range = c(0.5, 1), xform = exp, resolution = 50)
    expect_length(calls$ecx, 2)
    for (args in calls$ecx) {
      expect_equal(n_named(args, "x_range"), 1)
      expect_equal(args$x_range, c(0.5, 1))
      expect_identical(args$xform, exp)
      expect_equal(args$resolution, 50)
    }
    expect_equal(vapply(calls$ecx, `[[`, 0, "ecx_val"), c(10, 50))
    expect_named(out$ecs, c("ECx (10%) estimate:", "ECx (50%) estimate:"))
  }
})

test_that("without x_range the summary ECx rows use the stored grid", {
  for (fit in list(narrowed_grid(nec4param), narrowed_grid(manec_example))) {
    expected <- range(stored_grid(fit)$data$x)
    expect_lte(expected[2], 1)
    calls <- record_ecx_calls()
    summarise(fit, ecx = TRUE, ecx_vals = 50)
    expect_length(calls$ecx, 1)
    # Only the target and stored grid are forwarded by default.
    expect_setequal(names(calls$ecx[[1]]), c("ecx_val", "x_range"))
    expect_equal(calls$ecx[[1]]$x_range, expected)
    # resolution and xform reach ecx() beside the stored grid.
    calls <- record_ecx_calls()
    summarise(fit, ecx = TRUE, ecx_vals = 50, resolution = 50, xform = exp)
    args <- calls$ecx[[1]]
    expect_equal(n_named(args, "x_range"), 1)
    expect_equal(args$x_range, expected)
    expect_equal(args$resolution, 50)
    expect_identical(args$xform, exp)
  }
})

test_that("x_range = NULL leaves ecx() its own default, not the stored grid", {
  # As on the hurdle summary: the call reaches ecx() without an x_range, and
  # ecx() then applies its own default of NA and builds its grid from the
  # data.
  for (fit in list(narrowed_grid(nec4param), narrowed_grid(manec_example))) {
    calls <- record_ecx_calls()
    summarise(fit, ecx = TRUE, ecx_vals = 50, x_range = NULL, resolution = 50)
    expect_length(calls$ecx, 1)
    expect_equal(n_named(calls$ecx[[1]], "x_range"), 0)
    expect_equal(calls$ecx[[1]]$resolution, 50)
  }
})

test_that("an ecx_val given to summary() does not displace its arguments", {
  # ecx_val must reach the real generic beside the summary target and
  # raise a duplicate-argument error. Assert before mocking: partial
  # matching in the helper must not shift the remaining arguments.
  expect_error(summarise(nec4param, ecx = TRUE, ecx_val = 50),
               "matched by multiple actual arguments")
  for (fit in list(nec4param, manec_example)) {
    calls <- record_ecx_calls()
    summarise(fit, ecx = TRUE, ecx_val = 50)
    expect_length(calls$ecx, 3)
    for (args in calls$ecx) {
      expect_equal(n_named(args, "ecx_val"), 2)
      expect_equal(args$x_range, range(stored_grid(fit)$data$x))
      expect_equal(n_named(args, "resolution"), 0)
      expect_equal(sum(names(args) == ""), 0)
    }
  }
})

test_that("the xform for the no-effect row is matched as ecx() matches it", {
  # By name, by partial name and by position, with x_range named, supplied
  # or NULL, as ecx_row() passes it. A fourth positional argument is xform
  # where x_range is named in the call and x_range where it is not.
  xf <- bayesnec:::summary_ecx_xform
  expect_null(xf(list()))
  expect_identical(xf(list(xform = exp)), exp)
  expect_identical(xf(list(xfo = exp)), exp)
  expect_identical(xf(list(20, FALSE, "absolute", exp)), exp)
  expect_identical(xf(list(x_range = c(0.5, 1), 20, FALSE, "absolute", exp)),
                   exp)
  expect_identical(xf(list(x_range = NULL, 20, FALSE, "absolute", NA, exp)),
                   exp)
  expect_null(xf(list(x_range = NULL, 20, FALSE, "absolute", exp)))
})

test_that("x_range leaves the no-effect row as stored and xform transforms it", {
  # The no-effect row uses fit-time draws and censoring bounds, so
  # x_range does not change it. Apply xform even without ECx rows.
  for (fit in list(nec4param, manec_example)) {
    stored <- if (inherits(fit, "bayesmanecfit")) fit$w_ne else fit$ne
    by_nec <- suppressMessages(suppressWarnings(nec(fit, xform = exp)))
    calls <- record_ecx_calls()
    out <- summarise(fit, ecx = TRUE, ecx_vals = 50, x_range = c(0.5, 1))
    expect_identical(as.numeric(out$nec_vals), as.numeric(stored))
    calls <- record_ecx_calls()
    out <- summarise(fit, ecx = TRUE, ecx_vals = 50, x_range = c(0.5, 1),
                     xform = exp)
    expect_equal(as.numeric(out$nec_vals), as.numeric(by_nec))
    calls <- record_ecx_calls()
    out <- summarise(fit, x_range = c(0.5, 1), xform = exp)
    expect_length(calls$ecx, 0)
    expect_null(out$ecs)
    expect_equal(as.numeric(out$nec_vals), as.numeric(by_nec))
  }
})

test_that("the no-effect row is summarised as the stored row was", {
  # Identity transformation preserves the stored row. Include an ECx
  # fit whose stored no-effect row is an NSEC, which nec() refuses.
  ecx4 <- suppressMessages(pull_out(manec_example, "ecx4param"))
  ne_vals <- bayesnec:::summary_ne_vals
  for (fit in list(nec4param, manec_example, ecx4)) {
    set <- inherits(fit, "bayesmanecfit")
    stored <- if (set) fit$w_ne else fit$ne
    post <- if (set) fit$w_ne_posterior else fit$ne_posterior
    expect_identical(ne_vals(fit, identity), stored)
    expect_equal(as.numeric(ne_vals(fit, function(z) z)), as.numeric(stored))
    expect_equal(as.numeric(ne_vals(fit, exp)),
                 as.numeric(bayesnec:::estimates_summary(exp(post))))
  }
  expect_error(ne_vals(nec4param, "exp"), "xform must be a function")
})

test_that("posterior = TRUE is refused rather than printed (#439)", {
  # A summary prints quantiles; with posterior = TRUE the estimators return
  # draws, and the table showed columns of NA with no censoring note.
  for (fit in list(nec4param, manec_example)) {
    expect_error(summarise(fit, ecx = TRUE, ecx_vals = 50, posterior = TRUE),
                 "posterior sample")
    # By position, as ecx() reads it: resolution, then posterior.
    expect_error(summarise(fit, 20, TRUE, ecx = TRUE, ecx_vals = 50),
                 "posterior sample")
    expect_error(summarise(fit, posterior = TRUE), "posterior sample")
    expect_no_error(summarise(fit, posterior = FALSE))
  }
})

test_that("each summary ECx row equals a bare ecx() call (#439)", {
  skip_on_cran()
  # Both fixtures have EC50 above the supplied upper bound 1. Report
  # that censoring on the transformed scale: exp(1), approximately 2.72.
  for (fit in list(nec4param, manec_example)) {
    s <- summarise(fit, ecx = TRUE, ecx_vals = 50, x_range = c(0.5, 1),
                   resolution = 50, xform = exp)
    e <- suppressMessages(suppressWarnings(
      ecx(fit, ecx_val = 50, x_range = c(0.5, 1), resolution = 50,
          xform = exp)
    ))
    expect_identical(s$ecs[[1]], e)
    expect_equal(attr(s$ecs[[1]], "resolution"), 50)
    expect_equal(attr(e, "censored_summary")$bound, rep(">=", 3))
    printed <- utils::capture.output(suppressWarnings(print(s)))
    expect_true(any(grepl("Estimate >= 2.72 >= 2.72 >= 2.72", printed,
                          fixed = TRUE)))
    expect_true(any(grepl("draws of the ECx (50%) lie above 2.72", printed,
                          fixed = TRUE)))
  }
})

# Replaces nuts_params() so that each fit of manec_example reports the count
# named for its model in `counts`. An unnamed count is given to every fit,
# including a pulled-out one, which is not identical to the stored fit.
local_divergences <- function(counts, env = parent.frame()) {
  fits <- lapply(manec_example$mod_fits, "[[", "fit")
  local_mocked_bindings(
    nuts_params = function(object, pars = NULL, ...) {
      k <- if (is.null(names(counts))) {
        counts
      } else {
        counts[[names(fits)[vapply(fits, identical, logical(1), object)]]]
      }
      data.frame(Chain = 1L, Iteration = seq_len(100),
                 Parameter = factor("divergent__"),
                 Value = rep(c(1, 0), c(k, 100 - k)))
    },
    .package = "bayesnec",
    .env = env
  )
}

# The printed lines of a summary and the warnings its print() raised.
print_summary <- function(s) {
  w <- NULL
  printed <- utils::capture.output(w <- collect_warnings(print(s)))
  list(printed = printed, warnings = w$warnings)
}

# The warning line a model set's summary gives an equation over the cutoff.
flagged_line <- function(s, model, n) {
  sprintf("%s (%s, model weight %.2f)", model, n, s$mod_weights[model, "wi"])
}

test_that("summary() defaults to the divergence cutoff of check_sampling()", {
  cutoff <- formals(check_sampling)$divergence_cutoff
  for (cls in c("bayesnecfit", "bayesmanecfit")) {
    expect_identical(formals(getS3method("summary", cls))$divergence_cutoff,
                     cutoff)
  }
  expect_identical(formals(screen_models)$divergence_cutoff, cutoff)
})

test_that("summary() reports the divergences of every model in a set", {
  local_divergences(c(nec4param = 3, ecx4param = 0))
  s <- summarise(manec_example, rhat_cutoff = 99)
  expect_equal(s$divergences, c(nec4param = 3, ecx4param = 0))
  expect_equal(s$divergence_cutoff, 10)
  out <- print_summary(s)
  i <- grep("Divergent transitions after warmup", out$printed, fixed = TRUE)
  expect_length(i, 1)
  expect_match(out$printed[i + 1], "n_divergent", fixed = TRUE)
  expect_match(out$printed[i + 2], "^nec4param +3$")
  expect_match(out$printed[i + 3], "^ecx4param +0$")
  expect_length(out$warnings, 0)
})

test_that("a model over the divergence cutoff is named in the summary warning", {
  local_divergences(c(nec4param = 3, ecx4param = 14))
  s <- summarise(manec_example, rhat_cutoff = 99)
  out <- print_summary(s)
  expect_length(out$warnings, 1)
  expect_match(out$warnings, "Divergent transitions > 10", fixed = TRUE)
  # A literal weight, so that a lookup returning the wrong equation's weight
  # fails here.
  expect_match(out$warnings, paste0(
    "(see ?check_sampling):\n  -  ecx4param (14, model weight 0.17)\n",
    "Consider dropping"
  ), fixed = TRUE)
  expect_no_match(out$warnings, "nec4param", fixed = TRUE)
  expect_match(out$warnings, "screen_models", fixed = TRUE)
})

test_that("the summary flags the models check_sampling() fails on divergences", {
  # 10 is not flagged and 11 is: both compare with >.
  local_divergences(c(nec4param = 10, ecx4param = 11))
  s <- summarise(manec_example, rhat_cutoff = 99)
  w <- print_summary(s)$warnings
  expect_match(w, flagged_line(s, "ecx4param", 11), fixed = TRUE)
  expect_no_match(w, "nec4param", fixed = TRUE)
  tab <- check_sampling(manec_example, rhat_cutoff = 99, ess_cutoff = 0)
  expect_equal(tab$model[tab$failed], "ecx4param")
})

test_that("summary() honours a supplied divergence cutoff", {
  local_divergences(c(nec4param = 3, ecx4param = 14))
  s <- summarise(manec_example, rhat_cutoff = 99, divergence_cutoff = 2)
  expect_equal(s$divergence_cutoff, 2)
  w <- print_summary(s)$warnings
  expect_match(w, "Divergent transitions > 2", fixed = TRUE)
  expect_match(w, flagged_line(s, "nec4param", 3), fixed = TRUE)
  expect_match(w, flagged_line(s, "ecx4param", 14), fixed = TRUE)
  expect_error(summary(manec_example, divergence_cutoff = c(1, 2)))
  expect_error(summary(nec4param, divergence_cutoff = "10"))
})

test_that("a single fit reports its divergences under the cutoff", {
  local_divergences(4)
  s <- summarise(nec4param)
  expect_equal(s$divergences, c(nec4param = 4))
  expect_equal(s$divergence_cutoff, 10)
  out <- print_summary(s)
  expect_true("Divergent transitions after warmup: 4" %in% out$printed)
  expect_length(out$warnings, 0)
  s <- summarise(nec4param, divergence_cutoff = 3)
  expect_equal(s$divergence_cutoff, 3)
  expect_match(print_summary(s)$warnings, "nec4param (4)", fixed = TRUE)
})

test_that("a single fit over the divergence cutoff is flagged", {
  local_divergences(12)
  out <- print_summary(summarise(nec4param))
  expect_true("Divergent transitions after warmup: 12" %in% out$printed)
  expect_length(out$warnings, 1)
  expect_match(out$warnings, "nec4param (12)", fixed = TRUE)
  expect_match(out$warnings, "adapt_delta", fixed = TRUE)
})

test_that("a divergence failure beside a control mis-fit gives both advices", {
  local_divergences(c(nec4param = 3, ecx4param = 14))
  s <- summarise(manec_example, rhat_cutoff = 99)
  s$fit_issues <- list(nec4param = TRUE, ecx4param = FALSE)
  w <- print_summary(s)$warnings
  expect_length(w, 1)
  expect_match(w, flagged_line(s, "ecx4param", 14), fixed = TRUE)
  expect_match(w, "Sampler failures should be dropped", fixed = TRUE)
  expect_match(w, "inspect it with check_fit()", fixed = TRUE)
})

test_that("a grouped summary reports divergences at each level", {
  local_divergences(c(nec4param = 3, ecx4param = 14))
  grouped <- structure(
    list(fits = list(a = manec_example, b = manec_example),
         levels = c("a", "b")),
    class = c("bayesnecgroupfit", "bnecfit")
  )
  s <- suppressWarnings(suppressMessages(
    summary(grouped, check_fit = FALSE, rhat_cutoff = 99,
            divergence_cutoff = 5)
  ))
  expect_named(s, c("a", "b"))
  for (lvl in s) {
    expect_equal(lvl$divergences, c(nec4param = 3, ecx4param = 14))
    expect_equal(lvl$divergence_cutoff, 5)
  }
})

test_that("a summary stored before divergences were recorded still prints", {
  s <- summarise(manec_example, rhat_cutoff = 99)
  s$divergences <- NULL
  s$divergence_cutoff <- NULL
  out <- print_summary(s)
  expect_false(any(grepl("Divergent", out$printed)))
  expect_length(out$warnings, 0)
  single <- summarise(nec4param)
  single$divergences <- NULL
  single$divergence_cutoff <- NULL
  out <- print_summary(single)
  expect_false(any(grepl("Divergent", out$printed)))
  expect_length(out$warnings, 0)
})

test_that("a model not sampled by NUTS reports NA and is not flagged", {
  # nuts_params() stops on a variational fit, so it is not called on one.
  vb <- manec_example
  vb$mod_fits$ecx4param$fit$algorithm <- "meanfield"
  s <- summarise(vb, rhat_cutoff = 99)
  expect_true(is.na(s$divergences[["ecx4param"]]))
  expect_equal(s$divergences[["nec4param"]], 0)
  out <- print_summary(s)
  expect_length(out$warnings, 0)
  single <- nec4param
  single$fit$algorithm <- "meanfield"
  out <- print_summary(summarise(single))
  expect_true("Divergent transitions after warmup: NA" %in% out$printed)
  expect_length(out$warnings, 0)
})
