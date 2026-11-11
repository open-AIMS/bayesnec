test_that("only groupings observed across concentrations enter the curve", {
  d <- data.frame(x = rep(c(0, 1, 2), each = 4), y = 1,
                  plate = rep(c("a", "b"), 6),
                  cell = seq_len(12))
  fit <- nec4param
  fit$fit$data <- d
  fit$fit$formula <- list(
    formula = y ~ mu,
    pforms = list(beta = beta ~ 1 + (1 | plate),
                  ogl = ogl ~ 1 + (1 | cell))
  )
  spec <- bayesnec:::observed_level_spec(fit)
  expect_equal(nrow(spec$combinations), 2L)
  expect_identical(names(spec$combinations), "plate")
  expect_equal(spec$n, c(6L, 6L))
  expect_match(deparse1(spec$re_formula), "plate")
  expect_false(grepl("cell", deparse1(spec$re_formula)))
})

test_that("a grouping with incomplete series still spans the series", {
  d <- data.frame(x = c(0, 1, 0, 2, 1), y = 1,
                  batch = c("a", "a", "b", "b", "c"))
  fit <- nec4param
  fit$fit$data <- d
  fit$fit$formula <- list(formula = y ~ mu,
                          pforms = list(beta = beta ~ 1 + (1 | batch)))
  spec <- bayesnec:::observed_level_spec(fit)
  expect_equal(spec$n, c(2L, 2L))
  expect_equal(vapply(spec$x, length, integer(1)), spec$n)
  expect_equal(spec$excluded, "batch=c")
  fit$fit$data <- d[d$batch == "c", , drop = FALSE]
  expect_null(bayesnec:::observed_level_spec(fit))
})

test_that("excluded-level notes occur once across equations", {
  spec <- list(excluded = "plate=c")
  expect_message(
    bayesnec:::once_level_note({
      bayesnec:::note_excluded_levels(spec)
      bayesnec:::note_excluded_levels(spec)
    }),
    "plate=c"
  )
  messages <- character()
  withCallingHandlers(bayesnec:::once_level_note({
    bayesnec:::note_excluded_levels(spec)
    bayesnec:::note_excluded_levels(spec)
  }), message = function(m) {
    messages <<- c(messages, conditionMessage(m))
    invokeRestart("muffleMessage")
  })
  expect_length(messages, 1L)
})

test_that("NULL pool selects separate grouped and joint levels", {
  simple <- function(fit, ...) c(Q50 = fit, Q2.5 = fit, Q97.5 = fit)
  grouped <- list(fits = list(a = 1, b = 2), levels = c("a", "b"))
  g <- bayesnec:::group_estimate_table(grouped, "nec", simple, pool = NULL)
  expect_equal(g$Q50, c(1, 2))
  local_mocked_bindings(
    joint_level_fit = function(object, level) match(level, object$levels),
    .package = "bayesnec"
  )
  joint <- list(levels = c("a", "b"))
  j <- bayesnec:::joint_estimate_table(joint, "nec", simple, pool = NULL)
  expect_equal(j$Q50, c(1, 2))
})

test_that("a mixture gives each posterior draw to one level", {
  idx <- bayesnec:::mixture_draw_indices(c(10, 10, 10), c(1, 1, 2), 10)
  expect_equal(sort(unlist(idx)), 1:10)
  expect_equal(lengths(idx), c(3L, 2L, 5L))
  expect_error(bayesnec:::mixture_draw_indices(c(5, 10), c(1, 1), 8),
               "shortest level")
})

# Two metals tested in three shared batches, each on plates of its own, as one
# level of a mocked joint refit. `cu` and `zn` give each metal's replicate
# count per plate and batch.
shared_batch_level <- function(level, cu = c(2, 2, 2), zn = c(2, 2, 2),
                               zn_batches = c("b1", "b2", "b3"),
                               zn_plates = 1) {
  rows <- function(tox, batch, plate, reps) {
    expand.grid(x = c(0, 0.5, 1, 2), rep = seq_len(reps), tox = tox,
                batch = batch, plate = plate, stringsAsFactors = FALSE)
  }
  d <- do.call(rbind, c(
    Map(function(b, r) rbind(rows("Cu", b, paste("Cu", b, 1), r),
                             rows("Cu", b, paste("Cu", b, 2), r)),
        c("b1", "b2", "b3"), cu),
    Map(function(b, r) {
      do.call(rbind, lapply(seq_len(zn_plates), function(p) {
        rows("Zn", b, paste("Zn", b, p), r)
      }))
    }, zn_batches, zn)
  ))
  d$y <- 1
  d$tox <- factor(d$tox)
  fit <- nec4param
  fit$fit$data <- d
  fit$fit$formula <- list(
    formula = y ~ mu,
    pforms = list(beta = beta ~ 1 + (1 | gr(plate, by = tox)) + (1 | batch))
  )
  fit$group_var <- "tox"
  fit$levels <- c("Cu", "Zn")
  fit$level <- level
  fit$level_spec <- list()
  bayesnec:::allot_class(fit, c("bayesnecjointlevel", "bayesnecfit",
                                "bnecfit"))
}

# The batch read for each metal at each of n draws.
batch_draws <- function(n = 4000, ...) {
  lapply(c(Cu = "Cu", Zn = "Zn"), function(level) {
    spec <- bayesnec:::observed_level_spec(shared_batch_level(level, ...))
    spec$combinations$batch[bayesnec:::observed_assignment(spec, n)]
  })
}

test_that("a shared group term is matched and an unshared one is not", {
  spec <- bayesnec:::observed_level_spec(shared_batch_level("Cu"))
  expect_named(spec$terms, c("plate:tox", "batch"))
  keys <- spec$coupling$keys
  expect_equal(keys[[1]], spec$terms$batch)
  expect_equal(keys[[2]], c("batch=b1", "batch=b2", "batch=b3"))
  expect_equal(spec$coupling$which, 1L)
  expect_equal(bayesnec:::group_term_vars(quote(batch / plate)),
               list("batch", c("batch", "plate")))
})

test_that("levels with the same counts on a shared term match at every draw", {
  # Copper has two plates in each batch and zinc one, so their combinations
  # differ but their proportions over the batches agree.
  batch <- batch_draws()
  expect_true(all(batch$Cu == batch$Zn))
  three <- bayesnec:::coupled_draw_assignment(
    1000, rep(list(c(3, 1, 2)), 3), rep(list(c("a", "b", "c")), 3)
  )
  expect_identical(three[[1]], three[[2]])
  expect_identical(three[[1]], three[[3]])
})

test_that("unequal counts match in the largest share they allow", {
  n <- 4000
  batch <- batch_draws(n, zn = c(6, 1, 1))
  bound <- sum(pmin(c(1, 1, 1) / 3, c(6, 1, 1) / 8))
  expect_lte(abs(sum(batch$Cu == batch$Zn) - n * bound), 1)
  # Random pairing of the same shares would match in about a third of draws.
  expect_gt(mean(batch$Cu == batch$Zn), 0.5)
  partial <- batch_draws(n, zn = c(2, 2), zn_batches = c("b1", "b4"))
  expect_lte(abs(sum(partial$Cu == partial$Zn) - n / 3), 1)
})

test_that("unmatched draws are paired independently of their labels", {
  # Copper in b1 to b3 and zinc in b1, b4 and b5: only b1 can be matched, and
  # copper's b2 and b3 meet zinc's b4 and b5 at random.
  n <- 6000
  cu <- c("batch=b1", "batch=b2", "batch=b3")
  zn <- c("batch=b1", "batch=b4", "batch=b5")
  a <- bayesnec:::coupled_draw_assignment(n, list(c(1, 1, 1), c(1, 1, 1)),
                                          list(cu, zn))
  pair <- table(cu[a[[1]]], zn[a[[2]]]) / n
  expect_lte(abs(pair["batch=b1", "batch=b1"] - 1 / 3), 1 / n)
  off <- pair[c("batch=b2", "batch=b3"), c("batch=b4", "batch=b5")]
  expect_equal(as.numeric(off), rep(1 / 6, 4), tolerance = 0.1)
})

test_that("a level without a shared value does not stop the others matching", {
  n <- 4000
  spec <- function(batches, counts, metal) {
    list(n = counts, terms = list(
      "plate:tox" = paste0("plate=", metal, seq_along(counts), ", tox=", metal),
      batch = paste0("batch=", batches)
    ))
  }
  matched <- function(specs) {
    keys <- bayesnec:::shared_term_keys(specs)
    a <- bayesnec:::coupled_draw_assignment(n, lapply(specs, `[[`, "n"),
                                            keys)
    batch <- lapply(1:2, function(j) specs[[j]]$terms$batch[a[[j]]])
    sum(batch[[1]] == batch[[2]])
  }
  no_batch <- list(n = c(1, 1), terms = list(
    "plate:tox" = c("plate=Ni1, tox=Ni", "plate=Ni2, tox=Ni")
  ))
  other_batches <- spec(c("b4", "b5"), c(1, 1), "Ni")
  partial <- list(spec(c("b1", "b2"), c(1, 1), "Cu"),
                  spec(c("b2", "b3"), c(1, 1), "Zn"))
  for (third in list(NULL, no_batch, other_batches)) {
    expect_lte(abs(matched(c(partial, list(third))) - n / 2), 1)
  }
  unequal <- list(spec(c("b1", "b2", "b3"), c(2, 2, 4), "Cu"),
                  spec(c("b1", "b2", "b3"), c(3, 3, 4), "Zn"))
  bound <- sum(pmin(c(2, 2, 4) / 8, c(3, 3, 4) / 10))
  for (third in list(NULL, no_batch, other_batches)) {
    expect_lte(abs(matched(c(unequal, list(third))) - n * bound), 1)
  }
})

test_that("matching leaves each level's own shares unchanged", {
  n <- 4000
  for (zn in list(c(2, 2, 2), c(6, 1, 1))) {
    for (level in c("Cu", "Zn")) {
      spec <- bayesnec:::observed_level_spec(
        shared_batch_level(level, zn = zn)
      )
      # Counts are rounded from the shares, to within two draws.
      counts <- tabulate(bayesnec:::observed_assignment(spec, n),
                         nrow(spec$combinations))
      expect_lte(max(abs(counts - n * spec$n / sum(spec$n))), 2)
      alone <- spec
      alone$coupling <- NULL
      alone_counts <- tabulate(bayesnec:::observed_assignment(alone, n),
                               nrow(spec$combinations))
      expect_lte(max(abs(counts - alone_counts)), 2)
    }
  }
})

test_that("plates the levels do not share are chosen independently", {
  n <- 20000
  plates <- lapply(c(Cu = "Cu", Zn = "Zn"), function(level) {
    spec <- bayesnec:::observed_level_spec(
      shared_batch_level(level, zn_plates = 2)
    )
    spec$combinations$plate[bayesnec:::observed_assignment(spec, n)]
  })
  batch <- lapply(plates, function(p) sub("^.. (b[0-9]) [0-9]$", "\\1", p))
  expect_true(all(batch$Cu == batch$Zn))
  in_b1 <- batch$Cu == "b1"
  pair <- table(plates$Cu[in_b1], plates$Zn[in_b1]) / sum(in_b1)
  expect_equal(as.numeric(pair), rep(0.25, 4), tolerance = 0.02)
})

test_that("the matched assignment repeats and restores the RNG state", {
  spec <- bayesnec:::observed_level_spec(shared_batch_level("Zn", zn = c(6, 1, 1)))
  set.seed(9)
  before <- .Random.seed
  first <- bayesnec:::observed_assignment(spec, 500)
  expect_identical(.Random.seed, before)
  set.seed(10)
  expect_identical(bayesnec:::observed_assignment(spec, 500), first)
})

test_that("a label that is not valid UTF-8 does not stop the mixture", {
  bad <- "plate=\xff"
  out <- bayesnec:::pool_estimate_draws(
    stats::setNames(list(1:10, 11:20), c(bad, "plate=b")), c(1, 1),
    "mixture"
  )
  expect_length(out, 10L)
  keys <- list(c(bad, "plate=b"), c(bad, "plate=c"))
  matched <- bayesnec:::coupled_draw_assignment(10, list(c(1, 1), c(1, 1)),
                                                keys)
  expect_equal(sum(keys[[1]][matched[[1]]] == keys[[2]][matched[[2]]]), 5L)
})

test_that("a mixture holds draw d of its level at position d", {
  n <- 12L
  out <- bayesnec:::pool_estimate_draws(
    list(a = 100 + seq_len(n), b = 200 + seq_len(n)), c(1, 2), "mixture"
  )
  expect_equal(attr(out, "draw_id"), seq_len(n))
  expect_equal(as.numeric(out) %% 100, seq_len(n))
  expect_equal(as.numeric(out) %/% 100,
               ifelse(attr(out, "level") == "a", 1, 2))
  expect_equal(as.integer(table(attr(out, "level"))), c(4L, 8L))
})

test_that("mixtures over different levels of one posterior pair by draw", {
  # As the copper and zinc plates of one joint refit: every plate of a metal
  # shares that metal's value at draw d.
  n <- 40L
  cu <- stats::setNames(rep(list(seq_len(n)), 3), paste0("cu", 1:3))
  zn <- stats::setNames(rep(list(2 * seq_len(n)), 4), paste0("zn", 1:4))
  mix_cu <- bayesnec:::pool_estimate_draws(cu, c(1, 1, 1), "mixture")
  mix_zn <- bayesnec:::pool_estimate_draws(zn, c(1, 2, 1, 1), "mixture")
  expect_equal(as.numeric(mix_zn) / as.numeric(mix_cu), rep(2, n))
})

test_that("a mixture samples the full posterior when lengths differ", {
  draws <- list(a = 1:4, b = 101:108)
  out <- bayesnec:::pool_estimate_draws(draws, c(1, 3), "mixture", seed = 1)
  level <- attr(out, "level")
  draw_id <- attr(out, "draw_id")
  expect_true(any(draw_id[level == "b"] > 4L))
  expect_equal(as.numeric(out[level == "a"]), draws$a[draw_id[level == "a"]])
  expect_equal(as.numeric(out[level == "b"]), draws$b[draw_id[level == "b"]])
  expect_identical(out, bayesnec:::pool_estimate_draws(
    draws, c(1, 3), "mixture", seed = 1
  ))
})

test_that("a mixture keeps each censoring flag with its draw", {
  a <- c(1, 2, NA_real_, 4)
  attr(a, "censored") <- bayesnec:::censoring_record(
    3, 0, c(FALSE, FALSE, TRUE, FALSE), rep(FALSE, 4))
  b <- c(1.5, 2.5, 2.8, 2.9)
  out <- bayesnec:::pool_estimate_draws(list(a = a, b = b), c(1, 1),
                                        "mixture")
  expect_equal(attr(out, "censored")$above, is.na(out) | out > 3)
  from_a <- attr(out, "level") == "a"
  expect_equal(as.numeric(out)[from_a], a[from_a])
})

test_that("a numeric control changes the reference and rejects log zero", {
  skip_if(Sys.getenv("NOT_CRAN") == "")
  fit <- suppressMessages(pull_out(manec_example, model = "ecx4param"))
  low <- bayesnec:::control_x(fit)
  expect_equal(ecx(fit, x_control = low), ecx(fit))
  expect_equal(nsec(fit, x_control = low), nsec(fit))
  expect_warning(nsec(fit, x_control = 0),
                 "below the lowest observed concentration")
  logged <- fit
  logged$bayesnecformula <- bayesnecformula(
    y ~ crf(log(x), model = "ecx4param")
  )
  expect_error(nsec(logged, x_control = 0),
               "not finite under the crf\\(\\) predictor transformation")
})

test_that("a shared control uses the same grid for grouped levels", {
  fit <- suppressMessages(pull_out(manec_example, model = "ecx4param"))
  other <- fit
  other$fit$data$x <- other$fit$data$x + 0.1
  fits <- list(a = fit, b = other)
  expect_warning(
    dots <- bayesnec:::shared_level_control(fits, list(x_control = "shared")),
    "1 level\\(s\\)"
  )
  expect_equal(as.numeric(dots$x_control), bayesnec:::control_x(fit))
  expect_equal(dots$x_range[2], max(other$fit$data$x))
})

test_that("mean_curve reads the average curve before finding ECx", {
  fits <- list(a = list(label = "a"), b = list(label = "b"))
  local_mocked_bindings(
    level_x_bounds = function(object) c(0, 2),
    level_curve_draws = function(object, x_vec, ...) {
      if (object$label == "a") {
        list(curve = rbind(c(10, 9, 8), c(10, 9, 8)),
             control = c(10, 10), asymptote = 0)
      } else {
        list(curve = rbind(c(20, 15, 10), c(20, 15, 10)),
             control = c(20, 20), asymptote = 0)
      }
    },
    returns_recorded = function(...) TRUE,
    .package = "bayesnec"
  )
  out <- bayesnec:::mean_curve_estimate(
    fits, c(1, 1), "ecx",
    list(ecx_val = 10, resolution = 3, posterior = FALSE)
  )
  expect_equal(unname(as.numeric(out["Q50"])), 0.5)
  mean_estimate <- bayesnec:::pool_estimate_draws(
    list(a = c(1, 1), b = c(0.4, 0.4)), c(1, 1), "mean_estimate"
  )
  expect_equal(mean_estimate, c(0.7, 0.7))
})

test_that("mean_curve NSEC starts at the latest own control", {
  fits <- list(a = list(label = "a"), b = list(label = "b"))
  local_mocked_bindings(
    level_x_bounds = function(object) {
      if (object$label == "a") c(1, 3) else c(2, 3)
    },
    level_curve_draws = function(object, x_vec, ...) {
      list(curve = matrix(rep(c(10, 9, 8), each = 2), nrow = 2),
           control = c(10, 10), asymptote = 0)
    },
    returns_recorded = function(...) TRUE,
    .package = "bayesnec"
  )
  out <- bayesnec:::mean_curve_estimate(
    fits, c(1, 1), "nsec",
    list(x_range = c(0, 3), resolution = 3, posterior = TRUE)
  )
  expect_equal(as.numeric(out), c(2, 2))
  expect_equal(attr(out, "censored")$lower, 2)
})

test_that("mean_curve NSEC censors crossings before the common range", {
  fits <- list(a = list(label = "a"), b = list(label = "b"))
  local_mocked_bindings(
    level_x_bounds = function(object) {
      if (object$label == "a") c(1, 3) else c(2, 3)
    },
    level_curve_draws = function(object, x_vec, ...) {
      second <- if (object$label == "a") c(0, 0, 0) else c(20, 19, 18)
      list(curve = rbind(c(10, 9, 8), second),
           control = c(10, 20), asymptote = 0)
    },
    returns_recorded = function(...) TRUE,
    .package = "bayesnec"
  )
  out <- bayesnec:::mean_curve_estimate(
    fits, c(1, 1), "nsec",
    list(resolution = 3, posterior = TRUE)
  )
  expect_true(is.na(out[2]))
  expect_true(attr(out, "censored")$below[2])
  expect_equal(attr(out, "censored")$lower, 2)
})

test_that("mean_curve NSEC reads a finite extrapolation limit", {
  fits <- list(a = list(), b = list())
  local_mocked_bindings(
    level_x_bounds = function(object) c(0, 3),
    level_curve_draws = function(object, x_vec, ...) {
      list(curve = rbind(10 - 0.5 * x_vec, 12 - 0.5 * x_vec),
           control = c(10, 12), asymptote = 0)
    },
    returns_recorded = function(...) TRUE,
    .package = "bayesnec"
  )
  estimate <- function(extrapolate) bayesnec:::mean_curve_estimate(
    fits, c(1, 1), "nsec",
    list(resolution = 21, posterior = TRUE, extrapolate = extrapolate)
  )
  tested <- estimate(FALSE)
  extended <- estimate(5)
  expect_true(is.na(tested[2]))
  expect_true(is.finite(extended[2]))
  expect_gt(extended[2], 3)
  expect_equal(attr(extended, "censored")$upper, 5)
})

test_that("mean_curve refuses relative effects on a combined hurdle", {
  fits <- list(a = list(fit = list(family = brms::hurdle_poisson())))
  local_mocked_bindings(
    level_x_bounds = function(object) c(0, 3),
    .package = "bayesnec"
  )
  expect_error(
    bayesnec:::mean_curve_estimate(
      fits, 1, "ecx", list(type = "relative", resolution = 3)
    ),
    "combined curve"
  )
  expect_error(
    bayesnec:::mean_curve_estimate(
      fits, 1, "ecnsec",
      list(type = "relative", nsec = 1, resolution = 3)
    ),
    "combined curve"
  )
})

test_that("saved population displays identify their earlier prediction basis", {
  fit <- nec4param
  fit$pred_vals <- list(data = data.frame(x = c(0, 1)))
  local_mocked_bindings(
    observed_level_spec = function(object) list(combinations = 1),
    .package = "bayesnec"
  )
  expect_warning(bayesnec:::warn_stale_observed_display(fit),
                 "no-effect summary describes the population curve")
  fit$pred_vals$observed_levels <- TRUE
  expect_warning(bayesnec:::warn_stale_observed_display(fit),
                 "band mixes the curves of the observed levels")
  fit$pred_vals$band <- "population"
  expect_silent(bayesnec:::warn_stale_observed_display(fit))
})

test_that("pooled observed-level ECx retains the value used by plot labels", {
  spec <- list(combinations = data.frame(plate = c("a", "b")),
               n = c(2L, 2L), x = list(c(0, 1), c(0, 1)),
               excluded = character())
  local_mocked_bindings(
    observed_level_fit = function(object, spec, i, with_ne = FALSE) i,
    .package = "bayesnec"
  )
  estimate <- bayesnec:::estimate_observed_levels(
    list(), spec, "ecx", function(level, control, range) rep(0.5, 4),
    pool = "mixture", posterior = FALSE,
    prob_vals = c(0.5, 0.025, 0.975),
    curve_args = list(ecx_val = 10, resolution = 20)
  )
  plotted <- bayesnec:::bind_ecx(data.frame(x = 1:3), estimate)
  expect_equal(plotted$ecx_int[4], 10)
  expect_equal(attr(estimate, "resolution"), 20)
})

# Two plates whose lowest concentration is 0.05, on a fit that records a
# control of 0.01. `read_at` returns the control each level was read at.
recorded_control_levels <- function() {
  fit <- ecx4param
  fit$call_args <- list(x_control = 0.01)
  spec <- list(combinations = data.frame(plate = c("p1", "p2")),
               x = list(c(0.05, 0.4, 1), c(0.05, 0.4, 1)),
               n = c(3L, 3L), re_formula = NA)
  read_at <- function(level, control, range) {
    rep(bayesnec:::resolve_x_control(level, control, range)$value, 10)
  }
  list(fit = fit, spec = spec, read_at = read_at)
}

test_that("observed levels read and label a recorded control", {
  case <- recorded_control_levels()
  run <- function(x_control) {
    bayesnec:::estimate_observed_levels(
      case$fit, case$spec, "nsec", case$read_at, pool = "none",
      posterior = TRUE, prob_vals = c(0.5, 0.025, 0.975),
      x_control = x_control
    )
  }
  own <- run("own")
  expect_equal(unlist(own, use.names = FALSE), rep(0.01, 20))
  expect_equal(unname(attr(own, "x_control")), c(0.01, 0.01))
  shared <- run("shared")
  expect_equal(unlist(shared, use.names = FALSE), rep(0.01, 20))
  expect_equal(unname(attr(shared, "x_control")), c(0.01, 0.01))
})

test_that("a call-time control below the levels warns of extrapolation", {
  case <- recorded_control_levels()
  run <- function(x_control) {
    bayesnec:::estimate_observed_levels(
      case$fit, case$spec, "nsec", case$read_at, pool = "none",
      posterior = TRUE, prob_vals = c(0.5, 0.025, 0.975),
      x_control = x_control
    )
  }
  expect_warning(run(0.02),
                 "0.02, is below the lowest observation of 2 level\\(s\\)")
  # The recorded control was reported at fitting.
  expect_no_warning(run("shared"))
})

test_that("an observed level's nec censoring starts at the recorded control", {
  spec <- list(combinations = data.frame(plate = c("p1", "p2")),
               x = list(c(0.05, 0.4, 1), c(0.05, 0.4, 1)),
               n = c(3L, 3L), re_formula = NA)
  fit <- nec4param
  plain <- bayesnec:::observed_level_fit(fit, spec, 1, with_ne = TRUE)
  expect_equal(attr(plain$ne_posterior, "censored")$lower, 0.05)
  fit$call_args <- list(x_control = 0.01)
  level <- bayesnec:::observed_level_fit(fit, spec, 1, with_ne = TRUE)
  expect_equal(attr(level$ne_posterior, "censored")$lower, 0.01)
  expect_equal(attr(level$ne_posterior, "censored")$upper, 1)
})

test_that("an observed hurdle level's grid starts at the recorded control", {
  spec <- list(combinations = data.frame(plate = c("p1", "p2")),
               x = list(c(0.05, 0.4, 1), c(0.1, 0.4, 2)),
               n = c(3L, 3L), re_formula = NA)
  pair <- bayesnec:::allot_class(
    list(growth = nec4param, survival = nec4param),
    c("bayesnechurdlefit", "bnecfit")
  )
  ranges <- list()
  local_mocked_bindings(
    observed_level_spec = function(object) spec,
    newdata_eval = function(object, resolution, x_range) {
      ranges[[length(ranges) + 1]] <<- x_range
      stop("grid built")
    }
  )
  expect_error(bayesnec:::hurdle_component_preds(pair, resolution = 5),
               "grid built")
  expect_equal(ranges[[1]], c(0.05, 2))
  pair$survival$call_args <- list(x_control = 0.01)
  expect_error(bayesnec:::hurdle_component_preds(pair, resolution = 5),
               "grid built")
  expect_equal(ranges[[2]], c(0.01, 2))
  level <- bayesnec:::observed_level_fit(pair, spec, 2)
  expect_error(bayesnec:::hurdle_component_preds(level, resolution = 5),
               "grid built")
  expect_equal(ranges[[3]], c(0.01, 2))
})

test_that("pull_equation does not repeat a set's exact leave-one-out refits", {
  set <- with_expand_record(
    manec_example,
    expand_record(fitting = list(reloo = TRUE), leave_out = "observation")
  )
  seen <- NULL
  local_mocked_bindings(
    expand_manec = function(..., loo_controls) {
      seen <<- loo_controls
      stop("expand_manec reached")
    },
    .package = "bayesnec"
  )
  expect_error(bayesnec:::pull_equation(set, "ecx4param"),
               "expand_manec reached")
  expect_null(seen$fitting$reloo)
  # pull_out() itself keeps the recorded controls.
  expect_error(pull_out(set, model = "ecx4param"), "expand_manec reached")
  expect_true(seen$fitting$reloo)
})
