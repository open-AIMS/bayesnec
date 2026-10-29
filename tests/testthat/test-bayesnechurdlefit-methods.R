# Structural tests for the bayesnechurdlefit methods. None require sampling:
# they use a mock built from the pieces each method actually reads.
mock_hurdle <- function() {
  mk <- function(model) structure(list(model = model), class = c("bayesnecfit", "bnecfit"))
  structure(list(growth = mk("nec3param"), survival = mk("nec3param"),
                 data = data.frame(x = 1:4, y = c(2, 1, 0, 0)),
                 formula = bnf(y ~ crf(x, "nec3param")),
                 y_var = "y", n_exposed = 4L, n_dead = 2L),
            class = c("bayesnechurdlefit", "bnecfit"))
}

test_that("which defaults to combined and rejects anything else", {
  expect_equal(bayesnec:::hurdle_check_which("combined"), "combined")
  expect_equal(bayesnec:::hurdle_check_which("growth"), "growth")
  expect_error(bayesnec:::hurdle_check_which("mu"), "should be one of")
  # plots additionally allow "all" for the three-panel form
  expect_equal(bayesnec:::hurdle_check_plot_which("all"), "all")
  expect_error(bayesnec:::hurdle_check_which("all"), "should be one of")
})

test_that("combined is the default for every curve method", {
  for (f in list(posterior_epred.bayesnechurdlefit, fitted.bayesnechurdlefit,
                 predict.bayesnechurdlefit, posterior_predict.bayesnechurdlefit,
                 nsec.bayesnechurdlefit, ecx.bayesnechurdlefit,
                 nec.bayesnechurdlefit)) {
    expect_equal(eval(formals(f)$which), "combined")
  }
  expect_equal(eval(formals(plot.bayesnechurdlefit)$which), "combined")
  expect_equal(eval(formals(autoplot.bayesnechurdlefit)$which), "combined")
})

test_that("hurdle_raw_data recovers the response and a transformed predictor", {
  o <- mock_hurdle()
  d <- bayesnec:::hurdle_raw_data(o)
  expect_equal(d$y, c(2, 1, 0, 0))
  expect_equal(d$x, 1:4)
  o$formula <- bnf(y ~ crf(log(x), "nec3param"))
  expect_equal(bayesnec:::hurdle_raw_data(o)$x, log(1:4))
  # The plots read the column crf() names, the scale of the prediction grid.
  expect_equal(bayesnec:::hurdle_raw_data(o, recorded = TRUE)$x, 1:4)
  expect_equal(bayesnec:::hurdle_raw_data(o, recorded = TRUE)$y, d$y)
})

test_that("delegating methods return one element per component", {
  o <- mock_hurdle()
  out <- bayesnec:::hurdle_delegate(o, function(z, ...) z$model)
  expect_equal(out, list(growth = "nec3param", survival = "nec3param"))
})

test_that("hurdle amend validates the full predictor before either part (#317)", {
  object <- mock_hurdle()
  object$data$x <- c(1, 2, -1, -2)
  calls <- 0L
  local_mocked_bindings(
    amend = function(...) {
      calls <<- calls + 1L
      stop("component amend should not start")
    },
    .package = "bayesnec"
  )

  expect_error(
    bayesnec:::amend.bayesnechurdlefit(
      object, add = "ecxexp", predictor_scale = "concentration"
    ),
    "requires a non-negative predictor"
  )
  expect_equal(calls, 0L)
})

test_that("combining refuses objects that are not hurdle fits", {
  o <- mock_hurdle()
  plain <- structure(list(model = "nec3param"), class = c("bayesnecfit", "bnecfit"))
  expect_error(bayesnec:::hurdle_check_pair(list(o, plain)), "class bayesnechurdlefit")
  expect_true(bayesnec:::hurdle_check_pair(list(o, o)))
})

test_that("combining refuses fits on different responses", {
  o <- mock_hurdle(); p <- mock_hurdle(); p$y_var <- "z"
  expect_error(bayesnec:::hurdle_check_pair(list(o, p)), "same response")
})

test_that("update() re-splits newdata rather than passing it through", {
  # Zeros must reach the survival component as the hurdle and be withheld from
  # the growth component, exactly as bnec_hurdle() does when first fitting.
  # Checked on the split itself rather than through a fit.
  nd <- data.frame(x = 1:6, y = c(5, 4, 3, 0, 0, 0))
  y <- nd$y
  growth_rows <- nd[y > 0, , drop = FALSE]
  surv <- nd; surv$.alive <- as.integer(y > 0)
  expect_equal(nrow(growth_rows), 3)
  expect_false(any(growth_rows$y == 0))
  expect_equal(nrow(surv), 6)
  expect_equal(surv$.alive, c(1L, 1L, 1L, 0L, 0L, 0L))
  # and update() carries that split, not the raw frame
  # collapsed, because deparse() wraps mid-expression
  src <- paste(deparse(update.bayesnechurdlefit), collapse = " ")
  expect_match(src, "newdata\\[y >\\s*0")
  expect_match(src, "\\.alive")
})

test_that("update() requires the response column in newdata", {
  o <- mock_hurdle()
  expect_error(update(o, newdata = data.frame(x = 1:3)),
               "must contain the response column")
})

test_that("update() re-checks the zero-vs-censored invariant on newdata", {
  # The invariant is a property of the data, so a refit has to re-check it.
  # Otherwise a row that is both zero and censored is dropped from the growth
  # refit and coded as a death in the survival refit, with no message.
  o <- mock_hurdle()
  o$formula <- bnf(y | cens(cens) ~ crf(x, "nec3param"))
  bad <- data.frame(x = 1:6, y = c(5, 4, 3, 0, 0, 0),
                    cens = c(rep("none", 5), "left"))
  expect_error(update(o, newdata = bad),
               "zero and also carry a censoring code")
  # A censored survivor is a growth observation and must still get past the
  # check. The mock has no real component fit, so update() fails regardless --
  # the point is only that it does not fail on the invariant.
  ok <- bad
  ok$cens <- c("left", rep("none", 5))
  msg <- tryCatch(update(o, newdata = ok), error = conditionMessage)
  expect_false(grepl("zero and also carry", msg))
})

# ---- #435: update(newdata = ) on a hurdle fit -------------------------------
# A hurdle fit assembled from the packaged nec4param fit, as bnec_hurdle()
# builds one: a growth component storing the Gamma family under the tag brms
# records, "gamma", and a bernoulli survival component on `.alive`. The refit is
# mocked in every test that uses it, so nothing is sampled and the stored
# posterior, a gaussian one, is never read.
hurdle_update_fixture <- function(growth_family = brms::brmsfamily(
                                    "Gamma", link = "identity")) {
  growth <- nec4param
  growth$fit$family <- growth_family
  survival <- nec4param
  survival$fit$family <- brms::bernoulli(link = "identity")
  survival$bayesnecformula <- bnf(.alive ~ crf(x, model = "nec4param"))
  d <- data.frame(x = nec4param$fit$data$x,
                  y = stats::plogis(nec4param$fit$data$y))
  d$y[d$x > stats::quantile(d$x, 0.8)] <- 0
  structure(list(growth = growth, survival = survival, data = d,
                 formula = bnf(y ~ crf(x, model = "nec4param")), y_var = "y",
                 n_exposed = nrow(d), n_dead = sum(d$y == 0)),
            class = c("bayesnechurdlefit", "bnecfit"))
}

test_that("update() on a hurdle fit reaches both refits, growth first (#435)", {
  # The Gamma growth component was refused as a change of family, and the
  # survival component would have been as well: its 0/1 response, read off
  # the data, was poisson against a stored bernoulli.
  #
  # `update` is mocked where the package finds it, in its imports
  # environment: a binding mocked in stats is not seen, and the refit then
  # samples. The mock passes a component through to its own update() method,
  # so that method's family checks run, and halts at brms, recording what
  # brms would have received. stats::update() is called explicitly to reach
  # the methods rather than the mock.
  h <- hurdle_update_fixture()
  reached <- list()
  local_mocked_bindings(
    update = function(object, ...) {
      if (inherits(object, "brmsfit")) {
        reached[[length(reached) + 1]] <<- list(
          family = object$family$family, newdata = list(...)$newdata
        )
        stop("halted by test")
      }
      if (inherits(object, "bnecfit")) {
        try(stats::update(object, ...), silent = TRUE)
        return(object)
      }
      stats::update(object, ...)
    },
    .package = "bayesnec"
  )
  # try(silent = FALSE) in the refit loop prints the halt to stderr.
  out <- NULL
  invisible(capture.output(
    out <- suppressMessages(stats::update(h, newdata = h$data)),
    type = "message"
  ))
  expect_equal(vapply(reached, `[[`, character(1), "family"),
               c("gamma", "bernoulli"))
  n_dead <- sum(h$data$y == 0)
  growth_data <- reached[[1]]$newdata
  expect_equal(nrow(growth_data), nrow(h$data) - n_dead)
  expect_false(any(growth_data$y == 0))
  surv_data <- reached[[2]]$newdata
  expect_equal(nrow(surv_data), nrow(h$data))
  expect_equal(sum(surv_data$.alive == 0), n_dead)
  expect_s3_class(out, "bayesnechurdlefit")
  expect_equal(out$n_dead, n_dead)
})

test_that("update() on a hurdle fit refuses new data with no zero first (#435)", {
  # Every individual survived, so there is no survival response to fit. The
  # refusal comes before either refit, with bnec_hurdle()'s message for the
  # same data, whether or not a growth family is supplied.
  h <- hurdle_update_fixture()
  calls <- 0L
  local_mocked_bindings(
    update = function(...) {
      calls <<- calls + 1L
      stop("refit reached")
    },
    .package = "bayesnec"
  )
  nd <- h$data
  nd$y[nd$y == 0] <- 0.01
  from_hurdle <- expect_error(
    bnec_hurdle(y ~ crf(x, model = "nec4param"), data = nd)
  )
  expect_match(conditionMessage(from_hurdle),
               "contains no zeros, so there is no survival signal to model.",
               fixed = TRUE)
  err <- expect_error(stats::update(h, newdata = nd))
  expect_identical(conditionMessage(err), conditionMessage(from_hurdle))
  err <- expect_error(
    stats::update(h, newdata = nd, family = Gamma(link = "identity"))
  )
  expect_identical(conditionMessage(err), conditionMessage(from_hurdle))
  expect_false(grepl(".alive", conditionMessage(err), fixed = TRUE))
  # Every individual died: bnec_hurdle()'s message too, not the growth or
  # survival component's bound.
  all_dead <- h$data
  all_dead$y <- 0
  from_hurdle <- expect_error(
    bnec_hurdle(y ~ crf(x, model = "nec4param"), data = all_dead)
  )
  err <- expect_error(stats::update(h, newdata = all_dead))
  expect_identical(conditionMessage(err), conditionMessage(from_hurdle))
  expect_identical(calls, 0L)
})

test_that("update() on a hurdle fit refuses a negative response (#486)", {
  # Read as a death, a negative value would reach the survival component as
  # one, with no message.
  h <- hurdle_update_fixture()
  local_mocked_bindings(
    update = function(...) stop("refit reached"),
    .package = "bayesnec"
  )
  nd <- h$data
  nd$y[1] <- -0.5
  expect_error(stats::update(h, newdata = nd), "contains negative values")
})

test_that("a family supplied to a hurdle update() reaches growth only (#486)", {
  # The survival component is always bernoulli. Given the growth family, its
  # refit was refused as a change of family after growth had been refitted.
  h <- hurdle_update_fixture()
  reached <- list()
  local_mocked_bindings(
    update = function(object, ...) {
      reached[[length(reached) + 1]] <<- list(
        component = object$fit$family$family, args = ...names()
      )
      object
    },
    .package = "bayesnec"
  )
  for (nd in list(NULL, h$data)) {
    reached <- list()
    stats::update(h, newdata = nd, family = Gamma(link = "identity"),
                  chains = 4)
    expect_equal(vapply(reached, `[[`, character(1), "component"),
                 c("gamma", "bernoulli"))
    expect_true("family" %in% reached[[1]]$args)
    expect_false("family" %in% reached[[2]]$args)
    expect_true("chains" %in% reached[[2]]$args)
  }
})

test_that("the survival bound check reads the survival family (#486)", {
  h <- hurdle_update_fixture()
  seen <- list()
  local_mocked_bindings(
    check_refit_bound = function(part, data, family = NULL, ...) {
      seen <<- c(seen, list(family))
      invisible(NULL)
    },
    update = function(...) stop("refit reached"),
    .package = "bayesnec"
  )
  expect_error(
    stats::update(h, newdata = h$data, family = Gamma(link = "identity")),
    "refit reached"
  )
  expect_length(seen, 2)
  expect_equal(seen[[1]]$family, "Gamma")
  expect_null(seen[[2]])
})

test_that("a Gamma growth family leaves the survival refit bernoulli (#486)", {
  # Each component passes through its own update() method to the point where
  # brms would be called, as in the test above that refits both.
  h <- hurdle_update_fixture()
  reached <- list()
  local_mocked_bindings(
    update = function(object, ...) {
      if (inherits(object, "brmsfit")) {
        reached[[length(reached) + 1]] <<- list(
          stored = object$family$family, supplied = list(...)$family
        )
        stop("halted by test")
      }
      if (inherits(object, "bnecfit")) {
        try(stats::update(object, ...), silent = TRUE)
        return(object)
      }
      stats::update(object, ...)
    },
    .package = "bayesnec"
  )
  msgs <- character()
  invisible(capture.output(
    withCallingHandlers(
      stats::update(h, newdata = h$data, family = Gamma(link = "identity")),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    ),
    type = "message"
  ))
  expect_equal(vapply(reached, `[[`, character(1), "stored"),
               c("gamma", "bernoulli"))
  expect_equal(reached[[1]]$supplied$family, "Gamma")
  expect_null(reached[[2]]$supplied)
  expect_false(any(grepl("shifted|differs", msgs)))
})

test_that("update() on a hurdle fit checks the growth family first (#435)", {
  # A Beta growth component whose every survivor is 1 is at the upper bound of
  # its family. Refused before either refit, naming the growth component as
  # bnec_hurdle() does.
  h <- hurdle_update_fixture(brms::Beta(link = "identity"))
  calls <- 0L
  local_mocked_bindings(
    update = function(...) {
      calls <<- calls + 1L
      stop("refit reached")
    },
    .package = "bayesnec"
  )
  nd <- h$data
  nd$y[nd$y > 0] <- 1
  n_alive <- sum(nd$y > 0)
  expect_error(
    stats::update(h, newdata = nd),
    paste0("The growth component of the response \"y\" (its ", n_alive,
           " non-zero values) is at the upper bound of a beta response"),
    fixed = TRUE
  )
  expect_identical(calls, 0L)
})

test_that("a growth component at a bound is pointed to bnec_hurdle() (#486)", {
  # update() cannot give one component its own equations, so the remedy is a
  # bnec_hurdle() call, and bnec_hurdle() gives the same message for the same
  # data.
  h <- hurdle_update_fixture(brms::Beta(link = "identity"))
  # bnec_hurdle() updates formulas, so only a refit is halted.
  local_mocked_bindings(
    update = function(object, ...) {
      if (inherits(object, "bnecfit")) stop("refit reached")
      stats::update(object, ...)
    },
    bnec = function(...) stop("fit reached"),
    .package = "bayesnec"
  )
  nd <- h$data
  nd$y[nd$y > 0] <- 1
  err <- expect_error(stats::update(h, newdata = nd))
  msg <- conditionMessage(err)
  expect_match(msg, paste0("name it in the bnec_hurdle() formula, as in",
                           " crf(x, \"ecxflat\"), and give the survival",
                           " component its equations through",
                           " model_survival."),
               fixed = TRUE)
  expect_false(grepl("call bnec()", msg, fixed = TRUE))
  from_hurdle <- expect_error(suppressMessages(
    bnec_hurdle(y ~ crf(x, model = "nec4param"), data = nd,
                family_growth = Beta(link = "identity"))
  ))
  expect_identical(conditionMessage(from_hurdle), msg)
  # The remedy passes every check before the first fit.
  expect_error(
    suppressMessages(
      bnec_hurdle(y ~ crf(x, model = "ecxflat"), data = nd,
                  family_growth = Beta(link = "identity"),
                  model_survival = "nec4param")
    ),
    "fit reached"
  )
})

# ---- #503: the growth family a hurdle update() refits -------------------------

hurdle_counts <- function(y) as.integer(round(y * 20)) + as.integer(y > 0)

# The growth fit stores counts, so that a count family passes the check
# update.bnecfit() makes of the stored data when no newdata are given.
count_ready_fixture <- function(growth_family = brms::brmsfamily(
                                  "Gamma", link = "identity")) {
  h <- hurdle_update_fixture(growth_family)
  h$growth$fit$data$y <- hurdle_counts(stats::plogis(h$growth$fit$data$y))
  h
}

# Replace the installed brms version, and nothing else, in the version check.
local_brms_version <- function(version, env = parent.frame()) {
  real <- bayesnec:::check_count_truncation_support
  local_mocked_bindings(
    check_count_truncation_support = function(...) {
      real(package_version(version))
    },
    .package = "bayesnec", .env = env
  )
}

# Pass each component through its own update() method and halt where brms
# would be called. Records the bayesnec formula each method received, and the
# response and family brms received.
local_refit_recorder <- function(env = parent.frame()) {
  rec <- new.env()
  rec$bnf <- list()
  rec$brms <- list()
  local_mocked_bindings(
    update = function(object, ...) {
      if (inherits(object, "brmsfit")) {
        rec$brms[[length(rec$brms) + 1]] <- list(
          lhs = deparse1(object$formula$formula[[2]]),
          family = list(...)$family
        )
        stop("halted by test")
      }
      if (inherits(object, "bnecfit")) {
        rec$bnf[[length(rec$bnf) + 1]] <- object$bayesnecformula
        try(stats::update(object, ...), silent = TRUE)
        return(object)
      }
      stats::update(object, ...)
    },
    .package = "bayesnec", .env = env
  )
  rec
}

# try(silent = FALSE) in the refit loop prints the halt to stderr.
quiet_refit <- function(expr) {
  invisible(capture.output(
    {
      suppressMessages(expr)
      invisible(NULL)
    },
    type = "message"
  ))
}

test_that("a growth family bnec_hurdle() refuses is refused by update() (#503)", {
  h <- hurdle_update_fixture()
  # bnec_hurdle() updates formulas, so only a refit is halted.
  calls <- 0L
  local_mocked_bindings(
    update = function(object, ...) {
      if (inherits(object, "bnecfit")) {
        calls <<- calls + 1L
        stop("refit reached")
      }
      stats::update(object, ...)
    },
    bnec = function(...) stop("fit reached"),
    .package = "bayesnec"
  )
  for (fam in c("hurdle_gamma", "zero_inflated_poisson", "binomial")) {
    from_hurdle <- expect_error(
      bnec_hurdle(y ~ crf(x, model = "nec4param"), data = h$data,
                  family_growth = fam)
    )
    expect_match(conditionMessage(from_hurdle),
                 paste("cannot use", fam, "as the growth family"))
    for (nd in list(NULL, h$data)) {
      err <- expect_error(
        stats::update(h, newdata = nd, family = fam, force_fit = TRUE)
      )
      expect_identical(conditionMessage(err), conditionMessage(from_hurdle))
    }
  }
  # bnec_hurdle() refuses the logit link of hurdle_gamma() before the family.
  # update() reads the family alone first, so the refusal names the family.
  expect_error(
    stats::update(h, family = hurdle_gamma(), force_fit = TRUE),
    "bnec_hurdle cannot use hurdle_gamma as the growth family",
    fixed = TRUE
  )
  expect_identical(calls, 0L)
})

test_that("a count growth refit needs brms 2.23.2, as bnec_hurdle() does (#503)", {
  h <- count_ready_fixture()
  nd <- h$data
  nd$y <- hurdle_counts(nd$y)
  local_brms_version("2.23.1")
  # bnec_hurdle() updates formulas, so only a refit is halted.
  calls <- 0L
  local_mocked_bindings(
    update = function(object, ...) {
      if (inherits(object, "bnecfit")) {
        calls <<- calls + 1L
        stop("refit reached")
      }
      stats::update(object, ...)
    },
    bnec = function(...) stop("fit reached"),
    .package = "bayesnec"
  )
  from_hurdle <- expect_error(suppressMessages(
    bnec_hurdle(y ~ crf(x, model = "nec4param"), data = nd,
                family_growth = poisson())
  ))
  expect_match(conditionMessage(from_hurdle),
               "require brms 2.23.2 or later.*Installed brms is 2.23.1")
  for (d in list(NULL, nd)) {
    for (fam in list(poisson(), negbinomial())) {
      err <- expect_error(
        stats::update(h, newdata = d, family = fam, force_fit = TRUE)
      )
      expect_identical(conditionMessage(err), conditionMessage(from_hurdle))
    }
    # A growth component already fitted with the truncation needs the same
    # version when no family is supplied.
    truncated <- h
    truncated$growth <- bayesnec:::set_hurdle_growth_truncation(h$growth, TRUE)
    truncated$growth$fit$family <- brms::brmsfamily("poisson",
                                                    link = "identity")
    err <- expect_error(stats::update(truncated, newdata = d))
    expect_identical(conditionMessage(err), conditionMessage(from_hurdle))
  }
  expect_identical(calls, 0L)
})

test_that("every stored growth equation is checked for count truncation", {
  h <- count_ready_fixture(brms::brmsfamily("poisson", link = "identity"))
  nd <- h$data
  nd$y <- hurdle_counts(nd$y)
  growth_data <- h$growth$fit$data
  h$growth <- manec_example
  for (model in names(h$growth$mod_fits)) {
    h$growth$mod_fits[[model]]$fit$family <- brms::brmsfamily(
      "poisson", link = "identity"
    )
    h$growth$mod_fits[[model]]$fit$data <- growth_data
  }
  # A set can combine legacy untruncated fits with newer truncated fits.
  h$growth$mod_fits[[2]] <- bayesnec:::set_hurdle_growth_truncation(
    h$growth$mod_fits[[2]], TRUE
  )
  expect_no_error(bayesnec:::check_data_equality(
    bayesnec:::recover_prebayesnecfit(h$growth)
  ))
  calls <- 0L
  local_mocked_bindings(
    update = function(...) {
      calls <<- calls + 1L
      stop("refit reached")
    },
    .package = "bayesnec"
  )
  local({
    local_brms_version("2.23.1")
    for (reverse in c(FALSE, TRUE)) {
      pair <- h
      if (reverse) {
        pair$growth$mod_fits <- rev(pair$growth$mod_fits)
      }
      for (d in list(NULL, nd)) {
        expect_error(
          stats::update(pair, newdata = d),
          "require brms 2.23.2 or later.*Installed brms is 2.23.1"
        )
      }
    }
  })
  expect_identical(calls, 0L)
  local_brms_version("2.23.2")
  expect_identical(bayesnec:::hurdle_refit_growth(h), h$growth)
})

test_that("a count growth family is refitted with trunc(lb = 1) (#503)", {
  h <- count_ready_fixture()
  nd <- h$data
  nd$y <- hurdle_counts(nd$y)
  local_brms_version("2.23.2")
  rec <- local_refit_recorder()
  for (d in list(NULL, nd)) {
    for (fam in list(poisson(), negbinomial())) {
      rec$bnf <- list()
      rec$brms <- list()
      quiet_refit(
        stats::update(h, newdata = d, family = fam, force_fit = TRUE)
      )
      # Growth first, then survival.
      expect_length(rec$brms, 2)
      expect_identical(rec$brms[[1]]$lhs, "y | trunc(lb = 1)")
      expect_identical(rec$brms[[1]]$family$family, fam$family)
      expect_true(isTRUE(attr(rec$bnf[[1]], "bayesnec_internal_truncation")))
      expect_match(deparse1(rec$bnf[[1]]), "y | trunc(lb = 1) ~",
                   fixed = TRUE)
      expect_false(grepl("trunc", rec$brms[[2]]$lhs, fixed = TRUE))
      expect_null(rec$brms[[2]]$family)
      expect_null(attr(rec$bnf[[2]], "bayesnec_internal_truncation"))
    }
  }
  # The fit passed to update() is not changed.
  expect_null(attr(h$growth$bayesnecformula, "bayesnec_internal_truncation"))
  expect_identical(deparse1(h$growth$fit$formula$formula[[2]]), "y")
})

test_that("a count growth component keeps or loses trunc(lb = 1) by its refit family (#503)", {
  h <- count_ready_fixture(brms::brmsfamily("poisson", link = "identity"))
  h$growth <- bayesnec:::set_hurdle_growth_truncation(h$growth, TRUE)
  nd <- h$data
  nd$y <- hurdle_counts(nd$y)
  rec <- local_refit_recorder()
  # Without a family the stored formulas are refitted as they are.
  local_brms_version("2.23.2")
  for (d in list(NULL, nd)) {
    rec$brms <- list()
    quiet_refit(stats::update(h, newdata = d))
    expect_identical(rec$brms[[1]]$lhs, "y | trunc(lb = 1)")
    expect_null(rec$brms[[1]]$family)
  }
  # A Gamma refit takes the truncation off, and needs no particular brms.
  local_brms_version("2.23.1")
  for (d in list(NULL, nd)) {
    rec$bnf <- list()
    rec$brms <- list()
    quiet_refit(stats::update(h, newdata = d, family = Gamma(link = "identity"),
                              force_fit = TRUE))
    expect_identical(rec$brms[[1]]$lhs, "y")
    expect_identical(rec$brms[[1]]$family$family, "Gamma")
    expect_null(attr(rec$bnf[[1]], "bayesnec_internal_truncation"))
    expect_identical(deparse1(rec$bnf[[1]]), "y ~ crf(x, model = \"nec4param\")")
  }
})

test_that("cens() with a count growth family is refused by update() (#503)", {
  h <- count_ready_fixture()
  h$formula <- bnf(y | cens(cens) ~ crf(x, model = "nec4param"))
  local_brms_version("2.23.2")
  local_mocked_bindings(
    update = function(...) stop("refit reached"),
    .package = "bayesnec"
  )
  expect_error(
    stats::update(h, family = poisson(), force_fit = TRUE),
    "bnec_hurdle cannot combine cens() with a count growth family.",
    fixed = TRUE
  )
})

test_that("summary carries its own class and print method", {
  expect_true(!is.null(getS3method("print", "hurdlesummary", optional = TRUE)))
  expect_true(!is.null(getS3method("summary", "bayesnechurdlefit", optional = TRUE)))
})

test_that("every audited method has a bayesnechurdlefit method or a branch", {
  # Guards against a method being added for bayesnecfit later without a
  # matching hurdle path, which is how the silent failures arose originally.
  s3 <- c("summary", "print", "plot", "autoplot", "ggbnec_data", "predict",
          "fitted", "posterior_epred", "posterior_predict", "nsec", "nec",
          "ecx", "ecnsec", "rhat", "check_chains", "check_priors",
          "model.frame", "pull_brmsfit", "bnec_newdata",
          "update", "amend", "c")
  for (g in s3) {
    expect_true(!is.null(getS3method(g, "bayesnechurdlefit", optional = TRUE)),
                label = paste0(g, ".bayesnechurdlefit"))
  }
  # pull_out, dispersion and compare_fitted are plain functions, not generics,
  # so an S3 method for them would be silently inert -- they branch internally.
  for (f in list(bayesnec:::pull_out, bayesnec:::dispersion,
                 bayesnec:::newdata_eval_fitted)) {
    expect_true(any(grepl("is_bayesnechurdlefit", deparse(f))))
  }
  for (f in c("pull_out", "dispersion", "compare_fitted")) {
    expect_false(any(grepl("UseMethod", deparse(get(f)))), label = f)
  }
})


# ---- D15 ruling 2 on the two-block class -------------------------------------

# The three estimator methods took the control from p_samples[, 1], the first
# column of the prediction grid, so x_range moved the reference and therefore
# every estimate. hurdle_component_preds() now returns a control read at the
# lowest observed concentration and pinned to the same concentration on both
# sides. Asserted by mocking that function, because reproducing the defect by
# fitting needs two brms fits and a grid extended below the data.

hurdle_preds_stub <- function(control_at, curve_at_grid_start) {
  # A declining curve on a grid whose first column is NOT the control: this is
  # the situation an extended x_range creates. A method reading p_samples[, 1]
  # gets curve_at_grid_start; one reading preds$control gets control_at.
  x <- seq(1, 10, length.out = 10)
  g <- matrix(rep(seq(curve_at_grid_start, 0.1, length.out = 10), each = 4),
              nrow = 4)
  function(object, resolution = 1000, x_range = NA) {
    list(x = x, growth = g, survival = g, combined = g,
         control = list(growth = rep(control_at, 4),
                        survival = rep(control_at, 4),
                        combined = rep(control_at, 4)))
  }
}

test_that("ecx reads the control from preds$control, not the grid start", {
  obj <- mock_hurdle()
  # Grid starts at 2 but the control is 8. An EC50 measured from the control is
  # reached at 4; measured from the grid start it is reached much earlier.
  local_mocked_bindings(
    hurdle_component_preds = hurdle_preds_stub(control_at = 8,
                                               curve_at_grid_start = 2)
  )
  out <- ecx(obj, ecx_val = 50, posterior = TRUE)
  # The target is 4, which the curve (2 down to 0.1) never reaches, so every
  # draw is NA rather than an estimate read from the wrong reference.
  expect_true(all(is.na(out)))
})

test_that("the estimator methods survive a censored draw", {
  obj <- mock_hurdle()
  local_mocked_bindings(
    hurdle_component_preds = hurdle_preds_stub(control_at = 8,
                                               curve_at_grid_start = 2)
  )
  # quantile() without na.rm stopped with "missing values and NaN's not
  # allowed" here, on a vector the package had just built.
  est <- suppressWarnings(ecx(obj, ecx_val = 50))
  expect_equal(length(est), 3)
  expect_warning(ecx(obj, ecx_val = 50), "not identified for 4 of 4 draws")
})

test_that("ecnsec honours type and refuses the two it cannot compute", {
  obj <- mock_hurdle()
  local_mocked_bindings(
    hurdle_component_preds = hurdle_preds_stub(control_at = 8,
                                               curve_at_grid_start = 2)
  )
  # absolute measures control to 0; range measures control to the curve's
  # lowest predicted response, a shorter span, so the same decline is a larger
  # percentage of it. The version this replaces returned the absolute answer
  # whatever type was given.
  abs_out <- ecnsec(obj, nsec = 5, type = "absolute")
  rng_out <- ecnsec(obj, nsec = 5, type = "range")
  expect_gt(rng_out[1], abs_out[1])
  expect_error(ecnsec(obj, nsec = 5, type = "relative"), "not defined for a")
  expect_error(ecnsec(obj, nsec = 5, type = "direct"), "names a response value")
  expect_error(ecnsec(obj, nsec = 5, type = "nonsense"), "type must be one of")
  expect_error(ecnsec(obj, nsec = 5, hormesis_def = "max"),
               "hormesis_def has been removed")
})

test_that("the summary ECx grid is read from whichever class the survival block is", {
  # A hurdle component is a bayesnecfit or a bayesmanecfit depending on whether
  # crf() named one equation or a set, and the two store their prediction grid
  # under different names. Reading pred_vals alone returned NULL for the
  # commoner case, range() of nothing is c(Inf, -Inf), and summary(ecx = TRUE)
  # then errored inside seq() (#395).
  #
  # Every row is read over the survival component's grid, the whole
  # concentration series (D44, #412), whatever the growth component stores.
  mk <- function(grid, slot) {
    out <- list()
    out[[slot]] <- list(data = data.frame(x = grid))
    out
  }
  range_of <- function(o) bayesnec:::hurdle_summary_range(o)
  for (g_slot in c("pred_vals", "w_pred_vals")) {
    for (s_slot in c("pred_vals", "w_pred_vals")) {
      o <- list(growth = mk(c(1, 4), g_slot), survival = mk(c(0.5, 6), s_slot))
      expect_equal(range_of(o), c(0.5, 6))
    }
  }
  # No survival grid: NULL, which leaves ecx() to build its own rather than
  # being handed a range it cannot use. A growth grid is not used in its place.
  expect_null(range_of(list(growth = list(), survival = list())))
  expect_null(range_of(list(growth = mk(c(1, 4), "pred_vals"),
                            survival = list())))
})

# A component carrying only what summary.bayesnechurdlefit() reads once ecx()
# and nec() are mocked: the stored prediction grid, the equation names, the
# no-effect type and the family. grid = NULL stores no grid.
summary_component <- function(grid, averaged = FALSE) {
  fit <- list(family = list(family = "gaussian"))
  stored <- if (is.null(grid)) NULL else list(data = data.frame(x = grid))
  if (averaged) {
    out <- list(mod_fits = list(nec3param = list(fit = fit),
                                ecx4param = list(fit = fit)),
                ne_type = "N(S)EC", w_pred_vals = stored)
    structure(out, class = c("bayesmanecfit", "bnecfit"))
  } else {
    out <- list(model = "nec3param", ne_type = "NEC", fit = fit,
                pred_vals = stored)
    structure(out, class = c("bayesnecfit", "bnecfit"))
  }
}

summary_hurdle <- function(growth, survival) {
  structure(list(growth = growth, survival = survival,
                 n_exposed = 40L, n_dead = 10L),
            class = c("bayesnechurdlefit", "bnecfit"))
}

# Replaces ecx() and nec() with recorders. Each call's arguments are kept as
# supplied, so an argument passed twice appears twice rather than stopping the
# call as the real generic does.
record_summary_calls <- function(env = parent.frame()) {
  calls <- new.env()
  calls$ecx <- list()
  calls$nec <- list()
  estimate <- c(Estimate = 1, Q2.5 = 0.5, Q97.5 = 2)
  local_mocked_bindings(
    ecx = function(object, ...) {
      calls$ecx[[length(calls$ecx) + 1]] <- list(...)
      estimate
    },
    nec = function(object, ...) {
      calls$nec[[length(calls$nec) + 1]] <- list(...)
      estimate
    },
    .package = "bayesnec",
    .env = env
  )
  calls
}

n_named <- function(args, name) sum(names(args) == name)

test_that("an explicit x_range reaches each summary ECx call once (#416)", {
  # Every pairing of a single-equation and a model-averaged component, since
  # the two classes store their grid under different names.
  for (g_avg in c(FALSE, TRUE)) {
    for (s_avg in c(FALSE, TRUE)) {
      o <- summary_hurdle(summary_component(c(0, 10), g_avg),
                          summary_component(c(0, 40), s_avg))
      calls <- record_summary_calls()
      # The lower limit differs from the default of c(0, 40), so a default
      # left in place of the supplied range would show.
      out <- summary(o, ecx = TRUE, ecx_vals = c(10, 50),
                     x_range = c(0.5, 40), xform = exp, resolution = 50)
      expect_s3_class(out, "hurdlesummary")
      expect_length(calls$ecx, 6)
      for (args in calls$ecx) {
        expect_equal(n_named(args, "x_range"), 1)
        expect_equal(args$x_range, c(0.5, 40))
        expect_identical(args$xform, exp)
        expect_equal(args$resolution, 50)
      }
      expect_setequal(vapply(calls$ecx, `[[`, "", "which"),
                      c("combined", "growth", "survival"))
      expect_setequal(vapply(calls$ecx, `[[`, 0, "ecx_val"), c(10, 50))
      # The no-effect rows are read from the posterior stored at fit time, so
      # x_range is withheld from nec() while the rest of ... still reaches it.
      expect_length(calls$nec, 3)
      for (args in calls$nec) {
        expect_equal(n_named(args, "x_range"), 0)
        expect_identical(args$xform, exp)
        expect_equal(args$resolution, 50)
      }
    }
  }
})

test_that("without x_range every summary ECx row uses the survival grid", {
  # The whole concentration series for the growth rows as for the others
  # (D44, #412). Before #412 all three read the intersection, c(1, 10), and
  # under D28 the growth rows read growth's own grid.
  for (g_avg in c(FALSE, TRUE)) {
    for (s_avg in c(FALSE, TRUE)) {
      o <- summary_hurdle(summary_component(c(1, 10), g_avg),
                          summary_component(c(0, 40), s_avg))
      calls <- record_summary_calls()
      summary(o, ecx = TRUE, ecx_vals = 50, xform = exp, resolution = 50)
      expect_length(calls$ecx, 3)
      for (args in calls$ecx) {
        expect_equal(n_named(args, "x_range"), 1)
        expect_equal(args$x_range, c(0, 40))
        expect_identical(args$xform, exp)
        expect_equal(args$resolution, 50)
      }
      expect_setequal(vapply(calls$ecx, `[[`, "", "which"),
                      c("combined", "growth", "survival"))
    }
  }
})

test_that("a survival component with no stored grid leaves ecx() its default", {
  # Every row loses its x_range, the growth rows included: a growth grid is not
  # used in place of the survival one.
  o <- summary_hurdle(summary_component(c(1, 10)), summary_component(NULL))
  calls <- record_summary_calls()
  summary(o, ecx = TRUE, ecx_vals = 50)
  expect_length(calls$ecx, 3)
  for (args in calls$ecx) {
    expect_equal(n_named(args, "x_range"), 0)
  }
  # A growth component with no stored grid changes nothing.
  o <- summary_hurdle(summary_component(NULL),
                      summary_component(c(0, 40), averaged = TRUE))
  calls <- record_summary_calls()
  summary(o, ecx = TRUE, ecx_vals = 50)
  for (args in calls$ecx) {
    expect_equal(args$x_range, c(0, 40))
  }
  # A supplied range still applies where there is no default to replace.
  o <- summary_hurdle(summary_component(c(1, 10)), summary_component(NULL))
  calls <- record_summary_calls()
  summary(o, ecx = TRUE, ecx_vals = 50, x_range = c(0.5, 40))
  for (args in calls$ecx) {
    expect_equal(n_named(args, "x_range"), 1)
    expect_equal(args$x_range, c(0.5, 40))
  }
})

test_that("x_range = NULL leaves ecx() its own default, not the stored grid", {
  # The survival component stores a grid, so the summary's default would be
  # c(0, 40). An explicit NULL is not that default: the call reaches ecx()
  # without an x_range, and ecx() then applies its own default of NA and builds
  # its grid.
  o <- summary_hurdle(summary_component(c(0, 10)),
                      summary_component(c(0, 40), averaged = TRUE))
  calls <- record_summary_calls()
  summary(o, ecx = TRUE, ecx_vals = 50, x_range = NULL, resolution = 50)
  expect_length(calls$ecx, 3)
  for (args in calls$ecx) {
    expect_equal(n_named(args, "x_range"), 0)
    expect_equal(args$resolution, 50)
  }
})

test_that("x_range without ecx = TRUE reaches neither estimator", {
  o <- summary_hurdle(summary_component(c(0, 10)),
                      summary_component(c(0, 40)))
  calls <- record_summary_calls()
  expect_no_error(summary(o, x_range = c(0.5, 40)))
  expect_length(calls$ecx, 0)
  expect_length(calls$nec, 3)
  for (args in calls$nec) {
    expect_equal(n_named(args, "x_range"), 0)
  }
})

# ---- The range each hurdle estimate is read over (#412, D44) -----------------

# A hurdle fit whose growth data stop short of the survival data, built from
# the packaged nec4param fit rather than by sampling. The growth component
# keeps only the rows at or below cut, as a fit to survivors does where nothing
# lived above cut, and stores a grid over those rows, as bnec() did for growth
# before D44 and as a fit made before D44 still stores. The survival component
# keeps every row, so its range runs to 3.22. Both components share the one set
# of draws, which is enough here: what is asserted is which range each curve is
# read over.
hurdle_cut_fixture <- function(cut) {
  growth <- nec4param
  growth$fit$data <- growth$fit$data[growth$fit$data$x <= cut, , drop = FALSE]
  xg <- growth$fit$data$x
  growth$pred_vals$data <- data.frame(x = seq(min(xg), max(xg),
                                              length.out = 100))
  structure(list(growth = growth, survival = nec4param,
                 data = nec4param$fit$data,
                 formula = nec4param$bayesnecformula, y_var = "y",
                 n_exposed = nrow(nec4param$fit$data), n_dead = 0L),
            class = c("bayesnechurdlefit", "bnecfit"))
}

test_that("summary() and a bare ecx() agree for every curve (#412)", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  obj <- hurdle_cut_fixture(1.6)
  s <- suppressWarnings(summary(obj, ecx = TRUE, ecx_vals = c(10, 50, 90)))
  for (w in c("combined", "growth", "survival")) {
    for (v in c(10, 50, 90)) {
      bare <- suppressWarnings(ecx(obj, ecx_val = v, which = w))
      expect_equal(s$ecs[[w]][[paste0("ec", v)]], bare,
                   label = paste(w, "EC", v))
    }
  }
  # Before #412 the summary read every row over the intersection of the two
  # grids, which ends at growth's top, so its survival EC50 was censored there
  # while a bare ecx() identified it above that.
  g_top <- max(obj$growth$fit$data$x)
  expect_gt(s$ecs$survival$ec50[["Q50"]], g_top)
  expect_null(attr(s$ecs$survival$ec50, "censored_summary"))
})

test_that("a growth ECx above the last survivor is read as a value (D44)", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # The concentrations above the last survivor were tested, so a growth EC50
  # there lies inside the tested range. Under D28 it was censored at growth's
  # highest concentration, 1.595 on this fixture.
  obj <- hurdle_cut_fixture(1.6)
  g_top <- max(obj$growth$fit$data$x)
  s_range <- range(obj$survival$fit$data$x)
  est <- ecx(obj, ecx_val = 50, which = "growth")
  expect_gt(est[["Q50"]], g_top)
  expect_null(attr(est, "censored_summary"))
  expect_equal(est, ecx(obj, ecx_val = 50, which = "growth",
                        x_range = s_range))
  # An EC10 below growth's top is identified as before.
  inside <- ecx(obj, ecx_val = 10, which = "growth")
  expect_lt(inside[["Q50"]], g_top)
  expect_null(attr(inside, "censored_summary"))
})

test_that("the combined ECx is read beyond the last survivor (#412)", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  obj <- hurdle_cut_fixture(1.6)
  g_top <- max(obj$growth$fit$data$x)
  est <- ecx(obj, ecx_val = 90, which = "combined")
  expect_gt(est[["Q2.5"]], g_top)
  expect_null(attr(est, "censored_summary"))
  s_range <- range(obj$survival$fit$data$x)
  expect_equal(est, ecx(obj, ecx_val = 90, which = "combined",
                        x_range = s_range))
  expect_equal(ecx(obj, ecx_val = 50, which = "survival"),
               ecx(obj, ecx_val = 50, which = "survival", x_range = s_range))
})

test_that("a growth NSEC is searched over the whole series (D44)", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # Cut below the NSEC of nec4param, about 1.48. Under D28 most draws had not
  # reached the reference by growth's top and were censored there.
  obj <- hurdle_cut_fixture(1.4)
  g_top <- max(obj$growth$fit$data$x)
  s_range <- range(obj$survival$fit$data$x)
  est <- suppressWarnings(nsec(obj, which = "growth"))
  expect_gt(est[["Q50"]], g_top)
  expect_equal(est, suppressWarnings(nsec(obj, which = "growth",
                                          x_range = s_range)),
               ignore_attr = TRUE)
  # extrapolate is measured against the whole series for every curve, so a
  # limit inside it is refused for growth as it is for survival.
  expect_error(nsec(obj, which = "growth", extrapolate = 2),
               "inside the prediction range")
  expect_error(nsec(obj, which = "survival", extrapolate = 2),
               "inside the prediction range")
})

test_that("ecnsec reads the growth curve over the whole series (D44)", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # type = "range" measures towards the lowest response over the grid, so the
  # grid the curve is read over shows in the answer.
  obj <- hurdle_cut_fixture(1.6)
  g_range <- range(obj$growth$fit$data$x)
  s_range <- range(obj$survival$fit$data$x)
  bare <- ecnsec(obj, nsec = 1.5, which = "growth", type = "range")
  expect_equal(bare, ecnsec(obj, nsec = 1.5, which = "growth",
                            type = "range", x_range = s_range))
  expect_false(isTRUE(all.equal(
    bare, ecnsec(obj, nsec = 1.5, which = "growth", type = "range",
                 x_range = g_range)
  )))
  # A value above the last survivor is read, not refused, as it is for the
  # survival and combined curves.
  expect_length(ecnsec(obj, nsec = 2.5, which = "growth"), 3)
  expect_equal(ecnsec(obj, nsec = 2.5, which = "growth"),
               ecnsec(obj, nsec = 2.5, which = "growth", x_range = s_range))
})

test_that("the plots and posterior_epred() use the whole series (#412)", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  obj <- hurdle_cut_fixture(1.6)
  s_range <- range(obj$survival$fit$data$x)
  dat <- ggbnec_data(obj, which = "growth", resolution = 20)
  expect_equal(range(dat$curve$x), s_range)
  expect_equal(posterior_epred(obj, which = "growth", resolution = 20),
               posterior_epred(obj, which = "growth", resolution = 20,
                               x_range = s_range))
})

test_that("a fit-time x_range is used for every summary row", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # bnec_hurdle(x_range = ) stores the one range on both components. Both
  # stored grids are widened to 4 here, above the data of either component.
  # Under D28 the growth rows were cut to growth's data; under D44 every row
  # is read over the stored survival grid.
  obj <- hurdle_cut_fixture(1.6)
  lower <- min(obj$survival$fit$data$x)
  wide <- data.frame(x = seq(lower, 4, length.out = 100))
  obj$growth$pred_vals$data <- wide
  obj$survival$pred_vals$data <- wide
  s <- suppressWarnings(summary(obj, ecx = TRUE, ecx_vals = c(10, 50)))
  for (w in c("growth", "survival", "combined")) {
    expect_equal(s$ecs[[w]]$ec50,
                 suppressWarnings(ecx(obj, ecx_val = 50, which = w,
                                      x_range = c(lower, 4))),
                 label = paste(w, "EC50"))
  }
})

test_that("exceedance() reads a growth estimate over the whole series (D44)", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # exceedance() reads the posterior through ecx(). Under D28 a growth EC50
  # above growth's top reached it censored there, and against a threshold above
  # that top the probability was an interval. Read over the whole series the
  # draws are identified, and the probability is a number.
  obj <- hurdle_cut_fixture(1.6)
  post <- suppressWarnings(ecx(obj, ecx_val = 50, which = "growth",
                               posterior = TRUE))
  expect_equal(sum(attr(post, "censored")$above), 0)
  out <- suppressWarnings(exceedance(obj, threshold = 2, estimate = "ecx",
                                     ecx_val = 50, which = "growth"))
  expect_equal(out$n_above, 0)
  expect_false(is.na(out$prob))
  expect_equal(out$prob_lower, out$prob_upper)
})

# ---- A hurdle pair over the whole series --------------------------------------

# Both components are the packaged nec4param fit over every row, so every curve
# spans the concentrations 0.0323 to 3.22 and no component's range stops short
# of the other's. Sharing one set of draws is enough for what is asserted
# here: the range each curve is read over, the point it is read at, and what
# xform is applied to.
hurdle_pair <- function(growth = nec4param, survival = nec4param) {
  structure(list(growth = growth, survival = survival,
                 data = nec4param$fit$data,
                 formula = growth$bayesnecformula, y_var = "y",
                 n_exposed = nrow(nec4param$fit$data), n_dead = 0L),
            class = c("bayesnechurdlefit", "bnecfit"))
}

# ---- ecnsec() beyond the range, between grid points, and xform (#437) --------

test_that("ecnsec refuses an nsec beyond the range on every hurdle curve", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # D33. The nearest grid point was read, so 1000 on a series ending at 3.22
  # returned the effect at 3.22 for every curve, with nothing said.
  obj <- hurdle_pair()
  x <- obj$survival$fit$data$x
  for (w in c("combined", "growth", "survival")) {
    expect_error(ecnsec(obj, nsec = 1000, which = w),
                 paste0("nsec = 1000 lies outside the observed range of the ",
                        "predictor, 0.03235 to 3.221. ecnsec() reads the ", w,
                        " curve over that range only."),
                 fixed = TRUE)
    expect_error(ecnsec(obj, nsec = 0.01, which = w),
                 "lies outside the observed range of the predictor")
    # A supplied x_range is the range refused against, and the advice is to
    # widen it.
    expect_error(ecnsec(obj, nsec = 2.5, x_range = c(0.1, 2), which = w),
                 paste0("nsec = 2.5 lies outside the x_range supplied, 0.1 to ",
                        "2. ecnsec() reads the ", w, " curve over that range ",
                        "only. Supply an x_range that includes nsec."),
                 fixed = TRUE)
    expect_length(ecnsec(obj, nsec = 4, x_range = c(min(x), 5), which = w), 3)
    # The ends are inside, including an end that has been through a
    # transformation and its inverse, as a censored nsec() bound may have.
    at_top <- ecnsec(obj, nsec = max(x), which = w)
    expect_length(at_top, 3)
    expect_equal(ecnsec(obj, nsec = exp(log(max(x))), which = w), at_top)
  }
  # A survival NSEC passed to the combined curve, the likely call, is inside.
  s_nsec <- suppressWarnings(nsec(obj, which = "survival"))[["Q50"]]
  expect_length(ecnsec(obj, nsec = s_nsec, which = "combined"), 3)
  # One value: the curve is interpolated at one point.
  expect_error(ecnsec(obj, nsec = c(1, 2)), "nsec")
})

test_that("ecnsec reads a hurdle curve between grid points, not at the nearer", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  obj <- hurdle_pair()
  preds <- bayesnec:::hurdle_component_preds(obj, resolution = 5)
  x <- preds$x
  # Three tenths of the way from the second grid point to the third, so the
  # nearer point is the second.
  at <- x[2] + 0.3 * (x[3] - x[2])
  for (w in c("combined", "growth", "survival")) {
    p <- preds[[w]]
    control <- preds$control[[w]]
    got <- ecnsec(obj, nsec = at, resolution = 5, which = w, posterior = TRUE)
    interpolated <- 0.7 * p[, 2] + 0.3 * p[, 3]
    expect_equal(got, (control - interpolated) / control * 100,
                 label = paste(w, "interpolated"))
    nearer <- (control - p[, 2]) / control * 100
    expect_false(isTRUE(all.equal(got, nearer)), label = paste(w, "nearer"))
    # At a grid point the column itself is read.
    expect_equal(ecnsec(obj, nsec = x[3], resolution = 5, which = w,
                        posterior = TRUE),
                 (control - p[, 3]) / control * 100)
  }
})

test_that("ecnsec on a fit agrees with the growth curve of a pair built from it", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # Read at grid points, where the hurdle method reads the column itself and
  # interpolates nothing. x[24] lies inside the spread of the nec draws, and
  # x[37] above all of them.
  for (fit in list(nec4param, manec_example)) {
    obj <- hurdle_pair(fit, fit)
    x <- bayesnec:::hurdle_component_preds(obj, resolution = 50)$x
    for (at in x[c(24, 37)]) {
      for (ty in c("absolute", "range")) {
        on_fit <- suppressWarnings(ecnsec(fit, nsec = at, resolution = 50,
                                          type = ty, posterior = TRUE))
        on_pair <- suppressWarnings(ecnsec(obj, nsec = at, resolution = 50,
                                           type = ty, which = "growth",
                                           posterior = TRUE))
        expect_equal(on_fit, on_pair,
                     label = paste(class(fit)[1], ty, "at", signif(at, 3)))
      }
    }
  }
})

test_that("ecnsec applies xform to nsec on a hurdle fit, with a warning", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # D33: xform was applied to the percentage returned. It is now applied to
  # nsec, as on every other class, and a call supplying one is warned for one
  # release.
  obj <- hurdle_pair()
  plain <- ecnsec(obj, nsec = 1.5)
  expect_warning(via <- ecnsec(obj, nsec = log(1.5), xform = exp),
                 class = "bayesnec_ecnsec_xform_changed")
  expect_equal(via, plain)
  expect_warning(ecnsec(obj, nsec = log(1.5), xform = exp),
                 "now applied to nsec before the curve is read")
  expect_no_warning(ecnsec(obj, nsec = 1.5))
  expect_no_warning(ecnsec(obj, nsec = 1.5, xform = identity))
  expect_error(ecnsec(obj, nsec = 1.5, xform = "log"),
               "xform must be a function")
  # The range is checked after xform, and the message gives both values.
  expect_error(suppressWarnings(ecnsec(obj, nsec = log(1000), xform = exp)),
               "nsec = 6.908, which xform takes to 1000, lies outside",
               fixed = TRUE)
  expect_error(suppressWarnings(ecnsec(obj, nsec = -1, xform = log)),
               "nsec = -1, which xform takes to NaN, lies outside",
               fixed = TRUE)
})

test_that("ecnsec on a crf(log(x)) hurdle pair reads log(v) under exp at v", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # The pair is relabelled, not refitted: the formula says crf(log(x)) and the
  # stored fits are those of crf(x), so the grid and the draws are the same and
  # only the value nsec arrives as differs. Before D33 the inverse had to be
  # applied to nsec by hand.
  logged <- nec4param
  logged$bayesnecformula <- bnf(y ~ crf(log(x), model = "nec4param"))
  h_log <- hurdle_pair(logged, logged)
  h_plain <- hurdle_pair()
  for (v in c(0.5, 1.5, 3)) {
    for (w in c("combined", "growth", "survival")) {
      expect_equal(
        suppressWarnings(ecnsec(h_log, nsec = log(v), xform = exp, which = w)),
        ecnsec(h_plain, nsec = v, which = w),
        label = paste(w, "at", v)
      )
    }
  }
})

# ---- The hurdle plot legend (#440) --------------------------------------------

# The legend() and abline() calls a base plot of obj makes, read off the calls
# rather than the device.
hurdle_legend <- function(obj, ...) {
  drawn <- list(legend = character(), v = list())
  local_mocked_bindings(
    legend = function(..., legend = NULL) {
      drawn$legend <<- c(drawn$legend, legend)
    },
    abline = function(v = NULL, ...) drawn$v[[length(drawn$v) + 1L]] <<- v,
    .package = "bayesnec"
  )
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  suppressWarnings(plot(obj, ...))
  drawn
}

# The survival component with the top tenth of its no-effect draws recorded as
# beyond the top of the range, as a fit records a threshold above every
# concentration tested.
censored_survival_pair <- function() {
  s <- nec4param
  post <- s$ne_posterior
  top <- as.numeric(post) >= stats::quantile(as.numeric(post), 0.9)
  attr(s$ne_posterior, "censored") <- bayesnec:::censoring_record(
    max(s$fit$data$x), min(s$fit$data$x), top, logical(length(post))
  )
  hurdle_pair(survival = s)
}

test_that("the hurdle legend reads low to high under a decreasing xform", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # The legend applied xform to the summary nec() returned, so under
  # function(x) -x the interval was printed from high to low (#440).
  obj <- hurdle_pair()
  negate <- function(z) -z
  ne <- nec(obj, xform = negate)
  expect_lt(ne[[2]], ne[[3]])
  drawn <- hurdle_legend(obj, xform = negate)
  expect_identical(drawn$legend,
                   paste0("N(S)EC: ", signif(ne[[1]], 2), " (",
                          signif(ne[[2]], 2), "-", signif(ne[[3]], 2), ")"))
  expect_equal(as.numeric(drawn$v[[1]]), as.numeric(ne))
  # An identity xform draws what nec() returns.
  plain <- nec(obj)
  expect_identical(hurdle_legend(obj)$legend,
                   paste0("N(S)EC: ", signif(plain[[1]], 2), " (",
                          signif(plain[[2]], 2), "-", signif(plain[[3]], 2),
                          ")"))
})

test_that("the hurdle legend marks a censored no-effect estimate", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  # The legend formatted signif() values only, so a bound read as a number
  # under any xform (#440).
  obj <- censored_survival_pair()
  top <- signif(max(obj$survival$fit$data$x), 2)
  plain <- hurdle_legend(obj, which = "survival")$legend
  expect_match(plain, paste0("->= ", top, ")"), fixed = TRUE)
  # Under a decreasing xform the bound is the lower limit, marked <=.
  negated <- hurdle_legend(obj, which = "survival",
                           xform = function(z) -z)$legend
  expect_match(negated, paste0("(<= ", -top, "-"), fixed = TRUE)
  expect_no_match(negated, ">=", fixed = TRUE)
  ne <- suppressWarnings(nec(obj, which = "survival", xform = function(z) -z))
  expect_identical(negated, paste0(
    "N(S)EC: ", bayesnec:::bound_prefix(ne, 1), signif(ne[[1]], 2), " (",
    bayesnec:::bound_prefix(ne, 2), signif(ne[[2]], 2), "-",
    bayesnec:::bound_prefix(ne, 3), signif(ne[[3]], 2), ")"
  ))
})

# ---- The NSEC message on the combined no-effect estimate (#446) ---------------

test_that("nec() says when the combined value includes NSEC draws", {
  # The message read the "toxicity_estimate" attribute, which both nec()
  # methods set to "nec" whatever the equation, so it was never printed.
  # Decided now from each component's ne_type.
  approx_msgs <- function(expr) {
    msgs <- testthat::capture_messages(suppressWarnings(expr))
    grep("combined value is approximate", msgs, value = TRUE)
  }
  expect_length(approx_msgs(nec(hurdle_pair())), 0)
  # A model set holding a smooth equation, on either component.
  for (pair in list(hurdle_pair(growth = manec_example),
                    hurdle_pair(survival = manec_example))) {
    expect_length(approx_msgs(nec(pair)), 1)
    # Only the combined value is a combination.
    expect_length(approx_msgs(nec(pair, which = "growth")), 0)
    expect_length(approx_msgs(nec(pair, which = "survival")), 0)
  }
  # A component fitted to one smooth equation, on either side.
  expect_length(approx_msgs(nec(hurdle_pair(survival = ecx4param))), 1)
  expect_length(approx_msgs(nec(hurdle_pair(growth = ecx4param))), 1)
  expect_length(approx_msgs(nec(hurdle_pair(ecx4param, ecx4param))), 1)
  # A component built before ne_type was recorded states no kind, and the
  # message would assert one.
  unrecorded <- nec4param
  unrecorded$ne_type <- NULL
  expect_length(approx_msgs(nec(hurdle_pair(unrecorded, unrecorded))), 0)
})

# ---- amend(add = ) on a hurdle fit with a Gamma growth component -----------
# Both components are built from ecx4param. brm() is mocked and expand_manec()
# returns the fits it is given, so each component's amend() completes.
test_that("amend(add = ) reaches both components of a Gamma hurdle (#436)", {
  d <- data.frame(x = nec_data$x, y = nec_data$y * 10)
  growth <- ecx4param
  growth$fit$family <- brms::bf(y ~ x, family = Gamma(link = "identity"))$family
  growth$fit$data <- d
  growth$bayesnecformula <- bnf(y ~ crf(x, model = "ecx4param") +
                                  disp("power"))
  survival <- ecx4param
  survival$fit$family <- brms::bernoulli(link = "identity")
  survival$fit$data <- data.frame(x = nec_data$x,
                                  .alive = as.integer(nec_data$y > 0.3))
  survival$bayesnecformula <- bnf(.alive ~ crf(x, model = "ecx4param"))
  h <- structure(list(growth = growth, survival = survival, data = d,
                      formula = bnf(y ~ crf(x, model = "ecx4param")),
                      y_var = "y", n_exposed = nrow(d), n_dead = 0L),
                 class = c("bayesnechurdlefit", "bnecfit"))
  seen <- list()
  local_mocked_bindings(
    brm = function(formula, data, ...) {
      a <- list(...)
      seen[[length(seen) + 1]] <<- list(family = a$family$family,
                                        prior = a$prior)
      structure(list(), class = "brmsfit")
    },
    are_chains_correct = function(...) TRUE,
    make_good_inits = function(model, x, y, family, ..., chains) {
      replicate(chains, list(b_top = as.array(1)), simplify = FALSE)
    },
    expand_manec = function(object, ...) {
      object[vapply(object, is_prebayesnecfit, logical(1))]
    },
    .package = "bayesnec"
  )
  out <- suppressMessages(amend(h, add = "nec4param"))
  expect_s3_class(out, "bayesnechurdlefit")
  expect_identical(vapply(seen, `[[`, character(1), "family"),
                   c("gamma", "bernoulli"))
  # With expand_manec() mocked, each component is the list of its fits.
  expect_setequal(names(out$growth), c("ecx4param", "nec4param"))
  expect_setequal(names(out$survival), c("ecx4param", "nec4param"))
  # The growth prior is define_prior()'s for the validated Gamma family.
  expected <- define_prior("nec4param", validate_family("Gamma"), d$x, d$y,
                           disp_spec = list(route = "B", value = "power"))
  expect_identical(seen[[1]]$prior, expected)
})

# ---- A component fitted to one smooth equation --------------------------------

test_that("nec() and summary() read a component fitted to one smooth equation (#487)", {
  # nec() refuses ecx4param fitted on its own. As a hurdle component it
  # contributes the NSEC draws stored with it.
  expect_error(nec(ecx4param), "nec is not a parameter in ecx model types.")
  stored <- as.numeric(ecx4param$ne_posterior)
  pairs <- list(both = hurdle_pair(ecx4param, ecx4param),
                growth = hurdle_pair(growth = ecx4param),
                survival = hurdle_pair(survival = ecx4param))
  for (nm in names(pairs)) {
    h <- pairs[[nm]]
    smooth_side <- if (nm == "survival") "survival" else "growth"
    est <- suppressMessages(nec(h))
    expect_length(est, 3)
    expect_false(anyNA(est))
    expect_equal(as.numeric(nec(h, which = smooth_side, posterior = TRUE)),
                 stored, label = nm)
    s <- summary(h)
    expect_identical(unname(s$ne_types[smooth_side]), "NSEC")
    expect_identical(unname(s$ne_types["combined"]),
                     if (nm == "both") "NSEC" else "N(S)EC")
  }
  # The smooth component is held to the extrapolation rules of an NSEC.
  expect_error(nec(pairs$growth, which = "growth", extrapolate = TRUE),
               "growth component refused extrapolate")
})

test_that("plot() draws a component fitted to one smooth equation (#487)", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  for (h in list(hurdle_pair(ecx4param, ecx4param),
                 hurdle_pair(growth = ecx4param),
                 hurdle_pair(survival = ecx4param))) {
    drawn <- suppressMessages(hurdle_legend(h, which = "all",
                                            resolution = 50))
    expect_length(drawn$legend, 3)
    expect_length(drawn$v, 3)
    expect_equal(as.numeric(drawn$v[[2]]),
                 as.numeric(nec(h, which = "growth")))
  }
})

test_that("legacy smooth hurdle components retain NSEC extrapolation rules", {
  smooth <- ecx4param
  smooth$ne_type <- NULL
  for (w in c("growth", "survival")) {
    h <- if (w == "growth") hurdle_pair(growth = smooth) else
      hurdle_pair(survival = smooth)
    expect_equal(as.numeric(nec(h, which = w, posterior = TRUE)),
                 as.numeric(smooth$ne_posterior))
    expect_error(nec(h, which = w, extrapolate = TRUE),
                 paste0(w, " component refused extrapolate:.*infinite"))
    expect_error(nec(h, extrapolate = TRUE),
                 paste0(w, " component refused extrapolate:.*infinite"))
  }
  threshold <- nec4param
  threshold$ne_type <- NULL
  expect_equal(nec(hurdle_pair(threshold, threshold), extrapolate = TRUE),
               nec(hurdle_pair(), extrapolate = TRUE))
})

test_that("nec() computes only the component which names (#487)", {
  seen <- character()
  real <- bayesnec:::hurdle_component_ne
  local_mocked_bindings(
    hurdle_component_ne = function(part, ...) {
      seen <<- c(seen, part$model)
      real(part, ...)
    },
    .package = "bayesnec"
  )
  h <- hurdle_pair(nec4param, ecx4param)
  suppressMessages(nec(h, which = "growth"))
  expect_identical(seen, "nec4param")
  seen <- character()
  suppressMessages(nec(h, which = "survival"))
  expect_identical(seen, "ecx4param")
  seen <- character()
  suppressMessages(nec(h))
  expect_identical(seen, c("nec4param", "ecx4param"))
  # A model set holding a smooth equation prints its message only where read.
  h <- hurdle_pair(survival = manec_example)
  expect_no_message(g <- nec(h, which = "growth"))
  expect_equal(as.numeric(g), as.numeric(nec(nec4param)))
  expect_message(nec(h, which = "survival"), "weighted mixture of NEC and NSEC")
})

# ---- An inline-transformed predictor in the hurdle plots ----------------------

# The packaged nec4param fit relabelled, not refitted: the formula declares
# crf(log(x)) or crf(sqrt(x)) while the stored grid and draws are those of
# crf(x). nec() then reports the stored draws as values on the declared scale.
relabelled_pair <- function(formula) {
  f <- nec4param
  f$bayesnecformula <- formula
  hurdle_pair(f, f)
}

test_that("the hurdle plot data put points and curve on one scale (#487)", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  logged <- relabelled_pair(bnf(y ~ crf(log(x), model = "nec4param")))
  plain <- hurdle_pair()
  x <- nec4param$fit$data$x
  d_log <- ggbnec_data(logged, which = "all", resolution = 50)
  # The same grid and draws, so the same frame, on the recorded scale.
  expect_equal(d_log, ggbnec_data(plain, which = "all", resolution = 50))
  expect_equal(range(d_log$curve$x), range(x))
  expect_true(all(d_log$raw$x >= min(x) & d_log$raw$x <= max(x)))
  # xform is not applied to a transformed predictor, and is to a plain one.
  expect_equal(ggbnec_data(logged, xform = exp, resolution = 50),
               ggbnec_data(logged, resolution = 50))
  d_plain <- ggbnec_data(plain, resolution = 50)
  d_plain_log <- ggbnec_data(plain, xform = log, resolution = 50)
  expect_equal(d_plain_log$raw$x, log(d_plain$raw$x))
  expect_equal(d_plain_log$curve$x, log(d_plain$curve$x))
})

test_that("the hurdle plot draws its no-effect line on the recorded scale (#487)", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  x <- nec4param$fit$data$x
  # A supplied xform returns the estimate to the recorded scale.
  logged <- relabelled_pair(bnf(y ~ crf(log(x), model = "nec4param")))
  drawn <- hurdle_legend(logged, xform = exp, resolution = 50)
  expect_equal(as.numeric(drawn$v[[1]]),
               as.numeric(nec(logged, xform = exp)))
  # Without one, the transformation is inverted on the prediction grid. Read
  # as square roots, the stored draws square to values inside the series.
  rooted <- relabelled_pair(bnf(y ~ crf(sqrt(x), model = "nec4param")))
  on_fitted <- as.numeric(suppressMessages(nec(rooted)))
  drawn <- hurdle_legend(rooted)
  line <- as.numeric(drawn$v[[1]])
  expect_equal(line, as.numeric(nec(rooted, xform = function(z) z^2)),
               tolerance = 1e-5)
  expect_true(all(line > min(x) & line < max(x)))
  expect_false(isTRUE(all.equal(line, on_fitted)))
  expect_identical(drawn$legend,
                   paste0("N(S)EC: ", signif(line[1], 2), " (",
                          signif(line[2], 2), "-", signif(line[3], 2), ")"))
})

test_that("the hurdle plot maps estimates over the stored prediction range", {
  f <- nec4param
  f$bayesnecformula <- bnf(y ~ crf(sqrt(x), model = "nec4param"))
  # Set the grid and draws together: these draws are identified above the data.
  f$pred_vals$data <- data.frame(x = seq(min(f$fit$data$x), 5,
                                         length.out = 1000))
  f$ne_posterior <- sqrt(seq(3.6, 4.4, length.out = 100))
  h <- hurdle_pair(f, f)
  for (w in c("growth", "survival", "combined")) {
    expected <- as.numeric(nec(h, which = w, xform = function(z) z^2))
    for (xr in list(c(0.1, 5), c(0.2, 1))) {
      drawn <- hurdle_legend(h, which = w, x_range = xr, resolution = 50)
      actual <- as.numeric(drawn$v[[1]])
      expect_equal(actual, expected, tolerance = 1e-5)
      expect_identical(drawn$legend,
                       paste0("N(S)EC: ", signif(expected[1], 2), " (",
                              signif(expected[2], 2), "-",
                              signif(expected[3], 2), ")"))
    }
  }
})

# ---- The growth family in the hurdle summary ----------------------------------

test_that("the hurdle summary names a Gamma growth family as Gamma (#487)", {
  g <- nec4param
  g$fit$family <- brms::brmsfamily("Gamma", link = "identity")
  expect_identical(g$fit$family$family, "gamma")
  s <- summary(hurdle_pair(growth = g))
  expect_identical(s$growth_family, "Gamma")
  expect_true(any(grepl("growth   : Gamma -- nec4param",
                        utils::capture.output(print(s)), fixed = TRUE)))
})

# ---- The messages of the nec() calls plot() makes -----------------------------

test_that("plot() on a hurdle fit prints no message from nec() (#506)", {
  if (Sys.getenv("NOT_CRAN") == "") {
    skip_on_cran()
  }
  h <- hurdle_pair(survival = manec_example)
  msgs <- character()
  withCallingHandlers(nec(h), message = function(m) {
    msgs <<- c(msgs, conditionMessage(m))
    invokeRestart("muffleMessage")
  })
  expect_true(any(grepl("weighted mixture of NEC and NSEC", msgs)))
  expect_true(any(grepl("the combined value is approximate", msgs)))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_message(plot(h, which = "all", resolution = 50))
  # Warnings still reach the caller. Only the call plot() makes warns, not
  # the calls nec() makes on each component.
  real_nec <- bayesnec:::nec
  local_mocked_bindings(
    nec = function(object, ...) {
      if (inherits(object, "bayesnechurdlefit")) {
        warning("a warning from nec()")
      }
      real_nec(object, ...)
    },
    .package = "bayesnec"
  )
  expect_warning(
    expect_no_message(plot(h, resolution = 50)),
    "a warning from nec()",
    fixed = TRUE
  )
})
