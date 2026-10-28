# bnec_record() reports what bnec() did to the request before fitting. The set
# as requested, the set attempted, the reason for the difference, and what was
# altered in the response are what a methods section has to state, and all of
# it used to exist only as console output. See #261 and #93.

test_that("amend_requested extends the request with what was added", {
  expect_equal(bayesnec:::amend_requested(c("a", "b"), NULL), c("a", "b"))
  expect_equal(bayesnec:::amend_requested(c("a", "b"), "c"), c("a", "b", "c"))
  # A model asked for twice is requested once.
  expect_equal(bayesnec:::amend_requested(c("a", "b"), "b"), c("a", "b"))
})

test_that("amend_exclusions keeps requested partitioned exactly", {
  # Three things can put an equation in `requested` but not in the set
  # attempted, and all three have to be recorded or the partition the record
  # documents does not hold.
  rec <- list(
    requested = c("a", "b", "c"),
    attempted = c("a", "b"),
    excluded = data.frame(model = "c", reason = "not valid for this family",
                          stringsAsFactors = FALSE)
  )
  # (i) declined by this call's check_models().
  now <- data.frame(model = "b", reason = "declined now",
                    stringsAsFactors = FALSE)
  out <- bayesnec:::amend_exclusions(rec, now, "a", NULL)
  expect_setequal(c("a", out$model), rec$requested)
  # (ii) declined in the call being amended, and not asked for again: the
  # original reason is kept rather than replaced by a generic one.
  expect_equal(out$reason[out$model == "c"], "not valid for this family")
  # (iii) dropped here.
  out2 <- bayesnec:::amend_exclusions(rec, NULL, "a", NULL)
  expect_equal(out2$reason[out2$model == "b"], "dropped by amend()")
  # An added model that is attempted adds nothing to the table.
  out3 <- bayesnec:::amend_exclusions(rec, NULL, c("a", "b", "d"), "d")
  expect_equal(sort(out3$model), "c")
})

test_that("bnec_record refuses an object it cannot describe", {
  expect_error(bnec_record(nec_data), "not a bayesnecfit")
})

test_that("bnec() records the request, and leaves $model a plain string", {
  # Two things at once, because both need a real fit. The exclusion record is
  # attached to `model` as an attribute; unstripped, the single-model branch
  # passed that vector to fit_bayesnec(), which stores it as out$model, so
  # every single-model fit printed the record and compared unequal to the
  # plain string it used to be. See #261.
  skip_on_cran()
  set.seed(410)
  x <- rep(seq(0, 5, length.out = 20), 3)
  y <- 3 * exp(-exp(-0.5) * pmax(x - 2, 0)) + rnorm(length(x), 0, 0.1)
  d <- data.frame(x = x, y = y)
  fit <- suppressWarnings(suppressMessages(
    bnec(y ~ crf(x, model = "nec4param"), data = d, family = gaussian(),
         chains = 2, iter = 400, warmup = 200, seed = 410, refresh = 0)
  ))
  skip_if_not(is_bayesnecfit(fit), "the single candidate had to fit")

  expect_identical(fit$model, "nec4param")
  expect_null(attributes(fit$model))

  rec <- bnec_record(fit)
  expect_named(rec, c("requested", "attempted", "excluded", "substitutions"))
  expect_equal(rec$requested, "nec4param")
  expect_equal(rec$attempted, "nec4param")
  expect_equal(nrow(rec$excluded), 0)
  expect_null(rec$substitutions)
  # requested is partitioned by attempted and the exclusion table.
  expect_setequal(c(rec$attempted, rec$excluded$model), rec$requested)

  # The record survives an update. expand_nec() builds a new object, so an
  # attribute not re-attached is lost, and bnec_record() then returned NULL for
  # a fit this version had recorded -- the one thing the documented NULL is
  # meant to rule out.
  upd <- suppressWarnings(suppressMessages(
    update(fit, chains = 2, iter = 400, warmup = 200, refresh = 0)
  ))
  expect_equal(bnec_record(upd)$requested, "nec4param")
  expect_equal(bnec_record(upd)$attempted, "nec4param")
  # The record keeps its shape. `$<-` with a NULL value deletes the name, so an
  # update that substituted nothing returned three elements where bnec() and
  # amend() return four.
  expect_named(bnec_record(upd),
               c("requested", "attempted", "excluded", "substitutions"))

  # And an amend rebuilds it for the set it produced, rather than dropping it.
  # Amended by the weighting method rather than by adding an equation: this
  # exercises the same rebuild in amend_model_set() without compiling a second
  # Stan model, and the set arithmetic it would have tested is covered by the
  # amend_requested() and amend_exclusions() blocks above.
  amended <- suppressWarnings(suppressMessages(
    amend(fit, loo_controls = list(weights = list(method = "pseudobma")))
  ))
  rec2 <- bnec_record(amended)
  expect_named(rec2, c("requested", "attempted", "excluded", "substitutions"))
  expect_equal(rec2$requested, "nec4param")
  expect_setequal(c(rec2$attempted, rec2$excluded$model), rec2$requested)
  # amend() fits from the stored data with skip_check = TRUE, so it makes no
  # substitution of its own and the one bnec() recorded still stands.
  expect_equal(rec2$substitutions, rec$substitutions)
})

# ---- The record through pull_out(), c(), + and attach_failed_models() (#522) --

# manec_example with the record bnec() would attach to a request for four
# equations: nechorme4 declined, ecxsigm attempted and failed, and a response
# substitution made.
recorded_manec <- function() {
  m <- bayesnec:::attach_bnec_record(
    manec_example,
    requested = c("nec4param", "ecx4param", "nechorme4", "ecxsigm"),
    attempted = c("nec4param", "ecx4param", "ecxsigm"),
    excluded = data.frame(model = "nechorme4", reason = "declined",
                          stringsAsFactors = FALSE),
    substitutions = data.frame(variable = "response",
                               rule = "shifted off one", n_rows = 2L,
                               from = 1, to = 0.999, reason = "beta",
                               remedy = NA_character_,
                               stringsAsFactors = FALSE)
  )
  bayesnec:::attach_failed_models(
    m, list(ecxsigm = list(model = "ecxsigm", message = "failed",
                           prior = NULL, init = NULL))
  )
}

expect_partitioned <- function(rec) {
  expect_setequal(c(rec$attempted, rec$excluded$model), rec$requested)
  expect_length(intersect(rec$attempted, rec$excluded$model), 0)
}

test_that("attach_failed_models() keeps every attribute", {
  m <- recorded_manec()
  expect_identical(bnec_record(m)$attempted,
                   c("nec4param", "ecx4param", "ecxsigm"))
  expect_identical(names(failed_models(m)), "ecxsigm")
  versioned <- bayesnec:::attach_version(manec_example)
  attr(versioned, "bnec_record") <- bnec_record(m)
  out <- bayesnec:::attach_failed_models(versioned, failed_models(m))
  expect_identical(attributes(out)[c("bnec_record", "bnec_version", "class")],
                   attributes(versioned)[c("bnec_record", "bnec_version",
                                           "class")])
  # A second call replaces the element rather than adding another.
  again <- bayesnec:::attach_failed_models(out, list(a = list(model = "a")))
  expect_identical(names(failed_models(again)), "a")
  expect_identical(sum(names(again) == "failed_models"), 1L)
})

test_that("pull_out() keeps the record of the equations it keeps", {
  m <- recorded_manec()
  p <- suppressMessages(pull_out(m, model = "nec4param"))
  rec <- bnec_record(p)
  expect_named(rec, c("requested", "attempted", "excluded", "substitutions"))
  expect_identical(rec$requested, bnec_record(m)$requested)
  # A failure pull_out() keeps leaves its equation attempted.
  expect_identical(rec$attempted, c("nec4param", "ecxsigm"))
  expect_identical(rec$excluded$reason[rec$excluded$model == "ecx4param"],
                   "not kept by pull_out()")
  expect_identical(rec$excluded$reason[rec$excluded$model == "nechorme4"],
                   "declined")
  expect_identical(rec$substitutions, bnec_record(m)$substitutions)
  expect_partitioned(rec)
  # An object with no record still has none.
  expect_null(bnec_record(suppressMessages(
    pull_out(manec_example, model = "nec4param")
  )))
})

test_that("c() and + join the records of the objects they combine", {
  m <- recorded_manec()
  a <- suppressMessages(pull_out(m, model = "nec4param"))
  b <- suppressMessages(pull_out(m, model = "ecx4param"))
  ab <- suppressWarnings(suppressMessages(c(a, b)))
  rec <- bnec_record(ab)
  # The two halves rejoin to the record of the set they were pulled from.
  expect_setequal(rec$requested, bnec_record(m)$requested)
  expect_setequal(rec$attempted, bnec_record(m)$attempted)
  expect_identical(rec$excluded, bnec_record(m)$excluded)
  expect_identical(rec$substitutions, bnec_record(m)$substitutions)
  expect_partitioned(rec)
  expect_identical(names(failed_models(ab)), "ecxsigm")
  plus <- suppressWarnings(suppressMessages(a + b))
  expect_identical(bnec_record(plus), rec)
  expect_identical(failed_models(plus), failed_models(ab))
  # The installation record is kept beside it.
  expect_identical(bnec_version(ab)$model, c("nec4param", "ecx4param"))
})

test_that("c() returns no record where an object combined has none", {
  a <- suppressMessages(pull_out(recorded_manec(), model = "nec4param"))
  b <- suppressMessages(pull_out(manec_example, model = "ecx4param"))
  expect_null(bnec_record(suppressWarnings(suppressMessages(c(a, b)))))
  expect_null(bnec_record(suppressWarnings(suppressMessages(c(b, a)))))
})

test_that("c() leaves out the failure of an equation another object fitted", {
  a <- suppressMessages(pull_out(recorded_manec(), model = "nec4param"))
  b <- suppressMessages(pull_out(manec_example, model = "ecx4param"))
  b <- bayesnec:::attach_failed_models(
    b, list(nec4param = list(model = "nec4param", message = "b failed"),
            ecxsigm = list(model = "ecxsigm", message = "b also failed"))
  )
  ab <- suppressWarnings(suppressMessages(c(a, b)))
  # nec4param is held, from a; ecxsigm keeps the failure a records first.
  expect_identical(names(failed_models(ab)), "ecxsigm")
  expect_identical(failed_models(ab)$ecxsigm$message, "failed")
})

test_that("joined_record() joins exclusions that differ between objects", {
  none <- data.frame(model = character(), reason = character(),
                     stringsAsFactors = FALSE)
  sub <- data.frame(variable = "response", rule = "shifted off zero",
                    stringsAsFactors = FALSE)
  rec_a <- list(requested = c("A", "B", "C"), attempted = "A",
                excluded = data.frame(model = c("B", "C"),
                                      reason = c("r1", "r2"),
                                      stringsAsFactors = FALSE),
                substitutions = NULL)
  rec_b <- list(requested = c("B", "C", "D"), attempted = "B",
                excluded = data.frame(model = c("C", "D"),
                                      reason = c("r3", "r4"),
                                      stringsAsFactors = FALSE),
                substitutions = sub)
  out <- bayesnec:::joined_record(list(rec_a, rec_b), c("A", "B"), "gone")
  expect_identical(out$requested, c("A", "B", "C", "D"))
  expect_identical(out$attempted, c("A", "B"))
  # B is attempted by the second object, so it is not excluded; C keeps the
  # first object's reason.
  expect_identical(out$excluded$model, c("C", "D"))
  expect_identical(out$excluded$reason, c("r2", "r4"))
  expect_identical(out$substitutions, sub)
  expect_partitioned(out)
  # An equation attempted and no longer kept takes the reason given.
  out2 <- bayesnec:::joined_record(list(rec_a, rec_b), "A", "gone")
  expect_identical(out2$excluded$reason[out2$excluded$model == "B"], "gone")
  expect_partitioned(out2)
  # Substitutions recorded by both objects are listed once.
  rec_a$substitutions <- sub
  expect_identical(
    bayesnec:::joined_record(list(rec_a, rec_b), "A", "gone")$substitutions,
    sub
  )
  expect_null(bayesnec:::joined_record(list(rec_a, NULL), "A", "gone"))
  # Without an earlier reason, B and C take the one given.
  rec_a$excluded <- none
  expect_identical(
    bayesnec:::joined_record(list(rec_a), "A", "gone")$excluded$reason,
    c("gone", "gone")
  )
})

test_that("a hurdle pair keeps each component's record", {
  m <- recorded_manec()
  h <- structure(list(growth = m, survival = m, data = m$mod_fits[[1]]$fit$data,
                      formula = m$mod_fits[[1]]$bayesnecformula, y_var = "y",
                      n_exposed = 1L, n_dead = 0L),
                 class = c("bayesnechurdlefit", "bnecfit"))
  p <- suppressMessages(pull_out(h, model = "nec4param"))
  q <- suppressMessages(pull_out(h, model = "ecx4param"))
  for (part in c("growth", "survival")) {
    expect_identical(bnec_record(p[[part]])$attempted,
                     c("nec4param", "ecxsigm"))
  }
  pq <- suppressWarnings(suppressMessages(c(p, q)))
  for (part in c("growth", "survival")) {
    expect_setequal(bnec_record(pq[[part]])$attempted,
                    bnec_record(m)$attempted)
    expect_identical(bnec_record(pq[[part]])$excluded,
                     bnec_record(m)$excluded)
  }
})
