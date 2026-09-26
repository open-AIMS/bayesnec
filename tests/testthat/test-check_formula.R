nec3param <- function(beta, nec, top, x) {
  top * exp(-exp(beta) * (x - nec) *
    ifelse(x - nec < 0, 0, 1))
}

data <- data.frame(x = seq(1, 20, length.out = 10), tr = 100, wght = c(1, 2),
                   group_1 = sample(c("a", "b"), 10, replace = TRUE),
                   group_2 = sample(c("c", "d"), 10, replace = TRUE))
data$y <- nec3param(beta = -0.2, nec = 4, top = 100, data$x)

test_that("correct classes", {
  f_0 <- y ~ crf(x, "nec3param")
  expect_s3_class(check_formula(bnf(f_0), data), "bayesnecformula")
  expect_error(check_formula(f_0, data))
  expect_error(check_formula(bnf(f_0), as.matrix(data)))
  data_x_char <- data
  data_x_char$x <- as.character(data_x_char$x)
  expect_s3_class(check_formula(bnf(f_0), data_x_char), "bayesnecformula")
})

test_that("all variables actually exist in the data frame", {
  # population-level covariates are not allowed
  f_1 <- y ~ crf(x, "nec3param") + z
  expect_error(check_formula(bnf(f_1), data),
               "not allowed in a bayesnec formula")
})

test_that("Parameter checks", {
  # expect a series of messages for because not all
  # nec models have the "bot" parameter
  f_2 <- y | trials(tr) ~ crf(x, "nec") + (nec + bot | group_1)
  expect_s3_class(check_formula(bnf(f_2), data, run_par_checks = FALSE),
                  "bayesnecformula")
  check_formula(bnf(f_2), data, run_par_checks = TRUE) |>
    expect_message("Performing single parameter checks on all models...") |>
    expect_message("\"bot\" not valid parameters") |>
    expect_message("\"bot\" not valid parameters") |>
    expect_message("\"bot\" not valid parameters") |>
    expect_message("\"bot\" not valid parameters") |>
    expect_message("\"bot\" not valid parameters") |>
    expect_message("\"bot\" not valid parameters") |>
    expect_message("\"bot\" not valid parameters")
  f_3 <- "log(y) | trials(tr) ~ crf(sqrt(x), \"nec3param\")"
  expect_s3_class(check_formula(bnf(f_3), data), "bayesnecformula")
  f_4 <- y | trials(tr) ~ crf(x, "nec3param") + ogl(group_1) + pgl(group_2)
  expect_s3_class(check_formula(bnf(f_4), data), "bayesnecformula")
  # There can only be one variable inside crf
  f_5 <- y | trials(tr) ~ crf(x + tr, "nec3param")
  expect_error(check_formula(bnf(f_5), data), "can only have one variable")
  f_6 <- y | trials(tr) ~ crf(sqrt(x + tr), "nec3param")
  expect_error(check_formula(bnf(f_6), data), "can only have one variable")
  # right hand side is either predictor or random variable
  f_7 <- y | trials(tr) ~ crf(sqrt(x), "nec3param") + tr
  expect_error(check_formula(bnf(f_7), data),
               "not allowed in a bayesnec formula")
  f_8 <- y | trials(tr) ~ crf(sqrt(x), "nec3param") + ogl(group_1)
  expect_s3_class(check_formula(bnf(f_8), data), "bayesnecformula")
  f_9 <- y | trials(tr) ~ crf(sqrt(x), "nec3param") + kgl(group_1)
  expect_error(check_formula(bnf(f_9), data),
               "not allowed in a bayesnec formula")
  # group-level cannot be numeric
  f_10 <- y | trials(tr) ~ crf(sqrt(x), "nec3param") + pgl(tr)
  expect_error(check_formula(bnf(f_10), data), "variables cannot be numeric")
  f_11 <- y | trials(tr) ~ crf(sqrt(x), "nec3param") + ogl(tr)
  expect_error(check_formula(bnf(f_11), data), "variables cannot be numeric")
  f_12 <- y | trials(tr) ~ crf(sqrt(x), "nec3param") + (nec | tr)
  expect_error(check_formula(bnf(f_12), data), "variables cannot be numeric")
  f_13 <- y | trials(tr) ~ crf(sqrt(x), "nec3param") + (nec + top | tr)
  expect_error(check_formula(bnf(f_13), data), "variables cannot be numeric")
  f_14 <- y | trials(tr) ~ crf(sqrt(x), "nec3param") + (nec + top | tr) +
    (beta | x)
  expect_error(check_formula(bnf(f_14), data), "variables cannot be numeric")
  # nested/interacting random effects accepted as long as variables exist
  f_15 <- y | trials(tr) ~ crf(sqrt(x), "nec3param") + ogl(group_1 / group_2)
  f_16 <- y | trials(tr) ~ crf(sqrt(x), "nec3param") + ogl(group_1:group_2)
  f_17 <- y | trials(tr) ~ crf(sqrt(x), "nec3param") + pgl(group_1 / group_2)
  f_18 <- y | trials(tr) ~ crf(sqrt(x), "nec3param") + pgl(group_1:group_2)
  f_19 <- y | trials(tr) ~ crf(sqrt(x), "nec3param") + (nec | group_1 / group_2)
  f_20 <- y | trials(tr) ~ crf(sqrt(x), "nec3param") + (nec | group_1:group_2)
  expect_s3_class(check_formula(bnf(f_15), data), "bayesnecformula")
  expect_s3_class(check_formula(bnf(f_16), data), "bayesnecformula")
  expect_s3_class(check_formula(bnf(f_17), data), "bayesnecformula")
  expect_s3_class(check_formula(bnf(f_18), data), "bayesnecformula")
  expect_s3_class(check_formula(bnf(f_19), data), "bayesnecformula")
  expect_s3_class(check_formula(bnf(f_20), data), "bayesnecformula")
  # complex nested inline functions for x are allowed (but will fail internally)
  f_21 <- y ~ crf(scale(sqrt(x), scale = FALSE), "nec3param")
  expect_s3_class(check_formula(bnf(f_21), data), "bayesnecformula")
})

# Each measured column is held twice, under a syntactic name and under the kind
# of name a spreadsheet gives it, so that every refusal below can be set beside
# the same formula written with a name that is accepted. See #398.
odd_data <- data.frame(x = seq(0.5, 10, length.out = 12),
                       y = rep(c(0, 4, 7, 2), 3),
                       hours = rep(c(1, 2), 6),
                       status = rep(c("none", "left"), 6),
                       grp = rep(c("a", "b"), each = 6))
odd_data[["conc mg"]] <- odd_data$x
odd_data[["growth (mm)"]] <- odd_data$y
odd_data[["exposure hours"]] <- odd_data$hours
odd_data[["cens code"]] <- odd_data$status

test_that("a non-syntactic name is refused by name in each term (#398)", {
  expect_error(
    check_formula(bnf(y ~ crf(`conc mg`, "nec3param")), odd_data),
    "\"conc mg\" in crf(`conc mg`, \"nec3param\")", fixed = TRUE
  )
  expect_error(
    check_formula(bnf(y ~ crf(log(`conc mg`), "nec3param")), odd_data),
    "\"conc mg\" in crf(log(`conc mg`), \"nec3param\")", fixed = TRUE
  )
  # The call the issue was filed on, reached through model.frame().
  expect_error(
    model.frame(bnf(y | rate(`exposure hours`) ~ crf(x, "nec3param")),
                data = odd_data),
    "\"exposure hours\" in rate(`exposure hours`)", fixed = TRUE
  )
  expect_error(
    check_formula(bnf(y | cens(`cens code`) ~ crf(x, "nec3param")), odd_data),
    "\"cens code\" in cens(`cens code`)", fixed = TRUE
  )
  expect_error(
    check_formula(bnf(`growth (mm)` ~ crf(x, "nec3param")), odd_data),
    "\"growth (mm)\" in the response", fixed = TRUE
  )
  expect_error(
    check_formula(bnf(y ~ crf(x, "nec3param") + pgl(`cens code`)), odd_data),
    "\"cens code\" in pgl(`cens code`)", fixed = TRUE
  )
})

test_that("the same formulas with syntactic names are unchanged (#398)", {
  f_list <- list(
    bnf(y ~ crf(x, "nec3param")),
    bnf(y ~ crf(log(x), "nec3param")),
    bnf(y | rate(hours) ~ crf(x, "nec3param")),
    bnf(y | cens(status) ~ crf(x, "nec3param")),
    bnf(y ~ crf(x, "nec3param") + pgl(status))
  )
  for (f in f_list) {
    expect_identical(check_formula(f, odd_data), f)
  }
  mf <- model.frame(bnf(y | rate(hours) ~ crf(x, "nec3param")), data = odd_data)
  expect_identical(mf$hours, odd_data$hours)
})

test_that("every offending name is reported in one refusal (#398)", {
  f <- bnf(`growth (mm)` | cens(`cens code`) ~ crf(`conc mg`, "nec3param"))
  err <- expect_error(check_formula(f, odd_data), "not syntactic R names")
  msg <- conditionMessage(err)
  expect_match(msg, "\"growth (mm)\" in the response", fixed = TRUE)
  expect_match(msg, "\"cens code\" in cens(`cens code`)", fixed = TRUE)
  expect_match(msg, "\"conc mg\" in crf(`conc mg`, \"nec3param\")",
               fixed = TRUE)
  # The suggested replacements are make.names()'s own, in the same order.
  expect_match(msg, "\"growth..mm.\"", fixed = TRUE)
  expect_match(msg, "\"cens.code\"", fixed = TRUE)
  expect_match(msg, "\"conc.mg\"", fixed = TRUE)
  # A name used in two terms is reported once, with both terms.
  f_2 <- bnf(y | cens(status, `conc mg`) ~ crf(`conc mg`, "nec3param"))
  expect_error(
    check_formula(f_2, odd_data),
    "\"conc mg\" in cens(status, `conc mg`) and crf(`conc mg`, \"nec3param\")",
    fixed = TRUE
  )
  # make.names() also rejects a leading digit and a reserved word.
  digit_data <- odd_data
  digit_data[["1st"]] <- digit_data$x
  expect_error(check_formula(bnf(y ~ crf(`1st`, "nec3param")), digit_data),
               "\"1st\" in crf(`1st`, \"nec3param\")", fixed = TRUE)
  # Two names that make.names() maps to the same name are given distinct
  # suggestions.
  clash_data <- odd_data
  clash_data[["conc-mg"]] <- clash_data$hours
  err <- expect_error(
    check_formula(bnf(y | rate(`conc-mg`) ~ crf(`conc mg`, "nec3param")),
                  clash_data),
    "not syntactic R names"
  )
  expect_match(conditionMessage(err), "gives \"conc.mg\", \"conc.mg.1\".",
               fixed = TRUE)
})

test_that("a model set held in a non-syntactic variable is accepted (#398)", {
  # Only the predictor of crf() is checked. The model argument is evaluated by
  # get_model_from_formula() and never reaches brms, so this call already
  # worked, and the refusal must not take it away.
  `nec models` <- c("nec3param", "nec4param")
  f <- bnf(y ~ crf(x, `nec models`))
  expect_identical(check_formula(f, odd_data), f)
  expect_named(make_brmsformula(f, data = odd_data),
               c("nec3param", "nec4param"))
})

test_that("disp() given a non-syntactic variable is accepted (#398)", {
  # disp() given anything but a one-sided formula names a variance function,
  # which parse_disp_term() evaluates in the formula's environment, as it does
  # the crf() model set. It names no column and never reaches brms by name.
  pos_data <- odd_data
  pos_data$y <- pos_data$y + 1
  `my vf` <- "power"
  f <- bnf(y ~ crf(x, "nec3param") + disp(`my vf`))
  expect_identical(check_formula(f, pos_data), f)
  expect_named(make_brmsformula(f, data = pos_data,
                                family = Gamma(link = "identity")),
               "nec3param")
  # A one-sided formula in disp() is a sub-model on columns, and is checked.
  expect_error(
    check_formula(bnf(y ~ crf(x, "nec3param") + disp(~ `conc mg`)), pos_data),
    "\"conc mg\" in disp(~`conc mg`)", fixed = TRUE
  )
})

test_that("bnec(), make_brmsformula() and get_priors() refuse first (#398)", {
  fits <- 0L
  local_mocked_bindings(
    fit_bayesnec = function(...) {
      fits <<- fits + 1L
      stop("a model fit was started")
    },
    .package = "bayesnec"
  )
  # A model set, because bnec() wraps each model's fit in try(): a refusal
  # raised per model would be printed once for each and the call would end on
  # "None of the models fit successfully" instead of this message.
  expect_error(
    bnec(y | rate(`exposure hours`) ~ crf(x, c("nec3param", "ecx4param")),
         data = odd_data),
    "\"exposure hours\" in rate(`exposure hours`)", fixed = TRUE
  )
  expect_error(
    bnec(`growth (mm)` ~ crf(x, c("nec3param", "ecx4param")),
         data = odd_data),
    "\"growth (mm)\" in the response", fixed = TRUE
  )
  expect_error(
    bnec(y ~ crf(`conc mg`, c("nec3param", "ecx4param")), data = odd_data),
    "\"conc mg\" in crf(`conc mg`, c(\"nec3param\", \"ecx4param\"))",
    fixed = TRUE
  )
  expect_equal(fits, 0L)
  expect_error(
    make_brmsformula(y | cens(`cens code`) ~
                       crf(x, c("nec3param", "ecx4param")),
                     data = odd_data),
    "\"cens code\" in cens(`cens code`)", fixed = TRUE
  )
  # A bare predictor, singly and in a set. make_brmsformula() rebuilds the
  # crf() term from deparsed text in single_model_formula() before it reaches
  # model.frame(), so these reach the refusal only through its own call.
  expect_error(
    make_brmsformula(y ~ crf(`conc mg`, "nec3param"), data = odd_data),
    "\"conc mg\" in crf(`conc mg`, \"nec3param\")", fixed = TRUE
  )
  expect_error(
    make_brmsformula(y ~ crf(`conc mg`, c("nec3param", "ecx4param")),
                     data = odd_data),
    "\"conc mg\" in crf(`conc mg`, c(\"nec3param\", \"ecx4param\"))",
    fixed = TRUE
  )
  # get_priors() builds the same per-model formula, after its own
  # model.frame().
  expect_error(
    get_priors(y ~ crf(`conc mg`, c("nec3param", "ecx4param")),
               data = odd_data),
    "\"conc mg\" in crf(`conc mg`, c(\"nec3param\", \"ecx4param\"))",
    fixed = TRUE
  )
})

test_that("bnec_group() and bnec_hurdle() refuse before any fit (#398)", {
  fits <- 0L
  local_mocked_bindings(
    bnec = function(...) {
      fits <<- fits + 1L
      stop("a component or level fit was started")
    },
    .package = "bayesnec"
  )
  expect_error(
    bnec_group(y ~ crf(`conc mg`, "nec3param"), data = odd_data,
               group_var = "grp"),
    "\"conc mg\" in crf(`conc mg`, \"nec3param\")", fixed = TRUE
  )
  expect_error(
    bnec_hurdle(y ~ crf(`conc mg`, "nec3param"), data = odd_data),
    "\"conc mg\" in crf(`conc mg`, \"nec3param\")", fixed = TRUE
  )
  expect_error(
    bnec_hurdle(`growth (mm)` ~ crf(x, "nec3param"), data = odd_data),
    "\"growth (mm)\" in the response", fixed = TRUE
  )
  expect_equal(fits, 0L)
})
