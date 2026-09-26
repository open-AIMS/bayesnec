library(bayesnec)
library(brms)
library(testthat)

# stats:: and utils:: are namespaced explicitly because this file also runs in
# each of testthat's parallel worker subprocesses, which attach far less than an
# interactive session: data() and runif() are not visible there, and the setup
# fails before a single test runs. Nothing else in this file reaches outside
# base.
options(mc.cores = 1)

random_filename <- function(nchar) {
  paste0(c(round(stats::runif(nchar) * 15), sample(letters, nchar),
         sample(LETTERS, nchar))[sample(1:nchar * 3, nchar)], collapse = "")
}

add_na <- function(x, n = 3) {
  x_b <- x
  x_b[sample(seq_along(x), n)] <- NA
  x_b
}

utils::data(nec_data)
other_data <- nec_data
colnames(other_data) <- c("a", "b")
nec_data$count <- as.integer(round(nec_data$y * 20))
nec_data$trials <- as.integer(20)
nec_data$log_x <- log(nec_data$x)

utils::data(manec_example)
nec4param <- pull_out(manec_example, model = "nec4param") |>
  suppressMessages() |>
  suppressWarnings()
ecx4param <- pull_out(manec_example, model = "ecx4param") |>
  suppressMessages() |>
  suppressWarnings()

# Give a stored fit a formula that transforms its RESPONSE and nothing else.
# This is the fixture that reproduces #268 without fitting anything: no packaged
# fit transforms its response, and find_transformations() answers for the
# formula as a whole, so a transformation here is enough to make the plotting
# paths treat the untransformed predictor as transformed. exp() rather than
# log(): manec_example's response reaches -6.9 and log() of it is NaN, and only
# the presence of a transformation matters.
#
# Shared by test-plot.R and test-autoplot.R, which pin the same defect on the
# base-graphics and ggplot2 paths and would otherwise define it twice.
transformed_response_fit <- function(fit, model) {
  # The model name is substituted into the formula text rather than referenced,
  # so that the stored formula records the equation itself. Since #319 crf()
  # resolves a variable reference in the environment the formula was written
  # in, so a reference would work, but it would deparse as the variable name
  # and these fixtures are read by the plotting paths as text.
  fit$bayesnecformula <- bayesnecformula(
    stats::as.formula(paste0("exp(y) ~ crf(x, model = \"", model, "\")"))
  )
  fit
}
# NB: the returned object states a formula its stored fit was not fitted with.
# That is safe for the plotting paths, which read the formula only to decide
# whether a variable was transformed, and it is not safe for reading posterior
# quantities off the fit. Do not reuse it for anything else.

# The same fixture for a model set. On the model-average branch, which is the
# one the pinning tests use, both plotting paths read the formula off
# mod_fits[[1]] alone (R/plot.R:258, R/autoplot.R:347), so that is the only
# element that has to change. Not so with all_models = TRUE: plot() then draws
# each candidate through plot.bayesnecfit, which reads that candidate's own
# formula at R/plot.R:118. Do not use this fixture on that branch.
transformed_response_manec <- function(manec) {
  mod <- names(manec$mod_fits)[1]
  manec$mod_fits[[1]] <- transformed_response_fit(manec$mod_fits[[1]], mod)
  manec
}

# The largest predictor value the ggplot2 path put in its plotting frame. Used
# by test-autoplot.R, and by test-plot.R to compare the two paths' xform
# decisions against each other.
gg_x_max <- function(obj, ...) {
  max(suppressMessages(ggbnec_data(obj, ...))$x_e, na.rm = TRUE)
}

# Add a group-level term to a stored fit without refitting. The brms component
# is left unchanged because the plotting path needs its fitted curve only; the
# bayesnec formula and stored data provide the grouping metadata and values.
grouped_plot_fit <- function(fit = nec4param) {
  fit$fit$data$plate <- factor(rep(letters[1:4],
                                    length.out = nrow(fit$fit$data)))
  fit$bayesnecformula <- bayesnecformula(
    stats::as.formula(paste0(
      "y ~ crf(x, model = \"", fit$model, "\") + ogl(plate)"
    ))
  )
  fit
}

# Retain a categorical column that the fitted formula does not use. The source
# rows are deliberately reversed so tests observe name-based alignment rather
# than succeeding through the common case where both frames share an order.
unfitted_group_plot_fit <- function(fit = nec4param, values = NULL) {
  d <- fit$fit$data[rev(seq_len(nrow(fit$fit$data))), , drop = FALSE]
  if (is.null(values)) {
    values <- rep(c("ambient", "warm", "hot"), length.out = nrow(d))
  }
  d$climate <- factor(values)
  bayesnec:::retain_unused_data(fit, d)
}

# A bounded response with every observation at one of its bounds, once for each
# case #400 names: bernoulli at 1 and at 0, binomial with every count equal to
# its trials and with every count 0, beta at 1, and beta at 0, which a numeric
# response of zeros reaches because it is the family chosen for one. The
# response is not called y, so that a message naming the column is seen to
# name it. `near` is the same data with one observation set off the bound, at
# the concentration where a decline would place it, which is a response that
# varies and must still pass. Shared by test-bnec.R and test-get_priors.R,
# which assert the refusal through the two entry points.
at_bound_cases <- function() {
  x <- rep(c(0.1, 0.5, 1, 3, 10, 30), each = 5)
  n <- length(x)
  exposed <- rep(c(8L, 10L), length.out = n)
  top <- which.max(x)
  low <- which.min(x)
  set_crf <- "c(\"nec3param\", \"ecx4param\")"
  f <- function(lhs) {
    stats::as.formula(paste0(lhs, " ~ crf(x, ", set_crf, ")"))
  }
  one <- function(data, family, column, bound, lhs, near_row, near_value) {
    near <- data
    near[[column]][near_row] <- near_value
    list(formula = f(lhs), data = data, near = near, family = family,
         column = column, bound = bound)
  }
  list(
    bernoulli_one = one(data.frame(x = x, alive = rep(1L, n)), "bernoulli",
                        "alive", "upper", "alive", top, 0L),
    bernoulli_zero = one(data.frame(x = x, alive = rep(0L, n)), "bernoulli",
                         "alive", "lower", "alive", low, 1L),
    binomial_trials = one(data.frame(x = x, alive = exposed,
                                     exposed = exposed),
                          "binomial", "alive", "upper",
                          "alive | trials(exposed)", top, exposed[top] - 1L),
    binomial_zero = one(data.frame(x = x, alive = rep(0L, n),
                                   exposed = exposed),
                        "binomial", "alive", "lower",
                        "alive | trials(exposed)", low, 1L),
    beta_one = one(data.frame(x = x, cover = rep(1, n)), "Beta", "cover",
                   "upper", "cover", top, 0.9),
    beta_zero = one(data.frame(x = x, cover = rep(0, n)), "Beta", "cover",
                    "lower", "cover", low, 0.1)
  )
}
