library(bayesnec)
library(brms)
library(testthat)

# Namespace stats and utils calls explicitly: testthat worker subprocesses
# may not attach those packages before sourcing this setup.
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

# Collect and muffle warnings while retaining the result. Count warnings
# as well as matching their text: expect_warning() alone accepts repeats.
collect_warnings <- function(expr) {
  warnings <- character()
  value <- withCallingHandlers(expr, warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = warnings)
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

# Change only formula metadata on a stored fit to exercise response-transform
# handling in plotting. Use exp(y): this fixture includes negative responses,
# so log(y) would produce NaN.
transformed_response_fit <- function(fit, model) {
  # Embed the equation name in the formula text: plotting fixtures inspect
  # that text rather than resolving a model variable.
  fit$bayesnecformula <- bayesnecformula(
    stats::as.formula(paste0("exp(y) ~ crf(x, model = \"", model, "\")"))
  )
  fit
}
# This metadata-only fixture was not fitted with its new formula. Use it
# only for plotting-path checks, never to validate posterior quantities.

# Change only the first component, which the model-average plotting path
# reads. Do not use this fixture to plot named equations or all_models: those
# paths read each component's own formula.
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

# Use a response name other than y to check column-specific messages. Each
# near variant changes one value off the bound at a declining endpoint; it
# must pass the refusal shared by bnec() and get_priors().
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
                    "lower", "cover", low, 0.1),
    zib_one = one(data.frame(x = x, cover = rep(1, n)), "zero_inflated_beta",
                  "cover", "upper", "cover", top, 0.9),
    zib_zero = one(data.frame(x = x, cover = rep(0, n)), "zero_inflated_beta",
                   "cover", "lower", "cover", low, 0.1)
  )
}

# No equation, including ecxflat, can fit these all-zero beta components.
at_bound_refused <- c("beta_zero", "zib_zero")
