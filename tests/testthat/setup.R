library(bayesnec)
library(brms)
library(testthat)
options(mc.cores = 1)

random_filename <- function(nchar) {
  paste0(c(round(runif(nchar) * 15), sample(letters, nchar),
         sample(LETTERS, nchar))[sample(1:nchar * 3, nchar)], collapse = "")
}

add_na <- function(x, n = 3) {
  x_b <- x
  x_b[sample(seq_along(x), n)] <- NA
  x_b
}

data(nec_data)
other_data <- nec_data
colnames(other_data) <- c("a", "b")
nec_data$count <- as.integer(round(nec_data$y * 20))
nec_data$trials <- as.integer(20)
nec_data$log_x <- log(nec_data$x)

data(manec_example)
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
  # The model name is substituted into the formula text rather than referenced:
  # crf() evaluates its model argument where the formula is used, not where it
  # is written, so a variable reference is out of scope by then.
  fit$bayesnecformula <- bayesnecformula(
    stats::as.formula(paste0("exp(y) ~ crf(x, model = \"", model, "\")"))
  )
  fit
}
# NB: the returned object states a formula its stored fit was not fitted with.
# That is safe for the plotting paths, which read the formula only to decide
# whether a variable was transformed, and it is not safe for reading posterior
# quantities off the fit. Do not reuse it for anything else.

# The same fixture for a model set. Both plotting paths read the formula off
# mod_fits[[1]] alone (R/plot.R:258, R/autoplot.R:347), so that is the only
# element that has to change.
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
