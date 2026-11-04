# Run from the package root with one output directory, for example:
# Rscript notes/scripts/regularizing_shape_prior_fits.R ignore/prior_shape_535
# Use an empty output directory when checking a new package revision.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("Supply one output directory.", call. = FALSE)
}
out_dir <- args[[1]]
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
pkgload::load_all(".", quiet = TRUE)

make_case <- function(name) {
  seed <- switch(name, f_complete = 535L, f_sparse = 537L,
                 d_standard = 536L, d_nec = 538L, d_steep = 539L)
  set.seed(seed)
  if (name == "f_complete") {
    x <- rep(seq(0, 10, by = 1), each = 8)
    mu <- 0.1 + 0.8 / (1 + exp(x - 5))^1.5
    y <- stats::rnorm(length(x), mu, 0.04)
  } else if (name == "f_sparse") {
    x <- rep(seq(0, 4, by = 0.5), each = 4)
    mu <- 0.1 + 0.8 / (1 + exp(x - 5))^1.5
    y <- stats::rnorm(length(x), mu, 0.04)
  } else if (name == "d_standard") {
    x <- rep(seq(0, 5, by = 0.5), each = 8)
    mu <- 0.9 * exp(-0.1 * x^1.4)
    y <- stats::rbeta(length(x), mu * 80, (1 - mu) * 80)
  } else if (name == "d_nec") {
    x <- rep(seq(0, 6, by = 0.5), each = 7)
    mu <- 0.9 * exp(-0.18 * pmax(x - 2, 0)^1.4)
    y <- stats::rbeta(length(x), mu * 80, (1 - mu) * 80)
  } else if (name == "d_steep") {
    x <- rep(seq(0, 5, by = 0.5), each = 8)
    mu <- 0.9 * exp(-0.003 * x^4)
    y <- stats::rbeta(length(x), mu * 80, (1 - mu) * 80)
  } else {
    stop("Unknown case: ", name, call. = FALSE)
  }
  list(name = name, seed = seed, data = data.frame(x = x, y = y),
       family = if (startsWith(name, "f")) stats::gaussian() else
         brms::Beta(link = "identity"),
       model = switch(name, f_complete = "ecxll5", f_sparse = "ecxll5",
                      d_standard = "ecxsigm", d_nec = "necsigm",
                      d_steep = "ecxsigm"),
       par = if (startsWith(name, "f")) "f" else "d",
       asymptote_observed = name != "f_sparse",
       pilot_iter = switch(name, f_sparse = 1000L, d_steep = 800L, 600L),
       pilot_adapt = if (name %in% c("f_sparse", "d_steep")) 0.97 else 0.95)
}

fit_arm <- function(case, arm) {
  path <- file.path(out_dir, paste0(case$name, "_", arm, ".rds"))
  if (file.exists(path)) {
    return(readRDS(path))
  }
  form <- stats::as.formula(paste0('y ~ crf(x, model = "', case$model,
                                   '")'))
  prior <- bayesnec::get_priors(
    form, data = case$data, family = case$family,
    prior_type = "regularizing",
    asymptote_observed = case$asymptote_observed
  )
  # Supplying the complete prior keeps every other entry fixed across arms.
  prior$prior[prior$nlpar == case$par] <-
    if (arm == "baseline") "normal(0, 5)" else "normal(0, 1)"
  fit <- bayesnec::bnec(
    form, data = case$data, family = case$family, prior = prior,
    prior_type = "regularizing",
    asymptote_observed = case$asymptote_observed,
    chains = 2, cores = 2, iter = case$pilot_iter,
    warmup = case$pilot_iter / 2, seed = case$seed, refresh = 0,
    control = list(adapt_delta = case$pilot_adapt)
  )
  # brms stores two initial-value lists above; four chains need fresh values.
  fit$fit <- stats::update(
    fit$fit, chains = 4, cores = 4, iter = 2000, warmup = 1000,
    seed = 535, refresh = 0, init = "random", recompile = FALSE,
    control = list(adapt_delta = 0.99, max_treedepth = 12)
  )
  saveRDS(fit, path)
  fit
}

fit_comparator <- function(case, model) {
  path <- file.path(out_dir, paste0(case$name, "_", model, ".rds"))
  if (file.exists(path)) {
    return(readRDS(path))
  }
  form <- stats::as.formula(paste0('y ~ crf(x, model = "', model, '")'))
  fit <- bayesnec::bnec(
    form, data = case$data, family = case$family,
    prior_type = "regularizing", chains = 4, cores = 4,
    iter = 2000, warmup = 1000, seed = case$seed, refresh = 0,
    control = list(adapt_delta = 0.99, max_treedepth = 12)
  )
  saveRDS(fit, path)
  fit
}

cases <- c("f_complete", "f_sparse", "d_standard", "d_nec", "d_steep")
for (name in cases) {
  case <- make_case(name)
  saveRDS(case$data, file.path(out_dir, paste0(name, "_data.rds")))
  for (arm in c("baseline", "candidate")) {
    fit <- fit_arm(case, arm)
    ec10 <- bayesnec::ecx(fit, ecx_val = 10)
    cat(name, arm, "EC10", paste(round(ec10[1:3], 3), collapse = ","),
        "max_Rhat", round(max(brms::rhat(fit$fit), na.rm = TRUE), 3), "\n")
  }
  if (name == "f_complete") fit_comparator(case, "ecxll4")
  if (name == "d_standard") fit_comparator(case, "ecxexp")
}

writeLines(c(
  paste("Git commit:", system2("git", c("rev-parse", "HEAD"), stdout = TRUE)),
  capture.output(sessionInfo())
), file.path(out_dir, "provenance.txt"))
