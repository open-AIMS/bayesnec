# Exploratory design simulation for the hormesis vignette.
# Run from the package root with Rscript data-raw/hormesis_simulation.R.
# This is a Gaussian plug-in exercise, not a Bayesian coverage study of bnec().

set.seed(570)
mean_response <- function(x, top, amplitude, peak, decline) {
  top + amplitude * (x / peak) * exp(1 - x / peak) -
    decline * pmax(x - peak, 0)
}

crossing <- function(parameters, target, lower, upper = 6) {
  f <- function(x) do.call(mean_response, c(list(x = x), as.list(parameters))) - target
  if (f(lower) <= 0 || f(upper) >= 0) return(NA_real_)
  uniroot(f, c(lower, upper))$root
}

fit_one <- function(amplitude, replicates, concentrations, sigma = 0.08) {
  x <- rep(concentrations, each = replicates)
  y <- rnorm(length(x), mean_response(x, 1, amplitude, 1, 0.15), sigma)
  objective <- function(p) {
    sum((y - mean_response(x, p[1], p[2], p[3], p[4]))^2) / 2
  }
  fit <- tryCatch(optim(c(mean(y[x == 0]), amplitude, 1, 0.15), objective,
                        method = "L-BFGS-B", lower = c(0.3, 0.001, 0.2, 0.001),
                        upper = c(1.8, 0.8, 3, 0.8), hessian = TRUE),
                  error = function(e) NULL)
  if (is.null(fit) || fit$convergence != 0) {
    return(c(peak = NA, return = NA, lower_crossing = NA, ec10 = NA,
             top = NA, fit_amplitude = NA, decline = NA))
  }
  p <- setNames(fit$par, c("top", "amplitude", "peak", "decline"))
  inverse <- tryCatch(solve(fit$hessian), error = function(e) NULL)
  se_top <- if (is.null(inverse) || inverse[1, 1] <= 0) NA_real_ else
    sqrt(sum((y - mean_response(x, p[1], p[2], p[3], p[4]))^2) /
           (length(y) - 4) * inverse[1, 1])
  c(peak = unname(p[3]), return = crossing(p, p[1], p[3]),
    lower_crossing = if (is.na(se_top)) NA_real_ else
      crossing(p, p[1] - qnorm(0.99) * se_top, p[3]),
    ec10 = crossing(p, 0.9 * p[1], p[3]),
    top = unname(p[1]), fit_amplitude = unname(p[2]),
    decline = unname(p[4]))
}

design <- expand.grid(amplitude = c(0.1, 0.25), replicates = c(3, 10),
                      levels = c(6, 10))
results <- lapply(seq_len(nrow(design)), function(i) {
  d <- design[i, ]
  concentrations <- c(0, exp(seq(log(0.1), log(4), length.out = d$levels - 1)))
  draws <- replicate(250, fit_one(d$amplitude, d$replicates,
                                   concentrations))
  curve_x <- seq(0, 4, length.out = 101)
  curves <- apply(draws, 2, function(p) {
    if (anyNA(p[c("top", "fit_amplitude", "peak", "decline")])) {
      return(rep(NA_real_, length(curve_x)))
    }
    mean_response(curve_x, p["top"], p["fit_amplitude"], p["peak"],
                  p["decline"])
  })
  curve_row <- data.frame(
    d, conc = curve_x,
    true_mean = mean_response(curve_x, 1, d$amplitude, 1, 0.15),
    fitted_median = apply(curves, 1, median, na.rm = TRUE),
    fitted_q025 = apply(curves, 1, quantile, 0.025, na.rm = TRUE),
    fitted_q975 = apply(curves, 1, quantile, 0.975, na.rm = TRUE),
    row.names = NULL
  )
  estimates <- draws[c("peak", "return", "lower_crossing", "ec10"), ,
                     drop = FALSE]
  truth <- c(peak = 1,
             return = crossing(c(top = 1, amplitude = d$amplitude,
                                 peak = 1, decline = 0.15), 1, 1),
             lower_crossing = NA_real_,
             ec10 = crossing(c(top = 1, amplitude = d$amplitude,
                                peak = 1, decline = 0.15), 0.9, 1))
  list(summary = data.frame(
    d, metric = rownames(estimates),
    fit_success = rowMeans(is.finite(estimates)),
    median = apply(estimates, 1, median, na.rm = TRUE),
    q025 = apply(estimates, 1, quantile, 0.025, na.rm = TRUE),
    q975 = apply(estimates, 1, quantile, 0.975, na.rm = TRUE),
    truth = truth[rownames(estimates)], row.names = NULL
  ), curves = curve_row)
})
out <- do.call(rbind, lapply(results, `[[`, "summary"))
write.csv(out, "inst/extdata/hormesis_simulation_summary.csv", row.names = FALSE)
write.csv(do.call(rbind, lapply(results, `[[`, "curves")),
          "inst/extdata/hormesis_simulation_fitted_curves.csv", row.names = FALSE)
print(out, row.names = FALSE, digits = 3)
