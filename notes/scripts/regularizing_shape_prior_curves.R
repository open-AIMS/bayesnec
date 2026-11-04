# Run from the package root:
# Rscript notes/scripts/regularizing_shape_prior_curves.R

set.seed(535)
n <- 100000L
grid <- 0:10
scales <- c(baseline = 5, candidate = 1)

curve_summary <- function(par, scale, grid) {
  draws <- stats::rnorm(n, 0, scale)
  narrow <- logical(n)
  covers <- logical(n)
  for (i in seq_len(n)) {
    y <- switch(par,
      beta = exp(-exp(draws[i]) * grid),
      slope = 1 - exp(draws[i]) * grid,
      d = exp(-0.1 * grid^exp(draws[i])),
      f = (1 + exp(grid - 5))^(-exp(draws[i]))
    )
    narrow[i] <- any(abs(diff(y)) > 0.8)
    covers[i] <- y[1] >= 0.9 && y[length(y)] <= 0.1
  }
  c(narrow = mean(narrow), covers = mean(covers))
}

for (par in c("beta", "slope", "d", "f")) {
  for (arm in names(scales)) {
    scale <- if (par %in% c("beta", "slope") && arm == "candidate") 2 else
      scales[[arm]]
    metrics <- curve_summary(par, scale, grid)
    cat(par, arm, "sd", scale, "narrow", round(metrics[["narrow"]], 4),
        "covers", round(metrics[["covers"]], 4), "\n")
  }
}

# A rate of one per physical dose unit becomes 0.001 after converting the
# concentration to units 1,000 times smaller, so its log rate moves by -log(1000).
for (scale in c(5, 2)) {
  cat("beta", "sd", scale, "P(log rate < -log(1000))",
      signif(stats::pnorm(-log(1000), sd = scale), 4), "\n")
}
for (scale in c(5, 2)) {
  q <- stats::qnorm(c(0.025, 0.975), sd = scale)
  cat("slope power exponent", "sd", scale,
      paste(signif(1 / (1 + exp(q)), 4), collapse = ","), "\n")
}
