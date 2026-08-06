# Phase 5 acceptance: bnec() end to end with family = beta_ub().
suppressMessages(devtools::load_all("."))
options(mc.cores = 2)

set.seed(173)
x <- rep(seq(0, 6, length.out = 8), each = 5)
mu <- 0.8 * exp(-pmax(x - 2, 0))
d <- data.frame(x = x, y = rbeta(length(mu), mu * 30, (1 - mu) * 30))

fit <- bnec(y ~ crf(x, model = "nec3param"), data = d, family = beta_ub(),
            U_loc = 1, U_scale = 0.1, chains = 2, iter = 2000, warmup = 1000,
            seed = 173, refresh = 0, backend = "cmdstanr",
            control = list(adapt_delta = 0.95))

cat("\n== class ==\n"); print(class(fit))
cat("\n== response passed raw? ==\n")
cat("max(fit data y):", sprintf("%.6f", max(fit$fit$data$y)), "\n")
cat("max(input   y):", sprintf("%.6f", max(d$y)), "  identical:",
    isTRUE(all.equal(max(fit$fit$data$y), max(d$y))), "\n")
cat("ymax in standata:", sprintf("%.6f", brms::standata(fit$fit)$ymax), "\n")

cat("\n== convergence ==\n")
cat("max rhat:", sprintf("%.4f", max(brms::rhat(fit$fit), na.rm = TRUE)), "\n")
np <- brms::nuts_params(fit$fit)
cat("divergences:", sum(np$Value[np$Parameter == "divergent__"]), "\n")

cat("\n== parameters ==\n")
print(brms::fixef(fit$fit))
dr <- brms::as_draws_df(fit$fit)
ymax <- brms::standata(fit$fit)$ymax
cat("phi median:", sprintf("%.2f", median(dr$phi)), " (truth 30)\n")
cat("U   median:", sprintf("%.4f", median(ymax + dr$delta)), " (truth 1.0)\n")
cat("top sits at the control mean, not near U:  top =",
    sprintf("%.4f", median(dr$b_top_Intercept)),
    " control mean =", sprintf("%.4f", mean(d$y[d$x == 0])),
    " U =", sprintf("%.4f", median(ymax + dr$delta)), "\n")

cat("\n== summary() ==\n")
print(summary(fit))
saveRDS(fit, "dev/prototype/phase5_fit.rds")
cat("\nwritten: dev/prototype/phase5_fit.rds\n")
