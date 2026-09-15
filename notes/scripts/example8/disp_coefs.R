# The screen printed spec_pars, which is empty once dispersion is modelled: the
# shape moves into the distributional formula. Refit the power arm and report the
# coefficients, and the CV they imply at the control and at the bottom.
suppressMessages(devtools::load_all(
  Sys.getenv("BAYESNEC_SRC", "."), quiet = TRUE))
d <- droplevels(subset(lum31, toxicant == "Zn" & minutes == 15L))
f <- bnec(rlu_cens | cens(censoring) ~ crf(log(conc), "ecxll5") + (top | plate) + disp("power"),
          data = d, family = Gamma(link = "identity"), chains = 2, iter = 2000,
          warmup = 1000, seed = 228, cores = 2, refresh = 0,
          control = list(adapt_delta = 0.95))
saveRDS(f, file.path(dirname(tempdir()), "disp_power_fit.rds"))
cat("=== all fixed effects ===\n"); print(round(brms::fixef(f$fit), 4))
nd <- bnec_newdata(f, resolution = 40)
ep <- brms::posterior_epred(f$fit, newdata = nd, re_formula = NA)
mu <- colMeans(ep)
sh <- brms::posterior_epred(f$fit, newdata = nd, re_formula = NA, dpar = "shape")
shm <- colMeans(sh)
cv <- 1/sqrt(shm)
i <- c(1, which.min(abs(mu - max(mu)/2)), length(mu))
cat("\n=== implied CV along the curve ===\n")
print(data.frame(conc = signif(exp(nd[[1]])[i], 3), fitted_mean = signif(mu[i], 4),
                 shape = round(shm[i], 1), implied_cv = round(cv[i], 4)))
cat("\nobserved within-cell CV: control 0.051, mid-series ~0.06-0.08, bottom 0.362\n")
