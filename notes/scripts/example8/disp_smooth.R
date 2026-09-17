# The fitted power-law exponent (1.26) is steeper than the within-cell variance
# supports (0.72, which simulation says is recovered near-unbiasedly at that
# value). A smooth on the dose axis does not force a functional form and shows
# where the power law departs.
suppressMessages(devtools::load_all(
  Sys.getenv("BAYESNEC_SRC", "."), quiet = TRUE))
d <- droplevels(subset(lum31, toxicant == "Zn" & minutes == 15L))
f <- bnec(rlu_cens | cens(censoring) ~ crf(log(conc), "ecxll5") + ogl(plate) + disp(~s(log(conc))),
          data = d, family = Gamma(link = "identity"), chains = 2, iter = 2000,
          warmup = 1000, seed = 228, cores = 2, refresh = 0,
          control = list(adapt_delta = 0.95))
np <- brms::nuts_params(f$fit)
cat("divergent", sum(np$Value[np$Parameter == "divergent__"]), "of 2000\n")
nd <- data.frame(conc = sort(unique(d$conc)), plate = d$plate[1], censoring = "none")
sh <- brms::posterior_epred(f$fit, newdata = nd, re_formula = NA, dpar = "shape")
mu <- brms::posterior_epred(f$fit, newdata = nd, re_formula = NA)
obs <- tapply(d$rlu_cens, d$conc, function(z) sd(z)/mean(z))
out <- data.frame(conc = signif(nd$conc, 3),
                  fitted_mean = signif(colMeans(mu), 4),
                  shape = round(colMeans(sh), 1),
                  model_cv = round(1/sqrt(colMeans(sh)), 3),
                  observed_cv = round(as.numeric(obs), 3))
print(out, row.names = FALSE)
ec <- ecx(f, ecx_val = 10, xform = exp); cat("\nEC10", signif(ec, 3), "\n")
