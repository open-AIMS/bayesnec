# Is the Gamma's constant coefficient of variation the reason the variance at
# the control is too high? Observed within-cell CV is 0.051 at the control and
# 0.36 at the bottom of the series; the fitted shape gives one CV for all of it:
# 0.58 ungrouped, 0.43 with (top | plate), 0.26 with pgl(plate). A dispersion
# sub-model lets it vary. Screened at 2 chains on ecxll5, which holds most of
# the weight in the (top | plate) fit.
suppressMessages(devtools::load_all(
  Sys.getenv("BAYESNEC_SRC", "."), quiet = TRUE))
d <- droplevels(subset(lum31, toxicant == "Zn" & minutes == 15L))

arms <- list(
  constant = rlu_cens | cens(censoring) ~ crf(log(conc), "ecxll5") + (top | plate),
  power    = rlu_cens | cens(censoring) ~ crf(log(conc), "ecxll5") + (top | plate) + disp("power"),
  bydose   = rlu_cens | cens(censoring) ~ crf(log(conc), "ecxll5") + (top | plate) + disp(~log(conc))
)
for (nm in names(arms)) {
  t0 <- Sys.time()
  f <- try(bnec(arms[[nm]], data = d, family = Gamma(link = "identity"),
                chains = 2, iter = 2000, warmup = 1000, seed = 228,
                cores = 2, refresh = 0, control = list(adapt_delta = 0.95)),
           silent = TRUE)
  if (inherits(f, "try-error")) { cat(nm, ": FAILED\n"); next }
  bf <- f$fit
  np <- brms::nuts_params(bf)
  su <- posterior::summarise_draws(posterior::as_draws_array(bf), "rhat", "ess_bulk")
  su <- su[!is.na(su$rhat), ]
  # implied CV at the control and at the bottom of the series
  nd <- bnec_newdata(f, resolution = 50)
  pe <- try(posterior_epred(f, newdata = nd), silent = TRUE)
  cat(sprintf("\n== %s ==  %.1f min  divergent %d/2000  max_rhat %.4f  min_ess %.0f\n",
              nm, as.numeric(difftime(Sys.time(), t0, units = "mins")),
              sum(np$Value[np$Parameter == "divergent__"]), max(su$rhat), min(su$ess_bulk)))
  print(round(summary(bf)$spec_pars[, c("Estimate", "l-95% CI", "u-95% CI")], 3))
  ecv <- try(ecx(f, ecx_val = 10, xform = exp), silent = TRUE)
  if (!inherits(ecv, "try-error")) cat("EC10", signif(ecv, 3), "\n")
}
