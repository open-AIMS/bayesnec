# Does the within-concentration term survive a dispersion sub-model? ogl(conc_group)
# absorbs variation the constant-CV Gamma cannot represent; disp("power") models
# that variation directly. If the group-level sd collapses once dispersion varies
# with the mean, section 4 needs a different demonstration.
suppressMessages(devtools::load_all(
  Sys.getenv("BAYESNEC_SRC", "."), quiet = TRUE))
d <- droplevels(subset(lum31, plate == "Zn 23Apr24 Rep6B" & minutes == 15L))
grid <- expand.grid(eq = c("nec3param", "ecxll3"), disp = c("constant", "power"),
                    stringsAsFactors = FALSE)
for (i in seq_len(nrow(grid))) {
  eq <- grid$eq[i]; dp <- grid$disp[i]
  f <- if (dp == "constant") {
    bayesnecformula(rlu_cens | cens(censoring) ~ crf(log(conc), eq) + ogl(conc_group))
  } else {
    bayesnecformula(rlu_cens | cens(censoring) ~ crf(log(conc), eq) + ogl(conc_group) + disp("power"))
  }
  fit <- try(bnec(f, data = d, family = Gamma(link = "identity"), chains = 2,
                  iter = 2000, warmup = 1000, seed = 228, cores = 2, refresh = 0,
                  control = list(adapt_delta = 0.95)), silent = TRUE)
  if (inherits(fit, "try-error")) { cat(eq, dp, ": FAILED\n"); next }
  s <- summary(fit$fit)$random$conc_group
  np <- brms::nuts_params(fit$fit)
  cat(sprintf("\n%-10s %-9s sd(ogl) %.3f (%.3f - %.3f)  divergent %d/2000\n",
              eq, dp, s[1, "Estimate"], s[1, "l-95% CI"], s[1, "u-95% CI"],
              sum(np$Value[np$Parameter == "divergent__"])))
}
