# Section 4 of example8: within-concentration grouping on one plate.
#
# The plate is Zn 23Apr24 Rep6B, 15 minutes: the primary arm of the dataset,
# and the plate with the best response gradient (mid-band 40.9 per cent against
# a range of 20.5 to 45.5 across the 17 plates of that arm) outside the two-plate
# October batch, whose gain sits 4.6-fold below every other batch. A single
# plate is fitted because a plate is the only portion of these data free of the
# auto-scale gain differences that section 5 is about.
args <- commandArgs(trailingOnly = TRUE)
arm <- args[1]                        # "ungrouped" or "ogl"
out <- args[2]

suppressMessages(devtools::load_all(
  Sys.getenv("BAYESNEC_SRC", "."), quiet = TRUE))

d <- droplevels(subset(lum31, plate == "Zn 23Apr24 Rep6B" & minutes == 15L))

form <- if (arm == "ogl") {
  rlu_cens | cens(censoring) ~ crf(log(conc), "all") + ogl(conc_group)
} else {
  rlu_cens | cens(censoring) ~ crf(log(conc), "all")
}

# adapt_delta 0.99 rather than the default 0.80. At the default, ecxll5 gave 880
# divergent transitions of 16000 in the ungrouped fit and 726 with the term,
# while taking about a fifth of the stacking weight -- a contributing equation
# failing, which CLAUDE.md section 11 requires be resolved rather than reported.
# Fitted alone, ecxll5 gives 3 divergent transitions at adapt_delta 0.95 and 0 at
# 0.99, so the cause is the step size and not the five curve parameters against
# 11 concentrations. One control setting is used for the whole set.
t0 <- Sys.time()
fit <- bnec(form, data = d, family = Gamma(link = "identity"),
            chains = 4, iter = 8000, warmup = 4000, seed = 228,
            cores = 4, refresh = 0, control = list(adapt_delta = 0.99))
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))

saveRDS(fit, out)

# Diagnostics per equation: divergent transitions of 16000, maximum R-hat and
# minimum bulk effective sample size over all sampled parameters.
diag_one <- function(bf) {
  np <- brms::nuts_params(bf)
  drv <- sum(np$Value[np$Parameter == "divergent__"])
  su <- posterior::summarise_draws(posterior::as_draws_array(bf),
                                   "rhat", "ess_bulk")
  su <- su[!is.na(su$rhat), ]
  c(divergent = drv, max_rhat = max(su$rhat), min_ess = min(su$ess_bulk))
}
tab <- do.call(rbind, lapply(names(fit$mod_fits), function(m)
  data.frame(model = m, t(diag_one(fit$mod_fits[[m]]$fit)))))
tab$weight <- round(as.numeric(fit$mod_stats$wi[match(tab$model, fit$mod_stats$model)]), 4)
tab$max_rhat <- round(tab$max_rhat, 4)
tab$min_ess <- round(tab$min_ess)
cat("\n#### arm:", arm, " elapsed", round(elapsed, 1), "min\n")
print(tab, row.names = FALSE)
cat("\nfailed:", paste(fit$failed_models, collapse = ", "), "\n")
write.csv(tab, sub("\\.rds$", "_diag.csv", out), row.names = FALSE)

# Estimates on the recorded concentration scale. The predictor is log(conc), so
# nec() and ecx() return values on that scale and need the inverse.
n <- nec(fit, xform = exp); e10 <- ecx(fit, ecx_val = 10, xform = exp)
e50 <- ecx(fit, ecx_val = 50, xform = exp)
est <- data.frame(arm = arm, quantity = c("NEC", "EC10", "EC50"),
                  rbind(n, e10, e50))
names(est)[3:5] <- c("est", "lo", "hi")
est$width <- est$hi - est$lo
cat("\n#### estimates, mg/L\n"); print(est, row.names = FALSE)
write.csv(est, sub("\\.rds$", "_est.csv", out), row.names = FALSE)

sd_tab <- do.call(rbind, lapply(names(fit$mod_fits), function(m) {
  s <- summary(fit$mod_fits[[m]]$fit)$random$conc_group
  if (is.null(s)) return(NULL)
  data.frame(model = m, sd = round(s[1, "Estimate"], 3),
             lo = round(s[1, "l-95% CI"], 3), hi = round(s[1, "u-95% CI"], 3))
}))
if (!is.null(sd_tab)) {
  cat("\n#### group-level sd\n"); print(sd_tab, row.names = FALSE)
  write.csv(sd_tab, sub("\\.rds$", "_sd.csv", out), row.names = FALSE)
}
