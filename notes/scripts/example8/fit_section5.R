# Section 5: across-concentration grouping on the whole Zn 15 minute arm,
# 748 rows over 17 plates. Run against dev with PR #304 merged locally.
args <- commandArgs(trailingOnly = TRUE)
arm <- args[1]; out <- args[2]
suppressMessages(devtools::load_all(
  Sys.getenv("BAYESNEC_SRC", "."), quiet = TRUE))

d <- droplevels(subset(lum31, toxicant == "Zn" & minutes == 15L))
cat("rows", nrow(d), " plates", nlevels(d$plate), " censored", sum(d$censoring == "left"), "\n")

form <- switch(arm,
  ungrouped = rlu_cens | cens(censoring) ~ crf(log(conc), "all"),
  toppl     = rlu_cens | cens(censoring) ~ crf(log(conc), "all") + (top | plate),
  pgl       = rlu_cens | cens(censoring) ~ crf(log(conc), "all") + pgl(plate),
  # ogl(plate) multiplies the whole fitted value, which is the form auto-scale
  # gain takes. (top | plate) scales top alone and leaves bot common across
  # plates, which is wrong wherever the equation has a lower asymptote -- and
  # ecxwb1 and ecxll5, which take all of the pgl(plate) weight between them,
  # both do. Specification section 2.5 states this and the arm list omitted it.
  oglplate  = rlu_cens | cens(censoring) ~ crf(log(conc), "all") + ogl(plate))

t0 <- Sys.time()
fit <- bnec(form, data = d, family = Gamma(link = "identity"),
            chains = 4, iter = 8000, warmup = 4000, seed = 228,
            cores = 4, refresh = 0, control = list(adapt_delta = 0.99))
cat("elapsed", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
saveRDS(fit, out)

diag_one <- function(bf) {
  np <- brms::nuts_params(bf)
  su <- posterior::summarise_draws(posterior::as_draws_array(bf), "rhat", "ess_bulk")
  su <- su[!is.na(su$rhat), ]
  c(divergent = sum(np$Value[np$Parameter == "divergent__"]),
    max_rhat = max(su$rhat), min_ess = min(su$ess_bulk))
}
tab <- do.call(rbind, lapply(names(fit$mod_fits), function(m)
  data.frame(model = m, t(diag_one(fit$mod_fits[[m]]$fit)))))
tab$weight <- round(as.numeric(fit$mod_stats$wi[match(tab$model, fit$mod_stats$model)]), 4)
tab$max_rhat <- round(tab$max_rhat, 4); tab$min_ess <- round(tab$min_ess)
cat("\n#### arm:", arm, "\n"); print(tab, row.names = FALSE)
cat("\nfailed:", paste(names(fit$failed_models), collapse = ", "), "\n")

n <- nec(fit, xform = exp); e10 <- ecx(fit, ecx_val = 10, xform = exp)
e50 <- ecx(fit, ecx_val = 50, xform = exp)
est <- data.frame(arm = arm, quantity = c("N(S)EC", "EC10", "EC50"), rbind(n, e10, e50))
names(est)[3:5] <- c("est", "lo", "hi"); est$width <- est$hi - est$lo
cat("\n#### estimates, mg/L\n"); print(est, row.names = FALSE)
write.csv(est, sub("\\.rds$", "_est.csv", out), row.names = FALSE)
write.csv(tab, sub("\\.rds$", "_diag.csv", out), row.names = FALSE)
