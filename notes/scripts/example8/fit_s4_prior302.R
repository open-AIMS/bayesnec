# Section 4 refitted with the prior #302 proposes, applied on the scale the
# predictor is supplied on. The issue retains 10 sd(x) for a predictor already
# logged, so as written it changes nothing here; this run tests the unified
# version, in which option 2's rule -- a 95% prior interval spanning the tested
# predictor range, sigma = (log(max) - log(min positive)) / 3.92 -- is applied to
# the log branch as well. On this plate that is sd 2.03 against the default
# 23.53, the mean unchanged at the median log dose.
args <- commandArgs(trailingOnly = TRUE)
arm <- args[1]; out <- args[2]
suppressMessages(devtools::load_all(
  Sys.getenv("BAYESNEC_SRC", "."), quiet = TRUE))

d <- droplevels(subset(lum31, plate == "Zn 23Apr24 Rep6B" & minutes == 15L))
form <- if (arm == "ogl") {
  rlu_cens | cens(censoring) ~ crf(log(conc), "all") + ogl(conc_group)
} else {
  rlu_cens | cens(censoring) ~ crf(log(conc), "all")
}

z <- log(d$conc)
sigma <- diff(range(z)) / 3.92
mu <- median(z)
cat(sprintf("proposed nec/ec50 prior: normal(%.5f, %.5f)\n", mu, sigma))

prs <- get_priors(form, data = d, family = Gamma(link = "identity"))
# ecxexp has neither parameter, so the substitution is a no-op there.
nms <- names(prs)
changed <- character()
prs <- lapply(seq_along(prs), function(k) {
  p <- prs[[k]]
  i <- p$nlpar %in% c("nec", "ec50")
  stopifnot(sum(i) <= 1)
  if (sum(i) == 1) {
    p$prior[i] <- sprintf("normal(%.14f, %.14f)", mu, sigma)
    changed <<- c(changed, nms[k])
  }
  p
})
names(prs) <- nms
cat("prior replaced in", length(changed), "of", length(prs), "equations:",
    paste(changed, collapse = ", "), "\n")

t0 <- Sys.time()
fit <- bnec(form, data = d, family = Gamma(link = "identity"), prior = prs,
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
cat("\n#### arm:", arm, "(proposed prior)\n"); print(tab, row.names = FALSE)

n <- nec(fit, xform = exp); e10 <- ecx(fit, ecx_val = 10, xform = exp)
e50 <- ecx(fit, ecx_val = 50, xform = exp)
est <- data.frame(arm = arm, quantity = c("N(S)EC", "EC10", "EC50"), rbind(n, e10, e50))
names(est)[3:5] <- c("est", "lo", "hi"); est$width <- est$hi - est$lo
cat("\n#### estimates, mg/L\n"); print(est, row.names = FALSE)
write.csv(est, sub("\\.rds$", "_est.csv", out), row.names = FALSE)
write.csv(tab, sub("\\.rds$", "_diag.csv", out), row.names = FALSE)
