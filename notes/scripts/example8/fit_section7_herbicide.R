# Section 7: a factor covariate. bnec_group() over the seven herbicides, which
# reproduces the final figure of the JSS article (jss4916.tex:799-805,
# fig:fullbayesmanecplot) in one call where that figure was assembled from seven
# separate ones. The decline set is used to match it. The seven levels are fixed
# levels of interest, not a sample from a population, so no group-level term is
# placed across them -- which is the distinction the section makes.
suppressMessages(devtools::load_all(
  Sys.getenv("BAYESNEC_SRC", "."), quiet = TRUE))
SP <- Sys.getenv("E8_OUT", "cache/section-fits")

t0 <- Sys.time()
fits <- bnec_group(fvfm ~ crf(log(concentration), "decline"),
                   data = herbicide, group_var = "herbicide",
                   family = Beta(link = "identity"),
                   # 4000 iterations rather than the 8000 used for the lum31
                   # arms. bnec_group() holds every level's fits in one object:
                   # 7 levels x 11 equations is 77 brmsfits, and at 16000 draws
                   # each the first run reached 10.7 GB after 2.2 levels and
                   # extrapolated to about 34 GB, above this machine's 31 GB.
                   # 8000 draws halves that. The equations sampled cleanly at
                   # 16000 draws with minimum effective sample sizes in the
                   # thousands, so the reduction does not put convergence at
                   # risk; it is recorded because it differs from the other arms.
                   chains = 4, iter = 4000, warmup = 2000, seed = 228,
                   cores = 4, refresh = 0, control = list(adapt_delta = 0.99))
cat("elapsed", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
saveRDS(fits, file.path(SP, "fits7/s7_herbicide.rds"))

diag_one <- function(bf) {
  np <- brms::nuts_params(bf)
  su <- posterior::summarise_draws(posterior::as_draws_array(bf), "rhat", "ess_bulk")
  su <- su[!is.na(su$rhat), ]
  c(divergent = sum(np$Value[np$Parameter == "divergent__"]),
    max_rhat = max(su$rhat), min_ess = min(su$ess_bulk))
}
tab <- do.call(rbind, lapply(names(fits$fits), function(lv) {
  f <- fits$fits[[lv]]
  mf <- if (!is.null(f$mod_fits)) f$mod_fits else list(f)
  d <- do.call(rbind, lapply(mf, function(m) diag_one(m$fit)))
  data.frame(level = lv, equations = nrow(d), divergent = sum(d[, "divergent"]),
             max_rhat = round(max(d[, "max_rhat"]), 4),
             min_ess = round(min(d[, "min_ess"])))
}))
cat("\n#### bnec_group over herbicide\n"); print(tab, row.names = FALSE)
write.csv(tab, file.path(SP, "fits7/s7_herbicide_diag.csv"), row.names = FALSE)

nec_tab <- do.call(rbind, lapply(names(fits$fits), function(lv) {
  v <- nec(fits$fits[[lv]], xform = exp)
  data.frame(level = lv, est = signif(v[1], 3), lo = signif(v[2], 3), hi = signif(v[3], 3))
}))
cat("\n#### N(S)EC by herbicide, ug/L\n"); print(nec_tab, row.names = FALSE)
write.csv(nec_tab, file.path(SP, "fits7/s7_herbicide_nec.csv"), row.names = FALSE)
