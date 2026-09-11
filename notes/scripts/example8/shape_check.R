args <- commandArgs(trailingOnly = TRUE); lab <- args[1]; path <- args[2]
suppressMessages(devtools::load_all(
  Sys.getenv("BAYESNEC_SRC", "."), quiet = TRUE))
SP <- Sys.getenv("E8_OUT", "cache/section-fits")
fit <- readRDS(path)
wi <- setNames(as.numeric(fit$mod_stats$wi), fit$mod_stats$model)
out <- do.call(rbind, lapply(names(fit$mod_fits), function(m) {
  s <- summary(fit$mod_fits[[m]]$fit)$spec_pars
  sh <- if ("shape" %in% rownames(s)) s["shape", "Estimate"] else NA_real_
  data.frame(arm = lab, model = m, weight = round(unname(wi[m]), 4),
             shape = round(sh, 1), implied_cv = round(1/sqrt(sh), 4))
}))
print(out[order(-out$weight), ], row.names = FALSE)
write.csv(out, file.path(SP, paste0("shape_", lab, ".csv")), row.names = FALSE)
