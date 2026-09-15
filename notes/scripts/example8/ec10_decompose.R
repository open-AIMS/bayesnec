# Why does the model-averaged EC10 change so much between the section 5 arms
# while EC50 barely does? Three candidates: the stacking weights moving between
# equations whose upper shoulders differ, `top` itself moving, or the curve
# changing shape. Per-equation values separate them.
args <- commandArgs(trailingOnly = TRUE)
lab <- args[1]; path <- args[2]
suppressMessages(devtools::load_all(
  Sys.getenv("BAYESNEC_SRC", "."), quiet = TRUE))
SP <- Sys.getenv("E8_OUT", "cache/section-fits")

fit <- readRDS(path)
wi <- setNames(as.numeric(fit$mod_stats$wi), fit$mod_stats$model)

rows <- lapply(names(fit$mod_fits), function(m) {
  one <- pull_out(fit, model = m)
  e10 <- try(ecx(one, ecx_val = 10, xform = exp), silent = TRUE)
  e50 <- try(ecx(one, ecx_val = 50, xform = exp), silent = TRUE)
  # `top` is the modelled control that ecx() measures the drop from (#281).
  fx <- brms::fixef(one$fit)
  tp <- if ("top_Intercept" %in% rownames(fx)) fx["top_Intercept", "Estimate"] else NA_real_
  bt <- if ("bot_Intercept" %in% rownames(fx)) fx["bot_Intercept", "Estimate"] else NA_real_
  data.frame(arm = lab, model = m, weight = round(unname(wi[m]), 4),
             top = signif(tp, 4), bot = signif(bt, 4),
             ec10 = if (inherits(e10, "try-error")) NA else signif(e10[1], 4),
             ec50 = if (inherits(e50, "try-error")) NA else signif(e50[1], 4))
})
out <- do.call(rbind, rows)
out$ratio_50_10 <- round(out$ec50 / out$ec10, 2)
print(out[order(-out$weight), ], row.names = FALSE)
write.csv(out, file.path(SP, paste0("ec10_", lab, ".csv")), row.names = FALSE)
