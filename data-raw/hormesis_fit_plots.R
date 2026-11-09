# Reproduce the case-study figures and concentration-level fit checks in example10.
# Run after the non-evaluated fit code in vignettes/example10.Rmd.orig has saved
# the fitted objects under ignore/hormesis_data/.

pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)

fit_dir <- "ignore/hormesis_data"
marine_fits <- list(
  neclin = readRDS(file.path(fit_dir, "fit_neclin.rds")),
  neclinhorme = readRDS(file.path(fit_dir, "fit_neclinhorme.rds")),
  nechorme = readRDS(file.path(fit_dir, "fit_marine_nechorme.rds")),
  nechorme4 = readRDS(file.path(fit_dir, "fit_marine_nechorme4.rds")),
  ecxhormebc4 = readRDS(file.path(fit_dir, "fit_ecxhormebc4.rds")),
  ecxhormebc5 = readRDS(file.path(fit_dir, "fit_marine_bc5.rds"))
)
lettuce_fits <- list(
  neclinhorme = readRDS(file.path(fit_dir,
                                   "fit_lettuce_log_neclinhorme.rds")),
  nechorme = readRDS(file.path(fit_dir, "fit_lettuce_log_nechorme.rds")),
  nechorme4 = readRDS(file.path(fit_dir, "fit_lettuce_log_nechorme4.rds")),
  ecxhormebc4 = readRDS(file.path(fit_dir,
                                   "fit_lettuce_log_ecxhormebc4.rds")),
  ecxhormebc5 = readRDS(file.path(fit_dir,
                                   "fit_lettuce_log_ecxhormebc5.rds"))
)

set.seed(570)
marine_set <- do.call(base::c, unname(marine_fits))
set.seed(570)
lettuce_set <- do.call(base::c, unname(lettuce_fits))
marine_weights <- stats::setNames(as.numeric(marine_set$mod_stats$wi),
                                  marine_set$mod_stats$model)
lettuce_weights <- stats::setNames(as.numeric(lettuce_set$mod_stats$wi),
                                   lettuce_set$mod_stats$model)
print(data.frame(equation = names(marine_weights), weight = marine_weights),
      row.names = FALSE, digits = 3)
print(data.frame(equation = names(lettuce_weights), weight = lettuce_weights),
      row.names = FALSE, digits = 3)

predict_curve <- function(fit, concentrations) {
  draws <- if (inherits(fit, "bayesmanecfit")) {
    brms::posterior_epred(fit,
                          newdata = data.frame(conc = concentrations))
  } else {
    brms::posterior_epred(fit$fit,
                          newdata = data.frame(conc = concentrations))
  }
  q <- apply(draws, 2, stats::quantile, probs = c(0.025, 0.5, 0.975))
  data.frame(conc = concentrations, lower = q[1, ], median = q[2, ],
             upper = q[3, ])
}

negri <- utils::read.csv(file.path(fit_dir, "hormesis_negri_2021.csv"))
negri$conc <- (negri$source_conc_ug_l_tah - 1) / 1000
negri_means <- stats::aggregate(sgr ~ conc, negri, mean)
marine_grid <- seq(0, max(negri$conc), length.out = 151)
marine_curves <- rbind(
  data.frame(model = "Model average", predict_curve(marine_set, marine_grid)),
  do.call(rbind, lapply(names(marine_fits), function(model) {
    data.frame(model = model, predict_curve(marine_fits[[model]], marine_grid))
  }))
)
marine_curves$model <- factor(
  marine_curves$model, levels = c("Model average", names(marine_fits)),
  labels = c("Model average",
             sprintf("%s (w = %.3f)", names(marine_fits), marine_weights))
)
marine_marker <- data.frame(
  model = factor("Model average", levels = levels(marine_curves$model)),
  conc = unname(suppressWarnings(bayesnec::nec(marine_set))[1])
)
marine_plot <- ggplot2::ggplot(marine_curves, ggplot2::aes(x = conc, y = median)) +
  ggplot2::geom_vline(data = marine_marker,
                      mapping = ggplot2::aes(xintercept = conc),
                      inherit.aes = FALSE, colour = "#6A3D9A", linewidth = 0.6) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                       fill = "#A9CAD5", alpha = 0.55) +
  ggplot2::geom_line(colour = "#245D73", linewidth = 0.8) +
  ggplot2::geom_point(data = negri_means,
                      mapping = ggplot2::aes(x = conc, y = sgr),
                      inherit.aes = FALSE, size = 1.7) +
  ggplot2::facet_wrap(~ model, ncol = 3) +
  ggplot2::labs(x = "TAH relative to source-coded control (mg/L)",
                y = "Specific growth rate (d^-1)") +
  ggplot2::theme_bw(base_size = 10)
ggplot2::ggsave("vignettes/vignette-fig-example10-marine-fits.png",
                marine_plot, width = 9, height = 8, dpi = 150)

lettuce <- utils::read.csv(system.file("extdata", "hormesis_lettuce_1993.csv",
                                       package = "bayesnec"))
lettuce_means <- stats::aggregate(weight ~ conc, lettuce, mean)
lettuce_grid <- pmax(0, 10^seq(log10(0.1), log10(100.1), length.out = 151) - 0.1)
lettuce_curves <- rbind(
  data.frame(model = "Model average", predict_curve(lettuce_set, lettuce_grid)),
  do.call(rbind, lapply(names(lettuce_fits), function(model) {
    data.frame(model = model, predict_curve(lettuce_fits[[model]], lettuce_grid))
  }))
)
lettuce_curves$model <- factor(
  lettuce_curves$model, levels = c("Model average", names(lettuce_fits)),
  labels = c("Model average",
             sprintf("%s (w = %.3f)", names(lettuce_fits), lettuce_weights))
)
lettuce_curves$plot_x <- log10(lettuce_curves$conc + 0.1)
lettuce$plot_x <- log10(lettuce$conc + 0.1)
lettuce_means$plot_x <- log10(lettuce_means$conc + 0.1)
lettuce_marker <- data.frame(
  model = factor("Model average", levels = levels(lettuce_curves$model)),
  plot_x = log10(unname(suppressWarnings(bayesnec::nsec(
    lettuce_set, sig_val = 0.01))[1]) + 0.1)
)
lettuce_plot <- ggplot2::ggplot(lettuce_curves,
                                ggplot2::aes(x = plot_x, y = median)) +
  ggplot2::geom_vline(data = lettuce_marker,
                      mapping = ggplot2::aes(xintercept = plot_x),
                      inherit.aes = FALSE, colour = "#6A3D9A", linewidth = 0.6) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                       fill = "#E3BAA4", alpha = 0.6) +
  ggplot2::geom_line(colour = "#8D452A", linewidth = 0.9) +
  ggplot2::geom_point(data = lettuce,
                      mapping = ggplot2::aes(x = plot_x, y = weight),
                      inherit.aes = FALSE, colour = "#A25D3A88", size = 1.6) +
  ggplot2::geom_point(data = lettuce_means,
                      mapping = ggplot2::aes(x = plot_x, y = weight),
                      inherit.aes = FALSE, size = 2) +
  ggplot2::facet_wrap(~ model, ncol = 3) +
  ggplot2::scale_x_continuous(
    breaks = log10(sort(unique(lettuce$conc)) + 0.1),
    labels = sort(unique(lettuce$conc))
  ) +
  ggplot2::labs(x = "Isobutyl alcohol (mg/L)", y = "Shoot biomass") +
  ggplot2::theme_bw(base_size = 10)
ggplot2::ggsave("vignettes/vignette-fig-example10-lettuce-fit.png",
                lettuce_plot, width = 9, height = 6, dpi = 150)

nsec_summary <- function(fit, equation, weight) {
  estimate <- suppressWarnings(bayesnec::nsec(fit, sig_val = 0.01))
  effect <- attr(estimate, "ecnsec_relativeP")
  data.frame(equation = equation, weight = weight,
             nsec = unname(estimate[1]), nsec_lower = unname(estimate[2]),
             nsec_upper = unname(estimate[3]),
             ecnsec = unname(effect[1]), ecnsec_lower = unname(effect[2]),
             ecnsec_upper = unname(effect[3]))
}
marine_results <- do.call(rbind, lapply(names(marine_fits), function(model) {
  nsec_summary(marine_fits[[model]], model, marine_weights[[model]])
}))
lettuce_results <- do.call(rbind, lapply(names(lettuce_fits), function(model) {
  nsec_summary(lettuce_fits[[model]], model, lettuce_weights[[model]])
}))
marine_average <- suppressWarnings(bayesnec::nec(marine_set))
marine_average_effect <- suppressWarnings(bayesnec::ecnsec(
  marine_set, nsec = unname(marine_average[1])))
lettuce_average <- suppressWarnings(bayesnec::nsec(lettuce_set, sig_val = 0.01))
lettuce_average_effect <- attr(lettuce_average, "ecnsec_relativeP")
average_row <- function(estimate, effect) {
  data.frame(equation = "Model average", weight = 1,
             nsec = unname(estimate[1]), nsec_lower = unname(estimate[2]),
             nsec_upper = unname(estimate[3]),
             ecnsec = unname(effect[1]), ecnsec_lower = unname(effect[2]),
             ecnsec_upper = unname(effect[3]))
}
case_results <- rbind(
  data.frame(case = "marine", estimate_type = "NSEC", marine_results),
  data.frame(case = "marine", estimate_type = "N(S)EC",
             average_row(marine_average, marine_average_effect)),
  data.frame(case = "lettuce", estimate_type = "NSEC", lettuce_results),
  data.frame(case = "lettuce", estimate_type = "NSEC",
             average_row(lettuce_average, lettuce_average_effect))
)
utils::write.csv(case_results,
                 file.path(fit_dir, "hormesis_case_results.csv"),
                 row.names = FALSE)
print(marine_results, row.names = FALSE, digits = 3)
print(lettuce_results, row.names = FALSE, digits = 3)
print(marine_average)
print(marine_average_effect)
print(lettuce_average)
print(lettuce_average_effect)

fits_to_check <- c(
  stats::setNames(marine_fits, paste0("marine_", names(marine_fits))),
  stats::setNames(lettuce_fits, paste0("lettuce_", names(lettuce_fits))))
for (model in names(fits_to_check)) {
  check <- bayesnec::check_fit(fits_to_check[[model]], ndraws = 1000,
                               seed = 570)
  cat("\n", model, "\n", sep = "")
  print(as.data.frame(check)[, c("group", "n", "obs_mean", "sim_mean",
                                 "ppp_mean", "obs_sd", "sim_sd", "ppp_sd")],
        digits = 3, row.names = FALSE)
}
