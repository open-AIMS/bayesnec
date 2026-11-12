# Reproduce the Example 10 case-study figures and estimate summaries.
# Run from the package root after the compendium script
# analysis/hormesis_case_fits.R has saved the screened fits and model sets, and
# copy them, with hormesis_negri_2021.csv, to ignore/hormesis_data/. The
# compendium holds the same script with its own paths.

pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)

fit_dir <- "ignore/hormesis_data"
sig_val <- 0.01
marine_set <- readRDS(file.path(fit_dir, "marine_set_final.rds"))
lettuce_set <- readRDS(file.path(fit_dir, "lettuce_set_final.rds"))
read_case_fits <- function(case, models) {
  stats::setNames(lapply(models, function(model) {
    readRDS(file.path(fit_dir,
                      sprintf("fit_final_%s_%s.rds", case, model)))
  }), models)
}
marine_fits <- read_case_fits("marine", marine_set$success_models)
lettuce_fits <- read_case_fits("lettuce", lettuce_set$success_models)
stopifnot(all(!bayesnec::check_sampling(marine_set,
                                       divergence_cutoff = 0)$failed),
          all(!bayesnec::check_sampling(lettuce_set,
                                       divergence_cutoff = 0)$failed))
set_weights <- function(set) {
  stats::setNames(as.numeric(set$mod_stats$wi), set$mod_stats$model)
}
marine_weights <- set_weights(marine_set)
lettuce_weights <- set_weights(lettuce_set)
print(marine_set$mod_stats, digits = 3)
print(lettuce_set$mod_stats, digits = 3)

negri <- utils::read.csv(file.path(fit_dir, "hormesis_negri_2021.csv"))
negri$conc <- (negri$source_conc_ug_l_tah - 1) / 1000
lettuce <- utils::read.csv("inst/extdata/hormesis_lettuce_1993.csv")

# Posterior draws of the mean, one column per concentration. The first
# concentration must be the control.
epred <- function(fit, concentrations) {
  brms::posterior_epred(if (inherits(fit, "bayesmanecfit")) fit else fit$fit,
                        newdata = data.frame(conc = concentrations))
}
interval <- function(x) stats::quantile(x, c(0.5, 0.025, 0.975), names = FALSE)

# Compute every quantity for each candidate and for the model average, so that
# the figures and the text read the same numbers.
summarise_case <- function(case, set, fits, weights, observed, response) {
  members <- c(list(`Model average` = set), fits)
  member_weights <- c(`Model average` = 1, weights[names(fits)])
  tested <- sort(unique(observed$conc))
  obs_means <- tapply(observed[[response]], observed$conc, mean)
  grid <- if (case == "lettuce") {
    pmax(0, 10^seq(log10(0.1), log10(max(tested) + 0.1), length.out = 301) - 0.1)
  } else {
    seq(0, max(tested), length.out = 301)
  }
  rows <- lapply(names(members), function(model) {
    fit <- members[[model]]
    on_grid <- epred(fit, grid)
    at_tested <- epred(fit, tested)
    control <- at_tested[, 1]
    rise <- apply(on_grid, 1, max) / control - 1
    peak_at <- grid[apply(on_grid, 1, which.max)]
    estimate <- suppressWarnings(bayesnec::nsec(fit, sig_val = sig_val))
    effect <- attr(estimate, "ecnsec_relativeP")
    ec10 <- suppressWarnings(bayesnec::ecx(fit, ecx_val = 10))
    # nec() refuses a smooth ecx-type equation, which has no nec parameter.
    no_effect <- tryCatch(suppressWarnings(bayesnec::nec(fit)),
                          error = function(e) c(NA, NA, NA))
    peak_param <- if (inherits(fit, "bayesnecfit") &&
                      grepl("^nec.*horme", fit$model)) {
      cp <- bayesnec::curve_params(fit)
      unlist(cp[cp$parameter == "nec", c("Estimate", "Q2.5", "Q97.5")])
    } else {
      c(NA, NA, NA)
    }
    summary_row <- data.frame(
      case = case, model = model, weight = member_weights[[model]],
      control = stats::median(control),
      reference = stats::quantile(control, sig_val, names = FALSE),
      nsec = estimate[1], nsec_lower = estimate[2], nsec_upper = estimate[3],
      # The attribute on the summary already holds the median and interval.
      ecnsec = effect[[1]], ecnsec_lower = effect[[2]],
      ecnsec_upper = effect[[3]],
      nec_returned = no_effect[1], nec_returned_lower = no_effect[2],
      nec_returned_upper = no_effect[3],
      ec10 = ec10[1], ec10_lower = ec10[2], ec10_upper = ec10[3],
      peak_param = peak_param[1], peak_param_lower = peak_param[2],
      peak_param_upper = peak_param[3],
      peak_location = interval(peak_at)[1],
      peak_location_lower = interval(peak_at)[2],
      peak_location_upper = interval(peak_at)[3],
      rise_pct = 100 * interval(rise)[1],
      rise_pct_lower = 100 * interval(rise)[2],
      rise_pct_upper = 100 * interval(rise)[3],
      row.names = NULL
    )
    relative <- sweep(at_tested, 1, control, "/")
    means_row <- data.frame(
      case = case, model = model, conc = tested,
      observed = as.numeric(obs_means),
      observed_pct_control = 100 * as.numeric(obs_means) / obs_means[[1]],
      fitted = apply(at_tested, 2, stats::median),
      fitted_lower = apply(at_tested, 2, stats::quantile, 0.025),
      fitted_upper = apply(at_tested, 2, stats::quantile, 0.975),
      fitted_pct_control = 100 * apply(relative, 2, stats::median),
      row.names = NULL
    )
    curve_row <- data.frame(
      case = case, model = model, conc = grid,
      median = apply(on_grid, 2, stats::median),
      lower = apply(on_grid, 2, stats::quantile, 0.025),
      upper = apply(on_grid, 2, stats::quantile, 0.975)
    )
    list(summary = summary_row, means = means_row, curve = curve_row)
  })
  list(summary = do.call(rbind, lapply(rows, `[[`, "summary")),
       means = do.call(rbind, lapply(rows, `[[`, "means")),
       curves = do.call(rbind, lapply(rows, `[[`, "curve")))
}

marine <- summarise_case("marine", marine_set, marine_fits, marine_weights,
                         negri, "sgr")
lettuce_out <- summarise_case("lettuce", lettuce_set, lettuce_fits,
                              lettuce_weights, lettuce, "weight")

case_checks <- function(case, fits) {
  do.call(rbind, lapply(names(fits), function(model) {
    data.frame(case = case, model = model,
               bayesnec::check_fit(fits[[model]], ndraws = 1000, seed = 570))
  }))
}
checks <- rbind(case_checks("marine", marine_fits),
                case_checks("lettuce", lettuce_fits))
checks <- checks[, c("case", "model", "group", "n", "obs_mean", "sim_mean",
                     "ppp_mean", "obs_sd", "sim_sd", "ppp_sd")]

utils::write.csv(rbind(marine$summary, lettuce_out$summary),
                 file.path(fit_dir, "hormesis_case_results.csv"), row.names = FALSE)
utils::write.csv(rbind(marine$means, lettuce_out$means),
                 file.path(fit_dir, "hormesis_fitted_means.csv"), row.names = FALSE)
utils::write.csv(checks, file.path(fit_dir, "hormesis_fit_checks.csv"), row.names = FALSE)

# Each panel marks its fitted control (dashed), the lower sig_val quantile of
# its control posterior (dotted) and its NSEC with the 95% interval shaded.
# The model-averaged NSEC mixes the candidates' NSEC draws, each read against
# its own control, so the model-average panel has no reference line.
reference_label <- sprintf("Lower %g quantile of the fitted control", sig_val)
line_types <- stats::setNames(c("dashed", "dotted", "solid"),
                              c("Fitted control (median)", reference_label,
                                "NSEC (median and 95% interval)"))
case_plot <- function(out, points, means, x_lab, y_lab, fill, line, x_fun,
                      x_scale) {
  labels <- stats::setNames(
    ifelse(out$summary$model == "Model average", "Model average",
           sprintf("%s (weight %.3f)", out$summary$model, out$summary$weight)),
    out$summary$model
  )
  panels <- unname(labels)
  relabel <- function(d) {
    d$panel <- factor(labels[d$model], levels = panels)
    d
  }
  every_panel <- function(d) {
    do.call(rbind, lapply(panels, function(p) {
      d$panel <- factor(p, levels = panels)
      d
    }))
  }
  curves <- relabel(out$curves)
  curves$x <- x_fun(curves$conc)
  marks <- relabel(out$summary)
  marks$reference[marks$model == "Model average"] <- NA
  marks$x <- x_fun(marks$nsec)
  marks$x_lower <- x_fun(marks$nsec_lower)
  marks$x_upper <- x_fun(marks$nsec_upper)
  hlines <- rbind(
    data.frame(panel = marks$panel, y = marks$control,
               what = names(line_types)[1]),
    data.frame(panel = marks$panel, y = marks$reference,
               what = names(line_types)[2])
  )
  hlines <- hlines[!is.na(hlines$y), ]
  means <- every_panel(means)
  means$x <- x_fun(means$conc)
  p <- ggplot2::ggplot(curves, ggplot2::aes(x = x)) +
    ggplot2::geom_rect(data = marks,
                       ggplot2::aes(xmin = x_lower, xmax = x_upper,
                                    ymin = -Inf, ymax = Inf),
                       inherit.aes = FALSE, fill = "#6A3D9A", alpha = 0.10) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                         fill = fill, alpha = 0.55) +
    ggplot2::geom_hline(data = hlines,
                        ggplot2::aes(yintercept = y, linetype = what),
                        colour = "grey35", linewidth = 0.45) +
    ggplot2::geom_vline(data = marks,
                        ggplot2::aes(xintercept = x,
                                     linetype = names(line_types)[3]),
                        colour = "#6A3D9A", linewidth = 0.6) +
    ggplot2::geom_line(ggplot2::aes(y = median), colour = line,
                       linewidth = 0.8)
  if (!is.null(points)) {
    points <- every_panel(points)
    points$x <- x_fun(points$conc)
    p <- p + ggplot2::geom_point(data = points, ggplot2::aes(x = x, y = y),
                                 inherit.aes = FALSE, colour = "#A25D3A88",
                                 size = 1.5)
  }
  p +
    ggplot2::geom_point(data = means, ggplot2::aes(x = x, y = y),
                        inherit.aes = FALSE, size = 1.8) +
    ggplot2::scale_linetype_manual(
      name = NULL, values = line_types, breaks = names(line_types),
      guide = ggplot2::guide_legend(override.aes = list(
        colour = c("grey35", "grey35", "#6A3D9A")))
    ) +
    x_scale +
    ggplot2::facet_wrap(~ panel, ncol = 3) +
    ggplot2::labs(x = x_lab, y = y_lab) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(legend.position = "bottom",
                   panel.grid.minor = ggplot2::element_blank())
}

negri_means <- stats::aggregate(sgr ~ conc, negri, mean)
names(negri_means)[2] <- "y"
marine_plot <- case_plot(
  marine, points = NULL, means = negri_means,
  x_lab = "TAH relative to source-coded control (mg/L)",
  y_lab = expression(paste("Specific growth rate (", d^{-1}, ")")),
  fill = "#A9CAD5", line = "#245D73", x_fun = identity,
  x_scale = ggplot2::scale_x_continuous()
)
ggplot2::ggsave("vignettes/vignette-fig-example10-marine-fits.png", marine_plot,
                width = 9, height = 6.8, dpi = 150)

lettuce_means <- stats::aggregate(weight ~ conc, lettuce, mean)
names(lettuce_means)[2] <- "y"
log_x <- function(conc) log10(conc + 0.1)
lettuce_plot <- case_plot(
  lettuce_out, points = data.frame(conc = lettuce$conc, y = lettuce$weight),
  means = lettuce_means, x_lab = "Isobutyl alcohol (mg/L)",
  y_lab = "Shoot biomass", fill = "#E3BAA4", line = "#8D452A", x_fun = log_x,
  x_scale = ggplot2::scale_x_continuous(
    breaks = log_x(sort(unique(lettuce$conc))),
    labels = sort(unique(lettuce$conc))
  )
)
ggplot2::ggsave("vignettes/vignette-fig-example10-lettuce-fit.png", lettuce_plot,
                width = 9, height = 6.8, dpi = 150)

print(rbind(marine$summary, lettuce_out$summary), digits = 3)
print(rbind(marine$means, lettuce_out$means), digits = 3)
print(checks, digits = 3)
