library(dplyr)
library(ggplot2)
library(tidyr)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("usage: Rscript plot_divergences.R REPORT_DIRECTORY")
}
root <- normalizePath(args[[1L]], mustWork = TRUE)
output_dir <- file.path(root, "plots")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dat <- read.csv(file.path(root, "diagnostics.csv"), stringsAsFactors = FALSE)
dat$model[is.na(dat$model) | dat$model == "NA"] <- "joint refit"
arms <- c("default", "original_regularizing", "new_regularizing")
labels <- c("Default", "Original\nregularising", "New\nregularising")
dat$arm_code <- dat$arm
dat$arm <- factor(dat$arm, levels = arms, labels = labels)
dat$fit_key <- ifelse(dat$vignette == "example5", "example5.rds", dat$fit_id)
dat$vignette <- factor(dat$vignette,
                       levels = paste0("example", c(1:6, 8:9)),
                       labels = paste("Example", c(1:6, 8:9)))

matched <- dat |>
  group_by(vignette, fit_key, component, model) |>
  filter(n_distinct(arm) == 3L) |>
  ungroup()
summary <- matched |>
  group_by(vignette, arm) |>
  summarise(candidate_entries = n(), total_divergences = sum(n_divergent),
            mean_divergences = mean(n_divergent),
            median_divergences = median(n_divergent),
            with_divergences = sum(n_divergent > 0),
            above_10 = sum(n_divergent > 10), .groups = "drop")
summary_export <- summary
summary_export$arm <- arms[as.integer(summary_export$arm)]
write.csv(summary_export, file.path(root, "paired_divergence_summary.csv"),
          row.names = FALSE)
overall <- matched |>
  group_by(arm) |>
  summarise(candidate_entries = n(),
            total_divergences = sum(n_divergent),
            mean_divergences = mean(n_divergent),
            median_divergences = median(n_divergent),
            with_divergences = sum(n_divergent > 0),
            above_10 = sum(n_divergent > 10), .groups = "drop")
overall$arm <- arms[as.integer(overall$arm)]
write.csv(overall, file.path(root, "paired_divergence_overall.csv"),
          row.names = FALSE)

summary$panel <- paste0(summary$vignette, " (", summary$candidate_entries,
                        " paired candidates)")
summary$panel <- factor(summary$panel,
                        levels = unique(summary$panel))
p_summary <- ggplot(summary, aes(x = arm, y = total_divergences, group = 1)) +
  geom_line(linewidth = 0.55, colour = "#35536b") +
  geom_point(size = 2.8, colour = "#145d80") +
  geom_text(aes(label = format(total_divergences, big.mark = ",", trim = TRUE)),
            vjust = -0.7, size = 3.2) +
  facet_wrap(~panel, ncol = 2, scales = "free_y") +
  scale_y_continuous(trans = "log1p", breaks = c(0, 1, 10, 100, 1000, 10000),
                     labels = scales::comma,
                     expand = expansion(mult = c(0.08, 0.25))) +
  labs(title = "Divergences in the same candidate fits across prior settings",
       subtitle = "Totals use candidates present in all three arms; within each panel the mean has the same direction.",
       x = NULL, y = "Total postwarmup divergences (log1p scale)",
       caption = "Candidate entries repeated by amended or refitted objects are counted again. Example 8 includes its first three calls only.") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9),
        strip.text = element_text(face = "bold", hjust = 0),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0))
ggsave(file.path(output_dir, "paired-divergence-totals.png"), p_summary,
       width = 11, height = 12, dpi = 180)
ggsave(file.path(output_dir, "paired-divergence-totals.pdf"), p_summary,
       width = 11, height = 12)

model_max <- dat |>
  group_by(vignette, model, arm) |>
  summarise(max_divergences = max(n_divergent),
            candidate_entries = n(), .groups = "drop") |>
  complete(nesting(vignette, model), arm) |>
  mutate(class = case_when(
    is.na(max_divergences) ~ "Not fitted",
    max_divergences == 0 ~ "0",
    max_divergences <= 10 ~ "1-10",
    max_divergences <= 100 ~ "11-100",
    max_divergences <= 1000 ~ "101-1,000",
    TRUE ~ ">1,000"),
    label = ifelse(is.na(max_divergences), "-",
                   format(max_divergences, big.mark = ",", trim = TRUE)))
model_max$class <- factor(model_max$class,
                          levels = c("Not fitted", "0", "1-10", "11-100",
                                     "101-1,000", ">1,000"))
write.csv(model_max, file.path(root, "divergences_by_model.csv"),
          row.names = FALSE)

model_order <- model_max |>
  group_by(model) |>
  summarise(maximum = max(max_divergences, na.rm = TRUE), .groups = "drop") |>
  arrange(maximum, model) |>
  pull(model)
model_max$model <- factor(model_max$model, levels = model_order)
palette <- c("Not fitted" = "#e6e6e6", "0" = "#ffffff",
             "1-10" = "#fff4c6", "11-100" = "#f9cb7d",
             "101-1,000" = "#ed855a", ">1,000" = "#b73943")
p_models <- ggplot(model_max, aes(x = arm, y = model)) +
  geom_tile(aes(fill = class), colour = "#d5d5d5", linewidth = 0.22) +
  geom_text(aes(label = label, colour = class), size = 2.9) +
  facet_grid(vignette ~ ., scales = "free_y", space = "free_y",
             switch = "y") +
  scale_fill_manual(values = palette, drop = FALSE) +
  scale_colour_manual(values = c("Not fitted" = "#404040", "0" = "#404040",
                                 "1-10" = "#303030", "11-100" = "#303030",
                                 "101-1,000" = "#202020", ">1,000" = "#ffffff"),
                      guide = "none") +
  scale_x_discrete(position = "top") +
  labs(title = "Largest divergence count for each model in each vignette",
       subtitle = "Each cell shows the maximum across fitted calls or group levels using that model; numbers are actual counts.",
       x = NULL, y = NULL, fill = "Divergences",
       caption = "A dash means the model did not fit in that arm. Repeated fits are not added in this figure; see diagnostics.csv for every candidate.") +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(),
        panel.spacing.y = grid::unit(0.12, "lines"),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
        axis.text.x.top = element_text(size = 9),
        axis.text.y = element_text(size = 8),
        legend.position = "bottom",
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0))
ggsave(file.path(output_dir, "divergences-by-model.png"), p_models,
       width = 9, height = 29, dpi = 170, limitsize = FALSE)
ggsave(file.path(output_dir, "divergences-by-model.pdf"), p_models,
       width = 9, height = 29, limitsize = FALSE)

for (part in list(c("Example 1", "Example 2", "Example 3", "Example 4"),
                  c("Example 5", "Example 6", "Example 8", "Example 9"))) {
  stem <- if ("Example 1" %in% part) "divergences-by-model-1to4" else
    "divergences-by-model-5to9"
  page <- p_models
  page$data <- filter(model_max, as.character(vignette) %in% part)
  page_height <- if ("Example 1" %in% part) 16 else 19
  ggsave(file.path(output_dir, paste0(stem, ".png")), page,
         width = 9, height = page_height, dpi = 190)
  ggsave(file.path(output_dir, paste0(stem, ".pdf")), page,
         width = 9, height = page_height)
}

changed <- dat |>
  filter(arm_code %in% c("original_regularizing", "new_regularizing")) |>
  select(vignette, fit_key, component, model, arm_code, n_divergent) |>
  pivot_wider(names_from = arm_code, values_from = n_divergent) |>
  filter(!is.na(original_regularizing), !is.na(new_regularizing),
         original_regularizing != new_regularizing) |>
  mutate(description = case_when(
    vignette == "Example 4" ~ "Example 4: ametryn ecxll5",
    model == "ecxsigm" ~ "Example 9: Nassarius ecxsigm",
    grepl("^004", fit_key) ~ "Example 9: Nassarius ecxll5",
    TRUE ~ "Example 9: lum31 constant-dispersion ecxll5")) |>
  arrange(desc(original_regularizing))
changed$description <- factor(changed$description,
                              levels = rev(changed$description))
write.csv(changed, file.path(root, "changed_candidate_divergences.csv"),
          row.names = FALSE)
p_changed <- ggplot(changed, aes(y = description)) +
  geom_segment(aes(x = original_regularizing, xend = new_regularizing,
                   yend = description), colour = "#7c8b92", linewidth = 1) +
  geom_point(aes(x = original_regularizing, colour = "Original regularising"),
             size = 3.5) +
  geom_point(aes(x = new_regularizing, colour = "New regularising"),
             size = 3.5) +
  geom_text(aes(x = original_regularizing,
                label = scales::comma(original_regularizing)),
            vjust = -0.75, size = 3) +
  geom_text(aes(x = new_regularizing,
                label = scales::comma(new_regularizing)),
            vjust = 1.8, size = 3) +
  scale_x_continuous(trans = "log1p", breaks = c(0, 1, 5, 10, 100, 1000, 5000),
                     labels = scales::comma,
                     expand = expansion(mult = c(0.07, 0.14))) +
  scale_colour_manual(values = c("Original regularising" = "#d17a38",
                                 "New regularising" = "#145d80")) +
  labs(title = "Candidates whose divergence count changed in PR #537",
       subtitle = "Among candidates fitted under both regularising priors; all four have fewer divergences with the new prior.",
       x = "Postwarmup divergent transitions (log1p scale)", y = NULL,
       colour = NULL,
       caption = "The Nassarius ecxll5 fit still has 3,493 divergences and fails the screen.") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0))
ggsave(file.path(output_dir, "changed-candidate-divergences.png"), p_changed,
       width = 10, height = 5.5, dpi = 190)
ggsave(file.path(output_dir, "changed-candidate-divergences.pdf"), p_changed,
       width = 10, height = 5.5)
