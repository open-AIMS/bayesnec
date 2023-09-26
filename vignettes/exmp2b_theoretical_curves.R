library(tidyverse)
rm(list = ls())
source("R/pred_equations.R")

make_plot_data <- function(model_name, args_, x) {
  fct <- get(model_name)
  my_args <- c(
    sapply(args_[[model_name]], get, envir = sys.frame(sys.parent(0)),
           simplify = FALSE),
    list(x = x)
  )
  if (model_name == "pred_ecxhormebc5") {
    my_args$b_slope <- 1
  }
  data.frame(x = x, y = do.call(fct, my_args))
}

all_args <- sapply(ls(), function(x) {
  (deparse(args(get(x)))[1]) %>%
    gsub("function \\(|x\\) ", "", .) %>%
    gsub("function \\(|\\)", "", .) %>%
    strsplit(., ", ", fixed = TRUE) %>%
    `[[`(1)
})

x_vec <- seq(0.01, 0.99, 0.01)
b_top <- 1
b_bot <- -1
b_ec50 <- mean(x_vec)
b_beta <- 2
b_slope <- 0
b_f <- 0
b_d <- 0.4
b_nec <- quantile(x_vec, 0.3)

model_names <- names(all_args)
names(model_names) <- model_names

ecx_levels <- c(
  "ecxlin", "excexp", "ecxexp", "ecxsigm", "ecx4param", "ecxwb1", "ecxwb1p3",
  "ecxwb2", "ecxwb2p3", "ecxll5", "ecxll4", "ecxll3", "ecxhormebc5",
  "ecxhormebc4"
)
ecx_data <- purrr::map_dfr(
  grep("^pred_ecx", model_names, value = TRUE), make_plot_data,
  args_ = all_args, x = x_vec, .id = "fct"
) %>%
  dplyr::mutate(
    fct = gsub("pred_", "", fct, fixed = TRUE) |>
      factor(x = _, levels = .env$ecx_levels)
  )

ecx_plots <- ggplot(data = ecx_data, mapping = aes(x, y)) +
  geom_line(colour = "darkgrey", linetype = 1) +
  facet_wrap(~fct, ncol = 4, scales = "free") +
  labs(x = "Concentration (predictor)", y = "Response",
       title = substitute(italic("EC"["x"]) * " models")) +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())
ggsave("vignettes/vignette-fig-exmp2b-theoretical_ecx_curves.pdf",
       ecx_plots, width = 7.8, height = 6.4)

nec_levels <- c(
  "neclin", "nec3param", "nec4param", "nechorme", "nechormepwr", "neclinhorme",
  "nechorme4", "nechorme4pwr", "nechormepwr01", "necsigm"
)
nec_data <- purrr::map_dfr(
  grep("^pred_nec", model_names, value = TRUE), make_plot_data,
  args_ = all_args, x = x_vec, .id = "fct"
) %>%
  dplyr::mutate(
    fct = gsub("pred_", "", fct, fixed = TRUE) |>
      factor(x = _, levels = .env$nec_levels)
  )

nec_plots <- ggplot(data = nec_data, mapping = aes(x, y)) +
  geom_line(colour = "darkgrey", linetype = 1) +
  facet_wrap(~fct, ncol = 4, scales = "free") +
  labs(x = "Concentration (predictor)", y = "Response",
       title = substitute(italic("NEC") * " models")) +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())
ggsave("vignettes/vignette-fig-exmp2b-theoretical_nec_curves.pdf",
       nec_plots, width = 7.8, height = 4.88)
