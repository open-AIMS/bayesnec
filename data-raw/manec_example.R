library(bayesnec)
library(dplyr)

data("nec_data")
manec_example <- nec_data |>
  dplyr::mutate(y = qlogis(y)) |>
  bnec(formula = y ~ crf(x, model = c("nec4param", "ecx4param")),
       data = _, iter = 200, warmup = 150, chains = 2,
       stan_model_args = list(save_dso = FALSE), seed = 10)

usethis::use_data(manec_example, overwrite = TRUE)
