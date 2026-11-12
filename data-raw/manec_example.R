# The draws are the fewest that keep both equations above the screen_models()
# ESS cutoff with margin; the object size grows with them. Rerun this script
# whenever fitting code changes. save_dso = FALSE keeps the compiled model,
# about 150 MB per equation, out of the saved object.
library(bayesnec)
library(dplyr)

data("nec_data")
manec_example <- nec_data |>
  dplyr::mutate(y = qlogis(y)) |>
  bnec(formula = y ~ crf(x, model = c("nec4param", "ecx4param")),
       data = _, iter = 1700, warmup = 500, chains = 2, cores = 2,
       stan_model_args = list(save_dso = FALSE), seed = 10)

usethis::use_data(manec_example, overwrite = TRUE, compress = "xz")
