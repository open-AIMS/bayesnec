library(bayesnec)
library(dplyr)

random_filename <- function(nchar) {
  paste0(c(round(runif(nchar) * 15), sample(letters, nchar),
         sample(LETTERS, nchar))[sample(1:nchar * 3, nchar)], collapse = "")
}

add_na <- function(x, n = 3) {
  x_b <- x
  x_b[sample(seq_along(x), n)] <- NA
  x_b
}

logit <- function(x) {
  log(x / (1 - x))
}

data("nec_data")
manec_example <- nec_data %>%
  dplyr::mutate(y = logit(y)) %>%
  bnec(formula = y ~ crf(x, model = c("nec4param", "ecx4param")),
       data = ., iter = 200, warmup = 150, chains = 2,
       stan_model_args = list(save_dso = FALSE))

usethis::use_data(manec_example, overwrite = TRUE)
