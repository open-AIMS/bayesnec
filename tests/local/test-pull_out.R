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
  bnec(x_var = "x", y_var = "y", model = c("nec4param", "ecx4param"),
       data = ., iter = 200, warmup = 150, chains = 2)

test_that("loo_controls work", {
  my_ctrls <- list(fitting = list(moment_match = TRUE))
  pull_out(manec_example, model = "nec", loo_controls = my_ctrls) %>%
    expect_s3_class("bayesnecfit") %>%
    suppressWarnings %>%
    expect_message %>%
    expect_message
})
