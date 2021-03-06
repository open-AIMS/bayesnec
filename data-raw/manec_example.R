library(bayesnec)
library(dplyr)
library(testthat)

random_filename <- function(nchar) {
  paste0(c(round(runif(nchar) * 15), sample(letters, nchar),
         sample(LETTERS, nchar))[sample(1:nchar * 3, nchar)], collapse = "")
}

add_na <- function(x, n = 3) {
  x_b <- x
  x_b[sample(seq_along(x), n)] <- NA
  x_b
}

muted_bnec <- function(...) {
  bnec(...) %>%
    suppressWarnings %>%
    suppressMessages
}

logit <- function(x) {
  log(x / (1 - x))
}

data("nec_data")
manec_example <- nec_data %>%
  mutate(y = logit(y)) %>%
  muted_bnec("x", "y", model = c("nec4param", "ecx4param"),
             iter = 50, chains = 2)

usethis::use_data(manec_example, overwrite = TRUE)
