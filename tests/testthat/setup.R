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

data(nec_data)
other_data <- nec_data
colnames(other_data) <- c("a", "b")
nec_data <- nec_data %>% 
  mutate(count = as.integer(round(y * 20)),
         trials = as.integer(20),
         log_x = log(x))

data(manec_example)
nec4param <- pull_out(manec_example, model = "nec4param") %>%
  suppressMessages %>%
  suppressWarnings
ecx4param <- pull_out(manec_example, model = "ecx4param") %>%
  suppressMessages %>%
  suppressWarnings
