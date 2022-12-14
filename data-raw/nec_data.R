library(bayesnec)
library(usethis)

# fct == bayesnec:::pred_nec4param
fct <- function(b_beta, b_bot, b_nec, b_top, x) {
  b_bot + (b_top - b_bot) * exp(-exp(b_beta) * (x - b_nec) * 
      ifelse(x - b_nec < 0, 0, 1))
}

linear_rescale <- function(x, r_out) {
  p <- (x - min(x)) / (max(x) - min(x))
  r_out[[1]] + p * (r_out[[2]] - r_out[[1]])
}

set.seed(10)
x <- abs(rnorm(100, 1))
y <- fct(b_beta = 1.2, b_bot = 0.01, b_nec = 1.5, b_top = 0.95, x)
y <- linear_rescale(y + rnorm(100, sd = 0.04), c(0.001, 0.98))

nec_data <- data.frame(x, y)

usethis::use_data(nec_data, overwrite = TRUE)
