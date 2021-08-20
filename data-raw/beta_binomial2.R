library(usethis)
library(brms)
beta_binomial2 <- brms::custom_family(
    "beta_binomial2", dpars = c("mu", "phi"),
    links = c("identity", "log"), lb = c(NA, 0),
    type = "int", vars = "trials[n]"
  )

usethis::use_data(beta_binomial2, overwrite = TRUE)
