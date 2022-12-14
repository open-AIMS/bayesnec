library(bayesnec)
library(usethis)
library(tidyr)
library(dplyr)
library(tidyselect)

dat <- read.csv("data-raw/herbicide.csv", fileEncoding = "UTF-8-BOM")

dat1 <- dat[, seq(1, ncol(dat), 2)] %>%
  tidyr::pivot_longer(cols = tidyselect::everything())
dat2 <- dat[, seq(2, ncol(dat), 2)] %>%
  tidyr::pivot_longer(tidyselect::everything())

herbicide <- dat1 |>
  dplyr::transmute(
    herbicide = tolower(name),
    concentration = ifelse(value == 0, 0.1, value),
    fvfm = dat2$value
  ) |>
  dplyr::filter(herbicide != "ioxynil") |>
  data.frame() |>
  na.omit()

usethis::use_data(herbicide, overwrite = TRUE)
