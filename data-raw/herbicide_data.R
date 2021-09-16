library(bayesnec)
library(usethis)
library(tidyverse)

dat <- read.csv("data-raw/herbicide.csv", fileEncoding = "UTF-8-BOM")

dat1 <- dat[, seq(1, ncol(dat), 2)] %>% 
  tidyr::pivot_longer(cols = tidyselect::everything())
dat2 <- dat[, seq(2, ncol(dat), 2)] %>% 
  tidyr::pivot_longer(tidyselect::everything())


herbicide <- data.frame(herbicide = tolower(dat1$name),
                        concentration = dat1$value, fvfm = dat2$value) %>%
  mutate(concentration = ifelse(concentration == 0, 0.1, concentration)) %>%
  filter(herbicide != "ioxynil") %>%
  na.omit

usethis::use_data(herbicide, overwrite = TRUE)
