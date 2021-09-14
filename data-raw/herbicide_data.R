library(bayesnec)
library(usethis)
library(tidyverse)

dat <- read.csv("data-raw/herbicide.csv", fileEncoding = 'UTF-8-BOM')

dat1 <- dat[, seq(1, ncol(dat), 2)] %>% 
  pivot_longer(cols = everything())
dat2 <- dat[, seq(2, ncol(dat), 2)] %>% 
  pivot_longer(everything())


herbicide <- data.frame("name" = factor(tolower(dat1$name)), 
                        "conc" = dat1$value,
                        "fvfm" = dat2$value) %>% 
  mutate(conc = ifelse(conc==0, 0.1, conc)) %>% 
  filter(name!="ioxynil") %>% 
  na.omit()

usethis::use_data(herbicide, overwrite = TRUE)
