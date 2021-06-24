params <-
list(EVAL = FALSE)

## ----setup, include = FALSE---------------------------------------------------
stopifnot(require(knitr))
knitr::opts_chunk$set(
  warning = FALSE, message = FALSE,
  eval = ifelse(isTRUE(exists("params")), params$EVAL, FALSE)
)

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("remotes")
#  remotes::install_github("open-AIMS/bayesnec")

## ---- warning = FALSE, message = FALSE----------------------------------------
#  library(dplyr)
#  library(ggplot2)

## -----------------------------------------------------------------------------
#  binom_data <- "https://pastebin.com/raw/zfrUha88" %>%
#    read.table(header = TRUE, dec = ",", stringsAsFactors = FALSE) %>%
#    dplyr::rename(raw_x = raw.x) %>%
#    dplyr::mutate(raw_x = as.numeric(as.character(raw_x)),
#                  log_x = log(raw_x))
#  
#  str(binom_data)
#  range(binom_data$raw_x)
#  hist(binom_data$raw_x)

## ---- warning = FALSE, message = FALSE----------------------------------------
#  library(bayesnec)
#  
#  set.seed(333)
#  exp_1 <- bnec(data = binom_data, x_var = "log_x",
#                y_var = "suc", model = "neclin",
#                trials_var = "tot")

## ---- warning = FALSE, fig.width = 7, fig.height = 7--------------------------
#  plot(exp_1$fit)

## ---- fig.width = 7, fig.height = 7-------------------------------------------
#  pairs(exp_1$fit)

## -----------------------------------------------------------------------------
#  summary(exp_1)

## ---- fig.width = 7, fig.height = 7-------------------------------------------
#  plot(exp_1, lxform = exp)

## ---- message = FALSE, warning = FALSE, fig.width = 7, fig.height = 7---------
#  df <- exp_1$fit$data %>%
#    dplyr::mutate(prop = y / trials)
#  plot(brms::conditional_effects(exp_1$fit))[[1]] +
#       geom_point(data = df, aes(x = x, y = prop),
#                  inherit.aes = FALSE)

## -----------------------------------------------------------------------------
#  exp_1$dispersion

## ---- eval=FALSE--------------------------------------------------------------
#  set.seed(333)
#  exp_1b <- bnec(data = binom_data, x_var = "log_x",
#                y_var = "suc", model = "neclin",
#                family = beta_binomial2,
#                trials_var = "tot")

## ---- eval=FALSE--------------------------------------------------------------
#  plot(exp_1b, lxform = exp)
#  exp_1b$dispersion
#  #[1] NA NA NA

## ----echo = FALSE, out.width = "100%"-----------------------------------------
#  knitr::include_graphics("example1a.jpeg")

## ---- eval=FALSE--------------------------------------------------------------
#  exp_1b$nec
#  #Estimate     Q2.5    Q97.5
#  #3.436315 2.674583 4.00693
#  
#  hist(exp_1b$nec_posterior)

## ----echo = FALSE, out.width = "100%"-----------------------------------------
#  knitr::include_graphics("example1k.png")

## ---- warning = FALSE, eval=FALSE---------------------------------------------
#  ecx(exp_1b)
#  #  ec_10_Q50  ec_10_Q2.5 ec_10_Q97.5
#  #   3.799644    3.276595    4.223064

## ---- warning = FALSE, eval=FALSE---------------------------------------------
#  summary(exp_1b)
#  # Object of class bayesnecfit containing the following non-linear model: neclin
#  #
#  #  Family: beta_binomial2
#  #   Links: mu = logit; phi = identity
#  # Formula: y | trials(trials) ~ top - slope * (x - nec) * step(x - nec)
#  #          top ~ 1
#  #          slope ~ 1
#  #          nec ~ 1
#  #    Data: structure(list(x = c(-2.30258509299405, -2.3025850 (Number of observations: 48)
#  # Samples: 4 chains, each with iter = 10000; warmup = 9000; thin = 1;
#  #          total post-warmup samples = 4000
#  #
#  # Population-Level Effects:
#  #                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#  # top_Intercept       1.83      0.23     1.38     2.34 1.00     1667     1751
#  # slope_Intercept     1.52      0.24     1.06     2.04 1.00     1947     2129
#  # nec_Intercept       3.41      0.30     2.67     3.94 1.00     1625     1618
#  #
#  # Family Specific Parameters:
#  #     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#  # phi     5.74      1.30     3.55     8.75 1.00     2276     2206
#  #
#  # Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
#  # and Tail_ESS are effective sample size measures, and Rhat is the potential
#  # scale reduction factor on split chains (at convergence, Rhat = 1).
#  

## ---- eval=FALSE--------------------------------------------------------------
#  prop_data <- "https://pastebin.com/raw/123jq46d" %>%
#    read.table(header = TRUE, dec = ",", stringsAsFactors = FALSE) %>%
#    dplyr::rename(raw_x = raw.x) %>%
#    dplyr::mutate(raw_x = log(as.numeric(as.character(raw_x)) + 1),
#                  resp = as.numeric(as.character(resp)))
#  
#  set.seed(333)
#  exp_2 <- bnec(data = prop_data, x_var = "raw_x",
#                y_var = "resp", model = "neclin")
#  

## ---- warning = FALSE, fig.width = 7, fig.height = 7, eval=FALSE--------------
#  plot(exp_2$fit)

## ----echo = FALSE, out.width = "100%"-----------------------------------------
#  knitr::include_graphics("example1b.jpeg")

## ---- warning = FALSE, fig.width = 7, fig.height = 7, eval=FALSE--------------
#  plot(exp_2)

## ----echo = FALSE, out.width = "100%"-----------------------------------------
#  knitr::include_graphics("example1c.jpeg")

## ---- warning = FALSE, fig.width = 10, fig.height = 4.7-----------------------
#  count_data <- "https://pastebin.com/raw/ENgNSgf7" %>%
#    read.table(header = TRUE, dec = ",", stringsAsFactors = FALSE) %>%
#    dplyr::rename(raw_x = raw.x) %>%
#    dplyr::mutate(raw_x = as.numeric(as.character(raw_x)),
#                  sqrt_x = sqrt(raw_x))
#  
#  str(count_data)
#  range(count_data$raw_x)
#  
#  par(mfrow = c(1, 2))
#  hist(count_data$raw_x, xlab = "'x' variable", main = "")
#  hist(count_data$count, xlab = "Total counts", main = "")

## ---- warning = FALSE, eval=FALSE---------------------------------------------
#  set.seed(333)
#  exp_3 <- bnec(data = count_data, x_var = "sqrt_x",
#                y_var = "count", model = "neclin")

## ---- warning = FALSE, fig.width = 7, fig.height = 7, eval=FALSE--------------
#  plot(exp_3$fit)

## ----echo = FALSE, out.width = "100%"-----------------------------------------
#  knitr::include_graphics("example1d.jpeg")

## ---- warning = FALSE, fig.width = 7, fig.height = 7, eval=FALSE--------------
#  plot(exp_3)
#  exp_3$dispersion
#  #Estimate     Q2.5    Q97.5
#  #2.915520 1.927265 4.526432

## ----echo = FALSE, out.width = "100%"-----------------------------------------
#  knitr::include_graphics("example1e.jpeg")

## ---- warning = FALSE, eval=FALSE---------------------------------------------
#  set.seed(333)
#  exp_3b <- bnec(data = count_data, x_var = "sqrt_x",
#                y_var = "count", family = "negbinomial", model = "neclin")

## ---- warning = FALSE, fig.width = 7, fig.height = 7, eval=FALSE--------------
#  plot(exp_3b)

## ----echo = FALSE, out.width = "100%"-----------------------------------------
#  knitr::include_graphics("example1f.jpeg")

## ---- warning = FALSE, fig.width = 7, fig.height = 7, eval=FALSE--------------
#  plot(exp_3b$fit)

## ----echo = FALSE, out.width = "100%"-----------------------------------------
#  knitr::include_graphics("example1g.jpeg")

## -----------------------------------------------------------------------------
#  data(nec_data)
#  measure_data <- nec_data %>%
#    dplyr::mutate(measure = exp(y))

## ---- warning = FALSE, eval=FALSE---------------------------------------------
#  set.seed(333)
#  exp_4 <- bnec(data = measure_data, x_var = "x",
#                y_var = "measure", model = "neclin")

## ---- warning = FALSE, fig.width = 7, fig.height = 7, eval=FALSE--------------
#  plot(exp_4$fit)

## ----echo = FALSE, out.width = "100%"-----------------------------------------
#  knitr::include_graphics("example1h.jpeg")

## ---- warning = FALSE, fig.width = 7, fig.height = 7, eval=FALSE--------------
#  plot(exp_4)
#  summary(exp_4)
#  #Object of class bayesnecfit containing the following non-linear model: neclin
#  #
#  # Family: gamma
#  #  Links: mu = log; shape = identity
#  #Formula: y ~ top - slope * (x - nec) * step(x - nec)
#  #         top ~ 1
#  #         slope ~ 1
#  #         nec ~ 1
#  #   Data: structure(list(x = c(1.01874617094183, 0.815747457 (Number of observations: 100)
#  #Samples: 4 chains, each with iter = 10000; warmup = 9000; thin = 1;
#  #         total post-warmup samples = 4000
#  #
#  #Population-Level Effects:
#  #                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#  #top_Intercept       0.89      0.01     0.88     0.91 1.00     2625     2290
#  #slope_Intercept     0.55      0.03     0.50     0.60 1.00     1970     2145
#  #nec_Intercept       1.31      0.04     1.23     1.38 1.00     1648     2038
#  #
#  #Family Specific Parameters:
#  #      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#  #shape   248.66     36.61   185.55   328.96 1.00     2690     2229
#  #
#  #Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
#  #and Tail_ESS are effective sample size measures, and Rhat is the potential
#  #scale reduction factor on split chains (at convergence, Rhat = 1).
#  

## ----echo = FALSE, out.width = "100%"-----------------------------------------
#  knitr::include_graphics("example1i.jpeg")

