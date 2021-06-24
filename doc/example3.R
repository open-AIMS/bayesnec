params <-
list(EVAL = FALSE)

## ----include = FALSE----------------------------------------------------------
stopifnot(require(knitr))
knitr::opts_chunk$set(
  warning = FALSE, message = FALSE,
  eval = ifelse(isTRUE(exists("params")), params$EVAL, FALSE)
)

## ----echo = FALSE, warning = FALSE, message = FALSE, results = "hide"---------
#  library(bayesnec)
#  library(brms)
#  options(mc.cores = parallel::detectCores())

## ----eval = FALSE-------------------------------------------------------------
#  library(brms)
#  library(bayesnec)
#  options(mc.cores = parallel::detectCores())
#  data(nec_data)
#  
#  # a single model
#  exmp_a <- bnec(data = nec_data, x_var = "x", y_var = "y",
#                 model = "nec3param",
#                 family = Beta(link = "identity"),
#                 iter = 1e4, control = list(adapt_delta = 0.99))
#  
#  class(exmp_a) # bayesnecfit

## ---- fig.width = 7, fig.height = 7, eval = FALSE-----------------------------
#  check_priors(exmp_a)

## ----echo = FALSE, out.width = "100%"-----------------------------------------
#  knitr::include_graphics("example3a.jpeg")

## ----eval = FALSE-------------------------------------------------------------
#  pull_prior(exmp_a)
#  ##                        prior class      coef group resp dpar nlpar bound
#  ## 1              gamma(0.5, 2)     b                            beta
#  ## 2                                b Intercept                  beta
#  ## 3 gamma(2, 2.28313180499098)     b                             nec
#  ## 4                                b Intercept                   nec
#  ## 5                 beta(5, 1)     b                             top
#  ## 6                                b Intercept                   top
#  ## 7          gamma(0.01, 0.01)   phi

## ----eval = FALSE-------------------------------------------------------------
#  my_prior <- c(prior_string("beta(5, 1)", nlpar = "top"),
#                prior_string("normal(1.3, 2.7)", nlpar = "nec"),
#                prior_string("gamma(0.5, 2)", nlpar = "beta"))
#  
#  exmp_b <- bnec(data = nec_data, x_var = "x", y_var = "y",
#                 model = "nec3param", priors = my_prior,
#                 family = Beta(link = "identity"),
#                 iter = 1e4, control = list(adapt_delta = 0.99))

## -----------------------------------------------------------------------------
#  show_params(model = "nec3param")

## ----eval = FALSE-------------------------------------------------------------
#  my_priors <- list(nec3param = c(prior_string("beta(5, 1)", nlpar = "top"),
#                                  prior_string("normal(1.3, 2.7)", nlpar = "nec"),
#                                  prior_string("gamma(0.5, 2)", nlpar = "beta")),
#                    nec4param = c(prior_string("beta(5, 1)", nlpar = "top"),
#                                  prior_string("normal(1.3, 2.7)", nlpar = "nec"),
#                                  prior_string("gamma(0.5, 2)", nlpar = "beta"),
#                                  prior_string("beta(1, 5)", nlpar = "bot")))
#  
#  exmp_c <- bnec(data = nec_data, x_var = "x", y_var = "y",
#                 model = c("nec3param", "nec4param"),
#                 family = Beta(link = "identity"), priors = my_priors,
#                 iter = 1e4, control = list(adapt_delta = 0.99))

## ----eval = FALSE-------------------------------------------------------------
#  pull_prior(exmp_c)
#  ## $nec3param
#  ##               prior class      coef group resp dpar nlpar bound
#  ## 1     gamma(0.5, 2)     b                            beta
#  ## 2                       b Intercept                  beta
#  ## 3  normal(1.3, 2.7)     b                             nec
#  ## 4                       b Intercept                   nec
#  ## 5        beta(5, 1)     b                             top
#  ## 6                       b Intercept                   top
#  ## 7 gamma(0.01, 0.01)   phi
#  
#  ## $nec4param
#  ##               prior class      coef group resp dpar nlpar bound
#  ## 1     gamma(0.5, 2)     b                            beta
#  ## 2                       b Intercept                  beta
#  ## 3        beta(1, 5)     b                             bot
#  ## 4                       b Intercept                   bot
#  ## 5  normal(1.3, 2.7)     b                             nec
#  ## 6                       b Intercept                   nec
#  ## 7        beta(5, 1)     b                             top
#  ## 8                       b Intercept                   top
#  ## 9 gamma(0.01, 0.01)   phi
#  
#  check_priors(exmp_c, filename = "Check_priors")

## ----eval = FALSE-------------------------------------------------------------
#  my_priors <- list(nec3param = c(prior_string("beta(5, 1)", nlpar = "top"),
#                                  prior_string("normal(1.3, 2.7)", nlpar = "nec"),
#                                  prior_string("gamma(0.5, 2)", nlpar = "beta")),
#                    nec4param = c(prior_string("beta(5, 1)", nlpar = "top"),
#                                  prior_string("normal(1.3, 2.7)", nlpar = "nec"),
#                                  prior_string("gamma(0.5, 2)", nlpar = "beta"),
#                                  prior_string("beta(1, 5)", nlpar = "bot")))
#  
#  exmp_d <- bnec(data = nec_data, x_var = "x", y_var = "y",
#                 model = c("nec3param", "nec4param"),
#                 family = Beta(link = "identity"), priors = my_priors[1],
#                 iter = 1e4, control = list(adapt_delta = 0.99))=

## ----eval = FALSE-------------------------------------------------------------
#  ecxlin_priors <- c(prior_string("beta(5, 1)", nlpar = "top"),
#                     prior_string("gamma(2, 6.5)", nlpar = "slope"))
#  exmp_e <- amend(exmp_d, add = "ecxlin", priors = ecxlin_priors)

