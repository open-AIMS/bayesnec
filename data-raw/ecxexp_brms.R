library(brms)

empty_stanmodel <- function() {
	x <- ""
	class(x) <- "stanmodel"
	x
}

data <- data.frame(x = log(c(6.25, 100, 200, 50, 12.5)),
	               y = c(93, 81, 19, 91, 57),
                   trials = c(99, 111, 100, 95, 102))

priors <- c(prior_string("gamma(0.0001, 0.0001)", nlpar = "beta"),
	        prior_string("normal(2, 100)", nlpar = "top"))

bform <- brms::bf(y | trials(trials) ~ top * exp(-beta * x),
                top + beta ~ 1,
                nl = TRUE)

ecxexp_brms <- brms::brm(bform, data = data, family = binomial(),
                         prior = priors, chains = 0)

ecxexp_brms$fit@stanmodel <- empty_stanmodel()

save(ecxexp_brms, file = 'R/sysdata.rda')
