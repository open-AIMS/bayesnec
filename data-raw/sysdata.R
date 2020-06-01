library(brms)

#######################
# ACCEPTED MODEL GROUPS
#######################
mod_groups <- list(nec = c("nec3param", "nec4param", "nechorme", "necsigm"),
                       ecx = c("ecx4param", "ecxlin", "ecxexp", "ecxsimg",
                               "ecxwb1", "ecxwb2"),
                       all = c("nec3param", "nec4param", "nechorme", "necsigm",
                               "ecxlin", "ecxexp", "ecxsigm", "ecx4param",
                               "ecxwb1", "ecxwb2"),
                       bot_free = c("nec3param", "nechorme", "necsigm",
                                    "ecxlin", "ecxexp", "ecxsigm"))

empty_stanmodel <- function() {
  x <- ""
  class(x) <- "stanmodel"
  x
}

fams_loop <- function(mod_fams, bf_binom, bf_deflt,
                      priors, data_beta, data, ...) {
  for (i in seq_along(mod_fams)) {
    fam_i <- names(mod_fams)[i]
    out_s <- paste0("inst/stan/ecxexp", fam_i,
                    ".stan")
    if (fam_i == "binom") {
      nf_i <- bf_binom
    } else {
      nf_i <- bf_deflt
    }
    if (fam_i == "beta") {
      df_i <- data_beta
    } else {
      df_i <- data
    }
    mod_i <- brms::brm(nf_i, data = df_i,
                       family = get(mod_fams[i])(),
                       prior = priors, chains = 0,
                       save_model = out_s)
    mod_i$fit@stanmodel <- empty_stanmodel()
    assign(paste0("ecxexp_brms_", fam_i), mod_i, ...)
  }
}

##################################
# SET MOCK DATA AND DEFAULT MODELS
##################################
mod_fams <- c(gaussian = "gaussian",
              gamma = "Gamma",
              poisson = "poisson",
              negbin = "negbinomial",
              binom = "binomial",
              beta = "Beta")

data <- data.frame(x = log(c(6.25, 100, 200, 50, 12.5)),
                   y = c(93, 81, 19, 91, 57),
                   trials = c(99, 111, 100, 95, 102))

data_beta <- data.frame(x = log(c(6.25, 100, 200, 50, 12.5)),
                        y = c(0.1, 0.2, 0.5, 0.12, 0.7))

###############
# ECXEXP MODELS
###############
ecxexp_priors <- c(prior_string("gamma(0.0001, 0.0001)", nlpar = "beta"),
                   prior_string("normal(2, 100)", nlpar = "top"))

bf_ecxexp_binom <- brms::bf(y | trials(trials) ~ top * exp(-beta * x),
                         top + beta ~ 1,
                         nl = TRUE)

bf_ecxexp_deflt <- brms::bf(y ~ top * exp(-beta * x),
                            top + beta ~ 1,
                            nl = TRUE)

fams_loop(mod_fams, bf_ecxexp_binom, bf_ecxexp_deflt,
          ecxexp_priors, data_beta, data, envir = environment())

save(mod_groups, mod_fams,
     ecxexp_brms_gaussian, ecxexp_brms_gamma, ecxexp_brms_poisson,
     ecxexp_brms_negbin, ecxexp_brms_binom, ecxexp_brms_beta,
     file = "R/sysdata.rda")
