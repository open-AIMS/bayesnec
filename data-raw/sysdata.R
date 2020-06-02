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

fams_loop <- function(mod_fams, mod_name, bf_binom, bf_deflt,
                      priors, data_beta, data, ...) {
  for (i in seq_along(mod_fams)) {
    fam_i <- names(mod_fams)[i]
    out_s <- paste0("inst/stan/", mod_name, fam_i,
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
    assign(paste0(mod_name, "_brms_", fam_i), mod_i, ...)
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

############
# NEC MODELS
############
# nec3param
nec3param_priors <- c(prior_string("gamma(0.0001, 0.0001)", nlpar = "beta"),
                      prior_string("normal(2, 100)", nlpar = "top"),
                      prior_string("normal(0, 100)", nlpar = "nec", lb = 0))

bf_nec3param_binom <- brms::bf(y | trials(trials) ~ top *
                                 exp(-beta * (x - nec) *
                                   step(x - nec)),
                               top + beta + nec ~ 1,
                               nl = TRUE)

bf_nec3param_deflt <- brms::bf(y ~ top *
                                 exp(-beta * (x - nec) *
                                   step(x - nec)),
                               top + beta + nec ~ 1,
                               nl = TRUE)

fams_loop(mod_fams, "nec3param", bf_nec3param_binom, bf_nec3param_deflt,
          nec3param_priors, data_beta, data, envir = environment())

# nec4param
nec4param_priors <- c(prior_string("gamma(0.0001, 0.0001)", nlpar = "beta"),
                      prior_string("normal(2, 100)", nlpar = "top"),
                      prior_string("normal(0, 100)", nlpar = "bot"),
                      prior_string("normal(0, 100)", nlpar = "ec50"))

bf_nec4param_binom <- brms::bf(y | trials(trials) ~ top +
                                 (bot - top) / (1 + exp((ec50 - x) * beta)),
                               bot + ec50 + top + beta ~ 1,
                               nl = TRUE)

bf_nec4param_deflt <- brms::bf(y ~ top +
                                 (bot - top) / (1 + exp((ec50 - x) * beta)),
                               bot + ec50 + top + beta ~ 1,
                               nl = TRUE)

fams_loop(mod_fams, "nec4param", bf_nec4param_binom, bf_nec4param_deflt,
          nec4param_priors, data_beta, data, envir = environment())

# nechorme
nechorme_priors <- c(prior_string("gamma(0.0001, 0.0001)", nlpar = "beta"),
                     prior_string("normal(2, 100)", nlpar = "top"),
                     prior_string("normal(0, 100)", nlpar = "slope", lb = 0),
                     prior_string("uniform(0.0001, 0.9999)", nlpar = "nec"))

bf_nechorme_binom <- brms::bf(y | trials(trials) ~ (top + slope * x) *
                                exp(-beta * (x - nec) *
                                  step(x - nec)),
                              top + beta + nec + slope ~ 1,
                              nl = TRUE)

bf_nechorme_deflt <- brms::bf(y ~ (top + slope * x) *
                                exp(-beta * (x - nec) *
                                  step(x - nec)),
                              top + beta + nec + slope ~ 1,
                              nl = TRUE)

fams_loop(mod_fams, "nechorme", bf_nechorme_binom, bf_nechorme_deflt,
          nechorme_priors, data_beta, data, envir = environment())

# necsigm
necsigm_priors <- c(prior_string("gamma(0.0001, 0.0001)", nlpar = "beta"),
                    prior_string("normal(2, 100)", nlpar = "top"),
                    prior_string("normal(0, 100)", nlpar = "d"),
                    prior_string("uniform(0.0001, 0.9999)", nlpar = "nec"))

bf_necsigm_binom <- brms::bf(y | trials(trials) ~ top *
                               exp(-beta * (x - nec)^d *
                                 step(x - nec)),
                             top + beta + nec + d ~ 1,
                             nl = TRUE)

bf_necsigm_deflt <- brms::bf(y ~ top *
                               exp(-beta * (x - nec)^d *
                                 step(x - nec)),
                             top + beta + nec + d ~ 1,
                             nl = TRUE)

fams_loop(mod_fams, "necsigm", bf_necsigm_binom, bf_necsigm_deflt,
          necsigm_priors, data_beta, data, envir = environment())

###############
# ECXEXP MODELS
###############
# ecxlin
ecxlin_priors <- c(prior_string("gamma(0.0001, 0.0001)", nlpar = "beta"),
                   prior_string("normal(2, 100)", nlpar = "top"))

bf_ecxlin_binom <- brms::bf(y | trials(trials) ~ top - beta * x,
                            top + beta ~ 1,
                            nl = TRUE)

bf_ecxlin_deflt <- brms::bf(y ~ top - beta * x,
                            top + beta ~ 1,
                            nl = TRUE)

fams_loop(mod_fams, "ecxlin", bf_ecxlin_binom, bf_ecxlin_deflt,
          ecxlin_priors, data_beta, data, envir = environment())

# ecxexp
ecxexp_priors <- c(prior_string("gamma(0.0001, 0.0001)", nlpar = "beta"),
                   prior_string("normal(2, 100)", nlpar = "top"))

bf_ecxexp_binom <- brms::bf(y | trials(trials) ~ top * exp(-beta * x),
                            top + beta ~ 1,
                            nl = TRUE)

bf_ecxexp_deflt <- brms::bf(y ~ top * exp(-beta * x),
                            top + beta ~ 1,
                            nl = TRUE)

fams_loop(mod_fams, "ecxexp", bf_ecxexp_binom, bf_ecxexp_deflt,
          ecxexp_priors, data_beta, data, envir = environment())

# ecxsigm
ecxsigm_priors <- c(prior_string("gamma(0.0001, 0.0001)", nlpar = "beta"),
                    prior_string("normal(2, 100)", nlpar = "top"),
                    prior_string("normal(0, 100)", nlpar = "d"))

bf_ecxsigm_binom <- brms::bf(y | trials(trials) ~ top * exp(-beta * x)^d,
                             d + top + beta ~ 1,
                             nl = TRUE)

bf_ecxsigm_deflt <- brms::bf(y ~ top * exp(-beta * x)^d,
                             d + top + beta ~ 1,
                             nl = TRUE)

fams_loop(mod_fams, "ecxsigm", bf_ecxsigm_binom, bf_ecxsigm_deflt,
          ecxsigm_priors, data_beta, data, envir = environment())

# ecx4param
ecx4param_priors <- c(prior_string("gamma(0.0001, 0.0001)", nlpar = "beta"),
                      prior_string("normal(2, 100)", nlpar = "top"),
                      prior_string("normal(0, 100)", nlpar = "bot"),
                      prior_string("normal(0, 100)", nlpar = "ec50"))

bf_ecx4param_binom <- brms::bf(y | trials(trials) ~ top + (bot - top) /
                                 (1 + exp((ec50 - x) * beta)),
                               bot + ec50 + top + beta ~ 1,
                               nl = TRUE)

bf_ecx4param_deflt <- brms::bf(y ~ top + (bot - top) /
                                 (1 + exp((ec50 - x) * beta)),
                               bot + ec50 + top + beta ~ 1,
                               nl = TRUE)

fams_loop(mod_fams, "ecx4param", bf_ecx4param_binom, bf_ecx4param_deflt,
          ecx4param_priors, data_beta, data, envir = environment())

# ecxwb1
ecxwb1_priors <- c(prior_string("gamma(0.0001, 0.0001)", nlpar = "beta"),
                   prior_string("normal(2, 100)", nlpar = "top"),
                   prior_string("normal(0, 100)", nlpar = "bot"),
                   prior_string("normal(0, 100)", nlpar = "ec50"))

bf_ecxwb1_binom <- brms::bf(y | trials(trials) ~ bot + (top - bot) *
                              exp(-exp(beta * (x - ec50))),
                            bot + ec50 + top + beta ~ 1,
                            nl = TRUE)

bf_ecxwb1_deflt <- brms::bf(y ~ bot + (top - bot) *
                              exp(-exp(beta * (x - ec50))),
                            bot + ec50 + top + beta ~ 1,
                            nl = TRUE)

fams_loop(mod_fams, "ecxwb1", bf_ecxwb1_binom, bf_ecxwb1_deflt,
          ecxwb1_priors, data_beta, data, envir = environment())

# ecxwb2
ecxwb2_priors <- c(prior_string("gamma(0.0001, 0.0001)", nlpar = "beta"),
                   prior_string("normal(2, 100)", nlpar = "top"),
                   prior_string("normal(0, 100)", nlpar = "bot"),
                   prior_string("normal(0, 100)", nlpar = "ec50"))

bf_ecxwb2_binom <- brms::bf(y | trials(trials) ~ bot + (top - bot) *
                              (1 - exp(-exp(beta *
                                (x - ec50)))),
                            bot + ec50 + top + beta ~ 1,
                            nl = TRUE)

bf_ecxwb2_deflt <- brms::bf(y ~ bot + (top - bot) *
                              (1 - exp(-exp(beta *
                                (x - ec50)))),
                            bot + ec50 + top + beta ~ 1,
                            nl = TRUE)

fams_loop(mod_fams, "ecxwb2", bf_ecxwb2_binom, bf_ecxwb2_deflt,
          ecxwb2_priors, data_beta, data, envir = environment())

####################
# SAVE INTERNAL DATA
####################
save(mod_groups, mod_fams,
     # nec3param
     nec3param_brms_gaussian, nec3param_brms_gamma, nec3param_brms_poisson,
     nec3param_brms_negbin, nec3param_brms_binom, nec3param_brms_beta,
     # nec4param
     nec4param_brms_gaussian, nec4param_brms_gamma, nec4param_brms_poisson,
     nec4param_brms_negbin, nec4param_brms_binom, nec4param_brms_beta,
     # nechorme
     nechorme_brms_gaussian, nechorme_brms_gamma, nechorme_brms_poisson,
     nechorme_brms_negbin, nechorme_brms_binom, nechorme_brms_beta,
     # necsigm
     necsigm_brms_gaussian, necsigm_brms_gamma, necsigm_brms_poisson,
     necsigm_brms_negbin, necsigm_brms_binom, necsigm_brms_beta,
     # ecxlin
     ecxlin_brms_gaussian, ecxlin_brms_gamma, ecxlin_brms_poisson,
     ecxlin_brms_negbin, ecxlin_brms_binom, ecxlin_brms_beta,
     # ecxexp
     ecxexp_brms_gaussian, ecxexp_brms_gamma, ecxexp_brms_poisson,
     ecxexp_brms_negbin, ecxexp_brms_binom, ecxexp_brms_beta,
     # ecxsigm
     ecxsigm_brms_gaussian, ecxsigm_brms_gamma, ecxsigm_brms_poisson,
     ecxsigm_brms_negbin, ecxsigm_brms_binom, ecxsigm_brms_beta,
     # ecx4param
     ecx4param_brms_gaussian, ecx4param_brms_gamma, ecx4param_brms_poisson,
     ecx4param_brms_negbin, ecx4param_brms_binom, ecx4param_brms_beta,
     # ecxwb1
     ecxwb1_brms_gaussian, ecxwb1_brms_gamma, ecxwb1_brms_poisson,
     ecxwb1_brms_negbin, ecxwb1_brms_binom, ecxwb1_brms_beta,
     # ecxwb2
     ecxwb2_brms_gaussian, ecxwb2_brms_gamma, ecxwb2_brms_poisson,
     ecxwb2_brms_negbin, ecxwb2_brms_binom, ecxwb2_brms_beta,
     file = "R/sysdata.rda")
