library(brms)

####################################
# ACCEPTED MODEL GROUPS AND FAMILIES
####################################
mod_groups <- list(nec = c("nec3param", "nec4param", "nechorme",
                           "nechorme4", "necsigm", "neclin", "neclinhorme"),
                   ecx = c("ecx4param", "ecxlin", "ecxexp", "ecxsigm",
                           "ecxwb1", "ecxwb2", "ecxwb1p3", "ecxwb2p3"),
                   all = c("nec3param", "nec4param", "nechorme", "nechorme4",
                           "necsigm", "neclin", "neclinhorme", "ecxwb1p3", "ecxwb2p3",
                           "ecxlin", "ecxexp", "ecxsigm", "ecx4param",
                           "ecxwb1", "ecxwb2"),
                   bot_free = c("nec3param", "nechorme", "necsigm", "neclin",
                                "ecxlin", "ecxexp", "ecxsigm", "neclinhorme", "ecxwb1p3", "ecxwb2p3"),
                   zero_bounded = c("nec3param", "nechorme", "necsigm",
                                    "ecxexp", "ecxsigm", "ecxwb1p3", "ecxwb2p3"))

mod_fams <- c(gaussian = "gaussian",
              Gamma = "Gamma",
              poisson = "poisson",
              negbinomial = "negbinomial",
              binomial = "binomial",
              custom = "beta_binomial2",
              beta = "Beta")

############
# NEC MODELS
############
# neclin
bf_neclin_binom <- brms::bf(y | trials(trials) ~ top - slope *
                              (x - nec) * step(x - nec),
                            top + slope + nec ~ 1,
                            nl = TRUE)

bf_neclin_deflt <- brms::bf(y ~ top - slope * (x - nec) * step(x - nec),
                            top + slope + nec ~ 1,
                            nl = TRUE)

# nec3param
bf_nec3param_binom <- brms::bf(y | trials(trials) ~ top *
                                 exp(-exp(beta) * (x - nec) *
                                   step(x - nec)),
                               top + exp(beta) + nec ~ 1,
                               nl = TRUE)

bf_nec3param_deflt <- brms::bf(y ~ top *
                                 exp(-exp(beta) * (x - nec) *
                                   step(x - nec)),
                               top + exp(beta) + nec ~ 1,
                               nl = TRUE)

# nec4param
bf_nec4param_binom <- brms::bf(y | trials(trials) ~ bot + (top - bot) *
                                 exp(-exp(beta) * (x - nec) *
                                   step(x - nec)),
                               bot + top + exp(beta) + nec ~ 1,
                               nl = TRUE)

bf_nec4param_deflt <- brms::bf(y ~ bot + (top - bot) *
                                 exp(-exp(beta) * (x - nec) *
                                   step(x - nec)),
                               bot + top + exp(beta) + nec ~ 1,
                               nl = TRUE)

# nechorme
bf_nechorme_binom <- brms::bf(y | trials(trials) ~ (top + slope * x) *
                                exp(-exp(beta) * (x - nec) *
                                  step(x - nec)),
                              top + exp(beta) + nec + slope ~ 1,
                              nl = TRUE)

bf_nechorme_deflt <- brms::bf(y ~ (top + slope * x) *
                                exp(-exp(beta) * (x - nec) *
                                  step(x - nec)),
                              top + exp(beta) + nec + slope ~ 1,
                              nl = TRUE)

# nechorme4
bf_nechorme4_binom <- brms::bf(y | trials(trials) ~ bot + ((top + slope * x) -
                                 bot) * exp(-exp(beta) * (x - nec) * step(x - nec)),
                               bot + top + exp(beta) + nec + slope ~ 1,
                               nl = TRUE)

bf_nechorme4_deflt <- brms::bf(y ~ bot + ((top + slope * x) - bot) *
                                       exp(-exp(beta) * (x - nec) *
                                                   step(x - nec)),
                               bot + top + exp(beta) + nec + slope ~ 1,
                               nl = TRUE)

# "neclinhorme"
bf_neclinhorme_binom <- brms::bf(y | trials(trials) ~ (top + slope * x) -
                                   exp(beta) * (x - nec) * step(x - nec),
                                 top + exp(beta) + nec + slope ~ 1,
                                 nl = TRUE)

bf_neclinhorme_deflt <- brms::bf(y ~ (top + slope * x) - exp(beta) * (x - nec) *
                                   step(x - nec),
                                 top + exp(beta) + nec + slope ~ 1,
                                 nl = TRUE)

# necsigm
bf_necsigm_binom <- brms::bf(y | trials(trials) ~ top *
                               exp(-exp(beta) * (step(x - nec) *
                                 (x - nec))^exp(d) *
                                   step(x - nec)),
                             top + exp(beta) + nec + d ~ 1,
                             nl = TRUE)

bf_necsigm_deflt <- brms::bf(y ~ top *
                               exp(-exp(beta) * (step(x - nec) *
                                 (x - nec))^exp(d) *
                                   step(x - nec)),
                             top + exp(beta) + nec + d ~ 1,
                             nl = TRUE)

###############
# ECXEXP MODELS
###############
# ecxlin
bf_ecxlin_binom <- brms::bf(y | trials(trials) ~ top - slope * x,
                            top + slope ~ 1,
                            nl = TRUE)

bf_ecxlin_deflt <- brms::bf(y ~ top - slope * x,
                            top + slope ~ 1,
                            nl = TRUE)

# ecxexp
bf_ecxexp_binom <- brms::bf(y | trials(trials) ~ top * exp(-exp(beta) * x),
                            top + exp(beta) ~ 1,
                            nl = TRUE)

bf_ecxexp_deflt <- brms::bf(y ~ top * exp(-exp(beta) * x),
                            top + exp(beta) ~ 1,
                            nl = TRUE)

# ecxsigm
bf_ecxsigm_binom <- brms::bf(y | trials(trials) ~ top * exp(-exp(beta) * x^exp(d)),
                             d + top + exp(beta) ~ 1,
                             nl = TRUE)

bf_ecxsigm_deflt <- brms::bf(y ~ top * exp(-exp(beta) * x^exp(d)),
                             d + top + exp(beta) ~ 1,
                             nl = TRUE)

# ecx4param
bf_ecx4param_binom <- brms::bf(y | trials(trials) ~ top + (bot - top) /
                                 (1 + exp((ec50 - x) * exp(beta))),
                               bot + ec50 + top + exp(beta) ~ 1,
                               nl = TRUE)

bf_ecx4param_deflt <- brms::bf(y ~ top + (bot - top) /
                                 (1 + exp((ec50 - x) * exp(beta))),
                               bot + ec50 + top + exp(beta) ~ 1,
                               nl = TRUE)

# ecxwb1
# c + (d - c) * exp(- exp(b*(x - e)))
bf_ecxwb1_binom <- brms::bf(y | trials(trials) ~ bot + (top - bot) *
                              exp(-exp(exp(beta) * (x- ec50))),
                            bot + ec50 + top + exp(beta) ~ 1,
                            nl = TRUE)

bf_ecxwb1_deflt <- brms::bf(y ~ bot + (top - bot) *
                              exp(-exp(exp(beta) * (x - ec50))),
                            bot + ec50 + top + exp(beta) ~ 1,
                            nl = TRUE)

# ecxwb1p3
# 0 + (d - 0) * exp(- exp(b*(x - e)))
bf_ecxwb1p3_binom <- brms::bf(y | trials(trials) ~ 0 + (top - 0) *
                              exp(-exp(exp(beta) * (x - ec50))),
                            ec50 + top + exp(beta) ~ 1,
                            nl = TRUE)

bf_ecxwb1p3_deflt <- brms::bf(y ~ 0 + (top - 0) *
                              exp(-exp(exp(beta) * (x - ec50))),
                            ec50 + top + exp(beta) ~ 1,
                            nl = TRUE)

# ecxwb2
# c + (d - c)*(1 - exp(- exp(b*(x - e))))
bf_ecxwb2_binom <- brms::bf(y | trials(trials) ~ bot + (top - bot) *
                              (1 - exp(-exp(-exp(beta) *
                                (x - ec50)))),
                            bot + ec50 + top + exp(beta) ~ 1,
                            nl = TRUE)

bf_ecxwb2_deflt <- brms::bf(y ~ bot + (top - bot) *
                              (1 - exp(-exp(-exp(beta) *
                                (x - ec50)))),
                            bot + ec50 + top + exp(beta) ~ 1,
                            nl = TRUE)

# ecxwb2p3
bf_ecxwb2p3_binom <- brms::bf(y | trials(trials) ~ 0 + (top - 0) *
                              (1 - exp(-exp(-exp(beta) *
                                              (x - ec50)))),
                            ec50 + top + exp(beta) ~ 1,
                            nl = TRUE)

bf_ecxwb2p3_deflt <- brms::bf(y ~ 0 + (top - 0) *
                              (1 - exp(-exp(-exp(beta) *
                                              (x - ec50)))),
                            ec50 + top + exp(beta) ~ 1,
                            nl = TRUE)

###############
# CUSTOM FAMILY
###############
# from https://paul-buerkner.github.io/brms/articles/brms_customfamilies.html
stan_funs <- "
  real beta_binomial2_lpmf(int y, real mu, real phi, int T) {
    return beta_binomial_lpmf(y | T, mu * phi, (1 - mu) * phi);
  }
  int beta_binomial2_rng(real mu, real phi, int T) {
    return beta_binomial_rng(T, mu * phi, (1 - mu) * phi);
  }
"

stanvars <- brms::stanvar(scode = stan_funs, block = "functions")

##################
#PREDICT FUNCTIONS
##################
source("R/pred_equations.R")
pred_functions <- list("nec3param" = pred_nec3param, "nec4param" = pred_nec4param, "nechorme" = pred_nechorme, 
                       "nechorme4" = pred_nechorme4, "necsigm" = pred_necsigm,
                       "neclin" = pred_neclin, "neclinhorme" = pred_neclinhorme, 
                       "ecxlin" = pred_ecxlin, "ecxexp" = pred_ecxexp, "ecxsigm"= pred_ecxsigm,    
                       "ecx4param" = pred_ecx4param, "ecxwb1" = pred_ecxwb1, "ecxwb2" = pred_ecxwb2, 
                       "ecxwb1p3" = pred_ecxwb1p3, "ecxwb2p3" = pred_ecxwb2p3)

####################
# SAVE INTERNAL DATA
####################
save(mod_groups, mod_fams,
     # neclin
     bf_neclin_deflt, bf_neclin_binom,
     # nec3param
     bf_nec3param_deflt, bf_nec3param_binom,
     # nec4param
     bf_nec4param_deflt, bf_nec4param_binom,
     # nechorme
     bf_nechorme_deflt, bf_nechorme_binom,
     # neclinhorme
     bf_neclinhorme_deflt, bf_neclinhorme_binom,
     # nechorme4
     bf_nechorme4_deflt, bf_nechorme4_binom,
     # necsigm
     bf_necsigm_deflt, bf_necsigm_binom,
     # ecxlin
     bf_ecxlin_deflt, bf_ecxlin_binom,
     # ecxexp
     bf_ecxexp_deflt, bf_ecxexp_binom,
     # ecxsigm
     bf_ecxsigm_deflt, bf_ecxsigm_binom,
     # ecx4param
     bf_ecx4param_deflt, bf_ecx4param_binom,
     # ecxwb1
     bf_ecxwb1_deflt, bf_ecxwb1_binom,
     # ecxwb2
     bf_ecxwb2_deflt, bf_ecxwb2_binom,
     # ecxwb1p3
     bf_ecxwb1p3_deflt, bf_ecxwb1p3_binom,
     # ecxwb2p3
     bf_ecxwb2p3_deflt, bf_ecxwb2p3_binom,
     stan_funs, stanvars,
     pred_functions,
     file = "R/sysdata.rda")
