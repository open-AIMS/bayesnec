library(brms)

####################################
# ACCEPTED MODEL GROUPS AND FAMILIES
####################################
mod_groups <- list(nec = c("nec3param", "nec4param", "nechorme", "necsigm"),
                   ecx = c("ecx4param", "ecxlin", "ecxexp", "ecxsigm",
                           "ecxwb1", "ecxwb2"),
                   all = c("nec3param", "nec4param", "nechorme", "necsigm",
                           "ecxlin", "ecxexp", "ecxsigm", "ecx4param",
                           "ecxwb1", "ecxwb2"),
                   bot_free = c("nec3param", "nechorme", "necsigm",
                                "ecxlin", "ecxexp", "ecxsigm"))

mod_fams <- c(gaussian = "gaussian",
              Gamma = "Gamma",
              poisson = "poisson",
              negbinomial = "negbinomial",
              binomial = "binomial",
              beta = "Beta")

############
# NEC MODELS
############
# nec3param
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

# nec4param
bf_nec4param_binom <- brms::bf(y | trials(trials) ~ bot + (top - bot) *
                                 exp(-beta * (x - nec) *
                                   step(x - nec)),
                               bot + top + beta + nec ~ 1,
                               nl = TRUE)

bf_nec4param_deflt <- brms::bf(y ~ bot + (top - bot) *
                                 exp(-beta * (x - nec) *
                                   step(x - nec)),
                               bot + top + beta + nec ~ 1,
                               nl = TRUE)

# nechorme
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
# nechorme4
bf_nechorme4_binom <- brms::bf(y | trials(trials) ~ bot + ((top + slope * x) - bot) *
                                       exp(-beta * (x - nec) *
                                                   step(x - nec)),
                               bot + top + beta + nec + slope ~ 1,
                               nl = TRUE)

bf_nechorme4_deflt <- brms::bf(y ~ bot + ((top + slope * x) - bot) *
                                       exp(-beta * (x - nec) *
                                                   step(x - nec)),
                               bot + top + beta + nec + slope ~ 1,
                               nl = TRUE)


# necsigm
bf_necsigm_binom <- brms::bf(y | trials(trials) ~ top *
                               exp(-beta * (step(x - nec) *
                                 (x - nec))^exp(d) *
                                   step(x - nec)),
                             top + beta + nec + d ~ 1,
                             nl = TRUE)

bf_necsigm_deflt <- brms::bf(y ~ top *
                               exp(-beta * (step(x - nec) *
                                 (x - nec))^exp(d) *
                                   step(x - nec)),
                             top + beta + nec + d ~ 1,
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
bf_ecxexp_binom <- brms::bf(y | trials(trials) ~ top * exp(-beta * x),
                            top + beta ~ 1,
                            nl = TRUE)

bf_ecxexp_deflt <- brms::bf(y ~ top * exp(-beta * x),
                            top + beta ~ 1,
                            nl = TRUE)

# ecxsigm
bf_ecxsigm_binom <- brms::bf(y | trials(trials) ~ top * exp(-beta * x^exp(d)),
                             d + top + beta ~ 1,
                             nl = TRUE)

bf_ecxsigm_deflt <- brms::bf(y ~ top * exp(-beta * x^exp(d)),
                             d + top + beta ~ 1,
                             nl = TRUE)

# ecx4param
bf_ecx4param_binom <- brms::bf(y | trials(trials) ~ top + (bot - top) /
                                 (1 + exp((ec50 - x) * beta)),
                               bot + ec50 + top + beta ~ 1,
                               nl = TRUE)

bf_ecx4param_deflt <- brms::bf(y ~ top + (bot - top) /
                                 (1 + exp((ec50 - x) * beta)),
                               bot + ec50 + top + beta ~ 1,
                               nl = TRUE)

# ecxwb1
bf_ecxwb1_binom <- brms::bf(y | trials(trials) ~ bot + (top - bot) *
                              exp(-exp(beta * (x - ec50))),
                            bot + ec50 + top + beta ~ 1,
                            nl = TRUE)

bf_ecxwb1_deflt <- brms::bf(y ~ bot + (top - bot) *
                              exp(-exp(beta * (x - ec50))),
                            bot + ec50 + top + beta ~ 1,
                            nl = TRUE)

# ecxwb2
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

####################
# SAVE INTERNAL DATA
####################
save(mod_groups, mod_fams,
     # nec3param
     bf_nec3param_deflt, bf_nec3param_binom,
     # nec4param
     bf_nec4param_deflt, bf_nec4param_binom,
     # nechorme
     bf_nechorme_deflt, bf_nechorme_binom,
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
     file = "R/sysdata.rda")
