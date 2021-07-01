library(brms)

####################################
# ACCEPTED MODEL GROUPS AND FAMILIES
####################################
mod_groups <- list(nec = c("nec3param", "nec4param", "nechorme",
                           "nechorme4", "necsigm", "neclin", "neclinhorme",
                           "nechormepwr", "nechorme4pwr", "nechormepwr01"),
                   ecx = c("ecx4param", "ecxlin", "ecxexp", "ecxsigm",
                           "ecxwb1", "ecxwb2", "ecxwb1p3", "ecxwb2p3",
                           "ecxll5", "ecxll4", "ecxll3", "ecxhormebc4",
                           "ecxhormebc5"),
                   all = c("nec3param", "nec4param", "nechorme", "nechorme4",
                           "necsigm", "neclin", "neclinhorme",
                           "nechormepwr", "nechorme4pwr", "nechormepwr01",
                           "ecxlin", "ecxexp", "ecxsigm", "ecx4param",
                           "ecxwb1", "ecxwb2", "ecxwb1p3", "ecxwb2p3",
                           "ecxll5", "ecxll4", "ecxll3", "ecxhormebc4",
                           "ecxhormebc5"),
                   bot_free = c("nec3param", "nechorme", "necsigm", "neclin",
                                "neclinhorme", "nechormepwr", "ecxlin",
                                "ecxexp", "ecxsigm", "ecxwb1p3", "ecxwb2p3",
                                "ecxll3", "ecxhormebc4", "nechormepwr01"),
                   zero_bounded = c("nec3param", "nechorme", "necsigm",
                                    "nechormepwr", "nechormepwr01", "ecxexp",
                                    "ecxsigm", "ecxwb1p3", "ecxwb2p3",
                                    "ecxll3", "ecxhormebc4"),
                   decline = c("nec3param", "nec4param",
                               "necsigm", "neclin",
                               "ecxlin", "ecxexp", "ecxsigm", "ecx4param",
                               "ecxwb1", "ecxwb2", "ecxwb1p3", "ecxwb2p3",
                               "ecxll5", "ecxll4", "ecxll3"))

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
bf_neclin <- brms::bf(y ~ top - exp(slope) * (x - nec) * step(x - nec),
                      top + slope + nec ~ 1,
                      nl = TRUE)

# nec3param
bf_nec3param <- brms::bf(y ~ top * exp(-exp(beta) * (x - nec) * step(x - nec)),
                         top + beta + nec ~ 1,
                         nl = TRUE)

# nec4param
bf_nec4param <- brms::bf(y ~ bot + (top - bot) * exp(-exp(beta) * (x - nec) *
                           step(x - nec)),
                         bot + top + beta + nec ~ 1,
                         nl = TRUE)

# nechorme
bf_nechorme <- brms::bf(y ~ (top + exp(slope) * x) * exp(-exp(beta) *
                          (x - nec) * step(x - nec)),
                        top + beta + nec + slope ~ 1,
                        nl = TRUE)

# nechorme4
bf_nechorme4 <- brms::bf(y ~ bot + ((top + exp(slope) * x) - bot) *
                           exp(-exp(beta) * (x - nec) * step(x - nec)),
                         bot + top + beta + nec + slope ~ 1,
                         nl = TRUE)

# neclinhorme
bf_neclinhorme <- brms::bf(y ~ (top + exp(slope) * x) - exp(beta) *
                             (x - nec) * step(x - nec),
                           top + beta + nec + slope ~ 1,
                           nl = TRUE)

# necsigm
bf_necsigm <- brms::bf(y ~ top * exp(-exp(beta) * (step(x - nec) *
                         (x - nec))^exp(d) * step(x - nec)),
                       top + beta + nec + d ~ 1,
                       nl = TRUE)

# nechormepwr
bf_nechormepwr <- brms::bf(y ~ (top + x ^ (1 / (1 + exp(slope)))) *
                             exp(-exp(beta) * (x - nec) * step(x - nec)),
                           top + beta + nec + slope ~ 1,
                           nl = TRUE)

# nechormepwr01
bf_nechormepwr01 <- brms::bf(y ~ (1 / (1 + ((1 / top) - 1) * exp(-exp(slope) *
                               x))) * exp(-exp(beta) * (x - nec) *
                                 step(x - nec)),
                             top + beta + nec + slope ~ 1,
                             nl = TRUE)

# nechorme4pwr
bf_nechorme4pwr <- brms::bf(y ~ bot + ((top + x ^ (1 / (1 + exp(slope)))) - bot) *
                              exp(-exp(beta) * (x - nec) * step(x - nec)),
                            bot + top + beta + nec + slope ~ 1,
                            nl = TRUE)

###############
# ECXEXP MODELS
###############
# ecxlin
bf_ecxlin <- brms::bf(y ~ top - exp(slope) * x,
                      top + slope ~ 1,
                      nl = TRUE)

# ecxexp
bf_ecxexp <- brms::bf(y ~ top * exp(-exp(beta) * x),
                      top + beta ~ 1,
                      nl = TRUE)

# ecxsigm
bf_ecxsigm <- brms::bf(y ~ top * exp(-exp(beta) * x ^ exp(d)),
                       d + top + beta ~ 1,
                       nl = TRUE)

# ecx4param
bf_ecx4param <- brms::bf(y ~ top + (bot - top) / (1 + exp((ec50 - x) *
                           exp(beta))),
                         bot + ec50 + top + beta ~ 1,
                         nl = TRUE)

# ecxwb1
bf_ecxwb1 <- brms::bf(y ~ bot + (top - bot) * exp(-exp(exp(beta) *
                        (x - ec50))),
                      bot + ec50 + top + beta ~ 1,
                      nl = TRUE)

# ecxwb1p3
bf_ecxwb1p3 <- brms::bf(y ~ 0 + (top - 0) * exp(-exp(exp(beta) * (x - ec50))),
                        ec50 + top + beta ~ 1,
                        nl = TRUE)

# ecxwb2
bf_ecxwb2 <- brms::bf(y ~ bot + (top - bot) * (1 - exp(-exp(-exp(beta) *
                        (x - ec50)))),
                      bot + ec50 + top + beta ~ 1,
                      nl = TRUE)

# ecxwb2p3
bf_ecxwb2p3 <- brms::bf(y ~ 0 + (top - 0) * (1 - exp(-exp(-exp(beta) *
                          (x - ec50)))),
                        ec50 + top + beta ~ 1,
                        nl = TRUE)
# ecxll5
bf_ecxll5 <- brms::bf(y ~ bot + (top - bot) / (1 + exp(exp(beta) *
                        (x - ec50))) ^ f,
                      bot + top + beta + ec50 + f ~ 1,
                      nl = TRUE)
# ecxll4
bf_ecxll4 <- brms::bf(y ~ bot + (top - bot) / (1 + exp(exp(beta) *
                        (x - ec50))),
                      bot + top + beta + ec50 ~ 1,
                      nl = TRUE)

# ecxll3
bf_ecxll3 <- brms::bf(y ~ 0 + (top - 0) / (1 + exp(exp(beta) *
                        (x - ec50))),
                      top + beta + ec50 ~ 1,
                      nl = TRUE)

# ecxhormebc5
bf_ecxhormebc5 <- brms::bf(y ~ bot + (top - bot + exp(slope) * x) /
                             (1 + exp(exp(beta) * (x - ec50))),
                           bot + top + beta + ec50 + slope ~ 1,
                           nl = TRUE)

# ecxhormebc4
bf_ecxhormebc4 <- brms::bf(y ~ 0 + (top - 0 + exp(slope) * x) /
                             (1 + exp(exp(beta) * (x - ec50))),
                           top + beta + ec50 + slope ~ 1,
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
pred_functions <- list(nec3param = pred_nec3param,
                       nec4param = pred_nec4param,
                       nechorme = pred_nechorme,
                       nechorme4 = pred_nechorme4,
                       necsigm = pred_necsigm,
                       neclin = pred_neclin,
                       neclinhorme = pred_neclinhorme,
                       nechormepwr = pred_nechormepwr,
                       nechorme4pwr = pred_nechorme4pwr,
                       nechormepwr01 = pred_nechormepwr01,
                       ecxlin = pred_ecxlin,
                       ecxexp = pred_ecxexp,
                       ecxsigm = pred_ecxsigm,
                       ecx4param = pred_ecx4param,
                       ecxwb1 = pred_ecxwb1,
                       ecxwb2 = pred_ecxwb2,
                       ecxwb1p3 = pred_ecxwb1p3,
                       ecxwb2p3 = pred_ecxwb2p3,
                       ecxll5 = pred_ecxll5,
                       ecxll4 = pred_ecxll4,
                       ecxll3 = pred_ecxll3,
                       ecxhormebc4 = pred_ecxhormebc4,
                       ecxhormebc5 = pred_ecxhormebc5)

####################
# SAVE INTERNAL DATA
####################
usethis::use_data(
  mod_groups, mod_fams,
  # neclin
  bf_neclin,
  # nec3param
  bf_nec3param,
  # nec4param
  bf_nec4param,
  # nechorme
  bf_nechorme,
  # nechormepwr
  bf_nechormepwr,
  # nechormepwr01
  bf_nechormepwr01,
  # neclinhorme
  bf_neclinhorme,
  # nechorme4
  bf_nechorme4,
  # nechorme4pwr
  bf_nechorme4pwr,
  # necsigm
  bf_necsigm,
  # ecxlin
  bf_ecxlin,
  # ecxexp
  bf_ecxexp,
  # ecxsigm
  bf_ecxsigm,
  # ecx4param
  bf_ecx4param,
  # ecxwb1
  bf_ecxwb1,
  # ecxwb2
  bf_ecxwb2,
  # ecxwb1p3
  bf_ecxwb1p3,
  # ecxwb2p3
  bf_ecxwb2p3,
  #ecxll5
  bf_ecxll5,
  #ecxll4
  bf_ecxll4,
  #ecxll3
  bf_ecxll3,
  #ecxhormebc5
  bf_ecxhormebc5,
  #ecxhormebc4
  bf_ecxhormebc4,
  stan_funs,
  stanvars,
  pred_functions,
  internal = TRUE, overwrite = TRUE
)
