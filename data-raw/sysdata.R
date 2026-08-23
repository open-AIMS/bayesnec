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
                               "neclin",
                               "ecxlin", "ecxexp", "ecxsigm", "ecx4param",
                               "ecxwb1", "ecxwb2", "ecxwb1p3", "ecxwb2p3",
                               "ecxll5", "ecxll4", "ecxll3"),
                   hormesis = c("nechorme", "nechorme4", "neclinhorme", 
                                "nechormepwr", "nechorme4pwr", "nechormepwr01", 
                                "ecxhormebc4", "ecxhormebc5"))

mod_fams <- c(gaussian = "gaussian",
              Gamma = "Gamma",
              poisson = "poisson",
              negbinomial = "negbinomial",
              bernoulli = "bernoulli",
              binomial = "binomial",
              beta_binomial = "beta_binomial",
              beta = "Beta",
              hurdle_gamma = "hurdle_gamma",
              zero_inflated_beta = "zero_inflated_beta",
              zero_inflated_poisson = "zero_inflated_poisson",
              zero_inflated_negbinomial = "zero_inflated_negbinomial",
              hurdle_poisson = "hurdle_poisson",
              hurdle_negbinomial = "hurdle_negbinomial")

# Families with a second parameter block modelling the probability of a zero,
# mapped to the name brms gives that block. brms calls it "hu" for the hurdle
# families and "zi" for the zero-inflated ones; structurally they are the same
# model (see notes/hurdle_gamma_design.md 1.5), so bayesnec treats them alike and
# simply carries the name through. Kept separate from mod_fams so that code can
# ask "is this a two-block family?" without enumerating tags at each call site.
# NB the zero-inflated COUNT families are deliberately absent. zero_inflated_beta
# belongs here because Beta cannot emit a zero, so zero-inflation collapses to a
# hurdle and brms generates the hurdle density with no mixture. Poisson and
# negbinomial can emit zeros, so the equivalence fails: a zero-inflated count
# model is a genuine mixture and its likelihood does not factorise into two
# independent blocks. Note what that argument does and does not settle. It rules
# out bnec_hurdle(), which is the factorised two-fit procedure. It does NOT rule
# out a joint fit carrying a curve on zi, which brms can express perfectly well
# -- that is left out for the separate reasons given in ?bnec, namely that zi
# and mu are weakly separated exactly where mu is small, and that zi is a latent
# class rather than anything the experiment observed. Leaving these tags out of
# this registry is what routes them through the ordinary family path in bnec(),
# where brms fits the mixture itself with a constant zi. See #104.
# hurdle_poisson and hurdle_negbinomial ARE in this registry, where the
# zero-inflated count families deliberately are not (#104). The distinction is
# whether the zeros are observed or latent. Under zero-inflation a zero could
# have come from either component, so zi and mu are weakly separated exactly
# where mu is small -- the high-concentration end that sets the NEC -- and a
# curve on zi describes a class nobody measured. Under a hurdle the zeros are
# observed to be structural: the individual died, the replicate failed. The
# likelihood then factorises exactly and both blocks carry an interpretable
# concentration-response curve, which is the same situation hurdle_gamma
# already serves. See #209.
hurdle_fams <- c(hurdle_gamma = "hu", zero_inflated_beta = "zi",
                 hurdle_poisson = "hu", hurdle_negbinomial = "hu")

# The family whose defaults the mu block should reuse for priors and initial
# values, i.e. what the response looks like once the zeros are set aside.
hurdle_mu_fams <- c(hurdle_gamma = "Gamma", zero_inflated_beta = "beta",
                    hurdle_poisson = "poisson",
                    hurdle_negbinomial = "negbinomial")

###############################
# DISPERSION SUB-MODELS (disp)
###############################
# Families with a free dispersion parameter, mapped to the name brms gives it.
# Only these can carry a dispersion sub-model: for poisson, bernoulli and
# binomial the variance is a deterministic function of the mean, so there is no
# parameter to model. Over-dispersion there is remedied by changing family
# (poisson -> negbinomial, binomial -> beta_binomial), which is what the
# existing dispersion() diagnostic is for -- the two apply to disjoint sets of
# families and are complements rather than alternatives.
disp_dpars <- c(gaussian = "sigma", Gamma = "shape", negbinomial = "shape",
                beta = "phi", beta_binomial = "phi")

# Named variance functions for the disp() term, in the same spirit as the named
# models: each entry knows the expression it expands to, the non-linear
# parameters it introduces, and which families it is valid for. Adding a form
# later is an entry here plus a prior, not a change to the generator.
#
# The expression is written for the DISPERSION PARAMETER, which is what brms
# actually fits, not for the implied standard deviation. Every eligible family
# gives that parameter a log link (validate_family() forces identity on mu
# only), so a linear sub-model here is a power law on the response scale:
# log(dpar) = c0 + c1 * log(mu) is dpar = exp(c0) * mu^c1. Because each family
# already imposes its own mean-variance link the same c1 means different things
# per family -- documented in ?bayesnecformula rather than algebraically
# normalised away, which would change what is fitted for no gain.
#
# "@MU@" is replaced by the model's own curve expression at formula-build time.
# The curve has to be written out again because mu is not in scope for another
# distributional parameter's formula in brms; only the source is duplicated,
# not the fitted quantity.
# "positive_mu" marks the forms that take log(mu) and so cannot be used where
# the fitted mean reaches zero. "scale_free" marks those whose slope is
# dimensionless -- a slope multiplying log(mu) is, one multiplying mu itself is
# not, and its prior has to be scaled to the response (see define_disp_prior).
#
# THE COVARIATE IS CENTRED, and this is not cosmetic. Uncentred, c0 is the
# dispersion parameter at mu = 1 for the log forms and at mu = 0 for the linear
# one -- points that are nowhere near the data unless the response happens to be
# of order 1. Fitting algal cell density (mu ~ 1.8e4, so log(mu) ~ 9.8) with an
# uncentred "power" gave a posterior correlation between c0 and c1 of 0.99, a c1
# of the wrong sign, and an implied CV of 1e6 against an observed 0.03-0.6: the
# prior normal(0, 2) on c1 spreads log(dpar) at the data over +/- 19.6, which is
# no prior at all. Centring at a reference computed from the response makes c0
# the dispersion parameter at a TYPICAL mu, so its prior means something and the
# two parameters decorrelate. "@LOGREF@" / "@REF@" are replaced by that constant
# at formula-build time; it is a fixed number, not an estimated quantity, so
# nothing about the likelihood changes -- only the coordinates it is written in.
disp_functions <- list(
  power = list(
    expr = "c0 + c1 * (log(@MU@) - @LOGREF@)",
    pars = c("c0", "c1"),
    families = c("gaussian", "Gamma", "negbinomial", "beta", "beta_binomial"),
    positive_mu = TRUE,
    scale_free = TRUE,
    centre = "log"
  ),
  twosided = list(
    expr = paste0("c0 + c1 * (log(@MU@) - @LOGREF@)",
                  " + c2 * (log(1 - (@MU@)) - @LOG1MREF@)"),
    pars = c("c0", "c1", "c2"),
    families = c("beta", "beta_binomial"),
    positive_mu = TRUE,
    scale_free = TRUE,
    centre = "log"
  ),
  # Linear in mu rather than in log(mu), so it is defined for a response on the
  # real line, which the two above are not. This is the form a log-transformed
  # endpoint inherits from a power law on its original scale. If density has
  # sd ~ mu_N^p then sd(log N) ~ mu_N^(p - 1) by the delta method, and since
  # mu_N = N0 * exp(days * mu_sgr) for a specific growth rate, that is
  # log sd(sgr) = const + days * (p - 1) * mu_sgr -- log-linear in the mean.
  # So a growth rate is not a case the variance function cannot reach, only one
  # the power law cannot: p < 1 gives c1 < 0, dispersion falling as the growth
  # rate rises. See notes/alga_dataset.md.
  loglinear = list(
    expr = "c0 + c1 * ((@MU@) - @REF@)",
    pars = c("c0", "c1"),
    families = c("gaussian", "Gamma", "negbinomial", "beta", "beta_binomial"),
    positive_mu = FALSE,
    scale_free = FALSE,
    centre = "identity"
  )
)

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
                        (x - ec50))) ^ exp(f),
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
  mod_groups, mod_fams, hurdle_fams, hurdle_mu_fams,
  disp_dpars, disp_functions,
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
  pred_functions,
  internal = TRUE, overwrite = TRUE
)
