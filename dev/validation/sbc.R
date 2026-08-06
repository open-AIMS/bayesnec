# Simulation-based calibration for the beta_ub family (Phase 8, issue #173).
#
# Draws parameters from the prior, simulates a dataset, refits, and records the
# rank of each prior draw within its own posterior. If the sampler and the
# likelihood agree with the generative model, those ranks are uniform.
#
# Two things about this family need care before the ranks mean anything.
#
# 1. The prior looks data-dependent and is not. The family samples delta > 0
#    with U = ymax + delta, and states the prior as normal(U_loc - ymax,
#    U_scale) truncated at 0 -- so both the location and the truncation point
#    move with the data. But the likelihood is already zero for U <= ymax, so
#    that truncation adds nothing, and its normalising constant is fixed within
#    a fit. The posterior is therefore exactly the one implied by the fixed
#    prior U ~ normal(U_loc, U_scale), which is what this script simulates from.
#
# 2. Draws with top >= U cannot generate data, because the likelihood is zero
#    there. Rejecting them is legitimate rather than a fudge: the fit's
#    effective prior is already p(theta) restricted to the region where the
#    likelihood is proper, so simulating from that same restricted prior is
#    what makes the two agree.
#
# The Stan code is taken from brms with the shipped family, so this exercises
# the code that actually runs rather than a hand-written copy of it.
#
# Usage:  Rscript dev/validation/sbc.R [n_sims] [n_ranks]
# Writes dev/validation/sbc_results.rds.

suppressMessages(library(cmdstanr))
suppressMessages(library(brms))
suppressMessages(devtools::load_all("."))

args <- commandArgs(trailingOnly = TRUE)
N_SIMS <- if (length(args) >= 1) as.integer(args[1]) else 256L
N_RANKS <- if (length(args) >= 2) as.integer(args[2]) else 99L
N_WARMUP <- if (length(args) >= 3) as.integer(args[3]) else 1000L
N_CHAINS <- if (length(args) >= 4) as.integer(args[4]) else 1L
N_WORKERS <- max(1L, min(8L, parallel::detectCores() - 4L))

set.seed(20260806)

# ---- the generative model --------------------------------------------------

# The prior on `beta` is deliberately narrower than a default would be, and the
# reason is a real limit on the family rather than a convenience.
#
# `beta` is the log decline rate, so mu(x_max) = top * exp(-exp(beta) * range).
# Under beta ~ normal(0, 1) with a predictor range of 6, the upper tail drives
# mu(6) below 1e-300: the response underflows to a denormal, the beta shape
# m * phi follows it to ~1e-316, and beta_lpdf is numerically degenerate there.
# In a first run of this script those were exactly the replicates that failed to
# initialise -- and since they are the large-`beta` draws, dropping them selects
# on theta and the surviving ranks are calibrated for nothing.
#
# normal(-1, 0.5) keeps exp(beta) inside roughly (0.09, 0.9), so the smallest
# fitted mean over the design is about 0.5% of `top`: a near-total decline,
# still comfortably representable. The same prior simulates and fits, which is
# what SBC requires.
PR <- list(
  u_loc = 1, u_scale = 0.15,
  top_shape = 4, top_rate = 8,       # mean 0.5, sd 0.25
  beta_loc = -1, beta_scale = 0.5,
  nec_lb = 0, nec_ub = 6,
  phi_shape = 4, phi_rate = 0.1      # mean 40, sd 20
)

# Responses at or below this are an underflow failure rather than data. Counted
# and reported rather than silently dropped, because a non-zero count would
# mean the ranks are selected.
UNDERFLOW <- 1e-10

x_design <- rep(seq(0, 6, length.out = 8), each = 5)

draw_prior <- function() {
  repeat {
    u <- rnorm(1, PR$u_loc, PR$u_scale)
    top <- rgamma(1, PR$top_shape, PR$top_rate)
    # see note 2 above: draws the likelihood cannot generate from are rejected
    if (u > 0 && top < u) {
      break
    }
  }
  list(top = top, beta = rnorm(1, PR$beta_loc, PR$beta_scale),
       nec = runif(1, PR$nec_lb, PR$nec_ub),
       phi = rgamma(1, PR$phi_shape, PR$phi_rate), U = u)
}

sim_y <- function(th) {
  mu <- th$top * exp(-exp(th$beta) * pmax(x_design - th$nec, 0))
  m <- mu / th$U
  th$U * rbeta(length(m), m * th$phi, (1 - m) * th$phi)
}

# ---- Stan code straight from the shipped family ----------------------------

bf1 <- bf(y ~ top * exp(-exp(beta) * (x - nec) * step(x - nec)),
          top + beta + nec ~ 1, nl = TRUE)

# The delta prior is NOT given to brms. make_stancode() writes prior parameters
# into the program as literals, so a model compiled once would carry the first
# dataset's ymax in its prior for every later fit -- which is precisely the bug
# the first version of this script had. It showed up as U and phi failing SBC
# while top, beta and nec passed, and a prior-only fit recovering every prior
# except delta's. Compiling per replicate would fix it and cost about 40s each.
#
# Instead delta gets a flat prior from brms and the real one is added as a
# stanvar reading u_loc, u_scale and ymax from the data block. Same density,
# including the truncation normalisation; one compile.
make_prior <- function() {
  prior(gamma(4, 8), nlpar = "top", lb = 0) +
    prior_string(paste0("normal(", PR$beta_loc, ", ", PR$beta_scale, ")"),
                 nlpar = "beta") +
    prior(uniform(0, 6), nlpar = "nec", lb = 0, ub = 6) +
    prior(gamma(4, 0.1), class = "phi")
}

delta_prior_stanvars <- function(ymax) {
  beta_ub_stanvars(ymax) +
    stanvar(x = PR$u_loc, name = "u_loc", scode = "real u_loc;",
            block = "data") +
    stanvar(x = PR$u_scale, name = "u_scale",
            scode = "real<lower=0> u_scale;", block = "data") +
    stanvar(scode = paste(
      "target += normal_lpdf(delta | u_loc - ymax, u_scale)",
      "  - normal_lccdf(0 | u_loc - ymax, u_scale);"),
      block = "model", position = "end")
}

seed_dat <- local({
  th <- draw_prior()
  data.frame(x = x_design, y = sim_y(th))
})
seed_ymax <- max(seed_dat$y)
code <- make_stancode(bf1, data = seed_dat, family = beta_ub(),
                      stanvars = delta_prior_stanvars(seed_ymax),
                      prior = make_prior())
code_file <- file.path("dev/validation", "beta_ub_sbc.stan")
writeLines(as.character(code), code_file)
mod <- cmdstan_model(code_file)

standata_for <- function(d) {
  ymax <- max(d$y)
  sd_ <- make_standata(bf1, data = d, family = beta_ub(),
                       stanvars = delta_prior_stanvars(ymax),
                       prior = make_prior())
  # brms tags its standata elements with classes cmdstanr rejects. Only the
  # class goes: stripping every attribute would take `dim` with it, and the
  # design matrices would arrive as plain vectors where Stan wants matrices.
  sd_ <- lapply(sd_, function(z) {
    class(z) <- NULL
    z
  })
  sd_$ymax <- ymax
  sd_
}

# ---- initial values --------------------------------------------------------

# Random initialisation fails for this family often enough to matter. In a
# first run of this script, 6 of 16 replicates aborted with "no chains finished
# successfully" -- Stan's uniform(-2, 2) draws on the unconstrained scale put
# `top` above `U`, and every attempted start had zero density. That is not just
# a nuisance here: the replicates that fail are the ones with particular
# parameter values, so dropping them selects on theta and SBC on what survives
# is calibrated for nothing. The same failure is what add_beta_ub_inits()
# exists to prevent inside bnec().
#
# These starting values depend on the data but not on the generating values,
# which is what SBC requires: inits change where the sampler begins, not what
# it targets.
sbc_init <- function(d) {
  ymax <- max(d$y)
  ctl <- d$y[d$x == min(d$x)]
  mm <- mean(ctl) * (PR$u_loc - mean(ctl)) / max(var(ctl), 1e-8) - 1
  list(b_top = array(mean(ctl) * 0.95, dim = 1),
       b_beta = array(0, dim = 1),
       b_nec = array(stats::median(d$x), dim = 1),
       phi = min(max(if (is.finite(mm) && mm > 0) mm else 20, 1), 500),
       delta = max(PR$u_loc - ymax, 0.05))
}

# ---- one SBC replicate -----------------------------------------------------

# Thinning to N_RANKS draws is what makes the rank statistic valid: SBC assumes
# the posterior draws are independent, and consecutive HMC draws are not.
one_sim <- function(i) {
  set.seed(10000 + i)
  th <- draw_prior()
  d <- data.frame(x = x_design, y = sim_y(th))
  ymax <- max(d$y)
  if (th$U <= ymax) {
    return(NULL)   # cannot happen by construction, but guard anyway
  }
  if (min(d$y) <= UNDERFLOW) {
    # the response has decayed past what a double can carry; see the note on
    # PR$beta above. Flagged rather than dropped silently.
    return(c(top = NA, beta = NA, nec = NA, phi = NA, U = NA,
             divergent = NA, underflow = 1))
  }
  f <- try(mod$sample(data = standata_for(d), chains = N_CHAINS,
                      iter_warmup = N_WARMUP,
                      iter_sampling = ceiling(N_RANKS * 8 / N_CHAINS), thin = 8,
                      init = function(chain_id) sbc_init(d),
                      adapt_delta = 0.95, seed = 10000 + i, refresh = 0,
                      show_messages = FALSE, show_exceptions = FALSE),
           silent = TRUE)
  if (inherits(f, "try-error")) {
    return(NULL)
  }
  dr <- f$draws(format = "df")
  if (nrow(dr) < N_RANKS) {
    return(NULL)
  }
  dr <- dr[seq_len(N_RANKS), ]
  # Raw CmdStan names, not the brms ones: the non-linear coefficients are
  # declared vector[K_top] b_top, so the draws column is "b_top[1]". Reading
  # them as b_top_Intercept silently returns NULL, and every rank comes back 0.
  post <- list(top = dr[["b_top[1]"]], beta = dr[["b_beta[1]"]],
               nec = dr[["b_nec[1]"]], phi = dr$phi,
               U = ymax + dr$delta)
  if (any(vapply(post, is.null, logical(1)))) {
    stop("expected draws are missing: ", paste(names(post), collapse = ", "))
  }
  ranks <- vapply(names(post), function(p) sum(post[[p]] < th[[p]]), numeric(1))
  div <- sum(f$diagnostic_summary(quiet = TRUE)$num_divergent)
  c(ranks, divergent = div, underflow = 0)
}

res <- parallel::mclapply(seq_len(N_SIMS), one_sim, mc.cores = N_WORKERS)
bad <- vapply(res, function(z) is.null(z) || inherits(z, "try-error"),
              logical(1))
if (any(bad)) {
  cat("dropped", sum(bad), "replicate(s); first message:\n")
  print(res[bad][[1]])
}
res <- do.call(rbind, res[!bad])
n_uf <- sum(res[, "underflow"])
res <- res[res[, "underflow"] == 0, , drop = FALSE]

cat("completed replicates:", nrow(res), "of", N_SIMS, "\n")
cat("dropped for response underflow:", n_uf, "\n")
cat("dropped for sampler failure:", sum(bad), "\n")
cat("total divergences:", sum(res[, "divergent"]), "\n\n")

pars <- c("top", "beta", "nec", "phi", "U")
n_bin <- 10
tab <- do.call(rbind, lapply(pars, function(p) {
  r <- res[, p]
  # expected counts are equal across bins under uniformity
  br <- seq(0, N_RANKS + 1, length.out = n_bin + 1)
  obs <- table(cut(r, breaks = br, include.lowest = TRUE, right = FALSE))
  cs <- chisq.test(as.numeric(obs))
  ks <- suppressWarnings(ks.test((r + 0.5) / (N_RANKS + 1), "punif"))
  data.frame(parameter = p, chisq = round(cs$statistic, 2),
             chisq_p = signif(cs$p.value, 3),
             ks_p = signif(ks$p.value, 3),
             mean_rank = round(mean(r), 1),
             expected = (N_RANKS) / 2)
}))
print(tab, row.names = FALSE)
cat("\nUniform ranks are the null. Small p-values indicate miscalibration.\n")
cat("Bins:", n_bin, " ranks per replicate:", N_RANKS, "\n")

saveRDS(list(ranks = res, table = tab, n_ranks = N_RANKS, priors = PR),
        "dev/validation/sbc_results.rds")
cat("\nwritten: dev/validation/sbc_results.rds\n")
