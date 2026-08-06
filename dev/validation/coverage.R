# Coverage study for the beta_ub family (Phase 8, issue #173).
#
# Mirrors the design of Ritz, Gerhard & Streibig (2026): simulate from a known
# truth, refit under competing treatments of the ceiling, and report bias,
# interval width and coverage for the toxicity estimates across control
# replicate counts.
#
# Three arms, differing only in what the ceiling U is:
#
#   normalised   U = max(y) * 1.01, i.e. the response divided by its own
#                observed maximum -- the practice #173 documents
#   fixed        U = the true ceiling, known in advance. The best a constant
#                divisor can do, and what a design ceiling or historical
#                control value would give
#   estimated    U estimated with a prior on the ceiling: the beta_ub family
#
# The first two are the same Stan model with U supplied as data, which keeps
# the comparison to exactly one difference. Fitting the raw y with U fixed is
# equivalent to fitting y/U with a plain Beta, up to a constant Jacobian.
#
# Toxicity estimates are computed analytically per draw rather than off a grid.
# For nec3param with mu = top * exp(-exp(beta) * (x - nec) * step(x - nec)),
# the ECx of ecx(type = "absolute") solves mu(x) = (1 - p/100) * top, giving
#   ECp = nec - log(1 - p/100) / exp(beta)
# and the NEC is `nec` itself. This avoids confounding the comparison with
# grid resolution, and is the limit ecx() converges to.
#
# Usage:  Rscript dev/validation/coverage.R [n_rep]
# Writes dev/validation/coverage_results.rds.

suppressMessages(library(cmdstanr))

args <- commandArgs(trailingOnly = TRUE)
N_REP <- if (length(args) >= 1) as.integer(args[1]) else 100L
N_WORKERS <- max(1L, min(8L, parallel::detectCores() - 4L))

set.seed(20260806)

TRUTH <- list(top = 0.8, beta = 0, nec = 2, phi = 30, U = 1)
CONC <- seq(0, 6, length.out = 8)
N_CTL <- c(3, 6, 10)          # control replicates, as in Ritz et al.
N_TREAT <- 5                  # replicates at every other concentration

ecp <- function(p, nec, beta) nec - log(1 - p / 100) / exp(beta)
# `top` is included deliberately. The plan's prediction is that the arms differ
# less in where they put the toxicity estimates than in how wide the interval at
# the *top* of the curve is: an arm that treats an estimated ceiling as known
# has thrown away the uncertainty that belongs there.
TRUE_VALS <- c(EC10 = ecp(10, TRUTH$nec, TRUTH$beta),
               EC50 = ecp(50, TRUTH$nec, TRUTH$beta),
               NEC = TRUTH$nec,
               top = TRUTH$top)

sim_one <- function(n_ctl) {
  x <- c(rep(CONC[1], n_ctl), rep(CONC[-1], each = N_TREAT))
  mu <- TRUTH$top * exp(-exp(TRUTH$beta) * pmax(x - TRUTH$nec, 0))
  m <- mu / TRUTH$U
  list(x = x, y = TRUTH$U * rbeta(length(m), m * TRUTH$phi,
                                  (1 - m) * TRUTH$phi))
}

mod_fixed <- cmdstan_model("dev/validation/beta_fixed_u.stan")

# The estimated arm reuses the Stan program sbc.R writes out of brms with the
# shipped family, so this study and SBC exercise the same code. Generate it if
# it is not there yet.
est_stan <- "dev/validation/beta_ub_sbc.stan"
if (!file.exists(est_stan)) {
  stop("Run dev/validation/sbc.R first: it writes ", est_stan,
       " from the shipped family.", call. = FALSE)
}
mod_est <- cmdstan_model(est_stan)

# Priors shared across arms so that the ceiling is the only difference. `top`
# is given a gamma centred near the truth for all three; the estimated arm adds
# the ceiling prior with U_loc at the truth and U_scale 0.1.
common <- list(top_loc = 4, top_scale = 5, beta_loc = 0, beta_scale = 1,
               nec_lb = 0, nec_ub = 6, phi_shape = 0.01, phi_rate = 0.01)

fit_fixed <- function(d, u_value, seed) {
  sd_ <- c(list(N = length(d$y), x = d$x, y = d$y, U = u_value), common)
  ini <- list(top = min(mean(d$y[d$x == 0]), u_value * 0.9),
              beta = 0, nec = 3, phi = 20)
  f <- try(mod_fixed$sample(data = sd_, chains = 2, parallel_chains = 1,
                            iter_warmup = 1000, iter_sampling = 1000,
                            init = function(chain_id) ini, adapt_delta = 0.95,
                            seed = seed, refresh = 0, show_messages = FALSE,
                            show_exceptions = FALSE), silent = TRUE)
  if (inherits(f, "try-error")) {
    return(NULL)
  }
  dr <- f$draws(format = "df")
  list(nec = dr$nec, beta = dr$beta, top = dr$top,
       divergent = sum(f$diagnostic_summary(quiet = TRUE)$num_divergent))
}

fit_estimated <- function(d, seed) {
  ymax <- max(d$y)
  # the estimated arm uses the shipped family's Stan program, with the ceiling
  # prior supplied through the data block (see sbc.R for why)
  sd_ <- list(N = length(d$y), Y = d$y, C_1 = d$x,
              K_top = 1L, X_top = matrix(1, length(d$y), 1),
              K_beta = 1L, X_beta = matrix(1, length(d$y), 1),
              K_nec = 1L, X_nec = matrix(1, length(d$y), 1),
              prior_only = 0L, ymax = ymax,
              u_loc = TRUTH$U, u_scale = 0.1)
  ini <- list(b_top = array(mean(d$y[d$x == 0]) * 0.95, dim = 1),
              b_beta = array(0, dim = 1), b_nec = array(3, dim = 1),
              phi = 20, delta = max(TRUTH$U - ymax, 0.05))
  f <- try(mod_est$sample(data = sd_, chains = 2, parallel_chains = 1,
                          iter_warmup = 1000, iter_sampling = 1000,
                          init = function(chain_id) ini, adapt_delta = 0.95,
                          seed = seed, refresh = 0, show_messages = FALSE,
                          show_exceptions = FALSE), silent = TRUE)
  if (inherits(f, "try-error")) {
    return(NULL)
  }
  dr <- f$draws(format = "df")
  list(nec = dr[["b_nec[1]"]], beta = dr[["b_beta[1]"]],
       top = dr[["b_top[1]"]],
       divergent = sum(f$diagnostic_summary(quiet = TRUE)$num_divergent))
}

summarise_draws <- function(z) {
  if (is.null(z)) {
    return(NULL)
  }
  est <- list(EC10 = ecp(10, z$nec, z$beta), EC50 = ecp(50, z$nec, z$beta),
              NEC = z$nec, top = z$top)
  do.call(rbind, lapply(names(est), function(p) {
    q <- quantile(est[[p]], c(0.5, 0.025, 0.975))
    data.frame(quantity = p, med = q[1], lo = q[2], hi = q[3],
               covered = TRUE_VALS[[p]] >= q[2] && TRUE_VALS[[p]] <= q[3],
               divergent = z$divergent, stringsAsFactors = FALSE)
  }))
}

one_rep <- function(i, n_ctl) {
  set.seed(50000 + i)
  d <- sim_one(n_ctl)
  arms <- list(
    normalised = summarise_draws(fit_fixed(d, max(d$y) * 1.01, 50000 + i)),
    fixed = summarise_draws(fit_fixed(d, TRUTH$U, 50000 + i)),
    estimated = summarise_draws(fit_estimated(d, 50000 + i))
  )
  out <- lapply(names(arms), function(a) {
    if (is.null(arms[[a]])) {
      return(NULL)
    }
    cbind(arm = a, n_ctl = n_ctl, rep = i, arms[[a]])
  })
  do.call(rbind, out)
}

res <- list()
for (k in N_CTL) {
  cat("control replicates:", k, "\n")
  r <- parallel::mclapply(seq_len(N_REP), one_rep, n_ctl = k,
                          mc.cores = N_WORKERS)
  r <- Filter(function(z) is.data.frame(z), r)
  res[[as.character(k)]] <- do.call(rbind, r)
}
res <- do.call(rbind, res)

tab <- do.call(rbind, lapply(split(res, list(res$arm, res$n_ctl,
                                             res$quantity), drop = TRUE),
                             function(z) {
  truth <- TRUE_VALS[[z$quantity[1]]]
  data.frame(arm = z$arm[1], n_ctl = z$n_ctl[1], quantity = z$quantity[1],
             n = nrow(z),
             bias_pct = round(100 * (median(z$med) - truth) / truth, 2),
             cv_pct = round(100 * sd(z$med) / mean(z$med), 2),
             width = round(median(z$hi - z$lo), 4),
             coverage = round(mean(z$covered), 3),
             divergent = sum(z$divergent))
}))
tab <- tab[order(tab$quantity, tab$n_ctl,
                 match(tab$arm, c("normalised", "fixed", "estimated"))), ]

cat("\ntrue values:", paste(names(TRUE_VALS), round(TRUE_VALS, 4),
                            sep = " = ", collapse = ", "), "\n\n")
print(tab, row.names = FALSE)
cat("\nnominal coverage is 0.95;",
    "MC standard error at", N_REP, "replicates is",
    round(sqrt(0.95 * 0.05 / N_REP), 3), "\n")

saveRDS(list(raw = res, table = tab, truth = TRUE_VALS, n_rep = N_REP),
        "dev/validation/coverage_results.rds")
cat("\nwritten: dev/validation/coverage_results.rds\n")
