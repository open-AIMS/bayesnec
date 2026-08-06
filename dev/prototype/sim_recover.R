# Phase 0 driver for the upper-bounded beta family (issue #173).
#
# Simulates from known (top, beta, nec, phi, U), refits dev/prototype/beta_ub.stan
# and reports the three things the phase plan says must be settled with a number
# before anything lands in R/:
#
#   1. how often the mu < U constraint binds
#   2. whether U and phi are separately identified, and under what designs
#   3. how far NSEC/EC10/EC50 move when U_loc is misspecified
#
# Usage:  Rscript dev/prototype/sim_recover.R [n_rep_base] [n_rep_grid]
# Writes dev/prototype/phase0_results.rds and prints the tables.

suppressMessages(library(cmdstanr))

args <- commandArgs(trailingOnly = TRUE)
N_REP_BASE <- if (length(args) >= 1) as.integer(args[1]) else 50L
N_REP_GRID <- if (length(args) >= 2) as.integer(args[2]) else 15L
# leave headroom: this box is shared with other work
N_WORKERS <- max(1L, min(8L, parallel::detectCores() - 4L))

set.seed(173)

here <- "dev/prototype"
mod <- cmdstan_model(file.path(here, "beta_ub.stan"))

# ---- simulation ------------------------------------------------------------

nec3param <- function(x, top, beta, nec) {
  top * exp(-exp(beta) * pmax(x - nec, 0))
}

# y ~ U * Beta(m * phi, (1 - m) * phi), m = mu / U
sim_data <- function(n_conc, n_rep, top, beta, nec, phi, U, x_max = 6) {
  x <- rep(seq(0, x_max, length.out = n_conc), each = n_rep)
  mu <- nec3param(x, top, beta, nec)
  m <- mu / U
  y <- U * rbeta(length(m), m * phi, (1 - m) * phi)
  list(x = x, y = y)
}

# Priors are honest: only U_loc is allowed to know the truth, and the prior
# sensitivity study below deliberately breaks that.
stan_data <- function(d, U_loc, U_scale, count_rejections = 0L) {
  ctl <- d$y[d$x == min(d$x)]
  list(N = length(d$y), x = d$x, y = d$y, ymax = max(d$y),
       U_loc = U_loc, U_scale = U_scale,
       top_loc = mean(ctl), top_scale = max(sd(ctl) * 3, 0.1 * mean(ctl)),
       beta_loc = 0, beta_scale = 2,
       nec_loc = mean(d$x), nec_scale = sd(d$x),
       phi_shape = 0.01, phi_rate = 0.01,   # the brms default for Beta phi
       count_rejections = as.integer(count_rejections))
}

fit_one <- function(sd_list, adapt_delta = 0.95, seed = 1) {
  f <- try(mod$sample(data = sd_list, chains = 2, parallel_chains = 1,
                      iter_warmup = 1000, iter_sampling = 1000,
                      adapt_delta = adapt_delta, seed = seed,
                      refresh = 0, show_messages = FALSE,
                      show_exceptions = FALSE), silent = TRUE)
  if (inherits(f, "try-error")) {
    return(NULL)
  }
  f
}

# ---- toxicity estimates from the posterior ---------------------------------

# ECx, absolute: the x at which the fitted curve for a draw falls to
# (1 - p/100) * f(0). Computed per draw, which is the whole point.
ecx_absolute <- function(draws, p, x_grid) {
  vapply(seq_len(nrow(draws)), function(i) {
    mu <- nec3param(x_grid, draws$top[i], draws$beta[i], draws$nec[i])
    target <- max(mu) * (1 - p / 100)
    x_grid[which.min(abs(mu - target))]
  }, numeric(1))
}

summarise_fit <- function(f, truth, x_grid) {
  dr <- f$draws(format = "df")
  dg <- f$diagnostic_summary(quiet = TRUE)
  pars <- c("top", "beta", "nec", "phi", "U")
  qs <- lapply(pars, function(p) unname(quantile(dr[[p]], c(0.05, 0.5, 0.95))))
  names(qs) <- pars
  covered <- vapply(pars, function(p) {
    truth[[p]] >= qs[[p]][1] && truth[[p]] <= qs[[p]][3]
  }, logical(1))
  ec10 <- ecx_absolute(dr, 10, x_grid)
  ec50 <- ecx_absolute(dr, 50, x_grid)
  list(
    covered = covered,
    med = vapply(pars, function(p) qs[[p]][2], numeric(1)),
    width = vapply(pars, function(p) qs[[p]][3] - qs[[p]][1], numeric(1)),
    divergences = sum(dg$num_divergent),
    max_rhat = max(f$summary(pars)$rhat, na.rm = TRUE),
    cor_U_phi = cor(dr$U, dr$phi),
    top_over_U = median(dr$top_over_U),
    nec_med = median(dr$nec),
    ec10_med = median(ec10), ec10_lo = quantile(ec10, 0.025),
    ec10_hi = quantile(ec10, 0.975),
    ec50_med = median(ec50)
  )
}

run_replicates <- function(label, n_rep, n_conc, reps_per_conc, top, U,
                           beta = 0, nec = 2, phi = 30,
                           U_loc = NULL, U_scale = NULL,
                           count_rejections = 0L) {
  if (is.null(U_loc)) U_loc <- U
  if (is.null(U_scale)) U_scale <- 0.1 * U
  truth <- list(top = top, beta = beta, nec = nec, phi = phi, U = U)
  x_grid <- seq(0, 6, length.out = 1000)
  out <- parallel::mclapply(seq_len(n_rep), function(i) {
    set.seed(1000 + i)
    d <- sim_data(n_conc, reps_per_conc, top, beta, nec, phi, U)
    f <- fit_one(stan_data(d, U_loc, U_scale, count_rejections), seed = 1000 + i)
    if (is.null(f)) return(NULL)
    s <- try(summarise_fit(f, truth, x_grid), silent = TRUE)
    if (inherits(s, "try-error")) NULL else s
  }, mc.cores = N_WORKERS)
  out <- Filter(Negate(is.null), out)
  if (!length(out)) {
    return(NULL)
  }
  data.frame(
    label = label, n_conc = n_conc, reps = reps_per_conc,
    top_true = top, U_true = U, U_loc = U_loc, n_ok = length(out),
    cover_top = mean(vapply(out, function(z) z$covered[["top"]], logical(1))),
    cover_beta = mean(vapply(out, function(z) z$covered[["beta"]], logical(1))),
    cover_nec = mean(vapply(out, function(z) z$covered[["nec"]], logical(1))),
    cover_phi = mean(vapply(out, function(z) z$covered[["phi"]], logical(1))),
    cover_U = mean(vapply(out, function(z) z$covered[["U"]], logical(1))),
    med_U = median(vapply(out, function(z) z$med[["U"]], numeric(1))),
    width_U = median(vapply(out, function(z) z$width[["U"]], numeric(1))),
    med_phi = median(vapply(out, function(z) z$med[["phi"]], numeric(1))),
    cor_U_phi = median(vapply(out, function(z) z$cor_U_phi, numeric(1))),
    max_abs_cor = max(abs(vapply(out, function(z) z$cor_U_phi, numeric(1)))),
    top_over_U = median(vapply(out, function(z) z$top_over_U, numeric(1))),
    divergences = sum(vapply(out, function(z) z$divergences, numeric(1))),
    max_rhat = max(vapply(out, function(z) z$max_rhat, numeric(1))),
    nec_med = median(vapply(out, function(z) z$nec_med, numeric(1))),
    ec10_med = median(vapply(out, function(z) z$ec10_med, numeric(1))),
    ec50_med = median(vapply(out, function(z) z$ec50_med, numeric(1))),
    stringsAsFactors = FALSE
  )
}

res <- list()
say <- function(...) cat(..., "\n", sep = "")

# ---- base fixture: the acceptance criterion --------------------------------

say("== base fixture: 8 concentrations x 5 reps, top/U = 0.8, ", N_REP_BASE,
    " replicates")
res$base <- run_replicates("base", N_REP_BASE, 8, 5, top = 0.8, U = 1.0)
print(res$base)

# ---- decision 1: how often does the mu < U constraint bind? ----------------

say("\n== decision 1: rejection count with reject() enabled")

# CmdStan's rejection notices reach R as messages, not as chain stdout:
# f$output(i) is empty here regardless of show_messages, so the count has to
# come from capture.output(type = "message"). Verified against a fixture known
# to violate the constraint before being relied on.
reject_fixture <- function(label, top, seed) {
  set.seed(seed)
  d <- sim_data(8, 5, top, 0, 2, 30, 1.0)
  msg <- capture.output({
    f <- mod$sample(data = stan_data(d, 1.0, 0.1, count_rejections = 1L),
                    chains = 2, parallel_chains = 2, iter_warmup = 1000,
                    iter_sampling = 1000, adapt_delta = 0.95, seed = seed,
                    refresh = 0, show_messages = TRUE, show_exceptions = TRUE)
  }, type = "message")
  n_rej <- sum(grepl("beta_ub constraint violated", msg))
  n_init <- sum(grepl("initial value", msg))
  # Conservative denominator: one proposal per iteration. A NUTS iteration takes
  # several leapfrog steps, each of which evaluates the lpdf, so the true
  # per-evaluation rate is lower than this.
  iters <- 2 * 2000
  say("  ", label, " (top/U = ", top, "): ", n_rej, " rejections in >= ", iters,
      " lpdf evaluations (", sprintf("%.2f%%", 100 * n_rej / iters),
      "), ", n_init, " init failures")
  data.frame(label = label, top = top, rejections = n_rej,
             iters = iters, pct = 100 * n_rej / iters, init_failures = n_init,
             divergences = sum(f$diagnostic_summary(quiet = TRUE)$num_divergent),
             stringsAsFactors = FALSE)
}

res$rejections <- rbind(
  reject_fixture("base", 0.8, 7),
  reject_fixture("near-ceiling", 0.98, 8),
  # steep decline over a wide range: the case where mu * phi underflows
  reject_fixture("far-from-ceiling", 0.3, 9)
)

# Coverage is the acceptance criterion, so it needs a Monte Carlo error small
# enough to decide against the 90% threshold. At 50 replicates the standard
# error is 4.2 points, which cannot separate 0.86 from 0.90.
say("\n  MC standard error on a coverage of 0.90 at ", N_REP_BASE,
    " replicates: ", sprintf("%.3f", sqrt(0.9 * 0.1 / N_REP_BASE)))

# ---- decision 2: U/phi separability across a design grid -------------------

say("\n== decision 2: U/phi separability grid")
grid <- expand.grid(n_conc = c(4, 6, 8, 12), top = c(0.3, 0.5, 0.7, 0.9))
res$grid <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
  g <- grid[i, ]
  say("  n_conc = ", g$n_conc, ", top/U = ", g$top)
  run_replicates(sprintf("grid_c%d_t%.1f", g$n_conc, g$top), N_REP_GRID,
                 g$n_conc, 5, top = g$top, U = 1.0)
}))
print(res$grid[, c("label", "n_conc", "top_true", "cover_U", "cover_phi",
                   "cor_U_phi", "max_abs_cor", "width_U", "divergences")])

# ---- degenerate case: curve never approaches the ceiling -------------------

say("\n== degenerate fixture: top/U = 0.2, U should revert toward its prior")
res$degenerate <- run_replicates("degenerate", N_REP_GRID, 8, 5,
                                 top = 0.2, U = 1.0)
print(res$degenerate[, c("cover_U", "med_U", "width_U", "cor_U_phi",
                         "top_over_U", "divergences")])

# ---- decision 3: prior sensitivity ----------------------------------------

say("\n== decision 3: U_loc misspecified at 0.5x and 2x")
res$prior_sens <- do.call(rbind, lapply(c(0.5, 1.0, 2.0), function(mult) {
  say("  U_loc = ", mult, " x truth")
  run_replicates(sprintf("U_loc_%.1fx", mult), N_REP_GRID, 8, 5,
                 top = 0.8, U = 1.0, U_loc = mult * 1.0, U_scale = 0.1)
}))
print(res$prior_sens[, c("label", "U_loc", "med_U", "cover_U", "nec_med",
                         "ec10_med", "ec50_med", "divergences")])

saveRDS(res, file.path(here, "phase0_results.rds"))
say("\nwritten: ", file.path(here, "phase0_results.rds"))
