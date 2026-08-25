# Dispersion sensitivity check for example7 (#193 review).
#
# The vignette's account of the NSEC runs through a GLOBAL residual scale:
# flooring removes the most extreme low values, compresses sigma, raises the
# control reference quantile and pulls the NSEC crossing left, which is why B1
# reads below A at high noise. Under a dispersion sub-model the control-region
# scale is estimated locally, where flooring removed nothing, so that channel
# should close. This fits scenario 8 twice -- once homoscedastic, once with
# disp("loglinear") -- on the SAME datasets, so the contrast is paired and
# isolates the dispersion spec.
#
# Slice i of K, so several of these run concurrently on disjoint datasets.
suppressMessages({library(devtools); load_all(".", quiet = TRUE); library(brms); library(dplyr)})
args <- commandArgs(TRUE); slice <- as.integer(args[1]); K <- as.integer(args[2])
N <- as.integer(args[3])
# --- generating model, copied from example7's own chunks ---
nec4param_curve <- function(x, top, bot, beta, nec) bot + (top-bot)*exp(-exp(beta)*(x-nec)*(x>=nec))
sim_truth <- function(R=2.3, delta=4, t=7, nec=1.3, beta=-3.573) {
  mu_0 <- log(R)/t
  list(R=R, delta=delta, t=t, top=mu_0, bot=-delta*mu_0, beta=beta, nec=nec,
       zero_crossing = nec + log((1+delta)/delta)/exp(beta))
}
sim_design <- function(truth, top_factor=1, n_conc=12, n_rep=5, n_control=10, span=100) {
  x_max <- top_factor*truth$zero_crossing
  concs <- exp(seq(log(x_max/span), log(x_max), length.out=n_conc))
  data.frame(x=c(rep(0,n_control), rep(concs, each=n_rep)))
}
sigma_0 <- 0.096*log(2.3)/7
sim_sigma <- function(x, truth, sigma_0, sigma_ratio=8.09) {
  mu <- nec4param_curve(x, truth$top, truth$bot, truth$beta, truth$nec)
  sigma_0*(1 + (sigma_ratio-1)*((truth$top-mu)/(truth$top-truth$bot)))
}
sim_one <- function(seed, R=2.3, delta=4, top_factor=2.0) {
  tr <- sim_truth(R=R, delta=delta); dz <- sim_design(tr, top_factor)
  set.seed(seed)
  mu <- nec4param_curve(dz$x, tr$top, tr$bot, tr$beta, tr$nec)
  data.frame(x=dz$x, y=rnorm(length(mu), mu, sim_sigma(dz$x, tr, sigma_0)))
}

TRUTH  <- sim_truth(R = 2.3, delta = 4)
TRUE_ERC50 <- 5.0533; TRUE_NEC <- 1.3
CTRL <- list(adapt_delta = 0.95)
FIT <- function(form, data, prior = NULL, inits_ok = TRUE) {
  a <- list(formula = form, data = data, family = gaussian(link = "identity"),
            iter = 2000, warmup = 1000, chains = 4, cores = 4, control = CTRL,
            refresh = 0, open_progress = FALSE, seed = 1)
  if (!is.null(prior)) a$prior <- prior
  suppressMessages(suppressWarnings(do.call(bnec, a)))
}
grab <- function(fit, arm, spec, i) {
  e <- try(ecx(fit, ecx_val = 50, type = "absolute"), silent = TRUE)
  n <- try(nsec(fit), silent = TRUE)
  sm <- try(summary(fit$fit)$fixed, silent = TRUE)
  data.frame(
    dataset = i, arm = arm, spec = spec,
    erc50 = if (inherits(e, "try-error")) NA else unname(e[1]),
    erc50_lo = if (inherits(e, "try-error")) NA else unname(e[2]),
    erc50_hi = if (inherits(e, "try-error")) NA else unname(e[3]),
    nsec  = if (inherits(n, "try-error")) NA else unname(n[1]),
    max_rhat = if (inherits(sm, "try-error")) NA else max(sm[, "Rhat"], na.rm = TRUE),
    divergent = tryCatch(sum(rstan::get_divergent_iterations(fit$fit$fit)), error = function(e) NA)
  )
}

out <- list()
idx <- seq(slice, N, by = K)
for (i in idx) {
  dat <- sim_one(1000 + i)
  for (spec in c("homosced", "disp")) {
    rhs <- if (spec == "homosced") "crf(x, \"nec4param\")"
           else "crf(x, \"nec4param\") + disp(\"loglinear\")"
    # A first: it is the reference AND the source of the shared prior, exactly
    # as in the vignette, so a change of arm is a change of likelihood only.
    fA <- try(FIT(as.formula(paste("y ~", rhs)), dat), silent = TRUE)
    if (inherits(fA, "try-error")) next
    shared <- get_priors(fA)
    out[[length(out) + 1]] <- grab(fA, "A", spec, i)

    # D's truncation point is read off A's own posterior, as the approach is defined
    fitA <- fitted(fA, newdata = data.frame(x = dat$x))[, "Estimate"]
    cross <- suppressWarnings(min(dat$x[fitA <= 0]))

    pinned <- shared; pinned$prior[pinned$nlpar == "bot"] <- "constant(0)"
    datB1 <- mutate(dat, y = pmax(y, 0))
    datC  <- mutate(dat, cens = ifelse(y < 0, "left", "none"), y = pmax(y, 0))

    jobs <- list(
      C  = list(as.formula(paste("y | cens(cens) ~", rhs)), datC,  shared),
      D  = list(as.formula(paste("y ~", rhs)), dat[dat$x < cross, ], shared),
      B1 = list(as.formula(paste("y ~", rhs)), datB1, shared),
      B2 = list(as.formula(paste("y ~", rhs)), dat,   pinned),
      B3 = list(as.formula(paste("y ~", rhs)), datB1, pinned))
    for (a in names(jobs)) {
      j <- jobs[[a]]
      f <- try(FIT(j[[1]], j[[2]], j[[3]]), silent = TRUE)
      if (inherits(f, "try-error")) {
        out[[length(out) + 1]] <- data.frame(dataset = i, arm = a, spec = spec,
          erc50 = NA, erc50_lo = NA, erc50_hi = NA, nsec = NA, max_rhat = NA, divergent = NA)
      } else out[[length(out) + 1]] <- grab(f, a, spec, i)
    }
  }
  write.csv(do.call(rbind, out),
            sprintf("notes/disp_sensitivity_slice_%02d.csv", slice), row.names = FALSE)
  cat(sprintf("[slice %d] dataset %d done (%d rows) %s\n", slice, i,
              length(out), format(Sys.time(), "%H:%M:%S")))
}
