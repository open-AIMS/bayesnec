# Measurements behind #309: the structure and the criterion of the
# initial-value search.
#
# Run from the package root with the working tree loaded:
#   Rscript notes/scripts/init_search_audit.R [name ...]
#
# With no argument every measurement is run. Naming one or more of
# acceptance, width, proposals, truncation, logdensity, shortrun runs those
# alone, which is how they were run for the pull request: the proposal count
# and the two Stan measurements are slow and are independent of each other.
#
# Five measurements, each printed with the numbers quoted in the pull request:
#
#   1. per-chain acceptance and the contribution of each clause, released
#      criterion and band, over five designs and the declining set;
#   2. the width and the spread the band uses, chosen against coverage of the
#      asymptotes of a known curve and containment of every observed group mean;
#   3. the proposals the whole declining set draws, released rule against the
#      change, at five seeds;
#   4. how much of the prior each criterion keeps and where the kept part sits;
#   5. the log density and its gradient at the accepted starting points, read
#      off the compiled Stan program with no sampling, and a short sampler run.
#
# Measurements 5 needs rstan and compiles one Stan program per equation; it is
# skipped where rstan is absent. Everything else is draws alone and takes a few
# minutes.
#
# The released search is reimplemented here rather than checked out, so the
# script runs against one working tree.

suppressMessages(devtools::load_all(".", quiet = TRUE))
library(bayesnec)
`%||%` <- function(a, b) if (is.null(a)) b else a

DECLINE <- models()$decline
CHAINS <- 4
CAP <- 1e4

# ---------------------------------------------------------------- designs ----
# Two packaged growth series, one packaged unit-interval series, and the two
# small designs the n_trials comment in R/inits_functions.R describes. The
# simulated ones are generated under a fixed seed so the script is
# reproducible.
designs <- function() {
  d_a <- alga[alga$species == "c_proliferum" & alga$contaminant == "A", ]
  d_b <- alga[alga$species == "r_salina" & alga$contaminant == "B", ]
  set.seed(20260910)
  small_rep_y <- c(rnorm(5, 0.9, 0.05), rnorm(5, 0.75, 0.05),
                   rnorm(5, 0.35, 0.05), rnorm(5, 0.12, 0.05))
  list(
    alga_cp_A = list(x = d_a$dose, y = d_a$sgr,
                     family = validate_family("gaussian")),
    alga_rs_B = list(x = d_b$dose, y = d_b$sgr,
                     family = validate_family("gaussian")),
    nec_data = list(x = nec_data$x, y = nec_data$y,
                    family = validate_family("Beta")),
    small_rep = list(x = rep(c(0, 1, 5, 20), each = 5), y = small_rep_y,
                     family = validate_family("gaussian")),
    small_unrep = list(x = c(0, 0.5, 1, 2, 4, 8, 16, 32),
                       y = c(0.98, 0.95, 0.88, 0.70, 0.42, 0.20, 0.08, 0.04),
                       family = validate_family("gaussian"))
  )
}

pred_args <- function(model) {
  pf <- get(paste0("pred_", model), envir = asNamespace("bayesnec"))
  list(fct = pf, args = setdiff(names(unlist(as.list(args(pf)))), "x"))
}

draw_one <- function(model, x, y, priors) {
  pa <- pred_args(model)
  init <- make_inits(model, pa$args, priors, chains = 1)[[1]]
  list(init = init,
       pred = get_init_predictions(init, sort(x), pa$fct, pa$args))
}

# The clauses of check_init_predictions(), scored separately.
clauses <- function(p, limits) {
  c(min_gt = isTRUE(min(p) > min(limits)),
    max_lt = isTRUE(max(p) < max(limits)),
    finite = !any(is.na(p)) && !any(is.infinite(p)) && !any(is.nan(p)),
    declines = isTRUE(p[1] > p[length(p)]),
    distinct = length(unique(p)) > 3)
}

design_parts <- function(dg) {
  yl <- response_link_scale(dg$y, dg$family)
  list(x = dg$x, y = yl,
       released = range(yl, na.rm = TRUE),
       band = init_limits(dg$x, yl),
       models = suppressMessages(
         check_models(DECLINE, dg$family, list(predictor = dg$x))))
}

# ------------------------------------------------- 1. acceptance per chain ----
measure_acceptance <- function(n_draw = 2000, seed = 30910) {
  set.seed(seed)
  out <- list()
  for (dn in names(designs())) {
    dg <- designs()[[dn]]
    p <- design_parts(dg)
    for (m in p$models) {
      pr <- try(suppressMessages(define_prior(m, dg$family, p$x, p$y)),
                silent = TRUE)
      if (inherits(pr, "try-error")) next
      cl_o <- matrix(NA, n_draw, 5)
      cl_n <- matrix(NA, n_draw, 5)
      for (i in seq_len(n_draw)) {
        d <- draw_one(m, p$x, p$y, pr)
        cl_o[i, ] <- clauses(d$pred, p$released)
        cl_n[i, ] <- clauses(d$pred, p$band)
      }
      out[[length(out) + 1]] <- data.frame(
        design = dn, model = m,
        released = mean(apply(cl_o, 1, all)),
        band = mean(apply(cl_n, 1, all)),
        released_min = mean(cl_o[, 1]), released_max = mean(cl_o[, 2]),
        band_min = mean(cl_n[, 1]), band_max = mean(cl_n[, 2]),
        declines = mean(cl_o[, 4]), distinct = mean(cl_o[, 5]),
        stringsAsFactors = FALSE)
    }
    message("acceptance: ", dn)
  }
  do.call(rbind, out)
}

# --------------------------------------------- 2. the width and the spread ----
# The spread the band widens by. "pooled" is what group_spread() implements;
# the other two were measured and rejected.
spreads <- list(
  pooled = function(x, y) group_spread(x, y),
  maxgrp = function(x, y) {
    g <- split(y, factor(x))
    n <- lengths(g)
    if (!any(n > 1)) return(sd(y))
    sqrt(max(vapply(g[n > 1], var, numeric(1))))
  },
  total = function(x, y) sd(y)
)

alt_band <- function(x, y, width, spread) {
  centres <- c(regularizing_location(x, y, "top")[["location"]],
               regularizing_location(x, y, "bot")[["location"]],
               replicated_group_means(x, y))
  s <- width * spreads[[spread]](x, y)
  c(min(centres) - s, max(centres) + s)
}

# R1: the band must contain the asymptotes of the curve that generated the
# data, or the search rejects a correct starting value.
measure_coverage <- function(ks = c(1, 2, 3, 4, 5), n_seed = 20,
                             seed = 309309) {
  set.seed(seed)
  grids <- list(wide = c(0, 0.1, 0.3, 1, 3, 10, 30, 100),
                narrow = c(0, 0.5, 1, 2, 4, 8),
                dense = round(exp(seq(log(0.01), log(100), length.out = 20)), 3))
  top <- 10; bot <- 2
  out <- list()
  for (gn in names(grids)) for (rp in c(1, 3, 6))
    for (eq in c("nec4param", "ecx4param", "ecxwb1"))
      for (shape in c("steep", "shallow"))
        for (het in c(FALSE, TRUE))
          for (s_i in seq_len(n_seed)) {
    x <- rep(grids[[gn]], each = rp)
    pars <- if (shape == "steep") {
      list(b_top = top, b_bot = bot, b_nec = 3, b_ec50 = 3, b_beta = 1.5)
    } else {
      list(b_top = top, b_bot = bot, b_nec = 20, b_ec50 = 20, b_beta = -1)
    }
    pa <- pred_args(eq)
    mu <- do.call(pa$fct, c(pars[pa$args], list(x = x)))
    # het: the dispersion rises fivefold across the series, which is what the
    # alga growth series do and what the disp() variance function models.
    sigma <- 0.1 * (top - bot) * if (het) 1 + 4 * rank(x) / length(x) else 1
    y <- rnorm(length(x), mu, sigma)
    for (sp in names(spreads)) for (k in ks) {
      b <- if (sp == "pooled") init_limits(x, y, width = k) else
        alt_band(x, y, k, sp)
      out[[length(out) + 1]] <- data.frame(
        grid = gn, reps = rp, eq = eq, shape = shape, het = het,
        spread = sp, k = k,
        full = top <= b[2] && top >= b[1] && bot >= b[1] && bot <= b[2],
        # A design whose predictor stops short of the crossing never reaches
        # its lower asymptote, so its bot anchor is biased and no width covers
        # it. Those cells are reported separately rather than counted.
        reaches = shape == "steep" || gn != "narrow",
        stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, out)
}

# The spread has to be robust as well: a single aberrant observation must not
# decide where the band is. Reported against range(y), which moves by the whole
# of it.
measure_robustness <- function(seed = 309) {
  set.seed(seed)
  x <- rep(c(0, 1, 5, 20), each = 6)
  y <- c(rnorm(6, 1, 0.02), rnorm(6, 0.8, 0.02), rnorm(6, 0.4, 0.02),
         rnorm(6, 0.1, 0.02))
  y2 <- y
  y2[1] <- 3
  out <- list()
  for (sp in names(spreads)) {
    a <- alt_band(x, y, 4, sp)
    b <- alt_band(x, y2, 4, sp)
    out[[length(out) + 1]] <- data.frame(
      quantity = paste0("band, spread = ", sp),
      clean_upper = a[2], spiked_upper = b[2],
      width_ratio = diff(b) / diff(a), stringsAsFactors = FALSE)
  }
  # The reference itself, before any widening: the top anchor against max(y).
  out[[length(out) + 1]] <- data.frame(
    quantity = "upper reference: control mean",
    clean_upper = regularizing_location(x, y, "top")[["location"]],
    spiked_upper = regularizing_location(x, y2, "top")[["location"]],
    width_ratio = NA_real_, stringsAsFactors = FALSE)
  out[[length(out) + 1]] <- data.frame(
    quantity = "upper reference: max(y)",
    clean_upper = max(y), spiked_upper = max(y2),
    width_ratio = diff(range(y2)) / diff(range(y)), stringsAsFactors = FALSE)
  do.call(rbind, out)
}

# ------------------------------------------------------- 3. the proposals ----
# The released rule: every chain drawn again whenever any one failed. Both
# reimplementations exclude refine_inits(), so the comparison is of the two
# rules and not of a rescue both keep.
search_released <- function(model, x, y, priors, chains = CHAINS,
                            n_trials = CAP) {
  limits <- range(y, na.rm = TRUE)
  pa <- pred_args(model)
  xs <- sort(x)
  drawn <- 0
  ok_all <- function(inits) all(vapply(inits, function(i)
    check_init_predictions(get_init_predictions(i, xs, pa$fct, pa$args),
                           limits), logical(1)))
  inits <- make_inits(model, pa$args, priors, chains)
  drawn <- drawn + chains
  good <- ok_all(inits)
  n_t <- 1
  while (!good && n_t <= n_trials) {
    inits <- make_inits(model, pa$args, priors, chains)
    drawn <- drawn + chains
    good <- ok_all(inits)
    n_t <- n_t + 1
  }
  c(drawn = drawn, success = as.numeric(good))
}

search_changed <- function(model, x, y, priors, chains = CHAINS,
                           n_trials = CAP) {
  limits <- init_limits(x, y)
  pa <- pred_args(model)
  xs <- sort(x)
  drawn <- 0
  filled <- rep(FALSE, chains)
  n_t <- 0
  while (any(!filled) && n_t < n_trials) {
    need <- sum(!filled)
    inits <- make_inits(model, pa$args, priors, need)
    drawn <- drawn + need
    ok <- vapply(inits, function(i)
      check_init_predictions(get_init_predictions(i, xs, pa$fct, pa$args),
                             limits), logical(1))
    filled[which(!filled)[ok]] <- TRUE
    n_t <- n_t + 1
  }
  c(drawn = drawn, success = as.numeric(all(filled)))
}

measure_proposals <- function(seeds = 1:5) {
  out <- list()
  for (dn in names(designs())) {
    dg <- designs()[[dn]]
    p <- design_parts(dg)
    for (m in p$models) {
      pr <- try(suppressMessages(define_prior(m, dg$family, p$x, p$y)),
                silent = TRUE)
      if (inherits(pr, "try-error")) next
      for (s in seeds) {
        set.seed(1000 * s + 7)
        a <- search_released(m, p$x, p$y, pr)
        set.seed(1000 * s + 7)
        b <- search_changed(m, p$x, p$y, pr)
        out[[length(out) + 1]] <- data.frame(
          design = dn, model = m, seed = s,
          released_drawn = a[["drawn"]], released_ok = a[["success"]],
          changed_drawn = b[["drawn"]], changed_ok = b[["success"]],
          stringsAsFactors = FALSE)
      }
    }
    message("proposals: ", dn)
  }
  do.call(rbind, out)
}

# ------------------------------------------- 4. what the criterion keeps -----
# The accepted values are a draw from the prior restricted to the region the
# criterion admits, so the question is how much of the prior each keeps and
# where the kept part sits, in units of the prior's own standard deviation.
measure_truncation <- function(n_draw = 6000,
                               which_designs = c("alga_cp_A", "alga_rs_B",
                                                 "small_rep"),
                               seed = 30912) {
  set.seed(seed)
  out <- list()
  for (dn in which_designs) {
    dg <- designs()[[dn]]
    p <- design_parts(dg)
    for (m in p$models) {
      pr <- try(suppressMessages(define_prior(m, dg$family, p$x, p$y)),
                silent = TRUE)
      if (inherits(pr, "try-error")) next
      draws <- vector("list", n_draw)
      ok <- matrix(FALSE, n_draw, 2)
      for (i in seq_len(n_draw)) {
        d <- draw_one(m, p$x, p$y, pr)
        draws[[i]] <- unlist(d$init)
        ok[i, ] <- c(check_init_predictions(d$pred, p$released),
                     check_init_predictions(d$pred, p$band))
      }
      dm <- do.call(rbind, draws)
      for (par in colnames(dm)) {
        s <- sd(dm[, par])
        if (!is.finite(s) || s == 0) next
        out[[length(out) + 1]] <- data.frame(
          design = dn, model = m, par = par,
          keep_released = mean(ok[, 1]), keep_band = mean(ok[, 2]),
          shift_released = (median(dm[ok[, 1], par]) - median(dm[, par])) / s,
          shift_band = (median(dm[ok[, 2], par]) - median(dm[, par])) / s,
          stringsAsFactors = FALSE)
      }
    }
    message("truncation: ", dn)
  }
  do.call(rbind, out)
}

# -------------------------------- 5. the log density at the start point ------
# What a starting value has to do is give Stan a finite log density and a
# finite gradient, and not sit so far into a tail that warmup is spent
# travelling. Both are read off the compiled Stan program of the fit bnec()
# would build, with no sampling.
measure_log_density <- function(
    eqs = c("nec3param", "nec4param", "ecx4param", "ecxwb1", "ecxll4",
            "ecxlin"),
    widths = c(2, 3, 4, 5), n_accept = 400, seed = 3091) {
  if (!requireNamespace("rstan", quietly = TRUE)) {
    message("rstan not installed; skipping the log-density measurement")
    return(NULL)
  }
  set.seed(seed)
  d <- alga[alga$species == "c_proliferum" & alga$contaminant == "A", ]
  fam <- validate_family("gaussian")
  yl <- response_link_scale(d$sgr, fam)
  criteria <- c(list(released = range(yl, na.rm = TRUE)),
                stats::setNames(lapply(widths, function(k)
                  init_limits(d$dose, yl, width = k)), paste0("band_k", widths)))
  # sigma is not part of the init search under either criterion, so holding it
  # at the residual spread makes the comparison one of the curve parameters
  # alone.
  sigma_fixed <- group_spread(d$dose, yl)
  out <- list()
  for (eq in eqs) {
    # The equation name is written into the formula rather than referred to
    # through a variable: bnec() re-evaluates the formula outside this frame,
    # so `model = eq` reaches it as "object 'eq' not found".
    form <- stats::as.formula(sprintf("sgr ~ crf(dose, model = \"%s\")", eq))
    fit <- try(suppressMessages(suppressWarnings(
      bnec(form, data = d, chains = 1, iter = 150,
           warmup = 100, refresh = 0, backend = "rstan", seed = 1,
           open_progress = FALSE))), silent = TRUE)
    if (inherits(fit, "try-error")) {
      message("could not compile ", eq, ": ", conditionMessage(attr(fit,
              "condition")))
      next
    }
    sf <- fit$fit$fit
    pr <- suppressMessages(define_prior(eq, fam, d$dose, yl))
    score <- function(init) {
      pl <- c(lapply(init, function(z) array(as.numeric(z), dim = 1)),
              list(sigma = sigma_fixed))
      up <- try(rstan::unconstrain_pars(sf, pl), silent = TRUE)
      if (inherits(up, "try-error")) return(c(NA, NA))
      lp <- try(rstan::log_prob(sf, up), silent = TRUE)
      gr <- try(rstan::grad_log_prob(sf, up), silent = TRUE)
      c(if (inherits(lp, "try-error")) NA else lp,
        if (inherits(gr, "try-error")) NA else max(abs(gr)))
    }
    for (cn in names(criteria)) {
      lim <- criteria[[cn]]
      vals <- matrix(NA_real_, n_accept, 2)
      i <- 0
      att <- 0
      while (i < n_accept && att < 2e5) {
        att <- att + 1
        dd <- draw_one(eq, d$dose, yl, pr)
        if (check_init_predictions(dd$pred, lim)) {
          i <- i + 1
          vals[i, ] <- score(dd$init)
        }
      }
      out[[length(out) + 1]] <- data.frame(
        eq = eq, criterion = cn, accept = n_accept / att,
        finite = mean(is.finite(vals[, 1]) & is.finite(vals[, 2])),
        lp_median = median(vals[, 1], na.rm = TRUE),
        lp_q10 = unname(quantile(vals[, 1], 0.1, na.rm = TRUE)),
        grad_median = median(vals[, 2], na.rm = TRUE),
        grad_max = max(vals[, 2], na.rm = TRUE), stringsAsFactors = FALSE)
    }
    message("log density: ", eq)
  }
  do.call(rbind, out)
}

# A short run says whether the chains get going, which a posterior comparison
# does not: given convergence the posterior is the same wherever the chains
# start, so a match there confirms the sampler rather than the starting values.
measure_short_run <- function(eqs = c("nec3param", "nec4param", "ecx4param",
                                      "ecxwb1"), seeds = 1:3) {
  d <- alga[alga$species == "c_proliferum" & alga$contaminant == "A", ]
  out <- list()
  for (eq in eqs) for (s in seeds) {
    t0 <- Sys.time()
    form <- stats::as.formula(sprintf("sgr ~ crf(dose, model = \"%s\")", eq))
    fit <- try(suppressMessages(suppressWarnings(
      bnec(form, data = d, chains = 4, iter = 700,
           warmup = 400, refresh = 0, seed = s))), silent = TRUE)
    if (inherits(fit, "try-error")) next
    np <- brms::nuts_params(fit$fit)
    out[[length(out) + 1]] <- data.frame(
      eq = eq, seed = s,
      secs = as.numeric(Sys.time() - t0, units = "secs"),
      divergent = sum(np$Value[np$Parameter == "divergent__"]),
      stepsize = median(np$Value[np$Parameter == "stepsize__"]),
      max_rhat = max(brms::rhat(fit$fit), na.rm = TRUE),
      min_ess = min(summary(fit)$bayesnecfit$Bulk_ESS %||% NA, na.rm = TRUE),
      stringsAsFactors = FALSE)
    message("short run: ", eq, " seed ", s)
  }
  do.call(rbind, out)
}

# ------------------------------------------------------------------ run ------
selected <- commandArgs(trailingOnly = TRUE)
run_this <- function(name) length(selected) == 0 || name %in% selected

if (run_this("acceptance")) {
  acc <- measure_acceptance()
  cat("\n=== 1. per-chain acceptance, released criterion and band ===\n")
  print(acc, digits = 3, row.names = FALSE)
}

if (run_this("width")) {
  cov <- measure_coverage()
  cat("\n=== 2a. coverage of the true asymptotes, by spread and width ===\n")
  sub <- cov[cov$reaches, ]
  cel <- aggregate(full ~ spread + k + grid + shape + reps + eq + het, sub, mean)
  for (sp in unique(cel$spread)) for (k in sort(unique(cel$k))) {
    s <- cel[cel$spread == sp & cel$k == k, ]
    cat(sprintf("%-7s k=%4.1f  complete in %2d of %d cells\n", sp, k,
                sum(s$full == 1), nrow(s)))
  }
  cat("\n  designs whose predictor stops short of the lower asymptote:\n")
  print(aggregate(full ~ spread + k, cov[!cov$reaches, ], mean), digits = 3,
        row.names = FALSE)
  cat("\n=== 2b. one aberrant observation ===\n")
  print(measure_robustness(), digits = 4, row.names = FALSE)
  cat("\n=== 2c. the band as implemented, on each design ===\n")
  for (dn in names(designs())) {
    dg <- designs()[[dn]]
    p <- design_parts(dg)
    gm <- vapply(split(p$y, factor(p$x)), mean, numeric(1))
    cat(sprintf(
      "%-12s range(y) [%8.4f,%8.4f]  level means [%8.4f,%8.4f]  band [%8.4f,%8.4f]  spread %7.4f\n",
      dn, min(p$y), max(p$y), min(gm), max(gm), p$band[1], p$band[2],
      group_spread(p$x, p$y)))
  }
  cat("\n  band width as a multiple of the response range, by spread:\n")
  for (dn in names(designs())) {
    dg <- designs()[[dn]]
    p <- design_parts(dg)
    cat(sprintf("%-12s", dn))
    for (sp in names(spreads)) {
      cat(sprintf("  %-7s %6.2f", sp,
                  diff(alt_band(p$x, p$y, 4, sp)) / diff(range(p$y))))
    }
    cat("\n")
  }
}

if (run_this("proposals")) {
  prop <- measure_proposals()
  cat("\n=== 3. proposals drawn, released rule against the change ===\n")
  tot <- aggregate(cbind(released_drawn, changed_drawn) ~ design + seed, prop,
                   sum)
  print(aggregate(cbind(released_drawn, changed_drawn) ~ design, tot, mean),
        digits = 6, row.names = FALSE)
  cat("\n  per equation, mean over seeds:\n")
  print(aggregate(cbind(released_drawn, changed_drawn) ~ design + model, prop,
                  mean), digits = 6, row.names = FALSE)
  cat("\n  searches that exhausted the cap, of ", nrow(prop), ": released ",
      sum(prop$released_ok == 0), ", changed ", sum(prop$changed_ok == 0),
      "\n", sep = "")
  print(unique(prop[prop$released_ok == 0, c("design", "model")]),
        row.names = FALSE)
}

if (run_this("truncation")) {
  tr <- measure_truncation()
  cat("\n=== 4. what each criterion keeps of the prior ===\n")
  for (par in c("b_top", "b_bot", "b_nec", "b_ec50", "b_beta")) {
    s <- tr[tr$par == par, ]
    if (!nrow(s)) next
    cat(sprintf(
      "%-8s n=%3d  kept: released %.2f band %.2f   median shift, prior sd: released %+.3f band %+.3f\n",
      par, nrow(s), median(s$keep_released), median(s$keep_band),
      median(s$shift_released), median(s$shift_band)))
  }
  cat("\n  b_top, by design and equation:\n")
  print(tr[tr$par == "b_top", ], digits = 3, row.names = FALSE)
}

if (run_this("logdensity")) {
  ld <- measure_log_density()
  if (!is.null(ld)) {
    cat("\n=== 5a. log density and gradient at the accepted starting points ===\n")
    print(ld, digits = 4, row.names = FALSE)
  }
}

if (run_this("shortrun")) {
  sr <- try(measure_short_run(), silent = TRUE)
  if (!inherits(sr, "try-error") && !is.null(sr)) {
    cat("\n=== 5b. short sampler run ===\n")
    print(sr, digits = 4, row.names = FALSE)
  }
}
