# =====================================================================
# Fitted validation of the incomplete-design prior changes (#418)
#
# PR #409 removed the tested-range truncation of the `nec` and `ec50`
# priors (#393), added the `asymptote_observed` declaration (#394),
# censored-draw reporting (#395) and `extrapolate` (#392). The prior
# audit (notes/prior_audit.md parts 3 to 5) measured the priors and the
# initial-value search and fitted nothing. This script fits the designs
# and records what #418 lists. Results: notes/incomplete_design_fits.md.
#
# Builds
#   base  eff06ea91508e3e516efbc01b5aa22c4243af8d4, the parent of the
#         #386 programme
#   cand  c3824643de2840d85fbf3fec3a043d22bcec8220, the merge of PR #409
# Both report Version 2.1.3.39, so the build is identified by commit, not
# by packageVersion(). Only #393 changes a default-path fit between the
# two, so base against cand at defaults isolates the removal of the
# truncation, and cand with and without `asymptote_observed = FALSE`
# isolates the declaration.
#
# Build and library mechanics. Each build is installed from a detached
# worktree into a library of its own, and the commit is written beside
# it so that every fit can assert which build it loaded:
#
#   git worktree add --detach <s>/wt-base eff06ea9
#   git worktree add --detach <s>/wt-cand c3824643
#   R CMD INSTALL --no-test-load --library=<s>/lib-base <s>/wt-base
#   R CMD INSTALL --no-test-load --library=<s>/lib-cand <s>/wt-cand
#   git -C <s>/wt-base rev-parse HEAD > <s>/lib-base/bayesnec.commit
#   git -C <s>/wt-cand rev-parse HEAD > <s>/lib-cand/bayesnec.commit
#
# The fit mode prepends the build's library with .libPaths() rather than
# setting R_LIBS_USER, which would replace the user library (where brms
# and rstan live) instead of adding to it. It then asserts that
# find.package("bayesnec") resolves inside that library and that the
# recorded commit is the expected one, and stops otherwise.
#
# Running. From the repository root, with V418_LIB_BASE and V418_LIB_CAND
# naming the two libraries and V418_OUT a directory for the per-fit
# records (default ignore/incomplete_design_fits, which is git-ignored):
#
#   Rscript notes/scripts/incomplete_design_fits.R list |
#     xargs -P 8 -n 1 Rscript notes/scripts/incomplete_design_fits.R fit
#   Rscript notes/scripts/incomplete_design_fits.R report
#   Rscript notes/scripts/incomplete_design_fits.R markdown
#
# `list` prints the fit identifiers. `fit <id>` runs one fit in its own
# process and saves the fitted object beside its record; `extract <id>`
# rebuilds the record from that object without refitting. `report`
# prints every recorded quantity, and `markdown` prints the tables in
# notes/incomplete_design_fits.md. Every fit is a separate Rscript
# process: a Stan fit inside a future multisession worker deadlocks on
# the machine this was run on. Each process samples its four chains in
# sequence (mc.cores 1), so eight concurrent processes use eight cores.
#
# Provenance of the recorded run: 2026-09-26, R 4.6.1, brms 2.23.0,
# rstan 2.32.7, backend rstan. 24 fits, eight at a time, 1,667 s of wall
# clock; the records were then rebuilt with `extract` from the saved fits
# once the extraction code was final.
# =====================================================================

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) == 0) "list" else args[1]

# One data seed, per RF's decision on #418. The same seed is passed to
# every bnec() call, which seeds both brms and the initial-value search.
SEED_DATA <- 418L
SEED_FIT <- 418L

OUT <- Sys.getenv("V418_OUT", "ignore/incomplete_design_fits")
COMMITS <- c(base = "eff06ea91508e3e516efbc01b5aa22c4243af8d4",
             cand = "c3824643de2840d85fbf3fec3a043d22bcec8220")

# ---- curve generators, copied from notes/scripts/prior_audit.R --------
mu_nec4 <- function(x, top, bot, nec, beta) {
  bot + (top - bot) * exp(-exp(beta) * (x - nec) * ifelse(x - nec < 0, 0, 1))
}
mu_ecx4 <- function(x, top, bot, ec50, beta) {
  top + (bot - top) / (1 + exp((ec50 - x) * exp(beta)))
}
shifted_threshold <- function(model, x_max, rate, f) {
  if (model == "nec4param") {
    x_max + log(1 - f) / rate
  } else {
    x_max + log((1 - f) / f) / rate
  }
}

# ---- the series and the response --------------------------------------
# The `linear` series and replication of the audit's sweep, with its
# gaussian response (top 10, bot 2, residual sd 0.6), so that each design
# below is a sweep cell at the identity transform and identity link. The
# response is positive throughout, which the declaration needs on the
# gaussian branch: its floor is zero only for a non-negative response.
#
# #386's own series (eight log-spaced concentrations to 40) was not used.
# It has no concentration between 10 and 40, so every design whose decline
# begins above 10 shows it at one concentration only, and an ecx4param
# curve that is complete at both ends has at most one concentration on its
# slope. The evenly spaced series shows the ecx4param decline over several
# concentrations on each design.
x_series <- seq(0, 10, length.out = 11)
n_rep <- 6
top_true <- 10
bot_true <- 2
sigma_true <- 0.6
x_max <- max(x_series)

# The audit's rate rule: a decay of 5 over the distance from the nominal
# threshold to the highest concentration on the complete design, held
# fixed while the threshold is moved to reach a stated fraction of the
# span at the highest concentration.
rate_nec <- 5 / (x_max - 4)
rate_ecx <- 5 / (x_max - 5)
complete_f <- 1 - exp(-5)

designs <- list(
  nec4param_complete = list(
    model = "nec4param", par = "nec", rate = rate_nec,
    threshold = 4, setting = "complete"),
  # f = 0.36: the lower asymptote is not observed and the threshold is
  # inside the series. For nec4param the audit's completeness axis never
  # moves the threshold out of the series.
  nec4param_incomplete = list(
    model = "nec4param", par = "nec", rate = rate_nec,
    threshold = shifted_threshold("nec4param", x_max, rate_nec, 0.36),
    setting = "f36"),
  # A threshold above the series. The response is at `top` at every
  # concentration, as on #386's nec = 60 against 40; the ratio 1.5 is
  # #386's.
  nec4param_above = list(
    model = "nec4param", par = "nec", rate = rate_nec,
    threshold = 1.5 * x_max, setting = "above"),
  ecx4param_complete = list(
    model = "ecx4param", par = "ec50", rate = rate_ecx,
    threshold = 5, setting = "complete"),
  # f = 0.92: the lower asymptote is not observed and the midpoint is
  # inside the series. An ecx4param midpoint leaves the series once f
  # falls below one half, so this is the incomplete setting of the audit's
  # axis that keeps it inside.
  ecx4param_incomplete = list(
    model = "ecx4param", par = "ec50", rate = rate_ecx,
    threshold = shifted_threshold("ecx4param", x_max, rate_ecx, 0.92),
    setting = "f92"),
  # f = 0.36: the midpoint is above the series and the lower asymptote is
  # not observed.
  ecx4param_above = list(
    model = "ecx4param", par = "ec50", rate = rate_ecx,
    threshold = shifted_threshold("ecx4param", x_max, rate_ecx, 0.36),
    setting = "f36")
)

design_mu <- function(d, x) {
  if (d$model == "nec4param") {
    mu_nec4(x, top_true, bot_true, d$threshold, log(d$rate))
  } else {
    mu_ecx4(x, top_true, bot_true, d$threshold, log(d$rate))
  }
}

# One vector of residuals, drawn once and added to every design's mean,
# so that the six datasets differ in their mean curve and in nothing
# else. That is what "one data seed" means here.
simulate_data <- function() {
  set.seed(SEED_DATA)
  x <- rep(x_series, each = n_rep)
  eps <- rnorm(length(x), 0, sigma_true)
  lapply(designs, function(d) {
    y <- design_mu(d, x) + eps
    stopifnot(all(y > 0))
    data.frame(x = x, y = y)
  })
}

# The true absolute EC50, measured from the control as ecx() measures it:
# the concentration at which the mean falls to half its value at x = 0.
true_ec50_absolute <- function(d) {
  target <- 0.5 * design_mu(d, 0)
  uniroot(function(x) design_mu(d, x) - target, c(0, 1e3))$root
}

# ---- the fits ----------------------------------------------------------
# The default prior_type is "uninformative" on both builds, so the second
# setting #418 asks for is "regularizing".
incomplete <- setdiff(names(designs),
                      c("nec4param_complete", "ecx4param_complete"))
fit_table <- rbind(
  data.frame(build = "base", design = names(designs),
             prior_type = "uninformative", declared = FALSE),
  data.frame(build = "cand", design = names(designs),
             prior_type = "uninformative", declared = FALSE),
  data.frame(build = "cand", design = incomplete,
             prior_type = "uninformative", declared = TRUE),
  data.frame(build = "base", design = incomplete,
             prior_type = "regularizing", declared = FALSE),
  data.frame(build = "cand", design = incomplete,
             prior_type = "regularizing", declared = FALSE)
)
fit_table$id <- with(fit_table, paste(
  build, design, ifelse(prior_type == "regularizing", "reg", "uninf"),
  ifelse(declared, "declared", "default"), sep = "__"))

if (mode == "list") {
  cat(fit_table$id, sep = "\n")
  quit(save = "no")
}

# ---- prior helpers, after notes/scripts/prior_audit.R ------------------
parse_prior <- function(s) {
  if (is.na(s) || !nzchar(s)) return(NULL)
  fam <- sub("\\(.*$", "", s)
  a <- as.numeric(strsplit(gsub("^.*\\(|\\)$", "", s), ",")[[1]])
  list(fam = fam, a = a[1], b = a[2])
}
cdf_of <- function(p, q) switch(p$fam,
  gamma = pgamma(q, p$a, p$b), normal = pnorm(q, p$a, p$b),
  lognormal = plnorm(q, p$a, p$b), beta = pbeta(q, p$a, p$b),
  rep(NA_real_, length(q)))
qf_of <- function(p, q) switch(p$fam,
  gamma = qgamma(q, p$a, p$b), normal = qnorm(q, p$a, p$b),
  lognormal = qlnorm(q, p$a, p$b), beta = qbeta(q, p$a, p$b),
  rep(NA_real_, length(q)))
# The prior after truncation to its declared bounds, which is the density
# Stan samples: its central 95%, its CDF at the truth, and the mass it
# places above the highest concentration tested.
prior_facts <- function(pstr, lb, ub, truth, above = NA) {
  p <- parse_prior(pstr)
  if (is.null(p)) return(NULL)
  lo <- suppressWarnings(as.numeric(lb))
  hi <- suppressWarnings(as.numeric(ub))
  lo <- if (is.na(lo)) -Inf else lo
  hi <- if (is.na(hi)) Inf else hi
  Flo <- if (is.infinite(lo)) 0 else cdf_of(p, lo)
  Fhi <- if (is.infinite(hi)) 1 else cdf_of(p, hi)
  mass <- Fhi - Flo
  tq <- function(u) qf_of(p, Flo + u * mass)
  pt <- if (truth <= lo) 0 else if (truth >= hi) 1 else
    (cdf_of(p, truth) - Flo) / mass
  pa <- if (is.na(above)) NA_real_ else if (above >= hi) 0 else
    (Fhi - cdf_of(p, max(above, lo))) / mass
  list(q025 = tq(0.025), q975 = tq(0.975), p_truth = pt, p_above = pa)
}

# ---- loading a build ----------------------------------------------------
load_build <- function(build) {
  lib <- Sys.getenv(paste0("V418_LIB_", toupper(build)))
  stopifnot(nzchar(lib), dir.exists(lib))
  .libPaths(c(lib, .libPaths()))
  suppressPackageStartupMessages(library(bayesnec))
  loaded <- normalizePath(find.package("bayesnec"))
  if (!startsWith(loaded, normalizePath(lib))) {
    stop("bayesnec resolved to ", loaded, ", not to ", lib)
  }
  commit <- readLines(file.path(lib, "bayesnec.commit"))
  if (!identical(commit, COMMITS[[build]])) {
    stop("library ", lib, " holds ", commit, ", expected ", COMMITS[[build]])
  }
  # A second check that does not depend on the commit file: the
  # declaration exists on the candidate and not on the base.
  stopifnot(("asymptote_observed" %in% names(formals(bnec))) ==
              (build == "cand"))
  commit
}

# ---- what is recorded from one fitted object ---------------------------
# Every reporting check reads the fitted object; nothing is refitted.
extract_record <- function(fit, res, d, dat) {
  bf <- fit$fit
  truth <- res$truth
  stopifnot(identical(bf$family$family, "gaussian"),
            identical(bf$family$link, "identity"))

  # Initial values: a list of per-chain lists where the search succeeded,
  # list(random = "random") where it fell back to Stan's own.
  res$init_fallback <-
    any(grepl("failed to find initial values", res$fit_messages)) ||
    identical(fit$init, list(random = "random"))

  # The priors as the fit stores them. brms lists each non-linear
  # parameter twice, once for the class and once for its Intercept
  # coefficient; the populated row is the one bayesnec set.
  pr <- as.data.frame(pull_prior(fit)[[1]])
  pr <- pr[pr$nlpar %in% c("top", "bot", "beta", d$par) & pr$class == "b" &
             nzchar(pr$prior), c("prior", "nlpar", "coef", "lb", "ub")]
  pr <- pr[!duplicated(pr$nlpar), ]
  res$priors <- pr
  res$prior_facts <- lapply(setNames(pr$nlpar, pr$nlpar), function(np) {
    r <- pr[pr$nlpar == np, ]
    prior_facts(r$prior, r$lb, r$ub, truth[[np]],
                above = if (np == d$par) x_max else NA)
  })

  # Posterior summaries, with the truth's position in each posterior.
  pars <- c(paste0("b_", c("top", "bot", "beta", d$par), "_Intercept"),
            "sigma")
  dr <- posterior::as_draws_df(bf, variable = pars)
  summ <- posterior::summarise_draws(
    dr, median = median,
    q025 = function(v) unname(quantile(v, 0.025)),
    q975 = function(v) unname(quantile(v, 0.975)),
    sd = sd, "rhat", "ess_bulk", "ess_tail")
  summ$par <- sub("^b_(.*)_Intercept$", "\\1", summ$variable)
  summ$truth <- truth[summ$par]
  summ$cdf_at_truth <- vapply(seq_len(nrow(summ)), function(i) {
    mean(as.numeric(dr[[summ$variable[i]]]) <= summ$truth[i])
  }, numeric(1))
  # Prior standard deviation from the prior draws brms stores under
  # sample_prior = "yes", for a contraction ratio (posterior sd / prior
  # sd) on draws from the same, bounded, prior Stan used.
  pd <- try(brms::prior_draws(bf), silent = TRUE)
  summ$prior_sd <- vapply(summ$par, function(p) {
    if (inherits(pd, "try-error")) return(NA_real_)
    col <- grep(paste0("^(b_)?", p, "(_Intercept)?$"), names(pd),
                value = TRUE)
    if (length(col) == 0) NA_real_ else sd(pd[[col[1]]])
  }, numeric(1))
  res$prior_draw_names <- if (inherits(pd, "try-error")) NA else names(pd)
  res$posterior <- as.data.frame(summ)
  thr <- as.numeric(dr[[paste0("b_", d$par, "_Intercept")]])
  res$threshold_above <- mean(thr > x_max)
  res$threshold_below <- mean(thr < min(dat$x))
  res$threshold_mcse_median <- posterior::mcse_median(thr)

  np <- brms::nuts_params(bf)
  res$divergent <- sum(np$Value[np$Parameter == "divergent__"])
  res$treedepth_max <- max(np$Value[np$Parameter == "treedepth__"])
  res$treedepth_hits <- sum(np$Value[np$Parameter == "treedepth__"] >= 10)
  res$n_draws <- nrow(dr)

  # The reported estimates, with their censoring marks where the build
  # writes them.
  mark <- function(est) {
    cs <- attr(est, "censored_summary")
    list(values = as.numeric(est),
         bound = if (is.null(cs)) rep("", length(est)) else cs$bound,
         n_above = if (is.null(cs)) NA else cs$n_above,
         n_below = if (is.null(cs)) NA else cs$n_below,
         n_draws = if (is.null(cs)) NA else cs$n_draws)
  }
  est_capture <- function(expr) {
    m <- character(0)
    w <- character(0)
    v <- tryCatch(withCallingHandlers(expr,
      message = function(e) {
        m <<- c(m, conditionMessage(e))
        invokeRestart("muffleMessage")
      },
      warning = function(e) {
        w <<- c(w, conditionMessage(e))
        invokeRestart("muffleWarning")
      }), error = function(e) e)
    if (inherits(v, "error")) {
      return(list(error = conditionMessage(v), messages = m, warnings = w))
    }
    c(mark(v), list(messages = m, warnings = w))
  }
  res$ne_type <- fit$ne_type
  # What summary() prints as the no-effect estimate: the NEC for
  # nec4param, the NSEC read off the curve for ecx4param.
  res$ne <- est_capture(fit$ne)
  res$ecx50 <- est_capture(ecx(fit, ecx_val = 50))
  if (res$build == "cand") {
    if (d$model == "nec4param") {
      res$ne_extrapolated <- est_capture(nec(fit, extrapolate = TRUE))
    } else {
      # An NSEC is read off a curve, and no curve can be evaluated on an
      # infinite grid, so extrapolate = TRUE is refused on ecx4param; a
      # finite limit of ten times the highest concentration is used, and
      # the same grid for the EC50.
      res$ne_extrapolated <- est_capture(nsec(fit, extrapolate = 10 * x_max))
      res$ecx50_extrapolated <- est_capture(
        ecx(fit, ecx_val = 50, x_range = c(0, 10 * x_max)))
    }
  }
  res
}

# ---- fit mode ----------------------------------------------------------
# `fit <id>` fits and records. The fitted object is saved beside the
# record, so `extract <id>` can rebuild the record without refitting.
if (mode %in% c("fit", "extract")) {
  id <- args[2]
  row <- fit_table[fit_table$id == id, ]
  stopifnot(nrow(row) == 1)
  commit <- load_build(row$build)
  dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
  dat <- simulate_data()[[row$design]]
  d <- designs[[row$design]]
  fit_file <- file.path(OUT, paste0(id, "_fit.rds"))

  if (mode == "fit") {
    truth <- c(top = top_true, bot = bot_true, beta = log(d$rate),
               sigma = sigma_true)
    truth[[d$par]] <- d$threshold
    # The family is named. Left to its default, bnec() chooses it from the
    # response, and a positive continuous response is fitted as Gamma; the
    # first run of this script did exactly that. gaussian() takes the
    # identity link, which validate_family() would impose in any case.
    call_args <- list(
      formula = as.formula(sprintf('y ~ crf(x, model = "%s")', d$model)),
      data = dat, family = gaussian(), prior_type = row$prior_type,
      seed = SEED_FIT)
    if (row$declared) call_args$asymptote_observed <- FALSE

    msgs <- character(0)
    warns <- character(0)
    capture <- function(expr) {
      withCallingHandlers(expr,
        message = function(m) {
          msgs <<- c(msgs, conditionMessage(m))
          invokeRestart("muffleMessage")
        },
        warning = function(w) {
          warns <<- c(warns, conditionMessage(w))
          invokeRestart("muffleWarning")
        })
    }
    set.seed(SEED_FIT)
    t0 <- Sys.time()
    fit <- tryCatch(capture(do.call(bnec, call_args)),
                    error = function(e) e)
    elapsed <- as.numeric(Sys.time() - t0, units = "secs")

    res <- list(id = id, build = row$build, commit = commit,
                design = row$design, model = d$model, par = d$par,
                setting = d$setting, prior_type = row$prior_type,
                declared = row$declared, family = "gaussian",
                seed_data = SEED_DATA,
                seed_fit = SEED_FIT, truth = truth,
                r_version = R.version.string,
                brms_version = as.character(packageVersion("brms")),
                rstan_version = as.character(packageVersion("rstan")),
                elapsed = elapsed, fit_messages = msgs,
                fit_warnings = warns)
    res$observed <- list(
      x_range = range(dat$x),
      mean_control = mean(dat$y[dat$x == min(dat$x)]),
      mean_max = mean(dat$y[dat$x == max(dat$x)]),
      mu_max = design_mu(d, max(dat$x)),
      f_reached = (top_true - design_mu(d, max(dat$x))) /
        (top_true - bot_true))
    if (d$model == "ecx4param") {
      res$truth_ec50_absolute <- true_ec50_absolute(d)
    }
    res$failed <- if (inherits(fit, "error")) {
      conditionMessage(fit)
    } else {
      NA_character_
    }
    saveRDS(list(fit = if (inherits(fit, "error")) NULL else fit, res = res),
            fit_file)
  } else {
    saved <- readRDS(fit_file)
    fit <- saved$fit
    res <- saved$res
  }
  if (is.na(res$failed)) {
    res <- extract_record(fit, res, d, dat)
  }
  saveRDS(res, file.path(OUT, paste0(id, ".rds")))
  quit(save = "no")
}

# ---- report mode -------------------------------------------------------
load_records <- function() {
  files <- file.path(OUT, paste0(fit_table$id, ".rds"))
  have <- file.exists(files)
  if (!all(have)) {
    cat("Missing records:", fit_table$id[!have], sep = "\n  ")
  }
  recs <- lapply(files[have], function(f) {
    r <- readRDS(f)
    # Records written before the quantile columns were named.
    if (!is.null(r$posterior) && !"q025" %in% names(r$posterior)) {
      names(r$posterior)[names(r$posterior) == "2.5%"] <- "q025"
      names(r$posterior)[names(r$posterior) == "97.5%"] <- "q975"
    }
    r
  })
  names(recs) <- fit_table$id[have]
  recs
}
f3 <- function(v) ifelse(is.na(v), "NA", format(signif(v, 3)))

if (mode == "report") {
  recs <- load_records()
  est_str <- function(e) {
    if (is.null(e)) return("")
    if (!is.null(e$error)) return(paste("error:", e$error))
    v <- e$values
    b <- e$bound
    s <- paste0(ifelse(nzchar(b), paste0(b, " "), ""), f3(v))
    out <- sprintf("%s (%s to %s)", s[1], s[2], s[3])
    if (!is.na(e$n_above) && (e$n_above + e$n_below) > 0) {
      out <- paste0(out, sprintf(" [%d above, %d below of %d]", e$n_above,
                                 e$n_below, e$n_draws))
    }
    out
  }
  label <- function(r) {
    paste0(r$build, ", ", r$prior_type,
           if (r$declared) ", declared" else "")
  }

  cat("\n=== designs ===\n")
  for (dn in names(designs)) {
    hit <- which(vapply(recs, `[[`, "", "design") == dn)
    if (length(hit) == 0) next
    r <- recs[[hit[1]]]
    cat(sprintf(paste0("%-22s %s = %.4g, beta = %.4g (rate %.4g), ",
                       "f_reached %.3f; mean y at 0 = %.3f, at 10 = %.3f",
                       "%s\n"),
                dn, r$par, r$truth[[r$par]], r$truth[["beta"]],
                exp(r$truth[["beta"]]), r$observed$f_reached,
                r$observed$mean_control, r$observed$mean_max,
                if (!is.null(r$truth_ec50_absolute)) {
                  sprintf("; true absolute EC50 %.3f",
                          r$truth_ec50_absolute)
                } else ""))
  }

  for (dn in names(designs)) {
    rs <- recs[vapply(recs, `[[`, "", "design") == dn]
    if (length(rs) == 0) next
    par <- rs[[1]]$par
    cat("\n\n=== ", dn, " ===\n", sep = "")
    cat("\n-- priors (truncated to their bounds): central 95%, CDF at",
        "truth, mass above", x_max, "--\n")
    for (r in rs) {
      if (!is.na(r$failed)) {
        cat(label(r), ": FAILED ", r$failed, "\n")
        next
      }
      for (np in c("bot", par)) {
        p <- r$priors[r$priors$nlpar == np, ]
        pf <- r$prior_facts[[np]]
        cat(sprintf("%-38s %-5s %-28s lb=%-6s ub=%-6s [%s, %s] p_truth=%s%s\n",
                    label(r), np, p$prior, p$lb, p$ub, f3(pf$q025),
                    f3(pf$q975), f3(pf$p_truth),
                    if (np == par) paste0(" p_above=", f3(pf$p_above))
                    else ""))
      }
    }
    cat("\n-- posteriors: median (2.5% to 97.5%), CDF at truth,",
        "sd ratio posterior/prior --\n")
    for (r in rs) {
      if (!is.na(r$failed)) next
      ps <- r$posterior
      for (np in c("top", "bot", "beta", par, "sigma")) {
        q <- ps[ps$par == np, ]
        cat(sprintf("%-38s %-5s %s (%s to %s)  truth %s  cdf %s  ratio %s\n",
                    label(r), np, f3(q$median), f3(q$q025), f3(q$q975),
                    f3(q$truth), f3(q$cdf_at_truth),
                    f3(q$sd / q$prior_sd)))
      }
      cat(sprintf(paste0("%-38s %s draws above %g: %.4f, below 0: %.4f;",
                         " MCSE of median %.3g\n"),
                  label(r), par, x_max, r$threshold_above,
                  r$threshold_below, r$threshold_mcse_median))
    }
    cat("\n-- reported estimates --\n")
    for (r in rs) {
      if (!is.na(r$failed)) next
      cat(sprintf("%-38s %s: %s\n", label(r), toupper(r$ne_type),
                  est_str(r$ne)))
      cat(sprintf("%-38s EC50: %s\n", label(r), est_str(r$ecx50)))
      if (!is.null(r$ne_extrapolated)) {
        cat(sprintf("%-38s %s extrapolated: %s\n", label(r),
                    toupper(r$ne_type), est_str(r$ne_extrapolated)))
      }
      if (!is.null(r$ecx50_extrapolated)) {
        cat(sprintf("%-38s EC50 on c(0, %g): %s\n", label(r), 10 * x_max,
                    est_str(r$ecx50_extrapolated)))
      }
    }
    cat("\n-- conditions raised by the estimators --\n")
    for (r in rs) {
      if (!is.na(r$failed)) next
      for (nm in c("ne", "ecx50", "ne_extrapolated", "ecx50_extrapolated")) {
        e <- r[[nm]]
        if (is.null(e)) next
        m <- unique(c(e$messages, e$warnings))
        m <- gsub("\\s+", " ", trimws(m))
        m <- m[nzchar(m)]
        if (length(m)) {
          cat(label(r), " ", nm, ":\n",
              paste0("   ", substr(m, 1, 400), "\n"), sep = "")
        }
      }
    }
    cat("\n-- diagnostics --\n")
    for (r in rs) {
      if (!is.na(r$failed)) next
      ps <- r$posterior
      cat(sprintf(paste0("%-38s divergent %d of %d; treedepth 10 hit %d;",
                         " max rhat %.4f; min bulk ESS %.0f; min tail ESS",
                         " %.0f; init fallback %s; %.0f s\n"),
                  label(r), as.integer(r$divergent), r$n_draws,
                  as.integer(r$treedepth_hits), max(ps$rhat),
                  min(ps$ess_bulk), min(ps$ess_tail), r$init_fallback,
                  r$elapsed))
    }
    cat("\n-- messages and warnings raised by bnec() --\n")
    for (r in rs) {
      m <- unique(c(r$fit_messages, r$fit_warnings))
      m <- m[!grepl("^(Compiling|Start sampling|Response variable modelled)",
                    m)]
      m <- gsub("\\s+", " ", trimws(m))
      m <- m[nzchar(m)]
      if (length(m)) {
        cat(label(r), ":\n", paste0("   ", substr(m, 1, 300), "\n"), sep = "")
      }
    }
  }
}

# ---- markdown mode -----------------------------------------------------
# The condensed tables in notes/incomplete_design_fits.md, one per design
# and one of diagnostics, printed from the same records.
if (mode == "markdown") {
  recs <- load_records()
  lab <- function(r) {
    paste0(r$build, if (r$prior_type == "regularizing") ", `regularizing`",
           if (r$declared) ", declared")
  }
  # The declared gaussian entry puts its 2.5th percentile at the floor of
  # zero to within 1e-5, which is printed as 0.
  g3 <- function(v) {
    ifelse(abs(v) < 1e-4, "0",
           trimws(format(signif(v, 3), scientific = FALSE,
                         drop0trailing = TRUE)))
  }
  interval <- function(m, lo, hi) sprintf("%s (%s to %s)", g3(m), g3(lo),
                                          g3(hi))
  est <- function(e) {
    if (is.null(e)) return("")
    if (!is.null(e$error)) return("error")
    if (all(is.na(e$values))) return("NA")
    s <- paste0(ifelse(nzchar(e$bound), paste0("`", e$bound, "` "), ""),
                g3(e$values))
    sprintf("%s (%s to %s)", s[1], s[2], s[3])
  }
  for (dn in names(designs)) {
    rs <- recs[vapply(recs, `[[`, "", "design") == dn]
    if (length(rs) == 0) next
    par <- rs[[1]]$par
    ne <- toupper(rs[[1]]$ne_type)
    cat("\n####", dn, "\n\n")
    cat(sprintf(paste0("| fit | `bot` prior, 95%% | `bot` posterior | ",
                       "CDF at true `bot` | `%s` prior mass above 10 | ",
                       "`%s` posterior | share above 10 | %s | EC50 | ",
                       "extrapolated |\n"), par, par, ne))
    cat("|---|---|---|---|---|---|---|---|---|---|\n")
    for (r in rs) {
      pb <- r$prior_facts$bot
      pt <- r$prior_facts[[par]]
      qb <- r$posterior[r$posterior$par == "bot", ]
      qt <- r$posterior[r$posterior$par == par, ]
      ext <- if (is.null(r$ne_extrapolated)) "" else
        if (r$model == "nec4param") est(r$ne_extrapolated) else
          paste0("EC50 ", est(r$ecx50_extrapolated))
      cat(sprintf("| %s | %s to %s | %s | %s | %s | %s | %s | %s | %s | %s |\n",
                  lab(r), g3(pb$q025), g3(pb$q975),
                  interval(qb$median, qb$q025, qb$q975),
                  g3(qb$cdf_at_truth), g3(pt$p_above),
                  interval(qt$median, qt$q025, qt$q975),
                  g3(r$threshold_above), est(r$ne), est(r$ecx50), ext))
    }
  }
  cat("\n#### diagnostics\n\n")
  cat("| design | fit | divergent of 8000 | treedepth 10 | max R-hat |",
      "min bulk ESS | min tail ESS |\n|---|---|---|---|---|---|---|\n")
  for (r in recs) {
    ps <- r$posterior
    cat(sprintf("| %s | %s | %d | %d | %.3f | %.0f | %.0f |\n", r$design,
                lab(r), as.integer(r$divergent),
                as.integer(r$treedepth_hits), max(ps$rhat),
                min(ps$ess_bulk), min(ps$ess_tail)))
  }
  cat("\nFailed fits:", sum(!is.na(vapply(recs, `[[`, "", "failed"))),
      "; initial-value fallbacks:",
      sum(vapply(recs, function(r) isTRUE(r$init_fallback), logical(1))),
      "\n")
}
