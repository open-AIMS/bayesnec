# =====================================================================
# Joint refit of a grouped fit: families other than Beta, and a shared
# dispersion (#448, D40)
#
# D40 asks for the two untested cases of bnec_joint() on a bnec_group()
# fit to be sampled. Only Beta on the identity link had drawn a
# posterior on that route. This script fits, on simulated crossed data
# (three levels, every level over the same concentration series):
#
#   1. gaussian, Gamma, binomial with trials() and negbinomial: a
#      bnec_group() fit of one equation, then bnec_joint() on it with
#      the default disp_by_level = TRUE. Convergence of every fit, and
#      each level's curve parameters and estimates from the joint refit
#      against that level's own fit.
#   2. Gamma again, bnec_joint(disp_by_level = FALSE) on the same grouped
#      fit, against the disp_by_level = TRUE refit and the level fits.
#
# Two diagnostic cases follow, each added after the default run had shown
# what it tests; the comment above each states the reason.
#
#   negbinomial_priors  level a of the negbinomial data fitted alone under
#                       the joint refit's priors
#   Gamma_ad99          the Gamma case and its shared-dispersion refit
#                       again, at adapt_delta = 0.99
#
# Results: notes/joint_refit_families.md. Nothing in R/ is changed.
#
# Running. From the repository root:
#
#   Rscript notes/scripts/joint_refit_families.R
#
# The per-fit summaries go to ignore/joint_refit_families.rds, which is
# git-ignored; the fits themselves are not kept unless JOINT_FITS_DIR
# names a directory to save them in. JOINT_FAMILIES (comma-separated,
# default every case: gaussian, Gamma, binomial, negbinomial, shared,
# negbinomial_priors, Gamma_ad99) reruns a subset and replaces those
# entries in the existing .rds. Fits run one after another, four chains on
# four cores each, with future's plan set to sequential: a Stan fit inside
# a future multisession worker deadlocks on the machine this was written
# on.
#
# The recorded run, 2026-10-20, R 4.6.1, brms 2.23.0, rstan 2.32.7,
# backend rstan, package at ba30b91e. The five default cases were run
# first, by an earlier arrangement of this code that differed in having
# the two runners written inline rather than as functions and in not
# reporting shift_sd, so their records have no shift_sd column; the two
# diagnostics were then run with JOINT_FAMILIES set to them. The
# negbinomial_priors case refits the negbinomial case with the same seeds
# before its own fits, and that repeat reproduced every quantile of the
# first run exactly, so the two arrangements give the same numbers.
# =====================================================================

set.seed(448)

devtools::load_all(quiet = TRUE)
future::plan("sequential")

out_file <- file.path("ignore", "joint_refit_families.rds")
dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
fits_dir <- Sys.getenv("JOINT_FITS_DIR", "")
run_cases <- strsplit(
  Sys.getenv("JOINT_FAMILIES", paste0("gaussian,Gamma,binomial,negbinomial,",
                                      "shared,negbinomial_priors,Gamma_ad99")),
  ","
)[[1]]

brm_settings <- list(chains = 4, iter = 2000, warmup = 1000, cores = 4,
                     refresh = 0)

# ---- simulation -------------------------------------------------------

# One series for every level, which is what makes the design crossed: each
# level's prediction grid and control value are then the same whether they
# are read from that level's rows or from the whole data, so a disagreement
# below cannot come from the per-level range PR #463 implemented.
x_series <- c(0, 0.3, 0.6, 1, 1.5, 2.2, 3.2, 4.5)
n_rep <- 4
levs <- c("a", "b", "c")
design <- expand.grid(rep = seq_len(n_rep), x = x_series, site = levs,
                      stringsAsFactors = FALSE)
design$site <- factor(design$site, levels = levs)

mean_nec4param <- function(x, top, bot, nec, beta) {
  bot + (top - bot) * exp(-exp(beta) * pmax(x - nec, 0))
}
mean_nec3param <- function(x, top, nec, beta) {
  top * exp(-exp(beta) * pmax(x - nec, 0))
}
mean_ecx4param <- function(x, top, bot, ec50, beta) {
  top + (bot - top) / (1 + exp((ec50 - x) * exp(beta)))
}

# The true values. The levels differ in their threshold or midpoint, so that
# a joint refit that let one level's data inform another's curve would show
# as a shift. The mean is written on the natural response scale, because
# bayesnec fits every family on the identity link.
truth <- list(
  gaussian = data.frame(
    level = levs, top = 10, bot = c(2, 1.5, 2.5), nec = c(0.8, 1.3, 2.0),
    beta = log(c(1.2, 1.0, 1.5)), sigma = c(0.5, 0.8, 0.6)
  ),
  # One shape for every level. The Gamma data serve question 2 as well, and
  # a shared dispersion is the correct model only where the levels have one;
  # with three different shapes the comparison would measure the
  # misspecification instead of whether the shared refit is built right.
  Gamma = data.frame(
    level = levs, top = c(20, 18, 22), bot = c(2, 3, 2.5),
    ec50 = c(1.2, 1.8, 2.6), beta = log(c(2.5, 2.0, 3.0)), shape = 15
  ),
  binomial = data.frame(
    level = levs, top = c(0.9, 0.85, 0.92), nec = c(0.9, 1.5, 2.2),
    beta = log(c(1.0, 0.8, 1.4)), trials = 20L
  ),
  negbinomial = data.frame(
    level = levs, top = c(60, 45, 75), nec = c(0.9, 1.5, 2.2),
    beta = log(c(0.8, 1.0, 0.7)), shape = c(8, 15, 25)
  )
)

lev_par <- function(tr, par) tr[[par]][match(design$site, tr$level)]

# Drawn in a fixed order from the seed set above, so every dataset is
# reproduced by the seed alone.
sim <- list()
local({
  tr <- truth$gaussian
  mu <- mean_nec4param(design$x, lev_par(tr, "top"), lev_par(tr, "bot"),
                       lev_par(tr, "nec"), lev_par(tr, "beta"))
  sim$gaussian <<- transform(design,
                             y = stats::rnorm(nrow(design), mu,
                                              lev_par(tr, "sigma")))
  tr <- truth$Gamma
  mu <- mean_ecx4param(design$x, lev_par(tr, "top"), lev_par(tr, "bot"),
                       lev_par(tr, "ec50"), lev_par(tr, "beta"))
  shape <- lev_par(tr, "shape")
  sim$Gamma <<- transform(design,
                          y = stats::rgamma(nrow(design), shape = shape,
                                            rate = shape / mu))
  tr <- truth$binomial
  mu <- mean_nec3param(design$x, lev_par(tr, "top"), lev_par(tr, "nec"),
                       lev_par(tr, "beta"))
  n <- lev_par(tr, "trials")
  sim$binomial <<- transform(design, n = n,
                             y = stats::rbinom(nrow(design), n, mu))
  tr <- truth$negbinomial
  mu <- mean_nec3param(design$x, lev_par(tr, "top"), lev_par(tr, "nec"),
                       lev_par(tr, "beta"))
  sim$negbinomial <<- transform(
    design,
    y = as.integer(stats::rnbinom(nrow(design), size = lev_par(tr, "shape"),
                                  mu = mu))
  )
})

cases <- list(
  gaussian = list(family = "gaussian", model = "nec4param",
                  formula = y ~ crf(x, "nec4param"), seed = 4481),
  Gamma = list(family = "Gamma", model = "ecx4param",
               formula = y ~ crf(x, "ecx4param"), seed = 4482),
  binomial = list(family = "binomial", model = "nec3param",
                  formula = y | trials(n) ~ crf(x, "nec3param"), seed = 4483),
  negbinomial = list(family = "negbinomial", model = "nec3param",
                     formula = y ~ crf(x, "nec3param"), seed = 4484)
)

# ---- extraction -------------------------------------------------------

# Every message and warning is kept, so that a refusal or an advisory raised
# on the joint route is recorded in the call that raised it rather than lost
# to a console.
run_logged <- function(expr) {
  msgs <- character(0)
  warns <- character(0)
  t0 <- Sys.time()
  value <- withCallingHandlers(
    tryCatch(expr, error = function(e) e),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, messages = msgs, warnings = warns,
       seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")),
       error = if (inherits(value, "error")) conditionMessage(value) else NA)
}

# The adapt_delta Stan was run at, read from the sampler's own record of its
# arguments rather than from the call, because neither bnec_group() nor
# bnec_joint() is given a control list here and what is wanted is the value
# the package's defaults produced.
realised_control <- function(brmsfit) {
  a <- attr(brmsfit$fit@sim$samples[[1]], "args")$control
  c(adapt_delta = a$adapt_delta, max_treedepth = a$max_treedepth)
}

convergence <- function(brmsfit, label) {
  s <- suppressWarnings(summary(brmsfit))
  tab <- rbind(s$fixed, s$spec_pars)
  np <- brms::nuts_params(brmsfit)
  ctrl <- realised_control(brmsfit)
  el <- rstan::get_elapsed_time(brmsfit$fit)
  data.frame(
    fit = label, backend = brmsfit$backend,
    n_par = nrow(tab), max_rhat = max(tab$Rhat),
    min_bulk_ess = min(tab$Bulk_ESS), min_tail_ess = min(tab$Tail_ESS),
    divergent = sum(np$Value[np$Parameter == "divergent__"]),
    max_treedepth_hits = sum(np$Value[np$Parameter == "treedepth__"] >=
                               ctrl[["max_treedepth"]]),
    draws = brms::ndraws(brmsfit), adapt_delta = ctrl[["adapt_delta"]],
    sampling_seconds = max(rowSums(el)),
    stringsAsFactors = FALSE
  )
}

# Draws of one variable as an iterations-by-chains matrix, the shape the
# posterior package's Monte Carlo standard errors are defined on. The
# autocorrelation within each chain is what those account for, and a
# standard deviation over the square root of the draw count, which the
# gate tests in test-bnec_joint.R use, does not.
var_draws <- function(brmsfit, variable) {
  a <- brms::as_draws_array(brmsfit, variable = variable)
  matrix(as.numeric(a[, , 1]), nrow = dim(a)[1])
}

# Estimator draws come back as one vector ordered chain by chain, which is
# the order brms stores them in. A draw that never crosses the target is NA;
# it is set above every other draw so that it keeps its rank, which is how
# the package's own censored summary treats it.
vec_to_chains <- function(v, n_chains) {
  n_na <- sum(is.na(v))
  if (n_na > 0) {
    v[is.na(v)] <- max(v, na.rm = TRUE) * 1e3
  }
  m <- matrix(v, ncol = n_chains)
  attr(m, "n_na") <- n_na
  m
}

`%||%` <- function(a, b) if (is.null(a)) b else a

probs <- c(0.5, 0.025, 0.975)

draw_stats <- function(m) {
  q <- stats::quantile(m, probs = probs, names = FALSE)
  se <- posterior::mcse_quantile(m, probs = probs, names = FALSE)
  c(Q50 = q[1], Q2.5 = q[2], Q97.5 = q[3], se50 = se[1], se2.5 = se[2],
    se97.5 = se[3], sd = stats::sd(m), n_na = attr(m, "n_na") %||% 0L)
}

# The difference of two independent estimates of one quantile, in units of
# its Monte Carlo standard error. The two fits are separate runs, so their
# errors add in quadrature.
compare_draws <- function(mj, mg, what, level, ref_label) {
  j <- draw_stats(mj)
  g <- draw_stats(mg)
  data.frame(
    quantity = what, level = level, reference = ref_label,
    joint_Q50 = j[["Q50"]], ref_Q50 = g[["Q50"]],
    joint_Q2.5 = j[["Q2.5"]], ref_Q2.5 = g[["Q2.5"]],
    joint_Q97.5 = j[["Q97.5"]], ref_Q97.5 = g[["Q97.5"]],
    z50 = (j[["Q50"]] - g[["Q50"]]) / sqrt(j[["se50"]]^2 + g[["se50"]]^2),
    z2.5 = (j[["Q2.5"]] - g[["Q2.5"]]) /
      sqrt(j[["se2.5"]]^2 + g[["se2.5"]]^2),
    z97.5 = (j[["Q97.5"]] - g[["Q97.5"]]) /
      sqrt(j[["se97.5"]]^2 + g[["se97.5"]]^2),
    sd_ratio = j[["sd"]] / g[["sd"]],
    # The same difference of medians in units of the reference's posterior
    # standard deviation. Where the two fits are different models, as a
    # shared and a per-level dispersion are, Monte Carlo error is not the
    # scale a difference is read on, and this is.
    shift_sd = (j[["Q50"]] - g[["Q50"]]) / g[["sd"]],
    na_joint = j[["n_na"]], na_ref = g[["n_na"]],
    stringsAsFactors = FALSE
  )
}

# The coefficient a dummy-coded refit gives one level, read off the fit's
# variables rather than pasted together, because brms rewrites a level label
# that is not a syntactic name. The levels here are syntactic, so the two
# agree; the lookup is what the package itself does.
joint_var <- function(joint, par, level) {
  paste0("b_", par, "_", joint$group_var, level)
}

# Draws of a dispersion parameter on its natural scale. With
# disp_by_level = TRUE brms puts the predicted dispersion behind a log link,
# so the per-level coefficient is exponentiated before it is compared with a
# level fit's own, which brms declares on the natural scale.
disp_draws_joint <- function(joint, dpar, level) {
  if (isTRUE(joint$disp_by_level)) {
    exp(var_draws(joint$fit, joint_var(joint, dpar, level)))
  } else {
    var_draws(joint$fit, dpar)
  }
}

estimator_draws <- function(fun, obj, ...) {
  suppressWarnings(suppressMessages(fun(obj, posterior = TRUE, ...)))
}

# Each side of a comparison is either a joint refit or a list of single-level
# fits named by level, the form a bayesnecgroupfit holds them in. One set of
# accessors then makes the comparisons of question 1, the shared-against-
# per-level one of question 2, and the single-level diagnostic fits.
ref_par_draws <- function(ref, par, level) {
  if (inherits(ref, "bayesnecjointfit")) {
    var_draws(ref$fit, joint_var(ref, par, level))
  } else {
    var_draws(pull_brmsfit(ref[[level]]), paste0("b_", par, "_Intercept"))
  }
}

ref_disp_draws <- function(ref, dpar, level) {
  if (inherits(ref, "bayesnecjointfit")) {
    disp_draws_joint(ref, dpar, level)
  } else {
    var_draws(pull_brmsfit(ref[[level]]), dpar)
  }
}

# Estimator draws for each level. A joint refit returns every level from one
# call, so it is called once rather than once per level.
est_draws_by_level <- function(fun, obj, levels, args) {
  if (inherits(obj, "bayesnecjointfit")) {
    all_lev <- do.call(estimator_draws, c(list(fun, obj), args))
    all_lev[levels]
  } else {
    stats::setNames(lapply(levels, function(l) {
      do.call(estimator_draws, c(list(fun, obj[[l]]), args))
    }), levels)
  }
}

side_levels <- function(x) {
  if (inherits(x, "bayesnecjointfit")) x$levels else names(x)
}

compare_fits <- function(x, ref, model, dpar, ref_label, n_chains,
                         levels = side_levels(x)) {
  curve_pars <- names(get(paste0("bf_", model))[[2]])
  out <- list()
  for (lev in levels) {
    for (p in curve_pars) {
      out[[length(out) + 1]] <- compare_draws(
        ref_par_draws(x, p, lev), ref_par_draws(ref, p, lev), p, lev,
        ref_label
      )
    }
    if (!is.null(dpar)) {
      out[[length(out) + 1]] <- compare_draws(
        ref_disp_draws(x, dpar, lev), ref_disp_draws(ref, dpar, lev),
        dpar, lev, ref_label
      )
    }
  }
  # The estimators a user reads. Each goes through the per-level grid and
  # control value of the joint refit, which a parameter comparison does not
  # exercise.
  est <- list(ecx10 = list(fun = ecx, args = list(ecx_val = 10)),
              ecx50 = list(fun = ecx, args = list(ecx_val = 50)))
  if (model %in% mod_groups$nec) {
    est <- c(list(nec = list(fun = nec, args = list())), est)
  }
  for (e in names(est)) {
    xd <- est_draws_by_level(est[[e]]$fun, x, levels, est[[e]]$args)
    rd <- est_draws_by_level(est[[e]]$fun, ref, levels, est[[e]]$args)
    for (lev in levels) {
      out[[length(out) + 1]] <- compare_draws(
        vec_to_chains(as.numeric(xd[[lev]]), n_chains),
        vec_to_chains(as.numeric(rd[[lev]]), n_chains),
        e, lev, ref_label
      )
    }
  }
  do.call(rbind, out)
}

prior_table <- function(brmsfit) {
  pr <- as.data.frame(brms::prior_summary(brmsfit))
  pr[nzchar(pr$prior), c("prior", "class", "coef", "nlpar", "dpar", "lb",
                         "ub", "source")]
}

# The truth on the scale each comparison is made on, for the record.
truth_long <- function(tr) {
  pars <- setdiff(names(tr), c("level", "trials"))
  do.call(rbind, lapply(pars, function(p) {
    data.frame(quantity = p, level = tr$level, value = tr[[p]])
  }))
}

save_fit <- function(obj, name) {
  if (nzchar(fits_dir)) {
    dir.create(fits_dir, showWarnings = FALSE, recursive = TRUE)
    saveRDS(obj, file.path(fits_dir, paste0(name, ".rds")))
  }
}

# ---- running a case ---------------------------------------------------

results <- if (file.exists(out_file)) readRDS(out_file) else list()

load_kept <- function(name) {
  path <- file.path(fits_dir, paste0(name, ".rds"))
  if (nzchar(fits_dir) && file.exists(path)) readRDS(path) else NULL
}

with_control <- function(control) {
  settings <- brm_settings
  if (!is.null(control)) {
    settings$control <- control
  }
  settings
}

# One grouped fit and its joint refit for one family. `control` is passed to
# both, so that the diagnostic case below differs from the default case in
# adapt_delta alone, with the data and every seed unchanged; NULL leaves both
# at the package defaults.
run_family <- function(fam_name, key, control = NULL) {
  cs <- cases[[fam_name]]
  message("== ", key, " (", cs$family, ", ", cs$model, ") ==")
  settings <- with_control(control)
  dat <- sim[[fam_name]]
  rec <- list(family = cs$family, model = cs$model,
              formula = deparse1(cs$formula), seed = cs$seed,
              control = control, data = dat,
              truth = truth_long(truth[[fam_name]]))
  out <- list(rec = rec, grouped = NULL, joint = NULL)
  g <- run_logged(do.call(bnec_group, c(
    list(cs$formula, data = dat, group_var = "site", family = cs$family,
         seed = cs$seed),
    settings
  )))
  out$rec$grouped_call <- g[c("messages", "warnings", "seconds", "error")]
  if (!is.na(g$error)) {
    message("bnec_group failed: ", g$error)
    return(out)
  }
  grouped <- g$value
  out$grouped <- grouped
  save_fit(grouped, paste0(key, "_grouped"))
  out$rec$convergence <- do.call(rbind, lapply(levs, function(l) {
    convergence(pull_brmsfit(grouped$fits[[l]]), paste0("level ", l))
  }))
  out$rec$priors_level <- lapply(stats::setNames(levs, levs), function(l) {
    prior_table(pull_brmsfit(grouped$fits[[l]]))
  })
  j <- run_logged(do.call(bnec_joint, c(
    list(grouped, seed = cs$seed + 10), settings
  )))
  out$rec$joint_call <- j[c("messages", "warnings", "seconds", "error")]
  if (!is.na(j$error)) {
    message("bnec_joint failed: ", j$error)
    return(out)
  }
  joint <- j$value
  out$joint <- joint
  save_fit(joint, paste0(key, "_joint"))
  out$rec$convergence <- rbind(out$rec$convergence,
                               convergence(joint$fit, "joint"))
  out$rec$priors_joint <- prior_table(joint$fit)
  out$rec$joint_variables <- brms::variables(joint$fit)
  out$rec$joint_init <- joint$init
  out$rec$comparison <- compare_fits(
    joint, grouped$fits, cs$model, disp_dpar(validate_family(cs$family)),
    "level fit", brm_settings$chains
  )
  out
}

# The disp_by_level = FALSE refit of a grouped Gamma fit, compared with the
# per-level dispersion refit, which is the comparison D40 names, and with the
# level fits as well, so that a difference can be placed on one side or the
# other of the two refits.
run_shared <- function(grouped, by_level, key, control = NULL) {
  message("== ", key, " (Gamma, ecx4param, disp_by_level = FALSE) ==")
  settings <- with_control(control)
  rec <- list(family = "Gamma", model = "ecx4param", control = control)
  if (is.null(grouped) || is.null(by_level)) {
    rec$error <- paste("The Gamma grouped fit and its disp_by_level = TRUE",
                       "refit are needed; run the Gamma case in the same",
                       "call, or earlier with JOINT_FITS_DIR set.")
    message(rec$error)
    return(rec)
  }
  s <- run_logged(do.call(bnec_joint, c(
    list(grouped, disp_by_level = FALSE, seed = cases$Gamma$seed + 20),
    settings
  )))
  rec$joint_call <- s[c("messages", "warnings", "seconds", "error")]
  if (!is.na(s$error)) {
    message("bnec_joint(disp_by_level = FALSE) failed: ", s$error)
    return(rec)
  }
  shared <- s$value
  save_fit(shared, paste0(key, "_joint"))
  rec$convergence <- convergence(shared$fit, "joint, shared dispersion")
  rec$priors_joint <- prior_table(shared$fit)
  rec$joint_variables <- brms::variables(shared$fit)
  rec$comparison <- rbind(
    compare_fits(shared, by_level, "ecx4param", "shape",
                 "joint, dispersion per level", brm_settings$chains),
    compare_fits(shared, grouped$fits, "ecx4param", "shape", "level fit",
                 brm_settings$chains)
  )
  rec
}

# ---- question 1: each family on the joint route ------------------------

held <- list()
for (fam_name in intersect(names(cases), run_cases)) {
  out <- run_family(fam_name, fam_name)
  results[[fam_name]] <- out$rec
  # The Gamma fits are held for question 2, which refits the same grouped fit.
  if (fam_name == "Gamma") {
    held <- out
  }
  saveRDS(results, out_file)
}

# ---- question 2: one dispersion shared by every level ------------------

if ("shared" %in% run_cases) {
  results$shared <- run_shared(held$grouped %||% load_kept("Gamma_grouped"),
                               held$joint %||% load_kept("Gamma_joint"),
                               "shared")
  saveRDS(results, out_file)
}

# ---- diagnostic: the negative binomial priors --------------------------

# Added after the default run. The negbinomial refit converged, but level a's
# shape median differed from its level fit by 3.6 Monte Carlo standard
# errors and its top median by 3.0. The two fits are given different priors
# in two places: top's gamma prior is derived from the pooled response in
# the joint refit and from the level's own response in the level fit, and
# the dispersion is given normal(2, 2) on the log scale in the joint refit
# (add_level_disp_defaults()) against brms's default inv_gamma(0.4, 0.3) on
# shape in the level fit. A dummy-coded refit with a dispersion per level
# factorises by level -- every likelihood row and every coefficient prior
# involves one level's coefficients alone -- so level a fitted alone under
# the joint refit's priors has the joint refit's level-a posterior. Two such
# fits are made: one with both priors replaced, and one with the dispersion
# prior alone, which divides the difference between the two causes.
# lognormal(2, 2) on shape is normal(2, 2) on log(shape).
#
# The case and its joint refit are fitted again first, because the default
# run did not keep the fits, and its record is kept beside the first, which
# also checks that the same seeds reproduce the same draws.
if ("negbinomial_priors" %in% run_cases) {
  out <- run_family("negbinomial", "negbinomial_repeat")
  rec <- list(repeat_record = out$rec)
  if (!is.null(out$joint)) {
    da <- sim$negbinomial[sim$negbinomial$site == "a", ]
    pj <- out$rec$priors_joint
    pl <- out$rec$priors_level$a
    row_prior <- function(pr, nlpar) pr$prior[pr$nlpar == nlpar][1]
    curve_prior <- function(pr) {
      brms::prior_string(row_prior(pr, "beta"), nlpar = "beta") +
        brms::prior_string(row_prior(pr, "nec"), nlpar = "nec", lb = 0) +
        brms::prior_string(row_prior(pr, "top"), nlpar = "top", lb = 0)
    }
    shape_log_normal <- brms::prior_string("lognormal(2, 2)", class = "shape")
    variants <- list(
      both = curve_prior(pj) + shape_log_normal,
      shape_only = curve_prior(pl) + shape_log_normal
    )
    rec$variants <- lapply(variants, as.data.frame)
    rec$fits <- list()
    for (v in names(variants)) {
      message("== negbinomial level a alone, ", v, " ==")
      f <- run_logged(do.call(bnec, c(
        list(y ~ crf(x, "nec3param"), data = da, family = "negbinomial",
             prior = variants[[v]], seed = cases$negbinomial$seed + 30),
        brm_settings
      )))
      fit_rec <- list(call = f[c("messages", "warnings", "seconds", "error")])
      if (is.na(f$error)) {
        fit_rec$convergence <- convergence(pull_brmsfit(f$value),
                                           paste("level a alone,", v))
        fit_rec$priors <- prior_table(pull_brmsfit(f$value))
        alone <- list(a = f$value)
        fit_rec$comparison <- rbind(
          compare_fits(out$joint, alone, "nec3param", "shape",
                       paste("level a alone,", v), brm_settings$chains,
                       levels = "a"),
          compare_fits(alone, out$grouped$fits, "nec3param", "shape",
                       "level fit", brm_settings$chains, levels = "a")
        )
      }
      rec$fits[[v]] <- fit_rec
    }
  }
  results$negbinomial_priors <- rec
  saveRDS(results, out_file)
}

# ---- diagnostic: the Gamma case at adapt_delta = 0.99 ------------------

# Added after the default run. The Gamma refit at the default adapt_delta of
# 0.8 returned 120 divergent transitions, a maximum R-hat of 1.14 and a
# minimum bulk ESS of 19, with the divergent draws at large values of level
# b's slope. Refitting the same data with the same seeds at 0.99, the value
# bnec() uses for a group-level term on a constrained mean and the one D40
# names, separates a sampler that has not adapted to that region from a
# fault in how the joint model is built. The level fits are refitted at 0.99
# as well, because level b's own fit returned 9 divergent transitions at 0.8
# and is the reference the refit is compared with. The package default is
# not changed; the value is passed through `control`.
if ("Gamma_ad99" %in% run_cases) {
  ad99 <- list(adapt_delta = 0.99)
  out <- run_family("Gamma", "Gamma_ad99", control = ad99)
  results$Gamma_ad99 <- out$rec
  saveRDS(results, out_file)
  if (!is.null(out$joint)) {
    results$shared_ad99 <- run_shared(out$grouped, out$joint, "shared_ad99",
                                      control = ad99)
    saveRDS(results, out_file)
  }
}

results$provenance <- list(
  date = as.character(Sys.Date()),
  commit = system("git rev-parse HEAD", intern = TRUE),
  R = R.version.string,
  brms = as.character(utils::packageVersion("brms")),
  rstan = as.character(utils::packageVersion("rstan")),
  bayesnec = as.character(utils::packageVersion("bayesnec")),
  settings = brm_settings
)
saveRDS(results, out_file)
message("Saved ", out_file)
