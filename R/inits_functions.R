#' Treat an empty-string prior bound as absent
#'
#' \pkg{brms} records an absent bound as \code{""} in the \code{prior} slot a
#' fitted object carries, while \code{define_prior()} and \code{brms::prior()}
#' use \code{NA}. All three mean unbounded, but the bound-respecting redraw in
#' \code{make_inits()} tests with \code{is.na()}, so \code{""} was read as a
#' bound, then coerced to \code{NA} by \code{as.numeric()}, leaving a
#' \code{while (NA)} and the error "missing value where TRUE/FALSE needed".
#' That is what stopped a fit's own priors from being usable as a \code{prior}
#' argument. See #141.
#'
#' @param priors A \code{\link[base]{data.frame}} of priors.
#'
#' @return \code{priors}, with blank bounds set to \code{NA}.
#'
#' @noRd
blank_bounds_to_na <- function(priors) {
  for (bound in c("lb", "ub")) {
    if (bound %in% names(priors)) {
      priors[[bound]][!nzchar(as.character(priors[[bound]]))] <- NA
    }
  }
  priors
}

#' Recognise a brms constant() prior, and read the value it fixes
#'
#' A \code{constant()} prior is a point mass rather than a distribution, so it
#' has no entry in the sampling tables \code{make_inits()},
#' \code{refine_inits()} and \code{sample_priors()} use, and looking it up
#' there raised "attempt to apply non-function". These two helpers are the
#' single place the form is parsed. See #244.
#'
#' @param prior A \code{\link[base]{character}} vector of prior strings.
#'
#' @return \code{is_constant_prior()} a \code{\link[base]{logical}} vector;
#' \code{constant_prior_value()} a \code{\link[base]{numeric}} vector of the
#' fixed values.
#'
#' @noRd
is_constant_prior <- function(prior) {
  grepl("^\\s*constant\\s*\\(", as.character(prior))
}

#' @noRd
constant_prior_value <- function(prior) {
  out <- vapply(as.character(prior), constant_one_value, numeric(1),
                USE.NAMES = FALSE)
  if (any(is.na(out))) {
    stop("A constant() prior must fix a single numeric value; could not read ",
         paste0(prior[is.na(out)], collapse = ", "), ".")
  }
  out
}

#' The fixed value of a single constant() prior string
#'
#' Two things a plain \code{as.numeric()} on the bracket contents gets wrong,
#' both of them legal \code{brms} priors: \code{constant()} takes a second
#' \code{broadcast} argument, so \code{constant(0.5, broadcast = FALSE)} is
#' not a number; and the value is an R expression rather than a literal, so
#' \code{constant(1/2)} is as valid as \code{constant(0.5)}. Both used to
#' reach the "must fix a single numeric value" error.
#'
#' Evaluated in \code{\link[base]{baseenv}}, so the expression sees base R and
#' nothing of the caller's workspace --- a prior is a specification, not a hook
#' for arbitrary code from elsewhere in the session.
#'
#' @param x A \code{\link[base]{character}} string.
#'
#' @return A \code{\link[base]{numeric}} vector of length 1, \code{NA} if the
#' value could not be read.
#'
#' @noRd
constant_one_value <- function(x) {
  inner <- sub("^\\s*constant\\s*\\(\\s*", "", x)
  inner <- sub("\\s*\\)\\s*$", "", inner)
  arg <- strsplit(inner, ",", fixed = TRUE)[[1]][1]
  # The literal case first, so the overwhelmingly common form never goes near
  # parse(). suppressWarnings: a non-literal is handled below, and the coercion
  # warning would say the same thing less clearly.
  out <- suppressWarnings(as.numeric(arg))
  if (!is.na(out)) {
    return(out)
  }
  out <- tryCatch(eval(parse(text = arg), envir = baseenv()),
                  error = function(e) NA_real_)
  if (!is.numeric(out) || length(out) != 1) {
    return(NA_real_)
  }
  as.numeric(out)
}

#' make_inits
#'
#' Creates list of initialisation values
#'
#' @inheritParams bnec
#'
#' @param fct_args A \code{\link[base]{character}} string containing
#' the expected argument names to be used.
#' @param priors an object of class \code{\link[brms]{brmsprior}} from package
#' \pkg{brms}.
#' @param chains Number of chains to be passed to \pkg{brms} model.
#'
#' @importFrom stats rgamma rnorm rbeta runif
#'
#' @seealso \code{\link{bnec}}
#' @return A \code{\link[base]{list}} containing the initialisation values.
#'
#' @noRd
make_inits <- function(model, fct_args, priors, chains) {
  fcts <- c(gamma = rgamma,
            normal = rnorm,
            beta = rbeta,
            uniform = runif)
  priors <- blank_bounds_to_na(as.data.frame(priors))
  priors <- priors[priors$prior != "", ]
  # Only the curve's own coefficients are the business of the initial-value
  # search. Any prior carrying a class other than "b" describes no part of the
  # mean curve -- the family's dispersion parameter (sigma, shape, phi), the
  # mixing probability of a single-block zero-inflated family (zi, hu), a
  # group-level standard deviation (sd) -- and previously made the name check
  # below fail outright, so a user simply could not supply one. Note the filter
  # is general, not a list of dispersion classes: anything that is not "b" is
  # out, which is the correct rule and needs no maintenance as families are
  # added. Dropping them here is the same move add_brm_defaults() already makes
  # for the parameters a disp() variance function introduces, and it has the
  # second effect the fix needs: no initial value is generated for them, which
  # is correct. Stan random-initialises any parameter absent from an init list,
  # and bayesnec has never given sigma an init, so nothing downstream needs
  # teaching. The priors themselves still reach brm(); only the init search
  # ignores them. See #207 and #231.
  priors <- priors[priors$class == "b", ]
  par_names <- character(length = nrow(priors))
  for (j in seq_along(par_names)) {
    sep <- ifelse(priors$class[j] == "b", "_", "")
    par_names[j] <- paste(priors$class[j],
                          priors$nlpar[j],
                          sep = sep)
  }
  check_args <- identical(sort(par_names), sort(fct_args))
  if (!check_args) {
    out_args <- gsub("^b_", "", fct_args)
    out_pars <- gsub("^b_", "", par_names)
    stop("In model ", model, ", user-specific parameter ",
         "prior names (",
         paste0(out_pars, collapse = ", "), ") do not ",
         "match expectation (",
         paste0(out_args, collapse = ", "),
         "). Consider ",
         "reconstructing your priors; check necessary ",
         "parameters with show_params(\"", model, "\")")
  }
  out <- vector(mode = "list", length = chains)
  for (i in seq_along(out)) {
    out[[i]] <- vector(mode = "list", length = nrow(priors))
    names(out[[i]]) <- par_names
    for (j in seq_len(nrow(priors))) {
      # A constant() prior fixes the parameter, so it is assigned rather than
      # sampled, and the bound-respecting redraw below is skipped -- a constant
      # sitting outside its own lb/ub would spin that while loop forever. The
      # value is kept in the list here on purpose: make_good_inits() evaluates
      # the candidate curve, and a parameter fixed at, say, bot = 0 is
      # genuinely part of that curve. It is removed only where the list is
      # handed to brm(), in add_brm_defaults(), because Stan does not declare a
      # parameter whose prior is constant. See #244.
      if (is_constant_prior(priors$prior[j])) {
        out[[i]][[j]] <- constant_prior_value(priors$prior[j])
      } else {
        bits <- gsub("\\(|\\)", ",", priors$prior[j])
        bits <- strsplit(bits, ",", fixed = TRUE)[[1]]
        fct_i <- bits[1]
        v1 <- as.numeric(bits[2])
        v2 <- as.numeric(bits[3])
        out[[i]][[j]] <- fcts[[fct_i]](1, v1, v2)
        if (any(!is.na(priors[j, c("lb", "ub")]))) {
          n_bounds <- sum(!is.na(priors[j, c("lb", "ub")]))
          if (n_bounds == 2) {
            bounds <- as.numeric(priors[j, c("lb", "ub")])
            while (out[[i]][[j]] <= min(bounds) |
                     out[[i]][[j]] >= max(bounds)) {
              out[[i]][[j]] <- fcts[[fct_i]](1, v1, v2)
            }
          } else if (n_bounds == 1) {
            direction <- c("lb", "ub")[!is.na(priors[j, c("lb", "ub")])]
            bound_fct <- ifelse(direction == "lb", `<=`, `>=`)
            bounds <- as.numeric(priors[j, direction])
            while (bound_fct(out[[i]][[j]], bounds)) {
              out[[i]][[j]] <- fcts[[fct_i]](1, v1, v2)
            }
          }
        }
      }
      if (priors$class[j] == "b") {
        dim(out[[i]][[j]]) <- 1
      }
    }
  }
  out
}

#' refine_inits
#'
#' For a single chain's init list that fails prediction checks, attempt to
#' fix it by re-drawing one parameter at a time while holding the others
#' fixed.  Targets \code{slope}, \code{d} and \code{beta} -- the parameters
#' most likely to push hormesis / sigmoidal predictions out of range.
#'
#' @param init A named \code{\link[base]{list}} of init values for one chain.
#' @param x Sorted predictor values.
#' @param pred_fct The prediction function for the model.
#' @param fct_args Parameter names expected by the prediction function.
#' @param limits A length-2 \code{\link[base]{numeric}} vector (response range).
#' @param priors A \code{\link[base]{data.frame}} of priors (already filtered
#'   to non-empty rows).
#' @param n_sub Maximum number of single-parameter re-draws per parameter.
#'
#' @return The (possibly improved) init list.
#' @noRd
refine_inits <- function(init, x, pred_fct, fct_args, limits,
                         priors, n_sub = 500) {
  fcts <- c(gamma = rgamma,
            normal = rnorm,
            beta = rbeta,
            uniform = runif)
  preds <- get_init_predictions(init, x, pred_fct, fct_args)
  if (check_init_predictions(preds, limits)) {
    return(init)
  }
  # refine_inits nudges a finite-but-out-of-range curve back into range by
  # re-drawing slope/d/beta. NaN predictions signal a structural problem
  # instead -- e.g. models that raise the predictor to a fractional power
  # (nechormepwr, nechorme4pwr, ecxsigm) return NaN wherever x < 0, for every
  # parameter draw. No re-draw fixes that, so bail early rather than burn the
  # full n_sub search on a hopeless case. We test NaN specifically rather than
  # all non-finite values: Inf from numerical overflow can sometimes be cured
  # by re-drawing a parameter smaller, so those are left for the search below.
  if (any(is.nan(preds))) {
    return(init)
  }
  # Identify tunable parameters (slope, d, beta -- those most likely
  # to push predictions out of range in hormesis / sigmoidal models).
  tunable <- intersect(names(init), c("b_slope", "b_d", "b_beta"))
  for (par in tunable) {
    pr_row <- which(priors$nlpar == gsub("^b_", "", par))
    if (length(pr_row) != 1) next
    # Nothing to tune on a parameter the user has fixed, and the sampling
    # table below has no constant() entry. See #244.
    if (is_constant_prior(priors$prior[pr_row])) next
    bits <- gsub("\\(|\\)", ",", priors$prior[pr_row])
    bits <- strsplit(bits, ",", fixed = TRUE)[[1]]
    fct_i <- bits[1]
    v1 <- as.numeric(bits[2])
    v2 <- as.numeric(bits[3])
    for (k in seq_len(n_sub)) {
      candidate <- init
      new_val <- fcts[[fct_i]](1, v1, v2)
      if (priors$class[pr_row] == "b") {
        dim(new_val) <- 1
      }
      candidate[[par]] <- new_val
      preds <- get_init_predictions(candidate, x, pred_fct, fct_args)
      if (check_init_predictions(preds, limits)) {
        return(candidate)
      }
    }
  }
  init
}

#' make_good_inits
#'
#' Creates list of initialisation values that generate
#' data within the natural range of data
#'
#' @inheritParams bnec
#'
#' @param x A \code{\link[base]{numeric}} vector containing the x predictor.
#' @param y A \code{\link[base]{numeric}} vector containing the y response.
#' @param n_trials A \code{\link[base]{numeric}} vector indicating
#' how many attempts the function should run before giving up.
#' @param seed seed number for reproducible random number generation. Defaults
#' to \code{NULL}.
#' @param ... Additional arguments to \code{\link{make_inits}}.
#'
#' @seealso \code{\link{make_inits}}
#' @return A \code{\link[base]{list}} containing the initialisation values.
#'
#' @noRd
make_good_inits <- function(model, x, y, n_trials = 1e4, seed = NULL, ...) {
  limits <- range(y, na.rm = TRUE)
  pred_fct <- get(paste0("pred_", model))
  fct_args <- names(unlist(as.list(args(pred_fct))))
  fct_args <- setdiff(fct_args, "x")
  dots <- list(...)
  priors_df <- blank_bounds_to_na(as.data.frame(dots$priors))
  priors_df <- priors_df[priors_df$prior != "", ]
  set.seed(seed)
  inits <- make_inits(model, fct_args, ...)
  init_ranges <- lapply(inits, get_init_predictions, sort(x), pred_fct, fct_args)
  are_good <- all(sapply(init_ranges, check_init_predictions, limits))
  n_t <- 1
  while (!are_good && n_t <= n_trials) {
    inits <- make_inits(model, fct_args, ...)
    init_ranges <- lapply(inits, get_init_predictions, sort(x), pred_fct, fct_args)
    are_good <- all(sapply(init_ranges, check_init_predictions, limits))
    # If the full draw failed, try to fix each chain by re-drawing
    # one problematic parameter at a time (slope, d, beta).
    if (!are_good) {
      inits <- lapply(inits, refine_inits, sort(x), pred_fct, fct_args,
                      limits, priors_df)
      init_ranges <- lapply(inits, get_init_predictions, sort(x),
                            pred_fct, fct_args)
      are_good <- all(sapply(init_ranges, check_init_predictions, limits))
    }
    n_t <- n_t + 1
  }
  if (!are_good) {
    message("bayesnec failed to find initial values within the",
            " range of the response. Using Stan's default",
            " initialisation process.")
    list(random = "random")
  } else {
    inits
  }
}

#' make_good_hurdle_inits
#'
#' Initial values for both parameter blocks of a joint hurdle fit.
#'
#' @inheritParams make_good_inits
#'
#' @param predictor A \code{\link[base]{numeric}} vector containing the full
#' predictor, including the rows where the response is zero.
#' @param response A \code{\link[base]{numeric}} vector; zero denotes a
#' non-survivor.
#' @param priors An object of class \code{\link[brms]{brmsprior}} covering both
#' blocks, i.e. containing both \code{top} and \code{hutop} and so on.
#' @param model_survival The equation used for the second block, which need not
#' be the one used for the response block. Defaults to \code{NULL}, i.e. the
#' same as \code{model}.
#'
#' @details Each block is primed from the view of the data it actually models,
#' then the two are merged chain-wise. The mu block sees survivors only; the hu
#' block sees the proportion surviving at each unique predictor value, because
#' the sub-model is written as \code{1 - survival} and so the curve being
#' initialised is survival.
#'
#' Both passes reuse the same \code{pred_<model>()} prediction function, which
#' expects unprefixed parameter names -- the \code{hu} prefix is stripped before
#' the search and restored afterwards.
#'
#' @seealso \code{\link{make_good_inits}}
#' @return A \code{\link[base]{list}} of initial values, or
#' \code{list(random = "random")} if either block could not be initialised.
#'
#' @noRd
make_good_hurdle_inits <- function(model, predictor, response, priors, chains,
                                   dpar = "hu", seed = NULL,
                                   model_survival = NULL, ...) {
  if (is.null(model_survival)) {
    model_survival <- model
  }
  parts <- split_hurdle_response(predictor, response)
  pr <- as.data.frame(priors)
  is_hu <- nzchar(pr$nlpar) & grepl(paste0("^", dpar), pr$nlpar)
  mu_pr <- pr[!is_hu, , drop = FALSE]
  hu_pr <- pr[is_hu, , drop = FALSE]
  hu_pr$nlpar <- sub(paste0("^", dpar), "", hu_pr$nlpar)
  mu_inits <- make_good_inits(model, parts$mu$x, parts$mu$y, priors = mu_pr,
                              chains = chains, seed = seed, ...)
  hu_inits <- make_good_inits(model_survival, parts$hu$x, parts$hu$y,
                              priors = hu_pr, chains = chains, seed = seed,
                              ...)
  # If either block fell back to Stan's random initialisation there is nothing
  # coherent to merge -- hand the whole fit to Stan rather than half-priming it.
  fell_back <- function(x) length(x) == 1 && "random" %in% names(x)
  if (fell_back(mu_inits) || fell_back(hu_inits)) {
    return(list(random = "random"))
  }
  lapply(seq_len(chains), function(i) {
    hu_i <- hu_inits[[i]]
    names(hu_i) <- sub("^b_", paste0("b_", dpar), names(hu_i))
    c(mu_inits[[i]], hu_i)
  })
}

#' Initial values for the parameters a group-level term introduces
#'
#' @param brms_bf The \code{\link[brms]{brmsformula}} the fit will use.
#' @param data The \code{\link[base]{data.frame}} the fit will use.
#' @param family A \code{\link[stats]{family}} object.
#' @param priors The \code{\link[brms]{brmsprior}} the fit will use, read for
#' the scale of the generated group-level standard deviations.
#' @param ogl Whether the formula carries an \code{ogl} offset parameter.
#'
#' @details A prior is not enough here, and this is the part of #245 that is
#' easy to get wrong. Stan draws its default initial values as
#' \code{uniform(-2, 2)} on the \emph{unconstrained} scale, which it does
#' \strong{regardless of the prior declared}. A group-level standard deviation
#' is lower-bounded at zero, so the realised initial value is
#' \code{exp(uniform(-2, 2))}, between 0.135 and 7.39; the offset is
#' \code{sd * z} under the non-centred parameterisation \pkg{brms} uses, with
#' \code{z} initialised in the same range. On a response bounded in (0, 1)
#' under the identity link \code{\link{bnec}} forces, that puts the mean
#' outside its support before sampling begins, and no prior can prevent it.
#'
#' The \code{ogl} intercept is the same problem in a simpler form: it is a
#' population-level parameter with no bounds, so Stan starts it anywhere in
#' (-2, 2), which is already outside a unit-interval response.
#'
#' All group-level effects are therefore started at \strong{exactly zero}
#' deviation -- \code{z = 0}, and the \code{ogl} offset at 0 -- which is a
#' valid point for any family and any link, and is the model the fit reduces to
#' if the grouping turns out to explain nothing.
#'
#' Because \code{z} is zero, the value given to \code{sd} itself does not
#' affect whether the starting point is valid; it sets only where the sampler
#' begins exploring. The median of the generated \code{sd} prior scales is
#' used, so the starting scale tracks the data rather than being a constant that
#' is tiny for one response and large for another.
#'
#' The indices are read from \code{\link[brms]{make_standata}} rather than
#' reconstructed from the formula. \pkg{brms} numbers group-level terms by its
#' own internal ordering -- a single \code{pgl} term over four parameters
#' becomes four separately indexed terms, not one -- and guessing that ordering
#' would be a silent source of mismatched initial values.
#'
#' The query is made twice where it has to be, because no single family can
#' answer it for every fit. See the comment at the call.
#'
#' @return A named \code{\link[base]{list}} of initial values.
#'
#' @importFrom brms make_standata
#' @importFrom stats median gaussian
#'
#' @noRd
group_inits <- function(brms_bf, data, family, priors, ogl = FALSE) {
  # The group-level dimensions M_k and N_k come from the random-effects
  # structure alone, so the family is irrelevant to the answer -- but it is not
  # irrelevant to whether the call succeeds, and each of the two candidates
  # fails on a case the other handles.
  #
  # The fit's own family fails where the response has not yet been through
  # check_data(): a Beta response still carrying exact zeros and ones, which is
  # what reaches here when the formula transforms a variable, or on the amend()
  # path where check_data() never runs at all. gaussian() fails where the
  # formula carries a trials() aterm, because trials is not a valid aterm for
  # gaussian -- that is every binomial and beta_binomial fit, which is the
  # standard workflow for those families and one of the three bounded families
  # this function exists for.
  #
  # So: the fit's own family first, gaussian() as the fallback. Ordered that
  # way round because the real family is the one that describes the model, and
  # a query answered by it needs no justification; gaussian() is the escape
  # hatch for a response the family will not accept yet, and can only be wrong
  # about something this function does not ask.
  ask <- function(fam) {
    try(suppressMessages(make_standata(brms_bf, data = data, family = fam)),
        silent = TRUE)
  }
  sdata <- ask(family)
  if (inherits(sdata, "try-error")) {
    sdata <- ask(gaussian())
  }
  if (inherits(sdata, "try-error")) {
    # Genuinely unexpected now. Warn rather than return quietly: an empty init
    # list here is the difference between a fit that starts and one that does
    # not, and a silent one is very hard to trace back to this line.
    warning("Could not determine the group-level dimensions, so no initial ",
            "values were generated for them. The fit may fail to initialise. ",
            "See #245.", call. = FALSE)
    return(list())
  }
  m_names <- grep("^M_[0-9]+$", names(sdata), value = TRUE)
  if (length(m_names) == 0) {
    return(list())
  }
  sd_scales <- sd_prior_scales(priors)
  start_sd <- if (length(sd_scales) > 0) median(sd_scales) else 0.1
  out <- list()
  for (k in sort(as.integer(sub("^M_", "", m_names)))) {
    n_terms <- sdata[[paste0("M_", k)]]
    n_levels <- sdata[[paste0("N_", k)]]
    out[[paste0("sd_", k)]] <- as.array(rep(start_sd, n_terms))
    out[[paste0("z_", k)]] <- matrix(0, nrow = n_terms, ncol = n_levels)
  }
  if (isTRUE(ogl)) {
    out$b_ogl <- as.array(0)
  }
  out
}

#' The scales of the group-level standard deviation priors in a prior set
#'
#' @param priors An object of class \code{\link[brms]{brmsprior}}.
#'
#' @return A \code{\link[base]{numeric}} vector, possibly empty.
#'
#' @noRd
sd_prior_scales <- function(priors) {
  if (is.null(priors) || nrow(priors) == 0 || !"class" %in% names(priors)) {
    return(numeric(0))
  }
  strs <- priors$prior[priors$class == "sd"]
  strs <- strs[nzchar(strs)]
  if (length(strs) == 0) {
    return(numeric(0))
  }
  # Only read a scale from the distributions whose last argument is one. The
  # last argument of gamma() and inv_gamma() is a rate, exponential() has a rate
  # and no comma at all, and constant() carries a value rather than a scale --
  # taking the last number from any of those returns a number that is not a
  # scale, silently. A user-supplied gamma(2, 100) previously gave a starting
  # value of 100 on a unit-interval response.
  scale_families <- "^\\s*(student_t|normal|cauchy|lognormal|logistic)\\s*\\("
  strs <- strs[grepl(scale_families, strs)]
  if (length(strs) == 0) {
    return(numeric(0))
  }
  vals <- suppressWarnings(
    as.numeric(sub("^.*,\\s*([0-9.eE+-]+)\\)\\s*$", "\\1", strs))
  )
  vals[is.finite(vals) & vals > 0]
}
