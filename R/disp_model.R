# Internals supporting the disp() term, which lets a family's dispersion
# parameter vary rather than being held constant across the whole curve.
#
# Two routes, told apart by what disp() is given:
#
#   disp(~x)        route (A) -- an ordinary brms distributional formula on the
#                   predictor. Descriptive: "noise is larger at high dose".
#   disp("power")   route (B) -- a variance function of the mean curve. A
#                   statement about the measurement process rather than about
#                   the dose axis. Under ogl() the mean it reads is the curve
#                   before the group's deviation; see disp_curve().
#
# The two are near-observationally equivalent for a monotone curve, since mu is
# then a monotone function of x. They separate under hormesis, or where a
# design revisits the same mu.
#
# Route (B) needs the curve expression written out a second time inside the
# dispersion formula, because mu is not in scope for another distributional
# parameter's formula in brms. Only the source is duplicated, not the fitted
# quantity: the curve parameters are shared, being declared once for the whole
# formula. bayesnec owns every curve expression in sysdata, so what is awkward
# by hand is mechanical here -- the same argument that make_hu_block() rests on.

#' The family whose dispersion parameter a disp() term models
#'
#' @param family Either a \code{\link[stats]{family}} object or a family tag.
#'
#' @details For a joint two-block family this is the family of the positive
#' block: \code{Gamma} for \code{hurdle_gamma}, \code{beta} for
#' \code{zero_inflated_beta}, and the count family for the count hurdles. The
#' dispersion parameter of a two-block family belongs to that block alone,
#' since the \code{hu} or \code{zi} block is a probability and has none, so the
#' positive block's family decides which parameter is modelled, which variance
#' functions apply, and where its priors and initial values are centred. The
#' mapping is read from \code{hurdle_mu_fams}, which already records it for
#' the priors, rather than by adding the two-block tags to \code{disp_dpars},
#' so that the family of each positive block is stated in one place. See #410.
#'
#' The tag is read through \code{canonical_fam_tag()}, because a family taken
#' from a fitted object records Gamma as \code{gamma}, which
#' \code{disp_dpars} and every table read through this function key as
#' \code{Gamma}. \code{\link{amend}} builds the priors and the formula of an
#' added equation from that stored family (#436).
#'
#' @return A \code{\link[base]{character}} string, the family tag.
#'
#' @noRd
disp_family_tag <- function(family) {
  fam_tag <- canonical_fam_tag(family)
  if (is_hurdle_family(fam_tag)) {
    return(unname(hurdle_mu_fams[[fam_tag]]))
  }
  fam_tag
}

#' Does this family have a free dispersion parameter?
#'
#' @param family Either a \code{\link[stats]{family}} object or a family tag.
#'
#' @return A \code{\link[base]{logical}}. For a joint two-block family, whether
#' its positive block has one; see \code{disp_family_tag()}.
#'
#' @noRd
has_disp_par <- function(family) {
  isTRUE(disp_family_tag(family) %in% names(disp_dpars))
}

#' Name brms gives the dispersion parameter for this family
#'
#' @param family Either a \code{\link[stats]{family}} object or a family tag.
#'
#' @return A \code{\link[base]{character}} string, or \code{NULL} where the
#' family has no free dispersion parameter. For a joint two-block family it is
#' the positive block's parameter, which \pkg{brms} names as it does for the
#' single-block family: \code{shape} for \code{hurdle_gamma} and
#' \code{hurdle_negbinomial}, \code{phi} for \code{zero_inflated_beta}.
#'
#' @noRd
disp_dpar <- function(family) {
  if (!has_disp_par(family)) {
    return(NULL)
  }
  unname(disp_dpars[[disp_family_tag(family)]])
}

#' Extract the disp() specification from a bayesnec formula
#'
#' @param formula An object of class \code{\link{bayesnecformula}}.
#'
#' @return \code{NULL} where no disp() term is present, otherwise a
#' \code{\link[base]{list}} with elements \code{route} ("A" or "B") and
#' \code{value} (the sub-model formula for route A, the variance function name
#' for route B).
#'
#' @importFrom formula.tools rhs
#'
#' @noRd
parse_disp_term <- function(formula) {
  rhs_calls <- gsub("\\) \\+ ", ") impossiblestr ", deparse1(rhs(formula)))
  split_rhs_calls <- strsplit(rhs_calls, " impossiblestr ")[[1]]
  disp_str <- grep("disp(", split_rhs_calls, fixed = TRUE, value = TRUE)
  if (length(disp_str) == 0) {
    return(NULL)
  }
  if (length(disp_str) > 1) {
    stop("Your formula has more than one disp() term. Only one dispersion",
         " sub-model can be fitted. See ?bayesnecformula", call. = FALSE)
  }
  # The right-hand side is split on ") + ", which is how every other bayesnec
  # term is found. A disp() argument containing that sequence -- disp(~s(x) +
  # group) -- is therefore torn in half before it gets here. Report that rather
  # than let str2lang() fail on the fragment.
  arg <- try(
    str2lang(sub("^\\s*disp\\(", "", sub("\\)\\s*$", "", disp_str))),
    silent = TRUE
  )
  if (inherits(arg, "try-error")) {
    stop("Could not parse the disp() term \"", disp_str, "\". A sub-model",
         " combining a function call with another term, as in",
         " disp(~s(x) + group), cannot currently be written inline. Compute",
         " the term in your data first and pass it as a plain variable.",
         call. = FALSE)
  }
  if (inherits(arg, "formula") || identical(as.character(arg)[1], "~")) {
    # A one-sided formula, e.g. disp(~x): route (A). Kept as text and rebuilt
    # rather than evaluated, so that terms such as s(x) survive untouched for
    # brms to interpret.
    list(route = "A", value = deparse1(as.formula(arg)[[2]]))
  } else {
    # Resolved in the formula's own environment, so that a variance function
    # name held in a variable is looked up where the user wrote it rather than
    # in this frame, whose lexical parent is the package namespace. The same
    # defect as the crf() model set. See #319.
    list(route = "B", value = eval(arg, envir = formula_env(formula)))
  }
}

#' Validate a disp() specification against the family and the response
#'
#' @param spec The output of \code{\link{parse_disp_term}}.
#' @param family An object of class \code{\link[stats]{family}}.
#' @param response A \code{\link[base]{numeric}} vector, or \code{NULL} to skip
#' the data-dependent checks.
#'
#' @return \code{NULL}, invisibly. Called for its side effect of erroring.
#'
#' @noRd
check_disp_spec <- function(spec, family, response = NULL) {
  # The canonical tag, so that a family taken from a fitted object, which
  # records Gamma as gamma, is checked as the Gamma family and named as the
  # user wrote it. Read as stored, it was refused as having no dispersion
  # parameter (#436).
  fam_tag <- canonical_fam_tag(family)
  # The joint two-block families take the term on the dispersion parameter of
  # their positive block, which is the only block that has one; the hu or zi
  # block is a probability and is left as it is. hurdle_gamma,
  # zero_inflated_beta and hurdle_negbinomial pass on to the checks below,
  # which read the positive block's family through disp_family_tag(). RF, D29
  # and D39 on #410. The shape of hurdle_negbinomial is also read at the
  # asymptote of a relative ecx() of its positive counts, and
  # count_positive_asymptote() evaluates it there; see
  # disp_positive_asymptote().
  #
  # Given its own message because the general one below names the remedy for
  # a single-block family, poisson to negbinomial, and the corresponding
  # two-block family is named here instead.
  if (identical(fam_tag, "hurdle_poisson")) {
    stop("Family hurdle_poisson has no free dispersion parameter, so there is",
         " nothing for disp() to model: the variance of its positive counts is",
         " a deterministic function of their mean. Over-dispersion in the",
         " positive counts is modelled by family = \"hurdle_negbinomial\",",
         " whose shape parameter disp() can model.", call. = FALSE)
  }
  if (fam_tag %in% c("zero_inflated_poisson", "zero_inflated_negbinomial")) {
    # Refused before the has_disp_par() check below, whose message ("no free
    # dispersion parameter") would be wrong for zero_inflated_negbinomial, which
    # does have a shape. The reason is different: the parameter belongs to the
    # count component of a mixture, and what a variance function on it means for
    # the observed response -- which is a mixture, not that component -- has not
    # been worked out. See #104.
    stop("A disp() term is not currently supported for the zero-inflated count",
         " family ", fam_tag, ". Its dispersion parameter describes the count",
         " component alone, while the response is a mixture of that component",
         " and a point mass at zero, so a variance function on it does not",
         " describe the dispersion of the response. See ?dispersion for the",
         " diagnostic, and ?bnec for the zero-inflated families.",
         call. = FALSE)
  }
  if (!has_disp_par(fam_tag)) {
    stop("Family ", fam_tag, " has no free dispersion parameter, so there is",
         " nothing for disp() to model: its variance is a deterministic",
         " function of its mean. Over-dispersion here is remedied by changing",
         " family -- poisson to negbinomial, binomial to beta_binomial -- or",
         " by an observation-level group-level effect. See ?dispersion for the",
         " diagnostic that applies to these families.", call. = FALSE)
  }
  if (spec$route == "B") {
    # A variance function of the fitted mean is built by substituting the
    # model's own curve expression into the form. That expression is the linear
    # predictor on the LINK scale, so it is the mean only under an identity
    # link. bnec() assigns identity unless the caller wrote a link argument, or
    # supplied a family it could not read intent from (#256), so this fires on
    # those two routes. The failure it refuses is silent: under an inverse link
    # the block computes
    # log(1/mu) = -log(mu) and the slope is estimated with the wrong sign,
    # converging cleanly and excluding zero. Refuse rather than fit that.
    link <- if (inherits(family, "family")) family$link else "identity"
    if (!identical(link, "identity")) {
      stop("A variance function of the fitted mean needs the mean modelled on",
           " its natural scale, but family ", fam_tag, " was supplied with a \"",
           link, "\" link. Pass ", fam_tag, "(link = \"identity\"), or name",
           " the family without a link, as in family = \"", fam_tag, "\".",
           " To model dispersion on the predictor instead, pass a formula,",
           " e.g. disp(~x), which is valid under any link. See",
           " ?bayesnecformula", call. = FALSE)
    }
    if (!is.character(spec$value) || length(spec$value) != 1) {
      stop("The variance function passed to disp() must be a single character",
           " string naming one of: ",
           paste0("\"", names(disp_functions), "\"", collapse = ", "),
           ". To model dispersion on the predictor instead, pass a formula,",
           " e.g. disp(~x). See ?bayesnecformula", call. = FALSE)
    }
    if (!spec$value %in% names(disp_functions)) {
      stop("\"", spec$value, "\" is not a known variance function. bayesnec",
           " currently implements: ",
           paste0("\"", names(disp_functions), "\"", collapse = ", "), ".",
           call. = FALSE)
    }
    vf <- disp_functions[[spec$value]]
    # Read against the positive block's family for a two-block family, so that
    # hurdle_gamma takes the forms Gamma takes and zero_inflated_beta those of
    # beta, "twosided" included.
    if (!disp_family_tag(fam_tag) %in% vf$families) {
      # The list names the two-block families that take the form as well,
      # found through the family of their positive block.
      two_block <- names(hurdle_mu_fams)[hurdle_mu_fams %in% vf$families]
      stop("Variance function \"", spec$value, "\" is not valid for the ",
           fam_tag, " family. It applies to: ",
           paste0(c(vf$families, two_block), collapse = ", "), ".",
           call. = FALSE)
    }
    # Every implemented form takes log(mu), so a fitted mean reaching zero is
    # undefined rather than merely awkward. Only the gaussian family can put mu
    # at or below zero: Gamma and negbinomial are positive by construction, and
    # the beta families live on (0, 1). Testing the response is a proxy for
    # testing the fitted curve -- a strictly positive response can still be
    # fitted with a negative lower asymptote -- so this catches the common case
    # and Stan reports the rest at initialisation.
    if (isTRUE(vf$positive_mu) && identical(fam_tag, "gaussian") &&
          !is.null(response) && any(response <= 0, na.rm = TRUE)) {
      stop("Variance function \"", spec$value, "\" takes log(mu), but your",
           " response reaches ", signif(min(response, na.rm = TRUE), 3),
           " so the fitted mean crosses zero. This is the growth-rate case:",
           " a specific growth rate, yield or increment can be negative, and a",
           " power law in mu is undefined there. Use disp(\"loglinear\"),",
           " which is linear in mu rather than in log(mu) and is what a",
           " log-transformed endpoint inherits from a power law on its",
           " original scale, or disp(~x) to model dispersion on the predictor",
           " instead.", call. = FALSE)
    }
  }
  invisible(NULL)
}

#' The response a disp() term is checked and centred against
#'
#' @param data A model frame with the \code{bnec_pop} attribute, as built
#' by \code{model.frame()} on a \code{\link{bayesnecformula}}.
#' @param family Either a \code{\link[stats]{family}} object or a family tag.
#' @param rate_var The \code{rate()} denominator, or \code{NULL}. Defaults to
#' the one read from \code{data}; \code{wrangle_model_formula()} passes the
#' read it has already made for its own family check.
#'
#' @details The centring constant stands in for a typical fitted mean, so it
#' is computed on the scale of that mean rather than of the recorded response.
#' Under the identity link bnec() assigns, brms writes a rate() denominator
#' multiplicatively, so the mean of a negbinomial fit is a rate, and the mean
#' of a beta_binomial fit is a proportion of its trials. Computed from the
#' counts, the constant was displaced by the median log exposure under
#' "power", and the LOG1MREF term of "twosided" fell back to 0 because
#' 1 - count is never positive. The divisions are the ones fit_bayesnec()
#' makes before building priors, on the same family condition for trials, and
#' they are made on the formula-building path rather than in fit_bayesnec() so
#' that bnec() and make_brmsformula() build the same literal. See #397.
#'
#' A two-block family models the dispersion of its positive block, whose mean
#' the variance function is written in, so only the positive responses are
#' returned. This is also the response bnec_hurdle() gives its growth
#' component, so the joint and factorised routes build the same literal and
#' fit the same variance function. Kept, the zeros lowered "loglinear"'s
#' median and entered "twosided"'s second term as log(1 - 0); "power" already
#' dropped them before taking a log. See #410.
#'
#' One function serves both \code{wrangle_model_formula()} and the check
#' \code{\link{bnec}} makes before its model loop, so that the refusal raised
#' there is the one the loop would have raised.
#'
#' @return A \code{\link[base]{numeric}} vector.
#'
#' @noRd
disp_response <- function(data, family,
                          rate_var = retrieve_var(data, "rate_var")) {
  y <- retrieve_var(data, "y_var")
  fam_tag <- if (inherits(family, "family")) family$family else family
  if (fam_tag %in% c("binomial", "beta_binomial")) {
    trials <- retrieve_var(data, "trials_var")
    if (!is.null(trials)) {
      y <- y / trials
    }
  }
  if (!is.null(rate_var)) {
    y <- y / rate_var
  }
  if (is_hurdle_family(fam_tag)) {
    y <- y[which(y > 0)]
  }
  y
}

#' Non-linear parameter names introduced by a disp() specification
#'
#' @param spec The output of \code{\link{parse_disp_term}}.
#'
#' @return A \code{\link[base]{character}} vector, empty for route (A), which
#' introduces ordinary population-level terms rather than non-linear ones.
#'
#' @noRd
disp_pars <- function(spec) {
  if (is.null(spec) || spec$route != "B") {
    return(character(0))
  }
  disp_functions[[spec$value]]$pars
}

#' Centring constant for a variance function
#'
#' @param spec The output of \code{\link{parse_disp_term}}.
#' @param response A \code{\link[base]{numeric}} vector.
#'
#' @return A named \code{\link[base]{numeric}} vector of the constants the
#' form's expression refers to, or an empty one where it needs none.
#'
#' @details The reference is a summary of the RESPONSE standing in for a typical
#' fitted mean. It only has to be near the data for the centring to do its job,
#' so a robust summary is preferred to an exact one: the median for the linear
#' form, and the geometric median for the log forms, which is the median on the
#' scale the covariate is actually measured on. Both are computed once from the
#' data and baked into the formula as literals.
#'
#' @importFrom stats median
#'
#' @noRd
disp_centre <- function(spec, response) {
  if (is.null(spec) || spec$route != "B") {
    return(numeric(0))
  }
  vf <- disp_functions[[spec$value]]
  ctr <- vf$centre
  if (is.null(ctr)) {
    return(numeric(0))
  }
  y <- response[is.finite(response)]
  if (identical(ctr, "identity")) {
    return(c(REF = signif(median(y), 6)))
  }
  pos <- y[y > 0]
  # An all-non-positive response cannot reach a log form at all, and
  # check_disp_spec() has already refused that case for gaussian. Falling back
  # to 0 (i.e. no centring) keeps this total for the families that cannot get
  # here rather than erroring twice for the same reason.
  logref <- if (length(pos)) signif(median(log(pos)), 6) else 0
  out <- c(LOGREF = logref)
  if (grepl("@LOG1MREF@", vf$expr, fixed = TRUE)) {
    om <- 1 - y
    om <- om[om > 0]
    out <- c(out, LOG1MREF = if (length(om)) signif(median(log(om)), 6) else 0)
  }
  out
}

#' Initial values for the parameters a variance function introduces
#'
#' @param spec The output of \code{\link{parse_disp_term}}.
#' @param family An object of class \code{\link[stats]{family}}.
#' @param response A \code{\link[base]{numeric}} vector.
#' @param predictor A \code{\link[base]{numeric}} vector aligned with
#' \code{response}, or \code{NULL}. Read only for the centre the
#' \code{"regularizing"} set takes from the replicates.
#' @param prior_type One of \code{"uninformative"} or \code{"regularizing"}.
#' @param priors The \code{\link[brms]{brmsprior}} the fit will use, or
#' \code{NULL}. Read only for the bounds it declares on each slope.
#' @param rate_response Whether the response was divided by a \code{rate()}
#' denominator before reaching this function.
#' @param truncated_response Whether the count likelihood is conditioned on
#' a positive response by the internal hurdle-growth truncation.
#'
#' @return A named \code{\link[base]{list}} of one-dimensional arrays, suitable
#' for appending to a chain's init list, or an empty list for route (A).
#'
#' @details Every slope starts at the mean of its own default prior, truncated
#' to the bounds the prior in force declares. For an unbounded slope that is the
#' centre of the prior, which is zero, the constant-dispersion model, except
#' where the \code{"regularizing"} set has centred it on the replicate slope
#' instead. This is not cosmetic tidiness: the sign of a slope is tied to the
#' direction of the mean curve, so a chain that Stan starts in the mirror-image
#' basin converges to a solution with the curve and the slope both inverted.
#' Leaving these to Stan's default draw produced an R-hat of 1.85 on the
#' loglinear regression test, with one chain reporting a slope of the wrong
#' sign. Starting every chain at one value lets the data choose the direction
#' once, rather than once per chain.
#'
#' A bounded slope cannot start at zero where zero is a bound, as it is for
#' \code{c1} of \code{"power"} on a Gamma, gaussian or negative binomial
#' response: Stan maps a bounded parameter to the unconstrained scale through
#' a logit, which is infinite at the bound, and refuses the initial value. The
#' mean of the truncated prior is strictly inside any interval of positive
#' width, and is the middle of the prior in the sense the centre is for an
#' unbounded one. On \code{normal(0, 2)} truncated to the interval from 0 to
#' 2 it is 0.92, and on the interval from 0 to 1 it is 0.49. See #472.
#'
#' The bounds are read from \code{priors} rather than from the default, so that
#' a user who supplied their own prior on a slope, with bounds of their own,
#' gets an initial value inside those bounds. Without a row for the slope the
#' default bounds apply.
#'
#' @importFrom stats sd
#'
#' @noRd
disp_inits <- function(spec, family, response, predictor = NULL,
                       prior_type = "uninformative", priors = NULL,
                       rate_response = FALSE, truncated_response = FALSE) {
  pars <- disp_pars(spec)
  if (length(pars) == 0) {
    return(list())
  }
  # the same centres define_disp_prior() uses, so the chains start at the
  # middle of their own priors rather than somewhere else
  c0 <- disp_intercept_centre(family, response)
  # The slope priors of a two-block family are built from its positive block,
  # which is what define_prior() hands define_disp_prior().
  # response_link_scale() is left out because it is the identity for every
  # family that has a bound or a replicate centre on its slope (Gamma, gaussian
  # and negbinomial under the identity link check_disp_spec() requires); for
  # the beta families it would rescale a response that reaches a bound, which
  # only the c0 prior reads.
  slope_y <- response
  slope_x <- predictor
  if (is_hurdle_family(family)) {
    keep <- which(response > 0)
    slope_y <- response[keep]
    slope_x <- if (is.null(predictor)) NULL else predictor[keep]
  }
  slopes <- disp_slope_priors(spec, family, slope_y, slope_x,
                              prior_type = prior_type,
                              rate_response = rate_response,
                              truncated_response = truncated_response)
  out <- lapply(pars, function(p) {
    if (identical(p, "c0")) {
      return(as.array(c0))
    }
    s <- slopes[[p]]
    bounds <- prior_in_force_bounds(priors, p, c(s$lb, s$ub))
    as.array(truncated_normal_mean(s$centre, s$scale, bounds[1], bounds[2]))
  })
  names(out) <- paste0("b_", pars)
  out
}

#' The bounds a prior declares on one non-linear parameter
#'
#' @param priors A \code{\link[brms]{brmsprior}}, or \code{NULL}.
#' @param nlpar The name of the non-linear parameter.
#' @param default A \code{\link[base]{numeric}} vector of length 2, the lower
#' and upper bound to return where \code{priors} has no row for
#' \code{nlpar}.
#'
#' @details A row whose bound is \code{NA} or \code{""} is unbounded on that
#' side, which is how \code{define_prior()} and a fitted object record an
#' absent bound respectively; see \code{blank_bounds_to_na()}.
#'
#' @return A \code{\link[base]{numeric}} vector of length 2, \code{NA} where
#' unbounded.
#'
#' @noRd
prior_in_force_bounds <- function(priors, nlpar, default) {
  if (is.null(priors) || nrow(priors) == 0) {
    return(default)
  }
  pr <- as.data.frame(priors)
  row <- pr[pr$class == "b" & pr$nlpar == nlpar & !nzchar(pr$coef), ,
            drop = FALSE]
  if (nrow(row) == 0) {
    return(default)
  }
  read <- function(v) suppressWarnings(as.numeric(as.character(v[[1]])))
  c(read(row$lb), read(row$ub))
}

#' The mean of a normal distribution truncated to an interval
#'
#' @param mean,sd The location and scale of the normal distribution.
#' @param lb,ub The bounds, \code{NA} or infinite where absent.
#'
#' @details Returns \code{mean} where neither bound is finite, so that an
#' unbounded slope keeps the initial value it had before #472. Where the
#' interval is far in a tail of the normal, both terms of the closed-form ratio
#' underflow to zero and the ratio is not finite, and the midpoint of the
#' interval is returned, or a point one \code{sd} inside a single finite bound.
#'
#' @return A \code{\link[base]{numeric}} of length 1, strictly inside the
#' interval.
#'
#' @importFrom stats pnorm dnorm
#'
#' @noRd
truncated_normal_mean <- function(mean, sd, lb, ub) {
  lb <- if (is.na(lb)) -Inf else lb
  ub <- if (is.na(ub)) Inf else ub
  if (!is.finite(lb) && !is.finite(ub)) {
    return(mean)
  }
  a <- (lb - mean) / sd
  b <- (ub - mean) / sd
  out <- mean + sd * (dnorm(a) - dnorm(b)) / (pnorm(b) - pnorm(a))
  if (is.finite(out) && out > lb && out < ub) {
    return(out)
  }
  if (is.finite(lb) && is.finite(ub)) {
    (lb + ub) / 2
  } else if (is.finite(lb)) {
    lb + sd
  } else {
    ub - sd
  }
}

#' The range of the exponent of a variance function
#'
#' @param spec The output of \code{\link{parse_disp_term}}.
#' @param family An object of class \code{\link[stats]{family}} or a family
#' tag.
#'
#' @details The bounds on \code{c1} of the scale-free forms, by family. Each is
#' derived from a variance proportional to \code{mean^b} with \code{b} from
#' 0 to 2, which runs from a constant variance to a constant coefficient of
#' variation and is the span of Taylor's power law. The dispersion parameter is
#' proportional to \code{mu^c1}, and each family maps that onto the variance
#' through its own mean-variance relationship:
#'
#' \itemize{
#'   \item \code{Gamma}: the variance is \code{mu^2 / shape}, which is
#'     proportional to \code{mu^(2 - c1)}, so \code{c1} runs from 0 to 2.
#'     Below 0 the coefficient of variation would fall as the response
#'     falls; above 2 the variance would rise as the response falls.
#'   \item \code{gaussian}: the variance is \code{sigma^2}, proportional to
#'     \code{mu^(2 c1)}, so \code{c1} runs from 0 to 1.
#'   \item \code{negbinomial}: the overdispersion term is \code{mu^2 / shape},
#'     proportional to \code{mu^(2 - c1)}. \code{c1} runs from 0 to 1, from that
#'     term proportional to \code{mu^2} to proportional to \code{mu}. Above 1
#'     it would grow more slowly than the Poisson part of the variance.
#'   \item \code{beta} and \code{beta_binomial}: unbounded. The variance already
#'     falls at both ends through \code{mu (1 - mu)}, and the precision can
#'     plausibly rise in either direction, which \code{"twosided"} exists to
#'     model.
#' }
#'
#' \code{"loglinear"} is unbounded, because its slope is in units of
#' 1 / response and no fixed range means the same thing for every response. A
#' two-block family takes the range of its positive block's family through
#' \code{disp_family_tag()}. The ranges are RF's decision on #472, recorded
#' there with the \code{example8} fits that reached \code{c1} near -8.
#'
#' The bounds are hard bounds on the non-linear parameter, written as
#' \code{lb} and \code{ub} on its prior row, the way \code{define_prior()}
#' bounds \code{top} and \code{bot} to the support of the response. A posterior
#' that piles up at a bound shows that the data disagree with the range, which
#' a soft prior would hide.
#'
#' @return A named \code{\link[base]{numeric}} vector, \code{lb} and
#' \code{ub}, with \code{NA} where unbounded.
#'
#' @noRd
disp_slope_bounds <- function(spec, family) {
  unbounded <- c(lb = NA_real_, ub = NA_real_)
  if (is.null(spec) || spec$route != "B" ||
        !isTRUE(disp_functions[[spec$value]]$scale_free)) {
    return(unbounded)
  }
  switch(disp_family_tag(family),
         Gamma = c(lb = 0, ub = 2),
         gaussian = c(lb = 0, ub = 1),
         negbinomial = c(lb = 0, ub = 1),
         unbounded)
}

#' The exponent of a variance function read from the replicates
#'
#' @param family An object of class \code{\link[stats]{family}} or a family
#' tag.
#' @param predictor A \code{\link[base]{numeric}} vector, the predictor.
#' @param response A \code{\link[base]{numeric}} vector aligned with
#' \code{predictor}, on the scale of \code{mu}.
#'
#' @details The observations at one predictor value share one mean, so their
#' spread estimates the variance at that mean without a fitted curve. Each
#' family's dispersion parameter can be written as a statistic of a group's
#' mean and variance, and the slope of its log on the log of the group mean is
#' then an estimate of \code{c1} under \code{"power"}:
#'
#' \itemize{
#'   \item \code{Gamma}: \code{log(mean^2 / var)}, the log of
#'     \code{1 / CV^2}, which is the log of the shape;
#'   \item \code{gaussian}: \code{log(var) / 2}, the log of the standard
#'     deviation, which is the log of \code{sigma};
#'   \item \code{negbinomial}: \code{log(mean^2 / (var - mean))}, the shape
#'     implied by the overdispersion term.
#' }
#'
#' A group enters where it holds at least two observations, its mean is
#' positive and its statistic is finite. A negative binomial group with a
#' variance at or below its mean shows no overdispersion and has no statistic.
#' Each group is weighted by its degrees of freedom, \code{n - 1}, because the
#' sampling variance of the log of a sample variance is close to
#' \code{2 / (n - 1)}; on a balanced design that is ordinary least squares.
#'
#' Three groups are needed. Two determine a slope exactly, but it is then the
#' difference of two log variances, each with a sampling standard deviation of
#' about 0.8 at four replicates, and there is no residual to show whether a
#' power law describes them at all.
#'
#' The groups are the distinct predictor values, pooled over any group-level
#' term, so on a design with one the spread includes the variation between
#' groups as well as within them. A censored response enters at its recorded
#' value. A non-integer negative binomial response has no count-scale variance
#' statistic. The caller also excludes a response divided by a \code{rate()}
#' denominator, including when the resulting rates happen to be integers, and
#' positive-only counts from a zero-truncated negative binomial likelihood.
#'
#' @return A \code{\link[base]{numeric}} of length 1, \code{NA} where the
#' design has too few replicated groups or the family has no statistic.
#'
#' @importFrom stats var
#'
#' @noRd
disp_replicate_slope <- function(family, predictor, response) {
  fam_tag <- disp_family_tag(family)
  if (!fam_tag %in% c("Gamma", "gaussian", "negbinomial") ||
        is.null(predictor) || length(predictor) != length(response)) {
    return(NA_real_)
  }
  ok <- is.finite(predictor) & is.finite(response)
  y <- response[ok]
  if (identical(fam_tag, "negbinomial") &&
        any(abs(y - round(y)) > sqrt(.Machine$double.eps))) {
    return(NA_real_)
  }
  # Split as replicated_group_means() splits, by the distinct values of the
  # predictor, so that a group here is a group there.
  groups <- split(y, factor(predictor[ok]))
  groups <- groups[lengths(groups) > 1]
  if (length(groups) < 3) {
    return(NA_real_)
  }
  m <- vapply(groups, mean, numeric(1), USE.NAMES = FALSE)
  v <- vapply(groups, var, numeric(1), USE.NAMES = FALSE)
  w <- lengths(groups) - 1
  stat <- suppressWarnings(switch(fam_tag,
    Gamma = log(m^2 / v),
    gaussian = log(v) / 2,
    negbinomial = log(m^2 / (v - m))
  ))
  keep <- m > 0 & is.finite(stat)
  if (sum(keep) < 3) {
    return(NA_real_)
  }
  lx <- log(m[keep])
  ly <- stat[keep]
  w <- w[keep]
  dx <- lx - sum(w * lx) / sum(w)
  if (sum(w * dx^2) <= 0) {
    return(NA_real_)
  }
  out <- sum(w * dx * ly) / sum(w * dx^2)
  if (is.finite(out)) out else NA_real_
}

#' The priors on the slopes of a variance function
#'
#' @param spec The output of \code{\link{parse_disp_term}}, route (B).
#' @param family An object of class \code{\link[stats]{family}} or a family
#' tag; for a two-block family, the positive block's family or the two-block
#' tag.
#' @param response A \code{\link[base]{numeric}} vector, on the scale of
#' \code{mu}; for a two-block family, the positive responses.
#' @param predictor A \code{\link[base]{numeric}} vector aligned with
#' \code{response}, or \code{NULL}.
#' @param prior_type One of \code{"uninformative"} or \code{"regularizing"}.
#' @param rate_response Whether the response was divided by a \code{rate()}
#' denominator. The negative binomial statistic needs count-scale variance.
#' @param truncated_response Whether the count likelihood is zero-truncated.
#' The ordinary negative binomial variance formula does not apply to the
#' positive-only sample.
#'
#' @details One statement of each slope's prior, read by
#' \code{define_disp_prior()} for the prior rows and by \code{disp_inits()} for
#' the initial values, so that a chain starts inside the bounds its own prior
#' declares.
#'
#' Every slope is \code{normal(centre, scale)}. The scale is 2 for a slope on
#' \code{log(mu)} and \code{2 / sd(response)} for a slope on \code{mu}, for the
#' reason \code{define_disp_prior()} gives. \code{c1} of a scale-free form takes
#' the bounds \code{disp_slope_bounds()} gives its family, and \code{c2} of
#' \code{"twosided"} takes none.
#'
#' The centre is zero, the family's own mean-variance relationship, except
#' under \code{prior_type = "regularizing"} on a bounded \code{c1}. There it is
#' the slope \code{disp_replicate_slope()} reads from the replicates, clamped
#' into the bounds and rounded to four significant figures, and zero where that
#' slope cannot be read. The same set already places \code{top} and \code{bot}
#' at observed responses. The default \code{"uninformative"} set does not use
#' the replicates, so they enter its fit once, through the likelihood.
#'
#' @return A named \code{\link[base]{list}}, one element per slope parameter,
#' each a list with elements \code{prior} (a \pkg{brms} prior string),
#' \code{centre}, \code{scale}, \code{lb} and \code{ub}.
#'
#' @importFrom stats sd
#'
#' @noRd
disp_slope_priors <- function(spec, family, response, predictor = NULL,
                              prior_type = "uninformative",
                              rate_response = FALSE,
                              truncated_response = FALSE) {
  vf <- disp_functions[[spec$value]]
  slopes <- setdiff(vf$pars, "c0")
  # A slope on log(mu) is dimensionless, so a fixed scale means the same thing
  # whatever the response is measured in. A slope on mu itself is not: it
  # has units of 1/response, and normal(0, 2) would be near-flat for a
  # response spanning thousands and highly informative for one spanning a
  # fraction. Scaling by the observed spread restores the intended meaning --
  # that a one-standard-deviation change in the mean changes the log of the
  # dispersion parameter by about two units at the edge of the prior.
  scale <- if (isTRUE(vf$scale_free)) 2 else signif(2 / sd(response), 4)
  bounds <- disp_slope_bounds(spec, family)
  out <- lapply(slopes, function(p) {
    lb <- ub <- NA_real_
    centre <- 0
    if (identical(p, "c1")) {
      lb <- bounds[["lb"]]
      ub <- bounds[["ub"]]
      # The centre is estimated from the same observations used in the
      # likelihood, so this is an empirical prior even at the default width.
      # Keeping scale 2 limits its influence; narrowing it by the factor used
      # for top and bot would give the noisy centre more influence. The log
      # of a variance from n replicates has a sampling standard deviation near
      # sqrt(2 / (n - 1)), 0.8 at four, so on a design with few concentrations
      # the slope through those values is uncertain by a large fraction of the
      # range. The prior is therefore relocated and not narrowed. Inside the
      # bounds a scale of 2 still tilts it: on [0, 2] centred at 0.8, the
      # density at the far bound is 0.83 of that at the centre.
      # A rate() response needs the exposure to separate the Poisson variance
      # from overdispersion. Integer rates alone do not supply that information.
      # Truncation changes both the mean and variance of positive-only counts,
      # so the ordinary negative binomial variance identity also fails there.
      if (identical(prior_type, "regularizing") && is.finite(lb) &&
            is.finite(ub) &&
            !((rate_response || truncated_response) &&
                disp_family_tag(family) == "negbinomial")) {
        slope <- disp_replicate_slope(family, predictor, response)
        if (is.finite(slope)) {
          centre <- signif(min(max(slope, lb), ub), 4)
        }
      }
    }
    list(prior = paste0("normal(", centre, ", ", scale, ")"),
         centre = centre, scale = scale, lb = lb, ub = ub)
  })
  names(out) <- slopes
  out
}

#' Priors for a dispersion parameter modelled on the log scale
#'
#' @param response A \code{\link[base]{numeric}} vector, on the link scale.
#'
#' @details One statement of where a family's dispersion parameter sits once
#' brms has put it behind a log link, keyed by family tag. It is read by
#' \code{define_disp_prior()} for the \code{c0} intercept of a variance
#' function and by \code{\link{add_level_defaults}} for the per-level
#' coefficients of a joint refit. Both are the same quantity --- the log of the
#' dispersion parameter at a typical response --- so they are derived once
#' here rather than twice.
#'
#' @return A named \code{\link[base]{character}} vector of prior strings.
#'
#' @importFrom stats sd
#'
#' @noRd
disp_intercept_priors <- function(response) {
  c(
    gaussian = paste0("normal(", round(log(sd(response)), 3), ", 2)"),
    # shape is an inverse dispersion for both of these: a CV of 0.1 to 0.5 puts
    # a Gamma shape between about 4 and 100, i.e. 1.4 to 4.6 on the log scale.
    Gamma = "normal(2, 2)",
    negbinomial = "normal(2, 2)",
    # phi likewise, on the wider scale the PAM fits needed.
    beta = "normal(4, 3)",
    beta_binomial = "normal(4, 3)"
  )
}

#' The log-scale centre of a family's dispersion parameter
#'
#' @param family An object of class \code{\link[stats]{family}} or a family
#' tag.
#' @param response A \code{\link[base]{numeric}} vector.
#'
#' @details The location of the corresponding entry in
#' \code{\link{disp_intercept_priors}}, as a number, so that a chain starts at
#' the middle of its own prior. \code{\link{disp_inits}} carried this table
#' inline; it is shared because \code{\link{add_level_defaults}} needs the
#' same value.
#'
#' @return A \code{\link[base]{numeric}} of length one, finite.
#'
#' @importFrom stats sd
#'
#' @noRd
disp_intercept_centre <- function(family, response) {
  # The positive block's family for a two-block family, whose dispersion
  # parameter is that block's. Read from the two-block tag, hurdle_gamma would
  # fall through to 0, a shape of 1, against a c0 prior centred on 2. Read
  # here rather than by each caller, so that disp_inits() and
  # add_level_disp_defaults() cannot disagree about it. See #410.
  fam_tag <- disp_family_tag(family)
  c0 <- switch(fam_tag,
    gaussian = log(sd(response[is.finite(response)])),
    Gamma = 2, negbinomial = 2, beta = 4, beta_binomial = 4, 0)
  if (!is.finite(c0)) {
    c0 <- 0
  }
  c0
}

#' Build the dispersion sub-model for a bayesnec equation
#'
#' @param model A \code{\link[base]{character}} string naming a bayesnec model.
#' @param spec The output of \code{\link{parse_disp_term}}.
#' @param dpar The dispersion parameter's name, e.g. "sigma".
#' @param x_var The predictor column name, substituted for the generic "x".
#' @param response A \code{\link[base]{numeric}} vector, used only to compute the
#' centring constant.
#' @param curve The curve expression to substitute for \code{@MU@}, already on
#' the fit's own predictor name. Defaults to \code{NULL}, which rebuilds it
#' from the \code{bf_<model>} template.
#'
#' @details \strong{The curve is taken from the formula being built, not from
#' the template}, whenever the caller supplies it. \code{\link{add_formula_glef}}
#' rewrites the main expression where a group-level deviation is applied
#' multiplicatively --- \code{bot} becomes \code{bnecbot} (#294), and the whole
#' curve becomes \code{bnecmu} under \code{ogl()} (#257) --- and rebuilding from
#' the template discards that, so route (B) modelled the dispersion as a
#' function of a mean the fit was not using. \code{\link{wrangle_model_formula}}
#' already adds the dispersion block last for exactly this reason; the rebuild
#' was what stopped that having any effect.
#'
#' The template remains the fallback for the direct callers in
#' \code{tests/testthat/test-disp_model.R}, which have no assembled formula to
#' read a curve from and are testing the variance function rather than the
#' grouping.
#'
#' @return A \code{\link[base]{list}} with elements \code{nlf} (the block's
#' formula) and, for route (B), \code{lf} (the parameter formula).
#'
#' @importFrom stats as.formula
#'
#' @noRd
make_disp_block <- function(model, spec, dpar, x_var, response = NULL,
                            curve = NULL) {
  if (spec$route == "A") {
    # The environment is not set here. brms resolves a distributional sub-model
    # against the top-level brmsformula, not against this one, so
    # wrangle_model_formula() sets it there instead; measured on
    # disp(~cent(x)) with cent() defined by the caller. See #319.
    return(list(nlf = as.formula(paste0(dpar, " ~ ", spec$value)), lf = NULL))
  }
  vf <- disp_functions[[spec$value]]
  if (is.null(curve)) {
    bf_obj <- get(paste0("bf_", model))
    curve <- substitute_x_in_formula(x_var, deparse1(bf_obj$formula[[3]]))
  }
  # Wrapped in parentheses because the curve is substituted into log(@MU@) and
  # into log(1 - (@MU@)); an unwrapped sum would rebind against the surrounding
  # operators for the second of those.
  rhs <- gsub("@MU@", paste0("(", curve, ")"), vf$expr, fixed = TRUE)
  ctr <- disp_centre(spec, if (is.null(response)) 1 else response)
  for (nm in names(ctr)) {
    rhs <- gsub(paste0("@", nm, "@"), format(ctr[[nm]], scientific = FALSE),
                rhs, fixed = TRUE)
  }
  list(nlf = as.formula(paste0(dpar, " ~ ", rhs)),
       lf = as.formula(paste0(paste0(vf$pars, collapse = " + "), " ~ 1")))
}

#' Append the dispersion sub-model to a brms formula
#'
#' @param brms_bf An object of class \code{\link[brms]{brmsformula}}.
#' @param model A \code{\link[base]{character}} string naming a bayesnec model.
#' @param spec The output of \code{\link{parse_disp_term}}.
#' @param family An object of class \code{\link[stats]{family}}.
#' @param x_var The predictor column name.
#' @param response A \code{\link[base]{numeric}} vector, used only to compute the
#' centring constant.
#'
#' @return An object of class \code{\link[brms]{brmsformula}}.
#'
#' @importFrom brms nlf lf
#'
#' @noRd
add_disp_block <- function(brms_bf, model, spec, family, x_var,
                           response = NULL) {
  dpar <- disp_dpar(family)
  # Route A does not use the curve. For route B read the assembled curve before
  # any ogl() deviation. Rebuilding from the template would discard
  # parameter-level group terms; see make_disp_block().
  curve <- if (spec$route == "B") disp_curve(brms_bf) else NULL
  db <- make_disp_block(model, spec, dpar, x_var, response,
                        curve = curve)
  if (spec$route == "A") {
    # lf() rather than nlf(): the right-hand side is an ordinary linear
    # predictor in the data, so brms should apply its usual design-matrix
    # handling and let terms such as s(x) work.
    return(brms_bf + lf(db$nlf))
  }
  brms_bf + nlf(db$nlf) + lf(db$lf)
}

#' A variance function's form with the curve and its constants left open
#'
#' @param vf An element of \code{disp_functions}.
#'
#' @details \code{@MU@} becomes \code{(.bnec_mu)}, wrapped as
#' \code{make_disp_block()} wraps the curve, and each constant \code{@NAME@}
#' becomes \code{.bnec_NAME}, so that the form has the structure of the
#' fitted sub-model with a symbol wherever the fit has a curve or a literal.
#'
#' @return A \code{\link[base]{call}}.
#'
#' @noRd
disp_form_template <- function(vf) {
  txt <- gsub("@MU@", "(.bnec_mu)", vf$expr, fixed = TRUE)
  str2lang(gsub("@([A-Z0-9]+)@", ".bnec_\\1", txt))
}

#' The dispersion parameter a variance function gives at a stated mean
#'
#' @param fit A \code{\link[brms]{brmsfit}} whose formula has a route (B)
#' \code{disp()} term.
#' @param spec The output of \code{\link{parse_disp_term}}, route (B).
#' @param mu A \code{\link[base]{numeric}} vector of means on the response
#' scale, one per posterior draw, or of length one.
#'
#' @details The sub-model is evaluated draw by draw from the posterior draws of
#' its parameters, in the form \code{disp_functions} records, and passed
#' through the inverse of the family's link on the dispersion parameter.
#' \code{count_positive_asymptote()} needs the negative binomial shape at
#' \code{mu = bot}, a mean the curve approaches only beyond the data, so the
#' shape cannot be read off a row of a prediction grid as
#' \code{posterior_epred(dpar = "shape")} reads it. RF, D39 on #410.
#'
#' The centring constants are read from the fitted formula rather than
#' recomputed from the data: the dispersion formula is matched against
#' \code{disp_form_template()}, and each constant is the literal found where
#' the form has one. Recomputing them would need the response exactly as
#' \code{wrangle_model_formula()} saw it, after any division by trials or a
#' \code{rate()} denominator and with the zeros of a two-block family removed.
#'
#' The inverse link is taken from the \pkg{brms} negative binomial family
#' built with the dispersion link as its mean link, so that the definition is
#' the one \pkg{brms} fitted with. Every link \pkg{brms} accepts on the
#' negative binomial shape is one it accepts on that family's mean.
#'
#' @return A \code{\link[base]{list}} with elements \code{value}, the
#' dispersion parameter, one per draw; \code{pars}, the draws of each
#' parameter of the form; \code{centre}, the constants; and \code{link}.
#'
#' @importFrom brms as_draws_df brmsfamily
#'
#' @noRd
disp_at_mean <- function(fit, spec, mu) {
  vf <- disp_functions[[spec$value]]
  dpar <- disp_dpar(fit$family)
  template <- disp_form_template(vf)
  fitted_rhs <- fit$formula$pforms[[dpar]][[3]]
  found <- list()
  match_form <- function(t, e) {
    if (is.name(t) && startsWith(as.character(t), ".bnec_")) {
      found[[as.character(t)]] <<- e
      return(TRUE)
    }
    if (!is.call(t)) {
      return(identical(t, e))
    }
    is.call(e) && length(e) == length(t) &&
      all(vapply(seq_along(t), function(i) match_form(t[[i]], e[[i]]),
                 logical(1)))
  }
  if (is.null(fitted_rhs) || !match_form(template, fitted_rhs)) {
    stop("The ", dpar, " formula of this fit does not have the form of",
         " disp(\"", spec$value, "\"), so the sub-model cannot be evaluated",
         " at a stated mean.", call. = FALSE)
  }
  consts <- setdiff(names(found), ".bnec_mu")
  centre <- vapply(found[consts], function(e) eval(e, baseenv()), numeric(1))
  names(centre) <- sub("^\\.bnec_", "", consts)
  vars <- paste0("b_", vf$pars, "_Intercept")
  drws <- as_draws_df(fit, variable = vars)
  pars <- lapply(vars, function(v) as.numeric(drws[[v]]))
  names(pars) <- vf$pars
  env <- c(pars, list(.bnec_mu = mu), as.list(stats::setNames(centre, consts)))
  eta <- eval(template, env, baseenv())
  link <- fit$family[[paste0("link_", dpar)]]
  list(value = brmsfamily("negbinomial", link = link)$linkinv(eta),
       pars = pars, centre = centre, link = link)
}

#' The curve used by a disp() variance function
#'
#' @param brms_bf An object of class \code{\link[brms]{brmsformula}}, with every
#' group-level block already added.
#'
#' @details The curve before an \code{ogl()} deviation is applied. Under the
#' multiplicative form that is the intermediate \code{bnecmu}; under the
#' additive form, \code{ogl + <curve>}, it is the right-hand side without its
#' leading \code{ogl}. A parameter-level deviation, \code{(bot | g)} or
#' \code{pgl(g)}, is part of the curve and is kept, as #294 made it.
#'
#' The fitted mean after \code{ogl()} was read here until #468. For a Gamma
#' response it is \code{bnecmu * exp(ogl)}; for a Beta response the deviation
#' acts on the odds. Reading the fitted mean let
#' one deviation set both the mean and the spread of its group. Where the
#' variance rises more slowly than the mean --- \code{c1 > 1} for
#' \code{"power"} --- a deviation that lowered a group's mean far below its
#' data also shrank the Gamma shape until those data were tolerably likely
#' again, which is a second optimum. On the \code{lum31} zinc arm in
#' \code{example8} the deviations of several cells, each started at zero,
#' reached it during warmup, and every equation of a fit with a cell term
#' failed the sampler screen.
#'
#' With the curve before the deviation read instead, the dispersion parameter
#' no longer depends on the \code{ogl()} deviation. The family's observation
#' variance may still depend on the fitted mean: for a Gamma response it is
#' the squared fitted mean divided by the shape.
#'
#' Every route that combines \code{ogl()} with a variance function reaches this
#' function: a single-block fit, the joint two-block families, the growth
#' component of \code{bnec_hurdle()}, which is a single-block fit, and a joint
#' refit of a grouped fit in which every level is fitted one equation, whose
#' level terms are applied after the dispersion block and leave \code{bnecmu}
#' and the leading \code{ogl} as they are. A joint refit whose levels chose
#' different equations refuses a group-level term.
#'
#' @return A \code{\link[base]{character}} string.
#'
#' @noRd
disp_curve <- function(brms_bf) {
  if ("bnecmu" %in% names(brms_bf$pforms)) {
    return("bnecmu")
  }
  rhs_txt <- deparse1(brms_bf$formula[[3]])
  if (!is.null(brms_bf$pforms$ogl)) {
    # add_formula_glef() prefixes the curve with "ogl + ". A curve with several
    # sums is left-associated after parsing, so inspect the deparsed prefix.
    # Refuse a changed structure instead of silently including ogl in disp().
    if (!startsWith(rhs_txt, "ogl + ")) {
      stop("The additive ogl() curve must start with ogl +.", call. = FALSE)
    }
    rhs_txt <- sub("^ogl \\+ ", "", rhs_txt)
  }
  rhs_txt
}

#' The columns a disp(~...) sub-model names other than the predictor
#'
#' @param formula An object of class \code{\link{bayesnecformula}}.
#' @param x_var The predictor's column names. Defaults to the variables of the
#' \code{crf()} term.
#'
#' @details A \code{disp(~z)} term may name a column other than the predictor
#' (#443). \pkg{brms} evaluates every distributional parameter when it
#' predicts, so such a column must be on every grid a prediction is made over,
#' the expected value included, although the mean does not involve it. This is
#' the one definition of that set, read by the prediction grid, by the refusal
#' of a \code{newdata} that lacks one, by the marginalisation of predicted
#' readings, and by the refusal on a zero-truncated count. A variance function,
#' \code{disp("power")}, names no column, and a term on the predictor alone,
#' \code{disp(~log(x))}, names none other than it.
#'
#' @return A \code{\link[base]{character}} vector, empty where there is no such
#' column.
#'
#' @noRd
disp_covariates <- function(formula, x_var = all.vars(crf_x_call(formula))) {
  if (is.null(formula)) {
    return(character(0))
  }
  spec <- parse_disp_term(formula)
  if (is.null(spec) || !identical(spec$route, "A")) {
    return(character(0))
  }
  setdiff(all.vars(str2lang(spec$value)), x_var)
}

#' The value a disp(~...) column takes on a prediction grid
#'
#' @param x The column, as the fitted data hold it.
#'
#' @details The first level of a factor, and the median of a numeric column. A
#' character or logical column takes the first of its sorted values, which is
#' the level \code{\link[base]{factor}} would put first and so the reference
#' level \pkg{brms} codes it against. The value cannot change a quantity read
#' from the mean, because no family a \code{disp(~...)} column is fitted for
#' has a mean that involves the dispersion parameter. The two routes where it
#' would, a zero-truncated negative binomial growth component and the joint
#' \code{hurdle_negbinomial}, refuse the column; see
#' \code{check_disp_count_truncation()}. For a predicted reading
#' the value is a placeholder, which the prediction methods replace; see
#' \code{disp_marginal_draws()}.
#'
#' @return A vector of length one, of the class of \code{x}; a factor keeps
#' every level of \code{x}, because \pkg{brms} builds the design matrix
#' against the levels the fit was given.
#'
#' @importFrom stats median
#'
#' @noRd
disp_fixed_value <- function(x) {
  if (is.factor(x)) {
    return(factor(levels(x)[1], levels = levels(x), ordered = is.ordered(x)))
  }
  if (is.character(x) || is.logical(x)) {
    return(sort(unique(x))[1])
  }
  median(x)
}

#' Write and mark the disp(~...) columns of a prediction grid
#'
#' @param newdata The grid, holding the predictor and any column the grid sets
#' itself.
#' @param data The data the fit was made from, \code{fit$data}.
#' @param formula An object of class \code{\link{bayesnecformula}}.
#' @param x_var The predictor's column names.
#'
#' @details A column the grid already holds is left as it is: the trials or
#' \code{rate()} denominator, pinned at 1, and the level factor of a joint
#' refit. Each added column is named in the attribute
#' \code{"bnec_disp_placeholders"}, which is what tells the prediction methods
#' that its value is a placeholder rather than one a user chose. Nothing is
#' added, and no attribute is set, for a fit without such a column, so its grid
#' is the one it was before #443.
#'
#' @return \code{newdata}, with the columns added and marked.
#'
#' @noRd
add_disp_placeholders <- function(newdata, data, formula,
                                  x_var = all.vars(crf_x_call(formula))) {
  columns <- setdiff(disp_covariates(formula, x_var), names(newdata))
  if (length(columns) == 0) {
    return(newdata)
  }
  for (v in columns) {
    newdata[[v]] <- disp_fixed_value(data[[v]])
  }
  attr(newdata, "bnec_disp_placeholders") <- union(
    attr(newdata, "bnec_disp_placeholders"), columns
  )
  newdata
}

#' Does the reported mean of this fit depend on the negative binomial shape?
#'
#' @param formula An object of class \code{\link{bayesnecformula}}.
#' @param family Either a \code{\link[stats]{family}} object or a family tag.
#'
#' @details Two routes report the mean of zero-truncated negative binomial
#' counts, \code{mu / (1 - (shape / (mu + shape))^shape)}
#' (\code{hurdle_positive_mean()}). One is the growth component
#' \code{bnec_hurdle()} fits with \code{family_growth = negbinomial()}, whose
#' formula carries the internal truncation. The other is the joint
#' \code{hurdle_negbinomial}, whose \code{dpar = "mu"} curve is that mean
#' (\code{joint_hurdle_epred()}) and whose combined curve, the default of
#' \code{ecx()}, \code{nsec()}, \code{autoplot()} and the stored
#' predictions, is that mean multiplied by \code{1 - hu}, as \pkg{brms}
#' computes it. The truncated Poisson routes report a mean that depends on
#' \code{mu} alone, and have no shape to model.
#'
#' @return A \code{\link[base]{logical}}.
#'
#' @noRd
shape_in_reported_mean <- function(formula, family) {
  if (identical(canonical_fam_tag(family), "hurdle_negbinomial")) {
    return(TRUE)
  }
  inherits(family, "family") && is_factorised_count_formula(formula, family)
}

#' Refuse a disp(~...) column on a zero-truncated negative binomial count
#'
#' @param formula An object of class \code{\link{bayesnecformula}}.
#' @param family The family the term is fitted on, or its tag: the growth
#' component's \code{negbinomial}, the default, or the joint
#' \code{hurdle_negbinomial}. It decides only which route the message names.
#'
#' @details The routes \code{shape_in_reported_mean()} names report a mean
#' that depends on the shape. Under a \code{disp(~z)} term the shape varies
#' with \code{z}, so the value a prediction grid gives \code{z} would change
#' the curve and every estimate read from it: at \code{mu = 5} the mean is
#' 7.16 at a shape of 0.5 and 5.04 at 50. Refused rather than fitted. A term
#' on the predictor alone, \code{disp(~x)}, and a variance function,
#' \code{disp("power")}, are not refused here: neither names a column the grid
#' would have to choose a value for. RF, on #443 for the growth component;
#' the joint family is refused for the same reason once it takes \code{disp()}
#' (D39 on #410).
#'
#' Raised by \code{bnec_hurdle()} before anything is fitted, by
#' \code{bnec_joint()} before the refit is announced, by \code{bnec()} and
#' \code{get_priors()} before their model loops, and by
#' \code{wrangle_model_formula()} as the backstop for the routes that build a
#' formula directly, the placement \code{check_disp_spec()} has.
#'
#' @return \code{invisible(NULL)}, called for its side effect.
#'
#' @noRd
check_disp_count_truncation <- function(formula, family = "negbinomial") {
  columns <- disp_covariates(formula)
  if (length(columns) == 0) {
    return(invisible(NULL))
  }
  curve <- if (identical(canonical_fam_tag(family), "hurdle_negbinomial")) {
    paste0(" on the two-block family hurdle_negbinomial. Its positive-count",
           " curve, dpar = \"mu\", is the mean of the zero-truncated counts,",
           " mu / (1 - (shape / (mu + shape))^shape), and its combined curve",
           " is that mean multiplied by 1 - hu, so both depend on the shape",
           " parameter.")
  } else {
    paste0(" on a negbinomial growth component of bnec_hurdle(). That",
           " component's curve is the mean of the zero-truncated counts,",
           " mu / (1 - (shape / (mu + shape))^shape), which depends on the",
           " shape parameter.")
  }
  stop("A disp() term naming ", paste0("\"", columns, "\"", collapse = ", "),
       " cannot be fitted", curve, " With the shape modelled on ",
       if (length(columns) == 1) "a column" else "columns",
       " other than the predictor, the curve and every estimate read from it",
       " would depend on the value given to ",
       if (length(columns) == 1) "that column" else "those columns",
       " when predicting. A disp() term on the predictor, such as disp(~",
       all.vars(crf_x_call(formula))[1], "), or a variance function, such",
       " as disp(\"power\"), can be fitted.", call. = FALSE)
}
