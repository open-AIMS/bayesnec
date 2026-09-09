#' Quantile of a response on the scale of its positive part
#'
#' The gamma priors for "top" and "bot" set their rate from a quantile of the
#' response. Where a share of the response is exactly zero those quantiles are
#' pulled down towards zero, and the prior collapses onto a scale that has
#' nothing to do with the asymptote it is meant to locate. That is not a rare
#' corner: the zero-inflated count families added under #104 exist precisely for
#' responses where a quarter or more of the values are zero.
#'
#' \strong{Why the probability is rescaled rather than the vector.} A quantile
#' of a zero-inflated response is a quantile of the \emph{mixture}, and it is
#' biased downward by the zero fraction throughout its range -- reaching exactly
#' zero only at the extreme. So testing for an exactly-zero quantile and
#' substituting the positive part, as the first version of this guard did, is a
#' step function applied to a continuous problem: it left the worst case, just
#' below the threshold, entirely unguarded. On a `nec4param` response with a true
#' `top` of 40 the `top` prior mean ran 33.5 at no zeros, 6.8 at 72\% zeros, and
#' then jumped back to 29.0 at 76\% once the raw quantile finally hit zero.
#'
#' Level \code{p} of the distribution conditional on being positive sits at level
#' \code{z + (1 - z)p} of the mixture, where \code{z} is the zero fraction: a
#' share \code{z} of the mass has to be passed before any positive value is
#' reached. Equivalently \code{1 - (1 - p)(1 - z)}. That is the quantile taken
#' here. It reduces to the raw quantile exactly when \code{z} is zero, so a
#' response with no zeros is untouched, and it degrades smoothly rather than in a
#' jump.
#'
#' Note the multiplication. The dividing form \code{1 - (1 - p) / (1 - z)} moves
#' the level the wrong way -- it returns 0.5 where the answer is 0.875 at
#' \code{p = 0.75, z = 0.5} -- and goes negative past 75\% zeros, which is the
#' regime this exists for.
#'
#' Deliberately not the same trick as \code{define_hurdle_prior()}, which
#' computes the whole mu-block prior from the non-zero subset. That is exact for
#' \code{hurdle_gamma}, because a Gamma has no mass at zero, so the non-zero
#' subset \emph{is} the mu process. Under zero-inflation it is not: the base
#' distribution emits zeros of its own, so conditioning on the positives draws
#' from a truncated count distribution and biases the location upward. Here the
#' positive part informs only a \emph{scale}, never the estimate itself.
#'
#' See #210 for the original three failure modes and #232 for why the guard
#' became a rescaling.
#'
#' @param response A \code{\link[base]{numeric}} vector.
#' @param probs A \code{\link[base]{numeric}} vector of length 1.
#'
#' @return A \code{\link[base]{numeric}} vector of length 1, strictly positive.
#'
#' @importFrom stats quantile
#'
#' @noRd
positive_scale <- function(response, probs) {
  finite <- response[is.finite(response)]
  pos <- finite[finite > 0]
  if (!length(pos)) {
    stop("Cannot build priors for \"top\" and \"bot\": the response contains no",
         " positive values, so there is no scale to place them on. Check the",
         " response variable, and see ?bnec for the families bayesnec supports.",
         call. = FALSE)
  }
  zero_frac <- sum(finite <= 0) / length(finite)
  # z == 0 leaves probs untouched, so a response with no zeros gets exactly the
  # quantile it got before this guard existed.
  probs_adj <- 1 - (1 - probs) * (1 - zero_frac)
  q <- unname(quantile(finite, probs = probs_adj))
  # The rescaled level can still land on a zero: at probs = 0 it stays at 0, and
  # ties across the boundary can put it there too. min(pos) is the smallest
  # scale the data actually supports, and is what the caller's fudge terms are
  # already built around.
  if (!is.finite(q) || q <= 0) {
    return(min(pos))
  }
  q
}

#' The default prior for the nec and ec50 parameters
#'
#' \code{nec} and \code{ec50} are measured in units of the predictor, so their
#' prior has to describe the concentration series that was tested. That series
#' is usually spaced logarithmically, and this builds a normal prior on the log
#' of the predictor: \code{lognormal(mu, sigma)} where the predictor is
#' supplied on the dose scale, and \code{normal(mu, sigma)} where it spans
#' negative values and has therefore already been log transformed by the user.
#' The two are one rule stated on two scales. Truncation is applied by the
#' caller, to the observed predictor range, and is unchanged.
#'
#' @details Until #302 there were three entries, selected by the support of the
#' predictor: \code{gamma(5, 4/m)} where the predictor was non-negative and
#' reached above 1, \code{beta(2, 2)} where it lay within [0, 1], and
#' \code{normal(median(x), 10 sd(x))} where it spanned negative values, with
#' \emph{m} the median of the distinct predictor values. Support is a property
#' of the units a dose is recorded in, so the same experiment received priors
#' differing roughly 300-fold in width according to whether the dose was
#' recorded on a scale reaching above 1, on one confined to the unit interval,
#' or logged. Measured on the \code{\link{nassarius}} contaminant A dose series,
#' the central 95\% interval of the prior covered 1.7\% of the predictor range
#' under the gamma entry, 81\% under \code{beta(2, 2)} rescaled to that range,
#' and 1097\% under the normal entry before truncation, for the same experiment
#' expressed three ways. The last is measured untruncated because after
#' truncation it is 95\% of the range by construction, which is the point: it
#' is effectively flat over everything the bounds permit.
#'
#' The gamma entry could not describe a log-spaced series at any rate. The
#' spread of a gamma is tied to its shape, so \code{gamma(5, 4/m)} places its
#' maximum density at \emph{m} and its central 95\% interval at 0.41\emph{m} to
#' 2.56\emph{m} whatever the data are. It therefore reaches the highest dose
#' tested only where that dose is within about 2.6 times the median dose, and
#' that ratio is a property of the design: 2.0 for a series spaced evenly from
#' zero, and 13 to 125 for the four nassarius series. No fixed shape serves
#' both. The shape that puts the maximum density at \emph{m} and still reaches
#' the highest dose is 8.6 on a linear series and 1.03 on the widest nassarius
#' series, and at 1.03 the density decreases monotonically across the whole
#' tested range, so it no longer has a maximum at \emph{m} at all --- which is
#' the failure #273 reported with its direction reversed.
#'
#' The prior built here has a monotonically decreasing density on the dose scale
#' over the whole tested range on all four nassarius series, and that is not the
#' same defect. A lognormal's dose-scale mode is \code{exp(mu - sigma^2)}, so
#' any lognormal wide enough sits below the lowest dose; the density falls
#' because the change of variable from the log scale to the dose scale
#' redistributes it, not because the prior has lost its centre. Its maximum
#' density is at the median dose on the log scale, which is the scale the
#' convention is now stated on, and the median of the untruncated prior on the
#' dose scale is the median dose. What the gamma at shape 1.03 loses is the
#' centre itself: neither its mode nor its median is at \emph{m} on any scale.
#' Over the sweep below the truncated prior CDF at the true value runs 0.43 to
#' 0.95, so the mass is where the doses are.
#'
#' \code{mu} is the median of the distinct positive predictor values, on the
#' log scale. Distinct values rather than the observation vector so that
#' replication does not change the prior, which is the rule #269 established
#' for the gamma rate. Taking the median after logging returns the log of the
#' median dose where the number of distinct positive doses is odd, and the log
#' of the geometric mean of the two central doses where it is even, that being
#' their midpoint on the log axis rather than on the dose axis. The prior's
#' maximum density is therefore at that dose measured on the log scale, and the
#' median of the untruncated prior on the dose scale is that dose. Both
#' statements describe the untruncated prior. Truncation at the highest dose
#' removes part of the upper tail and so pulls the median down: on
#' \code{\link{nec_data}} the truncated median is 0.58 against a median dose of
#' 0.88, and on the nassarius contaminant B series 1.23 against 2.00.
#' Fisher et al. (2024) specify maximum
#' density at the median predictor without saying which scale the density is
#' measured on; this reads it on the log-dose scale, which is the scale a
#' dilution series is designed on, and it is the only reading under which a
#' prior peaking at the median can also reach 125 times it.
#'
#' \code{sigma} on the dose scale is set so that the prior's central 95\%
#' interval covers every dose tested: it is the larger of the two half-widths
#' from \code{mu} to the ends of the logged series, divided by
#' \code{qnorm(0.975)}. The criterion is the whole of the rule --- a prior on a
#' threshold should not exclude a concentration the experiment applied, at
#' either end --- and it is stated rather than a constant being chosen, so it
#' adapts to the design. Setting \code{sigma} from half the range instead gives
#' the interval the right width and the wrong centre wherever the series is
#' asymmetric about its median on the log axis: on the nassarius contaminant A
#' series that interval runs 0.005 to 9.96 against a series running 0.01 to 20,
#' so it stops short of the highest dose applied.
#'
#' A fixed multiple \emph{k} of \code{sd(log x)} was considered and not taken.
#' It states no criterion, so it cannot guarantee the coverage above on a design
#' it was not chosen against, and any multiple large enough to be broad on a
#' densely sampled continuous predictor puts a large share of the prior below
#' the lowest dose tested on a wide dilution series, where the lower truncation
#' bound is the zero control. At \emph{k} = 1.5, 21\% of the truncated prior on
#' the nassarius contaminant A series lies below its lowest dose of 0.01, and
#' the lower end of its 95\% interval is 0.00022, a factor of 45 below anything
#' applied. The rule adopted leaves 9.0\% below the lowest dose there and 3.3\%
#' on a series spaced evenly from zero. Expressed as a multiple of
#' \code{sd(log x)} it lands between 0.73 and 1.18 across the five designs
#' measured, at 0.92 to 1.03 on the four nassarius series, and at 1.75 on
#' \code{\link{nec_data}}, whose predictor is continuous and densely sampled, so
#' it is not equivalent to any one constant.
#'
#' \code{sigma} is therefore set by the two extreme doses and not by the spread
#' of the series between them, which makes it sensitive to how the control is
#' recorded. The prior is built from the concentrations as recorded, so a
#' control entered as a nominal small positive value states that the value was
#' applied and the prior covers it: on the nassarius contaminant A series
#' \code{sigma} is 2.30 with the control at 0, 2.59 with it at 0.001 and 6.11
#' with it at 1e-6. Record a control as 0, which is what \code{\link{bnec}}
#' expects and what the truncation bound is then taken from.
#'
#' \code{sigma} on the branch for a predictor supplied already logged stays at
#' \code{10 sd(x)}. That multiplier is the published default, every herbicide
#' analysis in Fisher et al. (2024) uses it, and it is the branch the other two
#' are being moved to rather than one being changed. Its location and spread are
#' now read from the distinct values rather than from the observation vector, as
#' on the other branch, so the entry is not identical to the released one: over
#' \code{log(herbicide$concentration)}, 580 rows and 9 distinct values, the
#' location changes from 1.10 to 2.30 and the spread from 26.3 to 31.5. The
#' consequence of keeping the multiplier is that the same data analysed as
#' \code{crf(x)} and as \code{crf(log(x))} still do not receive the same prior
#' on \code{nec}. That difference is roughly tenfold, against roughly 600-fold
#' before this change, and it is a difference in width alone rather than in the
#' shape or the location of the prior.
#'
#' A design with fewer than two distinct positive predictor values states no
#' range for the prior to span and no spread for \code{sd} to measure, so
#' \code{sigma} falls back to 1 on the log scale. Such a design cannot identify
#' a concentration-response curve at all, and the fallback is chosen to be
#' harmless rather than to be right: the remedy is more concentrations, not a
#' better prior. A predictor with no positive values at all is refused, because
#' there is then no dose scale to place the prior on. \code{check_data()} fails
#' first on such data, so this is a backstop.
#'
#' Evidence for all of the above is prior-only; nothing was fitted. Priors were
#' obtained through \code{\link{get_priors}} over five designs, three predictor
#' transforms, all 12 families, both links and both prior types, and scored by
#' the truncated prior CDF at a known parameter value. The prior is a function
#' of the predictor alone, so 30 design by transform by parameter cells exhaust
#' it. The defaults being replaced placed the true value outside the central
#' 95\% of the truncated prior in 5 of those 30, every one of them a
#' log-spaced series read on the recorded or the square-root scale; the prior
#' built here does so in none, with the truncated CDF at the true value running
#' 0.47 to 0.95 over the zero-control designs and 0.43 to 0.95 over all 60
#' cells. The sweep does not cover a true threshold at the very bottom of
#' a wide dilution series, which is checked separately: on the nassarius
#' contaminant B series a threshold at the lowest dose applied sits at a
#' truncated CDF of 0.030, inside the central 95\%, against 0.005 under a prior
#' set from half the range. See #302.
#'
#' @param predictor A \code{\link[base]{numeric}} vector, the predictor as it
#' was supplied.
#'
#' @return A \code{\link[base]{character}} string of length 1, a \pkg{brms}
#' prior string.
#'
#' @importFrom stats median sd qnorm
#'
#' @noRd
predictor_prior <- function(predictor) {
  u <- unique(predictor)
  # A concentration cannot be negative, so a predictor that spans negative
  # values is one the user has transformed. Selecting on that is what selects
  # the scale the prior is stated on.
  spans_negative <- min(u) < 0
  z <- if (spans_negative) u else log(u[u > 0])
  if (!length(z)) {
    stop("Cannot build a prior for \"nec\" or \"ec50\": the predictor",
         " contains no positive values, so there is no concentration scale",
         " to place them on. Check the predictor variable, and see ?bnec.",
         call. = FALSE)
  }
  mu <- median(z)
  if (spans_negative) {
    dist <- "normal"
    sigma <- sd(z) * 10
  } else {
    dist <- "lognormal"
    # The larger of the two half-widths, not half the range. Setting sigma from
    # the range alone gives the interval the right width and the wrong centre
    # wherever the series is not symmetric about its median on the log axis, and
    # the interval then stops short of one end: on the nassarius contaminant A
    # series it reached 9.96 against a highest dose of 20, which is the defect
    # #302 exists to remove.
    sigma <- max(mu - min(z), max(z) - mu) / qnorm(0.975)
  }
  # Both branches degenerate on a single distinct value -- a single distinct
  # positive value, on the lognormal branch. sd() is NA there and both
  # half-widths are zero. One fallback serves both because both are measured on
  # the scale the prior is stated on.
  if (!is.finite(sigma) || sigma <= 0) {
    sigma <- 1
  }
  paste0(dist, "(", mu, ", ", sigma, ")")
}

#' define_prior
#'
#' Generates prior model objects to pass to \pkg{brms}
#'
#' @inheritParams bnec
#'
#' @param family A \code{\link[stats]{family}} function.
#' @param predictor The predictor variable for the NEC model fit.
#' @param response The response variable for the NEC model fit.
#' @param predictor_scale The predictor the \code{nec} and \code{ec50} prior is
#' built from and truncated to. Defaults to \code{predictor}, and differs from
#' it only for the two blocks of a hurdle or zero-inflated fit, each of which is
#' primed from a subset of the predictor but evaluated over the whole of it.
#' See \code{define_hurdle_prior()}.
#'
#' @return An object of class \code{\link[brms]{brmsprior}}.
#' @importFrom brms prior_string
#' @importFrom stats sd
#' @importFrom stats median
#'
#' @noRd
define_prior <- function(model, family, predictor, response,
                         prior_type = "uninformative",
                         model_survival = NULL, disp_spec = NULL,
                         group_spec = NULL, predictor_scale = NULL) {
  if (is.null(predictor_scale)) {
    predictor_scale <- predictor
  }
  # Which scale an ogl deviation is applied on decides how wide its prior
  # should be, and it is a property of the model and the family, both of which
  # are in scope here and are not in define_group_prior(). See #257.
  ogl_kind <- if (isTRUE(group_spec$ogl)) {
    ogl_transform_kind(model, family)
  } else {
    "none"
  }
  # The same question for a term on top or bot, and it is decided by the family
  # alone rather than by the model. See par_transform_kind(). #294.
  par_kind <- if (is.null(group_spec)) "none" else par_transform_kind(family)
  prior_type <- match.arg(prior_type, c("uninformative", "regularizing"))
  if (is_hurdle_family(family)) {
    hurdle_priors <- define_hurdle_prior(model, family, predictor, response,
                                         prior_type = prior_type,
                                         model_survival = model_survival,
                                         predictor_scale = predictor_scale)
    # A group-level term reaches the mu block only. add_formula_glef() runs
    # before the hu sub-formulas are attached, so `ogl` and `pgl` never see
    # them -- checked against the formula wrangle_model_formula() actually
    # builds, not assumed. So the same priors apply here, on the same nlpar
    # names, and the hu block needs nothing. Without this the hurdle families
    # kept the whole of #245: they take the early return above, so group_spec
    # was ignored for exactly the fits vignette("example8") part 3 needs.
    # Scaled from the survivors only, which is the response the mu block is
    # actually fitted to -- including the structural zeros would drag the scale
    # down for the same reason define_hurdle_prior() excludes them from top and
    # bot. Put on the link scale of the mu family before it is measured, so
    # that this branch and the one below both take the scale from the quantity
    # the offsets are actually added to rather than differing over a step that
    # is a no-op only for as long as the fit is on the identity link, which is
  # what bnec() assigns unless the caller wrote a link argument (#256).
    mu_family <- hurdle_mu_family(family)
    mu_response <- response_link_scale(
      split_hurdle_response(predictor, response)$mu$y, mu_family
    )
    group_priors <- define_group_prior(group_spec, predictor, mu_response,
                                       prior_type = prior_type,
                                       ogl_transform = ogl_kind,
                                       par_transform = par_kind)
    if (!is.null(group_priors)) {
      hurdle_priors <- hurdle_priors + group_priors
    }
    return(hurdle_priors)
  }
  link_tag <- family$link
  custom_name <- check_custom_name(family)
  if (link_tag %in% c("logit", "log")) {
    fam_tag <- "gaussian"
  } else { 
    fam_tag <- family$family
   }
  # The mu block of a zero-inflated count family is an ordinary poisson or
  # negbinomial mean -- the mixture changes how many zeros are observed, not the
  # scale of mu -- so the base family's priors are the right ones rather than a
  # duplicated set of entries in every table below.
  #
  # The quantiles below are taken over the whole response, structural zeros
  # included. That used to collapse the `top` and `bot` priors once a large
  # share of the response was zero -- the regime these families exist for --
  # and is now guarded by positive_scale(), which falls back to the same
  # quantile of the positive part. See its documentation for why that is not
  # the same trick define_hurdle_prior() uses, and #210 for what the three
  # failure modes were.
  if (fam_tag %in% c("zero_inflated_poisson", "zero_inflated_negbinomial")) {
    fam_tag <- sub("^zero_inflated_", "", fam_tag)
  }
  if (family$family == "beta_binomial" || family$family == "binomial") {
    if (is.integer(response) || max(response) > 1) {
      stop("Response vector must be passed as a proportion to define_prior",
           " (not as integers) for the binomial and beta_binomial families.")
    }
  }
  response <- response_link_scale(response, family)
  # Called for its error alone. It rejects an integer predictor, which
  # check_data() also rejects and for the reason recorded there; this call is
  # the backstop for the routes that reach prior construction directly. Its
  # value is no longer read: the nec and ec50 prior is one construction
  # selected by whether the predictor spans negative values, not three selected
  # by which distribution describes the predictor's support. See #302.
  set_distribution(predictor, silence_y_msgs = TRUE, silence_x_msgs = FALSE)
  # Two prior sets for the response-scaled parameters (top, bot):
  #  - "uninformative": the weakly-informative defaults described in the JSS
  #    article (Fisher et al. 2024); wider, closer to truly uninformative.
  #  - "regularizing": narrower priors, with the no-effect (top) parameter
  #    centred on the control mean -- which, for these monotonically decreasing
  #    models, sits at the upper end of the response range.
  # Only the response-scaled (top/bot) priors differ between the two sets; the
  # predictor-scaled and fixed priors below are shared.
  # Only these three families read u_t_g / u_b_g out of the tables below; every
  # other entry is a literal or is built from quantile()/sd() directly, and is
  # well defined on a response that is entirely negative or entirely zero. So
  # the gamma-scaled strings are built only when they will be used. Building
  # them unconditionally made positive_scale()'s "no positive values" error --
  # and the unguarded min(response[response > 0]) beside it -- reachable for
  # gaussian, where an all-negative response (log ratios, growth increments,
  # anything expressed as a change) is ordinary input. See #229.
  gamma_scaled <- fam_tag %in% c("Gamma", "poisson", "negbinomial")
  if (prior_type == "uninformative") {
    u_t_g <- u_b_g <- NA_character_
    if (gamma_scaled) {
      u_t_g <- paste0("gamma(2, ",
                      1 / (positive_scale(response, probs = 0.75) / 2),
                      ")")
      u_b_g <- paste0("gamma(2, ",
                      1 / ((positive_scale(response, probs = 0.25) +
                        min(response[response > 0]) / 100) / 2),
                      ")")
    }
    y_t_prs <- c(Gamma = u_t_g,
                 poisson = u_t_g,
                 negbinomial = u_t_g,
                 gaussian = paste0("normal(",
                                   quantile(response, probs = 0.9),
                                   ", ", sd(response) * 2.5, ")"),
                 bernoulli = "beta(5, 2)",
                 binomial = "beta(5, 2)",
                 "beta_binomial" = "beta(5, 2)",
                 beta = "beta(5, 2)")
    y_b_prs <- c(Gamma = u_b_g,
                 poisson = u_b_g,
                 negbinomial = u_b_g,
                 gaussian = paste0("normal(",
                                   quantile(response, probs = 0.1),
                                   ", ", sd(response) * 2.5, ")"),
                 bernoulli = "beta(2, 5)",
                 binomial = "beta(2, 5)",
                 "beta_binomial" = "beta(2, 5)",
                 beta = "beta(2, 5)")
  } else {
    u_t_g <- u_b_g <- NA_character_
    if (gamma_scaled) {
      u_t_g <- paste0("gamma(5, ",
                      5 / (positive_scale(response, probs = 1)),
                      ")")
      # probs = 0 is the minimum, which is zero for a response containing a
      # single zero -- not merely for a mostly-zero one. So under
      # "regularizing" the collapse was unconditional on the zero fraction,
      # where under "uninformative" it needed a quarter of the response to be
      # zero.
      u_b_g <- paste0("gamma(5, ",
                      5 / ((positive_scale(response, probs = 0) +
                        min(response[response > 0]) / 10)),
                      ")")
    }
    y_t_prs <- c(Gamma = u_t_g,
                 poisson = u_t_g,
                 negbinomial = u_t_g,
                 gaussian = paste0("normal(",
                                   quantile(response, probs = 1),
                                   ", ", sd(response), ")"),
                 bernoulli = "beta(5, 1)",
                 binomial = "beta(5, 1)",
                 "beta_binomial" = "beta(5, 1)",
                 beta = "beta(5, 1)")
    y_b_prs <- c(Gamma = u_b_g,
                 poisson = u_b_g,
                 negbinomial = u_b_g,
                 gaussian = paste0("normal(",
                                   quantile(response, probs = 0),
                                   ", ", sd(response), ")"),
                 bernoulli = "beta(1, 5)",
                 binomial = "beta(1, 5)",
                 "beta_binomial" = "beta(1, 5)",
                 beta = "beta(1, 5)")
  }
  # One construction for nec and ec50, on whichever scale the predictor was
  # supplied on. See predictor_prior() for why, and #302 for the measurements.
  x_pr <- predictor_prior(predictor_scale)
  lbs <- c(Gamma = 0, poisson = 0, negbinomial = 0, gaussian = NA,
           bernoulli = 0, binomial = 0, "beta_binomial" = 0, beta = 0)
  ubs <- c(Gamma = NA, poisson = NA, negbinomial = NA, gaussian = NA,
           bernoulli = 1, binomial = 1, "beta_binomial" = 1, beta = 1)
  # y-dependent priors
  pr_top <- prior_string(y_t_prs[fam_tag], nlpar = "top",
                         lb = lbs[fam_tag], ub = ubs[fam_tag])
  pr_bot <- prior_string(y_b_prs[fam_tag], nlpar = "bot",
                         lb = lbs[fam_tag], ub = ubs[fam_tag])
  # x-dependent priors
  pr_nec <- prior_string(x_pr, nlpar = "nec",
                         lb = min(predictor_scale), ub = max(predictor_scale))
  pr_ec50 <- prior_string(x_pr, nlpar = "ec50",
                          lb = min(predictor_scale), ub = max(predictor_scale))
  # x- and y-independent priors
  pr_d <- prior_string("normal(0, 5)", nlpar = "d")
  pr_beta <- prior_string("normal(0, 5)", nlpar = "beta")
  pr_f <- prior_string("normal(0, 5)", nlpar = "f")
  pr_slope <- prior_string("normal(0, 5)", nlpar = "slope")
  # assemble
  if (model == "ecxsigm") {
    priors <- pr_beta + pr_top + pr_d
  }
  if (model %in% c("ecx4param", "ecxwb1", "ecxwb2", "ecxll4")) {
    priors <- pr_beta + pr_top + pr_bot + pr_ec50
  }
  if (model == "ecxll5") {
    priors <- pr_beta + pr_top + pr_bot + pr_ec50 + pr_f
  }
  if (model %in% c("ecxwb1p3", "ecxwb2p3", "ecxll3")) {
    priors <- pr_beta + pr_top + pr_ec50
  }
  if (model == "neclin") {
    priors <- pr_top + pr_slope + pr_nec
  }
  if (model == "nec3param") {
    priors <- pr_beta + pr_top + pr_nec
  }
  if (model == "nec4param") {
    priors <- pr_beta + pr_top + pr_bot + pr_nec
  }
  if (model %in% c("nechorme", "nechormepwr", "nechormepwr01")) {
    priors <- pr_beta + pr_top + pr_nec + pr_slope
  }
  if (model == "neclinhorme") {
    priors <- pr_beta + pr_top + pr_nec + pr_slope
  }
  if (model %in% c("nechorme4",  "nechorme4pwr")) {
    priors <- pr_beta + pr_top + pr_nec + pr_slope + pr_bot
  }
  if (model == "necsigm") {
    priors <- pr_beta + pr_top + pr_nec + pr_d
  }
  if (model == "ecxlin") {
    priors <- pr_slope + pr_top
  }
  if (model == "ecxexp") {
    priors <- pr_beta + pr_top
  }
  if (model == "ecxhormebc4") {
    priors <- pr_top + pr_beta + pr_ec50 + pr_slope
  }
  if (model == "ecxhormebc5") {
    priors <- pr_bot + pr_top + pr_beta + pr_ec50 + pr_slope
  }
  disp_priors <- define_disp_prior(disp_spec, family, response)
  if (!is.null(disp_priors)) {
    priors <- priors + disp_priors
  }
  # response is on the link scale by this point, which is what the group-level
  # offsets are added on, so it is the right scale to take the prior from.
  group_priors <- define_group_prior(group_spec, predictor, response,
                                     prior_type = prior_type,
                                     ogl_transform = ogl_kind,
                                     par_transform = par_kind)
  if (!is.null(group_priors)) {
    priors <- priors + group_priors
  }
  priors
}

#' define_disp_prior
#'
#' Builds priors for the non-linear parameters a variance function introduces.
#'
#' @param disp_spec The output of \code{\link{parse_disp_term}}.
#' @param family A \code{\link[stats]{family}} function.
#' @param response The response variable, already on the link scale.
#'
#' @details Only route (B) is given priors here. Route (A) is an ordinary
#' distributional formula and is left to the \pkg{brms} defaults, which already
#' suit a linear predictor on a log link.
#'
#' \code{c1} and \code{c2} are centred on zero, which is the constant-dispersion
#' case, so the prior asserts no mean-variance relationship and lets the data
#' supply one. \code{c0} is the dispersion parameter on the log scale at the
#' variance function's reference value (see \code{\link{disp_centre}}) -- that
#' is, at a typical response rather than at \code{mu = 1}. That is what makes
#' these priors mean anything at all: uncentred, \code{c0} and the slope are
#' near-perfectly confounded and the induced prior on the dispersion parameter
#' at the data spans many orders of magnitude whenever the response is far from
#' one. The scale is still deliberately loose, because the reference locates the
#' intercept but says nothing about how large the dispersion there should be.
#'
#' @return An object of class \code{\link[brms]{brmsprior}}, or \code{NULL}.
#'
#' @importFrom brms prior_string
#' @importFrom stats sd
#'
#' @noRd
define_disp_prior <- function(disp_spec, family, response) {
  if (is.null(disp_spec) || disp_spec$route != "B") {
    return(NULL)
  }
  fam_tag <- family$family
  c0_prs <- c(
    gaussian = paste0("normal(", round(log(sd(response)), 3), ", 2)"),
    # shape is an inverse dispersion for both of these: a CV of 0.1 to 0.5 puts
    # a Gamma shape between about 4 and 100, i.e. 1.4 to 4.6 on the log scale.
    Gamma = "normal(2, 2)",
    negbinomial = "normal(2, 2)",
    # phi likewise, on the wider scale the PAM fits needed.
    beta = "normal(4, 3)",
    beta_binomial = "normal(4, 3)"
  )
  vf <- disp_functions[[disp_spec$value]]
  # A slope on log(mu) is dimensionless, so a fixed scale means the same thing
  # whatever the response is measured in. A slope on mu itself is not: it
  # carries units of 1/response, and normal(0, 2) would be near-flat for a
  # response spanning thousands and highly informative for one spanning a
  # fraction. Scaling by the observed spread restores the intended meaning --
  # that a one-standard-deviation change in the mean moves the dispersion
  # parameter by about two units on the log scale at the edge of the prior.
  slope_prior <- if (isTRUE(vf$scale_free)) {
    "normal(0, 2)"
  } else {
    paste0("normal(0, ", signif(2 / sd(response), 4), ")")
  }
  out <- prior_string(unname(c0_prs[fam_tag]), nlpar = "c0")
  for (p in setdiff(vf$pars, "c0")) {
    out <- out + prior_string(slope_prior, nlpar = p)
  }
  out
}

#' define_group_prior
#'
#' Builds priors for the parameters a group-level term introduces.
#'
#' @param group_spec The output of \code{\link{parse_group_terms}}.
#' @param predictor A \code{\link[base]{numeric}} vector of the predictor.
#' @param response A \code{\link[base]{numeric}} vector of the response, on
#' the link scale.
#' @param prior_type One of \code{"uninformative"} or \code{"regularizing"}.
#' @param ogl_transform The output of \code{\link{ogl_transform_kind}} for this
#' model and family, or \code{"none"}.
#' @param par_transform The output of \code{\link{par_transform_kind}} for this
#' family, or \code{"none"}.
#'
#' @details Without this, no prior is generated for a group-level standard
#' deviation and it falls through to the \pkg{brms} default,
#' \code{student_t(3, 0, 2.5)}. On a bounded response under the identity link
#' \code{\link{bnec}} assigns, an offset drawn at that scale puts the mean
#' outside its support, where the likelihood is undefined. There is no inverse
#' link to rescue it -- that is the trade \code{\link{bnec}} makes so that
#' \code{top}, \code{bot} and \code{nec} stay directly interpretable -- so
#' keeping the mean in range falls entirely to the prior. See #245.
#'
#' \strong{The rule.} A group-level standard deviation is given one tenth of
#' the observed range of \emph{the scale its parameter lives on}, following the
#' same three-way split \code{\link{define_prior}} already makes for the
#' curve's own parameters:
#'
#' \itemize{
#'   \item \code{top}, \code{bot} and \code{ogl} are on the response scale:
#'     \code{diff(range(response)) / 10}, unless the deviation is applied
#'     multiplicatively, in which case it is on the log or log-odds scale and
#'     the width is converted onto it (see below).
#'   \item \code{nec} and \code{ec50} are on the predictor scale:
#'     \code{diff(range(predictor)) / 10}.
#'   \item \code{beta}, \code{slope}, \code{d} and \code{f} are
#'     dimensionless and take \code{normal(0, 5)} of their own, so 0.5.
#' }
#'
#' \strong{It is a rule about the data, not about the parameter's own prior},
#' and the distinction is worth keeping straight because the two coincide only
#' for the dimensionless parameters, where \code{normal(0, 5)} gives exactly
#' 0.5. Elsewhere the realised group-level standard deviation runs between
#' roughly a twentieth and a third of the spread of the parameter's own prior,
#' depending on family and data -- 0.06 of it for a \code{poisson} \code{top},
#' 0.34 for a Beta one. Tying it to the parameter's prior instead would inherit
#' scales chosen to be deliberately diffuse: \code{top} on a gaussian response
#' takes \code{2.5 * sd(response)}, which is a reasonable statement of
#' ignorance about a level and a poor one about deviation around it. The
#' observed range is the more defensible anchor, and it is the same quantity
#' the predictor-scaled priors are already bounded by.
#'
#' For \code{nec} and \code{ec50} the two readings do agree, because those
#' priors are truncated to \code{[min(predictor), max(predictor)]}, so the
#' range \emph{is} the scale the prior spans.
#'
#' \code{student_t(3, 0, s)} keeps the shape and heavy tail of the \pkg{brms}
#' default and changes only its scale, so this narrows a default that was never
#' scale-aware rather than substituting a differently-shaped one.
#'
#' \strong{What the prior cannot do.} Every prior
#' \code{\link{define_prior}} generates constrains its parameter to the region
#' where the model is defined: \code{beta(5, 2)} on (0, 1), \code{lb = 0} for
#' the count and Gamma families, \code{nec} truncated to the predictor range.
#' A group-level deviation cannot be constrained that way -- \pkg{brms} declares
#' \code{r_} unconstrained -- so a grouped fit does not inherit the property
#' that \code{top}, \code{bot} and \code{nec} remain in range, and no choice
#' of scale here restores it. Scaling the prior is what allows such a fit to
#' initialise; it does not stop the sampler reaching the boundary afterwards,
#' because the posterior for the standard deviation is an order of magnitude
#' smaller than the distance from the fitted mean to that boundary and it is the
#' length of the leapfrog trajectory that crosses it. Divergent transitions on a
#' grouped fit are therefore expected wherever the response distribution
#' restricts the range of the mean; see \code{\link{add_brm_defaults}} for the
#' \code{adapt_delta} that mitigates them and \code{vignette("example3")} for
#' what a user should check.
#'
#' \strong{Where the deviation is applied multiplicatively that argument no
#' longer holds}, and the prior is on a different scale. #257 did this for
#' \code{ogl} and #294 for \code{top} and \code{bot}: the deviation enters as
#' \code{p = m e^o / (1 - m + m e^o)} on (0, 1) or \code{p = m e^o} on
#' (0, Inf), so no value of \code{o} can put the parameter outside its support
#' and the prior on \code{o} is a statement about a ratio rather than about a
#' difference. The declared name changes with it: a transformed term puts the
#' standard deviation on \code{botgl} rather than on \code{bot}. The
#' population-level prior on \code{bot} itself is untouched, because \code{bot}
#' is still a population-level non-linear parameter.
#'
#' A transformed term gets \strong{one} prior, the standard deviation. It has no
#' population intercept to give a prior to; see \code{\link{add_par_gl_term}}
#' for why. \code{ogl} gets two, and that asymmetry is deliberate.
#'
#' \strong{prior_type.} \code{"regularizing"} halves every generated scale.
#' The two default sets differ only in the response-scaled parameters for the
#' curve itself, but a user reaching for the narrower set on a grouped fit is
#' usually reaching for it \emph{because} of the grouping, and leaving the one
#' parameter that provoked the choice untouched would make the argument inert
#' where it is most wanted. A factor of two is the same order as the narrowing
#' the response-scaled priors get (2.5 for gaussian, 1.6 for the gamma-scaled
#' families, 1.3 for the beta-scaled ones).
#'
#' The \code{ogl} \emph{intercept} gets a prior too, and needs one for a
#' different reason. \code{ogl} enters as an offset added to the whole curve,
#' so its population intercept is \strong{not identified}: a constant added to
#' \code{ogl} can be taken back out of \code{top} and \code{bot} with no
#' change to the likelihood, and \pkg{brms} leaves a non-linear
#' population-level parameter flat by default. Centring it at zero is what makes
#' the decomposition identified -- \code{top} and \code{bot} carry the level,
#' and the grouping term carries deviation about it.
#'
#' @return An object of class \code{\link[brms]{brmsprior}}, or \code{NULL}.
#'
#' @importFrom brms prior_string
#'
#' @noRd
define_group_prior <- function(group_spec, predictor, response,
                               prior_type = "uninformative",
                               ogl_transform = "none",
                               par_transform = "none") {
  if (is.null(group_spec) || length(group_spec$nlpars) == 0) {
    return(NULL)
  }
  narrow <- if (prior_type == "regularizing") 2 else 1
  # A response or predictor with no spread gives a scale of zero, which is not a
  # usable prior. It is degenerate input rather than something to model around,
  # so fall back to the dimensionless scale and let the fit fail on its own
  # terms if it is going to. The fallback is not narrowed: it is a stand-in for
  # a scale that could not be measured, not a measurement to be regularized.
  safe_scale <- function(x) {
    s <- diff(range(x)) / (10 * narrow)
    if (!is.finite(s) || s <= 0) 0.5 else s
  }
  s_y <- safe_scale(response)
  s_x <- safe_scale(predictor)
  # Where the ogl deviation is applied multiplicatively (#257), it is on the
  # log or log-odds scale rather than on the response scale, so the response
  # scale s_y is not the right width for it. These are delta-method conversions
  # of the same rule, evaluated at the mean of the response:
  #
  #   s_log   = s_y / mean(y)             a group-level coefficient of variation
  #   s_logit = s_y / (m * (1 - m))
  #
  # Both are evaluated at a single point and the Jacobian varies along the
  # curve -- at m = 0.9 the logit Jacobian is 11.1 and at m = 0.5 it is 4 -- so
  # this is a conversion of the existing convention onto the new scale, not an
  # exact reparameterisation of the same prior.
  m_y <- mean(response, na.rm = TRUE)
  # Taken as an argument rather than read off group_spec. It used to be set in
  # add_brm_defaults() and nowhere else, so get_priors() and amend() -- which
  # build group_spec straight from parse_group_terms() -- fell through to the
  # response-scale width and disagreed with what bnec() actually fitted by a
  # factor of four. get_priors() then misreported the prior in use, which the
  # initial-value fallback message explicitly tells the user to trust, and
  # amend() fitted a model into an existing set with a prior no other member
  # had. Computed once here, from the model and family define_prior() already
  # has, so the three callers cannot drift apart. See #257.
  #
  # `cap` is the difference between the ogl conversion and the parameter-level
  # one. #257 capped the log branch only, because s_y shrinks as the mean
  # approaches either bound of (0, 1) and the logit ratio is therefore
  # self-limiting at the response mean. A parameter-level deviation is
  # converted at the response mean as well, so the same argument holds, but the
  # parameter it is applied to -- bot -- is not the response mean and sits near
  # zero. The cap is applied to both branches there so that the width cannot
  # run away on a response whose own mean is close to a bound. Left off the ogl
  # branches so that #257's fits are unchanged. See #294.
  converted_scale <- function(kind, cap = FALSE) {
    out <- switch(
      kind,
      # Capped. s_y does not shrink as the response mean approaches zero, so
      # the ratio is unbounded there: on a count response with many structural
      # zeros -- zero_inflated_poisson and zero_inflated_negbinomial are both
      # accepted -- s_y / m_y can put several orders of magnitude on the
      # group-level mean. The logit branch below is self-limiting because s_y
      # shrinks as the mean approaches either bound; this one is not. The cap
      # is a coefficient of variation of 1, which at two prior standard
      # deviations still admits a factor of e^2 on the mean and is far wider
      # than any group-level effect these designs carry. See #257.
      log = if (is.finite(m_y) && m_y > 0) min(s_y / m_y, 1) else s_y,
      logit = if (is.finite(m_y) && m_y > 0 && m_y < 1) {
        s_y / (m_y * (1 - m_y))
      } else {
        s_y
      },
      s_y
    )
    # Capped at 1 / narrow rather than at 1. narrow is applied to s_y before the
    # conversion, so capping at a constant afterwards made the two prior_type
    # settings return exactly the same width wherever the uninformative one
    # already exceeded the cap -- which is the response mean close to a bound,
    # the case the cap exists for. A user selecting "regularizing" on a grouped
    # fit then changed nothing for the parameter that prompted the choice. See
    # #294.
    if (cap && !identical(kind, "none")) min(out, 1 / narrow) else out
  }
  s_ogl <- converted_scale(ogl_transform)
  s_par <- converted_scale(par_transform, cap = TRUE)
  scale_for <- function(par) {
    if (par == "ogl") {
      s_ogl
    } else if (par %in% c("top", "bot")) {
      # A transformed deviation is on the log or log-odds scale, so the
      # response-scale width is not the right one for it; the same delta-method
      # conversion the ogl prior uses is applied instead.
      if (par_is_transformed(par, par_transform)) s_par else s_y
    } else if (par %in% c("nec", "ec50")) {
      s_x
    } else {
      0.5 / narrow
    }
  }
  # The name the standard deviation is declared under is the deviation term's,
  # not the parameter's: a transformed term puts (1 | group) on botgl, and bot
  # itself keeps the population-level prior define_prior() already built for it.
  # A prior on nlpar "bot" of class "sd" would match nothing in the fit and brms
  # would drop it silently. See add_par_gl_term(). #294.
  nlpar_for <- function(par) {
    if (par_is_transformed(par, par_transform)) {
      unname(par_gl_names(par)[["dev"]])
    } else {
      par
    }
  }
  out <- NULL
  for (p in group_spec$nlpars) {
    pr <- prior_string(paste0("student_t(3, 0, ", signif(scale_for(p), 4), ")"),
                       class = "sd", nlpar = nlpar_for(p))
    out <- if (is.null(out)) pr else out + pr
  }
  if (isTRUE(group_spec$ogl)) {
    out <- out + prior_string(paste0("normal(0, ", signif(s_ogl, 4), ")"),
                              nlpar = "ogl")
  }
  # No prior on a deviation intercept, because there is no deviation intercept:
  # add_par_gl_term() writes botgl ~ 0 + (1 | group), so brms declares no
  # b_botgl. That is deliberate and is what keeps bot interpretable -- bnecbot
  # depends on bot and botgl only through their combination, so a free intercept
  # would be exactly unidentified against bot. ogl is the other case and does
  # get one, because it is documented as adding a population-level parameter.
  # See add_par_gl_term(). #294.
  out
}

#' define_hurdle_prior
#'
#' Builds priors for both parameter blocks of a joint hurdle fit.
#'
#' @inheritParams define_prior
#'
#' @details The two blocks are primed from different views of the same data.
#' The mu block is a zero-bounded response model and is given the existing
#' Gamma/identity defaults, but computed from the **survivors only** --
#' including the zeros would drag the \code{top} and \code{bot} quantiles well
#' below the real control mean. The hu block is a probability and is given the
#' existing bernoulli/identity defaults, which are already 0-1 bounded.
#'
#' The hu sub-model is written as \code{1 - survival}, so its \code{hutop} is
#' control *survival* and takes the same \code{beta(5, 2)} style prior that a
#' bernoulli fit would use.
#'
#' @return An object of class \code{\link[brms]{brmsprior}}.
#'
#' @importFrom stats Gamma
#' @importFrom brms prior_string
#'
#' @noRd
define_hurdle_prior <- function(model, family, predictor, response,
                                prior_type = "uninformative",
                                model_survival = NULL,
                                predictor_scale = NULL) {
  if (is.null(predictor_scale)) {
    predictor_scale <- predictor
  }
  dpar <- hurdle_dpar(family)
  # The second block may carry a different equation from the response block,
  # in which case its priors must be built for that equation's parameters.
  if (is.null(model_survival)) {
    model_survival <- model
  }
  parts <- split_hurdle_response(predictor, response)
  # mu block: reuse the defaults of whatever the non-zero response looks like
  # (Gamma for hurdle_gamma, Beta for zero_inflated_beta), built from the
  # non-zeros only -- including the zeros would drag the top and bot quantiles
  # well below the real control level.
  mu_priors <- define_prior(model, hurdle_mu_family(family),
                            parts$mu$x, parts$mu$y, prior_type = prior_type,
                            predictor_scale = predictor_scale)
  # second block: reuse the bernoulli/identity defaults on the proportion
  # non-zero, then rename every non-linear parameter into its namespace.
  hu_priors <- define_prior(model_survival, bernoulli(link = "identity"),
                            parts$hu$x, parts$hu$y, prior_type = prior_type,
                            predictor_scale = predictor_scale)
  hu_priors$nlpar <- ifelse(nzchar(hu_priors$nlpar),
                            paste0(dpar, hu_priors$nlpar), hu_priors$nlpar)
  # Both blocks are evaluated over the *whole* predictor range inside the joint
  # fit, but each is primed from a subset of it: mu from the non-zeros only,
  # which stop short of the concentrations where everything is zero, and the
  # second block from the deduplicated unique-x vector. `predictor_scale` above
  # is what makes the predictor-scaled priors and their bounds come from the
  # whole predictor rather than from those subsets, so that neither threshold is
  # boxed out of the range it must cover. Only the mu block is affected in
  # practice: survival_by_x() returns sort(unique(predictor)), so the second
  # block's own vector already has the whole predictor's distinct values, and
  # its prior and bounds are unchanged.
  #
  # The prior is taken from the whole predictor and not only its bounds. A prior
  # shaped by the survivor subset but truncated to the whole predictor states
  # that the threshold lies below the highest concentration at which anything
  # survived, which is the same failure #302 removes from the single-block path:
  # on a series reaching 100 whose survivors stop at 10 the mu block's nec prior
  # placed its 97.5% point at 10.0 while its bounds permitted 100. It also
  # restores the invariance the single-block path has, that the nec and ec50
  # prior is a function of the predictor alone, and it makes the two blocks of
  # one fit agree about the scale of their shared predictor, which is what #269
  # set out to achieve. #269's argument for priming from the survivors concerns
  # the response-scaled top and bot, which are still taken from that subset; it
  # does not reach a parameter measured in units of the predictor.
  #
  # It also removes a refusal this path would otherwise reach. Where every
  # survivor sits at the zero control the mu subset has no positive value and
  # predictor_prior() has no concentration scale to build on; the whole
  # predictor always has one wherever the fit is meaningful at all.
  mu_priors + hu_priors
}
