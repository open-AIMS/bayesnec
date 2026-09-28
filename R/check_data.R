#' on_rational_grid
#'
#' Are all values expressible as k / n for a single small integer n?
#'
#' Genuine proportion data derived from counts (survival, fertilisation,
#' bleaching scores) lies on such a grid; a continuous response divided by a
#' continuous maximum does not. This is the discriminator that makes the
#' divided-by-maximum check usable: without it the check fires on up to 38% of
#' simulated genuine count proportions, purely because one replicate happened
#' to record every individual as alive. With it the false positive rate is zero
#' in both the simulation and the real-data sweep (see
#' notes/normalisation_detection.md).
#'
#' The cost is that a count proportion that really was divided by its own
#' maximum stays on a rational grid and is therefore missed. That is the safe
#' direction to fail for a diagnostic message.
#'
#' @noRd
on_rational_grid <- function(y, max_n = 100, tol = 1e-8) {
  y <- y[is.finite(y)]
  if (!length(y)) {
    return(FALSE)
  }
  for (n in seq_len(max_n)) {
    if (all(abs(y * n - round(y * n)) < tol)) {
      return(TRUE)
    }
  }
  FALSE
}

#' check_normalisation
#'
#' Detect a response that has been normalised to a quantity estimated from the
#' dataset being analysed, and say why that is a problem.
#'
#' Two practices leave an exact arithmetic trace:
#'
#' A. divided by the observed maximum -- the maximum is exactly 1 and exactly
#'    one observation attains it, because \code{v / max(v)} is exactly 1 in
#'    floating point at the maximum and nowhere else.
#' B. divided by the control mean -- the mean of the observations at the lowest
#'    predictor value is exactly 1.
#'
#' Messages rather than warnings: neither is fatal, both are recoverable by
#' refitting the raw response, and the user may have a reason. They are emitted
#' from \code{\link{bnec}} rather than \code{check_data} so that they fire once
#' per call rather than once per model in a set.
#'
#' @noRd
check_normalisation <- function(data) {
  y <- try(retrieve_var(data, "y_var", error = TRUE), silent = TRUE)
  x <- try(retrieve_var(data, "x_var", error = TRUE), silent = TRUE)
  if (inherits(y, "try-error") || inherits(x, "try-error")) {
    return(invisible(NULL))
  }
  ok <- is.finite(y) & is.finite(x)
  y <- y[ok]
  x <- x[ok]
  if (length(y) < 5) {
    return(invisible(NULL))
  }
  cite <- paste0("See Ritz et al. (2026) doi:10.1007/s10651-025-00698-y, and",
                 " ?ecx for what to do instead.")
  if (max(y) == 1 && sum(y == 1) == 1 && !on_rational_grid(y)) {
    message("Your response has a maximum of exactly 1 attained by exactly one",
            " observation, which is the signature of a response divided by its",
            " own observed maximum. Dividing by an extreme order statistic",
            " correlates every observation, discards the uncertainty in the",
            " divisor and biases ECx estimates upwards; it also forces one",
            " observation outside the open support of the Beta family. Prefer",
            " fitting the raw response and reading effective concentrations",
            " off the fitted curve with ecx(type = \"absolute\"), which is",
            " already the default. ", cite)
  }
  ctl <- y[x == min(x)]
  if (length(ctl) >= 3 && !all(ctl == ctl[1]) && abs(mean(ctl) - 1) < 1e-8) {
    message("The observations at your lowest predictor value average to",
            " exactly 1, which is the signature of a response divided by the",
            " control mean. Dividing every observation by the same estimated",
            " quantity discards the uncertainty in the control level, so ECx",
            " is biased upwards and both ECx and NSEC intervals are narrower",
            " than the data support. Nothing is lost by not normalising: the",
            " concentration giving an x percent rise in inhibition is the",
            " concentration giving an x percent decline in the raw response,",
            " which ecx(type = \"absolute\") returns by default. ", cite)
  }
  invisible(NULL)
}


#' The generalised linear model whose variance function is a family's own
#'
#' The contrast at the top of the series is tested under the mean-variance
#' relationship the fit itself assumes, so that the standard error it is judged
#' against is the one the family implies. The quasi-likelihood families supply
#' that variance function exactly for every row but two: \code{quasipoisson} is
#' linear in the mean where the negative binomial variance is quadratic, and
#' \code{quasibinomial} absorbs a beta-binomial over-dispersion as a constant
#' multiplier only where the number of trials is constant within a level.
#' Neither is remedied by a parametric fit at this size, because the shape
#' parameter of a negative binomial or a beta-binomial is not identified by two
#' levels of a handful of replicates, and a pre-fit advisory message that
#' reports its own convergence failure is worse than one that is slightly
#' conservative.
#'
#' The \code{log} link is written out for the Gamma and count rows. The
#' \code{inverse} link \code{stats::Gamma()} takes by default reverses the sign
#' of the contrast, so a declining response would be tested as a rising one.
#'
#' The Beta row is the one that looks as though it needs a beta regression. Its
#' variance is \code{mu (1 - mu) / (1 + phi)}, which is \code{mu (1 - mu)} times
#' a constant, and that is what \code{quasibinomial} fits; the precision
#' parameter reappears as the estimated dispersion.
#'
#' The zero-inflated count families are not two-block families here, so the
#' check runs on their whole response and the extra zeros are carried by the
#' estimated dispersion.
#'
#' @param family Either a \code{\link[stats]{family}} object or a family tag.
#'
#' @return A \code{\link[base]{list}} with elements \code{family} (an object of
#' class \code{\link[stats]{family}}), \code{kind} ("plain" for a response
#' modelled as supplied, "matrix" for a two-column count response) and
#' \code{offset} (whether a \code{rate()} denominator enters as an offset), or
#' \code{NULL} where the family has no row.
#'
#' @importFrom stats gaussian Gamma binomial quasipoisson quasibinomial
#'
#' @noRd
flatness_spec <- function(family) {
  fam_tag <- if (inherits(family, "family")) family$family else family
  switch(
    fam_tag,
    gaussian = list(family = gaussian(), kind = "plain", offset = FALSE),
    Gamma = list(family = Gamma(link = "log"), kind = "plain", offset = FALSE),
    poisson = ,
    negbinomial = ,
    zero_inflated_poisson = ,
    zero_inflated_negbinomial = list(family = quasipoisson(link = "log"),
                                     kind = "plain", offset = TRUE),
    bernoulli = list(family = binomial(), kind = "plain", offset = FALSE),
    binomial = ,
    beta_binomial = list(family = quasibinomial(), kind = "matrix",
                         offset = FALSE),
    beta = list(family = quasibinomial(), kind = "plain", offset = FALSE),
    NULL
  )
}

#' The blocks of a response the flatness contrast is computed on
#'
#' A two-block family is split into the response of the individuals past the
#' hurdle and the survival that put them there, because the two describe
#' different curves and either can be the one that has not flattened.
#'
#' The survival block is built from the raw counts rather than from the
#' proportions \code{survival_by_x()} returns, which are clamped to
#' \code{[eps, 1 - eps]} so that Stan can fit them under an identity link. It
#' holds one proportion per predictor value, so there is no replication within
#' a level anywhere in it, and its information comes instead from the number of
#' individuals behind each proportion.
#'
#' @param x A \code{\link[base]{numeric}} predictor.
#' @param y A \code{\link[base]{numeric}} response.
#' @param trials A \code{\link[base]{numeric}} vector of binomial trials, or
#' \code{NULL}.
#' @param denominator A \code{\link[base]{numeric}} \code{rate()} denominator,
#' or \code{NULL}.
#' @param family The validated response family.
#'
#' @return A named \code{\link[base]{list}} of blocks, each with elements
#' \code{x}, \code{spec}, and either \code{y} or \code{successes} and
#' \code{trials}.
#'
#' @noRd
flatness_blocks <- function(x, y, trials, denominator, family) {
  if (is_hurdle_family(family)) {
    parts <- split_hurdle_response(x, y)
    ux <- sort(unique(x))
    counts <- vapply(ux, function(z) sum(x == z), numeric(1))
    alive <- vapply(ux, function(z) sum(y[x == z] > 0), numeric(1))
    mu_spec <- flatness_spec(hurdle_mu_family(family))
    mu_block <- list(x = parts$mu$x, y = parts$mu$y, spec = mu_spec)
    # Unreachable today: make_brmsformula() refuses a rate() term for anything
    # but poisson and negbinomial, so no hurdle fit has a denominator. Attached
    # anyway, because the mu block of a count hurdle has a count spec whose
    # offset is TRUE, and leaving it off would put that block's contrast on
    # counts rather than rates the day a rate() hurdle is allowed.
    if (isTRUE(mu_spec$offset)) {
      mu_block$denominator <- denominator[y > 0]
    }
    return(list(
      response = mu_block,
      # Dispersion is held at 1, which is the assumption the second block of
      # the fit makes as well: brms models it as a Bernoulli process per
      # observation.
      survival = list(x = ux, successes = alive, trials = counts,
                      spec = list(family = binomial(), kind = "matrix",
                                  offset = FALSE))
    ))
  }
  spec <- flatness_spec(family)
  if (is.null(spec)) {
    return(list())
  }
  block <- list(x = x, y = y, spec = spec)
  if (identical(spec$kind, "matrix")) {
    # A binomial family with no trials() term is refused by check_data(), which
    # runs after this report. Nothing is said here, so that the refusal is what
    # the user sees rather than a report that the contrast could not be
    # computed.
    if (is.null(trials)) {
      return(list())
    }
    block$successes <- y
    block$trials <- trials
  }
  if (isTRUE(spec$offset)) {
    block$denominator <- denominator
  }
  list(response = block)
}

#' Test one block for a decline between its two highest predictor values
#'
#' The two levels are compared by analysis of deviance rather than by the Wald
#' statistic \code{summary.glm()} prints. The two agree exactly for a gaussian
#' response, where both are the two-sample t test, and the deviance test is the
#' one that survives the separation the survival block routinely produces: a
#' series running from every individual alive to none alive gives a Wald
#' standard error of several thousand and a p-value near a half, which would
#' miss the clearest incomplete design there is.
#'
#' @param block One element of \code{\link{flatness_blocks}}.
#' @param alpha The one-sided significance level.
#' @param pool_dispersion Whether a gaussian dispersion is estimated over the
#' whole series rather than over the two levels being compared.
#'
#' @return A \code{\link[base]{list}} with element \code{status}, one of
#' "skipped" (fewer than two predictor values), "failed" (the contrast could
#' not be computed) or "tested", and for "tested" the p-value, the two levels
#' and the mean at each.
#'
#' @importFrom stats glm relevel pf pchisq as.formula
#'
#' @noRd
flatness_contrast <- function(block, alpha = 0.05, pool_dispersion = TRUE) {
  spec <- block$spec
  x <- block$x
  ux <- sort(unique(x[is.finite(x)]))
  if (length(ux) < 2) {
    return(list(status = "skipped"))
  }
  upper <- ux[length(ux)]
  lower <- ux[length(ux) - 1]
  # The dispersion is estimated from the two levels being compared, because for
  # every family here but gaussian the variance depends on the mean: one
  # estimated over a whole declining series is pulled up by the high-mean
  # levels and overstates the variance at the top, which is where the rule has
  # to work. bnec() fits a single sigma for a gaussian response unless a disp()
  # term is supplied, so pooling there is the assumption the fit itself makes,
  # and the caller passes in which of the two it is.
  pool <- isTRUE(pool_dispersion) && identical(spec$family$family, "gaussian")
  keep <- if (pool) is.finite(x) else x %in% c(lower, upper)
  if (identical(spec$kind, "matrix")) {
    keep <- keep & is.finite(block$trials) & is.finite(block$successes) &
      block$trials > 0
    response <- cbind(block$successes[keep],
                      block$trials[keep] - block$successes[keep])
  } else {
    keep <- keep & is.finite(block$y)
    response <- block$y[keep]
  }
  offset_term <- rep(0, sum(keep))
  if (isTRUE(spec$offset) && !is.null(block$denominator)) {
    denominator <- block$denominator[keep]
    if (any(!is.finite(denominator) | denominator <= 0)) {
      return(list(status = "failed"))
    }
    # An offset rather than a divided response, which keeps the counts integral
    # and puts the contrast on the rate scale. #389 refuses a rate() written as
    # an expression, so the denominator is a bare column and this is the same
    # exposure the fit uses.
    offset_term <- log(denominator)
  }
  x_keep <- x[keep]
  if (!all(c(lower, upper) %in% x_keep)) {
    return(list(status = "failed"))
  }
  # Only the binomial row holds its dispersion fixed. The count rows are mapped
  # to quasipoisson, so poisson never reaches here as itself.
  estimates_dispersion <- !identical(spec$family$family, "binomial")
  ux_keep <- sort(unique(x_keep))
  replicated <- max(tabulate(match(x_keep, ux_keep))) > 1
  # A binomial or beta-binomial design with one row per concentration falls
  # back to a fixed dispersion rather than being passed over.
  #
  # Those two rows are mapped to quasibinomial, whose dispersion needs
  # replication, so the replication rule below would pass over one composite
  # sample per dilution with twenty individuals scored in it --- the ordinary
  # whole effluent binomial layout, and the design this rule exists for. The
  # identical counts reaching this function as the survival block of a hurdle
  # fit are tested, so the same data would give two answers depending on which
  # family declared them.
  #
  # The denominator is known here, so the information is the individuals rather
  # than the replication, exactly as it is for the hu block. The fallback
  # trades the over-dispersion estimate for a test that exists at all, which is
  # the same trade the hu block makes for the same reason. Where replication is
  # present quasibinomial is kept, because over-dispersion between replicate
  # vessels is real and worth accounting for.
  #
  # One consequence is stated rather than left implicit: a beta_binomial
  # response with no replication is tested under a model that assumes no
  # over-dispersion, on a family whose declaration says to expect it, so the
  # test is anti-conservative there, and by more than the fallback's other
  # rows. Measured on a flat top, four levels of twenty trials, one row at
  # each, 4000 replicates: binomial reports on 0.058 and beta_binomial at an
  # intra-class correlation of 0.1 on 0.184, against the nominal 0.05. The
  # alternative was to pass the design over, which reports on none of the
  # designs that are genuinely incomplete, and a rate of 0.184 on a message
  # whose only consequence is to prompt the user is the better of the two. It
  # is stated in NEWS and is an audit item for #391 beside the varying-trials
  # rate.
  if (estimates_dispersion && identical(spec$kind, "matrix") && !replicated) {
    spec$family <- binomial()
    estimates_dispersion <- FALSE
  }
  # A block carrying too little information at the two levels is passed over in
  # silence, and what counts as information differs by family.
  #
  # Where the dispersion is estimated it is replication: a factor fit with one
  # observation per predictor value is saturated, so no dispersion can be
  # estimated and no contrast is defined. That is the ordinary shape of a
  # continuous predictor rather than a defect in the design --- the package's
  # own nec_data has 100 distinct predictor values in 100 rows, and every
  # vignette fits it --- so reporting would put an advisory on the documented
  # example of the package.
  #
  # Where it is fixed it is the number of individuals behind each level. One
  # observation per level is informative where each carries twenty individuals
  # and is not where each carries one: two single Bernoulli trials reading
  # (1, 0) give a deviance p of 0.048, against an exact conditional p of a
  # half, and an unreplicated bernoulli design reported on a fifth of flat
  # tops. Two individuals are required at each of the two levels, which a
  # dilution series of ten or twenty passes and one individual per
  # concentration does not.
  #
  # Section 2.2 of the plan asks for a report on an unreplicated design. It was
  # written for a designed series, and an advisory raised on the commonest call
  # there is teaches users to ignore the rule, which is the failure the rule
  # exists to avoid. What section 2.2 wants protected is the degenerate case
  # below, where the information is present and every observation is identical.
  #
  # tabulate(match(...)) rather than table(): table() builds its factor through
  # as.character(), which is both the rounding the comment below rules out and
  # 18 times slower on a 200,000-value predictor.
  thin <- if (estimates_dispersion) {
    !replicated
  } else {
    individuals <- function(value) {
      at <- x_keep == value
      if (identical(spec$kind, "matrix")) {
        sum(block$trials[keep][at])
      } else {
        sum(at)
      }
    }
    individuals(lower) < 2 || individuals(upper) < 2
  }
  if (thin) {
    return(list(status = "skipped"))
  }
  ref <- which(ux_keep == lower)
  # The levels of a factor built from a numeric vector are its sorted unique
  # values, so the coefficient wanted is found by position and never by
  # matching a label that as.character() has rounded.
  level <- relevel(factor(x_keep, levels = ux_keep), ref = ref)
  index <- 1L + which(ux_keep[-ref] == upper)
  merged <- x_keep
  merged[merged == upper] <- lower
  fit_data <- data.frame(level = level, merged = factor(merged),
                         offset_term = offset_term)
  fit_data$response <- response
  full <- try(suppressWarnings(
    glm(response ~ level + offset(offset_term), family = spec$family,
        data = fit_data)
  ), silent = TRUE)
  reduced_rhs <- if (nlevels(fit_data$merged) > 1) "merged" else "1"
  reduced <- try(suppressWarnings(
    glm(as.formula(paste0("response ~ ", reduced_rhs, " + offset(offset_term)")),
        family = spec$family, data = fit_data)
  ), silent = TRUE)
  if (inherits(full, "try-error") || inherits(reduced, "try-error") ||
      !isTRUE(full$converged) || !isTRUE(reduced$converged)) {
    return(list(status = "failed"))
  }
  coefs <- summary(full)$coefficients
  if (nrow(coefs) < index || !is.finite(coefs[index, 1])) {
    return(list(status = "failed"))
  }
  # Where no observation differs from another at its own predictor value the
  # dispersion is zero, and the contrast is then either exactly zero or
  # infinitely significant. Tested on the observations rather than on the
  # estimate, because the estimate of a zero dispersion is a rounding residue
  # -- 1.8e-32 on a constant response, which no comparison against zero
  # catches. Families whose dispersion is fixed are exempt: the variance of a
  # binomial count is fixed by its mean and its number of trials, so a standard
  # error is defined there with one observation per level.
  if (estimates_dispersion) {
    working <- if (identical(spec$kind, "matrix")) {
      block$successes[keep] / block$trials[keep]
    } else {
      block$y[keep]
    }
    varies <- vapply(split(working, x_keep), function(v) any(v != v[1]),
                     logical(1))
    if (!any(varies)) {
      return(list(status = "failed"))
    }
  }
  change <- reduced$deviance - full$deviance
  if (!is.finite(change)) {
    return(list(status = "failed"))
  }
  change <- max(change, 0)
  if (!estimates_dispersion) {
    two_sided <- pchisq(change, df = 1, lower.tail = FALSE)
  } else {
    dispersion <- summary(full)$dispersion
    if (!is.finite(dispersion) || dispersion <= 0 || full$df.residual < 1) {
      return(list(status = "failed"))
    }
    two_sided <- pf(change / dispersion, df1 = 1, df2 = full$df.residual,
                    lower.tail = FALSE)
  }
  # One-sided from the signed root of a one-degree-of-freedom statistic, so the
  # rule reports a decline and is silent on a rise.
  p_value <- if (coefs[index, 1] < 0) two_sided / 2 else 1 - two_sided / 2
  level_mean <- function(value) {
    at <- block$x == value
    if (identical(spec$kind, "matrix")) {
      sum(block$successes[at]) / sum(block$trials[at])
    } else if (isTRUE(spec$offset) && !is.null(block$denominator)) {
      sum(block$y[at]) / sum(block$denominator[at])
    } else {
      mean(block$y[at])
    }
  }
  list(status = "tested", declining = p_value < alpha, p_value = p_value,
       lower_x = lower, upper_x = upper,
       lower_mean = level_mean(lower), upper_mean = level_mean(upper))
}

#' Name a block of a response in the flatness report
#'
#' @param block "response" or "survival".
#' @param family The validated response family.
#' @param level The group level.
#' @param named_levels Whether the level is named in the report.
#'
#' @return A \code{\link[base]{character}} string.
#'
#' @noRd
flatness_label <- function(block, family, level, named_levels) {
  base <- if (!is_hurdle_family(family)) {
    "the response"
  } else if (identical(block, "survival")) {
    "the survival block"
  } else {
    "the response block"
  }
  if (named_levels) {
    paste0(base, " of level \"", level, "\"")
  } else {
    base
  }
}

#' Name the quantity a block's reported mean is a mean of
#'
#' The model frame holds the predictor and the response as the formula wrote
#' them, so a fit on \code{crf(log(concentration))} reports log concentrations.
#' Naming the expression is what stops a log concentration being read as a
#' concentration, which is the trap \code{bayesnec/CLAUDE.md} records for the
#' estimators.
#'
#' @param block One element of \code{\link{flatness_blocks}}.
#' @param y_label The response as the formula wrote it.
#'
#' @return A \code{\link[base]{character}} string.
#'
#' @noRd
flatness_mean_label <- function(block, y_label) {
  if (identical(block$spec$kind, "matrix")) {
    "proportion"
  } else if (isTRUE(block$spec$offset) && !is.null(block$denominator)) {
    "rate"
  } else {
    y_label
  }
}

#' The predictor and response as the formula wrote them
#'
#' @param data A model frame for a \code{\link{bayesnecformula}}.
#' @param var \code{"x_var"} or \code{"y_var"}.
#' @param fallback What to return where the attribute is absent.
#'
#' @return A \code{\link[base]{character}} string.
#'
#' @noRd
pop_var_label <- function(data, var, fallback) {
  pop_vars <- attr(data, "bnec_pop")
  if (is.null(pop_vars)) {
    return(fallback)
  }
  # The column name of the model frame, not the entry of bnec_pop. bnec_pop
  # records the bare variable, "x", while the column is named for the term the
  # formula wrote, "log(x)", and it is the term that says which scale the
  # numbers reported beside it are on. make_brmsformula() reads the name the
  # same way.
  position <- which(names(pop_vars) == var)
  if (length(position) != 1) {
    return(fallback)
  }
  label <- names(data)[position]
  if (!is.character(label) || length(label) != 1 || is.na(label) ||
      !nzchar(label)) {
    fallback
  } else {
    label
  }
}

#' Capitalise the first letter of a sentence
#'
#' The block labels are noun phrases, so that one reads as a sentence subject
#' and the same string reads inside a list. Only the first character changes.
#'
#' @param x A \code{\link[base]{character}} string.
#'
#' @return A \code{\link[base]{character}} string.
#'
#' @noRd
capitalise_first <- function(x) {
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}

#' Report a response still declining at the top of the tested series
#'
#' The default \code{bot} prior takes its location and spread from the observed
#' response, and the default \code{nec} and \code{ec50} priors take their
#' location and spread from the tested predictor range, so on a design whose
#' response is still falling at the highest concentration all three describe the
#' design rather than the curve. The condition is reported before anything is
#' fitted, because what the user does about it is a choice of prior. The two
#' threshold priors are no longer truncated to that range (#393), so a threshold
#' above the highest concentration is in the tail of the prior rather than
#' outside its support; the report still stands, because a tail is not a
#' statement that the design measured the threshold.
#'
#' The statistic is a one-sided contrast between the two highest distinct
#' predictor values present in a block, fitted as a generalised linear model
#' with the level as a two-valued factor and tested at \code{alpha}. The level
#' is stated rather than a multiple of a standard error, so that the number in
#' the code has a meaning: on a design whose top is flat the true contrast is
#' zero and the rule reports at \code{alpha}, which is its false-positive rate
#' by construction. The realised rate is measured in #391.
#'
#' The rule never reads the control, so a hormesis design, where the response
#' rises before it falls, is assessed against the top of its own series and
#' needs no special case, and a response on a log-ratio or centred scale whose
#' control mean is not positive is assessed like any other (#229).
#'
#' Each block is assessed against its own two highest levels. For a two-block
#' family the response block holds the individuals past the hurdle only, so
#' where none survived the highest concentration its two highest levels are not
#' the design's, and the levels used are named in the report.
#'
#' Called from \code{\link{bnec}} once before its model loop and from
#' \code{\link{bnec_group}} once before its level loop, so that it is reported
#' once per call rather than once per equation or once per level.
#'
#' @param data A model frame, as returned by \code{\link{model.frame}} for a
#' \code{\link{bayesnecformula}}.
#' @param family The validated response family.
#' @param group An optional factor or character vector defining independent
#' concentration-response series.
#' @param blocks Character vector naming the response blocks to assess, or a
#' named list giving them separately for each group level.
#' @param alpha The one-sided significance level of the contrast.
#' @param pool_dispersion Whether a gaussian dispersion is estimated over the
#' whole series. \code{TRUE} where the formula carries no \code{disp()} term,
#' which is the case in which \code{\link{bnec}} fits a single \code{sigma}.
#'
#' @return \code{NULL}, invisibly. Called for its message.
#'
#' @noRd
check_response_flattened <- function(data, family, group = NULL,
                                     blocks = c("response", "survival"),
                                     alpha = 0.05, pool_dispersion = TRUE) {
  y <- try(retrieve_var(data, "y_var", error = TRUE), silent = TRUE)
  x <- try(retrieve_var(data, "x_var", error = TRUE), silent = TRUE)
  if (inherits(y, "try-error") || inherits(x, "try-error")) {
    return(invisible(NULL))
  }
  trials <- retrieve_var(data, "trials_var")
  denominator <- retrieve_var(data, "rate_var")
  named_levels <- !is.null(group)
  group <- if (named_levels) {
    factor(group)
  } else {
    factor(rep("all data", length(y)))
  }
  if (length(group) != length(y)) {
    return(invisible(NULL))
  }
  x_label <- pop_var_label(data, "x_var", "the predictor")
  y_label <- pop_var_label(data, "y_var", "response")
  declining <- character(0)
  untestable <- character(0)
  for (level in levels(group)) {
    use <- group == level & is.finite(x) & is.finite(y)
    level_blocks <- if (is.list(blocks)) blocks[[level]] else blocks
    if (is.null(level_blocks) || !isTRUE(any(use))) {
      next
    }
    views <- flatness_blocks(x[use], y[use], trials[use], denominator[use],
                             family)
    views <- views[intersect(names(views), level_blocks)]
    for (name in names(views)) {
      result <- flatness_contrast(views[[name]], alpha = alpha,
                                  pool_dispersion = pool_dispersion)
      label <- flatness_label(name, family, level, named_levels)
      if (identical(result$status, "failed")) {
        untestable <- c(untestable, label)
      } else if (isTRUE(result$declining)) {
        declining <- c(
          declining,
          paste0(
            capitalise_first(label),
            " is still declining at the top of the series: the mean ",
            flatness_mean_label(views[[name]], y_label), " falls from ",
            signif(result$lower_mean, 3), " to ",
            signif(result$upper_mean, 3), " as ", x_label, " rises from ",
            signif(result$lower_x, 3), " to ", signif(result$upper_x, 3),
            " (p = ", signif(result$p_value, 2), ", one-sided)."
          )
        )
      }
    }
  }
  if (length(declining) > 0) {
    message(
      paste(declining, collapse = "\n"), "\n",
      "The lower asymptote may not be identified by this design: where an",
      " equation estimates bot its default prior is derived from the observed",
      " response, and the default nec and ec50 priors put a threshold above the",
      " tested range in their tail, where how much of the prior lies there",
      " depends on prior_type and on whether the predictor was supplied as a",
      " concentration or already logged; ?bnec tabulates the four under",
      " prior_type.",
      " Inspect the entries with get_priors() and",
      " supply scientifically justified ones through the prior argument where",
      " information beyond the design is available.",
      declaration_advice(family, y)
    )
  }
  if (length(untestable) > 0) {
    message(
      "Whether the response has flattened at the top of the series could not",
      " be assessed for ", paste(untestable, collapse = ", "), ". The",
      " contrast between the two highest predictor values has no standard",
      " error there: every observation at those values is identical, or the",
      " exposure is not positive, or the model fitting the contrast did not",
      " converge."
    )
  }
  invisible(NULL)
}

#' Does the fit still use a response-range-sensitive default prior?
#'
#' A supplied prior can be partial: \code{fill_missing_priors()} adds defaults
#' for parameter rows it does not replace. The flatness report is therefore
#' suppressed only where every response-range-sensitive row is supplied for
#' every equation that will be fitted.
#'
#' @param prior A \code{\link[brms]{brmsprior}} or named list of them.
#' @param models Character vector of concrete model names.
#' @param family The validated response family.
#' @param model_survival Optional equation for a hurdle survival block.
#' @param affected The parameter rows to read. The default is the three the
#' flatness report is raised for; \code{\link{check_asymptote_declaration}}
#' passes \code{"bot"} alone, because that is the only row the declaration
#' generates.
#'
#' @return A named logical vector, one element per response block.
#'
#' @noRd
uses_response_range_defaults <- function(prior, models, family,
                                         model_survival = NULL,
                                         affected = c("bot", "nec", "ec50")) {
  hurdle <- is_hurdle_family(family)
  out <- setNames(rep(FALSE, if (hurdle) 2L else 1L),
                  if (hurdle) c("response", "survival") else "response")
  for (model in models) {
    supplied <- if (inherits(prior, "brmsprior")) {
      prior
    } else if (is.list(prior) && model %in% names(prior)) {
      prior[[model]]
    } else {
      NULL
    }
    required <- list(
      response = intersect(equation_par_names(model), affected)
    )
    if (hurdle) {
      survival_model <- if (is.null(model_survival)) model else model_survival
      required$survival <- paste0(
        hurdle_dpar(family),
        intersect(equation_par_names(survival_model), affected)
      )
    }
    if (inherits(supplied, "brmsprior")) {
      prior_df <- as.data.frame(supplied)
      dpar <- if ("dpar" %in% names(prior_df)) prior_df$dpar else ""
      present <- prior_df$nlpar[
        prior_df$class == "b" & (is.na(dpar) | !nzchar(dpar))
      ]
    } else {
      present <- character(0)
    }
    for (block in names(required)) {
      if (length(required[[block]]) > 0L &&
          !all(required[[block]] %in% present)) {
        out[[block]] <- TRUE
      }
    }
  }
  out
}

#' The sentence naming the declaration, where the declaration is available
#'
#' @details \code{asymptote_observed = FALSE} needs a floor, and there are
#' responses for which none can be derived. Recommending it for one of those
#' would send the user to a hard error, so the sentence is omitted there and
#' the report ends on the prior advice that precedes it.
#'
#' The floor is read from the response as supplied rather than from the link
#' scale, which is what \code{\link{asymptote_floor}} expects. The two agree:
#' the only branch that reads the response is the gaussian one, where an
#' identity link leaves \code{\link{response_link_scale}} a no-op, and every
#' other tag returns zero without reading it. A two-block family is passed as
#' the joint family for the same reason --- its mu family has the same link, and
#' the link is what decides the answer for a non-identity fit.
#'
#' @param family The validated response family.
#' @param y The response as supplied.
#'
#' @return A \code{\link[base]{character}} string, empty where no floor can be
#' derived.
#'
#' @noRd
declaration_advice <- function(family, y) {
  if (!is.finite(asymptote_floor(family, y))) {
    return("")
  }
  paste0(" Where the design did stop before the asymptote, declare it with",
         " asymptote_observed = FALSE, which spans the bot prior from the",
         " floor of the response to the mean at the highest predictor level",
         " and widens the initial-value search to match.")
}

#' Act on a declaration that the lower asymptote was not observed
#'
#' \code{asymptote_observed = FALSE} needs a floor --- the smallest value the
#' mean can take --- and there are responses for which none can be derived. This
#' refuses those, and reports a model set that mixes equations estimating a
#' lower asymptote with equations asserting the response falls to that floor.
#'
#' Both are properties of the data, the family and the model set together, fixed
#' for the whole call, so they are raised here and called once from
#' \code{\link{bnec}} before its model loop and once from
#' \code{\link{bnec_group}} before its level loop. \code{\link{define_prior}}
#' keeps the refusal as the backstop for \code{\link{get_priors}} and
#' \code{\link{amend}}, which do not come through either. The placement follows
#' \code{\link{check_complete_cases}}, and matters for the same reason: from
#' inside the model loop the refusal is printed once per equation and the call
#' then ends on the generic all-models-failed advice, and from inside the level
#' loop it arrives only after the earlier levels have compiled and sampled.
#'
#' The response is built the way \code{fit_bayesnec()} builds it --- divided by
#' the trials or the \code{rate()} denominator, then put on the link scale ---
#' so the floor is derived from the quantity the prior will be derived from.
#' It is read from the model frame, before \code{\link{check_data}} has nudged a
#' zero or a one off a boundary, and the two agree because a nudge leaves a
#' value within its own support and the floor depends on the sign of the response and
#' on the link alone. Both divisors are positive wherever the fit is meaningful,
#' so neither changes that sign either.
#'
#' The model-set report states both branches, because the data decide neither.
#' The region that would separate an equation estimating \code{bot} from one
#' with no lower asymptote to estimate is the region the design did not reach,
#' so a model average over both is averaging over the assumption at issue with
#' weights the data cannot inform. Whether the response can reach zero is a
#' property of the endpoint, which the user knows and the package cannot infer,
#' so the set is reported and left alone.
#'
#' @param data A model frame, as returned by the \code{\link{model.frame}}
#' method for a \code{\link{bayesnecformula}}.
#' @param family The validated response family.
#' The model-set report is raised whatever prior was supplied, because the set
#' and the design are what decide it. The two refusals are raised only where a
#' \code{bot} prior will be generated from the declaration.
#' \code{\link{add_brm_defaults}} wraps the default-prior build in
#' \code{try()} so that a user who supplied a complete set is never blocked by
#' a default they will not use (#207, #229), and a check raised before the loop
#' would take that back. So the refusal is gated on
#' \code{\link{uses_response_range_defaults}} reading the \code{bot} row
#' alone, the same test that gates the flatness report above it. The
#' initial-value band is unaffected either way: with no floor to extend it to,
#' \code{\link{make_good_inits}} leaves it where it was.
#'
#' @param models A \code{\link[base]{character}} vector of concrete equation
#' names.
#' @param asymptote_observed The declaration, as passed to \code{\link{bnec}}.
#' @param prior The prior the caller supplied, or \code{NULL}.
#' @param model_survival The equation for a hurdle survival block, or
#' \code{NULL}.
#'
#' @return \code{NULL}, invisibly. Called for its error and its message.
#'
#' @noRd
check_asymptote_declaration <- function(data, family, models,
                                        asymptote_observed = TRUE,
                                        prior = NULL,
                                        model_survival = NULL) {
  if (isTRUE(asymptote_observed)) {
    return(invisible(NULL))
  }
  # Reported before the gate below and not behind it. Whether a set mixes
  # equations that estimate a lower asymptote with ones that do not is a
  # property of the set and of the design, and supplying a bot prior does not
  # make the region that would separate them observed. The gate exists for the
  # refusals alone.
  #
  # The message says the fourteen have no lower asymptote to estimate, which is
  # what having no bot parameter means, rather than that they fall to zero.
  # Eleven of them do fall to zero -- nec3param is top * exp(-exp(beta)
  # (x - nec)) -- but neclin, neclinhorme and ecxlin decay by subtraction and
  # are unbounded below, which ?models records at R/models.R. check_models()
  # keeps all three for a gaussian family, which is the branch where the floor
  # comes from the response being non-negative, so the claim would be false on
  # the common path and the advice that follows it would be acted on.
  #
  # The second side is read off each equation's parameters rather than taken
  # as the complement of the group. ecxflat has no bot and belongs to no group,
  # so the complement counted it as estimating a lower asymptote; and it does
  # not belong on the first side either, because a constant is told from a
  # declining curve whether or not the asymptote was observed. For the 23
  # equations of mod_groups$all the two readings agree. See #419.
  bot_free <- intersect(models, mod_groups$bot_free)
  bot_est <- models[vapply(models, function(m) {
    "bot" %in% equation_par_names(m)
  }, logical(1))]
  if (length(bot_free) > 0 && length(bot_est) > 0) {
    message(
      "This set mixes equations that estimate a lower asymptote with ones that",
      " have no lower asymptote to estimate. With the asymptote unobserved the",
      " fit cannot distinguish them. Use `model = \"zero_bounded\"` if the",
      " response can reach zero for this endpoint; otherwise, exclude the",
      " equations in `models()$bot_free`."
    )
  }
  supplied <- length(models) > 0 &&
    !any(uses_response_range_defaults(prior, models, family, model_survival,
                                      affected = "bot"))
  if (supplied) {
    return(invisible(NULL))
  }
  y <- try(retrieve_var(data, "y_var", error = TRUE), silent = TRUE)
  if (inherits(y, "try-error")) {
    return(invisible(NULL))
  }
  if (family$family %in% c("binomial", "beta_binomial")) {
    trials <- retrieve_var(data, "trials_var")
    if (!is.null(trials)) {
      y <- y / trials
    }
  }
  denominator <- retrieve_var(data, "rate_var")
  if (!is.null(denominator)) {
    y <- y / denominator
  }
  # A hurdle or zero-inflated fit primes its mu block from the survivors, which
  # is the subset define_hurdle_prior() reads, so the floor is derived from the
  # same subset and on the same family.
  floor_family <- if (is_hurdle_family(family)) {
    hurdle_mu_family(family)
  } else {
    family
  }
  x <- try(retrieve_var(data, "x_var", error = TRUE), silent = TRUE)
  if (inherits(x, "try-error")) {
    return(invisible(NULL))
  }
  if (is_hurdle_family(family)) {
    # The split is what makes this block do any work: the mu subset drops the
    # structural zeros, so its endpoint mean differs from the whole response's
    # and is the one define_hurdle_prior() reads.
    parts <- split_hurdle_response(x, y)
    x <- parts$mu$x
    y <- parts$mu$y
  }
  response <- try(response_link_scale(y, floor_family), silent = TRUE)
  if (inherits(response, "try-error")) {
    return(invisible(NULL))
  }
  floor_val <- asymptote_floor(floor_family, response)
  if (!is.finite(floor_val)) {
    stop(asymptote_floor_error(no_floor_reason(floor_family, response)),
         call. = FALSE)
  }
  # The second refusal unobserved_endpoint_mean() can raise, hoisted here for
  # the same reason as the first: from inside define_prior() it is printed once
  # per equation and the call then ends on the generic all-models-failed
  # advice. Reached only on a response with no value above the floor at all,
  # which for a gaussian response is every observation at zero.
  invisible(unobserved_endpoint_mean(x, response,
                                     zero_bounded_family(floor_family)))
  invisible(NULL)
}

#' Refuse a model frame from which incomplete cases were removed
#'
#' \code{stats::model.frame()} drops an incomplete case before \pkg{bayesnec}
#' sees the data, so an \code{NA} or \code{NaN} never reached the finiteness
#' guard in \code{\link{check_data}} and the fit ran on fewer rows than were
#' supplied with nothing said. Refused rather than announced: \code{Inf} is
#' refused already, and the sample the estimates are derived from is not
#' something the package should change silently.
#'
#' The rows removed are recorded on the model frame by
#' \code{\link[stats]{na.omit}}, which is the only remaining evidence that
#' they existed. They are reported by row name rather than by position: a name
#' is what the user sees in their own data frame, and where the model frame was
#' built from a subset --- one level of a \code{\link{bnec_group}} call --- a
#' position indexes the subset and names no row of the data that was supplied.
#'
#' @param data A model frame, as returned by the \code{\link{model.frame}}
#' method for a \code{\link{bayesnecformula}}.
#'
#' @return \code{NULL}, invisibly. Called for its error.
#'
#' @noRd
check_complete_cases <- function(data) {
  dropped <- attr(data, "na.action")
  if (is.null(dropped)) {
    return(invisible(NULL))
  }
  rows <- names(dropped)
  if (is.null(rows)) {
    rows <- as.character(unname(dropped))
  }
  stop("Your data contains ", length(dropped), " row(s) with missing values",
       " (NA or NaN), at row(s) ", paste0(rows, collapse = ", "),
       ". Every variable the formula names must be complete; remove or impute",
       " those rows before fitting.", call. = FALSE)
}

#' Does every dispersion sub-model term evaluate to finite values?
#'
#' \code{check_data()} tests the predictor and the response for finiteness and
#' names the column when either fails, but it inspects only the population
#' variables \code{crf()} declares. A \code{disp(~...)} term is an arbitrary
#' \pkg{brms} formula evaluated against the user's data frame, and its
#' variables are deliberately kept out of the model frame -- \code{brm()} is
#' handed the full data frame and resolves them itself -- so nothing tested
#' them and an infinite value reached Stan. The fit then did not run, reporting
#' a \pkg{brms} warning about the data in general with nothing naming the term
#' responsible. See #271.
#'
#' Raised from \code{\link{bnec}} and \code{bnec_group()} rather than from
#' \code{\link{check_data}}, which is the placement rule for a refusal that is
#' a property of the data and the formula together. Here there is a second
#' reason: \code{check_data()} is given the model frame, which by design does
#' not contain the dispersion sub-model's columns, so it could not perform this
#' check even if it were the right place for it.
#'
#' Only terms that evaluate to a numeric vector are tested. A smooth,
#' \code{disp(~s(x))}, evaluates to a specification object rather than to
#' numbers and is left to \pkg{brms}.
#'
#' @param formula A \code{\link{bayesnecformula}}.
#' @param data A \code{\link[base]{data.frame}}, the one the user supplied.
#'
#' @return \code{NULL}, invisibly. Called for the error.
#'
#' @importFrom stats as.formula terms
#' @noRd
check_disp_finite <- function(formula, data) {
  # parse_disp_term()'s own refusals -- more than one disp() term, and a term
  # it cannot parse -- are raised from here rather than swallowed by a try().
  # Suppressing them did not avoid the error, it deferred it to
  # add_brm_defaults(), which runs inside the per-model try() in bnec(), so one
  # malformed formula printed the refusal once for every member of the model
  # set and ended on the generic all-models-failed advice. That is the pattern
  # this check exists to remove.
  disp_spec <- parse_disp_term(formula)
  if (is.null(disp_spec) || !identical(disp_spec$route, "A")) {
    return(invisible(NULL))
  }
  # parse_disp_term() returns the right-hand side as an expression string
  # ("log(x)"), not as a formula string, so the tilde is added here. Without it
  # as.formula() errors and the check silently passed everything.
  disp_formula <- try(as.formula(paste("~", disp_spec$value)), silent = TRUE)
  if (inherits(disp_formula, "try-error")) {
    return(invisible(NULL))
  }
  labels <- attr(terms(disp_formula), "term.labels")
  bad <- character(0)
  incomplete <- character(0)
  for (label in labels) {
    # enclos is the formula's own environment. With eval()'s default of
    # parent.frame(), a term written with a function the user defined --
    # disp(~cent(x)) -- could not be evaluated, and the try() below then
    # skipped it, so the check silently passed rather than checking anything.
    # The skip is kept for a term that genuinely cannot be evaluated here, such
    # as a smooth. See #319.
    values <- try(eval(str2lang(label), envir = data,
                       enclos = formula_env(formula)), silent = TRUE)
    if (inherits(values, "try-error") || !is.numeric(values)) {
      next
    }
    # NA and Inf are separated, and tested independently so a term with both
    # is reported for both. They have different causes and different remedies,
    # and check_complete_cases() cannot report the NA because it is given the
    # model frame, which by design does not contain the dispersion sub-model's
    # columns. Reporting a missing value as non-finite named log() of a zero
    # as the usual cause, which is the wrong diagnosis.
    if (anyNA(values)) {
      incomplete <- c(incomplete, label)
    }
    if (!all(is.finite(values[!is.na(values)]))) {
      bad <- c(bad, label)
    }
  }
  msgs <- character(0)
  if (length(incomplete) > 0) {
    msgs <- c(msgs, paste0(
      "The dispersion sub-model term(s) ",
      paste0("\"", incomplete, "\"", collapse = "; "),
      " contain missing values on your data. A dispersion sub-model's",
      " variables are deliberately kept out of the model frame, so",
      " check_data()'s complete-cases check does not see them and brms is",
      " handed the NA. Drop or impute those rows before fitting."
    ))
  }
  if (length(bad) > 0) {
    msgs <- c(msgs, paste0(
      "The dispersion sub-model term(s) ",
      paste0("\"", bad, "\"", collapse = "; "),
      " evaluate to values that are not finite on your data. A dispersion",
      " sub-model is passed to brms unchanged, so this reaches Stan and the",
      " fit does not run. Check the term against the columns it names --",
      " log() of a zero and division by a zero are the usual causes."
    ))
  }
  if (length(msgs) > 0) {
    stop(paste(msgs, collapse = " "), call. = FALSE)
  }
  invisible(NULL)
}

#' Refuse a data column whose name collides with a generated parameter
#'
#' The group-level transform (#257) introduces an intermediate non-linear term
#' named \code{bnecmu}, \code{ogl()} introduces one named \code{ogl}, and the
#' parameter-level transform (#294) introduces \code{topgl}, \code{botgl},
#' \code{bnectop} and \code{bnecbot}. All are resolved by \pkg{brms} against
#' the user's data frame, so a column of any of those names would be silently
#' preferred over the generated term and the fit would be a different model with
#' no error. Refused by name here rather than left to produce a confusing
#' \pkg{brms} message about a formula the user did not write.
#'
#' The set is refused in full whatever the formula is, rather than only where
#' the term would actually be generated. Which terms a fit generates depends on
#' the family and on the group-level structure, so a conditional refusal would
#' accept a column on one call and refuse it on the next with the same data.
#'
#' @param data A \code{\link[base]{data.frame}}, the one the user supplied.
#'
#' @return \code{NULL}, invisibly. Called for the error.
#' @noRd
check_reserved_names <- function(data) {
  reserved <- generated_term_names()
  clash <- intersect(reserved, names(data))
  if (length(clash) > 0) {
    stop("Your data contains the column(s) ",
         paste0("\"", clash, "\"", collapse = "; "),
         ", which bayesnec generates as model terms. brms resolves formula",
         " terms against your data first, so a column of that name would be",
         " used in place of the generated term and the fit would silently be a",
         " different model. Rename the column(s) before fitting.",
         call. = FALSE)
  }
  invisible(NULL)
}

#' check_data
#'
#' Check data input for a Bayesian NEC model fit
#'
#' @inheritParams bnec
#'
#' @param family A \code{\link[stats]{family}} function.
#'
#' @details This is a wrapper function to test input data criteria and find the
#' correct priors for use in \code{\link{fit_bayesnec}}.
#'
#' @return A \code{\link[base]{list}} of modified elements
#' necessary for \code{\link{fit_bayesnec}}.
#'
#' @noRd
check_data <- function(data, family, model) {
  y <- retrieve_var(data, "y_var", error = TRUE)
  x <- retrieve_var(data, "x_var", error = TRUE)
  bnec_pop_vars <- attr(data, "bnec_pop")
  y_pos <- which(names(bnec_pop_vars) == "y_var")
  x_pos <- which(names(bnec_pop_vars) == "x_var")
  # Kept here for the routes that do not come through bnec(): get_priors(),
  # which checks each model of the set in turn, and amend(), which refits from
  # the data frame an existing fit stores. bnec() and bnec_group() run it
  # before any model is fitted, for the reason given at those call sites.
  # See #278.
  check_complete_cases(data)
  # is.finite() elementwise rather than on the mean, so that a column reaching
  # this point with NA still present -- a user with options(na.action =
  # "na.pass"), for which check_complete_cases() sees nothing -- is named as
  # well.
  if (!all(is.finite(x))) {
    stop("Your predictor column contains values that are not finite.")
  }
  if (!all(is.finite(y))) {
    stop("Your response column contains values that are not finite.")
  }
  resp_check <- mean(y[which(x < mean(x))]) <
    mean(y[which(x > mean(x))])
  if (resp_check && !grepl("horme", model)) {
    warning("The mean value of the response column for the lower half of the ",
            "predictor column are lower than that of the upper half ",
            "of the predictor column. bnec only allows for ",
            "response values to decline with increasing values of predictor.")
  }
  fam_tag <- family$family
  # A censored response states that the truth lies in an interval whose bound is
  # the recorded value. The boundary nudges below assert a point instead, so
  # where the two meet the nudge would silently move the bound the user declared.
  # NULL when the formula carried no cens() term, in which case nothing changes.
  cens <- retrieve_cens(data)
  # Called for its errors, not its value: nothing here needs to know which
  # distribution describes the predictor, but this call rejects an integer one,
  # which reaches this point unchallenged because is.numeric() is TRUE for an
  # integer and retrieve_var() preserves the type. define_prior() makes the same
  # call and would raise the same error, so deleting this one would relocate the
  # error rather than remove it -- but it belongs here, in the documented data
  # check, rather than inside prior construction. See #269.
  set_distribution(x, silence_y_msgs = TRUE, silence_x_msgs = FALSE)
  # Families whose support is open at a boundary cannot express a censored
  # observation sitting exactly on it: the censored likelihood contribution is
  # F(0) = 0 on the left and 1 - F(1) = 0 on the right, so Stan sees log(0) and
  # initialisation fails with nothing informative to point at. Catch it here,
  # where the remedy can be named. Checked before the nudges below, so the
  # diagnostic does not depend on whether a nudge would have fired.
  if (fam_tag %in% c("Gamma", "hurdle_gamma", "beta", "zero_inflated_beta")) {
    check_cens_support(y, cens, bound = 0, direction = -1L, fam_tag = fam_tag)
  }
  if (fam_tag %in% c("beta", "zero_inflated_beta")) {
    check_cens_support(y, cens, bound = 1, direction = 1L, fam_tag = fam_tag)
  }
  # Runs here as a backstop for get_priors() and for a direct fit_bayesnec()
  # call. bnec() runs it once for the whole call, before the model loop, so that
  # a model set stops once rather than repeating the message per model.
  check_inline_boundary(data, family)
  # The corrections below concern the response alone. No family constrains the
  # values a predictor may take, so a zero concentration is an ordinary control
  # and reaches brm() as recorded; the predictor corrections that used to sit
  # here were written by symmetry with these rather than for a reason of their
  # own. See #269.

  # Which zeros are a boundary artefact to be nudged. A censored row is exempt
  # for the same reason a hurdle zero is: the value there is a declared bound,
  # not an artefact, and moving it would restate the bound the user chose.
  to_shift <- y == 0 & !is_censored(cens)
  # NB: this nudge must never apply to a hurdle family. There the zeros are the
  # hurdle signal, not a boundary problem -- moving them off zero would leave
  # the hu block with nothing to identify itself from. fam_tag is
  # "hurdle_gamma" rather than "Gamma" in that case, so the condition below
  # already excludes it; the guard is explicit so it survives refactoring.
  # The three nudges below are silent here and reported once from the
  # user-facing entry points instead -- bnec(), bnec_group(), get_priors() and
  # update(). check_data() runs once per model, so a message from here is
  # repeated for every member of a model set; the substitution itself is a
  # property of the data and the family, fixed for the whole call. This is the
  # placement check_normalisation() and check_inline_boundary() already use.
  # Two of the three were silent altogether, so a user comparing bayesnec
  # against another engine had no way to see that the data had been altered.
  # See #93 and D16.
  if (any(to_shift) & fam_tag == "Gamma" & !is_hurdle_family(fam_tag)) {
    min_val <- min(y[y > 0])
    data[to_shift, y_pos] <- y[to_shift] + (min_val / 10)
  }
  if (any(to_shift) & fam_tag == "beta") {
    min_val <- min(y[y > 0])
    data[to_shift, y_pos] <- y[to_shift] + (min_val / 10)
  }
  # A zero-inflated Beta keeps its zeros -- they are the signal -- but ones are
  # still outside Beta's open (0, 1) support and must be nudged as usual. A
  # right-censored one is exempt, as a left-censored zero is above.
  to_drop <- y == 1 & !is_censored(cens)
  if (any(to_drop) & (fam_tag == "beta" || fam_tag == "zero_inflated_beta")) {
    data[to_drop, y_pos] <- y[to_drop] - 0.001
  }
  mod_dat <- data.frame(x = data[[x_pos]], y = data[[y_pos]],
                        trials = nrow(data))
  if (fam_tag == "binomial" || fam_tag == "beta_binomial") {
    mod_dat$trials <- retrieve_var(data, "trials_var", error = TRUE)
  }
  rate_var <- retrieve_var(data, "rate_var")
  if (!is.null(rate_var)) {
    # Named `denom` rather than `rate`: it is the denominator of the rate, not
    # the rate itself, and the response divided by it is what the priors below
    # are built from.
    mod_dat$denom <- rate_var
  }
  list(mod_dat = mod_dat, family = family,
       substitutions = substitution_record(y, cens, family))
}

#' What check_data() substitutes in the response, and why
#'
#' The conditions mirror the three nudges in \code{\link{check_data}} exactly,
#' censoring exemptions included, so the record describes what was done rather
#' than restating the rule. Returned by \code{\link{check_data}}, reported
#' once by \code{report_substitutions()} at each user-facing entry point, and
#' stored on the fitted object so that a user comparing \pkg{bayesnec} against
#' another engine can recover what was altered. See #93.
#'
#' @param y The response, as read from the model frame.
#' @param cens The censoring indicator, or \code{NULL}.
#' @param family A \code{\link[stats]{family}}.
#'
#' @return A \code{\link[base]{data.frame}} with one row per substitution
#' rule that fired, or \code{NULL} where none did.
#' @noRd
substitution_record <- function(y, cens, family) {
  fam_tag <- family$family
  out <- list()
  to_shift <- y == 0 & !is_censored(cens)
  if (any(to_shift) && fam_tag %in% c("Gamma", "beta") &&
        !is_hurdle_family(fam_tag)) {
    min_val <- min(y[y > 0])
    out[["zero"]] <- data.frame(
      variable = "response", rule = "shifted off zero",
      n_rows = sum(to_shift), from = 0, to = min_val / 10,
      reason = paste0("a ", fam_tag,
                      " distribution cannot represent a zero"),
      remedy = if (identical(fam_tag, "Gamma")) {
        "hurdle_gamma()"
      } else {
        "zero_inflated_beta()"
      },
      stringsAsFactors = FALSE
    )
  }
  to_drop <- y == 1 & !is_censored(cens)
  if (any(to_drop) && fam_tag %in% c("beta", "zero_inflated_beta")) {
    out[["one"]] <- data.frame(
      variable = "response", rule = "shifted off one",
      n_rows = sum(to_drop), from = 1, to = 0.999,
      reason = "a beta distribution is defined on the open interval (0, 1)",
      remedy = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  if (length(out) == 0) {
    return(NULL)
  }
  do.call(rbind, out)
}

#' Report the response substitutions, once per call
#'
#' @param record The \code{substitutions} element of \code{check_data()}'s
#' return.
#' @param on_fit Does this caller return a fitted object that stores the
#' record? \code{TRUE} for \code{\link{bnec}} and \code{update()};
#' \code{FALSE} for \code{\link{get_priors}}, which returns priors, so
#' pointing the user at \code{?bnec_record} there would name something the
#' call does not produce.
#'
#' @return \code{NULL}, invisibly. Called for the message.
#' @noRd
report_substitutions <- function(record, on_fit = TRUE) {
  if (is.null(record) || nrow(record) == 0) {
    return(invisible(NULL))
  }
  where <- if (on_fit) {
    " The substitution is recorded on the fitted object; see ?bnec_record."
  } else {
    " The priors below are derived from the substituted response."
  }
  for (i in seq_len(nrow(record))) {
    r <- record[i, ]
    extra <- if (!is.na(r$remedy)) {
      paste0(" If those zeros are meaningful -- for example individuals that",
             " died -- consider family = ", r$remedy, " instead, which models",
             " them explicitly.")
    } else {
      ""
    }
    message("Your response contains ", r$n_rows, " value(s) at ", r$from,
            ", which cannot be fitted because ", r$reason,
            ". They have been shifted to ", signif(r$to, 3), ".", where, extra)
  }
  invisible(NULL)
}

#' Refuse a formula whose transformed response sits on a boundary its family
#' excludes
#'
#' A property of the data and the formula together, fixed for a whole
#' \code{\link{bnec}} call, so it is checked once there rather than once per
#' model. \code{\link{check_normalisation}} is hoisted out of
#' \code{\link{check_data}} for the same reason: a model set would otherwise
#' repeat the diagnostic for each of its members and end on the generic
#' all-models-failed advice, long after the cause.
#'
#' The conditions mirror the corrections in \code{\link{check_data}} exactly,
#' censoring exemptions included, because a value this refuses is precisely one
#' that would otherwise have been corrected.
#'
#' @noRd
check_inline_boundary <- function(data, family) {
  if (!pop_var_is_transformed(data, "y_var")) {
    return(invisible(NULL))
  }
  y <- try(retrieve_var(data, "y_var", error = TRUE), silent = TRUE)
  if (inherits(y, "try-error")) {
    return(invisible(NULL))
  }
  fam_tag <- if (inherits(family, "family")) family$family else family
  bnec_pop_vars <- attr(data, "bnec_pop")
  expr <- names(data)[which(names(bnec_pop_vars) == "y_var")]
  cens <- retrieve_cens(data)
  at_zero <- any(y == 0 & !is_censored(cens))
  at_one <- any(y == 1 & !is_censored(cens))
  if (at_zero & fam_tag == "Gamma" & !is_hurdle_family(fam_tag)) {
    stop_inline_boundary(expr, fam_tag, bound = 0,
                         hint = paste0(" If those zeros are meaningful -- for",
                                       " example individuals that died --",
                                       " consider family = hurdle_gamma()",
                                       " instead, which models them",
                                       " explicitly."))
  }
  if (at_zero & fam_tag == "beta") {
    stop_inline_boundary(expr, fam_tag, bound = 0)
  }
  if (at_one & (fam_tag == "beta" || fam_tag == "zero_inflated_beta")) {
    stop_inline_boundary(expr, fam_tag, bound = 1)
  }
  invisible(NULL)
}

#' The bound at which every observation of a bounded response sits
#'
#' @param y The response, as read from the model frame.
#' @param trials The number of trials of a \code{binomial} or
#' \code{beta_binomial} response, or \code{NULL} for a response recorded as a
#' proportion.
#'
#' @return 0 or 1, the bound every observation sits at, or \code{NA} where the
#' response is not at one bound throughout.
#' @noRd
response_bound_reached <- function(y, trials = NULL) {
  if (length(y) == 0) {
    return(NA_real_)
  }
  if (all(y == 0)) {
    return(0)
  }
  # Compared as counts against their own trials rather than as y / trials
  # against 1, so that the test is exact for integer counts and does not
  # depend on how a division rounds.
  upper <- if (is.null(trials)) 1 else trials
  if (all(y == upper)) {
    return(1)
  }
  NA_real_
}

#' The bound a bounded response sits at in every observation
#'
#' The reading \code{\link{check_response_at_bound}},
#' \code{constant_fallback()} and \code{\link{bnec_group}} share, so that the
#' decision to refuse such a response and the decision to fit the constant
#' equation to it are taken on the same test.
#'
#' @inheritParams check_response_at_bound
#'
#' @return \code{NULL} where the family has no bound or the response cannot be
#' read. Otherwise a \code{\link[base]{list}} holding the family tag, the name
#' of the response column, whether the response is counted against its trials,
#' and \code{bounds}: 0, 1 or \code{NA} for the whole response, or one such
#' value per level of \code{group}, named by level.
#' @noRd
response_bound_state <- function(data, family, group = NULL) {
  fam_tag <- if (inherits(family, "family")) family$family else family
  if (!fam_tag %in% c("bernoulli", "binomial", "beta_binomial", "beta")) {
    return(NULL)
  }
  y <- try(retrieve_var(data, "y_var", error = TRUE), silent = TRUE)
  if (inherits(y, "try-error")) {
    return(NULL)
  }
  trials <- NULL
  if (fam_tag %in% c("binomial", "beta_binomial")) {
    trials <- retrieve_var(data, "trials_var")
    # A binomial response with no trials() term is refused by check_data().
    # Without the trials there is no upper bound to test against, so nothing
    # is said here and that refusal is left to arrive.
    if (is.null(trials)) {
      return(NULL)
    }
  }
  bnec_pop_vars <- attr(data, "bnec_pop")
  y_name <- names(data)[which(names(bnec_pop_vars) == "y_var")]
  bounds <- if (is.null(group)) {
    response_bound_reached(y, trials)
  } else {
    rows <- split(seq_along(y), group, drop = TRUE)
    vapply(rows, function(i) {
      response_bound_reached(y[i], trials[i])
    }, numeric(1))
  }
  list(fam_tag = fam_tag, y_name = y_name, counted = !is.null(trials),
       bounds = bounds)
}

#' Whether the constant equation can be fitted at a bound
#'
#' It can at every bound but one. A \code{beta} distribution cannot represent
#' a zero, and \code{\link{check_data}} shifts a zero off the boundary by a
#' tenth of the smallest positive observation, which a response of 0 in every
#' observation does not have: the shift is \code{Inf}. Its ones are shifted to
#' 0.999, which needs no other observation, so a \code{beta} response of 1 in
#' every observation can be fitted. The discrete families are fitted at either
#' bound as recorded.
#'
#' @param fam_tag The family tag.
#' @param bound 0 or 1.
#'
#' @return A \code{\link[base]{logical}} of length 1.
#' @noRd
constant_fits_bound <- function(fam_tag, bound) {
  !(identical(fam_tag, "beta") && bound == 0)
}

#' Refuse a bounded response with every observation at one of its bounds
#'
#' A \code{bernoulli}, \code{binomial}, \code{beta_binomial} or \code{beta}
#' response whose every observation is 0, or 1 (every count equal to its
#' trials), does not vary, so it identifies no concentration-response curve.
#' Before #400 the three discrete families failed inside prior construction:
#' \code{response_link_scale()} anchors a response at a bound on the largest
#' observation strictly inside it, which does not exist here, and
#' \code{define_prior()} then stopped in \code{quantile()} with a message that
#' named neither the column nor the cause. \code{beta} at 1 was shifted to
#' 0.999 and fitted. \code{beta} at 0 was shifted by a tenth of the smallest
#' positive observation, which with none present is \code{Inf}.
#'
#' Such a response does identify the constant equation \code{ecxflat}, whose
#' one parameter is the level of the response, so the refusal applies to the
#' curve equations only (#419). \code{\link{bnec}} and \code{\link{bnec_group}}
#' fit \code{ecxflat} alone in place of whatever set was requested, through
#' \code{constant_fallback()}, and reach this function only for the one case
#' \code{ecxflat} cannot be fitted to either, a \code{beta} response of 0 in
#' every observation (see \code{constant_fits_bound()}). The other routes keep
#' the refusal for a set holding any curve equation and let a set of
#' \code{ecxflat} alone through: \code{\link{get_priors}}, whose curve
#' equations have no prior to give on such a response, and
#' \code{\link{bnec_hurdle}}, \code{amend()} and \code{update()}, which were
#' not given the substitution. Each passes the equations it is about to build
#' a prior for as \code{model}.
#'
#' A property of the data and the family, fixed for the whole call, so it is
#' raised once from \code{\link{bnec}} and \code{\link{get_priors}} before any
#' model is considered, and from \code{\link{bnec_group}} for every level
#' before any level is fitted, following the placement of
#' \code{\link{check_inline_boundary}}. \code{\link{bnec_hurdle}} raises it on
#' the growth component before either component is fitted. \code{amend()} and
#' \code{update()} call it for the routes that do not come through those entry
#' points, each with the family its refit uses. It is not called from
#' \code{\link{check_data}}, which is given whatever family its caller chose:
#' on the \code{update()} route that is the family read off the new data, not
#' the one the refit uses. The whole decision is made here, so that a change to
#' what is done with such a response is made in one place.
#'
#' Censoring is not consulted: the response is judged on its recorded values,
#' because the default priors are built from them. A value left- or
#' right-censored at a bound of one of these families states either what the
#' family cannot represent (beyond the bound) or nothing at all (anywhere
#' within its support). An interval-censored observation whose upper end, the
#' second variable of \code{cens()}, lies inside the support holds
#' information the recorded value does not, but \code{check_data()},
#' \code{response_link_scale()} and \code{define_prior()} read only the
#' recorded values, so such a response reached the same \code{quantile()}
#' error as one with no censoring at all when it was let through. It is refused
#' by name instead. Supplying \code{prior} does not change this: the refusal is
#' raised before any prior is read.
#'
#' @param data A model frame from \code{model.frame()} on a
#' \code{\link{bayesnecformula}}.
#' @param family A \code{\link[stats]{family}}, or its name.
#' @param group A factor with one element per row of \code{data}, or
#' \code{NULL}. Where supplied, each level is tested separately and every level
#' refused is named.
#' @param group_name The name of the grouping column, for the message.
#' @param subject The subject of the message, where the rows tested are not the
#' whole response; \code{NULL} names the response column.
#' @param model The equations the caller is about to fit or build priors for,
#' or \code{NULL}, which is read as a set holding a curve equation. A set of
#' constant equations alone is refused only where it cannot be fitted either.
#'
#' @return \code{NULL}, invisibly. Called for its error.
#' @noRd
check_response_at_bound <- function(data, family, group = NULL,
                                    group_name = NULL, subject = NULL,
                                    model = NULL) {
  state <- response_bound_state(data, family, group)
  if (is.null(state)) {
    return(invisible(NULL))
  }
  fam_tag <- state$fam_tag
  if (is.null(subject)) {
    subject <- paste0("The response \"", state$y_name, "\"")
  }
  constant_only <- length(model) > 0 && all(model %in% constant_equations())
  describe <- function(bound) {
    if (bound == 1 && state$counted) {
      "every count equals its number of trials, a proportion of 1"
    } else if (state$counted) {
      "every count is 0"
    } else {
      paste("every value is", bound)
    }
  }
  side <- function(bound) if (bound == 1) "upper" else "lower"
  why <- " A response that does not vary identifies no concentration-response"
  # The remedy depends on whether the constant equation can be fitted at the
  # bound. Where it can, the message names it, because it is the one equation
  # that can be; where it cannot, naming it would send the user to a second
  # refusal. See #419.
  no_zero <- function(held, it) {
    paste0(" A beta distribution cannot represent a zero, and bayesnec",
           " shifts a zero off the boundary by a tenth of the smallest",
           " positive value,",
           " which ", held, ", so no equation can be fitted to ", it, ", the",
           " constant equation ecxflat included.")
  }
  use_flat <- paste0(" The constant equation ecxflat, whose mean does not",
                     " change with concentration, is the only one that can be",
                     " fitted to it: name it, as in crf(x, \"ecxflat\"), or",
                     " call bnec(), which fits it alone in place of the",
                     " equations requested.")
  if (is.null(group)) {
    bound <- state$bounds
    if (is.na(bound)) {
      return(invisible(NULL))
    }
    fits <- constant_fits_bound(fam_tag, bound)
    if (constant_only && fits) {
      return(invisible(NULL))
    }
    stop(subject, " is at the ", side(bound), " bound of a ", fam_tag,
         " response in every observation: ", describe(bound), ".", why,
         " curve, so bayesnec does not fit one to it or derive default priors",
         " from it.",
         if (fits) use_flat else no_zero("this response does not have", "it"),
         call. = FALSE)
  }
  bounds <- state$bounds
  hit <- bounds[!is.na(bounds)]
  if (constant_only) {
    # Only the levels the constant equation cannot be fitted to are refused;
    # bnec_group() fits every other level at a bound with ecxflat alone.
    hit <- hit[!vapply(hit, function(b) constant_fits_bound(fam_tag, b),
                       logical(1))]
  }
  if (length(hit) == 0) {
    return(invisible(NULL))
  }
  # Refused for the whole call rather than fitting the other levels and
  # reporting this one as skipped, so that the omission is made by the user and
  # is visible in their script (D30). Every such level is named at once, so
  # that one call finds them all.
  where <- vapply(names(hit), function(lev) {
    paste0("\"", lev, "\", where ", describe(hit[[lev]]), " (the ",
           side(hit[[lev]]), " bound)")
  }, character(1))
  fits <- all(vapply(hit, function(b) constant_fits_bound(fam_tag, b),
                     logical(1)))
  reason <- if (fits) {
    paste0(why, " curve.")
  } else {
    no_zero("these levels do not have", "them")
  }
  stop(subject, " is at a bound of a ", fam_tag, " response in every",
       " observation of ", length(hit), " level(s) of \"", group_name, "\": ",
       paste(where, collapse = "; "), ".", reason, " No level has been",
       " fitted. Remove ", if (length(hit) == 1) "that level" else
         "those levels", " from `data` to fit the others, so that the",
       " omission is recorded in the call.", call. = FALSE)
}

#' Fit the constant equation alone to a response with no variation
#'
#' A bounded response with every observation at one bound identifies no curve,
#' and it does identify \code{ecxflat}, whose one parameter is the level of the
#' response and whose every estimate lies above the tested range. So
#' \code{\link{bnec}} fits \code{ecxflat} alone in place of the set requested,
#' whatever that set was, rather than refusing the call as it did under #400
#' (D31). Decided once, before the model loop, for the reason
#' \code{\link{check_response_at_bound}} is raised there. The one bound
#' \code{ecxflat} cannot be fitted at is refused by that function first.
#'
#' The requested equations are recorded as excluded, with the reason, so that
#' \code{\link{bnec_record}} states why the set fitted is not the set asked for.
#' That is the record #261 keeps for every other equation \code{\link{bnec}}
#' declines to attempt.
#'
#' @param data A model frame.
#' @param family The validated family.
#' @param model The equations requested, as read from the formula.
#' @param report Whether to say so. \code{FALSE} where \code{\link{bnec_group}}
#' has already said it for the level, before any level was fitted.
#'
#' @return \code{NULL} where the response varies, and otherwise a
#' \code{\link[base]{list}} of \code{model}, the set to fit, and
#' \code{excluded}, the rows to add to the exclusion record.
#' @noRd
constant_fallback <- function(data, family, model, report = TRUE) {
  state <- response_bound_state(data, family)
  if (is.null(state) || is.na(state$bounds)) {
    return(NULL)
  }
  # Before the set is replaced. check_models() refuses a name that is no
  # equation, and it is given ecxflat alone from here, so without this a
  # misspelt name was recorded as excluded for the bound and the fit went on.
  check_equation_names(model)
  check_response_at_bound(data, family, model = constant_equations())
  flat <- constant_equations()
  dropped <- setdiff(model, flat)
  side <- if (state$bounds == 1) "upper" else "lower"
  reason <- paste0("the response is at the ", side, " bound of a ",
                   state$fam_tag, " response in every observation, so",
                   " ecxflat was fitted alone")
  if (report && length(dropped) > 0) {
    message("The response \"", state$y_name, "\" is at the ", side,
            " bound of a ", state$fam_tag, " response in every observation.",
            " A response that does not vary identifies no",
            " concentration-response curve, so it is fitted with the constant",
            " equation ecxflat alone, and the ", length(dropped), " other",
            " equation(s) requested are not fitted. The fit states that the",
            " response did not change over the concentrations tested. The",
            " equations not fitted are recorded; see ?bnec_record.")
  }
  list(model = flat,
       excluded = data.frame(model = dropped,
                             reason = rep(reason, length(dropped)),
                             stringsAsFactors = FALSE))
}

#' The levels of a grouped call that are fitted with the constant equation
#'
#' \code{\link{bnec_group}} fits each level with \code{\link{bnec}}, which fits
#' \code{ecxflat} alone to a level whose response does not vary. The levels are
#' found and reported here, once and before any level is fitted, so that the
#' report names them all and is not left to arrive from inside a level loop
#' that may run in parallel workers, from which a message arrives out of order
#' or not at all. A level \code{ecxflat} cannot be fitted to either is refused
#' first, for the whole call, as \code{\link{check_response_at_bound}}
#' refused every level at a bound under #400.
#'
#' @param data The model frame of the whole response.
#' @param family The validated family.
#' @param group The grouping factor, one element per row of \code{data}.
#' @param group_name The name of the grouping column.
#' @param model The equations the formula requests, or \code{NULL} where they
#' could not be read. Where they are \code{ecxflat} alone nothing is set aside,
#' so nothing is reported, as \code{constant_fallback()} reports nothing then.
#'
#' @return A \code{\link[base]{character}} vector of the levels at a bound,
#' possibly empty.
#' @noRd
constant_fallback_levels <- function(data, family, group, group_name,
                                     model = NULL) {
  if (!is.null(model)) {
    check_equation_names(model)
  }
  check_response_at_bound(data, family, group = group,
                          group_name = group_name,
                          model = constant_equations())
  state <- response_bound_state(data, family, group)
  if (is.null(state)) {
    return(character(0))
  }
  hit <- state$bounds[!is.na(state$bounds)]
  if (length(hit) == 0) {
    return(character(0))
  }
  # Silent where ecxflat alone was requested: every level then fits the set
  # asked for, and there is nothing set aside to explain.
  if (!is.null(model) && all(model %in% constant_equations())) {
    return(names(hit))
  }
  where <- vapply(names(hit), function(lev) {
    paste0("\"", lev, "\" (the ", if (hit[[lev]] == 1) "upper" else "lower",
           " bound)")
  }, character(1))
  message("The response \"", state$y_name, "\" is at a bound of a ",
          state$fam_tag, " response in every observation of ", length(hit),
          " level(s) of \"", group_name, "\": ", paste(where, collapse = "; "),
          ". A response that does not vary identifies no",
          " concentration-response curve, so each such level is fitted with",
          " the constant equation ecxflat alone, and every other level with",
          " the equations requested. The equations not fitted are recorded on",
          " the fit of each such level; see ?bnec_record.")
  names(hit)
}

#' Reject a boundary value on a response transformed inside the formula
#'
#' The shift a boundary value would receive is computed on the transformed
#' scale, but \code{brm()} is handed the user's data frame and re-evaluates the
#' transformation from the recorded column, so the shift cannot reach the fit.
#' Before #258 it was computed, reported to the user and then discarded, and
#' \code{brm()} failed naming the condition the package had just said it had
#' repaired. Raising the conflict here names the variable and gives a remedy.
#'
#' @param expr The response as written in the formula, e.g. \code{"log(y)"}.
#' @param fam_tag The family name, as \code{family$family} gives it.
#' @param bound The boundary the family excludes, 0 or 1.
#' @param hint Optional further advice appended to the message.
#'
#' @noRd
stop_inline_boundary <- function(expr, fam_tag, bound, hint = "") {
  stop("Your response reaches the model as \"", expr, "\", a transformation",
       " written inside the model formula, and the transformed response",
       " contains values of ", bound, ", which a ", fam_tag, " distribution",
       " cannot represent. bayesnec shifts such values off the boundary, but",
       " the shift cannot be carried through a transformation written inline:",
       " brm() re-evaluates \"", expr, "\" from the data it is given, so the",
       " shift would be discarded and the fit would fail. Compute the",
       " transformation into a column of your data and pass that column to the",
       " formula instead.", hint, call. = FALSE)
}

#' Which rows carry a censoring declaration
#'
#' Returns a scalar \code{FALSE} when there is no censoring variable at all, so
#' that it recycles harmlessly against the response in the callers above.
#'
#' @noRd
is_censored <- function(cens) {
  if (is.null(cens)) {
    FALSE
  } else {
    !is.na(cens) & cens != 0
  }
}

#' Reject a censored observation sitting on a boundary the family excludes
#'
#' @param bound The boundary value, 0 or 1.
#' @param direction The brms censoring code that would be degenerate there,
#' -1 (left) at 0 and 1 (right) at 1.
#'
#' @noRd
check_cens_support <- function(y, cens, bound, direction, fam_tag) {
  if (is.null(cens)) {
    return(invisible(NULL))
  }
  bad <- which(!is.na(cens) & cens == direction & y == bound)
  if (length(bad) == 0) {
    return(invisible(NULL))
  }
  side <- if (direction < 0) "left" else "right"
  beyond <- if (direction < 0) "at or below" else "at or above"
  stop("Row(s) ", paste0(bad[seq_len(min(10, length(bad)))], collapse = ", "),
       if (length(bad) > 10) ", ..." else "",
       " of your response are declared ", side, "-censored at ", bound,
       ", but a ", fam_tag, " distribution has no probability mass ", beyond,
       " ", bound, ", so the censored likelihood is degenerate there. For a ",
       side, "-censored row the response value must carry the bound -- the",
       " value the truth is known to be ", beyond, " -- so replace those",
       " entries with that bound (for a rounded response, half the recording",
       " resolution). See ?bayesnecformula.", call. = FALSE)
}
