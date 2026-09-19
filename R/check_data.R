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
#' response and the default \code{nec} and \code{ec50} priors are truncated to
#' the tested predictor range, so on a design whose response is still falling at
#' the highest concentration all three describe the design rather than the
#' curve. The condition is reported before anything is fitted, because what the
#' user does about it is a choice of prior.
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
      " response, and the default nec and ec50 priors exclude a threshold",
      " above the tested range. Inspect the entries with get_priors() and",
      " supply scientifically justified ones through the prior argument where",
      " information beyond the design is available."
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
#'
#' @return A named logical vector, one element per response block.
#'
#' @noRd
uses_response_range_defaults <- function(prior, models, family,
                                         model_survival = NULL) {
  affected <- c("bot", "nec", "ec50")
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
