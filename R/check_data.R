#' Whether every value is k / n for one integer n up to max_n
#'
#' Proportions derived from counts lie on such a grid, and a continuous
#' response divided by a continuous maximum does not.
#' \code{check_normalisation()} uses it so that a count proportion with every
#' individual alive in one replicate is not reported as divided by its
#' maximum. A count proportion that was divided by its maximum is missed.
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
#' Report a response normalised to a quantity estimated from the same data:
#' divided by the observed maximum, which leaves a maximum of exactly 1 at one
#' observation, or by the control mean, which leaves a mean of exactly 1 at the
#' lowest predictor value.
#'
#' Raises messages, not warnings. Call it from \code{\link{bnec}}, not
#' \code{check_data}, so that it reports once per call, not once per model.
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
#' The flatness contrast is tested under the mean-variance relationship the fit
#' assumes. quasipoisson stands in for the negative binomial and quasibinomial
#' for the beta-binomial, because their shape parameters cannot be estimated
#' from two levels. The beta row is quasibinomial because the beta variance is
#' \code{mu (1 - mu)} times a constant. The Gamma and count rows need the log
#' link: the inverse link \code{stats::Gamma()} takes by default reverses the
#' sign of the contrast.
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
#' A hurdle family is split into the response past the hurdle and the
#' survival, since either can be the curve that has not flattened. The survival
#' block is built from the raw counts, not from \code{survival_by_x()}, whose
#' proportions are clamped to \code{[eps, 1 - eps]}.
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
    # No hurdle fit has a rate() denominator yet: make_brmsformula() refuses
    # one. Attach it anyway, since a count hurdle's spec has an offset and the
    # contrast would otherwise be on counts, not rates.
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
    # bnec() and bnec_group() refuse a missing trials() term before this
    # report. A direct call returns nothing here, since a report would name the
    # symptom rather than the missing term.
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
#' Compares the two levels by analysis of deviance, not by the Wald statistic
#' \code{summary.glm()} prints. The Wald test fails under the separation the
#' survival block often shows, with every individual alive at one level and
#' none at the other.
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
  # Estimate the dispersion from the two levels compared: for every family but
  # gaussian the variance depends on the mean, so a whole-series estimate
  # overstates it at the top. A gaussian fit with no disp() term has one sigma,
  # so pool_dispersion pools it there.
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
    # An offset keeps the counts integral and puts the contrast on the rate
    # scale, with the same exposure column the fit uses.
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
  # Without replication, a binomial or beta-binomial block falls back to a
  # fixed dispersion rather than being passed over: quasibinomial needs
  # replication, and one sample of many individuals per concentration is the
  # common binomial design. For a beta-binomial response, which is
  # over-dispersed, the test is then anti-conservative.
  if (estimates_dispersion && identical(spec$kind, "matrix") && !replicated) {
    spec$family <- binomial()
    estimates_dispersion <- FALSE
  }
  # Pass over in silence a block with too little information at the two
  # levels. Where the dispersion is estimated, that is a block without
  # replication: a continuous predictor with one observation per value, as in
  # nec_data, is common and not a defect. Where it is fixed, that is fewer than
  # two individuals at either level, where the deviance test is unreliable.
  # `replicated` above uses tabulate(match()), because table() rounds the
  # predictor through as.character().
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
  # Where no observation differs from another at its own predictor value, the
  # dispersion is zero and the contrast is undefined. Test the observations,
  # not the estimate, which is a rounding residue rather than zero. A fixed
  # dispersion needs no such check.
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
#' them, so a fit on \code{crf(log(concentration))} reports log
#' concentrations. Naming the expression keeps them from being read as
#' concentrations.
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
  # Use the model frame's column name, "log(x)", not the bnec_pop entry, "x":
  # the term says which scale the reported numbers are on.
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
#' The default \code{bot} prior is built from the observed response, and the
#' default \code{nec} and \code{ec50} priors from the tested predictor range,
#' so on such a design all three describe the design rather than the curve.
#' The test is a one-sided generalised linear model contrast between the two
#' highest predictor values of each block, at level \code{alpha}. In a hurdle
#' family the response block holds survivors only, so the report names the
#' levels it used. Call it once, before the model or level loop.
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
#' whole series. \code{TRUE} where the formula has no \code{disp()} term.
#' @param asymptote_observed The \code{asymptote_observed} argument of
#' \code{\link{bnec}}. \code{FALSE} drops the advice to set it, and changes
#' the sentence on the \code{nec} and \code{ec50} priors to match the priors
#' it builds.
#'
#' @return \code{NULL}, invisibly. Called for its message.
#'
#' @noRd
check_response_flattened <- function(data, family, group = NULL,
                                     blocks = c("response", "survival"),
                                     alpha = 0.05, pool_dispersion = TRUE,
                                     asymptote_observed = TRUE) {
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
    # The declaration changes what the threshold priors admit, so the sentence
    # about them follows it.
    threshold_priors <- if (isTRUE(asymptote_observed)) {
      paste0(" the default nec and ec50 priors are bounded to the tested",
             " range, so they exclude a threshold above it.")
    } else {
      paste0(" the default nec and ec50 priors put a threshold above the",
             " tested range in their tail, where how much of the prior lies",
             " there depends on prior_type and on whether the predictor was",
             " supplied as a concentration or already logged; ?bnec tabulates",
             " the four under asymptote_observed.")
    }
    message(
      paste(declining, collapse = "\n"), "\n",
      "The lower asymptote may not be identified by this design: where an",
      " equation estimates bot its default prior is derived from the observed",
      " response, and", threshold_priors,
      " Inspect the entries with get_priors() and",
      " supply scientifically justified ones through the prior argument where",
      " information beyond the design is available.",
      declaration_advice(family, y, asymptote_observed)
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
#' @param affected The parameter rows to read: the three the flatness report
#' reads by default, or \code{"bot"} alone, the only row the declaration
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
#' @details The sentence is omitted where no floor can be derived, since
#' \code{asymptote_observed = FALSE} would then be refused, and where the call
#' already sets it. \code{\link{asymptote_floor}} expects the link scale but is
#' given the response as supplied. The two agree, because only its gaussian
#' branch reads the response, under an identity link. A hurdle family can be
#' passed whole, since its mu family has the same link.
#'
#' @param family The validated response family.
#' @param y The response as supplied.
#' @param asymptote_observed The \code{asymptote_observed} argument of
#' \code{\link{bnec}}.
#'
#' @return A \code{\link[base]{character}} string, empty where no floor can be
#' derived or where \code{asymptote_observed} is \code{FALSE}.
#'
#' @noRd
declaration_advice <- function(family, y, asymptote_observed = TRUE) {
  if (!isTRUE(asymptote_observed) ||
      !is.finite(asymptote_floor(family, y))) {
    return("")
  }
  paste0(" Where the design did stop before the asymptote, declare it with",
         " asymptote_observed = FALSE, which spans the bot prior from the",
         " floor of the response to the mean at the highest predictor level,",
         " removes the upper bound of the nec and ec50 priors, and widens the",
         " initial-value search to match.")
}

#' Act on a declaration that the lower asymptote was not observed
#'
#' \code{asymptote_observed = FALSE} needs a floor, the smallest value the mean
#' can take. This refuses a response for which none can be derived, unless the
#' supplied prior covers every \code{bot} row, and reports a model set that
#' mixes equations estimating a lower asymptote with ones that have none. Call
#' it once, before the model or level loop.
#'
#' The floor is derived from the response divided by its trials or
#' \code{rate()} denominator and put on the link scale, as
#' \code{fit_bayesnec()} builds it. Reading it before \code{\link{check_data}}
#' moves a value off a boundary is safe: the floor depends only on the sign of
#' the response and on the link.
#'
#' @param data A model frame, as returned by the \code{\link{model.frame}}
#' method for a \code{\link{bayesnecformula}}.
#' @param family The validated response family.
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
  # Report a mixed set whatever prior was supplied: a bot prior does not make
  # the region that separates the two kinds observed. The gate below is for
  # the refusals alone. The message says the bot_free equations have no lower
  # asymptote, not that they fall to zero: neclin, neclinhorme and ecxlin are
  # unbounded below. Read the other side off each equation's parameters, not
  # as the complement of bot_free, so that ecxflat is on neither side.
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
  # Raise unobserved_endpoint_mean()'s refusal here too, so that define_prior()
  # does not print it once per equation. It is reached only where no value lies
  # above the floor.
  invisible(unobserved_endpoint_mean(x, response,
                                     zero_bounded_family(floor_family)))
  invisible(NULL)
}

#' Refuse a model frame from which incomplete cases were removed
#'
#' \code{stats::model.frame()} drops an incomplete case before
#' \code{\link{check_data}} sees it, so the fit would run on fewer rows with
#' nothing said. The rows removed are reported by row name, not position: for
#' one level of a \code{\link{bnec_group}} call a position indexes the subset,
#' not the data supplied.
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
#' \code{check_data()} tests only the variables \code{crf()} declares, and a
#' \code{disp(~...)} term's variables are kept out of the model frame, so an
#' infinite value would reach Stan with nothing naming the term. Call it from
#' \code{\link{bnec}} and \code{bnec_group()}: \code{check_data()} is given the
#' model frame and cannot see these columns. A term that does not evaluate to
#' numbers, such as a smooth, is left to \pkg{brms}.
#'
#' @param formula A \code{\link{bayesnecformula}}.
#' @param data A \code{\link[base]{data.frame}}, the one the user supplied.
#'
#' @return \code{NULL}, invisibly. Called for the error.
#'
#' @importFrom stats as.formula terms
#' @noRd
check_disp_finite <- function(formula, data) {
  # Let parse_disp_term() raise its own refusals here. Under a try() they would
  # recur inside bnec()'s per-model try(), once per equation.
  disp_spec <- parse_disp_term(formula)
  if (is.null(disp_spec) || !identical(disp_spec$route, "A")) {
    return(invisible(NULL))
  }
  # disp_spec$value is an expression string, "log(x)", so add the tilde.
  # Without it as.formula() fails and the check passes everything.
  disp_formula <- try(as.formula(paste("~", disp_spec$value)), silent = TRUE)
  if (inherits(disp_formula, "try-error")) {
    return(invisible(NULL))
  }
  labels <- attr(terms(disp_formula), "term.labels")
  bad <- character(0)
  incomplete <- character(0)
  for (label in labels) {
    # Evaluate in the formula's environment, so that a user-defined function
    # such as disp(~cent(x)) is found; otherwise the term is skipped unchecked.
    values <- try(eval(str2lang(label), envir = data,
                       enclos = formula_env(formula)), silent = TRUE)
    if (inherits(values, "try-error") || !is.numeric(values)) {
      next
    }
    # Test NA and non-finite values separately: they have different causes,
    # and check_complete_cases() cannot see these columns.
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
#' \pkg{brms} resolves the generated non-linear terms that
#' \code{generated_term_names()} lists against the user's data first, so a
#' column of one of those names would silently replace the term. The whole set
#' is refused whatever the formula, so that the same data are not accepted on
#' one call and refused on the next.
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

#' Refuse a binomial or beta_binomial formula with no trials() term
#'
#' \pkg{brms} reads the number of trials from a \code{trials()} term, and
#' without it neither the default priors nor the fit can be built. Call it once
#' per call, before any equation or level is fitted; \code{\link{check_data}}
#' and \code{\link{amend}} keep it as a backstop.
#'
#' @param data A model frame for a \code{\link{bayesnecformula}}.
#' @param family A validated \code{\link[stats]{family}}, or a family name.
#'
#' @return \code{NULL}, invisibly. Called for the error.
#' @noRd
check_trials_term <- function(data, family) {
  fam_tag <- if (inherits(family, "family")) family$family else family
  if (!fam_tag %in% c("binomial", "beta_binomial")) {
    return(invisible(NULL))
  }
  # model.frame() leaves trials_var out of bnec_pop where the formula has no
  # trials() term, so its absence is the test.
  if ("trials_var" %in% names(attr(data, "bnec_pop"))) {
    return(invisible(NULL))
  }
  # Write the example with the formula's own response and predictor, so that
  # only the term to add is new.
  y_label <- pop_var_label(data, "y_var", "y")
  x_label <- pop_var_label(data, "x_var", "x")
  stop("The ", fam_tag, " family needs the number of trials for each",
       " observation, and the formula has no trials() term to supply it. Name",
       " the column that holds them in a trials() term on the left-hand side",
       " of the formula, for example ", y_label, " | trials(n) ~ crf(",
       x_label, ", \"nec3param\"), where n is that column. See",
       " ?bayesnecformula.", call. = FALSE)
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
  # bnec() and bnec_group() run this before any model is fitted. It stays here
  # for get_priors() and amend(), which do not come through them.
  check_complete_cases(data)
  # A backstop for update() and a direct fit_bayesnec() call. It precedes the
  # checks on the response, so that nothing about the response is reported for
  # a formula that cannot be fitted.
  check_trials_term(data, family)
  # Test every element, so that an NA left by options(na.action = "na.pass")
  # is reported too.
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
  # A censored value is a bound the user declared, so the nudges below leave
  # it alone. NULL where the formula has no cens() term.
  cens <- retrieve_cens(data)
  # Called for its error, not its value: it refuses an integer predictor,
  # which is.numeric() lets through.
  set_distribution(x, silence_y_msgs = TRUE, silence_x_msgs = FALSE)
  # A family open at a boundary cannot fit a censored value on it: Stan sees
  # log(0) and initialisation fails without explanation. Check before the
  # nudges below, so that the result does not depend on them.
  if (fam_tag %in% c("Gamma", "hurdle_gamma", "beta", "zero_inflated_beta")) {
    check_cens_support(y, cens, bound = 0, direction = -1L, fam_tag = fam_tag)
  }
  if (fam_tag %in% c("beta", "zero_inflated_beta")) {
    check_cens_support(y, cens, bound = 1, direction = 1L, fam_tag = fam_tag)
  }
  # A backstop for get_priors() and a direct fit_bayesnec() call; bnec() runs
  # it once, before the model loop.
  check_inline_boundary(data, family)
  # The corrections below change the response only. No family constrains the
  # predictor, so a zero concentration reaches brm() as recorded.

  # A censored zero is a declared bound, not an artefact, so it is not nudged.
  to_shift <- y == 0 & !is_censored(cens)
  # Never nudge a hurdle family's zeros: they identify the hu block. The
  # is_hurdle_family() test below repeats what fam_tag == "Gamma" already
  # ensures, and is kept on purpose.
  # The nudges are silent here, because check_data() runs once per model. The
  # entry points report them once, from substitution_record().
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
#' Keep the conditions identical to the nudges in \code{\link{check_data}},
#' censoring exemptions included, so that the record describes what was done.
#' \code{report_substitutions()} reports it, and the fit stores it.
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
#' @param on_fit Whether the caller returns a fit that stores the record. The
#' message then points to \code{?bnec_record}.
#' @param prints_priors Whether the caller returns priors derived from the
#' substituted response, as \code{\link{get_priors}} does. The default makes
#' \code{on_fit = FALSE} mean \code{get_priors()}, so \code{update()} on a fit
#' with no record must pass \code{FALSE} for both.
#'
#' @return \code{NULL}, invisibly. Called for the message.
#' @noRd
report_substitutions <- function(record, on_fit = TRUE,
                                 prints_priors = !on_fit) {
  if (is.null(record) || nrow(record) == 0) {
    return(invisible(NULL))
  }
  where <- paste0(
    "",
    if (on_fit) {
      " The substitution is recorded on the fitted object; see ?bnec_record."
    },
    if (prints_priors) {
      " The priors below are derived from the substituted response."
    }
  )
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
#' \code{\link{bnec}} calls it once, before the model loop. Keep its conditions
#' identical to the nudges in \code{\link{check_data}}: a value it refuses is
#' one that would otherwise have been nudged.
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
  # zero_inflated_beta is listed although 0 is inside its support: its zeros
  # go to the zero-inflation block, so a response of 0 throughout leaves the
  # beta block no data. Its ones are shifted as beta's are.
  if (!fam_tag %in% c("bernoulli", "binomial", "beta_binomial", "beta",
                      "zero_inflated_beta")) {
    return(NULL)
  }
  y <- try(retrieve_var(data, "y_var", error = TRUE), silent = TRUE)
  if (inherits(y, "try-error")) {
    return(NULL)
  }
  trials <- NULL
  if (fam_tag %in% c("binomial", "beta_binomial")) {
    trials <- retrieve_var(data, "trials_var")
    # Without trials there is no upper bound to test. check_trials_term()
    # refuses that case, before or after this on every route.
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
#' It can at every bound but 0 of the two beta families. A beta response of 0
#' throughout has no positive value to set the shift off zero, and a
#' zero_inflated_beta response of 0 throughout leaves its beta block no data.
#' A value of 1 is shifted to 0.999, which needs no other observation.
#'
#' @param fam_tag The family tag.
#' @param bound 0 or 1.
#'
#' @return A \code{\link[base]{logical}} of length 1.
#' @noRd
constant_fits_bound <- function(fam_tag, bound) {
  !(fam_tag %in% c("beta", "zero_inflated_beta") && bound == 0)
}

#' Refuse a bounded response with every observation at one of its bounds
#'
#' A bernoulli, binomial, beta_binomial, beta or zero_inflated_beta response
#' that is 0 throughout, or 1 throughout (every count equal to its trials),
#' identifies no curve. It does identify the constant equation \code{ecxflat},
#' so a set of \code{ecxflat} alone passes where \code{constant_fits_bound()}
#' allows. Call it before \code{\link{check_data}} and before any model or
#' level is fitted. Censoring is not consulted, because the default priors are
#' built from the recorded values.
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
#' @param flat_remedy The end of the sentence that names \code{ecxflat},
#' saying how to fit it from the caller, or \code{NULL} to name \code{crf()}
#' and \code{bnec()}. Read only where \code{group} is \code{NULL}.
#'
#' @return \code{NULL}, invisibly. Called for its error.
#' @noRd
check_response_at_bound <- function(data, family, group = NULL,
                                    group_name = NULL, subject = NULL,
                                    model = NULL, flat_remedy = NULL) {
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
  # Name ecxflat as the remedy only where it can be fitted at the bound. A
  # zero_inflated_beta response can hold a zero, so it gets its own reason.
  no_zero <- function(grouped) {
    it <- if (grouped) "them" else "it"
    if (identical(fam_tag, "zero_inflated_beta")) {
      return(paste0(
        " A zero_inflated_beta response fits its zeros in the zero-inflation",
        " block and its other values in the beta block, and no observation of ",
        if (grouped) "these levels" else "this response", " is above zero,",
        " so the beta block has no data and no equation can be fitted to ", it,
        ", the constant equation ecxflat included."
      ))
    }
    paste0(" A beta distribution cannot represent a zero, and bayesnec",
           " shifts a zero off the boundary by a tenth of the smallest",
           " positive value,",
           " which ", if (grouped) "these levels do not have" else
             "this response does not have",
           ", so no equation can be fitted to ", it, ", the",
           " constant equation ecxflat included.")
  }
  if (is.null(flat_remedy)) {
    flat_remedy <- paste0("name it, as in crf(x, \"ecxflat\"), or call bnec(),",
                          " which fits it alone in place of the equations",
                          " requested.")
  }
  use_flat <- paste0(" The constant equation ecxflat, whose mean does not",
                     " change with concentration, is the only one that can be",
                     " fitted to it: ", flat_remedy)
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
         if (fits) use_flat else no_zero(grouped = FALSE),
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
  # Refuse the whole call rather than skip the level, so that the user makes
  # the omission in their script. Name every such level at once.
  where <- vapply(names(hit), function(lev) {
    paste0("\"", lev, "\", where ", describe(hit[[lev]]), " (the ",
           side(hit[[lev]]), " bound)")
  }, character(1))
  fits <- all(vapply(hit, function(b) constant_fits_bound(fam_tag, b),
                     logical(1)))
  reason <- if (fits) {
    paste0(why, " curve.")
  } else {
    no_zero(grouped = TRUE)
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
#' A bounded response at one bound throughout identifies no curve but does
#' identify \code{ecxflat}. \code{\link{bnec}} fits \code{ecxflat} alone in
#' place of the set requested, and records the other equations as excluded so
#' that \code{\link{bnec_record}} says why. \code{check_response_at_bound()}
#' first refuses a response \code{ecxflat} cannot be fitted to either.
#'
#' @param data A model frame.
#' @param family The validated family.
#' @param model The equations requested, as read from the formula.
#' @param report Whether to report. \code{FALSE} where \code{\link{bnec_group}}
#' has already reported the level.
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
  # Check the names before the set is replaced: check_models() sees only
  # ecxflat from here, so a misspelt name would be recorded as excluded.
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
#' Finds and reports, once and before any level is fitted, the levels that
#' \code{\link{bnec}} will fit with \code{ecxflat} alone, since a message from
#' a level fitted in a worker arrives out of order or not at all. It first
#' refuses a name that is not an equation, and a level \code{ecxflat} cannot be
#' fitted to.
#'
#' @param data The model frame of the whole response.
#' @param family The validated family.
#' @param group The grouping factor, one element per row of \code{data}.
#' @param group_name The name of the grouping column.
#' @param model The equations the formula requests, or \code{NULL} where they
#' could not be read. Nothing is reported where they are \code{ecxflat} alone.
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
#' \code{brm()} re-evaluates an inline transformation from the user's data, so
#' a shift off the boundary computed on the transformed scale cannot reach the
#' fit. This names the variable and the remedy instead.
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
