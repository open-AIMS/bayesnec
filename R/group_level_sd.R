#' Group-level standard deviations of each equation
#'
#' Tabulates the group-level standard deviations of every equation of a fit,
#' such as the single deviation of an \code{ogl(plate)} term or the one per
#' parameter of a \code{pgl(plate)} term, beside that equation's model weight.
#' \code{\link{summary}} does not report them.
#'
#' @param x An object of class \code{\link{bayesnecfit}},
#' \code{\link{bayesmanecfit}}, \code{\link{bayesnecgroupfit}} or
#' \code{\link{bayesnechurdlefit}}.
#'
#' @details Each equation of a model set estimates its own group-level
#' standard deviations. Read each one against the weight of the equation it
#' was computed on: a deviation from an equation with almost no weight
#' describes that equation's curve and says little about the model-averaged
#' estimates.
#'
#' \code{parameter} names the non-linear parameter the deviation applies to,
#' as \pkg{brms} names it in \code{\link[brms]{VarCorr}}. \code{ogl} is the
#' deviation on the whole curve. Where the link permits the fitted mean to
#' leave the family's support and the equation permits a log or logit
#' transform, \code{ogl} is applied on that scale. Otherwise it is added on
#' the linear predictor scale. A deviation on \code{top} or \code{bot} is named
#' \code{topgl} or \code{botgl} when the link permits the mean to leave its
#' support; it is then applied on the log or log-odds scale. Other
#' deviations are added on the scale of the quantity they apply to. See
#' \code{\link{bayesnecformula}}.
#'
#' \code{Estimate} is the posterior median, as in \code{\link{ecx}} and
#' \code{\link{dispersion}}, and \code{Q2.5} and \code{Q97.5} bound the 95\%
#' credible interval. \code{\link[brms]{VarCorr}} reports the posterior mean
#' instead.
#'
#' @return A \code{\link[base]{data.frame}} with one row per equation, grouping,
#' parameter and \code{by} level, and columns \code{model}, \code{wi},
#' \code{group}, \code{by}, \code{parameter}, \code{Estimate}, \code{Q2.5}
#' and \code{Q97.5}. The \code{by} column is empty for ordinary grouping
#' terms and names the variable and level (for example, \code{h=A}) for a
#' \code{gr(g, by = h)} term. \code{wi} is 1 for a
#' \code{\link{bayesnecfit}}. A fit with no group-level term gives
#' a table with no rows, and a message. For a \code{\link{bayesnecgroupfit}},
#' the same table with a leading \code{level} column; each weight is from that
#' level's own model set. For a \code{\link{bayesnechurdlefit}}, a
#' named \code{\link[base]{list}} of two such tables, \code{growth} and
#' \code{survival}.
#'
#' @seealso \code{\link[brms]{VarCorr}}, \code{\link{check_sampling}},
#' \code{\link{dispersion}}, \code{\link{pull_out}}
#'
#' @importFrom brms VarCorr
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' data(nec_data)
#' nec_data$plate <- factor(rep(letters[1:4], length.out = nrow(nec_data)))
#' fit <- bnec(y ~ crf(x, c("nec4param", "ecx4param")) + ogl(plate),
#'             data = nec_data)
#' group_level_sd(fit)
#' }
#'
#' @export
group_level_sd <- function(x) {
  if (is_bayesnechurdlefit(x)) {
    out <- hurdle_delegate(x, gl_sd_table)
    none <- names(out)[vapply(out, nrow, integer(1)) == 0]
    if (length(none) > 0) {
      message("The ", paste(none, collapse = " and "), " component",
              if (length(none) > 1) "s have" else " has",
              " no group-level term.")
    }
    return(out)
  }
  if (is_bayesnecgroupfit(x)) {
    tabs <- group_lapply(x, gl_sd_table)
    out <- do.call(rbind, Map(function(tab, level) {
      cbind(data.frame(level = rep(level, nrow(tab)),
                       stringsAsFactors = FALSE), tab)
    }, tabs, x$levels))
    rownames(out) <- NULL
  } else {
    out <- gl_sd_table(x)
  }
  if (nrow(out) == 0) {
    message("No equation of this fit has a group-level term, so there are no",
            " group-level standard deviations to report.")
  }
  out
}

#' The group-level standard deviations of a bayesnecfit or bayesmanecfit
#'
#' @param x A \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @return A \code{\link[base]{data.frame}} as \code{group_level_sd()}
#' returns, with no rows where no equation has a group-level term.
#'
#' @noRd
gl_sd_table <- function(x) {
  if (is_bayesmanecfit(x)) {
    models <- x$success_models
    fits <- lapply(models, function(m) x$mod_fits[[m]]$fit)
    wi <- x$mod_stats[models, "wi"]
  } else if (is_bayesnecfit(x)) {
    models <- x$model
    fits <- list(pull_brmsfit(x))
    wi <- 1
  } else {
    stop("group_level_sd requires a bayesnecfit, a bayesmanecfit, a",
         " bayesnecgroupfit or a bayesnechurdlefit. For a bayesnecjointfit,",
         " use brms::VarCorr(x$fit).", call. = FALSE)
  }
  rows <- Map(gl_sd_rows, fits, models, wi)
  out <- do.call(rbind, c(list(gl_sd_empty()), rows))
  rownames(out) <- NULL
  out
}

#' The group-level standard deviations of one brmsfit
#'
#' @param fit A \code{\link[brms]{brmsfit}}.
#' @param model A \code{\link[base]{character}} string naming the equation.
#' @param wi The equation's model weight.
#'
#' @return A \code{\link[base]{data.frame}} with one row per grouping and
#' parameter, or \code{NULL} where the fit has no group-level term.
#'
#' @noRd
gl_sd_rows <- function(fit, model, wi) {
  re <- fit$ranef
  if (is.null(re) || nrow(re) == 0) {
    return(NULL)
  }
  # VarCorr() also returns sigma as residual__ for some families. Read only
  # the groupings in ranef, which excludes it.
  vc <- VarCorr(fit, summary = FALSE)
  out <- lapply(unique(re$group), function(g) {
    r <- re[re$group == g, , drop = FALSE]
    draws <- vc[[g]]$sd
    # brms repeats a grouping's ranef rows once for each by-factor level.
    by <- if (isTRUE(nzchar(r$by[1]))) r$by[1] else ""
    by_levels <- if (nzchar(by)) r$bylevels[[1]] else character()
    parameter <- ifelse(nzchar(r$nlpar), r$nlpar, r$dpar)
    if (nzchar(by)) {
      parameter <- rep(parameter, times = length(by_levels))
      by_label <- rep(paste0(by, "=", by_levels), each = nrow(r))
    } else {
      by_label <- rep("", nrow(r))
    }
    if (ncol(draws) != length(parameter)) {
      stop("The group-level standard deviations of ", model, " on ", g,
           " do not match its group-level terms. This should not happen;",
           " please report it.", call. = FALSE)
    }
    est <- t(apply(draws, 2, estimates_summary))
    data.frame(model = model, wi = wi, group = g, by = by_label,
               parameter = parameter,
               Estimate = est[, "Estimate"], Q2.5 = est[, "Q2.5"],
               Q97.5 = est[, "Q97.5"], row.names = NULL,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

#' @noRd
gl_sd_empty <- function() {
  data.frame(model = character(), wi = numeric(), group = character(),
             by = character(), parameter = character(), Estimate = numeric(),
             Q2.5 = numeric(), Q97.5 = numeric(), stringsAsFactors = FALSE)
}
