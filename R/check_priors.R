#' Plots the prior and posterior parameter probability densities from an
#' object of class \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}} returned by \code{\link{bnec}}.
#' @param filename An optional \code{\link[base]{character}} vector to be used
#' as a pdf filename in the case of a \code{\link{bayesmanecfit}}. Any non
#' empty character string will indicate the user wants to save the plots.
#' @param ask Should the user be asked to hit enter for next page? Defaults to
#' \code{TRUE}. Only relevant if \code{object} is of class
#' \code{\link{bayesmanecfit}}.
#'
#' @seealso \code{\link{bnec}}
#'
#' @return For a \code{\link{bayesnecfit}}, a \code{\link[ggplot2]{ggplot}} of
#' the prior and posterior probability densities of each parameter. For a
#' \code{\link{bayesmanecfit}}, one such plot per equation is printed, or
#' written to \code{filename}, and \code{NULL} is returned invisibly.
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' data(manec_example)
#' check_priors(manec_example)
#' }
#'
#' @export
check_priors <- function(object, filename = NA, ask = TRUE) {
  UseMethod("check_priors")
}

#' Plots the prior and posterior parameter probability densities from an
#' object of class \code{\link{bayesnecfit}}.
#'
#' @inheritParams check_priors
#'
#' @param object An object of class \code{\link{bayesnecfit}} returned by
#' \code{\link{bnec}}.
#'
#' @inherit check_priors examples return
#' 
#' @importFrom ggplot2 ggplot geom_density facet_wrap scale_fill_manual theme_bw labs
#' @importFrom brms hypothesis
#' @importFrom rlang .data
#'
#' @noRd
#'
#' @export
check_priors.bayesnecfit <- function(object, filename = NA, ask = TRUE) {
  if (!is.na(filename)) {
    chk_character(filename)
  }
  prior_posterior_plot(object$fit, rownames(brms::fixef(object$fit)))
}

#' Plot the prior and posterior densities of some parameters of a brmsfit
#'
#' @param brms_fit A \code{\link[brms]{brmsfit}}.
#' @param all_pars Population-level parameter names, as the row names of
#' \code{\link[brms]{fixef}} give them.
#'
#' @return A \code{\link[ggplot2]{ggplot}}.
#'
#' @importFrom ggplot2 ggplot geom_density facet_wrap scale_fill_manual theme_bw labs
#' @importFrom brms hypothesis
#' @importFrom rlang .data
#'
#' @noRd
prior_posterior_plot <- function(brms_fit, all_pars) {
  all_data <- vector(mode = "list", length = length(all_pars))
  for (i in seq_along(all_pars)) {
    hyp <- hypothesis(brms_fit, paste0(all_pars[i], " = 0"))
    dat <- plot(hyp, plot = FALSE)[[1]]$data
    dat$ind <- gsub("_Intercept", "", all_pars[i], fixed = TRUE)
    all_data[[i]] <- dat
  }
  all_data <- do.call("rbind.data.frame", all_data)
  ggplot(data = all_data) +
    geom_density(mapping = aes(x = .data$values, fill = .data$Type), adjust = 2,
                 alpha = 0.5) +
    facet_wrap(~.data$ind, scales = "free") +
    scale_fill_manual(values = c(Prior = "grey90", Posterior = "grey30")) +
    labs(x = "Value", y = "Density") +
    theme_bw()
}

#' Plots the prior and posterior parameter probability densities from an
#' object of class \code{\link{bayesmanecfit}}.
#'
#' @inheritParams check_priors
#'
#' @param object An object of class \code{\link{bayesmanecfit}} returned by
#' \code{\link{bnec}}.
#'
#' @inherit check_priors examples return
#'
#' @importFrom ggplot2 ggtitle
#' @importFrom grDevices devAskNewPage pdf dev.cur dev.list dev.off
#'
#' @noRd
#'
#' @export
check_priors.bayesmanecfit <- function(object, filename = NA, ask = TRUE) {
  if (!is.na(filename)) {
    chk_character(filename)
  }
  # Leave the devices as they were found, even when a plot stops the call:
  # close the PDF on exit, and restore the page prompt, as autoplot() does.
  if (!is.na(filename)) {
    pdf(file = paste(filename, ".pdf", sep = ""), onefile = TRUE,
        width = 12, height = 4)
    pdf_dev <- dev.cur()
    on.exit(if (pdf_dev %in% dev.list()) dev.off(pdf_dev), add = TRUE)
  } else {
    default_ask <- devAskNewPage()
    on.exit(devAskNewPage(default_ask), add = TRUE)
    devAskNewPage(ask = ask)
  }
  for (m in seq_len(length(object$mod_fits))) {
    out_plot <- check_priors(object = pull_out(object, model = names(object$mod_fits)[m])) +
      ggtitle(names(object$mod_fits)[m])
    print(out_plot)
  }
  if (!is.na(filename)) {
    dev.off(pdf_dev)
    message("Probability density plots saved to file ", filename, ".pdf")
  }
  invisible(NULL)
}
