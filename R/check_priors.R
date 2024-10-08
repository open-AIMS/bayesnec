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
#' @return A plot of the prior and posterior parameter probability densities.
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
  brms_fit <- object$fit
  all_pars <- rownames(brms::fixef(brms_fit))
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
#' @importFrom grDevices devAskNewPage pdf
#'
#' @noRd
#'
#' @export
check_priors.bayesmanecfit <- function(object, filename = NA, ask = TRUE) {
  if (!is.na(filename)) {
    chk_character(filename)
  }
  if (!is.na(filename)) {
    pdf(file = paste(filename, ".pdf", sep = ""), onefile = TRUE,
        width = 12, height = 4)
  } else {
    devAskNewPage(ask = ask)
  }
  for (m in seq_len(length(object$mod_fits))) {
    out_plot <- check_priors(object = pull_out(object, model = names(object$mod_fits)[m])) +
      ggtitle(names(object$mod_fits)[m])
    print(out_plot)
  }
  if (!is.na(filename)) {
    dev.off()
    message("Probability density plots saved to file ", filename, ".pdf")
  }
  devAskNewPage(ask = FALSE)
}
