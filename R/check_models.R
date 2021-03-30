#' check_models
#'
#' Check model input for a Bayesian model fit
#'
#' @inheritParams bnec
#'
#' @details
#'
#' This is a wrapper function to test input model criteria and find the
#' correct models for use in \code{\link{fit_bayesnec}}.
#'
#' @importFrom stats na.omit
#' @return A \code{\link[base]{list}} of modified elements
#' necessary for \code{\link{fit_bayesnec}}.
check_models <- function(model, family) {
  fam_tag <- family$family
  link_tag <- family$link
  if (link_tag %in% c("logit", "log")) {
    use_model <-  model[!model %in% mod_groups$zero_bounded]
    drop_model <- setdiff(model, use_model)
    if (length(drop_model) > 0) {
      message(paste("Dropping the model(s)",
                    paste0(drop_model, collapse = ", "),
                    "as they are not valid in the case of a",
                    link_tag, "link."))
    }
    if (length(use_model) == 0) {
      stop(paste("None of the model(s) specified are valid for a",
                 link_tag, "link."))
    } else {
      model <- use_model
    }
  }
  if (link_tag == "identity" & fam_tag %in% c("beta", "binomial", "custom")) {
    use_model <-  model[!model %in% c("neclin", "neclinhorme", "ecxlin")]
    drop_model <- setdiff(model, use_model)
    if (length(drop_model) > 0) {
      message(paste("Dropping the model(s)",
                    paste0(drop_model, collapse = ", "),
                    "as they are not valid in the case of a",
                    fam_tag, "with identity link."))
    }
    if (length(use_model) == 0) {
      stop(paste("None of the model(s) specified are valid for a",
                 fam_tag, "with identity link."))
    } else {
      model <- use_model
    }
  }
  if (link_tag == "identity" &
        fam_tag %in% c("Gamma", "poisson", "negbinomial")) {
    use_model <-  model[!model %in% c("neclin", "neclinhorme",
                                      "ecxlin", "nechormepwr01")]
    drop_model <- setdiff(model, use_model)
    if (length(drop_model) > 0) {
      message(paste("Dropping the model", paste0(drop_model, collapse = ", "),
                    "as they are not valid in the case of a",
                    fam_tag, "with identity link."))
    }
    if (length(use_model) == 0) {
      stop(paste("None of the model(s) specified are valid for a",
                 fam_tag, "with identity link."))
    } else {
      model <- use_model
    }
  }
  if (fam_tag == "gaussian") {
    use_model <-  model[!model %in% mod_groups$zero_bounded]
    drop_model <- setdiff(model, use_model)
    if (length(drop_model) > 0) {
      message(paste("Dropping the model(s)",
                    paste0(drop_model, collapse = ", "),
                    "as they are not valid in the case of gaussian y data."))
    }
    if (length(use_model) == 0) {
      stop("None of the model(s) specified are valid for gaussian y data.")
    } else {
      model <- use_model
    }
  }
  model
}
