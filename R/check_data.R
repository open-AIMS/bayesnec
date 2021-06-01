#' check_data
#'
#' Check data input for a Bayesian NEC model fit
#'
#' @inheritParams bnec
#'
#' @details
#'
#' This is a wrapper function to test input data criteria and find the
#' correct priors for use in \code{\link{fit_bayesnec}}.
#'
#' @importFrom stats na.omit
#' @return A \code{\link[base]{list}} of modified elements
#' necessary for \code{\link{fit_bayesnec}}.
check_data <- function(data, x_var, y_var,
                       trials_var, family,
                       model, random_vars = NA) {
  use_vars <- na.omit(c(y_var = y_var, x_var = x_var, trials_var))
  var_colms <- match(use_vars, colnames(data))
  missing_colms <- data.frame(val = use_vars[which(is.na(var_colms))],
                              stringsAsFactors = FALSE)
  missing_colms$element <- rownames(missing_colms)
  if (length(na.omit(var_colms)) < length(use_vars)) {
    stop(paste0("Your indicated ",
                paste(paste0(missing_colms$element, " '",
                             missing_colms$val, "'"),
                      collapse = ", "),
               " is not present in your input data. ",
               "Has this been misspecified?"))
  }

  y_dat <- data[, y_var]
  x_dat <- data[, x_var]

  if (!inherits(x_dat, "numeric")) {
    stop(paste0("Your indicated x_var column ", x_var,
                " contains data that is class ", class(x_dat),
                ". The function bnec requires the concentration",
                " data (argument x_var) to be numeric."))
  }
  if (any(x_dat < 0) & model == "necsigm") {
    message("necsigm should only be called when x values are >= 0")
    stop()
  }
  test_x <- mean(x_dat)
  test_y <- mean(y_dat)
  if (!is.finite(test_x)) {
    stop("Your x_var column contains values that are not finite.")
  }
  if (!is.finite(test_y)) {
    stop("Your y_var column contains values that are not finite.")
  }

  resp_check <- mean(y_dat[which(x_dat < mean(x_dat))]) <
    mean(y_dat[which(x_dat > mean(x_dat))])
  if (resp_check & !grepl("horme", model)) {
    stop("The mean value of the response for the lower half of the ",
         "concentration data are lower than that of the upper half ",
         "of the concentration data. bnec only fits concentration ",
         "response data where the response declines with increasing ",
         "values of concentration.")
  }

  if (!model %in% c("neclin", "nec3param", "necsigm", "nec4param",
                    "nechorme", "neclinhorme", "nechorme4",
                    "ecx4param", "ecxwb1", "ecxwb2", "ecxwb1p3", "ecxwb2p3", "ecxlin",
                    "ecxexp", "ecxsigm", "ecxll3", "ecxll4", "ecxll5", "ecxhormebc4", "ecxhormebc5", 
                    "nechormepwr", "nechorme4pwr","nechormepwr01")) {
    stop(paste("The model", model, "is not a valid model name.",
               "Please check ?bnec for valid model calls."))
  }


  fam_tag <- family$family

  x_type <- set_distribution(x_dat)
  if (min(data[, x_var]) == 0 & x_type == "Gamma") {
    tt <- data[, x_var]
    min_val <- min(tt[which(tt > 0)])
    data[which(tt == 0), x_var] <- tt[which(tt == 0)] + (min_val / 10)
  }

  if (min(data[, y_var]) == 0 & fam_tag == "Gamma") {
    tt <- data[, y_var]
    min_val <- min(tt[which(tt > 0)])
    data[which(tt == 0), y_var] <- tt[which(tt == 0)] + (min_val / 10)
  }

  if (min(data[, x_var]) == 0 & x_type == "beta") {
    tt <- data[, x_var]
    min_val <- min(tt[which(tt > 0)])
    data[which(tt == 0), x_var] <- tt[which(tt == 0)] + (min_val / 10)
  }

  if (min(data[, y_var]) == 0 & fam_tag == "beta") {
    tt <- data[, y_var]
    min_val <- min(tt[which(tt > 0)])
    data[which(tt == 0), y_var] <- tt[which(tt == 0)] + (min_val / 10)
  }

  if (max(data[, x_var]) == 1 & x_type == "beta") {
    tt <- data[, x_var]
    data[which(tt == 1), x_var] <- tt[which(tt == 1)] - 0.001
  }

  if (max(data[, y_var]) == 1 & fam_tag == "beta") {
    tt <- data[, y_var]
    data[which(tt == 1), y_var] <- tt[which(tt == 1)] - 0.001
  }

  mod_dat <- data.frame(x = data[, x_var],
                        y = data[, y_var],
                        trials = nrow(data))
  if(!is.na(random_vars[1])){
    rand_dat <- data.frame(data[, random_vars])
    colnames(rand_dat) <- random_vars
    mod_dat <- cbind(mod_dat, rand_dat)
  }

  response <- data[, y_var]

  custom_name <- check_custom_name(family)
  if (fam_tag == "binomial" | custom_name == "beta_binomial2") {
    mod_dat$trials <- data[, trials_var]
    response <- data[, y_var] / data[, trials_var]
  }
  priors <- define_prior(model = model, family = family,
                         predictor = mod_dat$x, response = response)
  list(priors = priors,
       mod_dat = mod_dat,
       family = family)
}
