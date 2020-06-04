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
                       trials_var, x_type = NA, family = NA,
                       over_disp, model) {

  if (!is.na(family)) {
    if (over_disp & family == "Beta") {
      family <- NA
    }
    if (!family %in% mod_fams) {
      stop(paste("You have specified family as",
                 family,
                 "which is not currently implemented."))
    }
  }

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
  if (resp_check & model != "nechorme") {
    stop("The mean value of the response for the lower half of the ",
         "concentration data are lower than that of the upper half ",
         "of the concentration data. bnec only fits concentration ",
         "response data where the response declines with increasing ",
         "values of concentration.")
  }

  if (is.na(x_type)) {
    x_type <- set_distribution(x_dat)
  }

  if (is.na(family)) {
    if (is.na(trials_var)) {
      family <- set_distribution(y_dat, support_integer = TRUE)
    } else {
      family <- set_distribution(y_dat, support_integer = TRUE,
                                 trials = data[, trials_var])
    }
  }

  if (!model %in% c("nec3param", "necsigm", "nec4param", "nechorme",
                    "ecx4param", "ecxwb1", "ecxwb2", "ecxlin",
                    "ecxexp", "ecxsigm")) {
    stop("The model type you have specified does not exist.")
  }

  if (family == "poisson" & over_disp) {
    family <- "negbinomial"
  }
  if (family == "binomial" & over_disp) {
    family <- "Beta"
    data[, y_var] <-  data[, y_var] / data[, trials_var]
  }

  if (min(data[, x_var]) == 0 & x_type == "Gamma") {
    tt <- data[, x_var]
    min_val <- min(tt[which(tt > 0)])
    data[which(tt == 0), x_var] <- tt[which(tt == 0)] + (min_val / 10)
  }

  if (min(data[, y_var]) == 0 & family == "Gamma") {
    tt <- data[, y_var]
    min_val <- min(tt[which(tt > 0)])
    data[which(tt == 0), y_var] <- tt[which(tt == 0)] + (min_val / 10)
  }

  if (min(data[, x_var]) == 0 & x_type == "Beta") {
    tt <- data[, x_var]
    min_val <- min(tt[which(tt > 0)])
    data[which(tt == 0), x_var] <- tt[which(tt == 0)] + (min_val / 10)
  }

  if (min(data[, y_var]) == 0 & family == "Beta") {
    tt <- data[, y_var]
    min_val <- min(tt[which(tt > 0)])
    data[which(tt == 0), y_var] <- tt[which(tt == 0)] + (min_val / 10)
  }

  if (max(data[, x_var]) == 1 & x_type == "Beta") {
    tt <- data[, x_var]
    data[which(tt == 1), x_var] <- tt[which(tt == 1)] - 0.001
  }

  if (max(data[, y_var]) == 1 & family == "Beta") {
    tt <- data[, y_var]
    data[which(tt == 1), y_var] <- tt[which(tt == 1)] - 0.001
  }

  mod_dat <- data.frame(x = data[, x_var],
                        y = data[, y_var],
                        N = nrow(data))

  response <- data[, y_var]

  if (family == "binomial") {
    mod_dat$trials <- data[, trials_var] # number of "trials"
    response <- data[, y_var] / data[, trials_var]
  }

  mod_file <- define_prior(model = model, x_type = x_type,
                           family = family, mod_dat = mod_dat)
  priors <- mod_file$priors
  mod_family <- mod_file$mod_family

  list(priors = priors,
       response = response,
       mod_dat = mod_dat,
       data = data,
       family = family,
       x_type = x_type,
       x_dat = x_dat,
       y_dat = y_dat,
       mod_family = mod_family)
}
