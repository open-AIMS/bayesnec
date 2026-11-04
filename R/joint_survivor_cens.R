#' Preserve known survival in a joint censored likelihood
#'
#' @details brms left censoring includes the zero mass. The interval from zero
#' to the bound excludes it; the original response stays available for priors
#' and diagnostics.
#'
#' @noRd
mark_joint_survivor_cens <- function(formula, data, family) {
  if (!family %in% c("hurdle_gamma", "zero_inflated_beta")) {
    return(formula)
  }
  cens_call <- hurdle_lhs_parts(formula[[2]])$aterms[["cens"]]
  if (is.null(cens_call)) {
    return(formula)
  }
  cens <- joint_survivor_cens_values(cens_call, data, formula)
  if (!any(cens == -1L, na.rm = TRUE)) {
    return(formula)
  }
  if (identical(family, "zero_inflated_beta")) {
    stop("bnec_joint cannot refit a known left-censored survivor with a",
         " zero_inflated_beta family. The interval from zero to its bound",
         " requires beta_lccdf(0), which prevents Stan from initialising",
         " when its shape parameters are sampled. Keep the factorised",
         " bnec_hurdle() fit, or supply a formula without cens() if treating",
         " the bound as an exact observation is scientifically justified.",
         call. = FALSE)
  }
  bases <- c(".bayesnec_survivor_left", ".bayesnec_joint_cens",
             ".bayesnec_joint_upper")
  names <- tail(make.unique(c(names(data), bases), sep = "_"), 3)
  attr(formula, "bayesnec_survivor_cens") <- stats::setNames(
    as.list(names), c("left", "code", "upper")
  )
  formula
}

#' @noRd
joint_survivor_cens_values <- function(cens_call, data, formula) {
  if (length(cens_call) < 2) {
    stop("A cens() term needs a censoring indicator.", call. = FALSE)
  }
  values <- eval(cens_call[[2]], envir = data,
                 enclos = environment(formula))
  if (!length(values) %in% c(1L, nrow(data))) {
    stop("The cens() indicator must have one value per row, or one value",
         " for every row.", call. = FALSE)
  }
  rep_len(normalise_cens(values), nrow(data))
}

#' @noRd
joint_survivor_cens_data <- function(formula, data, spec) {
  cens_call <- hurdle_lhs_parts(formula[[2]])$aterms[["cens"]]
  cens <- joint_survivor_cens_values(cens_call, data, formula)
  y_var <- hurdle_response_var(formula)
  y <- data[[y_var]]
  upper <- if (length(cens_call) >= 3) {
    rep_len(eval(cens_call[[3]], envir = data,
                 enclos = environment(formula)), nrow(data))
  } else {
    y
  }
  left <- !is.na(cens) & cens == -1L
  if (any(left & y <= 0)) {
    stop("A known left-censored survivor needs a positive response bound.",
         call. = FALSE)
  }
  # For other rows, a finite upper bound keeps brms from dropping a row whose
  # optional interval-bound column contains NA.
  data[[spec$left]] <- left
  data[[spec$code]] <- ifelse(left, 2L, cens)
  data[[spec$upper]] <- ifelse(!is.na(cens) & cens == 2L, upper, y)
  data
}

#' @noRd
prepare_joint_survivor_cens <- function(formula, data, brms_bf) {
  spec <- attr(formula, "bayesnec_survivor_cens")
  if (is.null(spec)) {
    return(list(data = data, formula = brms_bf))
  }
  data <- joint_survivor_cens_data(formula, data, spec)
  y_var <- hurdle_response_var(formula)
  # brms stores the marker as a factor in some fitted-data paths.
  left <- call("==", as.name(spec$left), TRUE)
  lower <- call("I", call("ifelse", left, 0, as.name(y_var)))
  indicator <- call("cens", as.name(spec$code), as.name(spec$upper))
  brms_bf$formula[[2]] <- call("|", lower, indicator)
  list(data = data, formula = brms_bf)
}

#' @noRd
restore_joint_survivor_data <- function(fit, data, formula) {
  if (is.null(attr(formula, "bayesnec_survivor_cens"))) {
    return(fit)
  }
  needed <- intersect(all.vars(formula), names(data))
  missing <- setdiff(needed, names(fit$data))
  if (length(missing) == 0) {
    return(fit)
  }
  rows <- match(rownames(fit$data), rownames(data))
  if (anyNA(rows)) {
    stop("The fitted rows could not be matched to the recorded response",
         " and censoring data.", call. = FALSE)
  }
  for (name in missing) {
    fit$data[[name]] <- data[[name]][rows]
  }
  fit
}
