#' linear_rescale
#' @param x A \code{\link[base]{numeric}} vector.
#' @param r_out A \code{\link[base]{numeric}} vector of length 2 containing
#' the new range of values in x.
#' @return A \code{\link[base]{numeric}} vector.
#' @noRd
linear_rescale <- function(x, r_out) {
  p <- (x - min(x)) / (max(x) - min(x))
  r_out[[1]] + p * (r_out[[2]] - r_out[[1]])
}

#' check_custom_name
#' @param family An object of class \code{\link[stats]{family}} or
#' \code{\link[brms]{brmsfamily}}.
#' @return A \code{\link[base]{character}} vector containing the brms
#' custom family or NA.
#' @importFrom brms fixef
#' @noRd
check_custom_name <- function(family) {
  custom_name <- "none"
  if (inherits(family, "customfamily")) {
    custom_name <- family$name
  }
  custom_name
}

#' extract_pars
#' @param x A \code{\link[base]{character}} vector.
#' @param model_fit An object of class \code{\link[brms]{brmsfit}}.
#' @return A named \code{\link[base]{numeric}} vector or NA.
#' @importFrom brms fixef
#' @noRd
extract_pars <- function(x, model_fit) {
  fef <- fixef(model_fit, robust = TRUE)
  tt <- fef[grep(x, rownames(fef)), c("Estimate", "Q2.5", "Q97.5")]
  if (is.na(tt["Estimate"])) {
    NA
  } else {
    tt
  }
}

#' min_abs
#' @param x A \code{\link[base]{numeric}} vector.
#' @return A \code{\link[base]{numeric}} vector.
#' @noRd
min_abs <- function(x) {
  which.min(abs(x))
}

#' paste_normal_prior
#'
#' Creates prior string given a number
#'
#' @param mean A \code{\link[base]{numeric}} vector.
#' @param param A \code{\link[base]{character}} vector indicating the
#' target non-linear parameter.
#' @param sd A \code{\link[base]{numeric}} vector indicating the
#' standard deviation.
#' @param ... Additional arguments of \code{\link[brms]{prior_string}}.
#'
#' @return A \code{\link[base]{character}} vector.
#' @importFrom brms prior_string
#' @noRd
paste_normal_prior <- function(mean, param, sd = 1, ...) {
    prior_string(paste0("normal(", mean, ", ", sd, ")"), nlpar = param, ...)
}

#' @noRd
extract_dispersion <- function(x) {
  x$dispersion
}

#' @noRd
extract_loo <- function(x) {
  x$fit$loo
}

#' @noRd
extract_waic <- function(x) {
  x$fit$waic$estimates["waic", "Estimate"]
}

#' @noRd
w_nec_calc <- function(index, mod_fits, sample_size, mod_stats) {
  sample(mod_fits[[index]]$nec_posterior,
         as.integer(round(sample_size * mod_stats[index, "wi"])))
}

#' @noRd
w_pred_calc <- function(index, mod_fits, mod_stats) {
  mod_fits[[index]]$predicted_y * mod_stats[index, "wi"]
}

#' @noRd
w_post_pred_calc <- function(index, mod_fits, sample_size, mod_stats) {
  x <- seq_len(sample_size)
  size <- round(sample_size * mod_stats[index, "wi"])
  mod_fits[[index]]$pred_vals$posterior[sample(x, size), ]
}

#' @noRd
w_pred_list_calc <- function(index, pred_list, sample_size, mod_stats) {
  x <- seq_len(sample_size)
  size <- round(sample_size * mod_stats[index, "wi"])
  pred_list[[index]][sample(x, size), ]
}

#' @noRd
do_wrapper <- function(..., fct = "cbind") {
  do.call(fct, lapply(...))
}

#' @noRd
#' @importFrom stats median quantile
estimates_summary <- function(x) {
  x <- c(median(x), quantile(x, c(0.025, 0.975)))
  names(x) <- c("Estimate", "Q2.5", "Q97.5")
  x
}

#' @noRd
handle_set <- function(x, add, drop) {
  msets <- names(mod_groups)
  tmp <- x
  if (!missing(add)) {
    y <- add
    if (any(add %in% msets)) {
      y <- unname(unlist(mod_groups[intersect(add, msets)]))
      y <- setdiff(union(y, add), msets)
    }
    tmp <- union(tmp, y)
  }
  if (!missing(drop)) {
    y <- drop
    if (any(drop %in% msets)) {
      y <- unname(unlist(mod_groups[intersect(drop, msets)]))
    }
    tmp <- setdiff(tmp, y)
    if (length(tmp) == 0) {
      stop("All models removed, nothing to return;\n",
           "Perhaps try calling function bnec with another ",
           "model set.")
    }
  }
  if (identical(sort(x), sort(tmp))) {
    message("Nothing to amend, please specify a model to ",
            "either add or drop that differs from the original set.")
    FALSE
  } else {
    tmp
  }
}

#' allot_class
#'
#' Assigns class to an object.
#'
#' @param x An object.
#' @param new_class The new object class.
#'
#' @return An object of class new_class.
#' @noRd
allot_class <- function(x, new_class) {
  class(x) <- new_class
  x
}

#' @noRd
expand_and_assign_nec <- function(x, ...) {
  allot_class(expand_nec(x, ...), "bayesnecfit")
}

#' are_chains_correct
#'
#' Checks if number of chains in brmsfit object is correct
#'
#' @param brms_fit An object of class \code{\link[brms]{brmsfit}}.
#' @param chains The expected number of correct chains.
#'
#' @return A \code{\link[base]{logical}} vector.
#' @noRd
are_chains_correct <- function(brms_fit, chains) {
  fit_chs <- brms_fit$fit@sim$chains
  if (is.null(fit_chs)) {
    FALSE
  } else {
    fit_chs == chains
  }
}

#' @noRd
get_init_ranges <- function(y, x, fct, .args) {
  y <- y[match(.args, names(y))]
  y <- lapply(y, as.numeric)
  y[["x"]] <- x
  range(do.call("fct", y))
}

#' @noRd
check_limits <- function(x, limits) {
  min(x) >= min(limits) & max(x) <= max(limits)
}

#' @noRd
clean_names <- function(x) {
  paste0("Q", gsub("%", "", names(x), fixed = TRUE))
}

#' @noRd
modify_posterior <- function(n, object, x_vec, p_samples, hormesis_def) {
  posterior_sample <- p_samples[n, ]
  if (hormesis_def == "max") {
    target <- x_vec[which.max(posterior_sample)]
    change <- x_vec < target
  } else if (hormesis_def == "control") {
    target <- posterior_sample[1]
    change <- posterior_sample >= target
  }
  posterior_sample[change] <- NA
  posterior_sample
}

#' extract_warnings
#'
#' Extract warnings from brmsfit object
#'
#' @param x An object of class \code{\link[brms]{brmsfit}}.
#'
#' @importFrom evaluate evaluate is.warning
#'
#' @return A \code{\link[base]{list}} containing all warning messages.
#' @noRd
extract_warnings <- function(x) {
  x <- evaluate("identity(x)", new_device = FALSE)
  to_extract <- which(sapply(x, is.warning))
  if (length(to_extract) > 0) {
    x[to_extract]
  } else {
    NULL
  }
}

#' @noRd
has_r_hat_warnings <- function(...) {
  x <- extract_warnings(...)
  any(grepl("some Rhats are > 1.05", x, fixed = TRUE))
}

#' @noRd
print_mat <- function(x, digits = 2) {
  fmt <- paste0("%.", digits, "f")
  out <- x
  for (i in seq_len(ncol(x))) {
    out[, i] <- sprintf(fmt, x[, i])
  }
  print(out, quote = FALSE, right = TRUE)
  invisible(x)
}

#' @noRd
clean_mod_weights <- function(x) {
  a <- x$mod_stats[, !sapply(x$mod_stats, function(z)all(is.na(z)))]
  as.matrix(a[, -1])
}

#' @noRd
clean_nec_vals <- function(x) {
  mat <- t(as.matrix(x$w_nec))
  rownames(mat) <- "NEC"
  mat
}

#' @noRd
nice_ecx_out <- function(ec, ecx_tag) {
  cat(ecx_tag)
  cat("\n")
  mat <- t(as.matrix(ec))
  rownames(mat) <- "Estimate"
  print_mat(mat)
}

#' @noRd
contains_zero <- function(x) {
  sum(x == 0, na.rm = TRUE) >= 1
}

#' @noRd
contains_one <- function(x) {
  sum(x == 1, na.rm = TRUE) >= 1
}

#' @noRd
response_link_scale <- function(response, family) {
  link_tag <- family$link
  min_n0val <- min(response[which(response > 0)])/100
  if (link_tag == "logit") {  
    max_n1val <- max(response[which(response < 1)]) + (1-max(response[which(response < 1)]))*0.99
  }  
  
  custom_name <- check_custom_name(family)
  if (link_tag %in% c("logit", "log")) {
    if (custom_name == "beta_binomial2") {
      if(contains_zero(response)){
        response <- linear_rescale(response, r_out = c(min_n0val, max(response)))
      }
      if(contains_one(response)){
        response <- linear_rescale(response, r_out = c(min(response), max_n1val))
      }
      response <- binomial(link = link_tag)$linkfun(response)
    } else if (family$family == "binomial") {
      if(contains_zero(response)){
        response <- linear_rescale(response, r_out = c(min_n0val, max(response)))
      }
      if(contains_one(response)){
        response <- linear_rescale(response, r_out = c(min(response), max_n1val))
      }
      response <- family$linkfun(response)
    } else {
      if(contains_zero(response)){
        response <- linear_rescale(response, r_out = c(min_n0val, max(response)))
      }
      response <- family$linkfun(response)       
    }
  }
  response
}

#' @noRd
rounded <- function(value, precision = 1) {
  sprintf(paste0("%.", precision, "f"), round(value, precision))
}

#' @noRd
#' @importFrom dplyr %>%
return_x_range <- function(x) {
  return_x <- function(object) {
    if (is_bayesmanecfit(object)) {
      object$w_pred_vals$data$x
    } else if (is_bayesnecfit(object)) {
      object$pred_vals$data$x
    } else {
      stop("Not all objects in x are of class bayesnecfit or bayesmanecfit.")
    }
  }
  lapply(x, return_x) %>%
    unlist %>%
    range(na.rm = TRUE)
}

#' @noRd
return_nec_post <- function(m, xform) {
  if (is_bayesnecfit(m)) {
    out <- unname(m$nec_posterior)
  }
  if (is_bayesmanecfit(m)) {
    out <- unname(m$w_nec_posterior)
  }
  if (inherits(xform, "function")) {
    out <- xform(out)
  }
  out
}

#' @noRd
gm_mean <- function(x, na_rm = TRUE, zero_propagate = FALSE) {
  if (any(x < 0, na.rm = TRUE)) {
    return(NaN)
  }
  if (zero_propagate) {
    if (any(x == 0, na.rm = TRUE)) {
      return(0)
    }
    exp(mean(log(x), na.rm = na_rm))
  } else {
    exp(sum(log(x[x > 0]), na.rm = na_rm) / length(x))
  }
}

#' @noRd
summarise_posterior <- function(mat, x_vec) {
  cbind(x = x_vec, data.frame(t(apply(mat, 2, estimates_summary))))
}
