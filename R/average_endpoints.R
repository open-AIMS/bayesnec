#' average_endpoints
#'
#' Extracts posterior predicted endpoint values from a list of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesnecfit}} model fits and
#' calculates a geometric mean.
#' 
#' @inheritParams compare_posterior
#' @inheritParams ecx
#' @inheritParams nsec
#' 
#' @param endpoint The type of endpoint to use in the mean. Takes values "nec", "ecx" or "nsec"
#' 
#' @details The goemetric mean of values are simply the mean calculated on a log scale and back transformed through exp(), although we have added the capacity to accommodate zero values. Note that the function assumes that x has been modelled on the natural scale. Often C-R models are more stable on a log transformed or sqrt scaling. If the input \code{\link{bayesnecfit}} or \code{\link{bayesnecfit}} model fits are already based on a re-scaling of the x (concentration) axis, it is important to pass an appropriate xform argument to ensure these are back transformed before the the geometric mean calculation is applied.
#'
#' @seealso \code{\link{bnec}}
#'
#' @return The geometric mean of the endpoints estimate values
#' of the \code{\link{bayesnecfit}} or \code{\link{bayesnecfit}}
#' model fits contained in x. See Details.
#'
#' @importFrom stats quantile predict
#' @importFrom dplyr %>% mutate bind_rows arrange
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect everything
#' @importFrom utils combn
#'
#' @examples
#' \dontrun{
#' library(brms)
#' library(bayesnec)
#' options(mc.cores = parallel::detectCores())
#' data(nec_data)
#'
#' exmp <- bnec(data = nec_data, x_var = "x", y_var = "y",
#'              model = c("ecx4param", "nec4param"),
#'              family = Beta(link = "identity"), priors = my_priors,
#'              iter = 1e4, control = list(adapt_delta = 0.99))
#' exmp_2 <- pull_out(exmp, "nec4param")
#' exmp_3 <- pull_out(exmp, "ecx4param")
#' average_endpoints(list(exmp_2, exmp_3), ecx_val = 50)
#' }
#'
#' @export
average_endpoints <- function(x, endpoint = "nec", ecx_val = 10, posterior = FALSE,
                              type = "absolute", hormesis_def = "control",
                              sig_val = 0.01, precision = 1000, x_range = NA, xform = NA,
                              prob_vals = c(0.5, 0.025, 0.975)) {
  
  if (class(x) != "list" | is.null(names(x))){
    stop("Argument x must be a named list")
  }
  if (class(endpoint) != "character"){
    stop("Argument endpoint must be a character vector")
  }
  
  if (is.na(x_range)){
    x_range <- range(unlist(
     lapply(x, FUN=function(l){
       if (class(l)=="bayesmanecfit"){l$w_pred_vals$data$x} else if 
         (class(l)=="bayesnecfit"){l$pred_vals$data$x} else {
           stop("Not all objects in x are of class bayesnecfit or bayesmanecfit")
         }
      })
    ), na.rm=TRUE)
  }
 
  if (endpoint == "nec") {
    posterior_list <- lapply(x, function(m) {
      if (class(m) == "bayesnecfit") {
        out <- unname(m$nec_posterior)
      }
      if (class(m) == "bayesmanecfit") {
        out <- unname(m$w_nec_posterior)
      }
      if (inherits(xform, "function")) {out <- xform(out)}
      return(out)
    })
  }
  if (endpoint == "ecx") {
    posterior_list <- lapply(x, ecx, ecx_val = ecx_val, precision = precision,
                             posterior = TRUE, type = type, 
                             hormesis_def = hormesis_def, x_range = x_range, xform = xform)
  }
  if (endpoint == "nsec") {
    posterior_list <- lapply(x, nsec, sig_val = sig_val, precision = precision,
                             posterior = TRUE, hormesis_def = hormesis_def,
                             x_range = x_range, xform = xform)
  }

  names(posterior_list) <- names(x)
  n_samples <- min(sapply(posterior_list, length)) 
  r_posterior_list <- lapply(posterior_list, FUN = function(m){m[sample(seq_len(n_samples), replace = FALSE)]})
  posterior_data <- do.call("cbind", r_posterior_list) %>%
      data.frame ()

  post_mean <- apply(posterior_data, MARGIN = 1, FUN = gm_mean)
  mean_estimate <- quantile(unlist(post_mean), probs = prob_vals)  

  if (!posterior) {
    mean_estimate        
  } else {
    post_mean
  }
}

gm_mean = function(x, na.rm=TRUE, zero.propagate = FALSE){
  if(any(x < 0, na.rm = TRUE)){
    return(NaN)
  }
  if(zero.propagate){
    if(any(x == 0, na.rm = TRUE)){
      return(0)
    }
    exp(mean(log(x), na.rm = na.rm))
  } else {
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }
}

