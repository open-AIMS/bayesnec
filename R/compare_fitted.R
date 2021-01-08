#' compare_fitted
#'
#' Extracts posterior predicted values from a list of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesnecfit}} model fits and
#' compares these across a vector of fitted values.
#'
#' @param x A named list \code{\link[base]{list}} of objects of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesnecfit}} returned by
#' \code{\link{bnec}}.
#' 
#' @param precision the number of x values over which to predict values.
#'
#' @param x_range The range of x values over which to make predictions.
#'
#' @seealso \code{\link{bnec}}
#'
#' @return A named list containing bootstrapped differences in posterior
#' predictions of the \code{\link{bayesnecfit}} or \code{\link{bayesnecfit}}
#' model fits contained in x. See Details.
#'
#' @importFrom stats quantile predict
#' @importFrom dplyr %>% mutate bind_rows group_by summarise ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect everything
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
#' compare_fitted(list(exmp_2, exmp_3))
#' }
#'
#' @export
compare_fitted <- function(x, precision = 100, x_range = NA) {
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

  posterior_list <- lapply(x, function(m, ...) {
      predict(m, ...)$posterior
    }, precision = precision, x_range = x_range)
    x_vec <- seq(min(x_range), max(x_range), length = precision)
    
  names(posterior_list) <- names(x)
 # mat_a <- mat_a[sample(seq_len(nrow(mat_a))), ]
  
  
  posterior_data <- posterior_list %>%
      lapply(summarise_posterior, x_vec = x_vec) %>%
      bind_rows(.id = "model") %>%
      data.frame
  
 diff_list <- boot_sample_fitted(posterior_list, n_samples, x_vec)
 prob_diff <- extract_probs_fitted(diff_list)
 
 diff_data <- extract_diffs_fitted(diff_list) #%>%
    mutate(index = 1) %>%
      group_by(x, comparison) %>%
      summarise(prob = sum(gr_0) / sum(index),
                diff.Estimate = quantile(diff, probs = 0.5),
                diff.Q2.5 = quantile(diff, probs = 0.025),
                diff.Q97.5 = quantile(diff, probs = 0.975),
                .groups = "keep") %>%
      ungroup %>%
      mutate(x = as.numeric(as.character(x))) %>%
      data.frame
  
  list(posterior_list = posterior_list,
       posterior_data = posterior_data,
       diff_list = diff_list,
       diff_data = diff_data,
       prob_diff = prob_diff)
}

