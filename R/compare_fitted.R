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
#' @importFrom dplyr %>% mutate bind_rows
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
 n_samples <- min(sapply(x, FUN = function(v){v$sample_size}))  
 posterior_list <- lapply(x, function(m, ...) {
      predict(m, ...)$posterior
    }, precision = precision, x_range = x_range)
 x_vec <- seq(min(x_range), max(x_range), length = precision)
    
 names(posterior_list) <- names(x)
 
 r_posterior_list <- lapply(posterior_list, FUN = function(m){m[sample(seq_len(n_samples), replace = FALSE), ]})
 posterior_data <- posterior_list %>%
      lapply(summarise_posterior, x_vec = x_vec) %>%
      bind_rows(.id = "model") %>%
      data.frame
 all_combn <- combn(names(x), 2, simplify = FALSE)
 diff_list <- lapply(all_combn, FUN = function(a){
      r_posterior_list[[a[1]]]-r_posterior_list[[a[2]]]})
 names(diff_list) <- sapply(all_combn, FUN=function(m){paste(m[1], m[2], sep="-")})
 diff_data <- lapply(diff_list, FUN = function(m){
   out_diffs <- t(apply(m, MARGIN = 2, FUN = quantile, probs = c(0.5, 0.025, 0.975))) %>% 
     data.frame() %>% 
     mutate(x=x_vec)
 })
 diff_data_out <- bind_rows(diff_data, .id = "comparison")
 colnames(diff_data_out) <- c("comparison", "diff.Estimate", "diff.Q2.5", "diff.Q97.5", "x")
 
 prob_diff <- lapply(diff_list, FUN = function(m){
   m[m>0] <- 1
   m[m<=0] <- 0
   cbind(x=x_vec, prob=colMeans(m)) %>% 
     data.frame()
 })
  
 prob_diff_out <- bind_rows(prob_diff, .id = "comparison")

 return(list(posterior_list = posterior_list,
       posterior_data = posterior_data,
       diff_list = diff_list,
       diff_data = diff_data_out,
       prob_diff = prob_diff_out))
}

