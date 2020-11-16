#' compare_posterior
#'
#' Extracts posterior predicted values from a list of class 
#' \code{\link{bayesnecfit}} or \code{\link{bayesnecfit}} model fits and compares these via bootstrap re sampling.
#'
#' @param x A named list \code{\link[base]{list}} of objects of class \code{\link{bayesnecfit}} or \code{\link{bayesnecfit}} returned by \code{\link{bnec}}.
#' @param comparison The posterior predictions to compare, takes values of "nec", "nsec", "ecx" or "fitted".
#' @param n_samples The number of bootstrap iterations to perform 
#' @inheritParams ecx
#' @inheritParams nsec
#' 
#' @details  
#'
#' @seealso \code{\link{bnec}}
#' 
#' @return A named list containing bootstrapped differences in posterior predictions of the \code{\link{bayesnecfit}} or \code{\link{bayesnecfit}} model fits contained in x. See Details.
#' 
#' @importFrom stats quantile predict
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
#' compare_posterior(list(exmp_2, exmp_3), ecx_val = 50)
#' }
#'
#' @export
compare_posterior <- function(x, comparison = "nec", 
                              ecx_val = 10, type = "absolute", 
                              hormesis_def = "control",
                              sig_val = 0.01, 
                              precision = 1000, x_range = NA, n_samples = 2000) {
  if (comparison == "nec") {
    posterior_list <- lapply(x, FUN=function(m){
        if (class(m)=="bayesnecfit") {
          out <- unname(m$nec_posterior)
        }
        if (class(m)=="bayesmanecfit") {
          out <- unname(m$w_nec_posterior) 
        }
      return(out)
    })
  }
  if (comparison == "ecx") {  
    posterior_list <- lapply(x, function(m){
      ecx(m, ecx_val = ecx_val, precision = precision,
          posterior = TRUE, type = type, hormesis_def = hormesis_def, x_range = x_range)
      })
  }
  if (comparison == "nsec") {  
    posterior_list <- lapply(x, function(m){
      nsec(m, sig_val = sig_val, precision = precision,
          posterior = TRUE, hormesis_def = hormesis_def, x_range = x_range)
    })
  }
  if (comparison == "fitted") {
    posterior_list <- lapply(x, FUN=function(m){
      m$pred_vals$posterior})
  }
  
  names(posterior_list) <- names(x)

  posterior_data <- do.call("cbind", posterior_list) %>% 
    data.frame() %>% 
    tidyr::pivot_longer(col = everything(), names_to = "model") 
  
  diff_list <- boot_sample(posterior_list, n_samples, comparison) 
  prob_diff <- extract_probs(diff_list)
  diff_data <- extract_diffs(diff_list)

  list_data <- list(
    posterior_list = posterior_list,
    posterior_data = posterior_data,
    diff_list = diff_list,
    diff_data = diff_data,    
    prob_diff = prob_diff

  )
  return(list_data)
}

boot_sample <- function(posterior_list, n_samples =  1000, comparison = "nec"){
  if (comparison!="fitted") {
    diff_list <- lapply(1:n_samples, FUN=function(n){
        v <- sapply(posterior_list, sample, size =  1)
        diff_mat <- outer(v, v, `-`) 
    })

   return(diff_list)  
  } else {
    stop("Comparisons of posterior predicted values are not available for the fitted comparison type yet, this is currently under development.")
  }
}

extract_probs <- function(diff_list){
  Reduce("+", lapply(diff_list, FUN = function(x){
    x[which(x>=0)] <- 1
    x[which(x<0)] <- 0  
    return(x)}))/length(diff_list)

}

extract_diffs <- function(diff_list){
  do.call("rbind", lapply(diff_list, FUN = melt_diff))
}

melt_diff <- function(diff, order = NULL, diff_name = 'diff') {
  if(!is.null(order)){
    diff <- diff[order, order]
  } else {
    order <- row.names(diff)
  }
  diag(diff) <- NA
  diff[upper.tri(diff)] <- NA
  diff_df <- as.data.frame(diff)
  diff_df$rows <- row.names(diff)
  diff_df <- diff_df %>%
    tidyr::gather_(key = "cols", value = lazyeval::interp("diff_name", diff_name = as.name(diff_name)), order, na.rm = T) %>% 
    tidyr::unite(comparison, rows, cols, sep="-")
  return(diff_df)
}




  



 