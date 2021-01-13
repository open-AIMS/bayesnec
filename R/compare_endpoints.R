#' compare_endpoints
#'
#' Extracts posterior predicted values from a list of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesnecfit}} model fits and
#' compares these via bootstrap re sampling.
#'
#' @param x A named list \code{\link[base]{list}} of objects of class
#' \code{\link{bayesnecfit}} or \code{\link{bayesnecfit}} returned by
#' \code{\link{bnec}}.
#' @param comparison The posterior predictions to compare, takes values of
#' "nec", "nsec", "ecx" or "fitted".
#' @param n_samples The number of bootstrap iterations to perform.
#'
#' @inheritParams ecx
#' @inheritParams nsec
#'
#' @seealso \code{\link{bnec}}
#'
#' @return A named list containing bootstrapped differences in posterior
#' predictions of the \code{\link{bayesnecfit}} or \code{\link{bayesnecfit}}
#' model fits contained in x. See Details.
#'
#' @importFrom stats quantile predict
#' @importFrom dplyr %>% mutate bind_rows 
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
#' compare_posterior(list(exmp_2, exmp_3), ecx_val = 50)
#' }
#'
#' @export
compare_endpoints <- function(x, comparison = "nec", ecx_val = 10, 
                              type = "absolute", hormesis_def = "control",
                              sig_val = 0.01, precision, x_range = NA) {
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
 
  if (comparison == "nec") {
    posterior_list <- lapply(x, function(m) {
      if (class(m) == "bayesnecfit") {
        out <- unname(m$nec_posterior)
      }
      if (class(m) == "bayesmanecfit") {
        out <- unname(m$w_nec_posterior)
      }
      return(out)
    })
  }
  if (comparison == "ecx") {
    posterior_list <- lapply(x, ecx, ecx_val = ecx_val, precision = precision,
                             posterior = TRUE, type = type,
                             hormesis_def = hormesis_def, x_range = x_range)
  }
  if (comparison == "nsec") {
    posterior_list <- lapply(x, nsec, sig_val = sig_val, precision = precision,
                             posterior = TRUE, hormesis_def = hormesis_def,
                             x_range = x_range)
  }

  names(posterior_list) <- names(x)
  n_samples <- min(sapply(posterior_list, length)) 
  r_posterior_list <- lapply(posterior_list, FUN = function(m){m[sample(seq_len(n_samples), replace = FALSE)]})
  posterior_data <- do.call("cbind", r_posterior_list) %>%
      data.frame %>%
      pivot_longer(cols = everything(), names_to = "model") %>% 
      arrange(model) %>% 
      data.frame() 
  
  all_combn <- combn(names(x), 2, simplify = FALSE)
  diff_list <- lapply(all_combn, FUN = function(a){
    r_posterior_list[[a[1]]]-r_posterior_list[[a[2]]]})
  names(diff_list) <- sapply(all_combn, FUN=function(m){paste(m[1], m[2], sep="-")})
  
  diff_data_out <- bind_rows(diff_list, .id = "comparison") %>% 
    pivot_longer(everything(), names_to = "comparison", values_to = "diff") %>% 
    data.frame()

  prob_diff <- lapply(diff_list, FUN = function(m){
    m[m>0] <- 1
    m[m<=0] <- 0
    prob=mean(m) 
  })

  prob_diff_out <- bind_rows(prob_diff, .id = "comparison") %>% 
    data.frame()

  return(list(posterior_list = posterior_list,
       posterior_data = posterior_data,
       diff_list = diff_list,
       diff_data = diff_data_out,
       prob_diff = prob_diff_out))
}

