#' sample_priors
#'
#' Creates list or generates a plot of prior samples
#'
#' @inheritParams bnec
#' 
#' @param priors an object of class "brmsprior" from package \pkg{brms}.
#' 
#' @param plot NA returns a list of numeric vectors of sampled priors, 
#' "ggplot" returns a ggplot and 
#' "base" returns a histogram in base R
#' 
#' @param n_samples the number of prior samples to return
#'
#' @importFrom stats rgamma rnorm rbeta runif
#' 
#' @seealso \code{\link{bnec}}
#' @return A \code{\link[base]{list}} containing the initialisation values.
sample_priors <- function(priors, n_samples = 10000, plot = "ggplot") {
  fcts <- c(gamma = rgamma,
            normal = rnorm,
            beta = rbeta,
            uniform = runif)
  priors <- as.data.frame(priors)
  priors <- priors[priors$prior != "", ]
  par_names <- character(length = nrow(priors))
  for (j in seq_along(par_names)) {
    sep <- ifelse(priors$class[j] == "b", "_", "")
    par_names[j] <- paste(priors$class[j],
                          priors$nlpar[j],
                          sep = sep)
  }
  out <- vector(mode = "list", length = nrow(priors))
    for (j in seq_len(nrow(priors))) {
      bits <- gsub("\\(|\\)", ",", priors$prior[j])
      bits <- strsplit(bits, ",", fixed = TRUE)[[1]]
      fct_i <- bits[1]
      v1 <- as.numeric(bits[2])
      v2 <- as.numeric(bits[3])

        out[[j]] <- fcts[[fct_i]](n_samples, v1, v2)
        if (priors$bound[j] != "") {
          to_keep <- "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?"
          bounds <- regmatches(priors$bound[j],
                               gregexpr(to_keep, priors$bound[j]))[[1]]
          bounds <- as.numeric(bounds)
          
          
          if (length(bounds) == 2) {
            out[[j]] <- sample(out[[j]][which(out[[j]] >= min(bounds) &
                                              out[[j]] <= max(bounds))],
                               n_samples, replace = TRUE)
            
          } else if (length(bounds) == 1) {
            bound_fct <- ifelse(grepl("lower", priors$bound[j]), `<=`, `>=`)
            out[[j]] <- sample(out[[j]][which(bound_fct(out[[j]], bounds)==FALSE)], 
                               n_samples, replace = TRUE) 

          }
        }
      }
names(out) <- par_names
if(plot=="base"){
  par(mfrow=c(ceiling(nrow(priors)/2), 2))
  for(j in 1:length(out)){
    hist(out[[j]], main=names(out)[j])
  }
}  
if(plot=="ggplot"){
  plot <- do.call("cbind", out) %>% 
    data.frame() %>% 
    tidyr::pivot_longer(names_to = "param", 
                        values_to = "value",
                        cols = starts_with("b_")) %>% 
    ggplot(aes(value)) +
    geom_histogram() +
    facet_wrap(~param, scales = 'free_x')
  return(plot)
}  

 if(is.na(plot)) {return(out)}
}

