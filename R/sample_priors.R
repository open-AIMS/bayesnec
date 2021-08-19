#' sample_priors
#'
#' Creates list or generates a plot of prior samples
#'
#' @param priors An object of class \code{\link[brms]{brmsprior}} from package
#' \pkg{brms}.
#' @param n_samples The number of prior samples to return.
#' @param plot NA returns a \code{\link[base]{list}} of numeric vectors of
#' sampled priors, "ggplot" (default) returns a \code{\link[ggplot2]{ggplot}}
#'  and "base" returns a histogram in base R.
#'
#' @importFrom stats rgamma rnorm rbeta runif
#' @importFrom graphics hist
#' @importFrom ggplot2 ggplot aes geom_histogram facet_wrap
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect starts_with
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#'
#' @seealso \code{\link{bnec}}
#' @return A \code{\link[base]{list}} containing the initialisation values.
#'
#' @export
sample_priors <- function(priors, n_samples = 10000, plot = "ggplot") {
  fcts <- c(gamma = rgamma, normal = rnorm, beta = rbeta, uniform = runif)
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
        out[[j]] <- sample(out[[j]][which(bound_fct(out[[j]], bounds) ==
                                      FALSE)],
                           n_samples, replace = TRUE)
      }
    }
  }
  names(out) <- par_names
  if (plot == "base") {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))      
    par(mfrow = c(ceiling(nrow(priors) / 2), 2))
    for (j in seq_along(out)) {
      hist(out[[j]], main = names(out)[j])
    }
  }
  if (plot == "ggplot") {
    plot <- do.call("cbind", out) %>%
      data.frame %>%
      pivot_longer(names_to = "param", values_to = "value",
                   cols = starts_with("b_")) %>%
      ggplot(mapping = aes(x = .data$value)) +
        geom_histogram() +
        facet_wrap(~.data$param, scales = "free_x")
    return(plot)
  }
  if (is.na(plot)) {
    return(out)
  }
}
