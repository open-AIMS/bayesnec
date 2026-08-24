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
#' @importFrom ggplot2 ggplot aes geom_histogram facet_wrap theme_bw labs
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect starts_with
#' @importFrom dplyr filter mutate
#' @importFrom rlang .data
#'
#' @seealso \code{\link{bnec}}
#' @return For \code{plot = NA}, a \code{\link[base]{list}} of numeric
#' vectors of sampled prior values, one per parameter. Otherwise a
#' \code{\link[ggplot2]{ggplot}} or, for \code{plot = "base"}, the histograms
#' drawn as a side effect.
#'
#' @examples
#' library(bayesnec)
#' data(manec_example)
#' exmp <- pull_brmsfit(manec_example, model = "nec4param")
#' sample_priors(exmp$prior)
#'
#' @export
sample_priors <- function(priors, n_samples = 10000, plot = "ggplot") {
  chk_numeric(n_samples)
  # NA is documented as the "return the draws" option, but `NA %in% c(...)` is
  # FALSE, so the guard rejected the very value the documentation offers and
  # there was no route to the sampled values at all. `%in%` cannot express it,
  # hence the explicit is.na() branch. The length check leads, because both
  # NULL and a length-2 `plot` reach `if` with something that is not a single
  # TRUE/FALSE and fail with an R internals message instead of this one.
  # See #244.
  if (!(length(plot) == 1 &&
          (is.na(plot) || plot %in% c("ggplot", "base")))) {
    stop("plot must be NA, or a character string of either ",
         "\"ggplot\" or \"base\"")
  }
  fcts <- c(gamma = rgamma, normal = rnorm, beta = rbeta, uniform = runif)
  priors <- as.data.frame(priors) |>
    filter(class == "b")
  priors <- priors[priors$prior != "", ]
  par_names <- character(length = nrow(priors))
  for (j in seq_along(par_names)) {
    sep <- ifelse(priors$class[j] == "b", "_", "")
    par_names[j] <- paste(priors$class[j], priors$nlpar[j], sep = sep)
  }
  out <- vector(mode = "list", length = nrow(priors))
  for (j in seq_len(nrow(priors))) {
    # A constant() prior is a point mass: every draw is the fixed value, and
    # the bound filtering below is skipped because there is nothing to reject
    # against. Without this branch the whole call failed on any prior set
    # containing a fixed parameter, so a user could not inspect the priors they
    # had just written. See #244.
    if (is_constant_prior(priors$prior[j])) {
      out[[j]] <- rep(constant_prior_value(priors$prior[j]), n_samples)
      next
    }
    bits <- gsub("\\(|\\)", ",", priors$prior[j])
    bits <- strsplit(bits, ",", fixed = TRUE)[[1]]
    fct_i <- bits[1]
    v1 <- as.numeric(bits[2])
    v2 <- as.numeric(bits[3])
    out[[j]] <- fcts[[fct_i]](n_samples, v1, v2)
    if (any(!is.na(as.numeric(priors[j, c("lb", "ub")])))) {
      n_bounds <- sum(!is.na(priors[j, c("lb", "ub")]))
      if (n_bounds == 2) {
        bounds <- as.numeric(priors[j, c("lb", "ub")])
        out[[j]] <- sample(out[[j]][which(out[[j]] >= min(bounds) &
                                          out[[j]] <= max(bounds))],
                           n_samples, replace = TRUE)
      } else if (n_bounds == 1) {
        direction <- c("lb", "ub")[!is.na(priors[j, c("lb", "ub")])]
        bound_fct <- ifelse(direction == "lb", `<=`, `>=`)
        bounds <- as.numeric(priors[j, direction])
        out[[j]] <- sample(out[[j]][!bound_fct(out[[j]], bounds)],
                           n_samples, replace = TRUE)
      }
    }
  }
  names(out) <- par_names
  if (is.na(plot)) {
    out
  } else if (plot == "base") {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))      
    par(mfrow = c(ceiling(nrow(priors) / 2), 2))
    for (j in seq_along(out)) {
      hist(out[[j]], main = names(out)[j])
    }
  } else if (plot == "ggplot") {
    do.call("cbind", out) |>
      data.frame() |>
      pivot_longer(names_to = "param", values_to = "value",
                   cols = starts_with("b_")) |>
      mutate(param = gsub("^b\\_", "", .data$param)) |>
      ggplot(mapping = aes(x = .data$value)) +
        geom_histogram() +
        labs(x = "Value", y = "Count") +
        facet_wrap(~.data$param, scales = "free_x") +
        theme_bw()
  }
}
