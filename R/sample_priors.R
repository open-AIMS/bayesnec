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
#' @param seed A \code{\link[base]{numeric}} vector of length 1. Passed to
#' \code{\link[base]{set.seed}} before the priors are sampled, so that two
#' calls on the same prior set return the same draws and the same figure. The
#' caller's saved RNG state is restored afterwards, and a
#' \code{\link[base]{set.seed}} in the session therefore does not change the
#' result. A different value gives an independent set of draws. \code{NULL} is
#' refused, because \code{set.seed(NULL)} re-initialises the stream from the
#' clock.
#' Reproducibility assumes the same RNG kind; the sampling algorithm is fixed
#' to \code{sample.kind = "Rejection"}. The saved RNG state is restored,
#' but the cached normal variate used by \code{normal.kind = "Box-Muller"}
#' is not part of that state. With Box-Muller, the next normal draw can change
#' after an odd number of preceding normal draws. Use R's default
#' \code{normal.kind = "Inversion"} to preserve subsequent normal draws.
#'
#' @importFrom graphics hist
#' @importFrom ggplot2 ggplot aes geom_histogram facet_wrap theme_bw labs
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect starts_with
#' @importFrom dplyr filter mutate
#' @importFrom chk chk_number
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
sample_priors <- function(priors, n_samples = 10000, plot = "ggplot",
                          seed = 10) {
  chk_numeric(n_samples)
  chk_number(seed)
  # Test is.na() explicitly: NA is the documented "return the draws" value,
  # and `NA %in% c(...)` is FALSE. Check the length first, so that NULL or a
  # length-2 `plot` gets this message rather than an R internals one.
  if (!(length(plot) == 1 &&
          (is.na(plot) || plot %in% c("ggplot", "base")))) {
    stop("plot must be NA, or a character string of either ",
         "\"ggplot\" or \"base\"")
  }
  priors <- as.data.frame(priors) |>
    filter(class == "b")
  priors <- blank_bounds_to_na(priors)
  priors <- priors[priors$prior != "", ]
  par_names <- character(length = nrow(priors))
  for (j in seq_along(par_names)) {
    sep <- ifelse(priors$class[j] == "b", "_", "")
    par_names[j] <- paste(priors$class[j], priors$nlpar[j], sep = sep)
  }
  out <- vector(mode = "list", length = nrow(priors))
  # Prior inspection should repeat without affecting later simulation.
  # Fix the sampling algorithm as well as the seed, and restore both on exit.
  with_preserved_rng_state({
    set.seed(seed, sample.kind = "Rejection")
    for (j in seq_len(nrow(priors))) {
      # A constant() prior is a point mass: every draw is the fixed value, and
      # there is nothing for the bound filter below to reject.
      if (is_constant_prior(priors$prior[j])) {
        out[[j]] <- rep(constant_prior_value(priors$prior[j]), n_samples)
        next
      }
      bits <- gsub("\\(|\\)", ",", priors$prior[j])
      bits <- strsplit(bits, ",", fixed = TRUE)[[1]]
      fct_i <- prior_sampler(bits[1])
      v1 <- as.numeric(bits[2])
      v2 <- as.numeric(bits[3])
      bounds <- as.numeric(priors[j, c("lb", "ub")])
      if (all(is.na(bounds))) {
        out[[j]] <- fct_i(n_samples, v1, v2)
      } else {
        out[[j]] <- draw_truncated(bits[1], n_samples, v1, v2, bounds,
                                   priors[j, ])
      }
    }
  })
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

#' Draw from a prior truncated to its bounds
#'
#' Inverts the distribution function over the part of the prior that lies
#' inside the bounds, so every draw is new and inside them.
#'
#' @param dist A \code{\link[base]{character}} string naming the distribution.
#' @param n_samples The number of draws to return.
#' @param v1,v2 The two parameters of the distribution, in the order the
#' matching generator in \code{prior_samplers()} takes them.
#' @param bounds A \code{\link[base]{numeric}} vector of length 2, the lower
#' and upper bound, \code{NA} where absent.
#' @param prior_row The one-row \code{\link[base]{data.frame}} of the prior,
#' read for the message.
#'
#' @details The prior is refused when its distribution function takes the same
#' value at both bounds, as when the bounds lie more than about eight standard
#' deviations into a tail. Every distribution \code{prior_samplers()} offers has
#' a distribution and a quantile function, so no rejection step is needed.
#'
#' @return A \code{\link[base]{numeric}} vector of length \code{n_samples}.
#'
#' @noRd
draw_truncated <- function(dist, n_samples, v1, v2, bounds, prior_row) {
  prior_sampler(dist)
  dist <- trimws(dist)
  p_fun <- switch(dist, gamma = pgamma, normal = pnorm, beta = pbeta,
                  uniform = punif, lognormal = plnorm)
  q_fun <- switch(dist, gamma = qgamma, normal = qnorm, beta = qbeta,
                  uniform = qunif, lognormal = qlnorm)
  lower <- if (is.na(bounds[1])) -Inf else min(bounds, na.rm = TRUE)
  upper <- if (is.na(bounds[2])) Inf else max(bounds, na.rm = TRUE)
  p_lower <- p_fun(lower, v1, v2)
  p_upper <- p_fun(upper, v1, v2)
  if (!(p_upper > p_lower)) {
    bound <- function(b) if (is.na(b)) "none" else b
    stop("No draw from the prior on ", prior_row$nlpar, ", ",
         prior_row$prior, ", fell inside its bounds",
         " (lb = ", bound(prior_row$lb), ", ub = ", bound(prior_row$ub),
         "). The bounds hold none of the prior's probability; check them",
         " against the prior.", call. = FALSE)
  }
  draws <- q_fun(runif(n_samples, p_lower, p_upper), v1, v2)
  # Rounding in the quantile function can place a draw on the wrong side of a
  # bound by one unit in the last place.
  pmin(pmax(draws, lower), upper)
}
