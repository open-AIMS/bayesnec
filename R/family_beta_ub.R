# The upper-bounded beta family.
#
# Motivation is in notes/beta_ub_phase0.md and issue #173. Briefly: growth-rate
# and cell-yield endpoints are compressed against an experimental ceiling near
# the control, so an unbounded family (Gamma, lognormal) misstates control
# variance. The conventional workaround -- divide by max(y) and fit a Beta --
# buys a bounded, mean-dependent variance function at the cost of dividing every
# observation by an extreme order statistic, which biases effective doses
# upwards and understates their uncertainty (Ritz et al. 2026). This family
# gives the ceiling its own parameter instead.
#
#   mu(x)      the existing bayesnec equation, on the natural response scale
#   y          ~ U * Beta(mu/U * phi, (1 - mu/U) * phi)
#   E[y | x]   = mu(x)
#   Var[y | x] = mu(x) * (U - mu(x)) / (1 + phi)
#
# Variance vanishes at both ends and is maximal in between, while `top` stays
# the natural-scale control mean rather than doubling as the support bound.

#' The upper-bounded beta family
#'
#' A \pkg{brms} custom family for a continuous, positive response with an
#' explicit upper bound that is estimated rather than taken from the data.
#'
#' @param link Link for \code{mu}. Only \code{"identity"} is supported:
#' \pkg{bayesnec} keeps every parameter on the natural response scale, and the
#' ceiling constraint \code{mu < U} is only meaningful there.
#' @param link_phi Link for the precision parameter \code{phi}.
#' @param link_delta Link for \code{delta}, the gap between the largest observed
#' response and the ceiling.
#'
#' @details The response is modelled as a beta variate scaled to \code{(0, U)},
#' where \code{U} is an upper bound estimated alongside everything else. The
#' mean is \code{mu} exactly, so \code{top} keeps its usual meaning as the
#' control-level response, and the variance \code{mu * (U - mu) / (1 + phi)}
#' vanishes at both \code{0} and \code{U}.
#'
#' \bold{The ceiling is parameterised as a gap, but the prior belongs on the
#' ceiling.} \code{U} must exceed every observed response, which would want
#' \code{real<lower=ymax> U} in Stan --- something
#' \code{\link[brms]{custom_family}} cannot express, since its bounds must be
#' constants rather than data. The family therefore samples \code{delta > 0} and
#' sets \code{U = ymax + delta} inside the Stan function, which gives the
#' constraint for free.
#'
#' The prior is nevertheless placed on \code{U}. The map is a pure location
#' shift with Jacobian 1, so \code{U ~ normal(U_loc, U_scale)} truncated to
#' \code{U > ymax} is exactly \code{delta ~ normal(U_loc - ymax, U_scale)} with
#' \code{lb = 0}. Putting the prior directly on \code{delta} would centre
#' \code{U} just above the sample maximum, so the ceiling would move with
#' \code{n} and with the noise in a single extreme order statistic --- a softer
#' form of the practice this family exists to avoid. See \code{\link{bnec}} for
#' the \code{U_loc} and \code{U_scale} arguments.
#'
#' \bold{When the data can identify \code{U}.} The simulation study in
#' \code{notes/beta_ub_phase0.md} varied how much of the \code{0..U} range the
#' fitted curve spans. The control response must reach roughly 70\% of the
#' ceiling before the data say anything about where the ceiling is, and only
#' near 90\% do they say much: at \code{top/U = 0.2} the posterior for \code{U}
#' equals its prior to three decimal places. How finely the predictor is sampled
#' barely matters. Toxicity estimates are far more robust than \code{U} itself
#' --- a two-fold misspecification of \code{U_loc} moved NEC, EC10 and EC50 by
#' under 2\% --- because \code{ecx(type = "absolute")} is a relative decline
#' from the fitted \code{top}, and \code{top} is identified by the control data
#' whatever \code{U} does.
#'
#' The response must be strictly positive. Zeros are not a boundary problem
#' here but a different model: see \code{\link{bnec}} on the hurdle families,
#' and note that a response which can reach or pass zero (a specific growth
#' rate, an increment) wants a family on the real line rather than a bounded
#' one.
#'
#' @return An object of class \code{\link[brms]{customfamily}}.
#'
#' @references
#' Ritz C, Gerhard D, Streibig JC (2026). Better alternatives than normalizing
#' to control: case studies with algae toxicity and dose-response analysis.
#' Environmental and Ecological Statistics, 33, 35-55.
#' doi:10.1007/s10651-025-00698-y.
#'
#' @seealso \code{\link{bnec}}, \code{\link{ecx}}
#'
#' @examples
#' beta_ub()
#'
#' @importFrom brms custom_family
#'
#' @export
beta_ub <- function(link = "identity", link_phi = "log",
                    link_delta = "log") {
  if (!identical(link, "identity")) {
    stop("The beta_ub family requires link = \"identity\". bayesnec keeps",
         " every parameter on the natural response scale, and the ceiling",
         " constraint mu < U is only meaningful there.", call. = FALSE)
  }
  custom_family(
    name = "beta_ub",
    dpars = c("mu", "phi", "delta"),
    links = c(link, link_phi, link_delta),
    lb = c(0, 0, 0),
    ub = c(NA, NA, NA),
    type = "real",
    # ymax is supplied as a data stanvar rather than recomputed downstream, so
    # the value the likelihood used is the value post-processing sees even if
    # the data are later subset.
    vars = "ymax",
    loop = TRUE,
    log_lik = log_lik_beta_ub,
    posterior_predict = posterior_predict_beta_ub,
    posterior_epred = posterior_epred_beta_ub
  )
}

#' Stan code for the beta_ub family
#'
#' @param ymax The largest observed response. Enters only as a support
#' constraint; the ceiling itself is \code{ymax + delta}.
#'
#' @return An object of class \code{\link[brms]{stanvars}}.
#'
#' @details Both the lpdf and the rng guard three ways the beta shape
#' parameters can leave \code{(0, Inf)}, not just the obvious one. \code{mu >= U}
#' is the ceiling violation. The other two are underflow: for a steep decline
#' over a wide predictor range \code{mu} reaches zero in floating point long
#' before the largest concentration, and \code{beta_lpdf} then errors on a zero
#' shape parameter. The Phase 0 study found the underflow case to be the more
#' common of the two --- a fixture that never approached the ceiling rejected
#' more often than one that sat right against it.
#'
#' @importFrom brms stanvar
#'
#' @noRd
beta_ub_stanvars <- function(ymax) {
  scode <- "
  real beta_ub_lpdf(real y, real mu, real phi, real delta, real ymax) {
    real U = ymax + delta;
    real m;
    if (mu >= U) {
      return negative_infinity();
    }
    m = mu / U;
    if (m * phi <= 0 || (1 - m) * phi <= 0) {
      return negative_infinity();
    }
    return beta_lpdf(y / U | m * phi, (1 - m) * phi) - log(U);
  }
  real beta_ub_rng(real mu, real phi, real delta, real ymax) {
    real U = ymax + delta;
    real m = mu / U;
    return U * beta_rng(m * phi, (1 - m) * phi);
  }
  "
  stanvar(scode = scode, block = "functions") +
    stanvar(x = as.numeric(ymax), name = "ymax",
            scode = "real<lower=0> ymax;", block = "data")
}

#' Ceiling implied by a draw of delta
#'
#' @param prep A \pkg{brms} prepared-predictions object.
#' @param delta A \code{\link[base]{numeric}} vector of draws.
#'
#' @return A \code{\link[base]{numeric}} vector.
#'
#' @noRd
beta_ub_ceiling <- function(prep, delta) {
  ymax <- prep$data$ymax
  if (is.null(ymax)) {
    stop("The fit does not carry `ymax`, which the beta_ub family needs to",
         " reconstruct the ceiling. This fit was not produced by bnec() with",
         " family = beta_ub().", call. = FALSE)
  }
  as.numeric(ymax) + delta
}

#' @importFrom brms get_dpar
#' @importFrom stats dbeta
#' @noRd
log_lik_beta_ub <- function(i, prep) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  phi <- brms::get_dpar(prep, "phi", i = i)
  delta <- brms::get_dpar(prep, "delta", i = i)
  y <- prep$data$Y[i]
  u <- beta_ub_ceiling(prep, delta)
  m <- mu / u
  out <- rep(-Inf, length(m))
  # Same three guards as the Stan function. A draw that violates them has zero
  # posterior mass, so -Inf is the honest log-likelihood rather than an error.
  ok <- is.finite(m) & m > 0 & m < 1 & phi > 0 & y < u & y > 0
  if (any(ok)) {
    out[ok] <- dbeta(y / u[ok], m[ok] * phi[ok], (1 - m[ok]) * phi[ok],
                     log = TRUE) - log(u[ok])
  }
  out
}

#' @importFrom brms get_dpar
#' @importFrom stats rbeta
#' @noRd
posterior_predict_beta_ub <- function(i, prep, ...) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  phi <- brms::get_dpar(prep, "phi", i = i)
  delta <- brms::get_dpar(prep, "delta", i = i)
  u <- beta_ub_ceiling(prep, delta)
  m <- mu / u
  m <- pmin(pmax(m, .Machine$double.eps), 1 - .Machine$double.eps)
  u * rbeta(length(m), m * phi, (1 - m) * phi)
}

#' @importFrom brms get_dpar
#' @noRd
posterior_epred_beta_ub <- function(prep) {
  # E[y | x] = mu exactly, by construction of the scaled beta.
  brms::get_dpar(prep, "mu")
}
