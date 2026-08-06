// Phase 0 prototype for the upper-bounded beta family proposed in
// https://github.com/open-AIMS/bayesnec/issues/173
//
//   mu(x) = top * exp(-exp(beta) * (x - nec) * step(x - nec))   // nec3param
//   m(x)  = mu(x) / U                                           // 0 < m < 1
//   y     ~ U * Beta(m * phi, (1 - m) * phi)
//
// giving E[y | x] = mu(x) and Var[y | x] = mu(x) * (U - mu(x)) / (1 + phi):
// variance vanishing at both ends and maximal in between, which is the
// ceiling-aware behaviour that motivates dividing by max(y) in the first place,
// without the bias that comes with it.
//
// Two things this file is deliberately doing:
//
// 1. U is not sampled directly. `real<lower=ymax> U` is what the model wants,
//    but brms::custom_family() cannot express a data-dependent lower bound, so
//    the eventual implementation must sample delta > 0 and set U = ymax + delta.
//    The prototype uses the same parameterisation so that Phase 1 inherits
//    whatever this phase learns rather than discovering it later.
//
// 2. The prior is nevertheless placed on U, not on delta. The map is a pure
//    location shift with Jacobian 1, so a prior normal(U_loc, U_scale) on U
//    truncated to U > ymax is exactly a prior normal(U_loc - ymax, U_scale) on
//    delta with lb = 0. Putting the prior on delta instead would centre U just
//    above the sample maximum, so the ceiling would move with n and with the
//    noise in a single extreme order statistic -- a softer version of the
//    practice issue #173 argues against.

functions {
  // nec3param mean curve, on the natural response scale.
  vector nec3param_mu(vector x, real top, real beta, real nec) {
    int N = num_elements(x);
    vector[N] mu;
    for (n in 1:N) {
      real d = x[n] - nec;
      mu[n] = top * exp(-exp(beta) * (d > 0 ? d : 0));
    }
    return mu;
  }
}

data {
  int<lower=1> N;
  vector[N] x;
  vector<lower=0>[N] y;
  real<lower=0> ymax;           // max(y); enters only as a support constraint

  // prior on the ceiling itself
  real U_loc;
  real<lower=0> U_scale;

  // priors on the mean curve and precision
  real top_loc;
  real<lower=0> top_scale;
  real beta_loc;
  real<lower=0> beta_scale;
  real nec_loc;
  real<lower=0> nec_scale;
  real<lower=0> phi_shape;
  real<lower=0> phi_rate;

  // 1 = use reject() so cmdstan logs each violation and the driver can count
  // them; 0 = silent -inf. Decision 1 in the phase plan needs the count.
  int<lower=0, upper=1> count_rejections;
}

parameters {
  real<lower=0> top;
  real beta;                    // log scale, as in bayesnec
  real nec;
  real<lower=0> phi;
  real<lower=0> delta;          // U = ymax + delta
}

transformed parameters {
  real U = ymax + delta;
}

model {
  vector[N] mu = nec3param_mu(x, top, beta, nec);

  top ~ normal(top_loc, top_scale);
  beta ~ normal(beta_loc, beta_scale);
  nec ~ normal(nec_loc, nec_scale);
  phi ~ gamma(phi_shape, phi_rate);
  // equivalent to U ~ normal(U_loc, U_scale) truncated to U > ymax
  delta ~ normal(U_loc - ymax, U_scale);

  for (n in 1:N) {
    // Two ways the beta shape parameters can leave (0, inf). mu >= U is the
    // ceiling violation the phase plan asks about. mu * phi underflowing to
    // zero is the other, and it is not hypothetical: for a steep decline over a
    // wide predictor range, exp(-exp(beta) * (x - nec)) underflows long before
    // the largest concentration, and beta_lpdf then errors on a zero shape.
    // Both are handled the same way.
    if (mu[n] >= U || mu[n] * phi <= 0 || (1 - mu[n] / U) * phi <= 0) {
      // For declining models mu <= top, so constraining top would suffice;
      // hormesis models peak above top, which is why the check is on mu.
      if (count_rejections) {
        reject("beta_ub constraint violated");
      } else {
        target += negative_infinity();
      }
    } else {
      real m = mu[n] / U;
      // change of variables z = y / U, hence the -log(U) Jacobian
      target += beta_lpdf(y[n] / U | m * phi, (1 - m) * phi) - log(U);
    }
  }
}

generated quantities {
  // How close the fitted curve gets to its ceiling. If this sits well below 1
  // the data never probe the ceiling and U is prior-driven -- the degenerate
  // case the phase plan asks to confirm rather than treat as a failure.
  real top_over_U = top / U;
  vector[N] log_lik;
  {
    vector[N] mu = nec3param_mu(x, top, beta, nec);
    for (n in 1:N) {
      real m = mu[n] / U;
      log_lik[n] = (m * phi > 0 && (1 - m) * phi > 0)
        ? beta_lpdf(y[n] / U | m * phi, (1 - m) * phi) - log(U)
        : negative_infinity();
    }
  }
}
