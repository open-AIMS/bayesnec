// Comparison model for the Phase 8 coverage study: the same scaled beta, but
// with the ceiling supplied as data rather than estimated.
//
// Setting U = max(y) * 1.01 reproduces the conventional practice this family
// exists to replace -- dividing the response by its own maximum and fitting a
// Beta. Setting U to the true ceiling reproduces the best case a fixed divisor
// can achieve. The only difference between the two runs is what U is, which is
// exactly the comparison Ritz et al. (2026) draw between a divisor estimated
// from the data and one known in advance.
//
// Fitting the raw y with U fixed is equivalent to fitting y/U with a plain
// Beta: the two differ only by the -log(U) Jacobian, a constant here, so the
// posterior for (top, beta, nec, phi) is the same up to the scale of top.
// Keeping y on its natural scale makes the three arms directly comparable.

data {
  int<lower=1> N;
  vector[N] x;
  vector<lower=0>[N] y;
  real<lower=0> U;              // fixed, not estimated

  real top_loc;
  real<lower=0> top_scale;
  real beta_loc;
  real<lower=0> beta_scale;
  real nec_lb;
  real nec_ub;
  real<lower=0> phi_shape;
  real<lower=0> phi_rate;
}

parameters {
  real<lower=0, upper=U> top;
  real beta;
  real<lower=nec_lb, upper=nec_ub> nec;
  real<lower=0> phi;
}

model {
  vector[N] mu;
  for (n in 1:N) {
    real d = x[n] - nec;
    mu[n] = top * exp(-exp(beta) * (d > 0 ? d : 0));
  }

  top ~ gamma(top_loc, top_scale);
  beta ~ normal(beta_loc, beta_scale);
  phi ~ gamma(phi_shape, phi_rate);
  // nec is uniform on its declared bounds

  for (n in 1:N) {
    real m = mu[n] / U;
    if (m * phi <= 0 || (1 - m) * phi <= 0 || y[n] >= U) {
      target += negative_infinity();
    } else {
      target += beta_lpdf(y[n] / U | m * phi, (1 - m) * phi) - log(U);
    }
  }
}
