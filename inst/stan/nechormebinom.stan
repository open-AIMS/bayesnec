// generated with brms 2.13.0
functions {
}
data {
  int<lower=1> N;  // number of observations
  int Y[N];  // response variable
  int trials[N];  // number of trials
  int<lower=1> K_top;  // number of population-level effects
  matrix[N, K_top] X_top;  // population-level design matrix
  int<lower=1> K_beta;  // number of population-level effects
  matrix[N, K_beta] X_beta;  // population-level design matrix
  int<lower=1> K_nec;  // number of population-level effects
  matrix[N, K_nec] X_nec;  // population-level design matrix
  int<lower=1> K_slope;  // number of population-level effects
  matrix[N, K_slope] X_slope;  // population-level design matrix
  // covariate vectors for non-linear functions
  vector[N] C_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  vector[K_top] b_top;  // population-level effects
  vector[K_beta] b_beta;  // population-level effects
  vector[K_nec] b_nec;  // population-level effects
  vector<lower=0>[K_slope] b_slope;  // population-level effects
}
transformed parameters {
}
model {
  // initialize linear predictor term
  vector[N] nlp_top = X_top * b_top;
  // initialize linear predictor term
  vector[N] nlp_beta = X_beta * b_beta;
  // initialize linear predictor term
  vector[N] nlp_nec = X_nec * b_nec;
  // initialize linear predictor term
  vector[N] nlp_slope = X_slope * b_slope;
  // initialize non-linear predictor term
  vector[N] mu;
  for (n in 1:N) {
    // compute non-linear predictor values
    mu[n] = (nlp_top[n] + nlp_slope[n] * C_1[n]) * exp( - nlp_beta[n] * (C_1[n] - nlp_nec[n]) * step(C_1[n] - nlp_nec[n]));
  }
  // priors including all constants
  target += normal_lpdf(b_top | 2, 100);
  target += gamma_lpdf(b_beta | 0.0001, 0.0001);
  target += uniform_lpdf(b_nec | 0.0001, 0.9999);
  target += normal_lpdf(b_slope | 0, 100)
    - 1 * normal_lccdf(0 | 0, 100);
  // likelihood including all constants
  if (!prior_only) {
    target += binomial_logit_lpmf(Y | trials, mu);
  }
}
generated quantities {
}
