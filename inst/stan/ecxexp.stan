data {
  int<lower=1> N;  // number of observations
  int Y[N];  // response variable
  int trials[N];  // number of trials
  int<lower=1> K_top;  // number of population-level effects
  matrix[N, K_top] X_top;  // population-level design matrix
  int<lower=1> K_beta;  // number of population-level effects
  matrix[N, K_beta] X_beta;  // population-level design matrix
  // covariate vectors for non-linear functions
  vector[N] C_1;
  int prior_only;  // should the likelihood be ignored?
}
parameters {
  vector[K_top] b_top;  // population-level effects
  vector[K_beta] b_beta;  // population-level effects
}
model {
  // initialize linear predictor term
  vector[N] nlp_top = X_top * b_top;
  // initialize linear predictor term
  vector[N] nlp_beta = X_beta * b_beta;
  // initialize non-linear predictor term
  vector[N] mu;
  for (n in 1:N) {
    // compute non-linear predictor values
    mu[n] = nlp_top[n] * exp( - nlp_beta[n] * C_1[n]);
  }
  // priors including all constants
  target += normal_lpdf(b_top | 2.85291756509455, 100);
  target += gamma_lpdf(b_beta | 1e-04, 1e-04);
  // likelihood including all constants
  if (!prior_only) {
    target += binomial_logit_lpmf(Y | trials, mu);
  }
}
