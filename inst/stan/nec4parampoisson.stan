// generated with brms 2.13.0
functions {
}
data {
  int<lower=1> N;  // number of observations
  int Y[N];  // response variable
  int<lower=1> K_bot;  // number of population-level effects
  matrix[N, K_bot] X_bot;  // population-level design matrix
  int<lower=1> K_ec50;  // number of population-level effects
  matrix[N, K_ec50] X_ec50;  // population-level design matrix
  int<lower=1> K_top;  // number of population-level effects
  matrix[N, K_top] X_top;  // population-level design matrix
  int<lower=1> K_beta;  // number of population-level effects
  matrix[N, K_beta] X_beta;  // population-level design matrix
  // covariate vectors for non-linear functions
  vector[N] C_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  vector[K_bot] b_bot;  // population-level effects
  vector[K_ec50] b_ec50;  // population-level effects
  vector[K_top] b_top;  // population-level effects
  vector[K_beta] b_beta;  // population-level effects
}
transformed parameters {
}
model {
  // initialize linear predictor term
  vector[N] nlp_bot = X_bot * b_bot;
  // initialize linear predictor term
  vector[N] nlp_ec50 = X_ec50 * b_ec50;
  // initialize linear predictor term
  vector[N] nlp_top = X_top * b_top;
  // initialize linear predictor term
  vector[N] nlp_beta = X_beta * b_beta;
  // initialize non-linear predictor term
  vector[N] mu;
  for (n in 1:N) {
    // compute non-linear predictor values
    mu[n] = nlp_top[n] + (nlp_bot[n] - nlp_top[n]) / (1 + exp((nlp_ec50[n] - C_1[n]) * nlp_beta[n]));
  }
  // priors including all constants
  target += normal_lpdf(b_bot | 0, 100);
  target += normal_lpdf(b_ec50 | 0, 100);
  target += normal_lpdf(b_top | 2, 100);
  target += gamma_lpdf(b_beta | 0.0001, 0.0001);
  // likelihood including all constants
  if (!prior_only) {
    target += poisson_log_lpmf(Y | mu);
  }
}
generated quantities {
}
