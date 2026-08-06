// generated with brms 2.22.0
functions {
  
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
  
}
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K_top;  // number of population-level effects
  matrix[N, K_top] X_top;  // population-level design matrix
  int<lower=1> K_beta;  // number of population-level effects
  matrix[N, K_beta] X_beta;  // population-level design matrix
  int<lower=1> K_nec;  // number of population-level effects
  matrix[N, K_nec] X_nec;  // population-level design matrix
  // covariates for non-linear functions
  vector[N] C_1;
  int prior_only;  // should the likelihood be ignored?
  real<lower=0> ymax;
  real u_loc;
  real<lower=0> u_scale;
}
transformed data {
}
parameters {
  vector<lower=0>[K_top] b_top;  // regression coefficients
  vector[K_beta] b_beta;  // regression coefficients
  vector<lower=0,upper=6>[K_nec] b_nec;  // regression coefficients
  real<lower=0> phi;  // precision parameter
  real<lower=0> delta;
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
  lprior += gamma_lpdf(b_top | 4, 8);
  lprior += normal_lpdf(b_beta | -1, 0.5);
  lprior += uniform_lpdf(b_nec | 0, 6)
    - 1 * log_diff_exp(uniform_lcdf(6 | 0, 6), uniform_lcdf(0 | 0, 6));
  lprior += gamma_lpdf(phi | 4, 0.1);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] nlp_top = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] nlp_beta = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] nlp_nec = rep_vector(0.0, N);
    // initialize non-linear predictor term
    vector[N] mu;
    nlp_top += X_top * b_top;
    nlp_beta += X_beta * b_beta;
    nlp_nec += X_nec * b_nec;
    for (n in 1:N) {
      // compute non-linear predictor values
      mu[n] = (nlp_top[n] * exp( - exp(nlp_beta[n]) * (C_1[n] - nlp_nec[n]) * step(C_1[n] - nlp_nec[n])));
    }
    for (n in 1:N) {
      target += beta_ub_lpdf(Y[n] | mu[n], phi, delta, ymax);
    }
  }
  // priors including constants
  target += lprior;
  target += normal_lpdf(delta | u_loc - ymax, u_scale)   - normal_lccdf(0 | u_loc - ymax, u_scale);
}
generated quantities {
}

