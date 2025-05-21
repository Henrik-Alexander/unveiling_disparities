
// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; // Sample size
  int<lower=0> K; // Number of parameters
  vector[N] y; // Response variable
  matrix[N, K] x; // Model matrix
  
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real alpha;
  vector[K] beta;
  real<lower=0> sigma;
}

transformed parameters {
  vector[N] mu;
  
  mu = alpha + x * beta;
  
}



// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  // Likelihood
  y ~ lognormal(mu, sigma);
  
  // Priors
  alpha ~ normal(0, 2);
  beta ~ gamma(1, 1);
  sigma ~ normal(0, 1);
}

