data {
  int N;
  array[N] int<lower=0, upper=1> A;
  array[N] real<lower=0, upper=1> Score;
  array[N] real<lower=0, upper=1> Y;
}

parameters {
  real b1;
  real b2;
  real b3;
  real<lower=0> sigma;
}

transformed parameters {
  array[N] real mu;
  for (n in 1:N)
    mu[n] = b1 + b2 * A[n] + b3 * Score[n];
}

model {
  Y ~ normal(mu, sigma);
}

generated quantities {
  array[N] real y_pred;
  for (n in 1:N)
    y_pred[n] = normal_rng(mu[n], sigma);
}
