data {
  int N;
  array[N] int<lower=0, upper=1> A;
  array[N] real<lower=0, upper=1> Score;
  array[N] int<lower=0> M;
  array[N] int<lower=0> Y;
}

parameters {
  real b1;
  real b2;
  real b3;
}

transformed parameters {
  array[N] real q;
  for (n in 1:N)
    q[n] = inv_logit(b1 + b2 * A[n] + b3 * Score[n]);
}

model {
  Y ~ binomial(M, q);
}

generated quantities {
  array[N] real y_pred;
  for (n in 1:N)
    y_pred[n] = binomial_rng(M[n], q[n]);
}
