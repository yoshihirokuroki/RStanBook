data {
  int N;
  array[N] real X;  // ← これが正しい記法
  array[N] real Y;
}

parameters {
  real a;
  real b;
  real<lower=0> sigma;
}

model {
  for (n in 1:N) {
    Y[n] ~ normal(a + b*X[n], sigma);
  }
}
