data {
  int N1;
  int N2;
  array[N1] real Y1;  // 修正: 配列の新しい構文
  array[N2] real Y2;  // 修正: 配列の新しい構文
}

parameters {
  real mu1;
  real mu2;
  real<lower=0> sigma;
}

model {
  for (n in 1:N1)
    Y1[n] ~ normal(mu1, sigma);
  for (n in 1:N2)
    Y2[n] ~ normal(mu2, sigma);
}
