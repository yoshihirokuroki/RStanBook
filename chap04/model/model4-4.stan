data {
  int N;
  array[N] real X;  // 観測値の年齢
  array[N] real Y;  // 観測値の年収
  int N_new;
  array[N_new] real X_new;  // 23歳から60歳 ← 修正した箇所（新しいStanの記法）
}

parameters {
  real a;
  real b;
  real<lower=0> sigma;
}

transformed parameters {
  array[N] real y_base;  // 明示的に配列として宣言
  for (n in 1:N)
    y_base[n] = a + b * X[n];  // 観測値のXに線形モデルを当てはめた時の基本年収
}

model {
  for (n in 1:N)
    Y[n] ~ normal(y_base[n], sigma);  // 観測年収は線形モデルで予測した年収を平均とした正規分布に従う
}

generated quantities {
  array[N_new] real y_base_new;  // 新しいデータXに対する基本年収
  array[N_new] real y_new;  // ノイズの項も考慮した年収
  for (n in 1:N_new) {
    y_base_new[n] = a + b * X_new[n]; // 各年齢での予測年収の算出式
    y_new[n] = normal_rng(y_base_new[n], sigma);  // 
  }
}
