# library(rstan)
#
# d <- read.csv(file = "chap04/input/data-salary.txt")
# data <- list(N = nrow(d), X = d$X, Y = d$Y)
# fit <- stan(file = "chap04/model/model4-5.stan", data = data, seed = 1234)
#
# save.image(file = "chap04/output/result-model4-5.RData")

# 必要なパッケージをロード
# remotes パッケージがない場合はインストール
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# GitHub から {cmdstanr} をインストール
if (!requireNamespace("cmdstanr", quietly = TRUE)) {
  remotes::install_github("stan-dev/cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
  cmdstanr::install_cmdstan(cores = 4, overwrite = TRUE) # 4コアを使用してビルド（適宜変更）
  cmdstanr::cmdstan_version()
}

library(cmdstanr)
# データの読み込み
d <- read.csv(file = "chap04/input/data-salary.txt")
data <- list(N = nrow(d), X = d$X, Y = d$Y)

# Stanモデルのコンパイル
model <- cmdstan_model("chap04/model/model4-5.stan")

# MCMCサンプリングの実行
fit <- model$sample(
  data = data,
  seed = 1234,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 2000
)

# fitオブジェクトをrdsファイルとして保存
fit$save_object("chap04/output/fit-model4-5.rds")
