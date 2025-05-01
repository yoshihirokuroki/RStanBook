# library(rstan)
#
# d <- read.csv(file='input/data-salary.txt')
# data <- list(N=nrow(d), X=d$X, Y=d$Y)
#
# stanmodel <- stan_model(file='model/model4-5.stan')
#
# fit <- sampling(
#   stanmodel,
#   data=data,
#   pars=c('b', 'sigma'),
#   init=function() {
#     list(a=runif(1,-10,10), b=runif(1,0,10), sigma=10)
#   },
#   seed=123,
#   chains=3, iter=1000, warmup=200, thin=2
# )
rm(list = ls()) # 現在の環境にあるすべてのオブジェクトを削除

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

# 初期値のリストを関数で定義

# MCMCサンプリングの実行
fit <- model$sample(
  data = data,
  #  variables = c("b", "sigma"), # {cmdstanr}では指定出来ない
  init = function() {
    list(a = runif(1, -10, 10), b = runif(1, 0, 10), sigma = 10)
  },
  seed = 123,
  chains = 3,
  iter_warmup = 200,
  iter_sampling = 800, # total 1000 = warmup 200 + sampling 800
  thin = 2,
  parallel_chains = 3,
  # chains = 4,
  # parallel_chains = 4,
  # iter_warmup = 1000,
  # iter_sampling = 2000
)

# fitオブジェクトをrdsファイルとして保存
fit$save_object("chap04/output/fit-model4-5.rds")
