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

d <- read.csv(file = "chap05/input/data-attendance-2.txt")
data <- list(N = nrow(d), A = d$A, Score = d$Score / 200, M = d$M, Y = d$Y)

# Stanモデルのコンパイル
model <- cmdstan_model("chap05/model/model5-4.stan")

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
fit$save_object("chap05/output/fit-model5-4.rds")
