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

library(dplyr)

fit <- readRDS("chap04/output/fit-model4-5.rds")

# ms <- rstan::extract(fit)
# N_mcmc <- length(ms$lp__)
# y50_base <- ms$a + ms$b * 50
# y50 <- rnorm(n=N_mcmc, mean=y50_base, sd=ms$sigma)
# d_mcmc <- data.frame(a=ms$a, b=ms$b, sigma=ms$sigma, y50_base, y50)

# 既にfitオブジェクトが存在している前提
ms <- fit$draws(variables = "b", format = "draws_matrix")
quantile(ms, probs = c(0.025, 0.975))

d_mcmc <- fit$draws(variables = c("lp__", "a", "b", "sigma"), format = "df")
N_mcmc <- length(d_mcmc$lp__)
y50_base <- d_mcmc$a + d_mcmc$b * 50
y50 <- rnorm(n = N_mcmc, mean = y50_base, sd = ms$sigma)
d_mcmc <- data.frame(a = d_mcmc$a, b = d_mcmc$b, sigma = d_mcmc$sigma, y50_base, y50)
