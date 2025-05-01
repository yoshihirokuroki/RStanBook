# after run-model5-3.R
library(cmdstanr)
library(posterior)

fit <- readRDS("~/rprojects/RStanBook/chap05/output/fit-model5-3.rds")

# mu と y_pred のサンプルを取得（draws_matrix 形式）
ms <- fit$draws(variables = c("mu", "y_pred"), format = "draws_matrix")

# 個別の変数に分割
mu <- ms[, grep("^mu\\[", colnames(ms))] # mu のみ抽出
y_pred <- ms[, grep("^y_pred\\[", colnames(ms))] # y_pred のみ抽出

eps <- y_pred - mu

# カラム名を "eps[n]" に変更
colnames(eps) <- paste0("eps[", seq_len(ncol(eps)), "]")
