# library(rstan)
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
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(posterior)
library(patchwork)

set.seed(1234)

d <- read.csv(file = "chap04/input/data-salary.txt")
X_new <- 23:60
data <- list(N = nrow(d), X = d$X, Y = d$Y, N_new = length(X_new), X_new = X_new)
# Stanモデルのコンパイル
model <- cmdstan_model("chap04/model/model4-4.stan")
fit <- model$sample(
  data = data, seed = 1234, chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 2000
)
# ms <- rstan::extract(fit)

# qua <- apply(ms$y_base_new, 2, quantile, probs = c(0.025, 0.25, 0.50, 0.75, 0.975))
# d_est <- data.frame(X = X_new, t(qua), check.names = FALSE)
#
# p <- ggplot() +
#   theme_bw(base_size = 18) +
#   geom_ribbon(data = d_est, aes(x = X, ymin = `2.5%`, ymax = `97.5%`), fill = "black", alpha = 1 / 6) +
#   geom_ribbon(data = d_est, aes(x = X, ymin = `25%`, ymax = `75%`), fill = "black", alpha = 2 / 6) +
#   geom_line(data = d_est, aes(x = X, y = `50%`), linewidth = 1) +
#   geom_point(data = d, aes(x = X, y = Y), shape = 1, size = 3) +
#   coord_cartesian(xlim = c(22, 61), ylim = c(200, 1400)) +
#   scale_y_continuous(breaks = seq(from = 200, to = 1400, by = 400)) +
#   labs(y = "Y")
# ggsave(p, file = "output/fig4-8-left-2.png", dpi = 300, w = 4, h = 3)
#
#
# qua <- apply(ms$y_new, 2, quantile, probs = c(0.025, 0.25, 0.50, 0.75, 0.975))
# d_est <- data.frame(X = X_new, t(qua), check.names = FALSE)
#
# p <- ggplot() +
#   theme_bw(base_size = 18) +
#   geom_ribbon(data = d_est, aes(x = X, ymin = `2.5%`, ymax = `97.5%`), fill = "black", alpha = 1 / 6) +
#   geom_ribbon(data = d_est, aes(x = X, ymin = `25%`, ymax = `75%`), fill = "black", alpha = 2 / 6) +
#   geom_line(data = d_est, aes(x = X, y = `50%`), linewidth = 1) +
#   geom_point(data = d, aes(x = X, y = Y), shape = 1, size = 3) +
#   coord_cartesian(xlim = c(22, 61), ylim = c(200, 1400)) +
#   scale_y_continuous(breaks = seq(from = 200, to = 1400, by = 400)) +
#   labs(y = "Y")
# ggsave(p, file = "output/fig4-8-right-2.png", dpi = 300, w = 4, h = 3)


# MCMCサンプルを取得（3次元array: iterations x chains x parameters）
draws <- fit$draws()

# 3次元配列からデータフレームに変換
df_draws <- as_draws_df(draws) %>%
  rename(iteration = .iteration) # iterationの名前を統一

# sigma に NA が含まれていないかチェック
df_draws <- df_draws %>% filter(!is.na(sigma))

# 変換対象のカラム名に対応する整数ベクトル
cols_to_select <- 23:60

# "y_base[" で始まる列を選択
df_y_base <- df_draws %>%
  select(starts_with("y_base_new["))

# 列名を整数ベクトルに基づき変換
colnames(df_y_base) <- as.character(cols_to_select)

# pivot_longer を使ってカラム名をXに、データをYに格納
credible <- df_y_base %>%
  pivot_longer(
    cols = everything(),
    names_to = "X",
    values_to = "Y"
  ) %>%
  group_by(X) %>%
  summarise(
    y_mean = mean(Y, na.rm = TRUE),
    y_lower_95 = quantile(Y, 0.025, na.rm = TRUE),
    y_upper_95 = quantile(Y, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(X = as.integer(X))

# ggplot で描画（ベイズ信用区間）
bci <- ggplot(credible, aes(x = X)) +
  geom_ribbon(aes(ymin = y_lower_95, ymax = y_upper_95), fill = "lightblue", alpha = 0.5) + # 95%信用区間
  geom_line(aes(y = y_mean), color = "blue", linewidth = 1) + # 回帰線（y = a + b * x の平均）
  geom_point(data = d, aes(x = X, y = Y), color = "red", size = 2) + # 実データ
  labs(x = "説明変数 x", y = "予測値 y", title = "ベイズ信用区間（95%）") +
  theme_minimal() +
  theme(text = element_text(family = "Hiragino Sans"))

# "y_base[" で始まる列を選択
df_y_new <- df_draws %>%
  select(starts_with("y_new["))

# 列名を整数ベクトルに基づき変換
colnames(df_y_new) <- as.character(cols_to_select)

# pivot_longer を使ってカラム名をXに、データをYに格納
predictions <- df_y_new %>%
  pivot_longer(
    cols = everything(),
    names_to = "X",
    values_to = "Y"
  ) %>%
  group_by(X) %>%
  summarise(
    y_mean = mean(Y, na.rm = TRUE),
    y_lower_95 = quantile(Y, 0.025, na.rm = TRUE),
    y_upper_95 = quantile(Y, 0.975, na.rm = TRUE),
    y_lower_50 = quantile(Y, 0.250, na.rm = TRUE),
    y_upper_50 = quantile(Y, 0.750, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(X = as.integer(X))

# ggplot で描画
bpi <- ggplot(predictions, aes(x = X)) +
  geom_ribbon(aes(ymin = y_lower_95, ymax = y_upper_95), fill = "lightblue", alpha = 0.5) + # 95%信頼区間
  geom_ribbon(aes(ymin = y_lower_50, ymax = y_upper_50), fill = "blue", alpha = 0.3) + # 50%信頼区間
  geom_line(aes(y = y_mean), color = "blue") + # 予測値の平均
  geom_point(data = d, aes(x = X, y = Y)) +
  labs(x = "説明変数 x", y = "予測値 y", title = "ベイズ予測区間（95%, 50%）") +
  theme_minimal() +
  theme(text = element_text(family = "Hiragino Sans"))

# 出力
bci | bpi
