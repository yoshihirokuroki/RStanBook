# library(ggplot2)
#
# load('output/result-model4-5.RData')
# ms <- rstan::extract(fit)
#
# X_new <- 23:60
# N_X <- length(X_new)
# N_mcmc <- length(ms$lp__)
#
# set.seed(1234)
# y_base_mcmc <- as.data.frame(matrix(nrow=N_mcmc, ncol=N_X))
# y_mcmc <- as.data.frame(matrix(nrow=N_mcmc, ncol=N_X))
# for (i in 1:N_X) {
#   y_base_mcmc[,i] <- ms$a + ms$b * X_new[i]
#   y_mcmc[,i] <- rnorm(n=N_mcmc, mean=y_base_mcmc[,i], sd=ms$sigma)
# }
#
#
# ## プロットするために50%信用区間と95%信用区間を作ります（ここが1番大切です）
# # y_base_mcmcは行方向がMCMCサンプル、列方向がX方向です。
# # applyの2つめの引数である「2」は, 列方向「2」を残して行方向「1」についてまとめるという意味です。
# # data.frame関数の引数「check.names = FALSE」は数字はじまりや%を含む文字列もそのまま列名にするという意味です。
# # 個人的には変な変換されるよりそのままの列名の方が分かりやすいのでそのようにしています。
# # ただし、数字始まりや%を含む列名を指定するときはバッククォートで囲む必要があります（後述）。
# qua <- apply(y_base_mcmc, 2, quantile, probs=c(0.025, 0.25, 0.50, 0.75, 0.975))
# d_est <- data.frame(X=X_new, t(qua), check.names = FALSE)
#
# ## plot
# # ggplot(data = d_est, aes(...)) のようにggplot関数の中でdataを指定する方法もありますが、
# # 複数種類以上のデータを使う場合には、各geom_xxx関数の中でdataを指定する方が分かりやすいかもしれないと思ったのでそのようにしました。
# p <- ggplot() +
#   theme_bw(base_size=18) +
#   geom_ribbon(data=d_est, aes(x=X, ymin=`2.5%`, ymax=`97.5%`), fill='black', alpha=1/6) +
#   geom_ribbon(data=d_est, aes(x=X, ymin=`25%`, ymax=`75%`), fill='black', alpha=2/6) +
#   geom_line(data=d_est, aes(x=X, y=`50%`), linewidth=1) +
#   geom_point(data=d, aes(x=X, y=Y), shape=1, size=3) +
#   coord_cartesian(xlim=c(22, 61), ylim=c(200, 1400)) +
#   scale_y_continuous(breaks=seq(from=200, to=1400, by=400)) +
#   labs(y='Y')
# ggsave(p, file='output/fig4-8-left.png', dpi=300, w=4, h=3)
#
#
# qua <- apply(y_mcmc, 2, quantile, probs=c(0.025, 0.25, 0.50, 0.75, 0.975))
# d_est <- data.frame(X=X_new, t(qua), check.names = FALSE)
#
# p <- ggplot() +
#   theme_bw(base_size=18) +
#   geom_ribbon(data=d_est, aes(x=X, ymin=`2.5%`, ymax=`97.5%`), fill='black', alpha=1/6) +
#   geom_ribbon(data=d_est, aes(x=X, ymin=`25%`, ymax=`75%`), fill='black', alpha=2/6) +
#   geom_line(data=d_est, aes(x=X, y=`50%`), linewidth=1) +
#   geom_point(data=d, aes(x=X, y=Y), shape=1, size=3) +
#   coord_cartesian(xlim=c(22, 61), ylim=c(200, 1400)) +
#   scale_y_continuous(breaks=seq(from=200, to=1400, by=400)) +
#   labs(y='Y')
# ggsave(p, file='output/fig4-8-right.png', dpi=300, w=4, h=3)


library(cmdstanr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(posterior)
library(patchwork)

set.seed(1234)

d <- read.csv(file = "chap04/input/data-salary.txt")

# StanモデルのMCMC結果を読み込む
fit <- readRDS("chap04/output/fit-model4-5.rds")

# MCMCサンプルを取得（3次元array: iterations x chains x parameters）
draws <- fit$draws()

# 3次元配列からデータフレームに変換
df_draws <- as_draws_df(draws) %>%
  rename(iteration = .iteration) # iterationの名前を統一

# sigma に NA が含まれていないかチェック
df_draws <- df_draws %>% filter(!is.na(sigma))

# x の範囲を指定
x_values <- 23:60

# すべての x に対して MCMC サンプルを適用（y = a + b * x）
credidible <- df_draws %>%
  crossing(x = x_values) %>%
  mutate(y_fit = a + b * x) %>% # 予測値の平均のみ計算
  group_by(x) %>%
  summarise(
    y_mean = mean(y_fit, na.rm = TRUE),
    y_lower_95 = quantile(y_fit, 0.025, na.rm = TRUE),
    y_upper_95 = quantile(y_fit, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

# ggplot で描画（ベイズ信用区間）
bci <- ggplot(credidible, aes(x = x)) +
  geom_ribbon(aes(ymin = y_lower_95, ymax = y_upper_95), fill = "lightblue", alpha = 0.5) + # 95%信用区間
  geom_line(aes(y = y_mean), color = "blue", size = 1) + # 回帰線（y = a + b * x の平均）
  geom_point(data = d, aes(x = X, y = Y), color = "red", size = 2) + # 実データ
  labs(x = "説明変数 x", y = "予測値 y", title = "ベイズ信用区間（95%）") +
  theme_minimal() +
  theme(text = element_text(family = "Hiragino Sans"))

# すべての x に対して MCMC サンプルを適用
predictions <- df_draws %>%
  crossing(x = x_values) %>% # expand.grid の代わりに crossing() を使う
  mutate(y_pred = a + b * x + rnorm(n(), mean = 0, sd = sigma)) %>%
  group_by(x) %>%
  summarise(
    y_mean = mean(y_pred, na.rm = TRUE),
    y_lower_50 = quantile(y_pred, 0.25, na.rm = TRUE),
    y_upper_50 = quantile(y_pred, 0.75, na.rm = TRUE),
    y_lower_95 = quantile(y_pred, 0.025, na.rm = TRUE),
    y_upper_95 = quantile(y_pred, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

# ggplot で描画
bpi <- ggplot(predictions, aes(x = x)) +
  geom_ribbon(aes(ymin = y_lower_95, ymax = y_upper_95), fill = "lightblue", alpha = 0.5) + # 95%信頼区間
  geom_ribbon(aes(ymin = y_lower_50, ymax = y_upper_50), fill = "blue", alpha = 0.3) + # 50%信頼区間
  geom_line(aes(y = y_mean), color = "blue") + # 予測値の平均
  geom_point(data = d, aes(x = X, y = Y)) +
  labs(x = "説明変数 x", y = "予測値 y", title = "ベイズ予測区間（95%, 50%）") +
  theme_minimal() +
  theme(text = element_text(family = "Hiragino Sans"))

# 出力
bci | bpi
