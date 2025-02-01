library(ggplot2)
library(dplyr)

d <- read.csv(file = "chap04/input/data-salary.txt")
res_lm <- lm(Y ~ X, data = d)
X_new <- data.frame(X = 23:60)
conf_95 <- predict(res_lm, X_new, interval = "confidence", level = 0.95)
pred_95 <- predict(res_lm, X_new, interval = "prediction", level = 0.95)
pred_50 <- predict(res_lm, X_new, interval = "prediction", level = 0.50)

# 結果をデータフレームにまとめる
X_new <- X_new %>%
  mutate(
    fit = conf_95[, "fit"],
    lwr_conf = conf_95[, "lwr"],
    upr_conf = conf_95[, "upr"],
    lwr_pred = pred_95[, "lwr"],
    upr_pred = pred_95[, "upr"]
  )

# ggplot2を使って描画
ggplot() +
  geom_point(data = d, aes(x = X, y = Y), color = "blue", alpha = 0.6) + # 散布図
  geom_line(data = X_new, aes(x = X, y = fit), color = "red", linewidth = 1) + # 回帰直線
  geom_ribbon(
    data = X_new, aes(x = X, ymin = lwr_conf, ymax = upr_conf),
    fill = "blue", alpha = 0.2
  ) + # 95%信頼区間
  geom_ribbon(
    data = X_new, aes(x = X, ymin = lwr_pred, ymax = upr_pred),
    fill = "gray", alpha = 0.2
  ) + # 95%予測区間
  labs(
    title = "線形回帰の結果",
    x = "説明変数 X",
    y = "目的変数 Y"
  ) +
  theme_minimal() +
  theme(text = element_text(family = "Hiragino Sans"))
