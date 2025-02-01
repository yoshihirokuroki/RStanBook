library(cmdstanr)
library(dplyr)

fit <- readRDS("chap04/output/fit-model4-5.rds")

# ms <- rstan::extract(fit)
# N_mcmc <- length(ms$lp__)
# y50_base <- ms$a + ms$b * 50
# y50 <- rnorm(n=N_mcmc, mean=y50_base, sd=ms$sigma)
# d_mcmc <- data.frame(a=ms$a, b=ms$b, sigma=ms$sigma, y50_base, y50)

# 既にfitオブジェクトが存在している前提

# MCMCサンプルの抽出
draws <- fit$draws()

set.seed(1234)

d_mcmc <- draws %>%
  as_draws_df() %>%
  mutate(y50_base = a + b * 50) %>%
  mutate(y50 = rnorm(n = dim(draws)[1] * dim(draws)[2], mean = y50_base, sd = sigma)) %>%
  as.data.frame() %>%
  select(a, b, sigma, y50, y50_base)
