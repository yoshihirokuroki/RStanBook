# generate-data.Rを実行
source("chap04/exercise/generate-data.R")

# excercise 1
library(ggplot2)
ggplot() +
  geom_density(aes(x = Y1), color = "blue") +
  geom_density(aes(x = Y2), color = "pink")



library(cmdstanr)
library(posterior)
library(dplyr)

fit <- readRDS("chap04/exercise/fit-ex3.rds")

# MCMCサンプルの抽出
draws <- fit$draws()
ms <- draws %>% as_draws_df()
prob <- mean(ms$mu1 < ms$mu2) # => 0.9265

N_mcmc <- length(ms$mu1)
prob <- sum(ms$mu1 < ms$mu2) / N_mcmc # => 0.9265
