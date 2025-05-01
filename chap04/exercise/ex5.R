library(cmdstanr)
library(posterior)
library(dplyr)

source("chap04/exercise/generate-data.R")

data <- list(N1 = N1, N2 = N2, Y1 = Y1, Y2 = Y2)
model <- cmdstan_model("chap04/exercise/ex5.stan")

fit <- model$sample(
  data = data,
  seed = 1234,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 2000
)

# MCMCサンプルの抽出
draws <- fit$draws()
ms <- draws %>% as_draws_df()
prob <- mean(ms$mu1 < ms$mu2) # => 0.9457
