library(cmdstanr)

source("chap04/exercise/generate-data.R")

data <- list(N1 = N1, N2 = N2, Y1 = Y1, Y2 = Y2)
model <- cmdstan_model("chap04/exercise/ex3.stan")

fit <- model$sample(
  data = data,
  seed = 1234,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 2000
)

# fitオブジェクトをrdsファイルとして保存
fit$save_object("chap04/exercise/fit-ex3.rds")
