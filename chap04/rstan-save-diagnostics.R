# library(rstan)
#
# load('output/result-model4-5.RData')
#
# write.table(data.frame(summary(fit)$summary, check.names=FALSE),
#   file='output/fit-summary.csv', sep=',', quote=TRUE, col.names=NA)
#
#
# library(ggmcmc)
# ggmcmc(ggs(fit, inc_warmup=TRUE, stan_include_auxiliar=TRUE),
#   file='output/fit-traceplot.pdf', plot='traceplot')
# ggmcmc(ggs(fit), file='output/fit-ggmcmc.pdf')
#
#
# library(coda)
# pdf(file='output/fit-traceplot-coda.pdf')
# plot(As.mcmc.list(fit))
# dev.off()

library(cmdstanr)
library(bayesplot)
library(posterior)
library(coda)
library(ggplot2)

# fitオブジェクトをrdsファイルから読み込む
fit <- readRDS("chap04/output/fit-model4-5.rds")

# サマリーの出力
summary_fit <- fit$summary()
write.table(summary_fit, file = "chap04/output/fit-summary.csv", sep = ",", quote = TRUE, col.names = NA)

traceplot_gg <- mcmc_trace(fit$draws())
print(traceplot_gg)
ggsave("chap04/output/fit-traceplot-bayesplot.png", plot = traceplot_gg)
