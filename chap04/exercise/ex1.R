library(ggplot2)

source("chap04/exercise/generate-data.R")

d1 <- data.frame(group = 1, Y = Y1)
d2 <- data.frame(group = 2, Y = Y2)
d <- rbind(d1, d2)
d$group <- as.factor(d$group)

p <- ggplot(data = d, aes(x = group, y = Y, group = group, col = group)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3) +
  geom_point(position = position_jitter(w = 0.1, h = 0), size = 2)
print(p)
ggsave(file = "chap04/exercise/fig-ex1.png", plot = p, dpi = 300, w = 4, h = 3)
