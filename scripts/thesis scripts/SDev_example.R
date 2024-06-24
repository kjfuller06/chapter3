library(tidyverse)

df = data.frame(elevation = rnorm(n = 1000, mean = 1000, sd = 10))

dfall = data.frame()
for(i in 2:1000){
  df10 = data.frame(n = i,
                    elevation = sample_n(df, i, replace = T))
  df10$elevation_sd = sd(df10$elevation)
  dfall = rbind(dfall, df10)
}

g1 = ggplot(data = dfall, aes(x = n, y = elevation_sd)) +
  geom_point() +
  xlab("Sample size") +
  ylab("Standard deviation")
setwd("C:/chapter3/outputs")
ggsave("SDev_example.jpg", g1, device = "jpeg", height = 5, width = 5)
