library(tidyverse)
library(ggplot2)    

setwd("C:/Users/ellen/OneDrive/DA2/Regression/Data")

tst <- read_csv("Idata.csv")
m3 <- lm(Y3 ~ X, data = tst)
tst$P3 <- predict(m3, tst)
#wlsMod <- lm(Y3 ~ X, weights = 1/(X^5), data = tst)
wlsMod <- lm(Y3 ~ X, weights = 1/((X+Y3)^5), data = tst)
tst$W3 <- predict(wlsMod, tst)

p <- ggplot(data = tst)
p <- p + geom_point( aes(x = X, y = Y3)) + geom_smooth(aes(x=X, y = P3), method = 'lm')
p <- p + geom_point( aes(x = X, y = W3), color = 'red') + geom_smooth(aes(x=X, y = W3), method = 'lm', color = 'red')
p

summary(wlsMod)
