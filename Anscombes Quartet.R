library(tidyverse)
library(ggplot2)

# Anscombe's "quartet"

setwd("C:/Users/ellen/OneDrive/DA2/Regression/Data")

tst <- read_csv("Idata.csv")

m1 <- lm(Y1 ~ X, data = tst)
m2 <- lm(Y2 ~ X, data = tst)
m3 <- lm(Y3 ~ X, data = tst)
m4 <- lm(Y4 ~ X4, data = tst)

p <- ggplot(data = tst, aes(x = X, y = Y1))
p <- p + geom_point()
p <- p + geom_abline(intercept = round(m1$coefficients[1], digits = 1), slope = round(m1$coefficients[2], digits = 1))
p <- p + scale_x_continuous(limits = c(0, 20))
p <- p + scale_y_continuous(limits = c(0, 20))
p

round(m1$coefficients, 1)

p <- ggplot(data = tst, aes(x = X, y = Y2))
p <- p + scale_x_continuous(limits = c(0, 20))
p <- p + scale_y_continuous(limits = c(0, 20))
p <- p + geom_point(color = 'red') + theme(legend.position="none")
p
p <- p + geom_abline(intercept = round(m2$coefficients[1], digits = 1), slope = round(m2$coefficients[2], digits = 1), color = 'red')  + theme(legend.position="none")
p

round(m2$coefficients, 1)


p <- ggplot(data = tst, aes(x = X, y = Y3))
p <- p + scale_x_continuous(limits = c(0, 20))
p <- p + scale_y_continuous(limits = c(0, 20))
p <- p + geom_point(color = 'dark green')
p
p <- p + geom_abline(intercept = round(m3$coefficients[1], digits = 1), slope = round(m3$coefficients[2], digits = 1), color = 'dark green')  + theme(legend.position="none")
p

round(m3$coefficients, 1)

p <- ggplot(data = tst, aes(x = X, y = Y4))
p <- p + scale_x_continuous(limits = c(0, 20))
p <- p + scale_y_continuous(limits = c(0, 20))
p <- p + geom_point(color = 'blue')
p
p <- p + geom_abline(intercept = round(m4$coefficients[1], digits = 1), slope = round(m4$coefficients[2], digits = 1), color = 'blue')
p <- p + theme(legend.position="none")
p

round(m4$coefficients, 1)


# think about the affect of outliers on the linear model

# let's take a close look at the second model

p <- ggplot(data = tst, aes(x = X, y = Y2)) +
  geom_point() + 
  geom_abline(intercept = round(m2$coefficients[1], digits = 1), slope = round(m2$coefficients[2], digits = 1))
p <- p + scale_x_continuous(limits = c(0, 20))
p <- p + scale_y_continuous(limits = c(0, 20))
p

# let's fit using a polynomial 
# remember, it's still a linear model as long as the coefficients are not exponential


tst2 <- select(tst, Y2, X)
lfit <- lm(Y2 ~ X + I(X^2), tst2)
X <- rep(1:20)
newD <- data.frame(X)
newY <- predict(lfit, newD)


p <- p +  geom_smooth(data = newD, aes(x=X, y = newY), se=FALSE, color = "blue")
p


# keep all this in mind as you read 3.3.3 in ISL
