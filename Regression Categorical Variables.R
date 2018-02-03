library(tidyverse)  
library(tidyr)
library(ggplot2)
library(e1071)

setwd("C:/Users/ellen/OneDrive/DA2/Regression/Data")
Autos <- read_csv(file="Automobile Price Prediction.csv")
p <- ggplot(Autos, aes(x=horsepower, y=price))+geom_point() 
p
model1 <- lm(price ~ horsepower, Autos)
summary(model1)
p <- p + geom_abline(intercept = coef(model1)[1], slope = coef(model1)[2], linetype = 'dashed')
p

model2 <- lm(price ~ horsepower + make, Autos)
summary(model2)

p <- p + geom_abline(intercept = coef(model2)[1], slope = coef(model2)[2])
p

# BMW
p <- p + geom_abline(intercept = coef(model2)[1] + coef(model2)[4] , slope = coef(model2)[2], color = 'blue')
p

# Honda

p <- p + geom_abline(intercept = coef(model2)[1] + coef(model2)[7] , slope = coef(model2)[2], color = 'red')
p

AllMakes <- length(unique(Autos$make))+1

p <- p + geom_abline(intercept = coef(model2)[1] + AllMakes , slope = coef(model2)[2], color = 'green')
p



# get dummy variables - just FYI

contrasts(as.factor(Autos$make))

