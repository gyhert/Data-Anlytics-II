library(dplyr)  
library(tidyr)
library(ggplot2)
library(e1071)

setwd("C:/Users/ellen/OneDrive/DA2/Regression/Data")

tstData <- read.csv(file="Testing.csv")

p <- ggplot(data = tstData, aes(x=X1, y=Y)) + geom_point() + geom_smooth()
p

x<-tstData$X1
y<-tstData$Y



#for simple models nls find good starting values for the parameters even if it throw a warning
m<-nls(y~a*x/(b+x))
#get some estimation of goodness of fit
summary(m)

cor(y,predict(m))

tstData$newY <- predict(m)

p <- ggplot(data = tstData, aes(x=tstData$X1, y=tstData$Y)) + geom_point()+ geom_smooth() 
p
p <- p+ geom_point(data = tstData,  x=tstData$X1, y=tstData$newY, color = 'red') 
p
p <- p +  geom_smooth(data = tstData, aes(x=X1, y = newY), se=FALSE, color = "red")
p

summary(m)

# test vals
xT <- 30
tst<-(102.526*xT)/(50.586+xT)
tst

# ----------- log transforms --------------------#


model2 <- lm (y ~ log(X1), tstData)
tstData$logmodY <- predict(model2)

p <- ggplot(data = tstData, aes(x=tstData$X1, y=tstData$Y)) + geom_point()
p
p <- p + geom_point (data = tstData, aes(x=tstData$X1, y=tstData$logmodY), color = 'red')
p <- p + geom_smooth(data = tstData, aes(x=tstData$X1, y=tstData$logmodY), color = 'red')
p



#get some estimation of goodness of fit
cor(y,predict(m))

tstData$logModY <- predict(m)

p <- ggplot(data = tstData, aes(x=tstData$X1, y=tstData$Y)) + geom_point()+ geom_smooth() 
p
p <- p+ geom_point(data = tstData,  x=tstData$X1, y=tstData$logModY, color = 'red') 
p
p <- p +  geom_smooth(data = tstData, aes(x=X1, y = logModY), se=FALSE, color = "red")
p

summary(model2)

# test vals
xL <- 30
tst2 <- -28.2997 + 19.5278*log(xL)
tst2

