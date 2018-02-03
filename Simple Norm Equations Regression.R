library(tidyverse)
library(ggplot2)

setwd("C:/Users/ellen/OneDrive/Documents/Spring 2017/Section III/History")
mydata <- read.csv(file="Ex1LS.csv", header=TRUE, sep=",")

p <- ggplot(mydata, aes(x=X, y=Y))+geom_point() 
p

model <- lm( Y ~ X, mydata)
mydata$newY <- predict(model, mydata)

# draw the lines for emphasis

l1 <- mydata[ which(mydata$X==1),]
newrow <- c(mydata[1,1], mydata[1,3], mydata[1,2])
l1<-rbind (l1, newrow)
l1$newY <- NULL

l2 <- mydata[ which(mydata$X==2),]
newrow <- c(mydata[2,1], mydata[2,3], mydata[2,2])
l2<-rbind (l2, newrow)
l2$newY <- NULL

l3 <- mydata[ which(mydata$X==3),]
newrow <- c(mydata[3,1], mydata[3,3], mydata[3,2])
l3<-rbind (l3, newrow)
l3$newY <- NULL

l4 <- mydata[ which(mydata$X==4),]
newrow <- c(mydata[4,1], mydata[4,3], mydata[4,2])
l4<-rbind (l4, newrow)
l4$newY <- NULL

ggplot(mydata, aes(x=X, y=Y)) +
geom_point(data=mydata, aes(X, Y), color = "blue") +
theme(
  panel.background = element_rect(fill = "white")
)


p <- ggplot(mydata, aes(x=X, y=Y)) 
p + geom_point() + geom_point(data=mydata, aes(X, newY), color = "blue") + 
  geom_smooth(method = "lm", se = FALSE) +
  geom_line (data=l1, color="red")+ 
  geom_line (data=l2, color="red")+ 
  geom_line (data=l3, color="red")+
  geom_line (data=l4, color="red")+
  theme(
        panel.background = element_rect(fill = "white")
)

summary(model)

# Residual sum of squares
rss <- sum(residuals(model)^2)
rss

# Mean squared error
mse <- (rss/df.residual(model))
mse

# Residual standard error
rse <- sqrt(deviance(model)/df.residual(model))
rse

# Root mean squared error
rmse <- sqrt(mse)
rmse

# we're not going to cover ANOVA, but here it is:
anova(model)

fitted(model)

# set up the matrix and add value for intercept
# solve the normal equation using linear algebra
X <- cbind(1, mydata$X)
y <- mydata$Y
betaHat <- solve(t(X)%*%X) %*% t(X) %*%y
print(betaHat)

# now solving using SVD
x <- t(X) %*% X
duv <- svd(x)
x.inv <- duv$v %*% diag(1 / duv$d) %*% t(duv$u)
x.pseudo.inv <- x.inv %*% t(X)
w <- x.pseudo.inv %*% y
w

# note we can also use SVD for dimension reduction (like PCA) 
# it's also used in advanced numerical solutions (won't be doing that here)


