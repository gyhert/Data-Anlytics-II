library(tidyverse)
library(plotly)

# ---------------- More Complex Dataset --------------#

setwd("C:/Users/ellen/OneDrive/DA2/Regression/Data")
incomeData <- read.csv(file="Income2.csv", header=TRUE, sep=",")

x <- as.matrix(cbind(incomeData[,2:3]))
y <- incomeData$Income

lmMod <- lm(Income ~ Education + Seniority, incomeData)
lmMod$coefficients

# squared error cost function
cost <- function(X, y, theta) {
  sum( (X %*% theta - y)^2 ) / (2*length(y))
}

# learning rate and iteration limit

alpha <- 0.1
num_iters <- 1000
epsilon <- .001

# keep history
cost_history <- double(num_iters)
theta_history <- list(num_iters)

# initialize coefficients
#theta <- matrix(c(0,0,0), nrow=3)
theta <- rep(0, ncol(x)+1)

# add a column of 1's for the intercept coefficient
X <- cbind(1, x)

# gradient descent
for (i in 1:num_iters) {
  error <- (X %*% theta - y)
  delta <- t(X) %*% error / length(y)
  theta <- theta - alpha * delta
  cost_history[i] <- cost(X, y, theta)
  theta_history[[i]] <- theta
  if ((sqrt(sum(delta^2)))<=epsilon) {break} 
}

tst <- data.frame(cost_history)

round(theta, 1)

# So what happened? NaN? (Not a Number)
# The problem is that the matrix that squares the errors gets to be really large numbers - REALLY large
# R has a way to deal with really large numbers and matrices, but not realistic here
# and you don't want to design an algorithm that blows up easily
# So, you scale the Data 

sX <- as.matrix(cbind(1, scale(X[,2:3])))
sy <- scale(y)


alpha <- 0.1
num_iters <- 1000
epsilon <- .001

# keep history
cost_history <- double(num_iters)
theta_history <- list(num_iters)

# initialize coefficients
#theta <- matrix(c(0,0,0), nrow=3)
theta <- rep(0, ncol(x)+1)

# add a column of 1's for the intercept coefficient
X <- sX
y <- sy
# gradient descent
for (i in 1:num_iters) {
  error <- (X %*% theta - y)
  delta <- t(X) %*% error / length(y)
  theta <- theta - alpha * delta
  cost_history[i] <- cost(X, y, theta)
  theta_history[[i]] <- theta
  #  if ((sqrt(sum(delta^2)))<=epsilon) {break} 
}

tst <- data.frame(cost_history)

theta

# OK that worked (whew!)
# ck against lm (different approaches will yield slightly different resutls - will study QR decomposition soon)
# and the 'intercept' becomes less relevant in 3+ dimemsional space
# it's really a 'garbage collector'

sMod <- lm(y ~ X[,2] + X[,3])
sMod$coefficients

# of course, you don't want scaled predictions.
# a number of ways we can deal with that
# you can run a model and backscale the results

# scale the original data again, without the intercept placeholer

sdata <- scale(incomeData[,2:4])
# Save scaled attibutes 
scaleList <- list(scale = attr(sdata, "scaled:scale"),
                  center = attr(sdata, "scaled:center"))


# convert the scaled data to a dataframe and run it through lm
sdata <- as.data.frame(sdata) 
smod <- lm(Income ~ Education + Seniority, data = sdata)

# create scaled predcitions for Income
sp <- predict(smod, sdata)

# Fit the same model to the original cars data:
omod <- lm(Income ~ Education + Seniority, data = incomeData)
op <- predict(omod, incomeData)

# Convert scaled prediction to original data scale:
usp <- sp * scaleList$scale["Income"] + scaleList$center["Income"]

# Compare predictions:
all.equal(op, usp)
# note that this all works because the method (QR) is the same

# so, now we've backscaled the predictions and that works
# let's try a different approach and backscale the coefficients

X <- incomeData[,2:3]
Y <- incomeData[,4]

library(R1magic)
betas.scaled <- matrix(smod$coefficients)
betas <- scaleBack.lm(X, Y, betas.scaled)
betas


# compare to original model
omod$coefficients


tM <- matrix(unlist(theta_history),ncol=2,byrow=TRUE)
Intercept <- tM[,1]
Slope <- tM[,2]
zM1 <- as.matrix(cost_history)


