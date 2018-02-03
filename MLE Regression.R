library(tidyverse)
library(ggplot2)
library(np)
library(foreign)
library(digest)
library(plotly)

setwd("C:/Users/ellen/OneDrive/Documents/Spring 2017/Section III/History")

mydata <- read.csv(file="Ex1LS.csv", header=TRUE, sep=",")

p <- plot_ly (x = ~ mydata$X, y = ~ mydata$Y, type = 'scatter')
p

tstMod <- lm(Y ~ X -1, mydata)
tstMod$coefficients


linear.lik <- function(theta, y, X){
  n      <- nrow(X)
  k      <- ncol(X)
  beta   <- theta[1:k]
  sigma2 <- theta[k+1]^2
  e      <- y - X%*%beta
  logl   <- -.5*n*log(2*pi)-.5*n*log(sigma2) - ( (t(e) %*% e)/ (2*sigma2) )
  return(-logl)
}

surface <- list()
k <- 0
for(beta in seq(0, 5, 0.1)){
  for(sigma in seq(0.1, 5, 0.1)){
    k <- k + 1
    logL <- linear.lik(theta = c(0, beta, sigma), y = mydata$Y, X = cbind(0, mydata$X))
    surface[[k]] <- data.frame(beta = beta, sigma = sigma, logL = -logL)
  }
}
surface <- do.call(rbind, surface)

dfSurface <- data.frame(surface)

zM <- as.matrix(dfSurface)

p <- plot_ly ( z = zM) %>% add_surface() %>%
layout(
  title = "Maximum Likelihood",
  scene = list(
    xaxis = list(title = "beta"),
    yaxis = list(title = "sigma"),
    zaxis = list(title = "likelihood")
  ))
p

max.row <- row.names(dfSurface)[(which(dfSurface$logL==max(dfSurface$logL)))]
maxRow <- dfSurface[max.row,]
round(maxRow$beta,1)
round(tstMod$coefficients,1)
max(dfSurface$sigma)

