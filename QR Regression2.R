library(pracma)
library(tidyverse)
library(ggplot2)

QR.regression <- function(y, X)
{
  nr <- length(y)
  nc <- NCOL(X)
  
  for (j in seq_len(nc))
  {
    id <- seq.int(j, nr) 
    sigma <- sum(X[id,j]^2) 
    s <- sqrt(sigma) 
    diag_ej <- X[j,j] 
    gamma <- 1.0 / (sigma + abs(s * diag_ej)) 
    kappa <- if (diag_ej < 0) s else -s 
    X[j,j] <- X[j,j] - kappa
    if (j < nc)
      for (k in seq.int(j+1, nc))
      {
        yPrime <- sum(X[id,j] * X[id,k]) * gamma
        X[id,k] <- X[id,k] - X[id,j] * yPrime
      }
    yPrime <- sum(X[id,j] * y[id]) * gamma
    y[id] <- y[id] - X[id,j] * yPrime
    X[j,j] <- kappa
  } # end Householder
  X2 <- X  
  y2 <- y
  
  # residual sum of squares
  rss <- sum(y[seq.int(nc+1, nr)]^2)
  
  # Backsolve
  beta <- rep(NA, nc)
  for (j in seq.int(nc, 1))
  {
    beta[j] <- y[j]
    if (j < nc)
      for (i in seq.int(j+1, nc))
        beta[j] <- beta[j] - X[j,i] * beta[i]
      beta[j] <- beta[j] / X[j,j]
      X3 <- X
      y3 <- y
      
  }
  
  # set zeros in the lower triangular side of X (which stores) 
  # not really necessary, this is just to return R for illustration
  for (i in seq_len(ncol(X)))
    X[seq.int(i+1, nr),i] <- 0
    X4 <- X

    list(R=X[1:nc,1:nc], y=y, beta=beta, rss=rss, X2 = X2, y2 = y2, X3 = X3, y3 = y3, X4 = X4)
    }



# --------- 1 variable model -------------------------------#

setwd("C:/Users/ellen/OneDrive/DA2/Regression/Data")
mydata <- read.csv(file="Ex1LS2.csv", header=TRUE, sep=",")
model <- lm( Y ~ X1 ,mydata)
model$coefficients

p <- ggplot(data = mydata, aes(x= X1, y= Y)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)
p

Y <- mydata$Y
X <- mydata$X1
X <- as.matrix(cbind(1, X))

res <- QR.regression(Y, X)

res$beta
R <- round(res$R,2)
R


# ----------------- 2 variable model ---------------------- #

library(pracma)
setwd("C:/Users/ellen/OneDrive/DA2/Regression/Data")
mydata <- read.csv(file="Ex1LS.csv", header=TRUE, sep=",")

model <- lm( Y ~ ., mydata)
model$coefficients


X <- mydata[1:2]

p <- ggplot(data = mydata, aes(x= X, y= Y)) + geom_point() + geom_smooth(method = 'lm', se = FALSE) 
p

Y <- mydata$Y
X <- mydata[1:2]
X <- as.matrix(X)
X <- as.matrix(cbind(1, X))

res <- QR.regression(Y, X)
res$beta
res$R


# ------------------ Auto Dataset regression ---------------#

setwd("C:/Users/ellen/OneDrive/DA2/Regression/Data")
Autos <- read.csv(file="Automobile Price Prediction.csv")
Autos <- dplyr::select(Autos, wheel.base, engine.size, horsepower, highway.mpg, price )


model <- lm( price ~ ., Autos)

model$coefficients

p <- ggplot(data = Autos, aes(x= horsepower, y= price)) + geom_point() + geom_smooth(method = 'lm', se = FALSE) 
p

Y <- Autos$price
X <- Autos[1:4]
X <- as.matrix(X)
X <- as.matrix(cbind(1, X))

res <- QR.regression(Y, X)
res$beta

R <- res$R
R



