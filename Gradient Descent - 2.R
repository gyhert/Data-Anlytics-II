library(tidyverse)
library(plotly)

setwd("C:/Users/ellen/OneDrive/DA2/Regression/Data")
mydata <- read.csv(file="Ex1LS-2-BU.csv", header=TRUE, sep=",")

x <- as.matrix(mydata[,1])
y <- mydata[,3]

lmMod <- lm(Y ~ X1, mydata)
lmMod$coefficients
  
# squared error cost function
cost <- function(X, y, theta) {
  sum( (X %*% theta - y)^2 ) / (2*length(y))
}

# learning rate and iteration limit

alpha <- 0.1 # learning rate
num_iters <- 1000 # try different values 500 and 1000
epsilon <- .001 # try different values .01 and .001

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
conv <- data.frame(cost_history)
conv[2]<- as.numeric(row.names(conv))
colnames(conv)[2]<-"iteration"

p <- ggplot (data = conv, aes(x = iteration, y = cost_history))+ geom_point()
p <- p + scale_x_continuous(limits = c(1, 500))
p

round(theta, 1)

tM <- matrix(unlist(theta_history),ncol=2,byrow=TRUE)
Intercept <- tM[,1]
Slope <- tM[,2]
zM1 <- as.matrix(cost_history)

# ------------------------ graphing 3 variables -----------#

Cost <- matrix(0,length(Intercept), length(Slope));

for(i in 1:length(Intercept)){
  for(j in 1:length(Slope)){
    t1 = c(Intercept[i],Slope[j])
    Cost[i,j]= zM1[j]
  }
}

dfSurface1 <- data.frame(Cost)
zM1 <- as.matrix(dfSurface1)

p <- plot_ly (x =  Intercept, y = Slope, z = zM1) %>% add_surface()
p



# ------------------------ graphing cost with GD -----------#


Intercept1 <- seq(from=(round((theta[1])-(theta[1]*.5)))
                  ,to=(round((theta[1])+(theta[1]*.5)))
                  ,length=num_iters)

Slope1 <- seq(from=(round((theta[2])-(theta[2]*.5)))
              ,to=(round((theta[2])+(theta[2]*.5)))
              ,length=num_iters)


Cost1 <- matrix(0,length(Intercept1), length(Slope1));
for(i in 1:length(Intercept1)){
  for(j in 1:length(Slope1)){
    t1 = c(Intercept1[i],Slope1[j])
    Cost1[i,j]= cost(X, y, t1)
  }
}

dfSurface1 <- data.frame(Cost1)
zM1 <- as.matrix(dfSurface1)

p <- plot_ly (x =  Intercept1, y = Slope1, z = zM1) %>% add_surface()
p


