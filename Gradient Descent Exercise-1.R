library(plotly)

setwd("C:/Users/ellen/OneDrive/DA2/Regression/Data")
mydata <- read.csv(file="Ex1LS-2-BU.csv", header=TRUE, sep=",")

lmMod <- lm(Y ~ X1, mydata)
lmMod$coefficients
Intercept <- lmMod$coefficients[1]
X <- 3
y <- 7

alpha <- 0.02
num_iters <- 50
theta <- 0

# first time through the loop with i=1 and theta =0
# walk through this and compare to spreadsheet

# simulate iteration 1

error <- ((X %*% theta)+Intercept - y)
delta <- t(X) %*% error / length(y) # this is the derivative
theta <- theta - (alpha * delta)

error
delta
theta

# simulate iteration 2

error <- ((X %*% theta)+Intercept - y)
delta <- t(X) %*% error / length(y)
theta <- theta - (alpha * delta)

error
delta
theta

# Now, let's put this together in a full loop 


# squared error cost function - used to compute the cost at each point

cost2 <- function(X, y, theta) {
  sum ((((X %*% theta)+Intercept) - y)^2 ) / (2*length(y))
}

# this captures the values so you can look at the progress
tracerMat <- matrix(numeric(0), nrow = num_iters, ncol = 6)
colnames(tracerMat) <- c("i", "error", "delta", "Theta2", "Cost", "tst")

alpha <- 0.02
num_iters <- 50
theta <- 0
epsilon <- .005

#full loop.
for (i in 1:num_iters) {
  error <- ((X %*% theta)+Intercept - y)
  delta <- t(X) %*% error / length(y)
  theta <- theta - (alpha * delta)
  cost <- cost2(X, y, theta) # this is just for charting - not really needed
  tst <- sqrt(sum(delta^2))
  if(tst < epsilon)break 
    tracerMat[i,] <- c(i, error, delta, theta, cost, tst)
}

dfTracer <- data.frame(tracerMat)
dfTracer <- filter(dfTracer, !is.na(i) )


# plot the full cost function
thetaV <- seq(0,3, by=.1)
FunchartPts <- NULL
for(i in 1:length(thetaV)){
  rwt <- cost2(X, y, thetaV[i])
  FunchartPts <- rbind(FunchartPts, c(thetaV[i], rwt)) 
}
dfFunChartPts <- data.frame(FunchartPts)
colnames(dfFunChartPts)[1] <- "Theta2"
colnames(dfFunChartPts)[2] <- "Cost"

p <- ggplot(data = dfFunChartPts, aes(x=Theta2, y=Cost)) + geom_smooth()
p <- p + scale_x_continuous(limits = c(0, 3))
p

# now we plot the GD points

p <- p + geom_point(data = dfTracer, aes(x=Theta2, y=Cost), color = 'red') 
p





