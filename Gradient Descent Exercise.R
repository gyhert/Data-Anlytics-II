library(plotly)

setwd("C:/Users/ellen/OneDrive/DA2/Regression/Data")
mydata <- read.csv(file="Ex1LS-2-BU.csv", header=TRUE, sep=",")

lmMod <- lm(y ~ X)
lmMod <- lm(Y ~ X1, mydata)
lmMod$coefficients
Intercept <- lmMod$coefficients[1]
X <- 3
y <- X*lmMod$coefficients[2] + lmMod$coefficients[1]


# squared error cost function - Cost2 forces an intercept
cost2 <- function(X, y, theta) {
  sum ((((X %*% theta)+Intercept) - y)^2 ) / (2*length(y))
}


alpha <- 0.02
num_iters <- 50
theta <- 0
chartPts <- NULL
#epsilon <- .01

tracerMat <- matrix(numeric(0), nrow = num_iters, ncol = 6)
colnames(tracerMat) <- c("i", "error", "delta", "theta", "cost", "sqrt(sum(delta^2))")

for (i in 1:num_iters) {
  error <- ((X %*% theta)+Intercept - y)
  delta <- t(X) %*% error / length(y)
  theta <- theta - (alpha * delta)
  cost <- cost2(X, y, theta)
  chartPts <- rbind(chartPts, c(theta, cost)) 
#  if ((sqrt(sum(delta^2)))<=epsilon) {break} 
  tracerMat[i,] <- c(i, error, delta, theta, cost, sqrt(sum(delta^2)))
}

write.csv(tracerMat, "tracerMat.csv")

round(theta,1)

#thetaV <- seq(lmMod$coefficients[2]-range,lmMod$coefficients[2]+range, by=.1)
 thetaV <- seq(0,3, by=.1)

FunchartPts <- NULL
for(i in 1:length(thetaV)){
  rwt <- cost2(X, y, thetaV[i])
  FunchartPts <- rbind(FunchartPts, c(thetaV[i], rwt)) 
  }


dfFunChartPts <- data.frame(FunchartPts)
colnames(dfFunChartPts)[1] <- "Theta2"
colnames(dfFunChartPts)[2] <- "Cost"

dfChartPts <- data.frame(chartPts)
colnames(dfChartPts)[1] <- "Theta2"
colnames(dfChartPts)[2] <- "Cost"

p <- ggplot(data = dfFunChartPts, aes(x=Theta2, y=Cost)) + 
  geom_point() + geom_smooth()
p <- p + scale_x_continuous(limits = c(0, 3))
p
p <- p + geom_point(data = dfChartPts, aes(x = Theta2, y = Cost, color = 'red'))  
p

