library(tidyverse)
library(e1071)
library(kernlab)
library(stringr)

setwd("Intro")

A <- matrix ( c(2, 2)) # this creates a vector

rm(A) #good to keep in mind if you're dealing wiht large datasets

A <- matrix ( c(2, 2)) # this creates a vector

A <- cbind(A, c(3,1)) # then combine vectors with cbind or

A <- matrix (c(2,3), nrow =1)

A <- rbind(A, c(2,1)) # then combine rows

A <- matrix( c(2, 2, 3, 1), nrow=2, ncol = 2) # or just create the matrix at once.

write.csv(A, file = "A.csv", row.names = FALSE)

A <- read.csv(file = "A.csv", header=TRUE, stringsAsFactors=FALSE)

# read.csv creates a dataframe (covered later)
str(A) #you always need to check on the data types that dataframes create
A <- as.matrix(A)

# s create a couple of vectors and move on with matrix operations
B <- c(3,2) # vector 1
C <- c(0,1) # Vector 2

D <- A #easy to duplicate data structures
D <- A[,1] # or parts - notice that D beccomes a vector
D <- cbind(D, A[,2]) # and back again
D <- t(D) 

E <- A+D # has to be same structure 
E <- B+C 
E <- B-C 

E <- A*D # elements
G <- A %*% D
H <- crossprod(A,D) # equal to t(A) %*% B or A %*% t(B)

# create identity matrix
E <- diag(A)
I <- diag(2)

J <- det(A)
J
#creates an inverse matrix
K <- solve(A)

# check to make sure it's right - should be identity matrix
L <- K%*%A

# singular value decomposition

X <- matrix( c(1, 1, 1, 1, 1, 2, 3, 4), nrow=4, ncol = 2) 

SingVal <- svd(X)
U <- SingVal$u
D <- diag(SingVal$d)
V <- t(SingVal$v)

X2 <- U %*% D %*% V

#  X = U D V'


# another use for solve - solving systems of equations.

# for example:
# 8x_1  + 20x_2 ???56  = 0
# 20x_1 + 60x_2 ???154 = 0 

X <- matrix( c(8, 20, 20, 60), nrow=2, ncol = 2) 
B <- matrix( c(56, 154), nrow=2, ncol = 1) 
solve(X, B)

# it just so happens, these are the two derivatives of the
# sum of squares of the following data

mydata <- read.csv(file="Ex1LS.csv", header=TRUE, sep=",")


X <- cbind(1, mydata$X)
y <- mydata$Y
# we can solve this from the raw data by using a transpose
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

# exercise for fun

V1 <- data.frame(x=c(0, 3), y=c(0, 4))

p <- ggplot(V1, aes(x=x, y=y))+geom_point(color="black") 
p <- p + geom_segment(aes(x = V1[1,1], y = V1[1,2], xend = V1[2,1], yend =V1[2,2] ), arrow = arrow(length = unit(0.5, "cm")))
p <- p +xlim(0, 20) + ylim(0,20)
p

# create and draw the eigenvector (transpose first)
tV1 <- t(V1)
eV1 <- cbind(eigen(tV1)$values, eigen(tV1)$vectors)
ev2 <- eigen(V1)$values

V2<-data.frame(X=c(0, (eV1[1,1]*eV1[1,2])),Y=c(0, (eV1[1,1]*eV1[2,2])))
p <- p + geom_segment(aes(x = V2[1,1], y = V2[1,2], xend = V2[2,1], yend =V2[2,2] ), col="blue", linetype="dashed", arrow = arrow(length = unit(0.5, "cm")))
p

#create and draw the dot product vector
V3 <- as.matrix(V1)
dpV3 <- V3%*%V3
p <- p + geom_segment(aes(x = dpV3[1,1], y = dpV3[1,2], xend = dpV3[2,1], yend =dpV3[2,2] ), col="red", linetype="dashed", arrow = arrow(length = unit(0.5, "cm")))
p

# draw a right triangle (just for visual reference)

p <- p + geom_segment(aes(x = V1[2,1], y = V1[1,1], xend = V1[2,1], yend =V1[2,2] ), col="blue", linetype="dashed")
p

# get the vector magnitude (eclidian norm)
norm_vec <- function(x)sqrt(sum(x^2)) 
mV1 <- norm_vec(V1)

# Calculte the Direction Vector and Show that the norm = 1
cosX <- V1[2,1]/mV1
cosY <- V1[2,2]/mV1
V4 <- data.frame(X=c(0,cosX), Y=c(0,cosY))
# norm of the direction vector should be 1


p <- p + geom_segment(aes(x = V4[1,1], y = V4[1,2], xend = V4[2,1], yend =V4[2,2] ), col="red", linetype="dashed", arrow = arrow(length = unit(0.5, "cm")))
p

norm_vec(V4)

mV4 <- as.matrix(V4)
norm(mV4, type = '2')


# ----------------- correlation matrix  review ---------------#

Advertising <- dbGetQuery(con,'
SELECT 
 [TV]
,[Radio]
,[Newspaper]
,[Sales]
FROM [Accounting].[dbo].[Advertising]
')


Ad <- dplyr::select(Advertising, Sales, TV, Radio, Newspaper)

cor(Ad, method = 'pearson', use = 'pairwise')

# ------------------------  reviewing intro again: bias vs variance ---------------------------#

# just an illustration to demo model bias vs variance

# this a a root mean squared error function - we'll use this a lot down the road

rmse <- function(error)
{
  sqrt(mean(error^2))
}

# now let's generate some data

x <- seq(from=0, to=20, by=0.1)
y <- 500 + 0.4 * (x-10)^3
noise <- rnorm(length(x), mean=10, sd=80)
y <- y + noise
x <- x+ 2000

# put it in a dataframe

kData <- data.frame(x, y)

# and let's say we've finished 2015, so we'll train the models to fit the 
# training data

kTrain <- filter(kData, x <= 2015)


# take a look
p <- ggplot(kTrain, aes(x, y))+geom_point() 
p


# now we'll train three models: linear, polynomial and support vector machine


linMod <- lm(y ~ x, data = kTrain)
polyMod <- lm(y ~ x + I(x^2), data = kTrain)
svmModel <- svm(y ~ x, data=kTrain, cost=100, gamma=10)

# and we'll use these models to fit the data 

kTrain$lmPred <- predict(linMod, kTrain)
kTrain$polyPred <- predict(polyMod, kTrain)
kTrain$svmPred <- predict(svmModel, kTrain)

# and plot these predictions out and compare

p <- p + geom_smooth(kTrain, mapping = aes(x=x, y=lmPred))
p
p <- p + geom_smooth(kTrain, mapping = aes(x=x, y=polyPred), color = 'red')
p
p <- p + geom_smooth(kTrain, mapping = aes(x=x, y=svmPred), color = 'green')
p

summary(linMod)

# now calculate the RMSE

lmError <- rmse(kTrain$y -kTrain$lmPred)
lmError
polyError <- rmse(kTrain$y -kTrain$polyPred)
polyError
svmError <- rmse(kTrain$y -kTrain$svmPred)
svmError

# note that the more complex the model, the smaller the error


# now let' use these models to predict througn 2020

p <- ggplot(kData, aes(x, y))+geom_point() 
p


# fit the models to the new data

kData$lmPred <- predict(linMod, kData)
kData$polyPred <- predict(polyMod, kData)
kData$svmPred <- predict(svmModel, kData)

# visualize the new predictions

p <- p + geom_smooth(kData, mapping = aes(x=x, y=lmPred))
p
p <- p + geom_smooth(kData, mapping = aes(x=x, y=polyPred), color = 'red')
p
p <- p + geom_smooth(kData, mapping = aes(x=x, y=svmPred), color = 'green')
p

# and compare errors

lmError <- rmse(kData$y -kData$lmPred)
lmError
polyError <- rmse(kData$y -kData$polyPred)
polyError
svmError <- rmse(kData$y -kData$svmPred)
svmError

# so the point here is: while the more complex models fit the trainig data better,
# if the new data is sufficiently different, the more complex models will not perform as well


#----------------- Thinking ahead - transformations and kernels -----------------#

# refresh  data from the last example

x <- seq(from=0, to=20, by=0.1)
y <- 500 + 0.4 * (x-10)^3
noise <- rnorm(length(x), mean=10, sd=80)
y <- y + noise

kData <- data.frame(x, y)

p <- ggplot(kData, aes(x, y))+geom_point() 
p

# split data between training and test

ntrain <- round(nrow(kData)*0.6) # number of training examples
tindex <- sample(nrow(kData),ntrain) # indices of training samples
xTrain <- kData[tindex,]
xTest <- kData[-tindex,]

# ------------------ dot product kernel 

# run a dotproduct function to build a kernel and model using svm regression

svmDotMod <- ksvm(y~x, data=xTrain, type="nu-svr", kernel="vanilladot")
xTest$SVDot <- predict(svmDotMod, xTest)
p <- p +  geom_smooth(data=xTest, aes(x, SVDot), se=FALSE, color = "black")
p

# take a look at the kernel

dotKernel <- data.frame(kernelMatrix(vanilladot(), as.matrix(xTrain)))

# compare to formula

fDotKernel <- function(){
  k <- function (x,y){crossprod(x,y)}
  class(k) <- "kernel"
  k}
l<-0.1 ; C<-1/(2*l)

dotKernelManual <- data.frame(kernelMatrix(fDotKernel(), as.matrix(xTrain)))

# should be the same, so the model should be the same

svmDotCustom <- ksvm(y~x, data=xTrain, type="nu-svr", kernel=fDotKernel())
xTest$svmDotCustom <- predict(svmDotCustom, xTest)

p <- p +  geom_smooth(data=xTest, aes(x, svmDotCustom), se=FALSE, color = "black")
p


# -------------- try a rbf kernel

svmrbfDot <- ksvm(y~x, data=xTrain, type="nu-svr", kernel="rbfdot")
xTest$rbfDot <- predict(svmrbfDot, xTest)

p <- p +  geom_smooth(data=xTest, aes(x, rbfDot), se=FALSE, color = "red")
p

rbfKernel <- data.frame(kernelMatrix(rbfdot(), as.matrix(xTrain)))


# -------------- try a complete custom kernel - quadratic

k2function <- function(linear =0, quadratic=0)
{
  k2 <- function (x,y)
  {
    linear*sum((x)*(y)) + quadratic*sum((x^2)*(y^2))
  }
  class(k2) <- "kernel"
  k2
}

svmRBFCustom <- ksvm(y~x, data=xTrain, type="nu-svr", kernel=k2function(0,1),scaled=c())
xTest$svmRBFCustom <- predict(svmRBFCustom, xTest)

p <- p +  geom_smooth(data=xTest, aes(x, svmRBFCustom), se=FALSE, color = "blue")
p

quadKernel <- data.frame(kernelMatrix(k2function(0,1), as.matrix(xTrain)))


