library(tidyverse)
library(stringr)
library(RODBC)
library(ggplot2)
library(matrixcalc)
library(kernlab)
library(reshape2)
library(ggraph)

myServer <- "tcp:analyticslab.database.windows.net,1433"
myUser <- "Student"
myPassword <- "Acct7397"
myDatabase <- "Accounting" 
myDriver <- "ODBC Driver 13 for SQL Server" # Must correspond to an entry in the Drivers tab of "ODBC Data Sources"

connectionString <- str_c(
  "Driver=", myDriver, 
  ";Server=", myServer, 
  ";Database=", myDatabase, 
  ";Uid=", myUser, 
  ";Pwd=", myPassword)

sq <- function (myQuery){
  conn <- odbcDriverConnect(connectionString)
  tQuery <- (sqlQuery(conn, myQuery))
  close(conn)
  return (tQuery)
}

myQuery <- "
SELECT 
[Obs]
,[make]
,[body-style]
,[wheel-base]
,[engine-size]
,[horsepower]
,[peak-rpm]
,[highway-mpg]
,[price]
FROM [dbo].[Automobile Price Prediction]
"
Auto <- sq(myQuery)

# this gets rid of the Obs column

Autos <- Auto[-1]


dim(Autos)
mAutos <- data.frame(data.matrix(Autos[1:7]))

Auto.pca <- prcomp(mAutos, retx=TRUE, center=TRUE, scale=TRUE) 
Auto.pca
plot(Auto.pca)
summary(Auto.pca)
biplot(Auto.pca, cex=c(.3,1))
qg_pca <- qgraph::qgraph.pca(mAutos, factors=2, rotation="varimax")
Auto.pca2 <- data.frame(Auto.pca$x[,1:2])


testSplit <- .4
totalSampleSize <- nrow(Autos)
testSampleSize <- round(totalSampleSize*testSplit)
trainSampleSize <- totalSampleSize - testSampleSize
tindexes <- sample(1:nrow(Autos), testSampleSize)
indexes <- sample(1:nrow(Autos[-tindexes,]), trainSampleSize)
xTrain <- Autos[indexes, ]
xTest <- Autos[tindexes,]
mTrain <- data.frame(data.matrix(xTrain))
mTest <- data.frame(data.matrix(xTest))

model1 <- lm( price ~ ., mTrain)
#xTest1 <- dplyr::select(xTest, price, horsepower)
xTest1 <- dplyr::select(xTest)
mTest$newPrice <- predict(model1, mTest)
mTest <- data.frame(mTest)

# chart the single variable LR and summarize performance / error

p <- ggplot(mTest, aes(horsepower, newPrice)) + geom_point() + geom_smooth(se=FALSE, color = "blue", method = lm)
p  

Autos2 <- cbind(Autos$price, Autos$horsepower, Auto.pca2)
colnames(Autos2)[1]<- 'price'
colnames(Autos2)[2]<- 'horsepower'

xTrain2 <- Autos2[indexes, ]
xTest2 <- Autos2[tindexes,]

xTest2$PC1
model2 <- lm( price ~ PC1 + PC2, xTrain2)
xTest2$newPrice <- predict(model2, xTest2[3:4])


mTest <- data.frame(mTest)

# chart the single variable LR and summarize performance / error


p <- p + geom_point(data=xTest2, aes(horsepower, newPrice), color = "red") + geom_smooth(data=xTest2, aes(horsepower, newPrice), se=FALSE, color = "red", method = lm)
p


