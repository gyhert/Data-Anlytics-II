library(dplyr)  
library(tidyr)
library(ggplot2)
library(e1071)

setwd("C:/Users/ellen/OneDrive/Documents/Final Content/Projects/AutomobileData")
Auto <- read.csv(file="Automobile Price Prediction.csv")

Autos <- Auto

# set up training and test splits

testSplit <- .4
totalSampleSize <- nrow(Autos)
testSampleSize <- round(totalSampleSize*testSplit)
trainSampleSize <- totalSampleSize - testSampleSize
tindexes <- sample(1:nrow(Autos), testSampleSize)
indexes <- sample(1:nrow(Autos[-tindexes,]), trainSampleSize)
xTrain <- Autos[indexes, ]
xTest <- Autos[tindexes,]

# chart out the data in terms of horsepower

p <- ggplot(Autos, aes(x=horsepower, y=price))+geom_point() 
p

# run a single variable linear regression

model1 <- lm( price ~ horsepower, xTrain)
xTest1 <-  dplyr::select(xTest, price, horsepower)
xTest1$newPrice <- predict(model1, xTest1)

# chart the single variable LR and summarize performance / error

p <- p +geom_point() + geom_point(data=xTest1, aes(horsepower, newPrice), color = "blue")  + geom_smooth(data=xTest1, aes(horsepower, newPrice), se=FALSE, color = "blue")
p

summary(model1)
rmse <- function(error)
{
  sqrt(mean(error^2))
}
error <- model1$residuals
modRMSE <- rmse(error)
modRMSE

# Now use a multivariate regression model

model3 <- lm( price ~ horsepower + engine.size, xTrain)
x3Test <- select(xTest, price, horsepower, engine.size)
x3Test$newY2 <- predict(model3, x3Test)

# chart the MV model too, and compare statisics

p <- p + geom_point(data=x3Test, aes(horsepower, newY2), color = "green") + geom_smooth(data=x3Test, aes(horsepower, newY2), se=FALSE, color = "green")
p

summary(model3)
error <- model3$residuals
modRMSE <- rmse(error)
modRMSE

# now create a support vector machine regression and do the same

svmModel <- svm(price ~., data=xTrain, cost=100, gamma=10)
pred <- predict(svmModel, xTest)
dfPred <- data.frame(pred)
BaseTest <- Autos[tindexes, ]
dfPred <- cbind(dfPred, BaseTest)

p <- p + geom_point(data=dfPred, aes(horsepower, pred), color = "red") + geom_smooth(data=dfPred, aes(horsepower, pred), se=FALSE, color = "red")
p <- p + theme(panel.background = element_rect(fill = "white"))
p

p <- p + annotate("text", x = 230, y = 30000, label = "SVR", color="blue")
p <- p + annotate("text", x = 230, y = 23000, label = "MVR", color="green")
p <- p + annotate("text", x = 230, y = 20000, label = "SVM", color="red")
p

error <- svmModel$residuals
predictionRMSE <- rmse(error)
predictionRMSE
summary(svmModel)
svmModel$coefs


# write the SVM to a file for further analysis

write.csv(dfPred, file ="dfPred.csv")



# ----- testing and extending ---------------#

dim(Autos)
mAutos <- data.frame(data.matrix(Autos[1:7]))

Auto.pca <- prcomp(mAutos, retx=TRUE, center=TRUE, scale=TRUE) 
Auto.pca
plot(Auto.pca)
summary(Auto.pca)
biplot(Auto.pca, cex=c(.3,1))

pairs(Autos)


qg_pca <- qgraph::qgraph.pca(mAutos, factors=2, rotation="varimax")

Auto.pca2 <- data.frame(Auto.pca$x[,1:2])

mTrain <- data.frame(data.matrix(xTrain))
mTest <- data.frame(data.matrix(xTest))

model1 <- lm( price ~ ., mTrain)
xTest1 <- select(xTest, price, horsepower)
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


