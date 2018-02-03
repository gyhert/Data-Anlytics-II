library(tidyverse)  
library(tidyr)
library(ggplot2)
library(e1071)


setwd("C:/Users/ellen/OneDrive/DA2/Regression/Data")
Autos <- read_csv(file="Automobile Price Prediction.csv")
Autos$ind <- as.numeric(rownames(Autos)) # this is to help the stratified sampling loop

tstMk <- unique(Autos$make)
ind <- tibble()
MkPop <- tibble()
tstMkInd <- NULL
tstInd <- numeric()
trnInd <- numeric()

# this is an ultra-simple version of stratfied sampling
# we need to make sure we get ALL of the models into both training and test
# and that the split is reasonably balanced.

for (i in tstMk)
{
    MkPop <- filter(Autos, make == i)
    sz <- nrow(MkPop)/2
    if (sz >1) 
    {
      sz = round(sz,0) 
    tstMkInd <-  sample(1:nrow(MkPop), sz)
    tstInd <- c(tstInd,MkPop$ind[tstMkInd])
    trnInd <- c(trnInd, MkPop$ind[-tstMkInd])
    } else
    {
      print(c(i, "not included as sample size < 1"))
    }
}


Autos <- select(Autos, price, make, 'body-style', 'wheel-base', 'engine-size', 'horsepower')
xTrain <- Autos[trnInd, ]
xTest <- Autos[tstInd,]


# Here, we're writing the stratified sample to files
# so we can compare with AML regressions

write.csv(xTrain, "xTrain.csv")
write.csv(xTest, "xTest.csv")


# just for orientation
p <- ggplot(Autos, aes(x=horsepower, y=price))+geom_point() 
p

# create a RMSE function - just a convenience
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Now use a multivariate regression model with all key variables

model3 <- lm(price ~., xTrain)
xTest$newY2 <- predict(model3, xTest)

# chart the MV model too, and compare statisics

p <- p + geom_point(data=xTest, aes(horsepower, newY2), color = 'green') + 
  geom_smooth(data=xTest, aes(horsepower, newY2), se=FALSE, color = "green")
p

# now create a support vector machine regression and do the same
svmModel <- svm(price ~., data=xTrain, cost=100, gamma=10)
xTest$SVMY <- predict(svmModel, xTest)

p <- p + geom_point(data = xTest, aes(horsepower, SVMY), color = "red") + 
      geom_smooth(data=xTest, aes(horsepower, SVMY), se=FALSE, color = "red")
p <- p + theme(panel.background = element_rect(fill = "white"))
p

p <- p + annotate("text", x = 230, y = 23000, label = "MVR", color="green")
p <- p + annotate("text", x = 230, y = 20000, label = "SVM", color="red")
p


# Now, let's gather metrics on these two models and put them in a tibble
# for easier comparison

# get Dof for future use
df <- df.residual(model3)
YMean <- mean(xTest$price)
SSE <- sum((svmModel$residuals)^2)
SSR <- sum((xTest$SVMY - YMean)^2)
SST <- SSE + SSR

MVR_RMSE <- rmse(model3$residuals)
MVR_MAE <- mean(abs(model3$residuals))
MVR_RAE <- sum(abs(model3$residuals))/sum(abs(mean(xTest$price) - xTest$price))
MVR_RSE <- sum((model3$residuals)^2)/sum((mean(xTest$price) - xTest$price)^2)
MVR_COD <- summary(model3)$r.squared

SVM_RMSE <- rmse(svmModel$residuals)
SVM_MAE <- mean(abs(svmModel$residuals))
SVM_RAE <- sum(abs(svmModel$residuals))/sum(abs(mean(xTest$price) - xTest$price))
SVM_RSE <- sum((svmModel$residuals)^2)/sum((mean(xTest$price) - xTest$price)^2)
SVM_COD <- SSR/SST

modelDiag <- tibble(Algorithm = character(), "Mean Absolute Error" = numeric(), "Root Mean Squared Error" = numeric(), "Relative Absolute Error" = numeric(), "Relative Squared Error"=numeric(), "Coefficient of Determination"= numeric())
modelDiag[1,] <- c("MVR", round(MVR_MAE,2), round(MVR_RMSE,2), round(MVR_RAE, 2), round(MVR_RSE, 2), round(MVR_COD, 2))
modelDiag[2,] <- c("SVM", round(SVM_MAE,2), round(SVM_RMSE,2), round(SVM_RAE, 2), round(SVM_RSE, 2), round(SVM_COD, 2))



