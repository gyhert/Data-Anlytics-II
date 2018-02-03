library(tidyverse)
library(ggplot2)
library(e1071)

#setwd("C:/Users/ellen/OneDrive/DA2/Regression/Data")
#mpg <- read.csv(file="Automobile Mileage2.csv")

# chart the mpg data

p <- ggplot(data = mpg) +   
  geom_point(mapping = aes(x = displ, y = hwy)) +  
  geom_smooth(method = lm, mapping = aes(x = displ, y = hwy), col = "red")+
  theme(
    panel.background = element_rect(fill = "white") # I do this so it's easier to see in  class
  )
p

# add the means to the chart

ym <- mean(mpg$hwy, na.rm = TRUE)
xm <- mean(mpg$displ, na.rm = TRUE) 
p <- p+ geom_hline(yintercept=ym, linetype="dashed", color = "red")
p <- p+ geom_vline(xintercept=xm, linetype="dashed", color = "red")
p  

# create model with lm

dfMPG <- mpg %>% drop_na()
  
dfMPG %>% count(year)

trainMPG <- filter(dfMPG, year == '1999') %>% select(hwy, displ)
testMPG <- filter(dfMPG, year == '2008') %>% select(hwy, displ)
filter(testMPG, displ > 6)
testMPG <- mutate(testMPG, thwy = ifelse(displ >5, hwy * .7, hwy))
testMPG <- testMPG %>% select(thwy, displ)
colnames(testMPG)[1] <- 'hwy'
filter(testMPG, displ > 6)

modelLin <- lm(hwy ~ displ, data = trainMPG)
modelPoly <- lm( formula = hwy ~ displ + I(displ^2), trainMPG)
modelSVM <- svm(hwy ~displ, data=trainMPG, cost=100, gamma=10)

TrainLM <- cbind(flexilibity = 1, cbind(tHwy = predict(modelLin, trainMPG), trainMPG))
TrainPoly <- cbind(flexilibity = 2, cbind(tHwy = predict(modelPoly, trainMPG), trainMPG))
TrainSVM <- cbind(flexilibity = 3, cbind(tHwy = predict(modelSVM, trainMPG), trainMPG))

TestLM <- cbind(flexilibity = 1, cbind(tHwy = predict(modelLin, testMPG), testMPG))
TestPoly <- cbind(flexilibity = 2, cbind(tHwy = predict(modelPoly, testMPG), testMPG))
TestSVM <- cbind(flexilibity = 3, cbind(tHwy = predict(modelSVM, testMPG), testMPG))

TrainLmMSE <- mean((TrainLM$tHwy - TrainLM$hwy)^2, na.rm = TRUE)
TrainPolyMSE <- mean((TrainPoly$tHwy - TrainPoly$hwy)^2, na.rm = TRUE)
TrainSVMMSE <- mean((TrainSVM$tHwy - TrainSVM$hwy)^2, na.rm = TRUE)

TestLmMSE <- mean((TestLM$tHwy - TestLM$hwy)^2, na.rm = TRUE)
TestPolyMSE <- mean((TestPoly$tHwy - TestPoly$hwy)^2, na.rm = TRUE)
TestSVMMSE <- mean((TestSVM$tHwy - TestSVM$hwy)^2, na.rm = TRUE)

p <- ggplot(data = trainMPG, aes(x=displ, y=hwy)) + geom_point() 
p <- p + geom_point(data = testMPG, aes(x=displ, y=hwy), color = 'red')
p <- p + theme(panel.background = element_rect(fill = "white"))
p

p <- ggplot(data = testMPG, aes(x=displ, y=hwy)) + geom_point() 
p <- p + geom_smooth(data=TestLM, aes(displ, tHwy), se=FALSE, color = "black")
p <- p + geom_smooth(data=TestPoly, aes(displ, tHwy), se=FALSE, color = "red")
p <- p + geom_smooth(data=TestSVM, aes(displ, tHwy), se=FALSE, color = "green")
p


