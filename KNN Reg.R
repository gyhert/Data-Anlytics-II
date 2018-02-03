library(tidyverse)
library(FNN)
library(ggplot2)    
library(dplyr)  

setwd("C:/Users/ellen/OneDrive/DA2/Regression/Data")

tst <- read_csv("Idata.csv")
tst <- select(tst, X, Y2)

# using data from the lr regression intro section - recall: 

m2 <- lm(Y2 ~ X, data = tst)
tst$newY <- predict(m2)


p <- ggplot(data = tst, aes(x = X, y = Y2)) +
  geom_point() +
  geom_smooth(aes(x=X, y=newY)) 
p

# now let's try a knn based model

tst.knn.VR<- knn.reg(tst, y=tst$Y2, k=2)
tst$KNN <- tst.knn.VR$pred

p <- p + geom_smooth(data = tst, aes(x=X, y=KNN), se=FALSE, color = "red") 
p

# so that looks pretty good - but don't get too excited - KNN has limits. 
# Let's understand how it works:

tstA <- tst %>% arrange (X)
write.csv(tstA, file = "knnTest.csv")

# refer to slide to calculate euclidean distance and normalized values



