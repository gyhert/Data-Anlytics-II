library(tidyverse)
dfMPG <- mpg 

setwd("C:/Users/ellen/OneDrive/Documents/Final Content/Projects/AutomobileData")
Auto <- read.csv(file="Automobile Price Prediction.csv")

dfMPG <- Auto


# chart the mpg data

p <- ggplot(data = mpg) +   
  geom_point(mapping = aes(x = displ, y = hwy)) +  
  geom_smooth(method = lm, mapping = aes(x = displ, y = hwy), col = "red")+
  theme(
    panel.background = element_rect(fill = "white") # I do this so it's easier to see in  class
  )
p

# add the means to the chart

ym <- mean(mpg$hwy)
xm <- mean(mpg$displ) 
p <- p+ geom_hline(yintercept=ym, linetype="dashed", color = "red")
p <- p+ geom_vline(xintercept=xm, linetype="dashed", color = "red")
p  

# this illustrates how sample size and CI relate
# adjust sample size and resampling to show effect on CI on slope

masterMod <- lm(data = mpg, hwy~displ) # get the full population
summary(masterMod) # display std errors
SEI <- summary(masterMod)$coefficients[1,2] # get the SE for the Intercept
MLI <- masterMod$coefficients[1] - (2*SEI) # set a point for lower CI
MUI <- masterMod$coefficients[1] + (2*SEI) # set a point for upper CI

SEI
MLI
MUI


n <- 130
# number of resampling iterations
resample <- 100

library(tidyverse)
indexes = sample(1:nrow(mpg), n)
mpg2 <- mpg[indexes,]

p <- ggplot(data=mpg2, aes(x=displ, y=hwy)) + 
  geom_smooth(method=lm, formula = y ~ x, color = "gray", se=FALSE) +
  theme(
    panel.background = element_rect(fill = "white")
  )+
  scale_y_continuous(limits = c(0, 50))
p

Modcoef <- matrix(NA, nrow = resample, ncol = 3)
icnt <- 1
while (icnt <= resample)
{
  indexes = sample(1:nrow(mpg), n)
  mpg2 <- mpg[indexes,]
  mod <- lm(data = mpg2, hwy~displ)
  Modcoef[icnt, 1] <- mod$coefficients[1]
  Modcoef[icnt, 2] <- mod$coefficients[2]
  Modcoef[icnt, 3] <- summary(mod)$coefficients[1,2]
  p <- p + 
        geom_abline(intercept = Modcoef[icnt, 1], slope = Modcoef[icnt, 2], color = "gray")  
      icnt <- icnt+1;
}
p

ym <- mean(mpg$hwy)
xm <- mean(mpg$displ) 
p <- p+ geom_hline(yintercept=ym, linetype="dashed")
p <- p+ geom_vline(xintercept=xm, linetype="dashed")
p  

dfModcoef <- data.frame (Modcoef)

CITest <- nrow(dfModcoef %>% filter(X1 < MUI & X1 > MLI))
CITest/resample


# Multiple Regression Considerations - Do this in MR

MpgMod <- lm(data = mpg, hwy~displ+cyl)


# Manually Calculate SE's

vY <- as.matrix(dplyr::select(mpg, hwy, cyl)) # set up y values in matrix                        
mX <- as.matrix(cbind(1, dplyr::select(mpg, displ, cyl))) # set up x values in matrix
vBeta <- solve(t(mX)%*%mX, t(mX)%*%vY) # solve using normal equations                    
dSigmaSq <- sum((vY - mX%*%vBeta)^2)/(nrow(mX)-ncol(mX)) # estimate the variance  
mVarCovar <- dSigmaSq*chol2inv(chol(t(mX)%*%mX))          
vStdErr <- sqrt(diag(mVarCovar))                          
print(cbind(vBeta, vStdErr))    
summary(MpgMod)

