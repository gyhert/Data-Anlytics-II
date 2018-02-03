library(tidyverse)
library(stringr)
library(RODBC)
library(ggplot2)
library(matrixcalc)
library(kernlab)
library(reshape2)
library(ggraph)
library(car)


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
 [TV]
,[Radio]
,[Newspaper]
,[Sales]
FROM [dbo].[Advertising]"
Advertising <- sq(myQuery)

rfit <- lm(Sales ~ Radio, data = Advertising)
nfit <- lm(Sales ~ Newspaper, data = Advertising)

summary(rfit)
summary(nfit)

mFit <- lm(Sales ~ TV + Radio + Newspaper, data = Advertising)
summary(mFit)

# correlation matrix
cor(Advertising, method = 'pearson', use = 'pairwise')


# Manually Calculate SE's

vY <- as.matrix(dplyr::select(Advertising, Sales)) # set up y values in matrix                        
mX <- as.matrix(cbind(1, dplyr::select(Advertising, TV, Radio, Newspaper))) # set up x values in matrix
vBeta <- solve(t(mX)%*%mX, t(mX)%*%vY) # solve using normal equations                    
dSigmaSq <- sum((vY - mX%*%vBeta)^2)/(nrow(mX)-ncol(mX)) # estimate the variance  
mVarCovar <- dSigmaSq*chol2inv(chol(t(mX)%*%mX))          
vStdErr <- sqrt(diag(mVarCovar))                          
print(cbind(vBeta, vStdErr))    
summary(mFit)

# Evaluate Collinearity
vif(mFit) # variance inflation factors 

# ------------------- Nonlinearality ----------------------#

crPlots(mFit)
# Ceres plots 
ceresPlots(mFit)

# ------------------- Non-independence of Errors ----------------------#

# Test for Autocorrelated Errors
durbinWatsonTest(mFit)
