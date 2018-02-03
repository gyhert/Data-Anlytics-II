library(tidyverse)
library(fitdistrplus)
library(stats4)
library(extremevalues)
library(knitr)
library(moments)

# NOTE: this has to be run on your personal machine 
# there are issues installing package(extremevalues) on Unix - no date on resolution


# -------------------  Probability Application: using Distributions to find unusual transactions  ---------- #

# if on PC, just get the data from the server

setwd("C:/Users/ellen/Documents/Spring 2018/DA2/Review of Probability")

# get billing data from ERP excluding nonbillable transactions
Billing <- dbGetQuery(con,'
SELECT *  FROM [Accounting].[ERP].[Billing] Where [Accounting].[ERP].[Billing].[BilledAmt] > 0                     
')

# Or, if on a Mac, read the Billing file
#Billing <- read_csv("Billing.csv")
#Billing$BilledAmt <- as.numeric(Billing$BilledAmt)


# review data

Billing %>% mutate( ints = cut(BilledAmt, breaks = 10)) %>% 
  group_by(ints) %>% 
  summarise( cnt = n()) %>% 
  mutate(pct=(100*cnt)/sum(cnt)) %>%
  arrange(desc(pct))

# visual review
p <- ggplot(data = Billing, aes(BilledAmt)) + geom_histogram()
p

# OK this looks a lot like either a lognormal or a weibull. 
# We're going to go with a Weibull
# break prior and current years (just to mimic an audit scenario)

Billing.prior <- filter(Billing, Date <= '2012-12-31')
Billing.current <- filter(Billing, Date > '2012-12-31')
# ALWAYS check yourself!!
checkSum <- nrow(Billing.prior) + nrow(Billing.current)

# this will seem weird - I'm just modifying the BilledAmt in the second year to test for adjustments to changes in data  
Billing.current <- Billing.current %>% mutate(BilledAmt = if_else(BilledAmt < 1000, BilledAmt*2, BilledAmt)) 
                                          
# now we'll fit the prior distribution to a weibull using maximum likelihood
fit.prior <- fitdist(Billing.prior$BilledAmt, distr = "weibull", method = "mle")
# get estimates
summary(fit.prior)
# get the shape parameters
shape.prior <- fit.prior$estimate[1]
scale.prior <- fit.prior$estimate[2]
# and now create theoretical weibull data based on those shape parameters
wei.prior<-data.frame(x = rweibull(length(Billing.prior$BilledAmt),shape=shape.prior, scale=scale.prior)) 
# now we plot out the data
p <- ggplot(data = Billing.prior, aes(BilledAmt)) + geom_histogram(alpha = .2, binwidth = 200,  fill = 'blue')
p <- p + theme(axis.text.y=element_blank(),axis.ticks=element_blank(),
               panel.background = element_rect(fill = "white"), 
               axis.title.y=element_blank())
p
# and compare to the theoretical distribution
p <-  p + geom_freqpoly(data = wei.prior, aes(x), color = 'blue')
p

# OK, so now we do the same for the current data
fit.current <- fitdist(Billing.current$BilledAmt, distr = "weibull", method = "mle")
# get estimates - how does this differ from prior?
summary(fit.current)
shape.current <- fit.current$estimate[1]
scale.current <- fit.current$estimate[2]
# and now create theoretical weibull data based on those shape parameters
wei.current <- data.frame(x = rweibull(nrow(Billing.current),shape=shape.current, scale=scale.current))
# now let's look at the current data comparatively 
p <- p + geom_histogram(data = Billing.current, aes(BilledAmt),alpha = .2, binwidth = 200,  fill = 'orange')
p
# and compare to the theoretical distribution
p <-  p + geom_freqpoly(data = wei.current, aes(x), color = 'red')
p


# manually adjust shape paraneters 
# NOTE: you wouldn't do this in anlaysis 
# I just to build some intuition about how shape paraemters impact distributions
# this will be a foundational concept with Bayesian modeling later

wei.update <- data.frame(x = rweibull(nrow(Billing.current),shape=(shape.current+(shape.current*-0.1)), scale=(scale.current+(scale.current+(scale.current*-0.1)))))
p <-  p + geom_freqpoly(data = wei.update, aes(x), color = 'green')
p

# OK, now that we have theoretical distributions, 
# we can identify expected values and outliers (which would be transacations we might want to inspect)

# left outliers quantile
left_thresh <- 0.05 
# right outliers quantile
right_thresh <- 0.95

# determine outliers

BillingOutliers <- extremevalues::getOutliersI(as.vector(Billing.current$BilledAmt), 
                                               FLim = c(left_thresh, right_thresh), 
                                               distribution = "weibull")
# plot outliners
extremevalues::outlierPlot(Billing.current$BilledAmt, BillingOutliers, mode="qq")
# pull outliner transactions from Billing
testTrans <- Billing.current[BillingOutliers$iRight,]

# take a look at the outliners compared to the theoretical distribution
p <- ggplot(data = testTrans, aes(BilledAmt)) + geom_histogram(binwidth = 200,  fill = 'red')
p <-  p + geom_freqpoly(data = wei.current, aes(x))
p <- p + theme(axis.text.y=element_blank(),axis.ticks=element_blank(),
               panel.background = element_rect(fill = "white"), 
               axis.title.y=element_blank())
p

