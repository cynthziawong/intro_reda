# REDA1-CE1000 (WEEK 3):  
# DEPLOYING AN ALGORITHM TO EXPLORE A CONJECTURE
# See Jupyter notebook.

# Load the libraries.
library(quantmod)
library(stargazer)
library(fredr)
library(tidyr)
library(PASWR2)
library(MASS)
library(repmis)
library(latex2exp)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(knitr)
library(fredr) # Another library to import data from FRED
fredr_set_key('30e6ecb242a73869e11cb35f6aa3afc3') # My key, please don't abuse.



# Identify and set the working directory.
getwd() 
setwd("/Users/timothysavage/Desktop/REDA")



# CAPM as an introduction to bivariate regression.
# Start with AAPL which trades on NASDAQ.
getSymbols(c('AAPL','^IXIC'), from="2006-01-01", to="2017-12-31")
AAPL = AAPL$AAPL.Adjusted
NASDAQ = IXIC$IXIC.Adjusted
plot(AAPL, main="AAPL ($/Share)", col = "darkblue")
plot(NASDAQ, main="NASDAQ Index", col = "darkblue")
data = merge(as.zoo(AAPL), as.zoo(NASDAQ))  ## Merge into single time-series dataset
names = c("AAPL", "NASDAQ")
colnames(data) = names

data.level = as.xts(data)  ## Levels 
data.returns = diff(log(data.level), lag=1)  ## Log returns
data.returns = na.omit(data.returns)  ## Dump missing values

summary(data.returns$AAPL)
hist(data.returns$AAPL, breaks=100, col="darkblue", freq=F, 
     main="Histogram of AAPL Daily Returns (2006-2017)", xlab="Daily Returns", 
     xlim=c(-.2, .2))  
abline(v=0, col="red")
# Histogram of returns.  Do they look normally distributed?  Lots of work in finance depends on normally distributed returns.

summary(data.returns$NASDAQ)
hist(data.returns$NASDAQ, breaks=100, col="darkblue", freq=F, 
     main="Histogram of NASDAQ Daily Returns (2006-2017)", xlab="Daily Returns",
     xlim=c(-.2, .2))  
abline(v=0, col="red")

plot.ts(data.returns$NASDAQ, data.returns$AAPL, pch=16, 
        col="darkblue", main="CAPM Data", xlab="Returns of NASDAQ", 
        xlim=c(-.1,.1), ylab="Returns of AAPL", ylim=c(-.1,.1))  ## time series plot in R  
grid(lw = 2)
abline(lm(data.returns$AAPL ~ data.returns$NASDAQ), col="red")  ## I added the best fit line so the graph looks similar to that presented in class for Apple.library(ggplot2)

capm.ols = lm(data.returns$AAPL ~ data.returns$NASDAQ)
stargazer(capm.ols, type="text", title="CAPM Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)



# Let's replicate with Amazon.
getSymbols(c('AMZN','^IXIC'), from="2006-01-01", to="2017-12-31")
AMZN = AMZN$AMZN.Adjusted
NASDAQ = IXIC$IXIC.Adjusted
plot(AMZN, main="AMZN ($/Share)", col = "darkblue")
plot(NASDAQ, main="NASDAQ Index", col = "darkblue")
data = merge(as.zoo(AMZN), as.zoo(NASDAQ))  ## Merge into single time-series dataset
names = c("AMZN", "NASDAQ")
colnames(data) = names

data.level = as.xts(data)  ## Levels 
data.returns = diff(log(data.level), lag=1)  ## Log returns
data.returns = na.omit(data.returns)  ## Dump missing values

summary(data.returns$AMZN)
hist(data.returns$AMZN, breaks=100, col="darkblue", freq=F, 
     main="Histogram of AMZN Daily Returns (2006-2017)", xlab="Daily Returns", 
     xlim=c(-.2, .2))  
abline(v=0, col="red")
# Histogram of returns.  Do they look normally distributed?  Lots of work in finance depends on normally distributed returns.

summary(data.returns$NASDAQ)
hist(data.returns$NASDAQ, breaks=100, col="darkblue", freq=F, 
     main="Histogram of NASDAQ Daily Returns (2006-2017)", xlab="Daily Returns",
     xlim=c(-.2, .2))  
abline(v=0, col="red")

plot.ts(data.returns$NASDAQ, data.returns$AMZN, pch=16, 
        col="darkblue", main="CAPM Data", xlab="Returns of NASDAQ", 
        xlim=c(-.1,.1), ylab="Returns of AMZN", ylim=c(-.1,.1))  ## time series plot in R  
grid(lw = 2)
abline(lm(data.returns$AMZN ~ data.returns$NASDAQ), col="red")  ## I added the best fit line so the graph looks similar to that presented in class for Apple.library(ggplot2)

capm.ols = lm(data.returns$AMZN ~ data.returns$NASDAQ)
stargazer(capm.ols, type="text", title="CAPM Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)



# Let's explore the Bernanke Conjecture.
threemonth = drop_na(fredr(series_id = "DGS3MO", observation_start = as.Date("2008-01-01")))
tenyear = drop_na(fredr(series_id = "DGS10", observation_start = as.Date("2008-01-01")))

plot(threemonth$date, threemonth$value, col = 'darkblue', pch=16, 
     ylab = "Rate", xlab = "Date", 
     main="3 Month US Treasurys")
lines(threemonth$date, threemonth$value, col = 'darkblue')
grid(lw=2)

plot(tenyear$date, tenyear$value, col = 'darkblue', pch=16, 
     ylab = "Rate", ylim=c(0, 5), xlab = "Date", 
     main="US Treasurys")
lines(tenyear$date, tenyear$value, col = 'darkblue')
grid(lw=2)
lines(threemonth$date, threemonth$value, col = 'red', pch=16)

plot(threemonth$value, tenyear$value, 
     xlab=TeX("3 Month Yields"), ylab=TeX("10 Year Yields"), 
     main="Daily Interest Rates Since 2008", pch=16, col='darkblue')
grid(lw = 2)
abline(lm(tenyear$value ~ threemonth$value), col='red')

rates.ols = lm(tenyear$value ~ threemonth$value)
stargazer(rates.ols, type="text", title="Interest Rate Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)



# ASSIGNMENT 3

# 1. Re-run all code above to ensure it works.


# 2. Read in Apple (AAPL) stock price data, as well the NASDAQ between 2010 and the present.
  # Fit the CAPM using bivariate linear regression.
  # Examine the null hypothesis assuming the Efficient Markets Hypothesis holds true. 


# 3. Read in the interest rate data discussed above between 2000 and the present.  
  # Graph the data as a scatterplot.
  # Examine the Bernanke conjecture that there is a linear relationship between short- and
    # long-term interest rates.
  # Do you reject a null hypothesis of no relationship?