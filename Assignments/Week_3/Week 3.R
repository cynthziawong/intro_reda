# REDA1-CE1000 (WEEK 3):  
# DEPLOYING AN ALGORITHM TO EXPLORE A CONJECTURE
# See Jupyter notebook.
install.packages("tidyverse")

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
setwd("/Users/cynthiaw/Desktop/REDA")


# ASSIGNMENT 3

# 1. Re-run all code above to ensure it works.
# Everything in code worked

# 2. Read in Apple (AAPL) stock price data, as well the NASDAQ between 2010 and the present.
  # Fit the CAPM using bivariate linear regression.
  # Examine the null hypothesis assuming the Efficient Markets Hypothesis holds true. 

  #Response:
  #In the null hypothesis model, we assume that alpha is 0 and beta is 1. 
  #According to the CAPM results, we would fail to reject the null hypothesis for both the alpha and beta. 
  #Meaning that the 95% confidence interval includes the following alpha = 0 and beta = 1.

# CAPM
getSymbols(c('AAPL','^IXIC'), from="2010-01-01", to="2020-07-05")
AAPL = AAPL$AAPL.Adjusted
NASDAQ = IXIC$IXIC.Adjusted
plot(AAPL, main="AAPL ($/Share)", col = "darkblue")
plot(NASDAQ, main="NASDAQ Index", col = "darkblue")
data = merge(as.zoo(AAPL), as.zoo(NASDAQ))  ## Merge into single time-series dataset
names = c("AAPL", "NASDAQ")
colnames(data) = names

data.level = as.xts(data)  ## Levels (Data is leveled)
data.returns = diff(log(data.level), lag=1)  ## Log difference returns (Change from one day to the next) or % returns
data.returns = na.omit(data.returns)  ## Dump missing values. First days are removed due to % change. First row values will be null.

#plot histogram
summary(data.returns$AAPL)
hist(data.returns$AAPL, breaks=100, col="darkblue", freq=F, 
     main="Histogram of AAPL Daily Returns (2010-2020)", xlab="Daily Returns", 
     xlim=c(-.2, .2))  
abline(v=0, col="red")

#plot histogram
summary(data.returns$NASDAQ)
hist(data.returns$NASDAQ, breaks=100, col="darkblue", freq=F, 
     main="Histogram of NASDAQ Daily Returns (2010-2020)", xlab="Daily Returns",
     xlim=c(-.2, .2))  
abline(v=0, col="red")

#CAPM Bivriate
plot.ts(data.returns$NASDAQ, data.returns$AAPL, pch=16, 
        col="darkblue", main="CAPM Data", xlab="Returns of NASDAQ", 
        xlim=c(-.1,.1), ylab="Returns of AAPL", ylim=c(-.1,.1))  ## time series plot in R  
grid(lw = 2)
abline(lm(data.returns$AAPL ~ data.returns$NASDAQ), col="red")  ## I added the best fit line so the graph looks similar to that presented in class for Apple.library(ggplot2)

capm.ols = lm(data.returns$AAPL ~ data.returns$NASDAQ)
stargazer(capm.ols, type="text", title="CAPM Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)



# 3. Read in the interest rate data discussed above between 2000 and the present.  
  # Graph the data as a scatter plot.
  # Examine the Bernanke conjecture that there is a linear relationship between short- and long-term interest rates.
  # Do you reject a null hypothesis of no relationship?

  #Response:
  #For every 100 basis point (1%) increase in 3 month treasury (short term rates), we see a 53.9 basis point increase 
  #in the 10 yr us treasury (long term yield). This shows a positive relationship although not 1:1. If we take a look at 
  #this report and compare it with the chart that examines the relationship between short term and long term interest rates 
  #after the financial crisis, we see a difference in the value of 0.539 and 0.164. 
  #Thus validating Bernanke’s claim that the relationship between short and long term interest rates have fallen.

threemonth = drop_na(fredr(series_id = "DGS3MO", observation_start = as.Date("2000-01-01")))
tenyear = drop_na(fredr(series_id = "DGS10", observation_start = as.Date("2000-01-01")))

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
     main="Daily Interest Rates Since 2000", pch=16, col='darkblue')
grid(lw = 2)
abline(lm(tenyear$value ~ threemonth$value), col='red')

rates.ols = lm(tenyear$value ~ threemonth$value) # fit the model 10 yr to 3 month
stargazer(rates.ols, type="text", title="Interest Rate Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)