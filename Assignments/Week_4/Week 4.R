# REDA1-CE1000 (WEEK 4):  
# EXTENSIONS
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
library(RCurl)
library(haven)
library(fredr) # Another library to import data from FRED
fredr_set_key('30e6ecb242a73869e11cb35f6aa3afc3') # My key, please don't abuse.



# Identify and set the working directory.
getwd() 
setwd("/Users/timothysavage/Desktop/REDA")



# Consider the following data.
data = read.csv('nyc.csv')
names = c("geo", "date", "rent", "vacancy", "cap_rate", "gross_rent")
colnames(data) = names

attach(data)
plot(date, vacancy, col="darkred", main="NYC Office Vacancy", pch=16, 
     xlab="Date", ylab="Vacancy", xlim=c(1990, 2019), ylim=c(0, 20))
grid(lw=2)
lines(date, vacancy, col="darkred", pch=16)

plot(date, rent, col="darkblue", main="NYC Office Rent Index (2008 $)", pch=16, 
     xlab="Date", ylab="Rents", xlim=c(1980, 2019), ylim=c(20, 80))
grid(lw=2)
lines(date, rent, pch=16)

plot(rent, vacancy, col="blue", main="NYC Office Vacancy v. Rent Index", pch=16, 
     ylab="Vacancy (%)", xlab="Rent Index (2008 $)", ylim=c(0, 20), xlim=c(20, 80))
grid(lw=2)
abline(lm('vacancy ~ rent'), col="red")

model = lm('vacancy ~ rent')
stargazer(model, type="text", title="What Causes What?", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

# Consider a null hypothesis test of no relationship between vacancy and rent.
  # We would reject this null based on a 95% confidence interval. 

# Suppose you were advising a landlord who wanted to reduce vacancy from 12% to 7%. 
  # Based on these results, you would advice the landlord to raise the rent:
  # from ~$40 to ~$65 per sqft.  

# Does this make sense?
  # Does lower vacancy drive up rents?
  # Do low rents drive up vacancy?
  # Is there a common unobervable?
  # Correlation and causation are different



# Expanding CAPM to Fama-French Factor Models
rm(list=ls())
ff5f = read_dta("FF5F.dta") # Data are stored in multiple formats.  


# Notes:
# Data downloaded from Professor Ken Frenh's website.
# Daily Five Factors.
# Data dictionary.
# mktrf: market returns minnus risk free rate.  Augmented Beta.
# smb: small minus big.  Effect unclear.
# hml: high minus low.  Effect unclear.
# rmw: robust minus weak.  Effect unclear.
# cma: conservative minus aggressive.  Effect unclear.
# FF5F data must be scaled to basis points.
ff5f$mktrf = ff5f$mktrf / 100
ff5f$smb = ff5f$smb / 100
ff5f$hml = ff5f$hml / 100
ff5f$rmw = ff5f$rmw / 100
ff5f$cma = ff5f$cma / 100
ff5f$rf = ff5f$rf / 100

summary(ff5f)
str(ff5f)

hist(ff5f$mktrf, breaks=50, col="darkblue", freq=FALSE, 
     main="Histogram of Market Returns", xlab="Daily Returns", 
     xlim=c(-.1, .1))  
abline(v=0, col="red")

hist(ff5f$smb, breaks=50, col="darkblue", freq=FALSE, 
     main="Histogram of Small Minus Big", xlab="Daily Returns", 
     xlim=c(-.1, .1))  
abline(v=0, col="red")

hist(ff5f$hml, breaks=50, col="darkblue", freq=F, 
     main="Histogram of High Minus Low", xlab="Daily Returns", 
     xlim=c(-.1, .1))  
abline(v=0, col="red")

hist(ff5f$rmw, breaks=50, col="darkblue", freq=F, 
     main="Histogram of Robust Minus Weak", xlab="Daily Returns", 
     xlim=c(-.1, .1))  
abline(v=0, col="red")

hist(ff5f$cma, breaks=50, col="darkblue", freq=F, 
     main="Histogram of Conservative Minus Aggressive", xlab="Daily Returns", 
     xlim=c(-.1, .1))  
abline(v=0, col="red")

hist(ff5f$rf, breaks=50, col="darkblue", freq=F, 
     main="Histogram of Risk Free", xlab="Daily Returns", 
     xlim=c(-.1, .1))  
abline(v=0, col="red")

a = xts(x=ff5f, order.by = ff5f$date) # Create an time series dataframe using xts.

# Example 1: AAPL
getSymbols(c('AAPL'), from="2006-01-01", to="2017-12-31") # Read in AAPL data using quantmod.
b = AAPL$AAPL.Adjusted
b = diff(log(b), lag=1)  ## Log returns
b = na.omit(b)

data = merge(a, b, join='right')  ## Merge into single time-series dataset

names = c("date", "mktrf", "smb", "hml", "rmw", "cma", "rf", "aapl") # factors (per Ken French) and AAPL returns
colnames(data) = names

data = data[, colnames(data) != "date"]  # Narrow dataframe 

ff1f.ols = lm('aapl - rf ~ mktrf', data = data)
stargazer(ff1f.ols, type="text", title="Baseline Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

ff3f.ols = lm('aapl - rf ~ mktrf + smb + hml', data = data)
stargazer(ff3f.ols, type="text", title="FF3F Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

ff5f.ols = lm('aapl - rf ~ mktrf + smb + hml + rmw + cma', data = data)
stargazer(ff5f.ols, type="text", title="FF5F Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)


# Example 2: AMZN
getSymbols(c('AMZN'), from="2006-01-01", to="2017-12-31")
b = AMZN$AMZN.Close
b = diff(log(b), lag=1)  ## Log returns
b = na.omit(b)

data = merge(data, b, join='right')  ## Merge into single time-series dataset
names = c("mktrf", "smb", "hml", "rmw", "cma", "rf", "aapl", "amzn") # factors (per Ken French) and AAPL returns
colnames(data) = names

ff1f.ols = lm('amzn - rf ~ mktrf', data = data)
stargazer(ff1f.ols, type="text", title="Baseline Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

ff3f.ols = lm('amzn - rf ~ mktrf + smb + hml', data = data)
stargazer(ff3f.ols, type="text", title="FF3F Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

ff5f.ols = lm('amzn - rf ~ mktrf + smb + hml + rmw + cma', data = data)
stargazer(ff5f.ols, type="text", title="FF5F Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)


# Example 3: FB
getSymbols(c('FB'), from="2006-01-01", to="2017-12-31")
b = FB$FB.Close
b = diff(log(b), lag=1)  ## Log returns
b = na.omit(b)

data = merge(data, b, join='left')  ## Here we do a left merge because FB went public in ~2012.
names = c("mktrf", "smb", "hml", "rmw", "cma", "rf", "aapl", "amzn", "fb") # factors (per Ken French) and AAPL returns
colnames(data) = names

ff1f.ols = lm('fb - rf ~ mktrf', data = data)
stargazer(ff1f.ols, type="text", title="Baseline Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

ff3f.ols = lm('fb - rf ~ mktrf + smb + hml', data = data)
stargazer(ff3f.ols, type="text", title="FF3F Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

ff5f.ols = lm('fb - rf ~ mktrf + smb + hml + rmw + cma', data = data)
stargazer(ff5f.ols, type="text", title="FF5F Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)



# Notes:
# Regression Diagnostics: Evaluating the quality of a statistical model and 
# its predictive power.
# The R^2 metric is one of the most cited regression diagnostics.

# If a linear regression uses a constant (which should be included in practice), 
# the R$^2$ is bounded between 0 and 1. 
# It measures the share of the variation in y explained by the variation 
# in the features used in a model. 
# Given this definition, "bigger is better" is the first place that people go 
# to evaluate the quality of the model, which is unwarranted.

# "However, it can still be challenging to determine what is a good R^2 value, 
# and in general, this will depend on the application. 
# For instance, in certain problems in physics, we may know that the data truly comes 
# from a linear model with a small residual error. 
# In this case, we would expect to see an R^2 value that is extremely close to 1, 
# and a substantially smaller R^2 might indicate serious problems 
# with the experiment in which the data were generated. 
# On the other hand, in typical application in biology, pyschology, 
# marketing and other domains, 
# the linear model is at best an extremely rough approximation to the data, 
# and residual errors due to other unmeasured factors are often very large. 
# In this setting, we would expect only a very small proportion of the variance 
# in the response to be explained by the predictor, 
# and an R^2 value well below 0.1 might be more realistic."
# Trevor Hastie, Robert Tibshirani, et al.

# R^2 in insolation.
rm(list=ls())
set.seed(1066)
e1 = rnorm(1000, mean=0, sd=1)  ## R's command to draw from N(0,1)
e2 = rnorm(1000,mean=0, sd=2)
x = rnorm(1000, mean=0, sd=1)  ## R's command to draw from N(0,1)
y1 = 1 + 2 * x + e1  ## DGP for y1
y2 = 1 + 2 * x + e2  ## DGP for y2

plot(x, y1, col="darkblue", main="Smaller Variance", pch=16, xlim=c(-10,10), ylim=c(-10,10))
grid(lw=2)

plot(x, y2, col="darkblue", main="Larger Variance", pch=16, xlim=c(-10,10), ylim=c(-10,10))
grid(lw=2)

cor(y1, y2)
plot(y1, y2, col="darkblue", main="Correlation in Y's Is 0.66", pch=16, xlim=c(-10,10), ylim=c(-10,10))
grid(lw=2)

lm.y1 = lm(y1 ~ x)
stargazer(lm.y1, type="text", title="Y1 Data Generating Process", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

lm.y2 = lm(y2 ~ x)
stargazer(lm.y2, type="text", title="Y2 Data Generating Process", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)


# Notes:
# These two DGPs are exactly the same
# There is more random noise in Y2 than Y1.
# Note the direct impact on the R^2 regression diagnostic.
# In isolation, the R^2 metric cannot be used in the following way: 
# "I have a big R^2."
# Here's another example of the same point.

gdp = fredr('GDP', observation_start = as.Date("1960-01-01"))
attach(gdp) # Attach and plot.
plot(date, value, pch=16, col="darkblue", main="GDP (2009 $)", 
     xlab="Date", ylab="GDP")  ## time series plot in R
lines(date, value, col="darkblue")
grid(lw=2)

gdp$lag.gdp = lag(gdp$value)

names = c("date", "id", "gdp", "lag_gdp")
colnames(gdp) = names
perfectmodel = lm('gdp ~ lag_gdp', data = gdp)
stargazer(perfectmodel, type="text", title="Perfect Model", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

# Notes:
# I've created a "perfect model" with an R^2 of 1.  
# Last year perfectly explains this year.
# I can go home now.
# But think about the underlying process of output per person.



# Notes:
# Let's return to a proper application of using the R^2 metric.
# Nested models: CAPM and Fama French.
rm(list=ls())
ff5f = read_dta("FF5F.dta") # Data are stored in multiple formats.  
ff5f$mktrf = ff5f$mktrf / 100
ff5f$smb = ff5f$smb / 100
ff5f$hml = ff5f$hml / 100
ff5f$rmw = ff5f$rmw / 100
ff5f$cma = ff5f$cma / 100
ff5f$rf = ff5f$rf / 100
a = xts(x=ff5f, order.by = ff5f$date) # Create an time series dataframe using xts.

getSymbols(c('AAPL'), from="2006-01-01", to="2017-12-31") # Read in AAPL data using quantmod.
b = AAPL$AAPL.Adjusted
b = diff(log(b), lag=1)  ## Log returns
b = na.omit(b)

data = merge(a, b, join='right')  ## Merge into single time-series dataset

names = c("date", "mktrf", "smb", "hml", "rmw", "cma", "rf", "aapl") # factors (per Ken French) and AAPL returns
colnames(data) = names

data = data[, colnames(data) != "date"]  # Narrow dataframe 

ff1f.ols = lm('aapl - rf ~ mktrf', data = data)
stargazer(ff1f.ols, type="text", title="Baseline Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

ff3f.ols = lm('aapl - rf ~ mktrf + smb + hml', data = data)
stargazer(ff3f.ols, type="text", title="FF3F Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

ff5f.ols = lm('aapl - rf ~ mktrf + smb + hml + rmw + cma', data = data)
stargazer(ff5f.ols, type="text", title="FF5F Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)



# ASSIGNMENT 4

# 1. Re-run all code above to ensure it works.


# 2. Consider the rent-vacancy example from above.  
  # Given the materials presented to date, write a thoughtful paragraph about whether
  # we could design an experiment to examine whether rents drive vacancy.


# 3. Increase the variance in the high variance DGP from above.  
  # If you replicate the example, what happens to the R^2?
  # Consider the current COVID-19 crisis.  
    # Given the materials presented to date, write a thoughtful paragraph about
    # current market volatility.