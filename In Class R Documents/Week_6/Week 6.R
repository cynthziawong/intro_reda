# REDA1-CE1000 (WEEK 6):  
# Characteristics of Time Series Data
# Common Economic Time Series: Time Series as Story Telling
# Seasonanility and Detrending

install.packages("tidyr")
install.packages("mFilter")

# Load the libraries
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
library(RCurl)
library(haven)
fredr_set_key('30e6ecb242a73869e11cb35f6aa3afc3') # My key, please don't abuse.

getwd() 
setwd("/Users/cynthiaw/Desktop/REDA/Course/Week_6")



# Common Economic Time Series Data: Time Series as Story Telling
tenyear = drop_na(fredr("DGS10", observation_start = as.Date("1960-01-01")))

plot(tenyear$date, tenyear$value, pch=16, col='blue',
     xlab="Date", ylab="%", 
     main="Long Duration Rates in the Modern Era")
grid(lw=2)
lines(tenyear$date, tenyear$value, col='blue', lw=2)
abline(v=as.Date("1970-01-30"), col="red", lw=2)
abline(v=as.Date("1979-08-06"), col="red", lw=2)
abline(v=as.Date("1987-08-11"), col="red", lw=2)
abline(v=as.Date("2006-02-01"), col="red", lw=2)
abline(v=as.Date("2014-02-03"), col="red", lw=2)
abline(v=as.Date("2018-02-05"), col="red", lw=2)



# Time Series as Story-Telling: The Usual Technique in CRE
threemonth = drop_na(fredr("DGS3MO", observation_start = as.Date("1990-01-01")))
tenyear = drop_na(fredr("DGS10", observation_start = as.Date("1990-01-01")))
fedfunds = drop_na(fredr(series_id = "FEDFUNDS", observation_start = as.Date("1990-01-01")))

plot(tenyear$date, tenyear$value, pch=16, col='blue',
     xlab="Date", ylab="%", ylim=c(0, 10), 
     main="Long- and Short-Rates Have Largely Decoupled")
grid(lw=2)
lines(tenyear$date, tenyear$value, col='blue', lw=2)
lines(threemonth$date, threemonth$value, col='green', lw=2)
lines(fedfunds$date, fedfunds$value, col='red', lw=2)


yieldcurve = drop_na(fredr("T10Y3M", observation_start = as.Date("2017-01-01")))
plot(yieldcurve$date, yieldcurve$value, pch=16, col='blue',
     xlab="Date", ylab="%",  
     main="Yield Curve Has Been Flat or Negative for 18 Months")
grid(lw=2)
lines(yieldcurve$date, yieldcurve$value, col='blue')
abline(h=0, col='black', lw=2)


breakeven = drop_na(fredr("T10YIE", observation_start = as.Date("1990-01-01")))
plot(breakeven$date, breakeven$value, pch=16, col='blue',
     xlab="Date", ylab="%",
     main="Long-Term Inflation Expectations Have Collapsed Again")
grid(lw=2)
lines(breakeven$date, breakeven$value, col='blue')
abline(v=as.Date("2008-09-30"), col="red", lw=2)
abline(v=as.Date("2020-03-01"), col="red", lw=2)
abline(h=2, col="black", lw=2)


growth = drop_na(fredr("A191RO1Q156NBEA", observation_start = as.Date("2000-01-01")))
plot(growth$date, growth$value, pch=16, col='blue',
     xlab="Date", ylab="%",
     main="GDP Growth")
grid(lw=2)
lines(growth$date, growth$value, col='blue')
abline(v=as.Date("2008-09-15"), col="red", lw=2)
abline(v=as.Date("2020-03-15"), col="red", lw=2)
abline(h=0, col="black", lw=2)


fedfunds = drop_na(fredr(series_id = "FEDFUNDS", observation_start = as.Date("2016-01-01")))
plot(fedfunds$date, fedfunds$value, pch=16, col='blue',
     xlab="Date", ylab="%",
     main="Federal Reserve Tries the Old Playbook")
grid(lw=2)
lines(fedfunds$date, fedfunds$value, col='blue')
abline(v=as.Date("2020-03-15"), col="red", lw=2)


wilshire = drop_na(fredr(series_id = "WILL5000PR", observation_start = as.Date("2016-01-01")))
plot(wilshire$date, wilshire$value, pch=16, col='blue',
     xlab="Date", ylab="Index Value",
     main="Equity Markets Have Nearly Recovered")
grid(lw=2)
lines(wilshire$date, wilshire$value, col='blue')
abline(v=as.Date("2020-03-15"), col="red", lw=2)


vix = drop_na(fredr(series_id = "VIXCLS", observation_start = as.Date("2016-01-01")))
plot(vix$date, vix$value, pch=16, col='blue',
     xlab="Date", ylab="VIX Index Value",
     main="Volatility Has Receded")
grid(lw=2)
lines(vix$date, vix$value, col='blue')
abline(v=as.Date("2020-03-15"), col="red", lw=2)


sales = drop_na(fredr(series_id = "RSAFS", observation_start = as.Date("2006-01-01")))
plot(sales$date, sales$value, pch=16, col='blue',
     xlab="Date", ylab="Millions ($)",
     main="Monthly Sales of Retail and Food")
grid(lw=2)
lines(sales$date, sales$value, col='blue')
abline(v=as.Date("2008-09-15"), col="red", lw=2)
abline(v=as.Date("2020-03-15"), col="red", lw=2)


growth = drop_na(fredr(series_id = "PRS85006012", observation_start = as.Date("1980-01-01")))
plot(growth$date, growth$value, pch=16, col='blue',
     xlab="Date", ylab="%",
     main="Employment Growth")
grid(lw=2)
lines(growth$date, growth$value, col='blue')
abline(v=as.Date("2008-09-15"), col="red", lw=2)
abline(v=as.Date("2020-03-15"), col="red", lw=2)
abline(h=0, col='black', lw=2)


claims = drop_na(fredr(series_id = "ICSA", observation_start = as.Date("2020-01-01")))
plot(claims$date, claims$value, pch=16, col='blue',
     xlab="Date", ylab="Weekly Claims",
     main="UI Claims Reflect the Natural Disaster")
grid(lw=2)
lines(claims$date, claims$value, col='blue')
abline(v=as.Date("2020-03-15"), col="red", lw=2)


rate = drop_na(fredr(series_id = "UNRATE", observation_start = as.Date("1980-01-01")))
plot(rate$date, rate$value, pch=16, col='blue',
     xlab="Date", ylab="%", ylim=c(3, 15),
     main="Unemployment Rate Reflect the Natural Disaster")
grid(lw=2)
lines(rate$date, rate$value, col='blue')
abline(v=as.Date("2008-09-15"), col="red", lw=2)
abline(v=as.Date("2020-03-15"), col="red", lw=2)


cash = drop_na(fredr(series_id = "MMMFFAQ027S", observation_start = as.Date("1990-01-01")))
plot(cash$date, cash$value, pch=16, col='blue',
     xlab="Date", ylab="Millions ($)", 
     main="Investors Remember Frank Knight")
grid(lw=2)
lines(cash$date, cash$value, col='blue')
abline(v=as.Date("2008-09-15"), col="red", lw=2)
abline(v=as.Date("2020-03-15"), col="red", lw=2)


balance = drop_na(fredr("WALCL", observation_start = as.Date("1990-01-01")))
plot(balance$date, balance$value, pch=16, col='blue',
     xlab="Date", ylab="Million ($)",
     main="Federal Reserve Remembers Bagehot")
grid(lw=2)
lines(balance$date, balance$value, col='blue')
abline(v=as.Date("2008-09-15"), col="red", lw=2)
abline(v=as.Date("2020-03-15"), col="red", lw=2)


share = drop_na(fredr("FYFSGDA188S", observation_start = as.Date("1950-01-01")))
plot(share$date, share$value, pch=16, col='blue',
     xlab="Date", ylab="%",
     main="Federal Government Remembers Keynes")
grid(lw=2)
lines(share$date, share$value, col='blue')
abline(v=as.Date("2008-09-15"), col="red", lw=2)
abline(v=as.Date("2020-03-15"), col="red", lw=2)
abline(h=0, col='black', lw=2)


# Real Estate?
data = read.csv('nyc.csv')
names = c("geo", "date", "rent", "vacancy", "cap_rate", "gross_rent")
colnames(data) = names

attach(data)
plot(date, rent, col="darkblue", main="NYC Office Rent Index (2008 $)", pch=16, 
     xlab="Date", ylab="Rents", xlim=c(1980, 2019), ylim=c(20, 80))
grid(lw=2)
lines(date, rent, pch=16)

plot(date, vacancy, col="darkred", main="NYC Office Vacancy", pch=16, 
     xlab="Date", ylab="Vacancy", xlim=c(1990, 2019), ylim=c(0, 20))
grid(lw=2)
lines(date, vacancy, col="darkred", pch=16)

plot(date, cap_rate, pch=16, col='blue',
     xlab="Date", ylab="%", xlim=c(2006, 2016), ylim=c(3, 8),
     main="NYC Office Cap Rates")
grid(lw=2)
lines(date, cap_rate, col='blue')
detach(data)


# Notes:
# Trends. Are there consistent upward or downward patterns?
# Serial dependence. Are there positive or negative correlations of time-adjacent observations?
# Stationarity. Would the series diverge to $\pm \infty$ in finite time?
# Can we use the logic of economics to explain?



# Seasonality and detrending.
# Consider the following series: JOLTS
rm(list=ls())
par(mfrow=c(1, 1))
jolts = drop_na(fredr("JTUJOL"))
plot(jolts$date, jolts$value, pch=16, col='blue',
     xlab="Date", ylab="Openings in Thousands",
     main="Monthly JOLTS Data")
lines(jolts$date, jolts$value, col='blue')
grid(lw=2)


# Notes:
# This series has monthly fluctuations that may be seasonal.
# It also has broader macroeconomic fluctuations.
# Is it possible to disentanlge the two?
# The Hodrick-Prescott (HP) filter can assist.
# Note that the frequency of data influences how we should do this.
# lambda = 6.25 for annual data.
# lambda = 129600 for monthly data.
# lambda = 104976000000 for daily data.
par(mfrow=c(1, 1))
library(mFilter)
hp = hpfilter(jolts$value, freq=129600, type="lambda", drift=FALSE)

plot(jolts$date, hp$trend, pch=16, col="blue", 
     xlab = "Date", ylab = "Trend Openings in Thousands", ylim=c(2000, 8000),
     main="De-Seasonalized Trend")
lines(jolts$date, hp$trend, col="blue")
grid(lw=2)
abline(h=mean(jolts$value), col="red", lw=2)
abline(v=as.Date("2009-01-01"), col="green", lw=2)

plot(jolts$date, hp$cycle, pch=16, col="blue", 
     xlab = "Date", ylab = "Cyclical Openings in Thousands", ylim=c(-2000, 2000),
     main="Seasonal Cyclicality")
lines(jolts$date, hp$cycle, col="blue")
grid(lw=2)
abline(h=0, col="red", lw=2)
abline(h=sd(jolts$value), col="green", lw=2)
abline(h=-sd(jolts$value), col="green", lw=2)


# Notes:
# We have decomposed this series into two components:
# A trend that captures broader macroeconomic variation.
# Seasonal cyclicality that captures seasonal above- and below-trend openings. 
# Note that variability stays within about +/- 1 sigma.


# Let's do another.
gdppc = drop_na(fredr("A229RX0A048NBEA"))
plot(gdppc$date, gdppc$value, pch=16, col='blue',
     xlab="Date", ylab="2009 Dollars",
     main="Real Output per Person (1929 - Present)")
lines(gdppc$date, gdppc$value, col='blue')
grid(lw=2)

hp = hpfilter(gdppc$value, freq=6.25, type="lambda", drift=FALSE)

plot(gdppc$date, hp$trend, pch=16, col="blue", 
     xlab = "Date", ylab="2009 Dollars", 
     main="Trend Component")
lines(gdppc$date, hp$trend, col="blue")
grid(lw=2)

plot(gdppc$date, hp$cycle, pch=16, col="blue", 
     xlab = "Date", ylab="2009 Dollars", ylim=c(-800, 800), 
     main="Cyclical Component")
lines(gdppc$date, hp$cycle, col="blue")
grid(lw=2)
abline(h=0, col="red", lw=2)
abline(h=500, col="green", lw=2)
abline(h=-500, col="green", lw=2)


# Notes:
# We have decomposed this series into two components:
# A trend that captures broader macroeconomic variation.
# Seasonal cyclicality that captures seasonal above- and below-trends. 
# Note that variability stays within about +/- $500.
# What do you think macroeconomists are interested in?



# ASSIGNMENT 6

# 1. Re-run all code above to ensure it works.


# 2. Read in a time series data source of your choice from FRED.
  # Describe whether it appears to contain the characteristics of time series discussed 
  # in this lecture.
  # In your description, relate your time series data to the narrative above, 
  # focusing on time series as story-telling.

gdppc = drop_na(fredr("CILSCBM027NBOG"))
plot(gdppc$date, gdppc$value, pch=16, col='blue',
     xlab="Date", ylab="Billions of US Dollars",
     main="Commercial and Industrial Loans, Small Domestically Chartered Commercial Banks")
lines(gdppc$date, gdppc$value, col='blue')
grid(lw=2)

hp = hpfilter(gdppc$value, freq=129600  , type="lambda", drift=FALSE)

plot(gdppc$date, hp$trend, pch=16, col="blue", 
     xlab = "Date", ylab="Billions of US Dollars", 
     main="Trend Component")
lines(gdppc$date, hp$trend, col="blue")
grid(lw=2)

plot(gdppc$date, hp$cycle, pch=16, col="blue", 
     xlab = "Date", ylab="Billions of US Dollars", ylim=c(-800, 800), 
     main="Cyclical Component")
lines(gdppc$date, hp$cycle, col="blue")
grid(lw=2)
abline(h=0, col="red", lw=2)
abline(h=500, col="green", lw=2)
abline(h=-500, col="green", lw=2)

# 3. Using the HP filter, decompose your time series data into trend and cyclical
  # component and graph the results.
  # How does your narrative change when you focus only the trend component?
  
  
  
  
  
  
  