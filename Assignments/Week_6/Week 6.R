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

# 3. Using the HP filter, decompose your time series data into trend and cyclical
  # component and graph the results.
  # How does your narrative change when you focus only the trend component?
  
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
  
  
  