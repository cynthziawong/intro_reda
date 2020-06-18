# REDA1-CE1000 (WEEK 2):  
# FOUNDATIONAL CONCEPTS FROM PROBABILITY AND STATISTICS
# See Jupyter notebook.


# Load the libraries.
library(lattice)
library(ggplot2)
library(PASWR2)
library(MASS)
library(repmis)
library(latex2exp)
library(dplyr)
library(tidyverse)
library(stargazer)
library(fredr)
library(quantmod)
fredr_set_key('30e6ecb242a73869e11cb35f6aa3afc3') # My key, please don't abuse.


# Identify and set the working directory.
getwd() 
setwd("/Users/cynthiaw/Desktop/REDA")


# ASSIGNMENT 2

# 1. Re-run all code above to ensure it works.
# 2. Consider the following list of numbers: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
  # Use R to calculate the mean, variance and standard deviation.

x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
mean(x)
var(x)
sd(x)

# 3. Read in the interest rate data discussed above between 2000 and the present.  
  # Graph the data as a scatterplot.

  #Figure 1.1:
  rm(list=ls())

  threemonth = drop_na(fredr(series_id = "DGS3MO", observation_start = as.Date("2000-01-01")))
  tenyear = drop_na(fredr(series_id = "DGS10", observation_start = as.Date("2000-01-01")))
  plot(threemonth$value, tenyear$value, 
      xlab=TeX("3 Month Yields"), ylab=TeX("10 Year Yields"), 
       main="Daily Interest Rates Since 2008", pch=16, col='blue')
  grid(lw = 2)
  abline(lm(tenyear$value ~ threemonth$value), col='red')

# Calculate a correlation between the series.
  #Correlation
  cor(threemonth$value,tenyear$value)
  #Covariance
  cov(threemonth$value,tenyear$value)

# Consider the discussion regarding causality when considering this correlation.
  # Where on the Pearl ladder of causality does your correlation lie?

#Based on Pearl's Ladder of causality, this correlation lies on the first rung of the ladder. 
#It is seeking a pattern and codifying the events based on past behavior. 
#The second rung would prioritize an intervention (See Figure 1.2).
#Such that the said intervention should provide a different scenario.
#In this case, we can potentially test for different time periods to see if this correlation still rings true.
#The third rung is about counterfactuals. 
#This would be to understand what would have happened if an alternate path was taken. 
#A potential scenario would be the understanding of that the 10 yr yields have no correlations to the 3 month yields dataset before the 2008 financial crisis.



#Figure 1.2: This would be an alternate scenario, exploring the second rung on Pearl's ladder
  threemonth = drop_na(fredr(series_id = "DGS3MO", observation_start = as.Date("2012-01-01")))
  tenyear = drop_na(fredr(series_id = "DGS10", observation_start = as.Date("2012-01-01")))
  plot(threemonth$value, tenyear$value, 
       xlab=TeX("3 Month Yields"), ylab=TeX("10 Year Yields"), 
       main="Daily Interest Rates Since 2008", pch=16, col='blue')
  grid(lw = 2)
  abline(lm(tenyear$value ~ threemonth$value), col='red')

  #Correlation
  cor(threemonth$value,tenyear$value)
  #Covariance
  cov(threemonth$value,tenyear$value)
  #Figure 1.2 correlation results indicates a low percentage of correlation. This is taken after the 2008 crisis, 
  #which would not have been effective in proving Bernanke's Conjecture. Ano

