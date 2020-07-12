# REDA1-CE1000 (WEEK 4):  
# EXTENSIONS
# See Jupyter notebook.

install.packages("haven")

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
setwd("/Users/cynthiaw/Desktop/2020_NYU_REDA/Course/Week_4")


# R^2 in insolation.
rm(list=ls())
set.seed(1066)
e1 = rnorm(1000, mean=0, sd=1)  ## R's command to draw from N(0,1)
e2 = rnorm(1000,mean=0, sd=3)
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