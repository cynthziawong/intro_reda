# REDA1-CE1000 (WEEK 1):  
# The power of modern, open-source environments.
# Getting Accustomed to R and the R Studio IDE.

# Load the libraries.
library(PASWR2)
library(MASS)
library(repmis)
library(latex2exp)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(stargazer)
library(fredr)
library(quantmod)


# Identify and set the working directory.
getwd() 
setwd("/Users/cynthiaw/Desktop/REDA")



# The power of the Application Protocol Interface (API)
fredr_set_key('0f8e56e0b28e0600bab600b469c1ed24') # My key, please don't abuse.


# ASSIGNMENT 1  

# 1. Rerun all code above to ensure it works.
#All codes worked with the exception of Line 233. See sample below

#d = read.csv("https://stats.idre.ucla.edu/stat/data/hsbraw.csv")
#>Error in file(file, "rt") : 
#  cannot open the connection to 'https://stats.idre.ucla.edu/stat/data/hsbraw.csv'
#In addition: Warning message:
#  In file(file, "rt") :
#  URL 'https://stats.idre.ucla.edu/stat/data/hsbraw.csv': status was 'SSL peer certificate or SSH remote key was not OK'


# 2. Import several economic data series of your choice from FRED.
  # Generate a variety of plots using different colors and point shapes.
  # Selecting one of the data series, write a one-paragraph narrative about 
  # how you would interpret the data series.

CIC = fredr(series_id = "MBCURRCIR", observation_start = as.Date("2000-01-01"))
plot(CIC$date, CIC$value, col = 'Red', pch=16, ylab = "Millions ($)", 
     xlab = "Months", main=" Monetary Base; Currency in Circulation")
lines(CIC$date, CIC$value, col = "Red") 
#Notice a distinct uptick in 2020

MorAvg30 = fredr(series_id = "MORTGAGE30US", observation_start = as.Date("2005-01-01"))
plot(MorAvg30$date, MorAvg30$value, col = 'Orange', pch=16, ylab = "Percent (%)", 
     xlab = "Years", main="30-Year Fixed Rate Mortgage Average in the United States")
lines(MorAvg30$date, MorAvg30$value, col = "Orange") 
#Look for data to investigate real estate loans on FRED: https://fred.stlouisfed.org/series/RELACBW027SBOG

#Plot data
#The current face value of mortgage-backed obligations held by Federal Reserve Banks. These securities are guaranteed by Fannie Mae, Freddie Mac, or Ginnie Mae.
MBS = fredr(series_id = "WSHOMCB", observation_start = as.Date("2005-01-01"))

# Summary of Data
str(MBS)
summary(MBS)

#Quick Exploratory Data Analysis
eda(MBS$value)

#Time Series Plot
plot(MBS$date, MBS$value, col = 'blue', pch=16, ylab = "Millions ($)", 
     xlab = "Weeks", main="Mortgage-Backed Securities")
lines(MBS$date, MBS$value, col = "Blue")
grid(lw=2)


#GGPLOT Version
ggplot(data = MBS, mapping = aes(x = date, y = value, color = series_id)) +
  geom_line() + labs(x = "Weeks", y = "Millions ($)", color = "Series")

#GGPLOT Version 2 With Diff Shape
ggplot(data = MBS, mapping = aes(x = date, y = value, color = series_id)) +
  geom_point(shape=18, fill="blue", color="darkred", size=1) +
  geom_line() + labs(x = "Weeks", y = "Millions ($)", color = "Series")

#GGPLOT Version 3 With Diff Shape
ggplot(data = MBS, mapping = aes(x = date, y = value, color = series_id)) +
  geom_point(shape=6, fill="blue", color="darkred", size=2) +
  geom_line() + labs(x = "Weeks", y = "Millions ($)", color = "Series")

# Histogram
hist(MBS$value, freq = F, breaks=40, col = 'blue', 
     main='Mortgage-Backed Securities', xlab='Millions Spread')
grid(lw=2)




