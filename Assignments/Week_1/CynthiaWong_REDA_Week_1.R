# REDA1-CE1000 (WEEK 1):  
# The power of modern, open-source environments.
# Getting Accustomed to R and the R Studio IDE.
# Install these packages using install.packages("").

install.packages(c("PASWR2", "MASS", "repmis", "latex2exp", "dplyr", 
                   "ggplot2", "tidyverse", "stargazer", "fredr", "quantmod"))

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


# Some example code to get used to.
set.seed(1492) # Set seed makes results reproducible.
ruv = runif(n = 20, min = 0, max = 1) # Generate a uniform[0, 1] RV with 20 draws.
round(ruv, 4) # Round answers to 4 decimals places. 


# Basic summary statistics.
wn = rnorm(1000, mean=0, sd=1) # Sample 1,000 draw from N(0, 1)
mean(wn)
var(wn)
summary(wn)
sqrt(var(wn))


(7 * 3) + 12/2 - 7^2 + sqrt(4) # R can act as basic calculator.


# Graphing
age = c(1, 3, 5, 2, 11, 9, 3, 9, 12, 3) # Generate some fake data.
weight = c(4.4, 5.3, 7.2, 5.2, 8.5, 7.3, 6, 10.4, 
            10.2, 6.1)
mean(weight)
sd(weight)
cor(age, weight)
plot(age, weight)

plot(age, weight, pch=16, col="blue")

plot(age, weight, pch=16, col="blue", 
     main="Weight versus Age", xlab="Age", ylab="Weight")

plot(age, weight, pch=16, col="blue", 
     main="Weight versus Age", xlab="Age", ylab="Weight")
grid(lw=2)
abline(lm(weight~age), col="red")


x = 1:50 # Generate a list from 1 to 50.
y = x + rnorm(n = 50, mean = 0, sd = 0.5) # Add normal errors.
model = lm(y ~ x) # Regress y on x.
model # Print results
stargazer(model, type="text", title="Random Model", single.row=TRUE, 
          ci=TRUE, ci.level=0.95) # Print a nice layout


# R as Excel: the R Dataframe
nv = c(1, 3, 6, 8) # Numeric list
cv = c("a", "d", "f", "p") # Character list
lv = c(TRUE, FALSE, FALSE, TRUE) # Logical list
DF1 = data.frame(nv, cv, lv) # Create an R dataframe
DF1 # Print out the dataframe.
str(DF1) # Describe its contents.

DF1$nv # Dollar sign prefix links dataframe to column name.

DF1$cv # Again.

attach(DF1) # Attach data in memory

nv

cv

detach(DF1) # Detach data from memory.



# The power of R, the CRAN Repository, and Library Vignettes.



# The power of the Application Protocol Interface (API)
fredr_set_key('0f8e56e0b28e0600bab600b469c1ed24') # My key, please don't abuse.
gdp_pc = fredr('A229RX0A048NBEA') # Grab GDP per capita
attach(gdp_pc) # Attach and plot.
plot(date, value) # Basic graphs in R.

plot(date, value, col = 'red', pch=16) # In red.

plot(date, value, col = 'blue', pch=16) # In blue.

plot(date, value, col = 'blue', pch=16, xlab="Date", ylab="GDP per Capita", 
     main="GDP per Capita")
grid(lw=2)
lines(date, value) # As good as Excel.
detach(gdp_pc)


unrate = fredr(series_id = "UNRATE", observation_start = as.Date("1990-01-01"))
plot(unrate$date, unrate$value, col = 'blue', pch=16, ylab = "Rate", xlab = "Date", 
     main="U3 Unemployment Rate")
lines(unrate$date, unrate$value, col = 'blue')
grid(lw=2)


tenyear = fredr(series_id = "DGS10", observation_start = as.Date("1990-01-01"))
plot(tenyear$date, tenyear$value, col = 'blue', pch=16, ylab = "Rate (%)", 
     xlab = "Date", main="10 Year US Treasurys")
lines(tenyear$date, tenyear$value, col = 'blue')
grid(lw=2)
abline(h=3, col='red')


yieldcurve = fredr(series_id = "T10Y3M", observation_start = as.Date("2017-01-01"))
plot(yieldcurve$date, yieldcurve$value, col = 'blue', pch=16, ylab = "Rate (%)", 
     xlab = "Date", main="Yield Curve")
lines(yieldcurve$date, yieldcurve$value, col = 'blue')
grid(lw=2)
abline(h=0, col='red')


vix = fredr(series_id = "VIXCLS", observation_start = as.Date("2006-01-01"))
plot(vix$date, vix$value, col = 'blue', pch=16, ylab = "Index", 
     xlab = "Date", main="Volatility Index")
lines(vix$date, vix$value, col = 'blue')
grid(lw=2)


sales = fredr(series_id = "RSAFS", observation_start = as.Date("2006-01-01"))
plot(sales$date, sales$value, col = 'blue', pch=16, ylab = "Millions ($)", 
     xlab = "Date", main="Monthly Sales of Retail and Food")
lines(sales$date, sales$value, col = 'blue')
grid(lw=2)


claims = fredr(series_id = "ICSA", observation_start = as.Date("2020-01-01")) 
claims$value = claims$value / 1000000 #Normalize by dividing it
plot(claims$date, claims$value, col = 'blue', pch=16, ylab = "Claims (Millions)", 
     xlab = "Date", main="Weekly UI Claims (Millions)")
lines(claims$date, claims$value, col = 'blue')
grid(lw=2)


balance_sheet = fredr(series_id = "WALCL", observation_start = as.Date("2000-01-01")) 
balance_sheet$value = balance_sheet$value / 1000000
plot(balance_sheet$date, balance_sheet$value, col = 'blue', pch=16, ylab = "Trillions ($)", 
     xlab = "Date", main="Nominal Federal Reserve Balance Sheet")
lines(balance_sheet$date, balance_sheet$value, col = 'blue')
grid(lw=2)


# It is possible to search for the most popular series.
fredr_series_search_text(
  search_text = "federal funds",
  order_by = "popularity",
  sort_order = "desc",
  limit = 1) %>%
  pull(id) %>%
  fredr(series_id = .)


fedfunds = fredr(series_id = "FEDFUNDS", observation_start = as.Date("1990-01-01"))
plot(fedfunds$date, fedfunds$value, col = 'blue', pch=16, ylab = "Rate", xlab = "Date",
     main="Fed Funds Rate")
grid(lw=2)
lines(fedfunds$date, fedfunds$value, col = 'blue')

ggplot(data = fedfunds, mapping = aes(x = date, y = value, color = series_id)) +
  geom_line() + labs(x = "Date", y = "Rate", color = "Series") # GG Plot
# R is a powerful graphing engine, but I won't cover too much of it here.
# Ample materials online to help you.
# Some code below to help.


# Data scrapping.
griliches = read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/Ecdat/Griliches.csv")
str(griliches)
summary(griliches)
hist(lw80) # Histograms
hist(griliches$lw80, freq = F)
hist(griliches$lw80, freq = F, breaks=40, col = 'blue')


# How would I add a title to this histogram?  Let's experiment.
hist(griliches$lw80, freq = F, breaks=40, col = 'blue', 
     main='Histogram of Log Wages', xlab='Log Wages')
grid(lw=2)

hist(griliches$lw80, freq = F, breaks=40, col = 'red', 
     main='Histogram of Log Wages', xlab='Log Wages')
grid(lw=2)


# Data manipulation: feature engineering.
griliches$age2 = (griliches$age)^2 # Generate an additional variable, the square of age.


plot(griliches$age, griliches$lw80, col='blue', pch=16, xlab="Age", 
     ylab="Log of Hourly Wages", main = "A Scatterplot")
plot(griliches$age2, griliches$lw80, col='blue', pch=16, 
     xlab="Age Squared", ylab="Log of Hourly Wages", main = "A Scatterplot")


# Another data example using NY Census data.
url = "http://www2.census.gov/geo/docs/maps-data/data/gazetteer/census_tracts_list_36.txt"
data = read.csv(url, header=TRUE, sep='\t')
names = c('usps', 'geo', 'pop', 'hu', 'land', 'water', 
          'landSqmi', 'waterSqmi', 'lat', 'long')
colnames(data) = names

plot(data$long, data$lat, pch=16, col="blue", 
     main="The Empire State by Census Centroid", xlab="Longitude", ylab="Latitude")
grid(lw=2)


# Another example of scrapping curated data. 
d = read.csv("https://stats.idre.ucla.edu/stat/data/hsbraw.csv")
d
mean(d$read) # Summary statistics
median(d$read)
var(d$read)
sd(d$read)


# Graphical power.
# Some very useful code for plotting the parabola: y = x ^ 2.
par(mfrow=c(3, 3), pty = "m")  # 3 by 3 layout
x = -4:4
y = x^2
plot(x, y, xlim=c(-8, 8), ylim = c(0, 20), main ="")
title(main = "Default values with limits \n for x and y axes altered")
plot(x, y, pch = "x", xlim=c(-8, 8), ylim = c(0, 20), main="")
title(main = "Default plotting character \n changed to x")
plot(x, y, type = "l", xlim = c(-8, 8), ylim = c(0, 20), main="")
title(main = "Lines connecting the data")
plot(x, y, type = "b", xlim = c(-8, 8), ylim = c(0, 20), main="")
title(main = "Both point and lines \n between data")
plot(x, y, type = "h", xlim = c(-8, 8), ylim = c(0, 20), main="")
title(main = "Vertical lines")
plot(x, y, type = "o", xlim = c(-8, 8), ylim = c(0, 20), main="")
title(main = "Overlaid points \n and connected lines")
plot(x, y, type = "s", xlim = c(-8, 8), ylim = c(0, 20), main="")
title(main = "Stairsteps")
plot(x, y, xlim = c(-8, 8), ylim = c(0, 20), main = "", xlab = "X Axis",
     ylab = "Y Axis")
title(main = "Basic plot with axes labeled")
plot(x, y, type = "n", xlim = c(-8, 8), ylim = c(0, 20), xlab = "",
     ylab = "", main = "")
title(main = "Empty Graph \n(No Plotted Points)")
text(0, 16, "Some Red Text", col = "red")
text(0, 10, expression(paste("Some Math: ", bar(x)==sum(frac(x[i],
                                                             n), i==1, n))))
Alpha = round(mean(y), 3)
text(0, 3, bquote(paste("The Mean: ", bar(y)==.(Alpha))))
par(mfrow=c(1, 1))


# Colors and points.
# figure margins of 2.2, 2.2, 0.2, and 0.2 lines
par(mar=c(2, 2, 0, 0) + 0.2)
plot(x = 1, y = 1, xlim = c(1, 16), ylim = c(-1.5, 5), type = "n",
     xlab = "", ylab = "")  # create empty plot with x and y axes
COLORS = c("black", "red", "green", "darkblue", "darkgreen",
            "magenta", "orange", "cyan")  # vector of colors
# symbols (pch = 0:7) placed at (1, 4), (3, 4), ...(15, 4) with
# character expansion 1:8 with color specified in COLORS
points(x = seq(1, 15, 2), y = rep(4, 8), cex = 1:8, col = COLORS,
       pch = 0:7, lwd = 2)
# labels 0:7 placed at (1, 2), (3, 2),..., (15, 2) with
# character expansion 1:8 with color specified in COLORS
text(x = seq(1, 15, 2), y = rep(2, 8), labels = paste(0:7), cex = 1:8,
     col = COLORS)
# symbols (pch = 8:15) placed at (1, 0), (3, 0),..., (15, 0)
# with character expansion of 2
points(x = seq(1, 15, 2), y = rep(0, 8), pch = 8:15, cex = 2)
# labels 8:15 placed 0.7 to the right of (1, 0), (3, 0),..., (15, 0)
# with character expansion of 2
text(x = seq(1, 15, 2) + 0.7, y = rep(0, 8), labels = paste(8:15),
     cex = 2)
# symbols (pch = 16:23) placed at (1, -1), (3, -1),..., (15, -1)
# with character expansion of 2
points(x = seq(1, 15, 2), y = rep(-1, 8), pch = 16:23, cex = 2)
# labels 16:23 placed 0.7 to the right of (1, -1), (3, -1),..., (15, -1)
# with character expansion of 2
text(x = seq(1, 15, 2) + 0.7, y = rep(-1, 8), labels = paste(16:23),
     cex = 2)



# Exploratory Data Analysis Using Real-World Data
data("SCORE")
attach(SCORE)
hist(scores, freq = F)

eda(scores) # Incredibly powerful command in R for exploratory data analysis
detach(SCORE)

data("mtcars")
attach(mtcars)
eda(mpg)
eda(hp)



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
  geom_point(shape=6, fill="blue", color="darkred", size=) +
  geom_line() + labs(x = "Weeks", y = "Millions ($)", color = "Series")

# Histogram
hist(MBS$value, freq = F, breaks=40, col = 'blue', 
     main='Mortgage-Backed Securities', xlab='Millions Spread')
grid(lw=2)




