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



# The Global Savings Glut: A Conjecture
fedfunds = drop_na(fredr(series_id = "FEDFUNDS", observation_start = as.Date("1990-01-01"), 
                         observation_end = as.Date("2015-01-01")))
tenyear = drop_na(fredr("DGS10", observation_start = as.Date("1990-01-01"), 
                        observation_end = as.Date("2015-01-01")))

plot(tenyear$date, tenyear$value, pch=16, col='blue',
     xlab="Date", ylab="%", ylim=c(0, 10),
     main="Bernanke's Conjecture: A Global Savings Glut")
grid(lw=2)
lines(tenyear$date, tenyear$value, col='blue')
lines(fedfunds$date, fedfunds$value, col = 'red', pch=16)



# Interest Rates: A Conjecture
tenyear = drop_na(fredr("DGS10", observation_start = as.Date("2006-11-08")))
plot(tenyear$date, tenyear$value, pch=16, col='blue',
     xlab="Date", ylab="%", ylim=c(0, 6),
     main="Consequence-Free Thought Leadership")
grid(lw=2)
lines(tenyear$date, tenyear$value, col='blue')
abline(h=5.0, col='red')
abline(v=as.Date("2016-11-08"), col='green')



# Measures of central tendency.
rm(list=ls())
x = c(2, 6, 5, 0, 7, 9, 5, 5, 7, 5)
y = c(10, 9, 10, 3, 6, 9, 9, 10, 2, 9)
mean(x)
var(x)
sd(x)

mean(y)
var(y)
sd(y)

cov(x, y)
cor(x, y)



# A Real-World Example: Returns and Risk
getSymbols(c('AAPL'), from="2006-01-01", to="2017-12-31")
AAPL = AAPL$AAPL.Adjusted
names = c("AAPL")
colnames(AAPL) = names
plot(AAPL, main="AAPL ($/Share)", col = "darkblue")
returns = na.omit(diff(log(AAPL), lag=1))
risk = sqrt(returns^2) - mean(returns) 

mean(returns)
plot(returns, col = "darkblue", main="AAPL Returns")
hist(returns, breaks=100, col="darkblue", freq=F, 
     main="Histogram of AAPL Daily Returns (2006-2017)", xlab="Daily Returns", 
     xlim=c(-.2, .2)) 
abline(v=0, col="red")

mean(risk)
plot(risk, col = "darkblue", main="Risk in AAPL Returns")
hist(risk, breaks=100, col="darkblue", freq=F, 
     main="Histogram of AAPL Risk (2006-2017)", xlab="Return Standard Deviation", 
     xlim=c(0, .2)) 
abline(v=mean(risk), col="red")

# What is the correlation between expost returns and risk?
cor(returns, risk)

# Sharpe Ratio
mean(returns) / mean(risk)



# The Nomral Distribution: Nature's Distribution
# This is a different version of Ex 9-1 of Geltner et al.
par(mfrow = c(1, 3))
# First
curve(dnorm(x, 0, 1/2), -3.3/2, 3.3/2, axes = FALSE, ann = FALSE, 
      xlim = c(-6.6, 6.6), ylim =c(0, .8))
segments(-3.3/2,0,3.3/2,0,lwd = 2)
segments(0, 0, 0, dnorm(0, 0, 1/2), lty="dotted")
mtext(side = 1, line = 0, "mu")
# Second
curve(dnorm(x, 0, 1), -3.3, 3.3, axes = FALSE, ann = FALSE, 
      xlim = c(-6.6, 6.6), ylim =c(0, .8))
segments(-3.3,0,3.3,0,lwd = 2)
segments(0, 0, 0, dnorm(0, 0, 1), lty="dotted")
mtext(side = 1, line = 0, "mu")
# Third
curve(dnorm(x, 0, 2), -3.3*2, 3.3*2, axes = FALSE, ann = FALSE, 
      xlim = c(-6.6, 6.6), ylim =c(0, .8))
segments(-3.3*2,0,3.3*2,0,lwd = 2)
segments(0, 0, 0, dnorm(0, 0, 2), lty="dotted")
segments(2, 0, 2, dnorm(2, 0, 2), lty="dotted")
mtext(side = 1, line = 0, "mu")
arrows(0,.05,2,.05,length=.05, code = 3)
text(1, .09, "sigma")
par(mfrow=c(1,1))



# Visualization of covariace and correlation.
opar = par(no.readonly = TRUE) # copy of current settings
par(mfrow= c(1, 3), mar = c(4.1,4.1,5.1,1.1))
x1 = c(58, 72, 72, 86, 86, 100, 100, 114, 114, 128)
y1 = c(80, 80, 90, 90, 100, 100, 110, 110, 120, 120)
plot(x1, y1, las = 0, xlab = TeX("$X_1$"), ylab = TeX("$Y_1$"), main = TeX("$Covariance>0$"), pch = 19)
abline(h = mean(y1), lty = "dashed")
abline(v = mean(x1), lty = "dashed")
x2 = c(58, 72, 72, 86, 86, 100, 100, 114, 114, 128)
y2 = c(1200, 1200, 1100, 1100, 1000, 1000, 900, 900, 800, 800)
plot(x2, y2, las = 0, xlab = TeX("$X_1$"), ylab = TeX("$Y_1$"), main = TeX("$Covariance<0$"), pch = 19)
abline(h = mean(y2), lty = "dashed")
abline(v = mean(x2), lty = "dashed")
x3 = c(25.5, 27.0, 30, 33, 34.5, 33, 30, 27, 28.8, 31.2)
y3 = c(30, 33, 34.5, 33, 30, 27, 25.5, 27, 30, 30)
plot(x3, y3, las = 0, xlab = TeX("$X_1$"), ylab = TeX("$Y_1$"), main = TeX("$Covariance=0$"), pch = 19)
abline(h = mean(y3), lty = "dashed")
abline(v = mean(x3), lty = "dashed")
par(opar)  # reset settings



# Obviously there are an in-built formulas.
cov(x1, y1)
cov(x2, y2)
cov(x3, y3)

cor(x1, y1)
cor(x2, y2)
cor(x3, y3)



# The Bivariate Normal: 
  # The return on wwo assets may be positively or negatively correlatted.
rm(list=ls())
f1 = function(x, y, p = 0){
  exp( (x^2 - 2*p*x*y + y^2) / (-2*(1 - p^2)) ) / (2*pi*sqrt(1 - p^2)) }
f2 = function(x, y, p = 0.4){
  exp( (x^2 - 2*p*x*y + y^2) / (-2*(1 - p^2)) ) / (2*pi*sqrt(1 - p^2)) }
f3 = function(x, y, p = 0.8){
  exp( (x^2 - 2*p*x*y + y^2) / (-2*(1 - p^2)) ) / (2*pi*sqrt(1 - p^2)) }
opar = par(no.readonly = TRUE) # copy of current settings
par(mfrow = c(1, 3), mar = c(1.1, 1.1, 1.1, 1.1), pty = "s")
x = seq(-3, 3, length = 40)
y = x
persp(x, y, outer(x, y, f1), zlab = "z", main = expression(rho == 0),
      theta = -25, expand = 0.65, phi = 25, shade = 0.2)
persp(x, y, outer(x, y, f2), zlab = "z", main = expression(rho == 0.4),
      theta = -25, expand = 0.65, phi = 25, shade = 0.2)
persp(x, y, outer(x, y, f3), zlab = "z", main = expression(rho == 0.8),
      theta = -25, expand = 0.65, phi = 25, shade = 0.3)
par(opar)


f1 = function(x, y, p = 0){
  exp( (x^2 - 2*p*x*y + y^2) / (-2*(1 - p^2)) ) / (2*pi*sqrt(1 - p^2)) }
f2 = function(x, y, p = -0.4){
  exp( (x^2 - 2*p*x*y + y^2) / (-2*(1 - p^2)) ) / (2*pi*sqrt(1 - p^2)) }
f3 = function(x, y, p = -0.8){
  exp( (x^2 - 2*p*x*y + y^2) / (-2*(1 - p^2)) ) / (2*pi*sqrt(1 - p^2)) }
opar = par(no.readonly = TRUE) # copy of current settings
par(mfrow = c(1, 3), mar = c(1.1, 1.1, 1.1, 1.1), pty = "s")
x = seq(-3, 3, length = 40)
y = x
persp(x, y, outer(x, y, f1), zlab = "z", main = expression(rho == 0),
      theta = -25, expand = 0.65, phi = 25, shade = 0.2)
persp(x, y, outer(x, y, f2), zlab = "z", main = expression(rho == -0.4),
      theta = -25, expand = 0.65, phi = 25, shade = 0.2)
persp(x, y, outer(x, y, f3), zlab = "z", main = expression(rho == -0.8),
      theta = -25, expand = 0.65, phi = 25, shade = 0.2)
par(opar)
opar = par(no.readonly = TRUE) # copy of current settings



# Contour plots or level sets of the above.
f1 = function(x, y, p = 0){
  exp( (x^2 - 2*p*x*y + y^2) / (-2*(1 - p^2)) ) / (2*pi*sqrt(1 - p^2)) }
f2 = function(x, y, p = 0.4){
  exp( (x^2 - 2*p*x*y + y^2) / (-2*(1 - p^2)) ) / (2*pi*sqrt(1 - p^2)) }
f3 = function(x, y, p = 0.8){
  exp( (x^2 - 2*p*x*y + y^2) / (-2*(1 - p^2)) ) / (2*pi*sqrt(1 - p^2)) }
opar = par(no.readonly = TRUE) # copy of current settings
par(mfrow = c(1, 3), mar = c(4.1, 4.1, 4.1, 1.1), pty = "s")
x = seq(-3, 3, length = 50)
y = x
contour(x, y, outer(x, y, f1), nlevels = 10, xlab = "x", ylab = "y",
        main = expression(rho == 0))
contour(x, y, outer(x, y, f2), nlevels = 10, xlab = "x", ylab = "y",
        main = expression(rho == 0.4))
contour(x, y, outer(x, y, f3), nlevels = 10, xlab = "x", ylab = "y",
        main = expression(rho == 0.8))
par(mfrow=c(1,1))



# Sampling: What We Really Observe and Why This Matters.
# Let's clear everything out.
rm(list=ls())
dev.off()
#opar = par(no.readonly = TRUE) # copy of current settings
set.seed(1066)
mu = matrix(c(0,0), nrow = 2, ncol = 1, byrow = TRUE)
rho = 0.0
Sigma = matrix(c(1, rho, rho, 1), nrow=2, ncol=2, byrow = TRUE) 
data = data.frame(mvrnorm(n = 1000, mu, Sigma))
plot(data$X2, data$X1, xlab=TeX("$X_1$"), ylab=TeX("$X_2$"), 
     main="Scatterplot of Simulated Data", pch=16, col='blue',
     xlim=c(-4,4), ylim=c(-4,4))
grid(lw = 2)

# What Do We Do?
abline(lm(X2 ~ X1, data = data), col='red')


set.seed(1066)
mu = matrix(c(0,0), nrow = 2, ncol = 1, byrow = TRUE)
rho = 0.5
Sigma = matrix(c(1, rho, rho, 1), nrow=2, ncol=2, byrow = TRUE) 
data = data.frame(mvrnorm(n = 1000, mu, Sigma))
plot(data$X2, data$X1, xlab=TeX("$X_1$"), ylab=TeX("$X_2$"), 
     main="Scatterplot of Simulated Data", pch=16, col='blue',
     xlim=c(-4,4), ylim=c(-4,4))
grid(lw = 2)
abline(lm(X2 ~ X1, data = data), col='red')


set.seed(1066)
rho = -0.5
Sigma = matrix(c(1, rho, rho, 1), nrow=2, ncol=2, byrow = TRUE) 
data = data.frame(mvrnorm(n = 1000, mu, Sigma))
plot(data$X2, data$X1, xlab=TeX("$X_1$"), ylab=TeX("$X_2$"), 
     main="Scatterplot of Simulated Data", pch=16, col='blue',
     xlim=c(-4,4), ylim=c(-4,4))
grid(lw = 2)
abline(lm(X2 ~ X1, data = data), col='red')


# Is there a relationship between short and long-term interest rates?
threemonth = drop_na(fredr(series_id = "DGS3MO", observation_start = as.Date("2008-01-01")))
tenyear = drop_na(fredr(series_id = "DGS10", observation_start = as.Date("2008-01-01")))
plot(threemonth$value, tenyear$value, 
     xlab=TeX("3 Month Yields"), ylab=TeX("10 Year Yields"), 
     main="Daily Interest Rates Since 2008", pch=16, col='blue')
grid(lw = 2)
abline(lm(tenyear$value ~ threemonth$value), col='red')



# ASSIGNMENT 2

# 1. Re-run all code above to ensure it works.
# 2. Consider the following list of numbers: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
  # Use R to calculate the mean, variance and standard deviation.

rm(list=ls())
x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
mean(x)
var(x)
sd(x)

# 3. Read in the interest rate data discussed above between 2000 and the present.  
  # Graph the data as a scatterplot.

rm(list=ls())
dev.off()

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
Based on Pearl's Ladder of causality, this correlation lies on the first rung of the ladder. 
It is seeking a pattern and codifying the events based on past behavior. 

The second rung would prioritize an intervention. 
Such that the said intervention should provide a different scenario.
In this case, we can potentially test for different time periods to see if this correlation still rings true.
The third rung is about counterfactuals. 
This would be to understand what would have happened if an alternate path was taken. 
A potential scenario would be the understanding of that the 10 yr yields have no correlations to the 3 month yields dataset.
It would prove Bernanke's conjecture.


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


