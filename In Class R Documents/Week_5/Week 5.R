# REDA1-CE1000 (WEEK 5):  
# The Zestimate: House Pricing Using Hedonics
# The Linear Probability Model: Making Predictions (Rather than Evaluating the Impact of an Intervention)
# Breaking the Algorithm: Omitted Variable Bias



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
library(RCurl)
library(haven)
fredr_set_key('30e6ecb242a73869e11cb35f6aa3afc3') # My key, please don't abuse.



# Identify and set the working directory.
getwd() 
setwd("/Users/cynthiaw/Desktop/2020_NYU_REDA/Course/Week_5")



# Multivariate linear regression and interpretation of results.
# Application: Predicting House Prices.
# How do home prices in Staten Island vary by their attributes?
# House size?
# Land size?
# Age?
# Location?
rm(list=ls())
SI_Sales = read_dta("SI Sales.dta")
summary(SI_Sales)
attach(SI_Sales)
hist(price, breaks = 100, col="blue", freq=FALSE, 
     xlab="Price $", ylab="%", xlim = c(0, 4000000),
     main = "Sales Prices of Staten Island Homes")

hist(unit_size, breaks = 100, col="blue", freq=FALSE, 
     xlab="Size", ylab="%", xlim = range(unit_size),
     main = "Size in Square Meters of Staten Island Homes")

plot(x=unit_size, y=price, col="blue", main="Price v. Size", pch=16, 
     xlab="Size", ylab="Price", xlim=range(unit_size), ylim=range(price))
grid(lw=2)
abline(lm('price ~ unit_size'), col="red")

model = lm('price ~ unit_size', data = SI_Sales)
stargazer(model, type="text", title="Model", single.row=TRUE, 
          ci=TRUE, ci.level=0.95, digits = 2)



# Notes:
# How to interpret these results?
# For every additional square meter of dwelling size, 
# sales price is 1,658 dollars higher.
# The t-stat is in excess of 2 in absolute value, 
# therefore we reject the null of no or zero effect.
#A 95% confidence interval of this effect is [1630, 1686], 
# which excludes zero. Therefore, we reject the null of no or zero effect.
# Average unit size is 161 square meters, 
# with a standard deviation of 79 square meters.
# Therefore, an increase of one standard deviation in dwelling size would increase 
# the sales price by 79 * 1,658 = 130,982 dollars.



model = lm('price ~ unit_size + land_size', data = SI_Sales)
stargazer(model, type="text", title="Model", single.row=TRUE, 
          ci=TRUE, ci.level=0.95, digits = 2)



# Notes:
# This is a proper application of R^2.  It increased considerably.
# This is evidence of omitted variable bias that we will discuss later.



model = lm('price ~ unit_size + land_size + age', data = SI_Sales)
stargazer(model, type="text", title="Model", single.row=TRUE, 
          ci=TRUE, ci.level=0.95, digits = 2)

model = lm('price ~ unit_size + land_size + age + todt', data = SI_Sales)
stargazer(model, type="text", title="Model", single.row=TRUE, 
          ci=TRUE, ci.level=0.95, digits = 2)



# Notes:
# How to interpret?
# Is this a better model than the first?



# Feature engineering.
# Exampple: Scaling

SI_Sales$priceper1000 = price / 1000
head(SI_Sales)

model = lm('priceper1000 ~ unit_size + land_size + age + todt', data = SI_Sales)
stargazer(model, type="text", title="Model", single.row=TRUE, 
          ci=TRUE, ci.level=0.95, digits = 2)



# Notes:
# Scaling should NOT affect anything.

# Feautre engineering.
# Example: Non-linearities
SI_Sales$age2 = age^2
head(SI_Sales)

model = lm('price ~ unit_size + land_size + age + age2 + todt', data = SI_Sales)
stargazer(model, type="text", title="Model", single.row=TRUE, 
          ci=TRUE, ci.level=0.95, digits = 2)



SI_Sales$unit_size2 = unit_size^2
SI_Sales$land_size2 = land_size^2



model = lm('price ~ unit_size +unit_size2 + land_size + land_size2 + age + age2 + todt', data = SI_Sales)
stargazer(model, type="text", title="Model", single.row=TRUE, 
          ci=TRUE, ci.level=0.95, digits = 2)



# Notes:
# Prices appear to be linear in unit size.
# Prices appear to be linear in land_size. 
# Prices appear to be quadatric in age: 
# initially rising and then falling in age as houses reach the sample's average age.



# Feature Engineering
# Example 3: Elasticities
# It may be of use to engineer your features so that results are unit free.
# The interpretation is: for at 1% change in the feature, what is the % change in the outcome.
# This can be achieved by tranforming the features and outcomes using logarithms.



SI_Sales$logprice = log(price)
SI_Sales$logunit = log(unit_size)
SI_Sales$logland = log(land_size)
SI_Sales$logage = log(age + 1)

model = lm('logprice ~ logunit + logland + logage + todt', data = SI_Sales)
stargazer(model, type="text", title="Model", single.row=TRUE, 
          ci=TRUE, ci.level=0.95, digits = 2)

# Notes
# A 10% change in unit size increases sales price by 3.4%.
# A 10% change in land size increases sales price by 2.6%.
# A 10% change in age decreases sales prices by 0.5%.
# Prices in Todt Hill are about 44% higher holding all else constant.
# Is this model directly comparable to the one above?
# No because we non-linearly transformed both the label and the features.



# Another application of multiviariate regression: the Linear Probability Model.
# Notes:
# Linear regression is a very powerful tool with many applications.
# One of its core strengths is the ease with which we can interpret its results.
# Let's use another application, the linear probability model.
# We will convert the linear model into a predictor.
rm(list=ls())
admit = read.csv("binary.csv")
attach(admit)
summary(admit)
head(admit)

# Notes:
# Admit is categorical: 0 is "Applicant Not Admitted", 1 is "Applicant Is Admitted".
# GRE is continuous measure of the applicant's Graduate Record Exam score.
# GPA is continuous measure of the applicant's Grade Point Average.
# Rank is the categorical ranking of the school the applicant attended as an undergraduate.

admit$rank = as.factor(rank) # Designate Rank as a categorical for R.
model = lm('admit ~ gre + gpa + rank', data = admit)
stargazer(model, type="text", title="Model", single.row=TRUE, 
          ci=TRUE, ci.level=0.95, digits = 4)

# Notes:
# Let's interpret the results from the linear probability model.
# GRE: Graduate Record Examine: a one point increase in GRE increases probability of admission by 0.0004 or 0.04%
# GPA: Grade Point Average: a one point increase in GPA increases probability of admission by 0.156 or 15.6%.
# rank: Going to a tier 2 school reduces probabilty of admission by 0.1624 or 16.24% (relative to a tier 1 school).

# Let's make a prediction of admission for the "average" applicant at a tier 4 school.
model$coefficients
mean(gre)
mean(gpa)

model$coefficients[1] + model$coefficients[6] + model$coefficients[2] * mean(gre) +
  model$coefficients[3] * mean(gpa)

# What about a tier 3 school?
model$coefficients[1] + model$coefficients[5] + model$coefficients[2] * mean(gre) +
  model$coefficients[3] * mean(gpa)

# What about a tier 2 school?
model$coefficients[1] + model$coefficients[4] + model$coefficients[2] * mean(gre) +
  model$coefficients[3] * mean(gpa)

# What about a tier 1 school?
model$coefficients[1] + model$coefficients[2] * mean(gre) +
  model$coefficients[3] * mean(gpa)



# Another application of multivariate regression: the Returns to Education.
rm(list=ls())
griliches = read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/Ecdat/Griliches.csv")
attach(griliches)
str(griliches)
griliches = subset(griliches, select = c('lw', 'school', 'iq', 'age', 'expr', 'tenure', 'med'))
summary(griliches)
cor(griliches)

# A Brief Digression on Human Capital Theory and the Role of Education.
# Signal or improvement in the stock of knowledge?

plot(density(lw), col="blue", main="Log Hourly Wages from Griliches (1976)", 
     ylab=" ", xlab="Log Wages") 
abline(v=mean(lw), col="red", lw=2)
grid(lw=2)

# We could start with a series of bivariate models.
plot(school, lw, col="blue", main="Wages versus Educataion", pch=16, 
     xlab="Years of Education", ylab="Wages")
grid(lw=2)
abline(lm('lw ~ school'), col="red")

plot(age, lw, col="blue", main="Wages versus Age", pch=16, 
     xlab="Age", ylab="Wages")
grid(lw=2)
abline(lm('lw ~ age'), col="red")

plot(expr, lw, col="blue", main="Wages versus Job Market Experience", pch=16, 
     xlab="Experience", ylab="Wages")
grid(lw=2)
abline(lm('lw ~ expr'), col="red")

plot(tenure, lw, col="blue", main="Wages versus Current Tenure", pch=16, 
     xlab="Tenure", ylab="Wages")
grid(lw=2)
abline(lm('lw ~ tenure'), col="red")

plot(med, lw, col="blue", main="Wages versus Mother's Education", pch=16, 
     xlab="Mother's Education", ylab="Wages")
grid(lw=2)
abline(lm('lw ~ med'), col="red")

# Obviously all of this can be brought together into a single model.
model = lm('lw ~ school + age + expr + tenure + med', data = griliches)
stargazer(model, type="text", title="Model", single.row=TRUE, 
          ci=TRUE, ci.level=0.95, digits = 4)


# Suppose that ability were not able to be observed.
# Let's think about this conceptually with two models.
rm(list=ls())
set.seed(1066)  ## Set RNG for replication

x1 = rnorm(10000, mean=0, sd=1)  ## R's command to draw from N(0,1)
x2 = rnorm(10000, mean=0, sd=1)  ## R's command to draw from N(0,1)
e = rnorm(10000, mean=0, sd=1)  ## R's command to draw from N(0,1)
y = 1 + 1 * x1 + 1 * x2 + e
cor(x1, x2)
lm = lm(y~x1)
stargazer(lm, title="Regression Results When We Omit Uncorrelated but Relevant Variable",
          single.row=TRUE, type="text", ci.level=0.95, ci=TRUE)

# Notes:
# This is a powerful result: 
# If we omit something that we know is relevant but uncorrelated with what we include, 
# we still recover an unbiased estimate of what we included in the model 
# that was estimated, even though it is misspecified.
rm(list=ls())
set.seed(1492)
z = rnorm(10000, mean=0, sd=1)  ## R's command to draw from N(0,1)
v = rnorm(10000, mean=0, sd=1)  ## R's command to draw from N(0,1)
w = rnorm(10000, mean=0, sd=1)  ## R's command to draw from N(0,1)
e = rnorm(10000, mean=0, sd=1)  ## R's command to draw from N(0,1)
x1 = z + v
x2 = z + w
y = 1 + 1 * x1 + 1 * x2 + e
cor(x1, x2)
lm = lm(y~x1)
stargazer(lm, title="Regression Results When We Omit Correlated Relevant Variable", 
          single.row=TRUE, type="text", ci.level=0.95, ci=TRUE)


# Notes:
# This is a powerfully disturbing result: 
# If we omit something that is correlated with what we include, we know we are 
# violating the key assumption discussed in the Griliches example--
# and we have a direct measurement of its effect.
# It biases the X1 coefficent by 0.5.  
rm(list=ls())


# Notes:
# To explore the implication of omitted variable bias, 
# we can use Monte Carlo replication of the type we used to "prove" the Central Limit Theorem.
beta1ub = rep(0, 1000)  ## Creates a real-valued vector of zeros of 1 by 1000
beta1b = rep(0, 1000)
set.seed(9212014)

for(i in 1:1000) {
  x1 = rnorm(10000, mean=0, sd=1)  ## R's command to draw from N(0,1)
  x2 = rnorm(10000, mean=0, sd=1)  ## R's command to draw from N(0,1)
  e = rnorm(10000, mean=0, sd=1)  ## R's command to draw from N(0,1)
  y = 1 + 1 * x1 + 1 * x2 + e
  linear.model = lm(y ~ x1)
  beta1ub[i] = linear.model$coefficients[2]
  
  z = rnorm(10000, mean=0, sd=1)  ## R's command to draw from N(0,1)
  v = rnorm(10000, mean=0, sd=1)  ## R's command to draw from N(0,1)
  w = rnorm(10000, mean=0, sd=1)  ## R's command to draw from N(0,1)
  e = rnorm(10000, mean=0, sd=1)  ## R's command to draw from N(0,1)
  x1 = z + v
  x2 = z + w
  y = 1 + x1 + x2 + e
  linear.model = lm(y ~ x1)
  beta1b[i] = linear.model$coefficients[2]
}

p1 = hist(beta1ub, freq=TRUE, breaks=25)    
p2 = hist(beta1b, freq=TRUE, breaks=25)     
plot(p1, col=rgb(0,0,1,1/4), xlim=c(.9, 1.6), ylab=" ",
     main="Distributions of Slope Coefficients", 
     xlab="Unbiased in Light Blue and Biased in Red")
plot(p2, col=rgb(1,0,0,1/4), add=T)


# Notes:
# This is a profoundly important point, like causality.
# Think about commercial real estate.  What don't we observe?
# Suppose your cap rate model is only based on a correlation with the 10-year.


# Let's return to the Griliches data.
rm(list=ls())
griliches = read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/Ecdat/Griliches.csv")
attach(griliches)
griliches = subset(griliches, select = c('lw', 'school', 'iq', 'age', 'expr', 'tenure', 'med'))

plot(iq, lw, col="blue", main="Wages versus IQ", pch=16, 
     xlab="IQ", ylab="Wages")
grid(lw=2)
abline(lm('lw ~ iq'), col="red")

plot(iq, school, col="blue", main="IQ versus Education", pch=16, 
     ylab="Years of Education", xlab="IQ")
grid(lw=2)
abline(lm('school ~ iq'), col="red")

model = lm('lw ~ school + age + expr + tenure + med', data = griliches)
stargazer(model, type="text", title="Model", single.row=TRUE, 
          ci=TRUE, ci.level=0.95, digits = 4)

model = lm('lw ~ school + iq + age + expr + tenure + med', data = griliches)
stargazer(model, type="text", title="Model", single.row=TRUE, 
          ci=TRUE, ci.level=0.95, digits = 4)

# What's a Lousy 5% Return?
tenyear = fredr('WGS10YR', observation_start = as.Date("2008-06-30"))

attach(tenyear) # Attach and plot.
plot(date, value, pch=16, col="darkblue", main="10-Year US Treasurys Since 2008", 
     xlab="Date", ylab="%", ylim=c(1, 5.5))  ## time series plot in R  
lines(date, value, col="darkblue")
grid(lw=2)
abline(h=5, col="red", lw=2)



# ASSIGNMENT 5

# 1. Re-run all code above to ensure it works.


# 2. Replicate the omitted variable bias example inducing negative correlation, 
  # rather than positive correlation.  (Pay attention to Z1.)
  # Given the materials presented to date, how do you interpret your finding?


# 3. Zillow's Zestimate is a hedonic price algorithm used to predict US house prices.  
  # Other than the features used above,
  # what other features would you consider relevant in determining US house prices?


# 4. In most circumstances, we seek to draw inferences about the effects of some type of policy or action.
  # Considering the ideas discussed in class regarding causality:
  # Do opportunity zones actually result in benefitial development, 
  # and how could we evaluate potential benefits using data? 