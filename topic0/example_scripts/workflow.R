# data analysis workflow

# go to appropriate directory
# ctrl + shift + h, or
setwd("testdir")

# load the data
oldcars <- read.csv("mtcars.csv")

# data visualization ----------------------------------------------------------

# take a look at the data
str(oldcars)
View(oldcars)

# plot miles per gallon versus horsepower
# get mpg
mpg <- oldcars$mpg

# get hp
hp <- oldcars$hp

plot(hp, mpg) # does the data make sense?

# another way to plot, save some typing
# take advantage of formula syntax
plot(mpg ~ hp, data = oldcars)

# descriptive statistics ------------------------------------------------------

# what is the mean mpg

# by hand

# number of observations 
n <- length(mpg)

# sum of mpg values
mpg_sum <- sum(mpg)

# average
mpg_sum / n

# built in function
mean(mpg)

# inference --------------------------------------------------------------------

# fit a linear regression model for association of hp and mpg
# the line of "best fit"

fit <- lm(mpg ~ hp, data = oldcars)

# compare fitted line to data
plot(hp, mpg)
lines(hp, fit$fitted.values)

# save results ----------------------------------------------------------------

# save as RDS file
saveRDS(fit, file = "mpg_hp_fit.RDS")

# check your handiwork
dir()

# load it again
reloaded_fit <- readRDS("mpg_hp_fit.RDS")

# analyze more of the dataset with me ------------------------------------------

# I want to know the relationship between weight (wt) and quarter mile time (qsec)

# get mean of weight and quarter mile time
oldcars$wt
oldcars$qsec

mean(oldcars$wt)
mean(oldcars$qsec)

# plot the two variables
plot(oldcars$wt, oldcars$qsec)
plot(qsec ~ wt, data = oldcars)

# fit linear regression
fit <- lm(qsec ~ wt, data = oldcars)

# compare data with fitted line
plot(oldcars$wt, oldcars$qsec)
lines(oldcars$wt, fit$fitted.values)

# save results
saveRDS(fit, file = "fit2.RDS")

# more analysis ---------------------------------------------------------------

# we will deal with concepts we haven't learned yet
# that's okay, because I want you to get used to using functions without fully
# understanding what they're doing on the inside
# what matters is the interface
# and getting used to figuring out the interface from help pages

# create a histogram of wt

# get the coefficients of the regression fit

# report a summary of the fit

# plot residuals
