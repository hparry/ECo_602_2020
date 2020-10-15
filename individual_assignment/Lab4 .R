#Lab 4 uncertainty and error
library(here)
#Plotting a PDF curve
# generate a vector of x-values
x = seq(-3, 3, length.out = 1000)
y = dnorm(x)

plot(x, y, main = "Normal PDF", type = "l")
abline(h = 0)
#example from lab for residuals
set.seed(123)
n = 17
slope = 0.7
intcp = 0.2

guess_x = 6
guess_y = 4
guess_slope = 0.72

x = runif(n = n, min = 1, max = 10)
y = rnorm(n = n, mean = slope * x + intcp)

plot(x, y, pch = 16)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

# create a data frame with random values, runif generates random deviates?
set.seed(123)
n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)
#fit a linear model
set.seed(123)
n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)

guess_x = 6
guess_y = 0
guess_slope = 0.1

plot(y_observed ~ x, data = dat, pch = 8)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)
# add predicted values dat$x is the x column of the dat 
#dataframe that I created earlier.
y_predicted<- c(line_point_slope(dat$x, guess_x, guess_y, guess_slope))
dat<- cbind(dat, y_predicted)
#subtract y_predicted from y_observed   y_observed - y_predicted, 
resids<- c(dat$y_observed - dat$y_predicted)
#bind resids to dat data frame and assign name dat
dat<- cbind(dat, resids)
#find the absolute values of the residuals
absolute_resids<- abs(resids)
#add up all of the residuals
sum(absolute_resids)
# trying out the sum of resids without taking the absolute value ( wouldn't this be
# a better indication of the line fitting?)
sum(resids)
