#Lab 5
#Walk through
library(here)
#create a Ricker function
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b* x))
}

# plop of the shape with parameters a= 1 and b= 1
# to and from is what the range of the x values should be
curve(
  ricker_fun(x, 1, 1),
  from = 0, to = 5, add = FALSE,
  main = "Ricker function: a = 1, b= 1",
  ylab = "f(x)", xlab = "x")

#exponential function
exp_fun = function (x, a, b)
{
  return(a * exp(-b*x))
}

curve(
  exp_fun(x, 0.1, 0.5),
  from = 0, to = 10, add = FALSE, 
  main = "Exponential function",
  ylab = "f(x)", xlab = "x")

#simulating data on a line
set.seed(1234567)

#specify the x-range and number of points
n_pts = 50
x_min = 2
x_max = 10

#Generate the x-values
x_sim = runif(n_pts, min = x_min, max = x_max)

#choose an intercept and slope for our model and generate the predicted y values
param_intercept = 2.3
param_slope = 0.67
y_pred = param_intercept + x_sim * param_slope
plot(x_sim, y_pred)
  
#generate our "observed" y values with normally- distributed noise
error_mean = 0
error_sd = 0.25

y_observed = 
  y_pred +
  rnorm(
    n = n_pts,
    mean = error_mean,
    sd = error_sd)
plot(x_sim, y_observed)

#Normal Errors 2
error_mean = 0
error_sd = 0.1

y_observed_2 =
  y_pred +
  rnorm(
    n = n_pts,
    mean = error_mean,
    sd = error_sd * x_sim)
plot(x_sim, y_observed_2)

#exponentially- distributed errors

y_observed_3 =
  y_pred +
  rexp(
    n = n_pts,
    rate = 1.2)
plot(x_sim, y_observed_3)
points(x_sim, y_pred, pch = 8)

#choosing a model
par(mfrow = c(3,1))
plot(x_sim, y_observed)
plot(x_sim, y_observed_2)
plot(x_sim, y_observed_3)

par(mfrow = c(3, 1))
hist(y_observed - y_pred, 
     main = "sim data 1",
     xlab = "observed y=values")
hist(y_observed_2 - y_pred,
     main = "sim data 2", 
     xlab = "observed y=values")
hist(y_observed_3 - y_pred,
     main = "sim data 3",
     xlab = "observed y=values")
#Salamander
#get data files
dat_salamander<- read.csv(here("data", "salamander_dispersal.csv"))

plot(x= dat_salamander$dist.class, 
     y=dat_salamander$disp.rate.ftb,
     type = "p",
     main= "Juvenial Salamander Dispersal", 
     xlab = "Distance Class (m)",
     ylab = "Dispersal Rate") 

dev.off()

#fit a linear plot to salamander data
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


guess_x = 800
guess_y = 0.35
guess_slope = -.0005

plot(x= dat_salamander$dist.class, 
     y=dat_salamander$disp.rate.ftb,
     type = "p",
     main= "Juvenial Salamander Dispersal", 
     xlab = "Distance Class (m)",
     ylab = "Dispersal Rate") 
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

#fit an exponential line
exp_fun = function (x, a, b)
{
  return(a * exp(-b*x))
}

plot(x= dat_salamander$dist.class, 
     y=dat_salamander$disp.rate.ftb,
     type = "p",
     main= "Juvenial Salamander Dispersal", 
     xlab = "Distance Class (m)",
     ylab = "Dispersal Rate") 

curve(
  exp_fun(x, .1, .5),
  from = 0, to = 10, add = TRUE, 
  main = "Exponential function",
  ylab = "f(x)", xlab = "x")

#fit a Ricker line
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")
