#Lab 5 Question 5
library(here)
#scatterplot of distance class vs. Dispersal Rate
png(filename = "salamander scatterplot.png")
dat_salamander<- read.csv(here("data", "salamander_dispersal.csv"))

plot(x= dat_salamander$dist.class, 
     y=dat_salamander$disp.rate.ftb,
     type = "p",
     main= "Juvenial Salamander Dispersal", 
     xlab = "Distance Class (m)",
     ylab = "Dispersal Rate") 

dev.off()

#Question 6
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

png(filename = "salamander scatter linear model.png")
guess_x = 800
guess_y = 0.25
guess_slope = -.0004

plot(x= dat_salamander$dist.class, 
     y=dat_salamander$disp.rate.ftb,
     type = "p",
     main= "Juvenial Salamander Dispersal", 
     xlab = "Distance Class (m)",
     ylab = "Dispersal Rate") 
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

dev.off()

#Question 7
#exponential model with salamander and save plot
png(filename = "salamander scatterplot exponent")
exp_fun = function (x, a, b)
{
  return(a * exp(-b*x))
}

plot(x= dat_salamander$dist.class, 
     y=dat_salamander$disp.rate.ftb,
     type = "p",
     main= "Juvenile Salamander Dispersal", 
     xlab = "Distance Class (m)",
     ylab = "Dispersal Rate") 

curve(
  exp_fun(x, 1.5, .003),
  from = 0, to = 1600, add = TRUE, 
  main = "Exponential function",
  ylab = "f(x)", xlab = "x")

dev.off()

#Question 8
#Salamander Ricker Model
png(filename = "ricker model salamander.png")
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

plot(x= dat_salamander$dist.class, 
     y=dat_salamander$disp.rate.ftb,
     type = "p",
     main= "Juvenile Salamander Dispersal", 
     xlab = "Distance Class (m)",
     ylab = "Dispersal Rate") 
b = .005
a = .00869 
curve(
  ricker_fun(x, a, b), 
  from = 0, to = 1600, add = TRUE)
dev.off()

#Question 9
guess_x = 800
guess_y = 0.25
guess_slope = -.0004

plot(x= dat_salamander$dist.class, 
     y=dat_salamander$disp.rate.ftb,
     type = "p",
     main= "Juvenial Salamander Dispersal", 
     xlab = "Distance Class (m)",
     ylab = "Dispersal Rate") 
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)
# create the y-predicted from the linear plot
y_predicted_linear<- c(line_point_slope(dat_salamander$dist.class, guess_x, guess_y, guess_slope))

dat_salamander<- cbind(dat_salamander, y_predicted_linear)

#Calculate the residuals
resids_linear<- c(dat_salamander$disp.rate.ftb- dat_salamander$y_predicted_linear)

png(filename = "Histogram of Linear Residuals.png")
hist(resids_linear, 
     main = "Histogram of the Residuals from the Linear Model",
     xlab = "Residuals")
dev.off()

#Question 10

exp_fun = function (x, a, b)
{
  return(a * exp(-b*x))
}

plot(x= dat_salamander$dist.class, 
     y=dat_salamander$disp.rate.ftb,
     type = "p",
     main= "Juvenile Salamander Dispersal", 
     xlab = "Distance Class (m)",
     ylab = "Dispersal Rate") 

curve(
  exp_fun(x, 1.5, .003),
  from = 0, to = 1600, add = TRUE)

#predicted values from curve using x as dat_salamander$dist.class
y_predicted_exp<- c(exp_fun(dat_salamander$dist.class, 1.5, .003))
# add y_predicted_exp values to data frame                         
dat_salamander<- cbind(dat_salamander, y_predicted_exp)
#calculate the residuals
#resids_linear<- c(dat_salamander$disp.rate.ftb- dat_salamander$y_predicted_linear)
resids_exp<- c(dat_salamander$disp.rate.ftb- dat_salamander$y_predicted_exp)
png(filename = "Histogram of residuals with Exp.png")
hist(resids_exp,
     main = "Histogram of the Residuals from the Exponential Model",
     xlab = "Residuals",
     xlim = c(-1.5, 1.5))
dev.off()

#Question 11
plot(x= dat_salamander$dist.class, 
     y=dat_salamander$disp.rate.ftb,
     type = "p",
     main= "Juvenial Salamander Dispersal", 
     xlab = "Distance Class (m)",
     ylab = "Dispersal Rate") 

ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

b = .005
a = .00869 
curve(
  ricker_fun(x, a, b), 
  from = 0, to = 1600, add = TRUE) 
#make a vector of the y_predicted_ricker values using dat_salamander$dist.class as x
y_predicted_ricker<- c(ricker_fun(dat_salamander$dist.class, .00869, .005))
#add y_predicted_ricker to dat_salamander
dat_salamander<- cbind(dat_salamander, y_predicted_ricker)                      
# calculate the residuals for the ricker function
resids_ricker<- c(dat_salamander$disp.rate.ftb- dat_salamander$y_predicted_ricker)
#make a histogram with the Ricker residuals
png(filename = "Histogram of Ricker model residuals.png")
hist(dat_salamander$y_predicted_ricker,
     main = "Histogram of the Residuals from the Ricker Model",
     xlab = "Residuals")
dev.off()