#Lab 4 
#Question 6
#fit line model
png(filename = "sim_model_fit1")
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

set.seed(345)
n_pts = 15
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot (y_observed ~ x, data = dat, pch = 2)
#below are my guesse about the values in the line
guess_x = 4
guess_y = -0.2
guess_slope = 0.4

plot(y_observed ~ x, data = dat, pch = 2, main= "Simulated Data Model Fit")
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)
dev.off()