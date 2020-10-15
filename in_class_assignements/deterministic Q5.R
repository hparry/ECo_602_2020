png(filename = "scatterplot_batot")
par(mfrow= c(1,3))
plot(x= dat_habitat$aspect, y = dat_habitat$ba.tot, 
     main= "Aspect",
     xlab = 'Aspect',
     ylab = 'Total Basal Area (m2/hectare)',
     cex= .5)

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
# make estimates of x1, y1 and slope. then visually select best fit for
#habitate aspect plot.
curve(line_point_slope(x, x1= 175, y1 = 50, slope = 0.2), add = TRUE)
#The last line fit the best with slope of .2

#Line of best fit for slope vs. basal total plot
plot(x= dat_habitat$slope, y = dat_habitat$ba.tot,
     main= "Slope",
     xlab= "Slope %",
     ylab= "",
       cex= .5)
#fitting a line
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
# make estimates of x1, y1 and slope. then visually select best fit for
# slope plot.
curve(line_point_slope(x, x1= 35, y1 = 25, slope = 0.5), add = TRUE)

#line of best fit for elevation vs. basal total
plot(x= dat_habitat$elev, y = dat_habitat$ba.tot, 
     main = "Elevation", 
     ylab= "", 
     xlab = "Elevation", 
     cex = .5)

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
# make estimates of x1, y1 and slope. then visually select best fit for
# slope plot.
curve(line_point_slope(x, x1= 200, y1 = 10, slope = 0.1), add = TRUE)
dev.off()
