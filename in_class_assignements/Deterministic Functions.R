#everytime you open a r session you need to load the library(here)
library(here)
#use here to get the file path
here("data", "hab.sta.csv")
here()
#use file.exist to ensure that the path is correct
file.exists(here("data", "hab.sta.csv"))
#use below text to download the file 
read.csv(here("data", "hab.sta.csv"))
#repeat the same steps to get file path for bird.sta.csv
file.exists(here("data", "bird.sta.csv"))
read.csv(here("data", "bird.sta.csv"))

# data frame for habitat file
dat_habitat <- read.csv(here("data", "hab.sta.csv"))
#create histogram of elevation, xlab is the label of the x axis
hist(dat_habitat$elev, main= "Elevation", xlab = "Elevation")
hist(dat_habitat$slope, main = "Slope", xlab = "Elevation")
#make a scatter plot of elev, slope, aspect all vs. ba.tot. cex=.5 makes 
# a smaller dot
plot(x= dat_habitat$elev, y = dat_habitat$ba.tot, 
     main = 'Elevation vs. Basal Area', 
     xlab= 'Elevation',
     ylab= 'Total Basal Area (m2/hectare)',
     cex = .5)
#saving scatterplot
png(filename = 'scatterplot_elev_batot.png')
plot(x= dat_habitat$elev, y = dat_habitat$ba.tot, 
     main = 'Elevation vs. Basal Area', 
     xlab= 'Elevation',
     ylab= 'Total Basal Area (m2/hectare)',
     cex = .5)
dev.off()

plot(x= dat_habitat$slope, y = dat_habitat$ba.tot, 
     main = "Slope vs. Total Basal Area",
     xlab = 'Slope',
     ylab = 'Total Basal Area (m2/hectare)',
     cex= .5)
png(filename = "scatterplot_slope_batot.png")
plot(x= dat_habitat$slope, y = dat_habitat$ba.tot, 
     main = "Slope vs. Total Basal Area",
     xlab = 'Slope',
     ylab = 'Total Basal Area (m2/hectare)',
     cex= .5)
dev.off()


plot(x= dat_habitat$aspect, y = dat_habitat$ba.tot, 
     main= "Aspect vs. Total Basal Area",
     xlab = 'Aspect',
     ylab = 'Total Basal Area (m2/hectare)',
       cex= .5)
#combine plots on one image
par(mfrow= c(1,3))
plot(x= dat_habitat$slope, y = dat_habitat$ba.tot, 
     main = "Slope vs. Total Basal Area",
     xlab = 'Slope',
     ylab = 'Total Basal Area (m2/hectare)',
     cex= .5)
plot(x= dat_habitat$elev, y = dat_habitat$ba.tot, 
     main = 'Elevation vs. Basal Area', 
     xlab= 'Elevation',
     ylab= 'Total Basal Area (m2/hectare)',
     cex = .5)
plot(x= dat_habitat$aspect, y = dat_habitat$ba.tot, 
     main= "Aspect vs. Total Basal Area",
     xlab = 'Aspect',
     ylab = 'Total Basal Area (m2/hectare)',
     cex= .5)
#saving all three to one png
png(filename = 'Scatterplot_batot.png')
par(mfrow= c(1,3))
plot(x= dat_habitat$slope, y = dat_habitat$ba.tot, 
     main = "Slope vs. Total Basal Area",
     xlab = 'Slope',
     ylab = 'Total Basal Area (m2/hectare)',
     cex= .5)
plot(x= dat_habitat$elev, y = dat_habitat$ba.tot, 
     main = 'Elevation vs. Basal Area', 
     xlab= 'Elevation',
     ylab= 'Total Basal Area (m2/hectare)',
     cex = .5)
plot(x= dat_habitat$aspect, y = dat_habitat$ba.tot, 
     main= "Aspect vs. Total Basal Area",
     xlab = 'Aspect',
     ylab = 'Total Basal Area (m2/hectare)',
     cex= .5)
dev.off()
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
#habitate aspect plot.
curve(line_point_slope(x, x1= 175, y1 = 50, slope = 0.2), add = TRUE)
#The last line fit the best with slope of .2

#Line of best fit for slope vs. basal total plot
plot(x= dat_habitat$slope, y = dat_habitat$ba.tot, cex= .5)
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
     main = "Elevation vs Basal Area", ylab= "Total Basal Area", xlab = "Elevation", cex = .5)

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
#save Histogram elevation as a png file
png(filename = "Elevation_hist.png")
hist(dat_habitat$elev, main= "Elevation", xlab = "Elevation")
dev.off()
#save Histogram of slope as a png file
png(filename = "Slope_hist.png")
hist(dat_habitat$slope, main = "Slope", xlab = "Elevation")
dev.off()
#save Histogram of Aspect as a png file
png(filename = "Aspect-hist.png")
hist(dat_habitat$aspect, main = "Habitat Aspect", xlab = "Habitat Aspect")
dev.off()

