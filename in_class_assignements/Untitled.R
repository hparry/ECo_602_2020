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
hist(dat_habitat$aspect, main = "Habitat Aspect", xlab = "Habitat Aspect")
#make a scatter plot of elev, slope, aspect all vs. ba.tot
plot(x= dat_habitat$elev, y = dat_habitat$ba.tot, 
     main = "elevation vs basal area")
plot(x= dat_habitat$slope, y = dat_habitat$ba.tot)
plot(x= dat_habitat$aspect, y = dat_habitat$ba.tot)
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
# make estimates of x1, y1 and slope. then visually select best fit.
curve(line_point_slope(x, x1= 175, y1 = 50, slope = 0.2), add = TRUE)
#The last line fit the best with slope of .2
