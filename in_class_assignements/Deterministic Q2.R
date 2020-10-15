#Deterministic Function
#Question 1
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
hist(dat_habitat$slope, main = "Slope", xlab = "Slope", xlim = c(0, 120))
png(filename = "histogram_slope.png")
hist(dat_habitat$slope, main = "Slope", xlab = "Slope", xlim = c(0, 120))
dev.off()
#Question 3
hist(dat_habitat$aspect, breaks= 40, main = "Aspect", xlab = "Aspect")
png(filename = "histogram_aspect.png")
hist(dat_habitat$aspect, breaks= 40, main = "Aspect", xlab = "Aspect")
dev.off()