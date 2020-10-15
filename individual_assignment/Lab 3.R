#Lab 3
library(here)
install.packages("psych")
library("psych")
pairs.panels(iris)
here("data", "bird.sta.csv")
#create a data frame for the bird.sta.csv file
dat_bird <- read.csv(here("data", "bird.sta.csv"))
here("data", "hab.sta.csv")
#create a data frame for the hab.sta.csv file
dat_habitat <- read.csv(here("data", "hab.sta.csv"))
#how many rows in each data set? 1046
nrow(dat_habitat)
nrow(dat_bird)
#merge data in dat_bird and dat_habitat into dataframe dat_all
dat_all <- merge(dat_bird, dat_habitat)
nrow(dat_all)
#plot merged data
plot(ba.tot ~ elev, data = dat_all)
#trying to see all of the zero values for CEWA cedar waxwing
sample(dat_all$CEWA, 100)
#example of using booleen functions
my_vec = rep(1:3, 5)
my_vec == 3
my_vec > 1  
#Boolean for CEWA 
CEWA_boolean = dat_bird[, "CEWA"] >= 1
# assign boolean numeric values
CEWA_present_absent <- as.numeric(CEWA_boolean)
#plot CEWA and elevation
plot(x = dat_all$elev, y = CEWA_present_absent)
#finding a logistic curve
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}
#olot the line
plot(x = dat_all$elev, y = CEWA_present_absent)
curve(logistic_midpoint_slope(x,midpoint = 400, slope = 0.1), add = TRUE)
# plot with a negative slope 
plot(x = dat_all$elev, y = CEWA_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)
# plot with a negative, less steep slope of -0.05
plot(x = dat_all$elev, y = CEWA_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.05), add = TRUE)

#made a subset of elev, aspect and slope colums from dat-habitat
dat_habitat[,c("elev", "aspect", "slope", "ba.tot")]
#used the pair panels function to construct pair plots 
pairs.panels(dat_habitat[,c("elev", "aspect", "slope", "ba.tot")])
ncol(dat_habitat)
#bird species presence absence and logistic plots
# Boolean for AMRO- American Robin
#Question 1
AMRO_boolean = dat_bird[, "AMRO"] >= 1
AMRO_present_absent <- as.numeric(AMRO_boolean)
plot(x = dat_all$ba.tot, y = AMRO_present_absent,
     main = "American Robin Presence vs. Elevation",
     xlab = "Total Basal Area",
     ylab = "Presence or Absence")
#plot a line
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}
plot(x = dat_all$ba.tot, y = AMRO_present_absent,
         main = "American Robin Presence vs. Elevation",
         xlab = "Total Basal Area",
         ylab = "Presence or Absence",
     xlim= c(0, 150), 
     cex= .5)
curve(logistic_midpoint_slope(x,midpoint = 30, slope = -0.8), add = TRUE)
#save file as png
png(filename = "AMRO_Elev.png")
plot(x = dat_all$ba.tot, y = AMRO_present_absent,
     main = "American Robin Presence vs. Total Basal Area",
     xlab = "Total Basal Area",
     ylab = "Presence or Absence",
     xlim= c(0, 150), 
     cex= .5)
curve(logistic_midpoint_slope(x,midpoint = 30, slope = -0.8), add = TRUE)
dev.off()

#Question 2
#bird Species BGWA (use this graph)
names(dat_bird)
BGWA_boolean = dat_bird[ , "BGWA"]>= 1
BGWA_present_absent <- as.numeric(BGWA_boolean)
plot(x = dat_all$ba.tot, y = BGWA_present_absent,
     main = " Black Throated Gray vs. Total Basal Area",
     xlab = "Total Basal Area",
     ylab = "Presence or Absence",
     cex = .5)
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}
plot(x = dat_all$ba.tot, y = BGWA_present_absent,
     main = " Black Throated Gray vs. Total Basal Area",
     xlab = "Total Basal Area",
     ylab = "Presence or Absence",
     xlim = c(0, 150),
     cex = .5)
curve(logistic_midpoint_slope(x,midpoint = 25, slope = -0.8), add = TRUE)
#Trying another Species AMCR- American Crow
AMCR_boolean = dat_bird[ , "AMCR"]>= 1
AMCR_present_absent <- as.numeric(AMCR_boolean)
plot(x = dat_all$ba.tot, y = AMCR_present_absent,
     main = " American Crow vs. Total Basal Area",
     xlab = "Total Basal Area",
     ylab = "Presence or Absence")
#Trying another Species of 
BCCH_boolean = dat_bird[ , "BCCH"]>= 1
BCCH_present_absent <- as.numeric(BCCH_boolean)
plot(x = dat_all$ba.tot, y = BCCH_present_absent,
     main = " Black Capped Chickadee vs. Total Basal Area",
     xlab = "Total Basal Area",
     ylab = "Presence or Absence")

#Question 3
sum(dat_all$GRJA)
#Answer to 3 is 181

#Question 4
GRJA_boolean<- dat_bird[ , "GRJA"]>= 1
sum(GRJA_boolean)
#anwser 110
