#lab 3 Q2
library(here)
here("data", "bird.sta.csv")
#create a data frame for the bird.sta.csv file
dat_bird <- read.csv(here("data", "bird.sta.csv"))
here("data", "hab.sta.csv")
#create a data frame for the hab.sta.csv file
dat_habitat <- read.csv(here("data", "hab.sta.csv"))
dat_all <- merge(dat_bird, dat_habitat)
plot(ba.tot ~ elev, data = dat_all)
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
png(filename = "BGWA_batot.png")
plot(x = dat_all$ba.tot, y = BGWA_present_absent,
     main = " Black Throated Gray vs. Total Basal Area",
     xlab = "Total Basal Area",
     ylab = "Presence or Absence",
     xlim = c(0, 150),
     cex = .5)
curve(logistic_midpoint_slope(x,midpoint = 25, slope = -0.8), add = TRUE)
dev.off()