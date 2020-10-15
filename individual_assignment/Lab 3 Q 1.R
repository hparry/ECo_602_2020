#Lab 3 Question 1
library(here)
dat_bird <- read.csv(here("data", "bird.sta.csv"))
dat_habitat <- read.csv(here("data", "hab.sta.csv"))
dat_all <- merge(dat_bird, dat_habitat)
AMRO_boolean = dat_bird[, "AMRO"] >= 1
AMRO_present_absent <- as.numeric(AMRO_boolean)
plot(x = dat_all$ba.tot, y = AMRO_present_absent,
     main = "American Robin Presence vs. Elevation",
     xlab = "Total Basal Area",
     ylab = "Presence or Absence")

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
     main = "American Robin Presence vs. Total Basal Area",
     xlab = "Total Basal Area",
     ylab = "Presence or Absence",
     xlim= c(0, 150), 
     cex= .5)
curve(logistic_midpoint_slope(x,midpoint = 30, slope = -0.8), add = TRUE)
png(filename = "AMRO_Elev.png")
plot(x = dat_all$ba.tot, y = AMRO_present_absent,
     main = "American Robin Presence vs. Total Basal Area",
     xlab = "Total Basal Area",
     ylab = "Presence or Absence",
     xlim= c(0, 150), 
     cex= .5)
curve(logistic_midpoint_slope(x,midpoint = 30, slope = -0.8), add = TRUE)
dev.off()