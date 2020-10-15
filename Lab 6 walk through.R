#Lab 6 walk through
library(here)
library(palmerpenguins)
penguins<- penguins

#writing our own function for sse_mean

sx = (penguins$bill_depth_mm)
#is.na gives you a binary (T,F) for if it's a number or na, then I added those up and got 2
#n = length(penguins$bill_depth_mm) - sum(is.na(penguins$bill_depth_mm))

sse_mean= function(sx)
  {
  n = length(sx) - sum(is.na(sx))
  standard_dev = sd(sx, na.rm= TRUE)
  
  return(standard_dev/sqrt(n))
}
sse_mean(penguins$bill_depth_mm)

#boxplots
boxplot(flipper_length_mm ~ species, data = penguins)

#boxplot 2 species
dat_pen = subset(penguins, species != "Gentoo")
boxplot(flipper_length_mm ~ species, data = dat_pen)

#droplevels
dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{
  par(mfrow = c(1,2))
boxplot(flipper_length_mm ~ species, data = penguins)
boxplot(flipper_length_mm ~ species, data = dat_pen)
}

dev.off()

#Resampling with Replacement
#Set.seed for reproducability
set.seed(123)

flipper_shuffled = sample(penguins$flipper_length_mm,
                          replace = TRUE)
par(mfrow = c(1,2))
boxplot(flipper_length_mm ~ species, data = penguins)
boxplot(flipper_shuffled ~ penguins$species, xlab = "species")

#t-test:Adelie and Chinstrap Penguins
t.test(dat_pen$flipper_length_mm ~ dat_pen$species)

#Two sample resampling
set.seed(1)
flipper_shuffled = sample(dat_pen$flipper_length_mm)

boxplot(flipper_shuffled ~ dat_pen$species)

#classical test on resamples data, What if we reshuffle the data
#and re-ran the t-test?
t_test_1 = t.test(flipper_shuffled ~ dat_pen$species)
t_test_1

#difference of means
t_test = t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
t_test

t_test$estimate

#the difference of means is below
diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)

#using aggregate to calculate the differnece of means
agg_means = aggregate(flipper_length_mm ~ species,
                       data= dat_pen,
                       FUN= mean,
                       na.rm = TRUE)
diff_observed = diff(agg_means[,2])

agg_means
diff_observed

#Sample Sizes- the number of individuals of each species in the data
table(dat_pen$species)

#resampling with replacement
n_1 = 68
n_2 = 152

dat_1 = sample(dat_pen$flipper_length_mm, n_1, replace = TRUE)
dat_2 = sample(dat_pen$flipper_length_mm, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE)- mean(dat_2, na.rm = TRUE)

print(c(observed = diff_observed, simulated = diff_simulated))

#Simulation function
n_1 = 68
n_2 = 152

two_group_resample = function(x, n_1, n_2) 
{

  dat_1 = sample(x, n_1, replace = TRUE)
  dat_2 = sample(x, n_2, replace = TRUE)
  
  return(mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE))
}
set.seed(4321)
two_group_resample(dat_pen$flipper_length_mm, 68, 152)

#Resampling experiment with histogram
n = 200
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences)

sum(abs(mean_differences) >= diff_observed)

#rerun it n = 2000
n = 2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}
png(filename = "flipper length n 2000.png")
hist(mean_differences, main = "Mean Difference in Penguin Flipper Length, n = 2000",
     xlab = "mean difference")

dev.off()
sum(abs(mean_differences) >= diff_observed)

#Retrieving named elements
t_test = t.test(flipper_shuffled ~ dat_pen$species)

#str() lets us see what the object contains
str(t_test)

# there is an item called estimate we can access with $
t_test$estimate

t.test(two_group_resample)


