#Lab 6 Questions
library(here)

#Question 1
library(palmerpenguins)
penguins<- penguins
sx = (penguins$bill_depth_mm)

sse_mean= function(sx)
{
  n = length(sx) - sum(is.na(sx))
  standard_dev = sd(sx, na.rm= TRUE)
  
  return(standard_dev/sqrt(n))
}
sse_mean(penguins$bill_depth_mm)
sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)

#Question 2
dat_pen = subset(penguins, species != "Gentoo")
dat_pen = droplevels(subset(penguins, species != "Gentoo"))
two_group_resample = function(x, n_1, n_2) 
{
  
  dat_1 = sample(x, n_1, replace = TRUE)
  dat_2 = sample(x, n_2, replace = TRUE)
  
  return(mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE))
}
set.seed(54321)
two_group_resample(dat_pen$flipper_length_mm, 68, 152)

#Question 3
#histogram of resampled differences of means

#Question 4
n = 2000
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

sum(abs(mean_differences) >= 5.8)

#Question 5- answer= 1 in 10 million simulations

#Question 6
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
boxplot(bill_depth_mm ~ species, data = penguins)

dat_pen = subset(penguins, species != "Gentoo")
boxplot(bill_depth_mm ~ species, data = dat_pen)

#droplevels
png(filename = "boxplot bill depth.png")
dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{
  boxplot(bill_depth_mm ~ species, data = dat_pen, 
          main = "Bill Depth of Adelie and Chinstrap Penguins",
          ylab = "bill depth (mm)")
}

dev.off()

#aggregate()
agg_means = aggregate(bill_depth_mm ~ species,
                      data= dat_pen,
                      FUN= mean,
                      na.rm = TRUE)
diff_crit = diff(agg_means[,2])

agg_means
diff_crit

sum(abs(mean_differences) >= diff_crit)

#t-test:Adelie and Chinstrap Penguins
t.test(dat_pen$bill_depth_mm ~ dat_pen$species)

n = 1000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$bill_depth_mm, 68, 152)
  )
}
hist(mean_differences)

#Question 7 bill_length
dev.off()
sx = (penguins$bill_length_mm)

sse_mean(penguins$bill_length_mm)


dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{

  boxplot(bill_length_mm ~ species, data = dat_pen)#histogram with only Adelie and Chinstap
}

agg_means = aggregate(bill_length_mm ~ species,
                      data= dat_pen,
                      FUN= mean,
                      na.rm = TRUE)
diff_crit = diff(agg_means[,2])

agg_means
diff_crit
sum(abs(mean_differences) >= diff_observed)

#t.test
t.test(dat_pen$bill_length_mm ~ dat_pen$species)

#resample 1000 times
n = 1000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$bill_length_mm, 68, 152)
  )
}
hist(mean_differences)

#Question 8

sx = (penguins$body_mass_g)

sse_mean(penguins$body_mass_g)

dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{
  
  boxplot(body_mass_g ~ species, data = dat_pen)#histogram with only Adelie and Chinstap
}

agg_means = aggregate(body_mass_g ~ species,
                      data= dat_pen,
                      FUN= mean,
                      na.rm = TRUE)
diff_crit = diff(agg_means[,2])

agg_means
diff_crit

t.test(dat_pen$body_mass_g ~ dat_pen$species)

n = 1000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$body_mass_g, 68, 152)
  )
}

sum(abs(mean_differences) >= diff_crit)

png(filename = "Histogram body mass.png")
hist(mean_differences,
     main = "Mean Differences in Body Mass of Adelie and Chinstrap Penguins, n = 1000",
     xlab = "mean differences")

dev.off()

sum(abs(mean_differences) >= diff_crit)


