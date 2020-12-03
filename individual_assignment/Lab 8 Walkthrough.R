#Lab 8 Walkthrough
library(here)
library(palmerpenguins)

#remove Gentoo penguin data
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))

#Parametric Two-Sample Test
#t-test with tthe alternative hypothesis that Adelie penguins have 
#shorter flippers than Chinstrap penguins
t.test(flipper_length_mm ~ species, data = penguin_dat, alternative = "less")





#bootstrap Two-Sample Test
#use  two.boot()
#Install package "simpleboot"
install.packages("simpleboot")
library(simpleboot)
install.packages("boot")

#Subset and take the mean
#save mean values below for later
#mean_adelie_flipper = mean(subset(penguin_dat, species =="Adelie")$flipper_length_mm, na.rm = TRUE)
#mean_chinstrap_flipper = mean(subset(penguin_dat, species =="Chinstrap")$flipper_length_mm, na.rm = TRUE)

##-----------Question 1---------------
adelie_flipper = subset(penguin_dat, species =="Adelie")$flipper_length_mm
chinstrap_flipper = subset(penguin_dat, species =="Chinstrap")$flipper_length_mm


pen_boot = two.boot(adelie_flipper, chinstrap_flipper, mean, R = 10000, na.rm = TRUE)
str(pen_boot)  

png(filename = "Lab 8 Histogram flipper length.png")
hist(pen_boot$t, 
     main = "Histogram of Bootstrap Differences in Flipper Length",
     xlab= "difference in flipper length (mm)")

dev.off()

##---------------Question 2--------

quantile(pen_boot$t, 0.95)
#what is this telling us, I got -4.232032

#Tree Data
library(here)
veg = read.csv(here("data", "vegdata.csv"))
boxplot(pine ~ treatment, dat = veg)

dat_tree = droplevels(subset(veg, treatment %in% c("control", "clipped")))
#create boxplot of pine only with clipped and control 
boxplot(pine~ treatment, dat = dat_tree)

#Table
table(dat_tree$treatment)

##----------Nonparametric 2 sample test ?????? Have to find p-value--------
t.test(pine~ treatment, data = dat_tree, alternative = "less")
wilcox.test(pine~ treatment, data = dat_tree, alternative = "less")
#trying out alternative = "greater"
wilcox.test(pine~ treatment, data = dat_tree, alternative = "greater")

#Bootstrap of tree data
tree_boot = 
  two.boot(
    subset(dat_tree, treatment == "clipped")$pine,
    subset(dat_tree, treatment == "control")$pine,
    FUN = mean,
    R = 10000,
    na.rm = TRUE
  )

boot.ci(tree_boot)
?boot.ci()
?two.boot()
#histogram of bootstrapped samples
hist(tree_boot, main = "Bootstrap sampling distribution")
#
quantile(tree_boot$t, 0.025)

##-------Resampling Linear regression-----
library(here)

dat_bird = read.csv(here("data", "bird.sub.csv"))
dat_habitat = read.csv(here("data", "hab.sub.csv"))

#basin( 3 unique basins), sub (10 unique sub-basins in each basin)
dat_all = merge(
  dat_bird, 
  dat_habitat,
  by = c("basin", "sub"))

#Model Variables
# Simpson's diversity index for breeding birds: b.sidi
#Simpson's diversity indes of vegetation cover types: s.sidi

#plot data,seems to show negative coorelation
plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")

#Simple Linear Regression (least squares)
#model coefficients coef()
#slope coefficient s.sidi (predictor we specify in the model)

fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)

slope_observed = coef(fit_1)[2]

#add regression line to plot
plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_1)


# The slope Coefficient, Monte Carlo randomization to construct our own null 
#hypothesis test
# First simplify our data by extractin the 2 variables that we need
dat_1 = 
  subset(
    dat_all, 
    select = c(b.sidi, s.sidi))

#resampling the data 
#create 2 vectors of randomly generated row indices
index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)
 
#create 2 new vectors of bird and vegetation diversity indices
dat_resampled_i = 
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]

print(slope_resampled_i)

# scatterplot with regression line
plot(
  b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)

##-------Randomization Loop-------
#create a vector to hold results
m= 10000
result = numeric(m)

#create loop, is result[i] the slope parameter
for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE) 
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  result[i] = coef(fit_resampled_i)[2]
}
print(result[i])

#Null distribution, slope_observed comes from earlier in the lab
hist(result, main = "Null Distribution of Regression Slope", xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)

dev.off()
#Critical Slope Value
critical_value <- quantile(result, c(.05))




