#Using Models
library(here)
catrate = read.csv(here("data", "catrate.csv"))
head(catrate)
summary(catrate)

#histogram of cat.rate
hist(catrate$cat.rate,
     main = "Histogram of Catastrophe Rates",
     xlab = "Catastrophe Rate")

##-----------Check for Normality---------

#Shapiro- Wilk Test, shapiro.test()
shapiro.test(catrate$cat.rate)

#Other Normality Tests, nortest, ad.test, lillie.test

##-----------------One-Sample Tests: Tests for Difference from Expectation----------
#parametric one-sample test, t-test
t.test(catrate$cat.rate, mu = 2/7)

#One-sidede Alternative Hypothesis, add argument, alternative= "greater" to t-test
t.test(catrate$cat.rate, alternative = "greater", mu = 2/7)

#if we proposed a 1 tailed hypothesis that the observed data would be less than
#the null hypothesis, we would us alternative = "less"
t.test(catrate$cat.rate, alternative = "less", mu = 2/7)


##--------Non-Parametric One Sample test----------
#the Wilcoxon Rank Sum Test

wilcox.test(catrate$cat.rate, mu = 2/7)
?wilcox.test()

##---------------Comparing two sample means--------------
require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))

# Numerical/ Graphical Exploration
summary(penguin_dat)

boxplot(flipper_length_mm ~ species, data = penguin_dat)

# Testing for Normality, shaprio test
#subset by species
dat_adelie = subset(penguin_dat, species == "Adelie")
dat_chinstrap = subset(penguin_dat, species == "Chinstrap")
?shapiro.test
shapiro.test(dat_adelie$flipper_length_mm)
shapiro.test(dat_chinstrap$flipper_length_mm)

##-------------Parametric and Nonparametric Tests-----------
t.test(flipper_length_mm ~ species, data = penguin_dat)

wilcox.test(flipper_length_mm ~ species, data = penguin_dat)
levels(penguin_dat$species)
