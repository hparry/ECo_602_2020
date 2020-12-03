#Using Models Questions

##-------------Question 1----------
png(filename = "hist catastrophic salamander.png")
hist(catrate$cat.rate,
     main = "Histogram of Salamander Reproduction Catastrophe Rates",
     xlab = "Catastrophe Rate")

dev.off()

##-------------Question 2----------
shapiro.test(catrate$cat.rate)

##-------------Question 3--------------
#non-normal distribution
#the null hypotheis for a Shaprio test is that the data is normally distributed

##-------------Question 4-------------
t.test(catrate$cat.rate, mu = 2/7)

##-------------Question 5-----------
# pvalue is 0.01193

##----------------Question 6-----------
wilcox.test(catrate$cat.rate, mu = 2/7)

##------------Question 7--------------
#the pvalue is .006275
#the pvalue is small so we should reject the null hypothesis
#which is that the observed mean the same as the expected cate rate. 
#we have evidence that the observed mean is not the same as the expected mean.

##------------Question 8---------------

##------------Question 9---------------
dat_adelie = subset(penguin_dat, species == "Adelie")
dat_chinstrap = subset(penguin_dat, species == "Chinstrap")

shapiro.test(dat_adelie$flipper_length_mm)
shapiro.test(dat_chinstrap$flipper_length_mm)

##------------Question 10--------------
png(
  filename = "histogram adelie chinstrap 1400",
  width = 1400, height = 800, res = 120, units = "px")
par(mfrow= c(1,2))
hist(dat_adelie$flipper_length_mm,
     main = "Histogram of Adelie Penguins Flipper Lengths",
     xlab = "flipper length(mm)")
hist(dat_chinstrap$flipper_length_mm,
     main = "Histogram of Chinstrap Penguins Flipper Lengths",
     xlab = "flipper length(mm)")
dev.off()

##-----------Question 11-------------
t.test(flipper_length_mm ~ species, data = penguin_dat)
