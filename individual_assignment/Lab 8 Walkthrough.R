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

#Nonparametric 2 sample test ?????? Have to find p-value
t.test(flipper_length_mm ~ species, data = penguin_dat, alternative = "less")
wilcox.test(treatment~ pine, data = dat_tree, alternative = "less" )

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

