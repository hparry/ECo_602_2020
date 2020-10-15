library(here)
install.packages("palmerpenguins")
library("palmerpenguins") 
#check what type of data is in Palmerpenguins
class("palmerpenguins")
#construct a data frame of penguins data
penguins = data.frame(penguins)
#calculate the mean bill length of the penguins
mean(penguins$body_mass_g)
#I got an NA with the code above, ran the code below and saw that there 
#is a NA in the data
head(penguins$body_mass_g)
# to remove NA data use na.rm = TRUE
mean(penguins$body_mass_g, na.rm = TRUE)
summary(penguins)
#construct a boxplot
boxplot(penguins$bill_depth_mm)
#construct boxplot with bill depth and data
boxplot(bill_depth_mm ~ sex, data = penguins)
# look at the boxplots side by side
par(mfrow = c(1,2))
boxplot(penguins$bill_depth_mm)
boxplot(bill_depth_mm ~ sex, data = penguins)
#make a coplot, sex is the conditional varialble
coplot(body_mass_g ~ bill_depth_mm | sex, data = penguins)
coplot(body_mass_g ~ bill_depth_mm | bill_length_mm, data = penguins)
#saving plots
library(here)
 png(filename = here("boxplot_1.png"), width = 800, height = 600)
  boxplot(bill_depth_mm ~ sex, data = penguins)
  dev.off()
  #save second boxplot
library(here)  
  png(filename = here("boxplot_2.png"), width = 800, height = 600)
  par(mfrow = c(1,2))
  boxplot(penguins$bill_depth_mm)
  boxplot(bill_depth_mm ~ sex, data = penguins)
  dev.off()
#saving coplots
library(here)
  png(filename = here("coplot_1.png"), width = 800, height = 600)
  coplot(body_mass_g ~ bill_depth_mm | sex, data = penguins)
  dev.off() 
  
library(here)
  png(filename= here ("coplot_2.png"), width = 800, height = 600)
  coplot(body_mass_g ~ bill_depth_mm | bill_length_mm, data = penguins)
  dev.off()

