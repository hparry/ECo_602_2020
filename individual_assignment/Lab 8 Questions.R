#Lab8 Questions

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
