#Lab 4 
#load library(here)
library(here)
#Question 1
#create 3 vectors or normally- distributed random numbers with mean is 10.4, sd = 2.4
norm_17 <- rnorm(17, mean = 10.4, sd = 2.4)
norm_30 <- rnorm(30, mean = 10.4, sd = 2.4)
norm_300 <- rnorm(300, mean = 10.4, sd = 2.4)

#Question 2
# create a histogram for each
# create a file with histograms as 3 rows and 1 column using par(mfrow= c(3.1))
# Save to file using png
png(filename = "lab_04_hist_01.png",
    width = 700,
    height = 1400, 
    res = 180, 
    units = "px")
par(mfrow= c (3,1))
hist(norm_17,
     main = "Normal Distribution with 17 Observations",
     xlab = "Observations")
hist(norm_30, 
     main = "Normal Distribution with 30 Observations",
     xlab = "Observations")
hist(norm_300,
     main = "Normal Distribution with 300 Observations",
     xlab = "Observations")
dev.off()

#Question 4
#create a Normal Density Function (PDF) graph with mean= 10.4, sd= 2.4
# save as 
png(filename = "norm_1.png")
x = seq(0, 20, length.out = 1000)
y = dnorm(x, mean = 10.4, sd = 2.4)

plot(x, y, 
     main = "Normal PDF Mean of 10.4 and Standard Deviation of 2.4", 
     ylab= "Density",
     type = "l")
abline(h = 0)
dev.off()