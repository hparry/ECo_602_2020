#Likelihood group activitiy
library(here)
x_observed = c(2,6)
print(x_observed)
dpois(x = 2, lambda = 4.5)
dpois(x = 6, lambda = 4.5)

#product of those particular counts
dpois(x = 2, lambda = 4.5) * dpois(x = 6, lambda = 4.5)

#store as a vector
wiwa_counts = c(2,6)
dpois(x = wiwa_counts, lambda = 4.5)

# calculate the product
prod(dpois(x =wiwa_counts, lambda = 4.5))

#sum of the log-likelihoods
sum(log(dpois(x = wiwa_counts, lambda = 4.5)))


#Data for bird files
dat_bird = read.csv(here::here("data", "bird.sta.csv"))
dat_habitat = read.csv(here::here("data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)

summary(dat_all$WIWA)

hist(dat_all$WIWA)

# try a poisson with lambda = 1.0
sum(log(dpois(x = dat_all$WIWA, lambda = 1.0)))

wiwa_counts = c(2, 6)
dpois(x = wiwa_counts, lambda = 3)
sum(log(dpois(x = wiwa_counts, lambda = 5)))


#Winter Wren
summary(dat_all$WIWR)
png(filename = "histogram winter wrens.png")
hist(dat_all$WIWR, 
main= "Histogram of Winter Wren Counts",
xlab = "Counts",
breaks = 0:7 - 0.5)
dev.off()

sum(log(dpois(x = dat_all$WIWR, lambda = 1.455)))

length_WIWR = length(dat_all$WIWR)

sum(log(dpois(x = wiwa_counts, lambda = 4.5)))

#Question 3
#I'm not sure about this, n=6 because it is the maximum number or birds see at a site
#maybe n should be 7 because n+1 , prob= 0.15 = 1/6
# not sure because the smaller I make prob the smaller value gets


sum(log(dbinom(x= dat_all$WIWR, size = 6, prob = .243)))


