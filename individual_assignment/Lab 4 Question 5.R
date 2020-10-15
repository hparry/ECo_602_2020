#Lab 4
#Question 5
#create a dataframe with random data
#try 4 different plots
#set.seed
#arrange scatterplots into single figure

png(filename = "sim_data_scatterplots.png")
par(mfrow = c(2,2))
set.seed(123)
n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot (y_observed ~ x, data = dat, pch = 8)

set.seed(345)
n_pts = 15
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot (y_observed ~ x, data = dat, pch = 2)

set.seed(158)
n_pts = 5
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot (y_observed ~ x, data = dat, pch = 9)

set.seed(592)
n_pts = 18
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot (y_observed ~ x, data = dat, pch = 3)
dev.off()