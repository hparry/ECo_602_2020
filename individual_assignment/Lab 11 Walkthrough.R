#Lab 11
#Import data
library(here)
#upload dataset
bird= read.csv(here("data", "bird.sub.csv"))
hab= read.csv(here("data", "hab.sub.csv"))

#merge bird and hab into birdhab
birdhab = merge(bird, hab)
dim(birdhab)

##-------------Graphical Exploration---------
#create a scatterplot of brown creeper abundance and the late successional forest
plot(birdhab$ls, birdhab$BRCR)

##----------Fit a simple linear regression model using lm()----------

fit_1 = lm(BRCR ~ ls, data = birdhab)
abline(fit_1)
summary(fit_1)

##----------------Simulator Function---------------------
#Deterministic Model: Linear Function

linear= function(x, y_int, slope)
{
  return(y_int + slope * x)
}

# testing my function
linear(x = 1, y_int = 1, slope = 1)
linear(x = 3:5, y_int = 1, slope = 1)
linear(x = 3:5, y_int = -1, slope = 1)
linear(x = 3:5, y_int = -1, slope = 0.01)

#Stochastic model
#rnorm
?rnorm()
#generate our "observed" y values with normally- distributed noise

linear_simulator= function(x, y_int, slope, st_dev)
{
 
  n_pts = length(x)
  
  y_observed = 
  linear(x, y_int, slope)
  
  y_out_error= rnorm(
    n = n_pts,
    mean = y_observed,
    sd = st_dev)
  return(y_out_error)
}


length(n_pts)
#testing my function
png(filename = "lab 11 first test.png")
n = 200

par(mfrow = c(2, 2))
for (i in 1:4)
{
  x = runif(n = n)
  plot(
    x, linear_simulator(x, 1, 4.5, 0.1),
    main = "", xlab = "x", ylab = "y",
    pch = 16, col = rgb(0, 0.2, 0, 0.2))
}

# test with .75 standard deviation
dev.off()


#testing function
png(filename = "lab 11 second test.png")
n = 400

par(mfrow = c(2, 2))
for (i in 1:4)
{
  x = runif(n = n)
  plot(
    x, linear_simulator(x, 10, slope = -6.5, st_dev = 1.1),
    main = "", xlab = "x", ylab = "y",
    pch = 16, col = rgb(0, 0.2, 0, 0.2))
}

dev.off()

##-----------------Build simulation------------
#retrieve the model coefficients
fit_1_coefs = coefficients(fit_1)
str(fit_1_coefs)

#retrieve the standard deviation parameter
fit_1_summary = summary(fit_1)
fit_1_summary$sigma

#store the intercept, slope, and standard deviation
int_obs = fit_1_coefs[1] 
slope_obs = fit_1_coefs[2]
sd_obs = fit_1_summary$sigma

##---------------Simulate Data-----------
plot(
  x = birdhab$ls, 
  y = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  ),
  main = "Simulated Data",
  xlab = "late-successional forest",
  ylab = "Brown Creeper Abundance")

##-------------Power Analysis for the linear regression model-------
#Single Simulation
y_sim = linear_simulator(
  x = birdhab$ls,
  y_int = int_obs,
  slope = slope_obs,
  st_dev = sd_obs
)

fit_2 = lm(y_sim ~ ls, data = birdhab)
fit_2_coefs = coefficients(fit_2)
str(fit_2_coefs) #lintercept= .07517, slope= .00552
fit_2_summary = summary(fit_2)
fit_2_summary$sigma
#sigma = .1629711

##-----------Repeated Simulations-------------
n_sims = 1000
p_vals = numeric(n_sims)
for(i in 1:n_sims)
{
  y_sim = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  )
  fit_sim = lm(y_sim ~ birdhab$ls)
  
  p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
}
sum(p_vals < 0.05) / n_sims# statistical power us the number times we
#correctly rejected the null hypothesis / total number of simulations

# function to test the rate that the model can detect a slope of b= .006 and N= 30linear_sim_fit = function(x, y_int, slope, st_dev)
linear_sim_fit = function(x, y_int, slope, st_dev)
{
  y_sim = linear_simulator(
    x = x,
    y_int = y_int,
    slope = slope,
    st_dev = st_dev
  )
  return(lm(y_sim ~ x))
}

##----------Simulating Effect Sizes-----------
#This example simulation estimates statistical power as a 
#function of the slope (the effect size).
alpha = 0.05
n_sims = 10
p_vals = numeric(n_sims)

n_effect_sizes = 20
effect_sizes_1 = seq(-.01, .01, length.out = n_effect_sizes)

effect_size_powers = numeric(n_effect_sizes)

for(j in 1:n_effect_sizes)
{
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = birdhab$ls,
      y_int = int_obs,
      slope = effect_sizes_1[j],
      st_dev = sd_obs
    )
    
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  effect_size_powers[j] = sum(p_vals < alpha) / n_sims
}

sim_effect_size = 
  data.frame(
    power       = effect_size_powers,
    effect_size = effect_sizes_1)

#plot the result adn add a vertical line to show the slope of original data set
plot(
  power ~ effect_size, data = sim_effect_size,
  type = 'l', xlab = 'Effect size', ylab = 'Power')
abline(v = coef(fit_1)[2], lty = 2, col = 'red')

##--------------Simulating Sample Size---------

alpha = 0.05
n_sims = 1000
p_vals = numeric(n_sims)

sample_sizes = seq(5, 100)
sample_size_powers = numeric(length(sample_sizes))

for(j in 1:length(sample_sizes))
{
  x_vals = seq(0, 100, length.out = sample_sizes[j])
  
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = x_vals,
      y_int = int_obs,
      slope = slope_obs,
      st_dev = sd_obs
    )
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  sample_size_powers[j] = sum(p_vals < alpha) / n_sims
}

sim_sample_size = 
  data.frame(
    power       = sample_size_powers,
    sample_size = sample_sizes)

#plot the power results
plot(
  power ~ sample_size, data = sim_sample_size,
  type = 'l', xlab = 'Sample size', ylab = 'Power')
abline(v = nrow(birdhab), lty = 2, col = 'red')

##-------------bivariate Power Analysis---------
#vary combinations of parameters, like slope and sample size, using a loop, 
#saving the results in a matrix, and using contour() or persp() to plot the results.

##------------Effect and Sample Size---------
alpha = 0.01
n_sims = 50

p_vals = numeric(n_sims)

n_effect_sizes = 20
effect_sizes = seq(-.01, .01, length.out = n_effect_sizes)
sample_sizes = seq(10, 50)

sim_output_2 = matrix(nrow = length(effect_sizes), ncol = length(sample_sizes))

for(k in 1:length(effect_sizes))
{
  effect_size = effect_sizes[k]
  for(j in 1:length(sample_sizes))
  {
    x_vals = seq(0, 100, length.out = sample_sizes[j])
    
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(
        x = x_vals,
        y_int = int_obs,
        slope = effect_size,
        st_dev = sd_obs
      )
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    sim_output_2[k, j] = sum(p_vals < alpha) / n_sims
  }
  print(paste0("computing effect size ", k," of ", length(effect_sizes)))
}

sim_n_effect_size = 
  list(
    power = sim_output_2,
    effect_size = effect_sizes,
    sample_size = sample_sizes
  )

#Since the results are stored in a matrix we need a 2-d way of visualizing it.
#Use image()
image(sim_n_effect_size$power)

##------------Plotting 3-d data---------
#hint: set n_sims and n_effect_sizes to small values while experimenting
#Save as an output file

#contour plotting
contour(x= sim_n_effect_size$effect_size,
        y= sim_n_effect_size$sample_size,
        z=  sim_output_2)

#Perspective Plots- 3-d surface, persp()
persp(
  x = sim_n_effect_size$effect_size,
  y = sim_n_effect_size$sample_size,
  z = sim_n_effect_size$power,
  xlab = "beta", ylab = "n", zlab = "power",
  col = 'lightblue',
  theta = 30, phi = 30, expand = .75,
  ticktype = 'detailed')

#interactive 3-d plots
persp3d(x = sim_n_effect_size$effect_size,
        y = sim_n_effect_size$sample_size,
        z = sim_n_effect_size$power,
        xlab = "beta", ylab = "n", zlab = "power",
        col = 'lightblue',
        theta = 30, phi = 30, expand = .75,
        ticktype = 'detailed')

#Saving an interactive plot
rgl::writeWebGL(
  dir = here::here("docs", "webGL"), 
  filename = here::here(
    "docs", "webGL",
    "n_effect_size_power_sim_plot.html"),
  width = 1200, height = 1200
)

#Saving data files
save(
  sim_n_effect_size,
  file = here::here("data", "lab_11_n_effect_sizes.Rdata"))

#to load the data again do the following
load(file = here::here("data", "lab_11_n_effect_sizes.Rdata"))


##-------------------Lab Assignment-----------------
#using population dispersion(ie population standard deviation)
#Population Dispersion Analysis
alpha = 0.05
n_sims = 1000
p_vals = numeric(n_sims)# taken from line 170

# What was the observed standard deviation?
#sd_obs = 0.14   3x .14=.42

# specify the number of different standard deviation values to simulate:
n_sds = 20
pop_sds = seq(from = 0.01, to = 4, length.out = n_sds)

pop_sd_power = numeric(n_sds)

for(j in 1:length(pop_sds))
{
  pop_sd_j = pop_sds[j]
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit( 
      x = birdhab$ls,
      y_int = int_obs,
     slope = slope_obs,
     st_dev = pop_sd_j)
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  pop_sd_power[j] = sum(p_vals < alpha) / n_sims
}

sim_output_dispersion = data.frame(
  sd = pop_sds,
  power = pop_sd_power)

# You should save your simulation results so you don't have to run it every time.
save(
  sim_output_dispersion, 
  file = here::here("data", "lab_ll_dat_dispersion_sim.RData"))

# Line plot of standard deviation (x-axis) and statistical power (y-axis)

png(filename = "dispersion power plot. png")
plot(power ~ sd, data = sim_output_dispersion,
     main = "Population Dipersion and Statistical Power",
     type = 'l', xlab = "Standard Deviation", 
     ylab = "Power")
abline(v = coef(fit_1)[2], lty = 2, col ='red')

dev.off()

# Add a dotted vertical red line at the observed population standard deviation value.
abline(v = coef(fit_1)[2], lty = 2, col ='red')

##------------Population Dispersion and Sample Size Analysis---------
alpha = 0.05

# Start with a small number
n_sims = 100
p_vals = numeric(n_sims)
# What was the observed standard deviation?
#sd_obs = 0.14   3x .14=.42

# specify the number of different standard deviation values to simulate:
# Start with a small number
n_sds = 50
pop_sds = seq(from = 0.05, to = 1.5 , length.out = n_sds)

pop_sd_power = numeric(n_sds)

sample_sizes = seq(5, 100)

sim_output_3 = matrix(nrow = length(pop_sds), ncol = length(sample_sizes))
#dispersion

for(k in 1:length(pop_sds))
{
  pop_sd_k = pop_sds[k]
  
  for(j in 1:length(sample_sizes))
  {
    x_vals = seq(0, 100, length.out = sample_sizes[j])
    
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(
        x = x_vals,
        y_int = int_obs,
        slope = slope_obs,  
        st_dev = pop_sd_k         
      )
      p_vals[i] =  p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    
    sim_output_3[k, j] = sum(p_vals < alpha) / n_sims
  }
  print(paste0("Testing standard deviation ", k, " of ", n_sds))
}


image(sim_output_3)

sim_3_dat = 
  list(
    power       = sim_output_3,
    sample_size = sample_sizes,
    pop_sd      = pop_sds)

# You should save your simulation results so you don't have to run it every time.
save(
  sim_3_dat, 
  file = here::here("data", "lab_ll_sim_output_dispersion_n_1000.RData"))

# Question 2 Contour plot of sample size and contour plot

png(filename = "contour_n_sd.png")
contour(
x = sim_3_dat$pop_sd,
y = sim_3_dat$sample_size,
z = sim_output_3)

dev.off()

# for the plot below I got an error say there was a dimesion match between my x and y values
#contour(
  #x = sim_3_dat$sample_size,
  #y = sim_3_dat$pop_sd,
  #z = sim_output_3)

# Question 3
persp3d(x = sim_3_dat$pop_sd,
        y = sim_3_dat$sample_size,
        z = sim_3_dat$power,
        xlab = "sd", ylab = "n", zlab = "power",
        col = 12,
        theta = 30, phi = 30, expand = .75, alpha = .2,
        ticktype = 'detailed')

#Saving an interactive plot
rgl::writeWebGL(
  dir = here::here("docs", "webGL"), 
  filename = here::here(
    "docs", "webGL",
    "n_size_sd_power_sim_plot.html"),
  width = 1000, height = 1000
)
