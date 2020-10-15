#Lab 7 walkthrough
# Create simulated data
library(here)
dat = matrix(1:49, nrow = 7, ncol = 7)
print(dat)
#min, rows
apply(dat, MARGIN = 1, FUN = min)
#max, rows
apply(dat, MARGIN = 1, FUN = max)
#mean, colums
apply(dat, MARGIN = 2, FUN = mean)
#data files, moths
moths = read.csv(here("data", "moths.csv"))
head(moths)

#histogram of a species of moths code name anst
hist(moths$anst)

# Parametric Confidence Interval, qt is quantile
# Standard way to construct confidence intervals for the mean and test the hypothesis
# that the mean differs from 0
alpha = 0.05
anst = moths$anst
n = sum(!is.na(anst))
t_crit = abs(qt(alpha/ 2, df = n-1))

sse = sd(anst) / sqrt(n)

sample_mean = mean(anst)
ci_parametric = sse * t_crit

confidence_intervals = 
  data.frame(
    technique = c("parametric: t-dist"),
    mean = sample_mean,
    ci_radius = sse * t_crit,
    lower = sample_mean - ci_parametric,
    upper = sample_mean + ci_parametric
  )
 

#Simple Bootstrap confidence interval
# step 1 create an empty vector to hold the bootstrap sample means
m = 10000

# numeric() creates an vector of length m with all values initiailized to zero
result = numeric(m)
head(result)

#Preform the bootstrap, Create the resampled data sets and calculate the means
for(i in 1:m)
{
  result[i] = mean(sample(anst, replace=TRUE))
}

#Calculate the quantiles, calculate the ci from teh quantiles of the resulting
# bootstrap means
mean(result)
quantile(result, c(0.025, 0.975))

# install boot package
install.packages("boot")
library(boot)
#boot(data, statistic, R)
boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

# bootstrap with 10000 resampling
myboot =
  boot(data = anst,
       statistic = boot_mean,
       R = 10000)
print(myboot)

# you can see the attributes are available for retrieval using the dollar sign
str(myboot)

mean(anst)
myboot$t0
mean(myboot$t) - myboot$t0
sd(myboot$t)

#bootstrap confidencde interval
quantile(
  myboot$t,
  c(0.025, 0.975)
)


#Rarefaction Curve
#setting up the bootstrap
moth_dat = moths[,-1]
head(moth_dat)

#running the bootstrap simulation
n = nrow(moth_dat)# number of rows or sample observation
m = 100 #number of bootstrap iterations
moth_results = matrix(
  nrow = m,
  ncol = n)

#data[sample(...), ]

for(i in 1:m)
{
  for(j in 1:n)
  {row_j = sample(n, size = j, replace = TRUE)
  t1 = moth_dat[row_j, ]
  t2 = apply(t1, 2, sum)
  moth_results[i, j] = sum(t2> 0)
  }
}

head(moth_results)

#First Draft

rarefaction_sampler = function(input_dat, n_iterations)
{
n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  for(i in 1:n_iterations)
  {
    for(j in 1:n)
    {
      rows_j = sample(n, size = j, replace = TRUE)
      t1 = input_dat[rows_j, ]
      t2 = apply(t1, 2, sum)
      
      results_out[i, j] = sum(t2> 0)
      
    }
  }
  
  return(results_out)
}

rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)

# clear current R sessions's environment
rm(list = ls())
#test out function in clear environment
moths = read.csv(here("data", "moths.csv"))
moth_dat = moths[,-1]

rarefaction_sampler = function(input_dat, n_iterations)
{
  
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  for(i in 1:n_iterations)
  {
  
    #for(j in 1:n)
    for(j in 1:n_input_rows)
    {
     
      #rows_j = sample(n, size = j, replace=TRUE)
      rows_j = sample(n_input_rows, size = j, replace=TRUE)
      
      t1 = input_dat[rows_j, ]
      
      t2 = apply(t1, 2, sum)
      
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moth_dat, 100)

#test code
rm(list = ls())

# Re-read my data:
moths = read.csv(here("data", "moths.csv"))

rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)

results_out = matrix(
  nrow = n_iterations,
  ncol = n_input_rows)

for(i in 1:n_iterations)
{
  
  #for(j in 1:n)
  for(j in 1:n_input_rows)
  {
    
    #rows_j = sample(n, size = j, replace=TRUE)
    rows_j = sample(n_input_rows, size = j, replace=TRUE)
    
    t1 = input_dat[rows_j, ]
    
    t2 = apply(t1, 2, sum)
    
    results_out[i, j] = sum(t2 > 0)
  }
}
return(results_out)
}

rarefact = rarefaction_sampler(moths[,-1], 100)
head(rarefact)

#simulation with 10,000 iterations 
moths = read.csv(here("data", "moths.csv"))
rarefact = rarefaction_sampler(moths[,-1], 10000)

head(rarefact)


rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))

#Plotting the curve, lty= line type, col= color of line
matplot(
  rare,
  type='l',
  xlab='Number of sampling plots',
  ylab='Species richness', 
  main='Rarefaction Curve')

legend(
  'bottomright',
  legend=c('mean','2.5%','97.5%'),
  lty=c(1,2,3),col=c(1,2,3), inset=c(.1,.1))