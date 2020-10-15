library(here)
#Lab 7 Questions
rm(list = ls())
library(palmerpenguins)


# T-test from lab need to change for Gentoo Penguin data
alpha_peng = 0.05
gentoo = subset(penguins, species =="Gentoo")
n = sum(!is.na(gentoo$bill_length_mm))
t_crit = abs(qt(alpha_peng / 2, df = n - 1))

sd_gentoo = sd(gentoo$bill_length_mm, na.rm = TRUE)

sse = sd_gentoo/sqrt(n)
sse_mean= function(sx)
{
  n = length(sx) - sum(is.na(sx))
  standard_dev = sd(sx, na.rm= TRUE)
  
  return(standard_dev/sqrt(n))
}
sse_mean(gentoo$bill_length_mm)

sample_mean = mean(gentoo$bill_length_mm, na.rm = TRUE)
ci_parametric = sse * t_crit


confidence_intervals = 
  data.frame(
    technique = c("parametric: t-dist"),
    mean = sample_mean,
    ci_radius = sse * t_crit,
    lower = sample_mean - ci_parametric,
    upper = sample_mean + ci_parametric
  )

confidence_intervals


#Question 2
#Copied from walkthrough, change for penguin data
library(boot)
boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

# bootstrap with 10000 resampling
myboot =
  boot(data = gentoo$bill_length_mm,
       statistic = boot_mean,
       R = 10000)
print(myboot)

# you can see the attributes are available for retrieval using the dollar sign
str(myboot)

mean(gentoo$bill_length_mm, na.rm = TRUE) 
myboot$t0
mean(myboot$t) - myboot$t0
sd(myboot$t)

#bootstrap confidencde interval
quantile(
  myboot$t,
  c(0.025, 0.975)
)


#Question 3
rm(list = ls())

# Re-read my data:
library(here)
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
library(here)
moths = read.csv(here("data", "moths.csv"))
rarefact = rarefaction_sampler(moths[,-1], 10000)

head(rarefact)


rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))


#Plotting the curve, lty= line type, col= color of line
png(filename = "Lab 7 Rarefaction Curve.png")
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

dev.off()



